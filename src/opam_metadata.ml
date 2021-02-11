open Util
module URL = OpamFile.URL
module OPAM = OpamFile.OPAM
module Descr = OpamFile.Descr
module StringSet = OpamStd.String.Set
module StringSetMap = OpamStd.String.SetMap

type url = [ `http of string * Digest_cache.opam_digest list ]

let string_of_url : url -> string = function `http (url, _digest) -> url


type unsupported_archive = [ `unsupported_archive of string ]

exception Invalid_package of string

open Format

let string_of_relop : OpamTypes.relop -> string = function
  | `Eq -> "="
  | `Geq -> ">="
  | `Gt -> ">"
  | `Leq -> "<="
  | `Lt -> "<"
  | `Neq -> "!="

module Dependency = struct
  type t =
    (* | Nix of string *)
    | SimpleOpam of string
    | External of (OpamSysPkg.Set.t * OpamTypes.filter) list
    | Package of OpamTypes.filtered_formula

  let pp ppf = function
    (* | Nix dep -> fprintf ppf "nix:%s" dep *)
    | SimpleOpam dep -> fprintf ppf "package:%s" dep
    | External deps ->
        fprintf ppf "@[<hov>external:[%a]@]"
          ((pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf ",@ "))
             (fun ppf (deps, filter) ->
               fprintf ppf "[<hov>%a@ {%s}@]"
                 (fun ppf set ->
                   OpamSysPkg.Set.iter
                     (fun pkg -> fprintf ppf "%s," (OpamSysPkg.to_string pkg))
                     set)
                 deps
                 (OpamFilter.to_string filter)))
          deps
    | Package formula ->
        (* of OpamTypes.formula *)
        let string_of_filter : OpamTypes.filter OpamTypes.filter_or_constraint -> string = function
          | Filter f -> OpamFilter.to_string f
          | Constraint (op, f) -> string_of_relop op ^ OpamFilter.to_string f
        in
        let string_of_atom :
            OpamPackage.Name.t * OpamTypes.filter OpamTypes.filter_or_constraint OpamFormula.formula ->
            string =
         fun (name, formula) ->
          OpamPackage.Name.to_string name
          ^ ":"
          ^ OpamFormula.string_of_formula string_of_filter formula
        in
        fprintf ppf "package_formula:%s"
          (OpamFormula.string_of_formula string_of_atom formula)

  let to_string t = Format.asprintf "%a" pp t (* TODO:  *)
end

module Importance = struct
  type t = Required | Optional

  let compare a b =
    match (a, b) with
    | Required, Required | Optional, Optional -> 0
    | Required, _ -> 1
    | Optional, _ -> -1

  let more_important a b = compare a b > 0
end

module Requirement = struct
  type t = Importance.t * Dependency.t 
  open Importance
  let pp ppf = function
    | Required, dep -> Dependency.pp ppf dep
    | Optional, dep -> fprintf ppf "{%a}" Dependency.pp dep

  let to_string t = Format.asprintf "%a" pp t


  let optional d = Optional, d

  let required d = Required, d

  (* let compare d1 d2 =
   *   match d1, d2 with
   *   | Required _ , Required _ | Optional _, Optional _ -> 0
   *   | Required _ , Optional _ -> 1
   *   | Optional _, Required _ -> -1
   * 
   * let ( > ) d1 d2 = compare d1 d2 > 0 *)
end

let add_var (scope : OpamVariable.t -> OpamVariable.Full.t) name v vars =
  let var : OpamVariable.Full.t = scope (OpamVariable.of_string name) in
  vars |> OpamVariable.Full.Map.add var v

let package_var pkgname =
  OpamVariable.Full.create (OpamPackage.Name.of_string pkgname)

let global_var = OpamVariable.Full.global

let add_global_var = add_var global_var

let add_package_var pkgname = add_var (package_var pkgname)

let native_system_vars () =
  let state = OpamVariable.Full.Map.empty in
  let system_variables = OpamSysPoll.variables in
  List.fold_left
    (fun state ((name : OpamVariable.t), value) ->
      Lazy.force value
      |> Option.map (fun value ->
             OpamVariable.Full.Map.add
               (OpamVariable.Full.global name)
               value state)
      |> Option.value ~default:state)
    state system_variables

let nixos_vars () =
  native_system_vars ()
  |> add_global_var "os-family" (OpamTypes.S "unknown")
  |> add_global_var "os-distribution" (S "nixos")
  |> add_global_var "os-version" (S "unknown")

(* I don't think we can easily get a number here, but it should
   * rarely matter *)

let add_base_variables base_vars =
  base_vars
  |> add_global_var "make" (S "make")
  |> add_global_var "opam-version"
       (S (OpamVersion.to_string OpamVersion.current))
  |> add_global_var "pinned" (B false) (* probably ? *)
  |> add_global_var "jobs"
       (S (Util.getenv_opt "NIX_BUILD_CORES" |> Option.value ~default:"1"))
  |> add_global_var "enable-ocaml-beta-repository" (OpamTypes.B false)
  (* With preinstalled packages suppose they can't write
     in the ocaml directory *)
  |> add_global_var "preinstalled" (OpamTypes.B true)
  |> add_package_var "ocaml" "preinstalled" (OpamTypes.B true)
  |> add_package_var "ocaml" "native" (OpamTypes.B true)
  |> add_package_var "ocaml" "native-tools" (OpamTypes.B true)
  |> add_package_var "ocaml" "native-dynlink" (OpamTypes.B true)

let init_variables () = add_base_variables (nixos_vars ())

(* let installed_pkg_var key =
 *   let open OpamVariable in
 *   match Full.scope key with
 *   | Full.Package pkg
 *     when Full.variable key |> OpamVariable.to_string = "installed" ->
 *       Some pkg
 *   | _ -> None *)

let add_nix_inputs ~(add_native : Importance.t -> string -> unit)
    ~(add_opam : Importance.t -> string -> unit) importance dep =
  let desc =
    let open Importance in
    match importance with Required -> "dep" | Optional -> "optional dep"
  in
  let nixos_env = Vars.simple_lookup ~vars:(nixos_vars ()) in
  debug "Adding dependency: %s\n" (Dependency.to_string dep);

  match dep with
  (* | Dependency.Nix name -> add_native importance name *)
  | Dependency.SimpleOpam dep -> add_opam importance dep
  | Dependency.External externals ->
      let apply_filters env (deps, filter) =
        try
          if OpamFilter.eval_to_bool ~default:false env filter then Some deps
          else None
        with Invalid_argument desc ->
          Printf.eprintf "  Note: depext filter raised Invalid_argument: %s\n"
            desc;
          None
      in

      let importance, deps =
        match Util.filter_map (apply_filters nixos_env) externals with
        | [] ->
            debug
              "  Note: package has depexts, but none of them `nixos`:\n    %s\n"
              (Dependency.to_string dep);
            debug "  Adding them all as `optional` dependencies.\n";
            Requirement.optional @@ List.map fst externals
        | nixos_deps -> Requirement.required nixos_deps
      in
      List.iter
        (fun deps ->
          OpamSysPkg.Set.iter
            (fun dep ->
              let name = OpamSysPkg.to_string dep in
              debug "  adding nix %s: %s\n" desc name;
              add_native importance name)
            deps)
        deps
  | Dependency.Package formula ->
      let add importance (pkg, _version) =
        add_opam importance (OpamPackage.Name.to_string pkg)
      in
      let rec add_formula importance =
        let open OpamFormula in
        function
        | Empty -> ()
        | Atom x -> add importance x
        | Block x -> add_formula importance x
        | And (x, y) ->
            add_formula importance x;
            add_formula importance y
        | Or (x, y) ->
            add_formula Optional x;
            add_formula Optional y
      in
      OpamPackageVar.filter_depends_formula ~build:true ~post:false ~test:false
        ~doc:false ~default:false ~env:nixos_env formula
      |> add_formula importance;

      OpamPackageVar.filter_depends_formula ~build:true ~post:false ~test:false
        ~doc:false ~default:false ~env:nixos_env formula
      |> add_formula Optional

class dependency_map =
  let map : Requirement.t list OpamPackage.Map.t ref =
    ref OpamPackage.Map.empty
  in
  let get_existing package_id =
    try OpamPackage.Map.find package_id !map with Not_found -> []
  in
  object
    method init_package package_id =
      let existing = get_existing package_id in
      map := OpamPackage.Map.add package_id existing !map

    method add_dep package_id (dep : Requirement.t) =
      let existing = get_existing package_id in
      map := OpamPackage.Map.add package_id (dep :: existing) !map

    method to_string =
      let reqs_to_string reqs =
        String.concat "," (List.map Requirement.to_string reqs)
      in
      OpamPackage.Map.to_string reqs_to_string !map
  end

let url urlfile : (url, [> unsupported_archive ]) Result.t =
  let url, checksums = (URL.url urlfile, URL.checksum urlfile) in
  let OpamUrl.{ hash; transport; backend; _ } = url in
  let url_without_backend = OpamUrl.base_url url in
  let checksums =
    if checksums = [] then Error (`unsupported_archive "Checksum required")
    else Ok checksums
  in
  match (backend, transport, hash) with
  | `git, _, _ -> Error (`unsupported_archive "git")
  | `darcs, _, _ -> Error (`unsupported_archive "darcs")
  | `hg, _, _ -> Error (`unsupported_archive "hg")
  | `http, "file", None | `rsync, "file", None ->
      Error (`unsupported_archive "local path")
  | `http, _, None ->
      checksums
      |> Result.map (fun checksums -> `http (url_without_backend, checksums))
      (* drop the VCS portion *)
  | `http, _, Some _ -> Error (`unsupported_archive "http with fragment")
  | `rsync, transport, None ->
      Error (`unsupported_archive ("rsync transport: " ^ transport))
  | `rsync, _, Some _ -> Error (`unsupported_archive "rsync with fragment")

let load_opam path =
  Util.debug "Loading opam file: %s\n" path;
  if not (Sys.file_exists path) then
    raise (Invalid_package ("No opam file at " ^ path));
  let open OpamFilename in
  (* TODO could pass this in if we want to embrace OpamFilename more fully *)
  let dir = Dir.of_string (Filename.dirname path) in
  let base = Base.of_string (Filename.basename path) in
  let file = OpamFilename.create dir base |> OpamFile.make in
  let loaded = OPAM.read file in
  if OPAM.format_errors loaded <> [] then (
    OPAM.print_errors loaded;
    failwith (Printf.sprintf "Invalid OPAM file: %s" path) );
  loaded |> OpamFormatUpgrade.opam_file

let nix_of_url ~cache (url : url) :
    (Nix_expr.t, Digest_cache.error) Result.t Lwt.t =
  let open Nix_expr in
  match url with
  | `http (src, checksums) ->
      Digest_cache.add src checksums cache
      |> Lwt.map (fun digest ->
             digest
             |> Result.map (function `sha256 sha256 -> ("sha256", str sha256))
             |> Result.map (fun digest ->
                    call
                      [
                        lit "pkgs.fetchurl"; attrs [ ("url", str src); digest ];
                      ]))

let unsafe_drvname_chars = Str.regexp "[^-_.0-9a-zA-Z]"

let drvname_safe str = Str.global_replace unsafe_drvname_chars "-" str

let add_implicit_build_dependencies ~add_dep commands =
  let implicit_optdeps = ref StringSet.empty in
  (* If your build command depends on foo:installed, you have an implicit optional
     * build dependency on foo. Packages *should* declare this, but don't always... *)
  let lookup_var key =
    match Vars.implicit_package_var key with
    | None -> None
    | Some pkg ->
        let pkgname = OpamPackage.Name.to_string pkg in
        debug "  adding implied dep: %s\n" pkgname;
        implicit_optdeps := !implicit_optdeps |> StringSet.add pkgname;
        (* value doesn't actually matter, since we don't use the result *)
        Some (OpamTypes.B true)
  in
  List.iter
    (fun commands -> ignore @@ OpamFilter.commands lookup_var commands)
    commands;
  !implicit_optdeps
  |> StringSet.iter (fun pkg ->
         add_dep Importance.Optional (Dependency.SimpleOpam pkg))

let add_opam_deps ~add_dep (opam : OPAM.t) =
  add_implicit_build_dependencies ~add_dep
    [ OPAM.build opam; OPAM.install opam ];
  add_dep Optional (Dependency.Package (OPAM.depopts opam));
  add_dep Required (Dependency.Package (OPAM.depends opam));
  let depexts = OPAM.depexts opam in
  if depexts <> [] then add_dep Required (Dependency.External depexts)

module InputMap = struct
  include Nix_expr.AttrSet

  (* override `add` to keep the "most required" entry *)
  let add k v map =
    match find_opt k map with
    | Some existing when Importance.more_important existing v -> map
    | Some _ | None -> add k v map
end

type opam_src = [ `Dir of Nix_expr.t | `File of Nix_expr.t ]

let nix_of_opam ~pkg ~deps ~(opam_src : opam_src) ~opam ~src ~url () :
    Nix_expr.t =
  let name = OpamPackage.name pkg |> OpamPackage.Name.to_string in
  let version = OpamPackage.version pkg |> OpamPackage.Version.to_string in
  let adder r importance name = r := InputMap.add name importance !r in

  deps#init_package pkg;

  let opam_inputs = ref InputMap.empty in
  let nix_deps = ref InputMap.empty in
  let add_native = adder nix_deps in
  let add_opam_input = adder opam_inputs in

  let add_dep importance dep =
    add_nix_inputs ~add_native ~add_opam:add_opam_input importance dep
  in

  add_opam_deps ~add_dep opam;

  let url_ends_with ext =
    match url with
    | Some (`http (url, _)) -> Util.ends_with ext url
    | None -> false
  in

  if url_ends_with ".zip" then add_native Required "unzip";

  let property_of_input src (name, importance) : Nix_expr.t =
    match importance with
    | Importance.Optional -> Property_or (src, name, Null)
    | Importance.Required -> PropertyPath (src, String.split_on_char '.' name)
  in
  let sorted_bindings_of_input input =
    input |> InputMap.bindings
    |> List.sort (fun (a, _) (b, _) -> String.compare a b)
  in

  let opam_inputs =
    !opam_inputs
    |> InputMap.mapi (fun name importance ->
           property_of_input (Id "selection") (name, importance))
  in

  let nix_deps =
    !nix_deps |> sorted_bindings_of_input
    |> List.map (property_of_input (Id "pkgs"))
  in

  (* TODO: separate build-only deps from propagated *)
  Nix_expr.attrs
    (let base =
       [
         ("pname", Nix_expr.str (drvname_safe name));
         ("version", Nix_expr.str (drvname_safe version));
         ("src", src |> Option.value ~default:Nix_expr.Null);
         ("opamInputs", Attrs opam_inputs);
         ("opamSrc", match opam_src with `Dir expr | `File expr -> expr);
       ]
     in
     match nix_deps with
     | [] -> base
     | nix_deps -> ("buildInputs", List nix_deps) :: base)
