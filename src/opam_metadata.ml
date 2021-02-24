module Url = struct
  type t = Url of string * Digest_cache.opam_digest list

  let to_string (Url (u, _)) = u

  let create u d = Url (u, d)

  let ends_with ext (Url (u, _)) = Util.ends_with ext u
end

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
    | SimpleOpam of string (* package name *)
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
        let string_of_filter :
            OpamTypes.filter OpamTypes.filter_or_constraint -> string = function
          | Filter f -> OpamFilter.to_string f
          | Constraint (op, f) -> string_of_relop op ^ OpamFilter.to_string f
        in
        let string_of_atom :
            OpamPackage.Name.t
            * OpamTypes.filter OpamTypes.filter_or_constraint
              OpamFormula.formula ->
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

type 'a requirement = Required of 'a | Optional of 'a

module Requirement = struct
  type t = Dependency.t requirement

  let _pp ppf = function
    | Required dep -> Dependency.pp ppf dep
    | Optional dep -> fprintf ppf "{%a}" Dependency.pp dep

  let _optional d = Optional d

  let _required d = Required d

  let _dependency = function _, d -> d

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

let add_nix_inputs ~add_native ~(add_opam : Importance.t -> string -> unit)
    (importance, dependency) =
  let nixos_env = Vars.simple_lookup ~vars:(nixos_vars ()) in
  Log.debug "Adding dependency: %s\n" (Dependency.to_string dependency);
  let desc =
    match importance with
    | Importance.Required -> "dep"
    | Importance.Optional -> "optional dep"
  in

  match dependency with
  (* | Dependency.Nix name -> add_native importance name *)
  | Dependency.SimpleOpam dep -> add_opam importance dep
  | Dependency.External externals ->
      let apply_filters env (deps, filter) =
        try
          if
            OpamFilter.eval_to_bool ~default:false env
              (filter : OpamTypes.filter)
          then Some deps
          else None
        with Invalid_argument desc ->
          Printf.eprintf "  Note: depext filter raised Invalid_argument: %s\n"
            desc;
          None
      in

      (* What happens if some are nixos, some are not ?*)
      let importance, deps =
        match Util.filter_map (apply_filters nixos_env) externals with
        | [] ->
            Log.debug
              "  Note: package has depexts, but none of them `nixos`:\n    %s\n"
              (Dependency.to_string dependency);
            Log.debug "  Adding them all as `optional` dependencies.\n";
            (Importance.Optional, List.map fst externals)
        | nixos_deps -> (Importance.Required, nixos_deps)
      in

      List.iter
        (OpamSysPkg.Set.iter (fun dep ->
             let name = OpamSysPkg.to_string dep in
             Format.printf "  adding nix %s: %s@." desc name;
             add_native importance name))
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
            (* hum : if it's optional, we could have none while we need at least one *)
            add_formula Optional x;
            add_formula Optional y
      in
      let formula =
        OpamPackageVar.filter_depends_formula ~build:true ~post:false
          ~test:false ~doc:false ~default:false ~env:nixos_env formula
      in
      add_formula importance formula;
      (* why ? *)
      add_formula Optional formula

let url urlfile : (Url.t, [> unsupported_archive ]) Result.t =
  let url, checksums =
    (OpamFile.URL.url urlfile, OpamFile.URL.checksum urlfile)
  in
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
      |> Result.map (fun checksums -> Url.create url_without_backend checksums)
      (* drop the VCS portion *)
  | `http, _, Some _ -> Error (`unsupported_archive "http with fragment")
  | `rsync, transport, None ->
      Error (`unsupported_archive ("rsync transport: " ^ transport))
  | `rsync, _, Some _ -> Error (`unsupported_archive "rsync with fragment")

let load_opam path =
  Log.debug "Loading opam file: %s\n" path;
  if not (Sys.file_exists path) then
    raise (Invalid_package ("No opam file at " ^ path));
  let open OpamFilename in
  (* TODO could pass this in if we want to embrace OpamFilename more fully *)
  let dir = Dir.of_string (Filename.dirname path) in
  let base = Base.of_string (Filename.basename path) in
  let file = OpamFilename.create dir base |> OpamFile.make in
  let loaded = OpamFile.OPAM.read file in
  if OpamFile.OPAM.format_errors loaded <> [] then (
    OpamFile.OPAM.print_errors loaded;
    failwith (Printf.sprintf "Invalid OPAM file: %s" path) );
  loaded |> OpamFormatUpgrade.opam_file

let nix_of_url ~cache url : (Nix_expr.t, Digest_cache.error) Result.t Lwt.t =
  let open Nix_expr in
  match url with
  | Url.Url (src, checksums) ->
      Digest_cache.add src checksums cache
      |> Lwt.map (fun digest ->
             digest
             |> Result.map (function `sha256 sha256 -> ("sha256", str sha256))
             |> Result.map (fun digest ->
                    call
                      [
                        lit "pkgs.fetchurl"; attrs [ ("url", str src); digest ];
                      ]))


module InputMap = struct
  include Nix_expr.AttrSet

  (* override `add` to keep the "most required" entry *)
  let add k v map =
    match find_opt k map with
    | Some existing when Importance.more_important existing v -> map
    | Some _ | None -> add k v map
end

module Dependencies = struct
  type t = {
    mutable optional : Dependency.t list;
    mutable required : Dependency.t list;
  }

  let create () = { optional = []; required = [] }

  let requires t dep = t.required <- dep :: t.required

  let optional t dep = t.optional <- dep :: t.optional

  let require_pkg name = Required name

  let may_need_pkg name = Optional name

  let nixos_env = Vars.simple_lookup ~vars:(nixos_vars ())

  let is_nixos_dependency =
    let env = nixos_env in
    OpamFilter.eval_to_bool ~default:false env

  let partition t =
    let opam_deps = Hashtbl.create 7 and nix_deps = Hashtbl.create 7 in
    let register_dependencies mark =
      List.iter (function
        | Dependency.SimpleOpam depname ->
            Hashtbl.replace opam_deps (mark depname) ()
        | Dependency.Package formula ->
            let rec add_formula mark =
              let open OpamFormula in
              function
              | Empty -> ()
              | Atom (pkg, _version) ->
                  let name = OpamPackage.Name.to_string pkg in
                  Hashtbl.replace opam_deps (mark name) ()
              | Block x -> add_formula mark x
              | And (x, y) ->
                  add_formula mark x;
                  add_formula mark y
              | Or (x, y) ->
                  (* hum : if it's optional, we could have none while we need at least one *)
                  add_formula may_need_pkg x;
                  add_formula may_need_pkg y
            in
            let formula =
              OpamPackageVar.filter_depends_formula ~build:true ~post:false
                ~test:false ~doc:false ~default:false ~env:nixos_env formula
            in
            add_formula mark formula
        | Dependency.External externals ->
            List.iter
              (fun (pkg_set, filter) ->
                let mark' =
                  if is_nixos_dependency filter then mark else may_need_pkg
                in
                OpamSysPkg.Set.iter
                  (fun syspkg ->
                    let pkg_name = OpamSysPkg.to_string syspkg in
                    Format.printf "Adding Nix dependency: %s@." pkg_name;
                    let requirement = mark' pkg_name in
                    match requirement with
                    | Required m ->
                        Hashtbl.remove nix_deps (may_need_pkg m);
                        Hashtbl.replace nix_deps requirement ()
                    | Optional _ -> ())
                        (* if not @@ Hashtbl.mem nix_deps (require_pkg m) then
                         *   Hashtbl.replace nix_deps requirement ()) *)
                  pkg_set)
              externals)
    in
    register_dependencies may_need_pkg t.optional;
    register_dependencies require_pkg t.required;
    let htbl_to_list h = Hashtbl.fold (fun k _ l -> k :: l) h [] in
    (htbl_to_list opam_deps, htbl_to_list nix_deps)
end

let add_implicit_build_dependencies ~add_dep commands =
  let implicit_optdeps = ref OpamStd.String.Set.empty in
  (* If your build command depends on foo:installed, you have an implicit optional
     * build dependency on foo. Packages *should* declare this, but don't always... *)
  let lookup_var key =
    match Vars.implicit_package_var key with
    | None -> None
    | Some pkg ->
        let pkgname = OpamPackage.Name.to_string pkg in
        Log.debug "  adding implied dep: %s\n" pkgname;
        implicit_optdeps := !implicit_optdeps |> OpamStd.String.Set.add pkgname;
        (* value doesn't actually matter, since we don't use the result *)
        Some (OpamTypes.B true)
  in
  List.iter
    (fun commands -> ignore @@ OpamFilter.commands lookup_var commands)
    commands;
  !implicit_optdeps
  |> OpamStd.String.Set.iter (fun pkg ->
         add_dep Importance.Optional (Dependency.SimpleOpam pkg))

let add_opam_deps ~add_dep (opam : OpamFile.OPAM.t) =
  add_implicit_build_dependencies ~add_dep
    [ OpamFile.OPAM.build opam; OpamFile.OPAM.install opam ];
  add_dep Optional (Dependency.Package (OpamFile.OPAM.depopts opam));
  add_dep Required (Dependency.Package (OpamFile.OPAM.depends opam));
  match OpamFile.OPAM.depexts opam with
  | [] -> ()
  | depexts -> add_dep Required (Dependency.External depexts)

let add_build_dependencies deps (commands : OpamTypes.command list) =
  let lookup_var key =
    match Vars.implicit_package_var key with
    | None -> None
    | Some pkg ->
        let pkgname = OpamPackage.Name.to_string pkg in
        Log.debug "  adding implied dep: %s\n" pkgname;
        Dependencies.optional deps (Dependency.SimpleOpam pkgname);
        (* value doesn't actually matter, since we don't use the result *)
        Some (OpamTypes.B true)
  in
  ignore @@ OpamFilter.commands lookup_var commands

let filter_nixos_dependencies depexts = depexts

let add_opam_dependencies deps opam =
  List.iter
    (add_build_dependencies deps)
    [ OpamFile.OPAM.build opam; OpamFile.OPAM.install opam ];
  Dependencies.optional deps (Dependency.Package (OpamFile.OPAM.depopts opam));
  Dependencies.requires deps (Dependency.Package (OpamFile.OPAM.depends opam));
  match OpamFile.OPAM.depexts opam with
  | [] -> ()
  | depexts -> Dependencies.requires deps (Dependency.External (filter_nixos_dependencies depexts))

let opam2nix ?url ?src ~pkg ~opam_src opam =
  let name = OpamPackage.(name pkg |> Name.to_string) in
  let version = OpamPackage.(version pkg |> Version.to_string) in
  let dependencies = Dependencies.create () in
  add_opam_dependencies dependencies opam;
  ( match url with
  | Some u ->
      if Url.ends_with ".zip" u then
        Dependencies.requires dependencies (Dependency.SimpleOpam "unzip")
  | None -> () );

  let property_of_input src pkg_dep =
    match pkg_dep with
    | Optional name -> Nix_expr.optional name src
    | Required name -> PropertyPath (src, String.split_on_char '.' name)
  in

  let opam_inputs, nix_deps = Dependencies.partition dependencies in

  let opam_inputs =
    let selection = Nix_expr.Id "selection" in
    List.fold_left
      (fun map -> function
        | (Required name | Optional name) as dep ->
            let v = property_of_input selection dep in
            Nix_expr.AttrSet.add name v map)
      Nix_expr.AttrSet.empty opam_inputs
  in

  let build_inputs =
    let pkgs = Nix_expr.Id "pkgs" in
    let name_of = function Required name | Optional name -> name in
    List.sort (fun a b -> String.compare (name_of a) (name_of b)) nix_deps
    |> List.map (property_of_input pkgs)
  in
  
  (* TODO: separate build-only deps from propagated *)
  Nix_expr.opam_attrset
    ~pname:name ~version ?src ~opam_inputs ~opam_src ~build_inputs

let nix_of_opam ?url ?src ~pkg ~opam_src opam =
  let nix_e = opam2nix ?url ?src ~pkg ~opam_src opam in
  let name = OpamPackage.(name pkg |> Name.to_string) in
  let version = OpamPackage.(version pkg |> Version.to_string) in
  let adder r importance name = r := InputMap.add name importance !r in
  let opam_inputs = ref InputMap.empty in
  let nix_deps = ref InputMap.empty in
  let add_native = adder nix_deps in
  let add_opam_input = adder opam_inputs in

  let add_dep importance dep =
    add_nix_inputs ~add_native ~add_opam:add_opam_input (importance, dep)
  in

  add_opam_deps ~add_dep opam;

  ( match url with
  | Some u -> if Url.ends_with ".zip" u then add_native Required "unzip"
  | None -> () );
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

  let nix_e_res = 
  Nix_expr.opam_attrset
    ~pname:name ~version ?src ~opam_inputs ~opam_src ~build_inputs:nix_deps

  in
  Format.printf "Writing nix attrset files %s@." name;
  Nix_expr.write_file ~filename:"nix_refactor.nix" nix_e;
  Nix_expr.write_file ~filename:"nix_orig.nix" nix_e_res;
  let n = Sys.command "diff nix_refactor.nix nix_orig.nix" in
  if n <> 0 then Format.printf "AHAH: %s@." name;
  nix_e_res

let nixify = nix_of_opam
