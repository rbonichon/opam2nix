module OPAM = OpamFile.OPAM
module Zi = Opam_0install

(* external constraints are the preselected boundaries of
 * the selection - what repos and compiler we're using *)
type external_constraints = {
  ocaml_version : OpamPackage.Version.t;
  repos : Repo.t list;
}

type error = [ Opam_metadata.unsupported_archive | `unavailable of string ]

type universe = {
  lookup_var :
    OpamPackage.t ->
    OpamVariable.Full.t ->
    OpamVariable.variable_contents option;
  repos : Repo.t list;
  mutable packages : (Repo.loaded_package, error) result OpamPackage.Map.t;
  constrained_versions : OpamPackage.Version.t OpamPackage.Name.Map.t;
}

let build_universe ~external_constraints ~base_packages ~constrained_versions
    ~direct_definitions () : universe =
  let ocaml_version = external_constraints.ocaml_version in
  let global_vars = Opam_metadata.init_variables () in
  let lookup_var package =
    let version = OpamPackage.version package in
    let name = OpamPackage.name package in
    Vars.(
      lookup
        {
          ocaml_version;
          packages =
            OpamPackage.Name.Map.of_list
              ( [
                  ( name,
                    Installed
                      { path = None; (* not yet known *)
                                     version = Some version } );
                  ( OpamPackage.Name.of_string "ocaml",
                    Installed
                      {
                        path = None;
                        (* not yet known *)
                        version = Some ocaml_version;
                      } );
                ]
              @ (base_packages |> List.map (fun name -> (name, Provided))) );
          prefix = None;
          (* not known *)
          self = name;
          vars = global_vars;
        })
  in

  let initial_packages =
    direct_definitions
    |> List.map (fun package ->
           let open Repo in
           let name = package.direct_name in
           let version =
             package.direct_version
             |> Option.value
                  ~default:(OpamPackage.Version.of_string "development")
           in
           ( OpamPackage.create name version,
             Ok
               {
                 loaded_opam = package.direct_opam;
                 loaded_url = None;
                 src_expr =
                   (fun _ ->
                     Lwt.return
                       (Ok
                          (Some
                             (Nix_expr.Call
                                [
                                  Lit "self.directSrc";
                                  Nix_expr.str (OpamPackage.Name.to_string name);
                                ]))));
                 repository_expr =
                   (fun () ->
                     Lwt.return
                       Nix_expr.(File (str package.direct_opam_relative)));
               } ))
    |> OpamPackage.Map.of_list
  in

  {
    lookup_var;
    repos = external_constraints.repos;
    packages = initial_packages;
    constrained_versions = OpamPackage.Name.Map.of_list constrained_versions;
  }

let load_package ~url pkg : Repo.loaded_package =
  let src_expr cache =
    url
    |> Option.map (fun url ->
           Opam_metadata.nix_of_url ~cache url
           |> Lwt.map (Result.map Option.some))
    |> Option.value ~default:(Lwt.return (Ok None))
  in
  let repository_expr () =
    Repo.nix_digest_of_path (Repo.full_path pkg)
    |> Lwt.map (fun (Digest_cache.Sha256 digest) ->
           let digest = "sha256:" ^ digest in
           Nix_expr.(
             Dir
               (Call
                  [
                    Id "repoPath";
                    PropertyPath (Id "repos", [ Repo.repo_key pkg.repo; "src" ]);
                    Attrs
                      (AttrSet.of_list
                         [ ("package", str pkg.rel_path); ("hash", str digest) ]);
                  ])))
  in
  { loaded_opam = pkg.opam; loaded_url = url; src_expr; repository_expr }

let check_availability ~lookup_var pkg =
  let available_filter = OPAM.available pkg.Repo.opam in
  let available =
    try
      Ok
        ( pkg.package.name <> OpamPackage.Name.of_string "opam"
        && OpamFilter.eval_to_bool (lookup_var pkg.package) available_filter )
    with e -> Error (`unavailable (Printexc.to_string e))
  in
  available
  |> Result.bind (fun available ->
         if available then Ok pkg
         else
           let vars = OpamFilter.variables available_filter in
           let vars_str =
             String.concat "/" (List.map OpamVariable.Full.to_string vars)
           in
           Error (`unavailable (Printf.sprintf "incompatible with %s" vars_str)))

module Context : Zi.S.CONTEXT with type t = universe = struct
  type t = universe

  type rejection = error

  let pp_rejection f = function
    | `unavailable s -> Fmt.pf f "Unavailable: %s" s
    | `unsupported_archive s -> Fmt.pf f "Unsupported archive: %s" s

  let check_url pkg =
    pkg.Repo.url |> Option.map Opam_metadata.url |> Option.sequence_result

  let candidates :
      t ->
      OpamPackage.Name.t ->
      (OpamPackage.Version.t * (OpamFile.OPAM.t, rejection) Stdlib.result) list
      =
   fun env name ->
    let name_str = OpamPackage.Name.to_string name in
    let () =
      let open Repo in
      env.repos
      |> List.concat_map (fun repo -> Repo.list_package repo name_str)
      (* Drop duplicates from multiple repos *)
      |> List.filter (fun pkg ->
             not (OpamPackage.Map.mem pkg.package env.packages))
      |> List.filter (fun pkg ->
             OpamPackage.Name.Map.find_opt pkg.package.name
               env.constrained_versions
             |> Option.map (OpamPackage.Version.equal pkg.package.version)
             (* OK if versions equal *)
             |> Option.value ~default:true
             (* or if there is no constraint on the version *))
      |> List.iter (fun pkg ->
             check_url pkg
             |> Result.iter (fun url ->
                    let result =
                      check_availability ~lookup_var:env.lookup_var pkg
                    in
                    let loaded = result |> Result.map (load_package ~url) in
                    env.packages <-
                      OpamPackage.Map.add pkg.package loaded env.packages))
    in

    OpamPackage.Map.bindings env.packages
    |> List.filter_map (fun (k, v) ->
           if OpamPackage.Name.equal (OpamPackage.name k) name then
             Some
               ( k.version,
                 v |> Result.map (fun loaded -> loaded.Repo.loaded_opam) )
           else None)
    |> List.sort (fun (va, _) (vb, _) -> OpamPackage.Version.compare vb va)

  (* Not supported *)
  let user_restrictions :
      t -> OpamPackage.Name.t -> OpamFormula.version_constraint option =
   fun _ _ -> None

  let filter_deps :
      t -> OpamPackage.t -> OpamTypes.filtered_formula -> OpamTypes.formula =
   fun env pkg f ->
    f
    |> OpamFilter.partial_filter_formula (env.lookup_var pkg)
    |> OpamFilter.filter_deps ~build:true ~post:true ~test:false ~doc:false
         ~dev:false ~default:false
end

include Zi.Solver.Make (Context)
