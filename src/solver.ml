module OPAM = OpamFile.OPAM
module Zi = Opam_0install

(* external constraints are the preselected boundaries of
 * the selection - what repos and compiler we're using *)
type external_constraints = {
  ocaml_version : OpamPackage.Version.t;
  repos : Repo.t list;
}

type error = Unsupported_archive of string | Unavailable of string

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
    List.fold_left
      (fun map package ->
        let open Repo in
        let name = package.direct_name in
        let version =
          package.direct_version
          |> Option.value ~default:(OpamPackage.Version.of_string "development")
        in
        let key = OpamPackage.create name version in
        let value =
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
                  Lwt.return Nix_expr.(File (str package.direct_opam_relative)));
            }
        in
        OpamPackage.Map.add key value map)
      OpamPackage.Map.empty direct_definitions
  in

  {
    lookup_var;
    repos = external_constraints.repos;
    packages = initial_packages;
    constrained_versions = OpamPackage.Name.Map.of_list constrained_versions;
  }

let load_package ~url pkg : Repo.loaded_package =
  let src_expr cache =
    match url with
    | None -> Lwt.return_ok None
    | Some url ->
        Opam_metadata.nix_of_url ~cache url |> Lwt.map (Result.map Option.some)
  in
  let repository_expr () =
    Lwt.map
      (fun (Digest_cache.Sha256 digest) ->
        let open Repo in
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
      (Repo.nix_digest_of_path (Repo.full_path pkg))
  in
  { loaded_opam = pkg.opam; loaded_url = url; src_expr; repository_expr }

let check_availability ~lookup_var pkg =
  let available_filter = OPAM.available pkg.Repo.opam in
  try
    let available =
      pkg.package.name <> OpamPackage.Name.of_string "opam"
      && OpamFilter.eval_to_bool (lookup_var pkg.package) available_filter
    in
    if available then Ok pkg
    else
      let vars = OpamFilter.variables available_filter in
      let vars_str =
        (* FIXME: Do not do map + concat here *)
        String.concat "/" (List.map OpamVariable.Full.to_string vars)
      in
      Error (Unavailable (Printf.sprintf "incompatible with %s" vars_str))
  with e -> Error (Unavailable (Printexc.to_string e))

module Context : Zi.S.CONTEXT with type t = universe = struct
  type t = universe

  type rejection = error

  let pp_rejection ppf = function
    | Unavailable s -> Format.fprintf ppf "Unavailable: %s" s
    | Unsupported_archive s -> Format.fprintf ppf "Unsupported archive: %s" s

  let check_url pkg =
    pkg.Repo.url |> Option.map Opam_metadata.url |> Option.sequence_result

  let candidates :
      t ->
      OpamPackage.Name.t ->
      (OpamPackage.Version.t * (OpamFile.OPAM.t, rejection) Stdlib.result) list
      =
   fun env name ->
    let name_str = OpamPackage.Name.to_string name in
    let open Repo in
    let pkgs =
      List.concat_map (fun repo -> Repo.list_package repo name_str) env.repos
    in
    (* Drop duplicates from multiple repos *)
    List.iter
      (fun pkg ->
        if
          (not (OpamPackage.Map.mem pkg.package env.packages))
          &&
          match
            OpamPackage.Name.Map.find_opt pkg.package.name
              env.constrained_versions
          with
          | None -> true
          | Some version ->
              OpamPackage.Version.equal pkg.package.version version
        then
          check_url pkg
          |> Result.iter (fun url ->
                 let loaded =
                   Result.map (load_package ~url)
                     (check_availability ~lookup_var:env.lookup_var pkg)
                 in
                 env.packages <-
                   OpamPackage.Map.add pkg.package loaded env.packages))
      pkgs;

    (* FIXME: Can we do better ? *)
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
