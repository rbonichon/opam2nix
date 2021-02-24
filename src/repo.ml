open OpamFile
module Version = OpamPackage.Version
module Name = OpamPackage.Name

type spec = {
  github_owner : string;
  github_name : string;
  spec_commit : string option;
}

type t = {
  repo_key : string;
  spec : spec;
  (* TODO: OpamFilename.Dir.t? *)
  repo_path : string;
  repo_commit : string;
  (* returned as Lwt.t because it's needed lazily much later than commit and version *)
  repo_digest : Digest_cache.nix_digest Lwt.t;
}

let repo_key { repo_key; _ } = repo_key

type package = {
  repo : t;
  rel_path : string;
  package : OpamPackage.t;
  opam : OPAM.t;
  url : URL.t option;
}

(* Loaded package, either from an opam repo or direct package supplied on the commandline *)
type loaded_package = {
  loaded_opam : OPAM.t;
  repository_expr : unit -> Nix_expr.opam_src Lwt.t;
  src_expr :
    Digest_cache.t -> (Nix_expr.t option, Digest_cache.error) Result.t Lwt.t;
  loaded_url : Opam_metadata.Url.t option;
}

(* a direct package is passed on the commandline, and is
 * not from any repository *)
type direct_package = {
  direct_opam_relative : string;
  direct_opam : OPAM.t;
  direct_name : Name.t;
  direct_version : Version.t option;
}

let packages_dir = "packages"

let full_path pkg = Filename.concat pkg.repo.repo_path pkg.rel_path

let git_url spec =
  Printf.sprintf "https://github.com/%s/%s.git" spec.github_owner
    spec.github_name

(* let package_desc pkg = OpamPackage.to_string pkg.package *)

let load_url path =
  if Sys.file_exists path then (
    let url_file = open_in path in
    let rv = URL.read_from_channel url_file in
    close_in url_file;
    Some rv )
  else None

let list_package =
  let version_sep = "." in
  let version_join package version =
    package ^ version_sep ^ Version.to_string version
  in
  fun repo package ->
    Log.debug "processing package %s\n" package;
    let package_base = Filename.concat packages_dir package in
    let package_abs = Filename.concat repo.repo_path package_base in
    let list_versions () =
      Log.debug "listing %s\n" package_abs;
      let dirs =
        try Util.list_dirs package_abs
        with Sys_error e ->
          Log.debug "Skipping (%s)\n" e;
          []
      in
      Util.filter_map (Util.without_leading (package ^ version_sep)) dirs
      |> List.map Version.of_string
      |> List.filter (fun version ->
             Sys.file_exists
               (Filename.concat
                  (Filename.concat package_abs (version_join package version))
                  "opam"))
    in

    list_versions ()
    |> List.map (fun version ->
           let rel_path =
             Filename.concat package_base (version_join package version)
           in
           let full_path = Filename.concat repo.repo_path rel_path in
           let opam =
             Opam_metadata.load_opam (Filename.concat full_path "opam")
           in
           {
             repo;
             rel_path;
             opam;
             package = OpamPackage.create (Name.of_string package) version;
             url =
               OPAM.url opam
               |> Option.or_else (load_url (Filename.concat full_path "url"));
           })

let nix_digest_of_path p =
  let hash_cmd =
    [ "nix-hash"; "--type"; "sha256"; "--flat"; "--base32"; "/dev/stdin" ]
  in
  let readable, writeable = Unix.pipe ~cloexec:true () in
  let open Cmd in
  run_exn
    (exec_r ~stdin:(`FD_move readable))
    ~print:false
    ~block:(fun hash_proc ->
      Lwt.both
        (file_contents hash_proc#stdout)
        (run_unit_exn
           (exec_none ~stdout:(`FD_move writeable))
           ~print:false
           [ "nix-store"; "--dump"; p ])
      |> Lwt.map (fun (output, ()) -> output))
    hash_cmd
  |> Lwt.map (fun hash -> Digest_cache.Sha256 hash)
