type spec = {
  github_owner : string;
  github_name : string;
  spec_commit : string option;
}

type t = {
  repo_key : string;
  spec : spec;
  repo_path : string;
  repo_commit : string;
  repo_digest : Digest_cache.nix_digest Lwt.t;
}

val repo_key : t -> string

val nix_digest_of_path : string -> [> `sha256 of string ] Lwt.t
(** [nix_digest_of_path] *)

type direct_package = {
  direct_opam_relative : string;
  direct_opam : OpamFile.OPAM.t;
  direct_name : OpamPackage.Name.t;
  direct_version : OpamPackage.Version.t option;
}

type loaded_package = {
  loaded_opam : OpamFile.OPAM.t;
  repository_expr : unit -> Opam_metadata.opam_src Lwt.t;
  src_expr :
    Digest_cache.t -> (Nix_expr.t option, Digest_cache.error) result Lwt.t;
  loaded_url : Opam_metadata.url option;
}

type package = {
  repo : t;
  rel_path : string;
  package : OpamPackage.t;
  opam : OpamFile.OPAM.t;
  url : OpamFile.URL.t option;
}

val full_path : package -> string
(** [full_path] *)

val list_package : t -> string -> package list
(** [list_package] *)

val git_url : spec -> string
(** [git_url] *)


