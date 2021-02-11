type opam_src = [ `Dir of Nix_expr.t | `File of Nix_expr.t ]

type url = [ `http of string * Digest_cache.opam_digest list ]

type unsupported_archive = [ `unsupported_archive of OpamTypes.file_name ]

val load_opam : string -> OpamFile.OPAM.t
(** [load_opam tag] *)

val init_variables :
  unit -> OpamVariable.variable_contents OpamVariable.Full.Map.t
(** [init_variables] *)

val nix_of_url :
  cache:Digest_cache.t -> url -> (Nix_expr.t, Digest_cache.error) result Lwt.t
(** [nix_of_url] *)

val url : OpamFile.URL.t -> (url, [> unsupported_archive ]) result
(** [url] *)


(** [Requirement] *)
module Requirement : sig
  type t

end

class dependency_map :
  object
    method add_dep : OpamTypes.package -> Requirement.t -> unit

    method init_package : OpamTypes.package -> unit

    method to_string : string
  end

val string_of_url : url -> string
(** [string_of_url] *)

val nix_of_opam :
  pkg:OpamPackage.t ->
  deps:< init_package : OpamPackage.t -> unit ; .. > ->
  opam_src:opam_src ->
  opam:OpamFile.OPAM.t ->
  src:Nix_expr.t option ->
  url:url option ->
  unit ->
  Nix_expr.t
(** [nix_of_opam] *)
