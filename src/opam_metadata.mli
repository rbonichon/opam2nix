(** [Url] *)
module Url: sig
  type t = private Url of string * OpamHash.t list
  val to_string : t -> string
  val create : string -> OpamHash.t list -> t
end 

type opam_src = [ `Dir of Nix_expr.t | `File of Nix_expr.t ]


type unsupported_archive = [ `unsupported_archive of OpamTypes.file_name ]

val load_opam : string -> OpamFile.OPAM.t
(** [load_opam tag] *)

val init_variables :
  unit -> OpamVariable.variable_contents OpamVariable.Full.Map.t
(** [init_variables] *)

val nix_of_url :
  cache:Digest_cache.t -> Url.t -> (Nix_expr.t, Digest_cache.error) result Lwt.t
(** [nix_of_url] *)

val url : OpamFile.URL.t -> (Url.t, [> unsupported_archive ]) result
(** [url] *)


(** [Requirement] *)
module Requirement : sig
  type t

end

val nix_of_opam :
  ?url:Url.t ->
  ?src:Nix_expr.t ->
  pkg:OpamPackage.t ->
  opam_src:opam_src ->
  OpamFile.OPAM.t ->
  Nix_expr.t
(** [nix_of_opam] *)


