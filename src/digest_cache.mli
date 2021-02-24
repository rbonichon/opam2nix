module Cache : OpamStd.MAP with type key = string
(** [Cache] *)

type opam_digest = OpamHash.t

type key = string

type error =
  [ `checksum_mismatch of key
  | `command_failed of int option * key list
  | `download_failed of key
  | `error of key ]

type nix_digest = Sha256 of key

type state = {
  digests : (nix_digest, error) result Lwt.t Cache.t;
  path : key;
  download_ctx : Download.Ctx.t option;
}

type t = state ref

val add : key -> opam_digest list -> t -> (nix_digest, error) result Lwt.t
(** [add] *)

val add_custom :
  t ->
  keys:key list ->
  (unit -> (nix_digest, error) result Lwt.t) ->
  (nix_digest, error) result Lwt.t
(** [add_custom] *)

val string_of_error : error -> key
(** [string_of_error] *)

val save : t -> unit Lwt.t
(** [save] *)

val try_load : key -> t
(** [try_load] *)
