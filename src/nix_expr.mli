module AttrSet : OpamStd.MAP with type key = string
(** [AttrSet] *)

type t

type expr = t

val null : t

val lets : t AttrSet.t -> t -> t
(** [lets bindings e] binds a set of values to identifiers in expression [e] **)

val func : t -> t -> t

val property : t -> string -> t

val property_path : t -> string list -> t

val property_or : t -> string -> t -> t

val _with : t -> t -> t

val str : string -> t
(** [str] *)

val attrset : (string * 'a) list -> 'a AttrSet.t
(** [attrset] *)

val attrs : (string * t) list -> t

val rec_attrs : (string * t) list -> t

val call : t list -> t

val lit : string -> t

val _int : int -> t

val id : string -> t

val optional : string -> t -> t

module Opam_src : sig
  type t

  val directory : expr -> t

  val file : expr -> t
end

val opam_attrset :
  ?src:t ->
  ?build_inputs:t list ->
  pname:string ->
  version:string ->
  opam_inputs:t AttrSet.t ->
  opam_src:Opam_src.t ->
  t

val write_file : filename:string -> t -> unit
(** [write_file] *)
