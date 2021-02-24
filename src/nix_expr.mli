module AttrSet : OpamStd.MAP with type key = string
(** [AttrSet] *)

type t =
  | String of string list
  (* | MultilineString of string list *)
  | List of t list
  | Property of t * string
  | PropertyPath of t * string list
  | Property_or of t * string * t
  | Attrs of t AttrSet.t
  | Rec_attrs of t AttrSet.t
  (* | NamedArguments of arg list *)
  | Function of t * t
  | Id of string
  | Int of int
  | Let_bindings of t AttrSet.t * t
  | Call of t list
  (* | Template of string list *)
  | Lit of string
  (* | BinaryOp of t * string * t *)
  | Null
  | With of t * t

val str : string -> t
(** [str] *)

val attrset : (string * 'a) list -> 'a AttrSet.t
(** [attrset] *)

val attrs : (string * t) list -> t

val rec_attrs : (string * t) list -> t

val call : t list -> t

val lit : string -> t

val optional : string -> t -> t

type opam_src = Dir of t | File of t

val opam_attrset :
  ?src:t ->
  ?build_inputs:t list ->
  pname:string ->
  version:string ->
  opam_inputs:t AttrSet.t ->
  opam_src:opam_src ->
  t
    


val write_file : filename:string -> t -> unit
(** [write_file] *)
