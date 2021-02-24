type ('a, 'b) t = ('a, 'b) Stdlib.result

val bind : ('a -> ('b, 'c) result) -> ('a, 'c) result -> ('b, 'c) result
(** [bind] *)

val map : ('a -> 'b) -> ('a, 'c) result -> ('b, 'c) result
(** [map] *)

val iter : ('a -> unit) -> ('a, 'b) result -> unit
(** [iter] *)

val ok : 'a -> ('a, 'b) result
(** [ok] *)

val get_exn : ('a -> string) -> ('b, 'a) result -> 'b
(** [get_exn] *)
