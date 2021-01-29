include module type of Stdlib.Option

val tap : ('a -> unit) -> 'a t -> 'a t
(** [tap] *)

val may : ('a -> unit) -> 'a t -> unit
(** [may] *)

val or_else : 'a t -> 'a t -> 'a t
(** [or_else] *)

val default_fn : (unit -> 'a) -> 'a t -> 'a
(** [default_fn] *)

val or_else_fn : (unit -> 'a t) -> 'a t -> 'a t
(** [or_else_fn] *)

val to_string : ('a -> string) -> 'a t -> string
(** [to_string] *)

val sequence_result : ('a, 'b) result t -> ('a t, 'b) result
(** [sequence_result] *)

val or_failwith : string -> 'a t -> 'a
(** [or_failwith] *)

val filter : ('a -> bool) -> 'a t -> 'a t
(** [filter] *)
