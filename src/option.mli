
include module type of Stdlib.Option

(** [tap] *)
val tap: ('a -> unit) -> 'a t -> 'a t ;;


(** [may] *)
val may: ('a -> unit) -> 'a t -> unit ;;

(** [or_else] *)
val or_else: 'a t -> 'a t -> 'a t ;;


(** [default_fn] *)
val default_fn: (unit -> 'a) -> 'a t -> 'a ;;

(** [or_else_fn] *)
val or_else_fn: (unit -> 'a t) -> 'a t -> 'a t ;;


(** [to_string] *)
val to_string: ('a -> string) -> 'a t -> string ;;


(** [sequence_result] *)
val sequence_result: ('a, 'b) result t -> ('a t, 'b) result ;;


(** [or_failwith] *)
val or_failwith: string -> 'a t -> 'a ;;


(** [filter] *)
val filter: ('a -> bool) -> 'a t -> 'a t ;;
