type 'a t = ('a, Format.formatter, unit) format -> 'a

val debug : 'a t

val info : 'a t

val warn : 'a t

val error : 'a t

val verbose : unit -> bool
(** [verbose] *)

val set_verbose : bool -> unit
(** [set_verbose] *)
