val verbose : unit -> bool
(** [verbose] *)

val without_trailing : string -> string -> string option
(** [without_trailing] *)

val debug : ('a, out_channel, unit) format -> 'a
(** [debug] *)

(** [List] *)
module List : sig
  include module type of List

  val to_string : ('a -> string) -> 'a t -> string
end

val getenv_opt : string -> string option
(** [getenv_opt] *)

val filter_map : ('a -> 'b option) -> 'a list -> 'b list
(** [filter_map] *)

val ends_with : string -> string -> bool
(** [ends_with] *)

val list_dirs : string -> string list
(** [list_dirs] *)

val without_leading : string -> string -> string option
(** [without_leading] *)

val rm_r : string -> unit
(** [rec] *)

module StringMap : Map.S with type key = string
(** [StringMap] *)

val _verbose : bool ref
(** [_verbose] *)
