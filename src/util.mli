

(** [verbose] *)
val verbose: unit -> bool ;;


(** [without_trailing] *)
val without_trailing: string -> string -> string option ;;


(** [debug] *)
val debug: ('a, out_channel, unit) format -> 'a ;;


(** [List] *)
module List: sig
  include module type of List 
  val to_string : ('a -> string) -> 'a t -> string
end ;;


(** [getenv_opt] *)
val getenv_opt: string -> string option ;;


(** [filter_map] *)
val filter_map: ('a -> 'b option) -> 'a list -> 'b list ;;


(** [ends_with] *)
val ends_with: string -> string -> bool ;;


(** [list_dirs] *)
val list_dirs: string -> string list ;;


(** [without_leading] *)
val without_leading: string -> string -> string option ;;


(** [rec] *)
val rm_r: string -> unit ;;


(** [StringMap] *)
module StringMap : Map.S with type key = string 


(** [_verbose] *)
val _verbose: bool ref ;;
