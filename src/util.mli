

(** [verbose] *)
val verbose: unit -> bool ;;


(** [without_trailing] *)
val without_trailing: string -> string -> string option ;;


(** [debug] *)
val debug: ('a, out_channel, unit) format -> 'a ;;


(** [List] *)
module List: sig
  type 'a t = 'a list = [] | (::) of 'a * 'a list
  val length : 'a t -> int
  val compare_lengths : 'a t -> 'b t -> int
  val compare_length_with : 'a t -> int -> int
  val cons : 'a -> 'a t -> 'a t
  val hd : 'a t -> 'a
  val tl : 'a t -> 'a t
  val nth : 'a t -> int -> 'a
  val nth_opt : 'a t -> int -> 'a option
  val rev : 'a t -> 'a t
  val init : int -> (int -> 'a) -> 'a t
  val append : 'a t -> 'a t -> 'a t
  val rev_append : 'a t -> 'a t -> 'a t
  val concat : 'a t t -> 'a t
  val flatten : 'a t t -> 'a t
  val iter : ('a -> unit) -> 'a t -> unit
  val iteri : (int -> 'a -> unit) -> 'a t -> unit
  val map : ('a -> 'b) -> 'a t -> 'b t
  val mapi : (int -> 'a -> 'b) -> 'a t -> 'b t
  val rev_map : ('a -> 'b) -> 'a t -> 'b t
  val filter_map : ('a -> 'b option) -> 'a t -> 'b t
  val concat_map : ('a -> 'b t) -> 'a t -> 'b t
  val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
  val fold_right : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val iter2 : ('a -> 'b -> unit) -> 'a t -> 'b t -> unit
  val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  val rev_map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  val fold_left2 : ('a -> 'b -> 'c -> 'a) -> 'a -> 'b t -> 'c t -> 'a
  val fold_right2 : ('a -> 'b -> 'c -> 'c) -> 'a t -> 'b t -> 'c -> 'c
  val for_all : ('a -> bool) -> 'a t -> bool
  val exists : ('a -> bool) -> 'a t -> bool
  val for_all2 : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool
  val exists2 : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool
  val mem : 'a -> 'a t -> bool
  val memq : 'a -> 'a t -> bool
  val find : ('a -> bool) -> 'a t -> 'a
  val find_opt : ('a -> bool) -> 'a t -> 'a option
  val find_map : ('a -> 'b option) -> 'a t -> 'b option
  val filter : ('a -> bool) -> 'a t -> 'a t
  val find_all : ('a -> bool) -> 'a t -> 'a t
  val partition : ('a -> bool) -> 'a t -> 'a t * 'a t
  val assoc : 'a -> ('a * 'b) t -> 'b
  val assoc_opt : 'a -> ('a * 'b) t -> 'b option
  val assq : 'a -> ('a * 'b) t -> 'b
  val assq_opt : 'a -> ('a * 'b) t -> 'b option
  val mem_assoc : 'a -> ('a * 'b) t -> bool
  val mem_assq : 'a -> ('a * 'b) t -> bool
  val remove_assoc : 'a -> ('a * 'b) t -> ('a * 'b) t
  val remove_assq : 'a -> ('a * 'b) t -> ('a * 'b) t
  val split : ('a * 'b) t -> 'a t * 'b t
  val combine : 'a t -> 'b t -> ('a * 'b) t
  val sort : ('a -> 'a -> int) -> 'a t -> 'a t
  val stable_sort : ('a -> 'a -> int) -> 'a t -> 'a t
  val fast_sort : ('a -> 'a -> int) -> 'a t -> 'a t
  val sort_uniq : ('a -> 'a -> int) -> 'a t -> 'a t
  val merge : ('a -> 'a -> int) -> 'a t -> 'a t -> 'a t
  val to_seq : 'a t -> 'a Seq.t
  val of_seq : 'a Seq.t -> 'a t
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
