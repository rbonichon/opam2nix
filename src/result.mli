type ('a, 'b) t = ('a, 'b) Stdlib.result


(** [bind] *)
val bind: ('a -> ('b, 'c) result) -> ('a, 'c) result -> ('b, 'c) result ;;


(** [map] *)
val map: ('a -> 'b) -> ('a, 'c) result -> ('b, 'c) result ;;


(** [iter] *)
val iter: ('a -> unit) -> ('a, 'b) result -> unit ;;


(** [ok] *)
val ok: 'a -> ('a, 'b) result ;;


(** [get_exn] *)
val get_exn: ('a -> string) -> ('b, 'a) result -> 'b ;;
