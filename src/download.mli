

type error = [ `download_failed of string ] ;;


(** [string_of_error] *)
val string_of_error: [< `download_failed of string ] -> string ;;


(** [Ctx] *)
module Ctx: sig
  type t
  val init : unit -> t
  val destroy : t -> unit
  val use : t -> (unit -> 'a Lwt.t) -> 'a Lwt.t
end ;;


(** [fetch] *)
val fetch: Ctx.t -> dest:out_channel -> string -> (unit, [> error ]) result Lwt.t ;;
