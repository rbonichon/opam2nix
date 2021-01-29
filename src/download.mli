type error = [ `download_failed of string ]

val string_of_error : [< `download_failed of string ] -> string
(** [string_of_error] *)

(** [Ctx] *)
module Ctx : sig
  type t

  val init : unit -> t

  val destroy : t -> unit

  val use : t -> (unit -> 'a Lwt.t) -> 'a Lwt.t
end

val fetch :
  Ctx.t -> dest:out_channel -> string -> (unit, [> error ]) result Lwt.t
(** [fetch] *)
