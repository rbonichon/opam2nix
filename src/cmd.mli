

type command_failed = [ `command_failed of int option * string list ] ;;


(** [run_unit_result] *)
val run_unit_result: (Lwt_process.command -> < close : Unix.process_status Lwt.t; .. >) ->
?print:bool -> string list -> (unit, [> command_failed ]) result Lwt.t ;;


(** [run_exn] *)
val run_exn: (Lwt_process.command -> (< close : Unix.process_status Lwt.t; .. > as 'a)) ->
?print:bool -> block:('a -> 'b Lwt.t) -> string list -> 'b Lwt.t ;;

(** [exec_none] *)
val exec_none: ?stdin:Lwt_process.redirection ->
?stdout:Lwt_process.redirection ->
?stderr:Lwt_process.redirection ->
?timeout:float ->
?env:string array ->
?cwd:string -> Lwt_process.command -> Lwt_process.process_none ;;


(** [run_unit_exn] *)
val run_unit_exn: (Lwt_process.command -> < close : Unix.process_status Lwt.t; .. >) ->
?print:bool -> string list -> unit Lwt.t ;;

(** [exec_r] *)
val exec_r: ?stdin:Lwt_process.redirection ->
?stderr:Lwt_process.redirection ->
?timeout:float ->
?env:string array ->
?cwd:string -> Lwt_process.command -> Lwt_process.process_in ;;


(** [run_output_result] *)
val run_output_result: ?print:bool -> string list -> (string, [> command_failed ]) result Lwt.t ;;

(** [run_output_opt] *)
val run_output_opt: ?print:bool -> string list -> string option Lwt.t ;;


(** [run_output_exn] *)
val run_output_exn: ?print:bool -> string list -> string Lwt.t ;;


(** [file_contents] *)
val file_contents: Lwt_io.input_channel -> string Lwt.t ;;


(** [string_of_command_failed] *)
val string_of_command_failed: [< `command_failed of 'a * string list ] -> string ;;


(** [run_unit] *)
val run_unit: (Lwt_process.command -> < close : Unix.process_status Lwt.t; .. >) ->
?print:bool ->
join:(unit -> (unit, command_failed) result -> 'a) -> string list -> 'a Lwt.t ;;


(** [join_success_bool] *)
val join_success_bool: unit -> (unit, 'a) result -> bool ;;
