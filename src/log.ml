open Format

module Channel = struct
  type t = {
      name: string;
      ppf : Format.formatter 
    }

  let ppf t = t.ppf
  let name t = t.name

  let stdout name = { name; ppf = formatter_of_out_channel stdout }
  let stderr name = { name; ppf = formatter_of_out_channel stderr }
  let warning = stderr "warning"
  let error = stderr "error"
  let debug = stdout "debug"
  let info = stdout "info"
end 

type 'a t = ('a, Format.formatter, unit) format -> 'a

let log channel txt =
  let ppf = Channel.ppf channel in
  kfprintf
    (fun ppf -> fprintf ppf txt)
    ppf "[%s] " (Channel.name channel)

let error txt = log Channel.error txt

let warn txt = log Channel.warning txt

let info txt = log Channel.info txt


let _verbose = ref false

let verbose () = !_verbose

let debug txt = log Channel.debug txt

let set_verbose v =
  if v then debug "Verbose output enabled@.";
  _verbose := v


let () =
  let envvar = Util.getenv_opt "OPAM2NIX_VERBOSE" |> Option.value ~default:"" in
  set_verbose (envvar = "1" || envvar = "true")
