let commands : (string * (int -> string array -> unit)) list =
  let open Opam2nix in
  [
    ("invoke", Invoke.main); ("resolve", Resolve.main); ("version", Version.main);
  ]

let usage ppf () =
  Format.fprintf ppf
    "@[<v>Usage: opam2nix <command> [args]@,\
     @[<v 2>Available commands:@,\
     %a@]@,\
     @,\
     @[<hov>Use `opam2nix <command> --help` to get@ the documentation@ for@ a \
     specific command.@]@]@."
    (Format.pp_print_list ~pp_sep:Format.pp_print_cut (fun ppf (name, _) ->
         Format.fprintf ppf "@[<h>- %s@]" name))
    commands

let () =
  if Array.length Sys.argv <= 1 then (
    usage Format.err_formatter ();
    exit 1 )
  else
    let commandName = Sys.argv.(1) in
    let command =
      List.find_opt (fun (name, _action) -> name = commandName) commands
    in

    match command with
    | Some (_name, action) -> (
        Printf.eprintf "+ %s\n" (Sys.argv |> Array.to_list |> String.concat " ");
        OpamStateConfig.update
          ~root_dir:
            (let open OpamFilename in
            let open Op in
            Dir.of_string (Filename.get_temp_dir_name ()) / "opam2nix")
          ();
        try action 1 Sys.argv
        with Arg.Help err ->
          prerr_string err;
          exit 1 )
    | None ->
        Format.eprintf "@[<v>Unknown command: %s@,@,%a@]" commandName usage ();
        exit 1
