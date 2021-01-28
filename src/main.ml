open Printf
open Opam2nix
open Util

let commands : (string * (int -> string array -> unit)) list =
  [
    ("invoke", Invoke.main); ("resolve", Select.main); ("version", Version.main);
  ]

let () =
  if Array.length Sys.argv <= 1 then (
    eprintf "Usage: opam2nix <command> [args]\n\nAvailable commands: %s"
      (commands |> List.map (fun (name, _) -> name) |> String.concat ", ");
    exit 1 )
  else
    let commandName = Sys.argv.(1) in
    let command =
      try
        Some (commands |> List.find (fun (name, _action) -> name = commandName))
      with Not_found -> None
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
        eprintf "Unknown command: %s\n" commandName;
        exit 1
