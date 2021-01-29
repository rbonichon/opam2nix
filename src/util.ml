let filter_map fn lst =
  lst
  |> List.fold_left
       (fun acc item ->
         match fn item with None -> acc | Some result -> result :: acc)
       []
  |> List.rev

let ends_with suffix s =
  let suffix_len = String.length suffix in
  let len = String.length s in
  len >= suffix_len && String.sub s (len - suffix_len) suffix_len = suffix

let without_leading prefix s =
  let prefix_len = String.length prefix in
  let len = String.length s in
  if len >= prefix_len && String.sub s 0 prefix_len = prefix then
    Some (String.sub s prefix_len (len - prefix_len))
  else None

let without_trailing suffix s =
  let suffix_len = String.length suffix in
  let len = String.length s in
  if len >= suffix_len && String.sub s (len - suffix_len) suffix_len = suffix
  then Some (String.sub s 0 (len - suffix_len))
  else None

let list_dirs root =
  Sys.readdir root |> Array.to_list
  |> List.filter (fun name -> Sys.is_directory (Filename.concat root name))
  |> List.sort String.compare

let rec rm_r root =
  if Sys.file_exists root then (
    Sys.readdir root |> Array.to_list
    |> List.iter (fun name ->
           let path = Filename.concat root name in
           if Sys.is_directory path then rm_r path else Unix.unlink path);
    Unix.rmdir root )

module List = struct
  include List

  let to_string fn lst = "[" ^ String.concat ", " (map fn lst) ^ "]"
end

let _verbose = ref false

let verbose () = !_verbose

let set_verbose v =
  if v then Printf.eprintf "Verbose output enabled\n";
  _verbose := v

let debug fmt =
  (if verbose () then Printf.eprintf else Printf.ifprintf stderr) fmt

let getenv_opt name = try Some (Unix.getenv name) with Not_found -> None

let () =
  let envvar = getenv_opt "OPAM2NIX_VERBOSE" |> Option.value ~default:"" in
  set_verbose (envvar = "1" || envvar = "true")

module StringMap = Map.Make (String)
