(* Logic for resolving opam vars.
 * Note that vars are used in multiple places:
 * Solve time: typically make use of global vars like ocaml version, platform, etc
 * Build / Install time: make use of self and dependency variables for locations and presence (typically by passing into configure scripts)
 *)

open Util
open OpamVariable
open OpamFilename.Op
module Name = OpamPackage.Name
module Version = OpamPackage.Version

let string_of_dir = OpamFilename.Dir.to_string

type package_installation = {
  path : OpamFilename.Dir.t option;
  version : OpamPackage.Version.t option;
}

type package_implementation =
  | Provided
  | Installed of package_installation
  | Absent

type env = {
  packages : package_implementation Name.Map.t;
  prefix : OpamFilename.Dir.t option;
  ocaml_version : OpamPackage.Version.t;
  self : Name.t;
  vars : OpamTypes.variable_contents OpamVariable.Full.Map.t;
}

let implicit_package_var key =
  let open OpamVariable.Full in
  match (scope key, OpamVariable.to_string (variable key)) with
  | Package pkg, "installed" | Package pkg, "enabled" -> Some pkg
  | _ -> None

(* TODO `env` could just be ocaml version *)
let path_var ~env ~prefix ~scope key =
  (* global vars reference dirs inside the prefix (swtich) location, whereas scoped vars
     * refer to the package dir within the above location *)
  let scope = ref scope in
  let lib_rel () =
    "lib/ocaml/" ^ (env.ocaml_version |> Version.to_string) ^ "/site-lib"
  in
  let rec relpath key =
    match key with
    | "lib" -> Some (lib_rel ())
    | "stublibs" | "toplevel" -> Some (Filename.concat (lib_rel ()) key)
    | "bin" | "sbin" | "man" | "libexec" | "etc" | "doc" | "share" -> Some key
    | "lib_root" | "share_root" ->
        scope := None;
        Option.bind (without_trailing "_root" key) relpath
    | _ -> None
  in
  relpath key
  |> Option.map (fun relpath ->
         let base = prefix / relpath in
         S
           (OpamFilename.Dir.to_string
              ( match !scope with
              | None -> base
              | Some pkgname -> prefix / relpath / Name.to_string pkgname )))

let package_var ~env =
  let r_false = Some (B false) in
  let r_true = Some (B true) in
  let s v = Some (S v) in
  let b v = Some (B v) in
  let installed = function Absent -> false | Provided | Installed _ -> true in
  let rec lookup ~pkg key =
    debug "(variable `%s` of package `%s`)\n" key (Name.to_string pkg);
    let impl = Name.Map.find_opt pkg env.packages |> Option.value ~default:Absent in
    match key with
    | "installed" -> b (installed impl)
    | "enable" -> s (if installed impl then "enable" else "disable")
    | "name" -> s (Name.to_string pkg)
    | _ -> (
        match impl with
        | Provided -> lookup ~pkg:(Name.of_string "ocaml") key
        | Absent ->
            None
            (* all vars aside from `installed` are undefined if the package is absent *)
        | Installed { path; version } ->

            Option.bind             path (fun path ->
                   path_var ~env ~prefix:path ~scope:(Some pkg) key)
            |> Option.or_else
                 ( match key with
                 | "dev" -> r_false
                 | "build-id" ->
                     path |> Option.map string_of_dir |> fun v -> Option.bind v s
                 (* nix paths are unique :) *)
                 (* test and doc are undocumented, but appear in the wild... *)
                 | "test" | "with-test" -> r_false
                 | "doc" | "with-doc" -> r_false
                 | "with-build" ->
                     r_true
                     (* dep used at build time. One day we'll remove these from PropagatedBuildInputs *)
                 | "build" -> (
                     (* Build acts as a filter when selecting, but as a path when building *)
                     match path with
                     | Some p -> s (string_of_dir p)
                     | None -> r_true )
                 | "post" ->
                     r_false
                     (* dep is to be installed after (or unrelated to) this package. Doesn't make much sense in nix *)
                 | "pinned" ->
                     r_false
                     (* dep is to be installed after (or unrelated to) this package. Doesn't make much sense in nix *)
                 | "version" ->
                     version
                     |> Option.map OpamPackage.Version.to_string
                     |> fun v -> Option.bind v s
                 | "hash" | "depends" ->
                     (* no reasonable way to implement these, and no use cases reported *)
                     s "NOT_IMPLEMENTED_IN_OPAM2NIX"
                 | _ -> None ) )
  in
  lookup

let simple_lookup ~vars key =
  (* lookup in env or static set of variables *)
  Full.read_from_env key
  |> Option.or_else_fn (fun () ->
         try Some (Full.Map.find key vars) with Not_found -> None)

let lookup env key =
  let open OpamVariable in
  let keystr = Full.to_string key in
  let package_var = package_var ~env in
  debug "Looking up opam var %s ..\n" keystr;
  let result =
    simple_lookup ~vars:env.vars key
    |> Option.or_else_fn (fun () ->
           let unqualified = Full.variable key |> OpamVariable.to_string in
           match Full.scope key with
           | Full.Self -> package_var ~pkg:env.self unqualified
           | Full.Package pkg -> package_var ~pkg unqualified
           | Full.Global -> (
               match keystr with
               | "prefix" | "switch" | "root" ->
                   env.prefix |> Option.map (fun v -> S (string_of_dir v))
               | "user" -> Some (S (Unix.getlogin ()))
               | "group" -> (
                   Unix.(
                     try
                       let gid = getgid () in
                       let gname = (getgrgid gid).gr_name in
                       Some (S gname)
                     with Not_found -> None) )
               | "sys-ocaml-version" ->
                   (* delegate to the installed ocaml version *)
                   package_var ~pkg:(Name.of_string "ocaml") "version"
               | _ ->
                   (* Try resolving remaining vars as scope-less directories,
                      * and then fallback to resolving a self-var. *)
                   Option.bind env.prefix (fun prefix ->
                          path_var ~env ~prefix ~scope:None unqualified)
                   |> Option.or_else_fn (fun () ->
                          package_var ~pkg:env.self unqualified) ))
  in
  debug " -> %s\n"
    (Option.to_string OpamVariable.string_of_variable_contents result);
  if Option.is_none result then
    Printf.eprintf "WARN: opam var %s not found...\n" keystr;
  result
