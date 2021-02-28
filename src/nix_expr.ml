module AttrSet = OpamStd.String.Map

(* type arg =
 *   | Formal of string
 *   | Optional of string * t 
 * 
 * and *)
type t =
  | String of string list
  (* | MultilineString of string list *)
  | List of t list
  | Property of t * string
  | PropertyPath of t * string list
  | Property_or of t * string * t
  | Attrs of t AttrSet.t
  | Rec_attrs of t AttrSet.t
  (* | NamedArguments of arg list *)
  | Function of t * t
  | Id of string
  | Int of int
  | Let_bindings of t AttrSet.t * t
  | Call of t list
  (* | Template of string list *)
  | Lit of string
  (* | BinaryOp of t * string * t *)
  | Null
  | With of t * t

type expr = t

let lets attrset e = Let_bindings (attrset, e)

let func a b = Function (a, b)

let property t name = Property (t, name)

let property_or e name alt = Property_or (e, name, alt)

let property_path e s = PropertyPath (e, s)

let _int n = Int n

let id s = Id s

let _with t1 t2 = With (t1, t2)

let null = Null

let str s = String [ s ]

let attrset = AttrSet.of_list

let attrs l = Attrs (attrset l)

let call l = Call l

let lit s = Lit s

let rec_attrs l = Rec_attrs (attrset l)

let optional name e = property_or e name null

let apply_replacements (replacements : (Str.regexp * string) list) (s : string)
    : string =
  List.fold_left
    (fun s (re, repl) -> Str.global_replace re repl s)
    s replacements

let escape_string (s : string) : string =
  apply_replacements
    [
      (Str.regexp "\\", "\\\\");
      (Str.regexp "${", "\\${");
      (Str.regexp "\"", "\\\"");
      (Str.regexp "\n", "\\n");
      (Str.regexp "\t", "\\t");
    ]
    s

(* let escape_multiline_string (s : string) : string =
 *   apply_replacements
 *     [
 *       (Str.regexp "''", "'''");
 *       (Str.regexp "${", "''${");
 *       (Str.regexp "\t", "'\\t");
 *     ]
 *     s *)

let keysafe s = Str.string_match (Str.regexp "^[-a-zA-Z_][-a-zA-Z_0-9]*$") s 0

let escape_key s = if keysafe s then s else "\"" ^ escape_string s ^ "\""

let write ppf (t : t) =
  let open Format in
  let indent_width = 2 in
  let put = pp_print_string ppf in
  let nl = pp_force_newline ppf in
  let space = pp_print_space ppf in
  let rec _write ppf (t : t) =
    let dbl = "\"" in
    (* let two_singles = "''" in *)
    let string_component escape s = pp_print_string ppf (escape s) in
    let parens_if_needed part =
      match part with
      (* for neatness, we don't bother enclosing simple expressions in parens *)
      | Id _ | Int _ | Lit _ | String _ (* | MultilineString _ *) | List _
      | Attrs _ | Rec_attrs _ ->
          _write ppf part
      | _ -> fprintf ppf "(%a)" _write part
    in
    let property name = fprintf ppf ".%s" (escape_key name) in

    let write_attrs ~prefix a =
      pp_print_cut ppf ();
      pp_open_box ppf indent_width;
      put prefix;
      put "{";
      a
      |> AttrSet.iter (fun key v ->
             (* XXX what about quoted keys? *)
             nl ();
             put (if keysafe key then key else "\"" ^ escape_string key ^ "\"");
             put " = ";
             _write ppf v;
             put ";");
      pp_close_box ppf ();
      nl ();
      put "}"
    in

    match t with
    | String parts ->
        put dbl;
        parts |> List.iter (string_component escape_string);
        put dbl
    (* | MultilineString parts ->
     *     put two_singles;
     *     parts |> List.iter (string_component escape_multiline_string);
     *     put two_singles *)
    | List parts ->
        put "[";
        pp_open_box ppf indent_width;
        space ();
        parts
        |> List.iteri (fun i part ->
               if i > 0 then space ();
               parens_if_needed part);
        space ();
        pp_close_box ppf ();
        put "]"
    | Id id -> put id
    | Int i -> put (string_of_int i)
    | Lit str -> put str
    | Null -> put "null"
    (* | BinaryOp (a, op, b) ->
     *     fprintf ppf "(%a) %a (%a)" _write a pp_print_string op _write b *)
    | Property (src, name) ->
        parens_if_needed src;
        property name
    | PropertyPath (src, path) ->
        parens_if_needed src;
        path |> List.iter property
    | Property_or (src, name, alt) ->
        fprintf ppf "%a or %a" _write (Property (src, name)) _write alt
    | Function (args, body) ->
        fprintf ppf "@[<v>%a:@,%a@,@]" _write args _write body
    | Call args ->
        List.iteri
          (fun i arg ->
            if i > 0 then space ();
            parens_if_needed arg)
          args
    | Let_bindings (vars, expr) ->
        fprintf ppf "@[<v>@[<v 2>let %a@]@,in@,%a@]"
          (fun ppf ->
            AttrSet.iter (fun key v -> fprintf ppf "@,%s = %a;" key _write v))
          vars _write expr
    (* | NamedArguments parts ->
     *     fprintf ppf "{@ @[<%d>%a@]@ }" indent_width
     *       (fun ppf ->
     *         List.iteri (fun i part ->
     *             if i <> 0 then fprintf ppf ",@ ";
     *             match part with
     *             | Formal arg -> pp_print_string ppf arg
     *             | Optional (arg, e) -> fprintf ppf "@[%s ? %a@]" arg _write e))
     *       parts *)
    (* | Template parts -> pp_print_list pp_print_string ppf parts *)
    | Rec_attrs a -> write_attrs ~prefix:"rec " a
    | Attrs a -> write_attrs ~prefix:"" a
    | With (scope, expr) -> fprintf ppf "with %a; %a" _write scope _write expr
  in
  fprintf ppf "@[%a]@." _write t

let write oc (t : t) =
  let open Format in
  let ppf = formatter_of_out_channel oc in
  Format.fprintf ppf "### This file is generated by opam2nix.@\n@.";
  write ppf t

let write_file ~filename t =
  let oc = open_out filename in
  write oc t;
  flush oc;
  close_out oc

let drv_escape =
  let unsafe_drvname_chars = Str.regexp "[^-_.0-9a-zA-Z]" in
  fun str -> Str.global_replace unsafe_drvname_chars "-" str

module Opam_src = struct
  type t = Dir of expr | File of expr

  let directory e = Dir e (* TODO: Check ? *)

  let file e = File e

  let to_expr = function Dir e | File e -> e
end

let opam_attrset ?src ?(build_inputs = []) ~pname ~version ~opam_inputs
    ~opam_src =
  attrs
    (let base =
       [
         ("pname", str (drv_escape pname));
         ("version", str (drv_escape version));
         ("src", Option.value ~default:null src);
         ("opamInputs", Attrs opam_inputs);
         ("opamSrc", Opam_src.to_expr opam_src);
       ]
     in
     match build_inputs with
     | [] -> base
     | deps -> ("build_inputs", List deps) :: base)
