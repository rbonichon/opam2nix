module AttrSet = struct
  include Util.StringMap

  let build pairs = List.fold_left (fun map (k, v) -> add k v map) empty pairs

  let keys map = bindings map |> List.map fst
end

type string_component = [ `Lit of string | `Expr of t ]

and arg = [ `Id of string | `Default of string * t ]

and t =
  [ `String of string_component list
  | `MultilineString of string_component list
  | `List of t list
  | `Property of t * string
  | `PropertyPath of t * string list
  | `Property_or of t * string * t
  | `Attrs of t AttrSet.t
  | `Rec_attrs of t AttrSet.t
  | `NamedArguments of arg list
  | `Function of t * t
  | `Id of string
  | `Int of int
  | `Let_bindings of t AttrSet.t * t
  | `Call of t list
  | `Template of string_component list
  | `Lit of string
  | `BinaryOp of t * string * t
  | `Null
  | `With of t * t ]

let str s = `String [ `Lit s ]

let attrset pairs = `Attrs (AttrSet.build pairs)

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

let escape_multiline_string (s : string) : string =
  apply_replacements
    [
      (Str.regexp "''", "'''");
      (Str.regexp "${", "''${");
      (Str.regexp "\t", "'\\t");
    ]
    s

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
    let two_singles = "''" in
    let string_component (escape : string -> string) c =
      match c with
      | `Lit s -> pp_print_string ppf (escape s)
      | `Expr s -> fprintf ppf "${%a}" _write s
    in
    let parens_if_needed part =
      match part with
      (* for neatness, we don't bother enclosing simple expressions in parens *)
      | `Id _ | `Int _ | `Lit _ | `String _ | `MultilineString _ | `List _
      | `Attrs _ | `Rec_attrs _ ->
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
    | `String parts ->
        put dbl;
        parts |> List.iter (string_component escape_string);
        put dbl
    | `MultilineString parts ->
        put two_singles;
        parts |> List.iter (string_component escape_multiline_string);
        put two_singles
    | `List parts ->
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
    | `Id id -> put id
    | `Int i -> put (string_of_int i)
    | `Lit str -> put str
    | `Null -> put "null"
    | `BinaryOp (a, op, b) ->
        fprintf ppf "(%a) %a (%a)" _write a pp_print_string op _write b
    | `Property (src, name) ->
        parens_if_needed src;
        property name
    | `PropertyPath (src, path) ->
        parens_if_needed src;
        path |> List.iter property
    | `Property_or (src, name, alt) ->
        fprintf ppf "%a or %a" _write (`Property (src, name)) _write alt
    | `Function (args, body) ->
        fprintf ppf "@[<v>%a:@,%a@,@]" _write args _write body
    | `Call args ->
         List.iteri (fun i arg ->
               if i > 0 then space ();
               parens_if_needed arg) args
    | `Let_bindings (vars, expr) ->
        fprintf ppf "@[<v>@[<v 2>let %a]@,in@,%a@]"
          (fun ppf ->
            AttrSet.iter (fun key v -> fprintf ppf "@,%s = %a;" key _write v))
          vars _write expr
    | `NamedArguments parts ->
        fprintf ppf "{@ @[<%d>%a@]@ }" indent_width
          (fun ppf ->
            List.iteri (fun i part ->
                if i <> 0 then fprintf ppf ",@ ";
                match part with
                | `Id arg -> pp_print_string ppf arg
                | `Default (arg, e) -> fprintf ppf "@[%s ? %a@]" arg _write e))
          parts
    | `Template parts ->
       pp_print_list
        (fun ppf -> function `Lit s -> pp_print_string ppf s | `Expr e -> _write ppf e)
        ppf 
          parts
    | `Rec_attrs a -> write_attrs ~prefix:"rec " a
    | `Attrs a -> write_attrs ~prefix:"" a
    | `With (scope, expr) ->
       fprintf ppf "with %a; %a" _write scope _write expr 
  in
  fprintf ppf "@[%a]@." _write t

let write dest (t : t) =
  let open Format in
  let ppf = formatter_of_out_channel dest in
  write ppf t
