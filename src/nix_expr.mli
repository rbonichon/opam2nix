(** [AttrSet] *)
module AttrSet: sig
  include Map.S with type key = string 
  val build : (key * 'a) list -> 'a t
  val keys : 'a t -> key list
end ;;

type string_component = [ `Lit of string | `Expr of t ]

and arg = [ `Id of string | `Default of string * t ]

and t =   [ `String of string_component list
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



(** [str] *)
val str: 'a -> [> `String of [> `Lit of 'a ] list ] 



(** [attrset] *)
val attrset: (string * 'a) list -> [> `Attrs of 'a AttrSet.t ] ;;


(** [write] *)
val write: out_channel -> t -> unit ;;
