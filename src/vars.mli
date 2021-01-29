val simple_lookup :
  vars:OpamTypes.variable_contents OpamVariable.Full.Map.t ->
  OpamVariable.Full.t ->
  OpamTypes.variable_contents option
(** [simple_lookup] *)

val implicit_package_var : OpamVariable.Full.t -> OpamPackage.Name.t option
(** [implicit_package_var] *)

type package_installation = {
  path : OpamTypes.dirname option;
  version : OpamPackage.Version.t option;
}

type package_implementation =
  | Provided
  | Installed of package_installation
  | Absent

type env = {
  packages : package_implementation OpamTypes.name_map;
  prefix : OpamTypes.dirname option;
  ocaml_version : OpamPackage.Version.t;
  self : OpamPackage.Name.t;
  vars : OpamTypes.variable_contents OpamVariable.Full.Map.t;
}

val path_var :
  ?scope:OpamPackage.Name.t ->
  env:env ->
  prefix:OpamTypes.dirname ->
  string ->
  OpamTypes.variable_contents option
(** [path_var] *)

val lookup : env -> OpamVariable.Full.t -> OpamTypes.variable_contents option
(** [lookup] *)
