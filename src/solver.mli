type external_constraints = {
  ocaml_version : OpamTypes.version;
  repos : Repo.t list;
}

type error = Unsupported_archive of string | Unavailable of string

type universe = {
  lookup_var :
    OpamPackage.t ->
    OpamTypes.full_variable ->
    OpamVariable.variable_contents option;
  repos : Repo.t list;
  mutable packages : (Repo.loaded_package, error) result OpamTypes.package_map;
  constrained_versions : OpamTypes.version OpamTypes.name_map;
}

val build_universe :
  external_constraints:external_constraints ->
  base_packages:OpamPackage.Name.t list ->
  constrained_versions:(OpamPackage.Name.t * OpamTypes.version) list ->
  direct_definitions:Repo.direct_package list ->
  unit ->
  universe
(** [build_universe] *)

include Opam_0install.S.SOLVER with type t := universe
