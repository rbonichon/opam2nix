val prepare_package_build :
  OpamFilter.env ->
  OpamFile.OPAM.t ->
  OpamPackage.t ->
  OpamTypes.dirname ->
  exn option OpamProcess.job
(** [prepare_package_build] *)
