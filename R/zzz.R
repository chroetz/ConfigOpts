.onLoad <- function(libname, pkgname) {

  addPackageToPathDefaults(
    system.file("defaultOpts", package=pkgname, lib.loc=libname),
    pos = "last")

  v <- getOption("ConfigOpts.validate")
  if (is.null(v)) {
    options(ConfigOpts.validate = TRUE)
  }

  invisible(NULL)
}
