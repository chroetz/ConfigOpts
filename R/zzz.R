.onLoad <- function(libname, pkgname) {
  paths <- getOption("ConfigOpts.pathDefaults")
  thisPath <- system.file("defaultOpts", package=pkgname, lib.loc=libname)
  if (!thisPath %in% paths) {
    options(ConfigOpts.pathDefaults = c(paths, thisPath))
  }
  invisible()
}
