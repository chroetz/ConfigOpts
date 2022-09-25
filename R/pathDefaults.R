#' @export
addPackageToPathDefaults <- function(newPath, pos = c("first", "last"), force = FALSE) {
  pos <- match.arg(pos)
  paths <- getOption("ConfigOpts.pathDefaults")
  if (!newPath %in% paths || force) {
    switch(
      pos,
      first = options(ConfigOpts.pathDefaults = c(newPath, paths)),
      last = options(ConfigOpts.pathDefaults = c(paths, newPath)))
  }
  invisible(NULL)
}
