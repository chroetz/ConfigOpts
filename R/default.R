#' @export
checkOptsHasDefault <- function(opts, optsClass = NULL) {
  opts <- asOpts(opts, optsClass)
  optsClass <- getOptsClass(opts)
  defaultOpts <- getDefaultOpts(optsClass)
  unknownNames <- setdiff(names(opts), names(defaultOpts))
  lapply(unknownNames, \(nm) cat("Opts entry unknown: ", nm, "\n"))
  for (i in seq_along(opts)) {
    if (inherits(opts[[i]], "Opts")) {
      cat("Checking Opts ", names(opts)[i], "\n")
      checkOptsHasDefault(opts[[i]])
    } else if (inherits(opts[[i]], "OptsList")) {
      cat("Checking OptsList ", names(opts)[i], "\n")
      for (j in seq_along(opts[[i]])) {
        checkOptsHasDefault(opts[[i]][[j]])
      }
    }
  }
  return(invisible(NULL))
}


getDefaultOptsFileName <- function(optsClass) {
  paste0(paste0(optsClass, collapse = "_"), ".json")
}


getDefaultOpts <- function(optsClass) {
  defaultPath <- getOption("ConfigOpts.pathDefaults")
  if (is.null(defaultPath)) {
    stop("The path to the folder where the default Opts are located is not defined.
         Call `optsions(ConfigOpts.pathDefaults = <path>)` to set it.")
  }
  fl <- file.path(defaultPath, getDefaultOptsFileName(optsClass))
  fex <- file.exists(fl)
  if (!any(fex)) {
    stop("Cannot find any defaultOpts file:\n", paste0(fl, collapse="\n"))
  }
  readOpts(
    fl[which(fex)[1]],
    optsClass = optsClass,
    .fill = FALSE,
    removeUnderscoreEntries = FALSE)
}
