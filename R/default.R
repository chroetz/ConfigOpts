#' @export
checkOptsHasDefault <- function(opts, optsClass = NULL) {
  opts <- asOpts(opts, optsClass)
  optsClass <- getOptsClass(opts)
  defaultOpts <- getDefaultOpts(optsClass)
  unknownNames <- setdiff(names(opts), names(defaultOpts))
  lapply(unknownNames, \(nm) cat("Opts entry unknown: ", nm, "\n"))
  for (i in seq_along(opts)) {
    if (isOpts(opts[[i]])) {
      cat("Checking Opts ", names(opts)[i], "\n")
      checkOptsHasDefault(opts[[i]])
    }
  }
  if (isListOpts(opts)) {
    for (i in seq_along(opts$list)) {
      cat("Checking ListOpts entry nr ", i, "\n")
      checkOptsHasDefault(opts$list[[i]])
    }
  }
  return(invisible(NULL))
}


getDefaultOptsFileName <- function(optsClass) {
  paste0("Opts_", paste0(rev(optsClass), collapse = "_"), ".json")
}


getDefaultOpts <- function(optsClass, removeUnderscoreEntries = TRUE) {
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
  readOptsBare(fl[which(fex)[1]], removeUnderscoreEntries)
}
