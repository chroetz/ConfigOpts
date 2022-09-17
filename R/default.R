#' @export
getDefaultOpts <- function(optsClass, removeUnderscoreEntries = TRUE, fill = FALSE) {
  defaultPath <- getOption("ConfigOpts.pathDefaults")
  if (is.null(defaultPath)) {
    stop("The path to the folder where the default Opts are located is not defined.
         Call `optsions(ConfigOpts.pathDefaults = <path>)` to set it.")
  }
  thisDefaultOpts <- getSingleDefaultOpts(
    optsClass,
    defaultPath,
    removeUnderscoreEntries = FALSE)
  if ("_defaultSubClass" %in% names(thisDefaultOpts)) {
    return(getDefaultOpts(c(thisDefaultOpts[["_defaultSubClass"]], optsClass), removeUnderscoreEntries))
  }
  optsLst <- lapply(rev(seq_along(optsClass)), function(i) {
    getSingleDefaultOpts(
      optsClass[i:length(optsClass)],
      defaultPath,
      removeUnderscoreEntries)
  })
  opts <- overwriteConsecutively(optsLst)
  if (fill) {
    opts <- fillWithDefaultOpts(opts, fillThis = FALSE)
  }
  return(opts)
}


getSingleDefaultOpts <- function(optsClass, defaultPath, removeUnderscoreEntries) {
  fl <- file.path(defaultPath, getDefaultOptsFileName(optsClass))
  fex <- file.exists(fl)
  if (!any(fex)) {
    stop("Cannot find any defaultOpts file:\n", paste0(fl, collapse="\n"))
  }
  readOptsBare(fl[which(fex)[1]], removeUnderscoreEntries)
}


getDefaultOptsFileName <- function(optsClass) {
  paste0("Opts_", paste0(rev(optsClass), collapse = "_"), ".json")
}


overwriteConsecutively <- function(optsLst) {
  opts <- optsLst[[1]]
  for (i in seq_len(length(optsLst)-1) + 1) {
    opts[names(optsLst[[i]])] <- optsLst[[i]]
  }
  oldClass(opts) <- oldClass(optsLst[[length(optsLst)]])
  return(opts)
}

