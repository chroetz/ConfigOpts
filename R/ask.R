#' @export
isOpts <- function(x, filled=FALSE) {
  res <- tryCatch(validateOpts(x, filled), error = function(cond) FALSE)
  return(!isFALSE(res))
}

#' @export
inheritsOptsClass <- function(x, optsClass) {
  isOpts(x, filled=FALSE) && (inherits(x, optsClass) || length(optsClass) == 0)
}

#' @export
isListOpts <- function(x, filled=FALSE) {
  isOpts(x, filled) && inheritsOptsClass(x, "List") && hasEntry(x, "list")
}

#' @export
hasEntry <- function(opts, entryName) {
  isTRUE(entryName %in% names(opts))
}

