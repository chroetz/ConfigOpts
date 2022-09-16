#' @export
isOpts <- function(x) {
  res <- tryCatch(validateOpts(x), error = function(cond) FALSE)
  return(!isFALSE(res))
}

#' @export
inheritsOptsClass <- function(x, optsClass) {
  isOpts(x) && (inherits(x, optsClass) || length(optsClass) == 0)
}

#' @export
isListOpts <- function(x) {
  isOpts(x) && inheritsOptsClass(x, "List") && hasEntry(x, "list")
}

#' @export
hasEntry <- function(opts, entryName) {
  isTRUE(entryName %in% names(opts))
}

