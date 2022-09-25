#' @export
isOpts <- function(x, filled=FALSE) {
  res <- tryCatch(validateOpts(x, filled, force=TRUE), error = function(cond) FALSE)
  return(!isFALSE(res))
}

#' @export
inheritsOptsClass <- function(x, optsClass) {
  if (!isOpts(x, filled=FALSE)) return(FALSE)
  if (length(optsClass) == 0) return(TRUE)
  xOptsClass <- oldClass(x)
  xOptsClass <- xOptsClass[-length(xOptsClass)]
  if (length(optsClass) > length(xOptsClass)) return(FALSE)
  paddedOptsCalls <- c(rep(NA, length(xOptsClass) - length(optsClass)), optsClass)
  eqTest <- xOptsClass == paddedOptsCalls
  eqTest[is.na(eqTest)] <- TRUE
  return(all(eqTest))
}

#' @export
isListOpts <- function(x, filled=FALSE) {
  isOpts(x, filled) && inheritsOptsClass(x, "List") && hasEntry(x, "list")
}

#' @export
hasEntry <- function(opts, entryName) {
  isTRUE(entryName %in% names(opts))
}

