#' @export
isOpts <- function(x) {
  if (!inherits(x, "Opts")) return(FALSE)
  if (!is.list(x)) return(FALSE)
  if (is.null(names(x))) return(FALSE)
  if (length(unique(names(x))) != length(x)) return(FALSE)
  return(TRUE)
}

#' @export
inheritsOptsClass <- function(x, optsClass) {
  if (!isOpts(x)) return(FALSE)
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
isListOpts <- function(x) {
  isOpts(x) && inheritsOptsClass(x, "List") && hasEntry(x, "list")
}

#' @export
hasEntry <- function(opts, entryName) {
  isTRUE(entryName %in% names(opts))
}

