#' @export
validateOpts <- function(x) {
  stopifnot(inherits(x, "Opts"))
  stopifnot(length(oldClass(x)) >= 2)
  stopifnot(is.list(x))
  stopifnot(!is.null(names(x)) || length(x) == 0)
  stopifnot(length(unique(names(x))) == length(x))
  # TODO: Compare with default opts. validate sub classes. Check names and entries from default.
  return(invisible(x))
}


#' @export
validateOptsList <- function(optsList) {
  stopifnot(inherits(optsList, c("List", "Opts")))
  stopifnot(is.list(optsList))
  stopifnot("list" %in% names(optsList))
  lapply(optsList$list, validateOpts)
  return(invisible(optsList))
}
