#' @export
asOpts <- function(x, optsClass = NULL, .fill = TRUE) {
  UseMethod("asOpts")
}

#' @export
asOpts.Opts <- function(x, optsClass = NULL, .fill = TRUE) {
  if (!is.null(optsClass) && !inheritsOptsClass(x, optsClass)) {
    x <- setOptsClass(x, optsClass)
  }
  if (.fill) {
    x <- fillWithDefaultOpts(x)
  }
  validateOpts(x, filled = .fill)
}

#' @export
asOpts.character <- function(x, optsClass = NULL, .fill = TRUE) {
  stopifnot(length(x) == 1, file.exists(x))
  readOpts(x, optsClass, .fill = .fill)
}

#' @export
asOpts.list <- function(x, optsClass = NULL, .fill = TRUE) {
  stopifnot(!is.null(optsClass))
  makeOpts(optsClass, .lst = x, .fill = .fill)
}
