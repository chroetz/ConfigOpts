#' @export
asOpts <- function(x, optsClass = NULL, .fill = TRUE) {
  UseMethod("asOpts")
}

#' @export
asOpts.Opts <- function(x, optsClass = NULL, .fill = TRUE) {
  if (!is.null(optsClass)) {
    x <- setOptsClass(x, optsClass)
  }
  if (.fill) {
    x <- fillWithDefaultOpts(x)
  }
  validateOpts(x)
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


#' @export
asOptsList <- function(x, optsListClass=NULL) {
  UseMethod("asOpts")
}

#' @export
asOptsList.List <- function(x, optsListClass=NULL) {
  if (!is.null(optsListClass)) {
    x <- setOptsListClass(x, optsListClass)
  }
  validateOptsList(x)
}

#' @export
asOptsList.Opts <- function(x, optsListClass=NULL) {
  makeOptsList(optsListClass, .lst = x)
}

#' @export
asOptsList.list <- function(x, optsListClass=NULL) {
  makeOptsList(optsListClass, .lst = x)
}
