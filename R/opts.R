#' @export
makeOpts <- function(optsClass, ..., .lst = NULL, .fill = TRUE) {
  opts <- c(.lst, list(...))
  opts <- setOptsClass(opts, optsClass)
  if (.fill) opts <- fillWithDefaultOpts(opts)
  validateOpts(opts)
}


#' @export
`[.Opts` <- function(x, i, ...) {
  a <- attributes(x)
  attributes(x) <- NULL
  res <- x[i, ...]
  a$names <- a$names[i]
  attributes(res) <- a
  return(res)
}


setOptsClass <- function(opts, optsClass) {
  oldClass(opts) <- c(optsClass, "Opts")
  return(opts)
}


getOptsClass <- function(opts) {
  cl <- oldClass(opts)
  cl[-length(cl)]
}

