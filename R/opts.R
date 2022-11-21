#' @export
makeOpts <- function(optsClass, ..., .lst = NULL, .fill = TRUE) {
  opts <- c(.lst, list(...))
  opts <- setOptsClass(opts, optsClass)
  if (.fill) opts <- fillWithDefaultOpts(opts)
  if (length(opts) == 0) {
    opts <- list()
    names(opts) <- character(0)
    opts <- setOptsClass(opts, optsClass)
  }
  opts <- expandList(opts)
  if (isOpts(opts)) return(validateOpts(opts))
  for (o in opts) validateOpts(o)
  return(opts)
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

