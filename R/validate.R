#' @export
validateOpts <- function(x, filled = FALSE) {
  stopifnot(inherits(x, "Opts"))
  classOfX <- oldClass(x)
  stopifnot(length(classOfX) >= 2) # Opts is abstract class
  stopifnot(is.list(x))
  stopifnot(!is.null(names(x)) || length(x) == 0)
  stopifnot(length(unique(names(x))) == length(x))

  optsClass <- classOfX[-length(classOfX)]
  defaultOpts <- getDefaultOpts(optsClass)

  stopifnot(all(names(x) %in% names(defaultOpts)))
  if (filled) {
    # TODO: does not work with Typed Opts
    stopifnot(all(
      startsWith(names(defaultOpts), "_") |
      names(defaultOpts) %in% names(x)))
  }

  for (nm in names(x)) {
    stopifnot(doesTypeMatch(x[nm], defaultOpts[nm]))
  }

  for (i in seq_along(x)) {
    if (isOpts(x[[i]])) {
      validateOpts(x[[i]])
    }
  }
  if (inherits(x, "List") && "list" %in% names(x)) {
    for (i in seq_along(x$list)) {
      # inheritsOptsClass() calls isOpts() calls validateOpts()
      stopifnot(inheritsOptsClass(x$list[[i]], setdiff(optsClass, "List")))
    }
  }

  return(invisible(x))
}

doesTypeMatch <- function(x, y) {
  (typeof(x) == typeof(y) || (is.numeric(x) && is.numeric(y))) &&
    (length(dim(x)) == length(dim(y)))
}

