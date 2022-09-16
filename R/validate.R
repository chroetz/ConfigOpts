#' @export
validateOpts <- function(x, filled = TRUE) {
  stopifnot(inherits(x, "Opts"))
  stopifnot(is.list(x))
  stopifnot(!is.null(names(x)) || length(x) == 0)
  stopifnot(length(unique(names(x))) == length(x))

  classOfX <- oldClass(x)
  objectName <- paste0(rev(classOfX), collapse="_")
  optsClass <- classOfX[-length(classOfX)]
  defaultOpts <- getDefaultOpts(optsClass)

  if (!(all(names(x) %in% names(defaultOpts)))) {
    stop(
      objectName, " has unknown entires: ",
      paste0(setdiff(names(x), names(defaultOpts)), collapse=","))
  }
  if (filled && !all(names(defaultOpts) %in% names(x))) {
    stop(
      objectName, " has missing entires: ",
      paste0(setdiff(names(defaultOpts), names(x)), collapse=","))
  }

  for (nm in names(x)) {
    stopifnot(doesTypeMatch(x[[nm]], defaultOpts[[nm]]))
  }

  for (nm in names(x)) {
    if (isOpts(x[[nm]])) {
      validateOpts(x[[nm]], filled)
    }
  }
  if (inherits(x, "List") && "list" %in% names(x)) {
    for (i in seq_along(x$list)) {
      stopifnot(all(setdiff(optsClass, "List") %in% oldClass(x$list[[i]])))
      validateOpts(x$list[[i]], filled)
    }
  }

  return(invisible(x))
}

doesTypeMatch <- function(x, proto) {
  (typeof(x) == typeof(proto) || (is.numeric(x) && is.numeric(proto))) &&
    (length(dim(x)) == length(dim(proto))) &&
    (all(oldClass(proto) %in% oldClass(x)))
}

