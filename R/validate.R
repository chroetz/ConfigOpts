#' @export
validateOpts <- function(x, filled = TRUE) {
  stopifnot(inherits(x, "Opts"))
  stopifnot(is.list(x))
  stopifnot(!is.null(names(x)))
  stopifnot(length(unique(names(x))) == length(x))

  classOfX <- oldClass(x)
  objectName <- paste0(rev(classOfX), collapse="_")
  optsClass <- classOfX[-length(classOfX)]
  defaultOpts <- getDefaultOpts(optsClass)

  namesX <- names(x)
  namesX <- namesX[!startsWith(namesX, "_")]
  namesDefault <- names(defaultOpts)
  namesDefault <- namesDefault[!startsWith(namesDefault, "_")]

  if (!(all(namesX %in% namesDefault))) {
    stop(
      objectName, " has unknown entires: ",
      paste0(setdiff(namesX, namesDefault), collapse=","))
  }
  if (filled && !all(namesDefault %in% namesX)) {
    stop(
      objectName, " has missing entires: ",
      paste0(setdiff(namesDefault, namesX), collapse=","))
  }

  for (nm in namesX) {
    stopifnot(doesTypeMatch(x[[nm]], defaultOpts[[nm]]))
  }

  for (nm in namesX) {
    if (isOpts(x[[nm]])) {
      validateOpts(x[[nm]], filled)
    }
  }
  if (inherits(x, "List") && "list" %in% namesX) {
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

