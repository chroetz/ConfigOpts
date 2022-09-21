#' @export
validateOpts <- function(x, filled = TRUE) {

  if (isFALSE(getOption("ConfigOpts.validate"))) return(invisible(x))

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
    entry <- x[[nm]]
    proto <- defaultOpts[[nm]]
    if (!(typeof(entry) == typeof(proto) || (is.numeric(entry) && is.numeric(proto))))
      stop("Type of entry ", nm, " does not match: is ", typeof(entry),
           ", expect: ", typeof(proto))
    if (!(length(dim(entry)) == length(dim(proto))))
      stop("Dim length of entry ", nm, " does not match: is ", length(dim(entry)),
           ", expect: ", length(dim(proto)))
    if (!(all(oldClass(proto) %in% oldClass(entry))))
      stop("Entry ", nm, " must be of class ", paste(oldClass(proto), collapse="_"),
           ", but is of class ", paste(oldClass(entry), collapse="_"))
  }

  for (nm in namesX) {
    if (isOpts(x[[nm]])) {
      validateOpts(x[[nm]], filled)
    }
  }
  if (inherits(x, "List") && "list" %in% namesX) {
    for (i in seq_along(x$list)) {
      expect <- setdiff(optsClass, "List")
      this <- oldClass(x$list[[i]])
      if (!(all(expect %in% this)))
        stop("Entries of list must be of class ", paste0(expect, collapse="_"),
             ", but entry nr ", i, "is of class ", paste0(this, collapse="_"))
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

