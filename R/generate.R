#' @export
hasGenerativeExpands <- function(object) {
  if (inherits(object, "expansion")) {
    if ("generate" %in% names(object)) return(TRUE)
  }
  if (!is.list(object)) return(FALSE)
  elementsGenerative <- vapply(object, hasGenerativeExpands, FUN.VALUE = logical(1))
  return(any(elementsGenerative))
}

#' @export
replaceExpandValues <- function(proto, replacement) {
  if (inherits(proto, "expansion")) {
    if (!"generate" %in% names(proto)) return(proto)
    proto$values <- generateExpansionValues(replacement, proto$generate)
    return(proto)
  }
  if (is.list(proto)) {
    stopifnot(is.list(replacement))
    if (!is.null(names(proto))) {
      stopifnot(all(names(proto) %in% names(replacement)))
      for (nm in names(proto)) {
        proto[[nm]] <- replaceExpandValues(proto[[nm]], replacement[[nm]])
      }
    } else {
      stopifnot(length(proto) == length(replacement))
      for (i in seq_along(proto)) {
        proto[[i]] <- replaceExpandValues(proto[[i]], replacement[[i]])
      }
    }
    return(proto)
  }
  return(proto)
}


generateExpansionValues <- function(value, generateInfo) {
  out <- switch(
    generateInfo$kind,
    multiply = generateExpansionValuesMultiply(value, generateInfo$value),
    add = generateExpansionValuesAdd(value, generateInfo$value),
    stop("Unknown generate kind ", kind))
  out <- out[out >= generateInfo$min & out <= generateInfo$max]
  return(out)
}

generateExpansionValuesMultiply <- function(value, factor) {
  c(value / factor, value, value * factor)
}

generateExpansionValuesAdd <- function(value, summand) {
  c(value - summand, value, value + summand)
}
