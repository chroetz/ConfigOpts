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
replaceExpandValues <- function(proto, replacement, keepReplacementOutOfLimit = TRUE) {
  if (inherits(proto, "expansion")) {
    if (!"generate" %in% names(proto)) return(proto)
    proto$values <- generateExpansionValues(replacement, proto$generate, keepReplacementOutOfLimit)
    return(proto)
  }
  if (is.list(proto)) {
    stopifnot(is.list(replacement))
    if (!is.null(names(proto))) {
      stopifnot(all(names(proto) %in% names(replacement)))
      for (nm in names(proto)) {
        proto[[nm]] <- replaceExpandValues(proto[[nm]], replacement[[nm]], keepReplacementOutOfLimit)
      }
    } else {
      stopifnot(length(proto) == length(replacement))
      for (i in seq_along(proto)) {
        proto[[i]] <- replaceExpandValues(proto[[i]], replacement[[i]], keepReplacementOutOfLimit)
      }
    }
    return(proto)
  }
  return(proto)
}


#' @export
isOfPrototype <- function(query, proto, ignoreLimits = TRUE, exclude = "name", excludeRecursive = FALSE) {
  if (!ignoreLimits) stop("ignoreLimits = FALSE is not implemented")
  if (length(proto) == 0 && length(query) == 0) return(TRUE)
  if (inherits(proto, "expansion")) return(TRUE)
  if (length(query) > length(proto)) return(FALSE)
  if (is.list(proto)) {
    if (!is.list(query)) return(FALSE)
    if (length(query) != length(proto)) return(FALSE)
    if (length(names(query)) > 0) {
      qNames <- setdiff(names(query), exclude)
      if (!all(qNames %in% names(proto))) return(FALSE)
      for (nm in qNames) {
        if (!isOfPrototype(query[[nm]], proto[[nm]], ignoreLimits)) return(FALSE)
      }
    } else {
      for (i in seq_along(query)) {
        isOfPrototype <- isOfPrototype(
          query[[i]],
          proto[[i]],
          ignoreLimits = ignoreLimits,
          exclude = if (excludeRecursive) exclude else character(),
          excludeRecursive = excludeRecursive)
        if (!isOfPrototype) return(FALSE)
      }
    }
    return(TRUE)
  }
  if (length(query) != length(proto)) return(FALSE)
  return(isTRUE(all.equal(query, proto)))
}





generateExpansionValues <- function(value, generateInfo, keepValueOutOfLimit) {
  out <- switch(
    generateInfo$kind,
    multiply = generateExpansionValuesMultiply(value, generateInfo$value),
    add = generateExpansionValuesAdd(value, generateInfo$value),
    stop("Unknown generate kind ", kind))
  if (keepValueOutOfLimit) {
    out <- unique(c(value, out[out >= generateInfo$min & out <= generateInfo$max]))
  } else {
    out <- unique(out[out >= generateInfo$min & out <= generateInfo$max])
  }
  return(out)
}

generateExpansionValuesMultiply <- function(value, factor) {
  c(value / factor, value, value * factor)
}

generateExpansionValuesAdd <- function(value, summand) {
  c(value - summand, value, value + summand)
}
