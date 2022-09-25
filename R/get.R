#' @export
getThisClass <- function(x) {
  oldClass(x)[1]
}

#' @export
getClassAt <- function(x, pos) {
  oldClass(x)[length(oldClass(x)) - pos]
}
