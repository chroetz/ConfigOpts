#' @export
isOpts <- function(x) {
  res <- tryCatch(validateOpts(x), error = function(cond) FALSE)
  return(!isFALSE(res))
}

#' @export
isOptsList <- function(x) {
  res <- tryCatch(validateOptsList(x), error = function(cond) FALSE)
  return(!isFALSE(res))
}
