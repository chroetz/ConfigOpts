#' @export
makeOptsList <- function(optsListClass, ..., .lst = NULL) {
  optsList <- c(.lst, list(...))
  optsList <- setOptsListClass(optsList, optsListClass)
  validateOptsList(optsList)
}


setOptsListClass <- function(optsList, optsListClass) {
  oldClass(optsList) <- c(optsListClass, "List", "Opts")
  return(optsList)
}

getOptsListClass <- function(optsList) {
  validateOptsList(optsList)
  cl <- oldClass(optsList)
  len <- length(cl)
  cl[-c(len-1, len)]
}
