#' @export
expand <- function(...) {
  x <- list(...)
  attr(x, "expand") <- TRUE
  x
}

expandList <- function(x) {
  if (!is.list(x) || length(x) == 0) return(x)
  needsExpansion <- sapply(x, \(y) isTRUE(attr(y, "expand")))
  x[!needsExpansion] <- lapply(x[!needsExpansion], expandList)
  needsExpansion <- sapply(x, \(y) isTRUE(attr(y, "expand")))
  tbl <- tidyr::expand_grid(!!!x[needsExpansion])
  if (nrow(tbl) <= 1) return(x)
  res <- list()
  attr(res, "expand") <- TRUE
  for (i in seq_len(nrow(tbl))) {
    entry <- x
    for (nm in names(tbl)) entry[[nm]] <- tbl[[nm]][[i]]
    res[[i]] <- entry
  }
  return(res)
}
