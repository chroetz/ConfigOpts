#' @export
expansion <- function(...) {
  values <- c(...)
  lst <- list(values = values)
  class(lst) <- "expansion"
  lst
}

#' @export
expansionList <- function(...) {
  values <- list(...)
  lst <- list(values = values)
  class(lst) <- "expansion"
  lst
}

#' @export
expandList <- function(opts) {
  stopifnot(inheritsOptsClass(opts, "List"))
  listOfExpansionLists <- lapply(opts$list, .expandList)
  opts$list <- unlist(listOfExpansionLists, recursive = FALSE)
  return(opts)
}

.expandList <- function(x) {
  if (!is.list(x) || length(x) == 0) return(x)
  if (inherits(x, "expansion")) {
    x <- x$values
    if (!is.list(x)) {
      if (is.array(x)) {
        x <- apply(x, 1, force, simplify = FALSE)
      } else {
        x <- as.list(x)
      }
    }
    attr(x, "expand") <- TRUE
  }
  needsExpansion <- sapply(x, \(y) isTRUE(attr(y, "expand")))
  x[!needsExpansion] <- lapply(x[!needsExpansion], .expandList)
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
