#' @export
expansion <- function(...) {
  values <- c(...)
  if (!is.null(names(values)))
    values <- as.list(values)
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
  opts$list <- unlist(listOfExpansionLists, recursive = FALSE, use.names = FALSE)
  return(opts)
}

# TODO: clean up code and create test and write a vignette to explain
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
  if (sum(needsExpansion) == 0) {
    if (isTRUE(attr(x, "expand"))) return(x)
    res <- list(x)
    attr(res, "expand") <- TRUE
    return(res)
  }
  namesX <- names(x)
  names(x) <- seq_along(x)
  expansionTags <- lapply(
    x[needsExpansion],
    \(a) {
      nms <- names(a)
      if (is.null(nms)) return(rep("*", length(a)))
      return(nms)
    })
  tblValues <- tidyr::expand_grid(!!!x[needsExpansion])
  tblTags <- tidyr::expand_grid(!!!expansionTags)
  stopifnot(identical(dim(tblValues), dim(tblTags)))
  tags <- validateTagsTable(tblTags)
  tblValues <- tblValues[tags$valid,]
  tags <- tags[tags$valid,]
  res <- list()
  attr(res, "expand") <- TRUE
  for (i in seq_len(nrow(tblValues))) {
    entry <- x
    for (nm in names(tblValues)) {
      entry[[nm]] <- tblValues[[nm]][[i]]
    }
    names(entry) <- namesX
    res[[i]] <- entry
    names(res)[i] <- tags$name[i]
  }
  return(res)
}

validateTagsTable <- function(tblTags) {
  res <- list(valid = logical(0), name = character(0))
  for (i in seq_len(nrow(tblTags))) {
    v <- validateTags(tblTags[i,])
    res$valid[i] <- v$valid
    res$name[i] <- v$name
  }
  return(as.data.frame(res))
}

validateTags <- function(tags) {
  tags <- unlist(lapply(tags, strsplit, split=",", fixed=TRUE))
  tags <- grep("[[:alpha:]]+[[:digit:]]+", tags, value = TRUE)
  tagName <- sub("[[:digit:]]+", "", tags)
  tagNr <- as.numeric(sub("[[:alpha:]]+", "", tags))
  lst <- list()
  uniNms <- unique(tagName)
  for (j in seq_len(length(uniNms))) {
    lst[[j]] <- unique(tagNr[tagName == uniNms[j]])
  }
  name = ""
  valid <- all(sapply(lst, length) == 1)
  if (valid) {
    name <- paste0(uniNms, unlist(lst), collapse=",")
  }
  return(list(valid = valid, name = name))
}

