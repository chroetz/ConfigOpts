#' S3 class as a single string
#'
#' @param opts Any object, in particular an Opts object.
#' @return A single string. The concatenation of the S3 class elements.
#' @export
classString <- function(opts, collapse="_") {
  paste(oldClass(opts), collapse=collapse)
}

#' @export
format.Opts <- function(opts, short=FALSE, flat=FALSE) {
  if (short) {
    str <- classString(opts)
  } else {
    str <- jsonlite::toJSON(
      putClassAttributAsListEntry(opts),
      pretty=!flat,
      auto_unbox=TRUE)
  }
  return(str)
}

#' @export
print.Opts <- function(opts, short=FALSE, flat=FALSE) {
  cat(format(opts, short, flat), "\n")
}
