#' S3 class as a single string
#'
#' @param opts Any object, in particular an Opts object.
#' @return A single string. The concatenation of the S3 class elements.
#' @export
classString <- function(opts, collapse="_") {
  paste(oldClass(opts), collapse=collapse)
}
