#' @export
overwriteOpts <- function(opts, lst) {
  opts <- asOpts(opts)
  lst <- as.list(lst)
  for (nm in names(lst)) {
    if (is.function(lst[[nm]])) {
      opts[[nm]] <- lst[[nm]](opts[[nm]])
    } else if (isOpts(opts[[nm]])) {
      opts[[nm]] <- overwriteOpts(opts[[nm]], lst[[nm]])
    } else {
      opts[[nm]] <- lst[[nm]]
    }
  }
  return(opts)
}
