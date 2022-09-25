#' @export
overwriteOpts <- function(opts, lst) { # Does not overwrite opts structure, i.e., cant replace opts entries
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
