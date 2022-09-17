#' @export
addDescriptionEntries <- function(file) {
  opts <- readOptsBare(file, removeUnderscoreEntries=FALSE)
  optsAttr <- attributes(opts)
  if (!"_description" %in% names(opts)) {
    if ("_class" %in% names(opts)) {
      i <- which("_class" == names(opts))[1]
    } else {
      i <- 0
    }
    dlst <- list("")
    names(dlst) <- "_description"
    opts <- append(opts, dlst, i)
  }
  z <- 0
  for (i in seq_along(opts)) {
    nm <- names(opts)[i+z]
    if (startsWith(nm, "_")) next
    descNm <- paste0("_", nm, "_description")
    if (descNm %in% names(opts)) next
    dlst <- list("")
    names(dlst) <- descNm
    opts <- append(opts, dlst, i+z)
    z <- z + 1
  }
  attributes(opts) <- optsAttr
  writeOpts(opts, file, addMetaInfo=FALSE)
}

#' @export
addDescriptionEntriesDir <- function(path) {
  files <- dir(path, all.files=TRUE)
  files <- files[endsWith(files, ".json")]
  invisible(lapply(files, addDescriptionEntries))
}

