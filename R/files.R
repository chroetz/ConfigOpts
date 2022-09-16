#' @export
writeOpts <- function(opts, file) {
  stopifnot(isOpts(opts))
  opts[["_timeStamp"]] <- date()
  opts[["_packageVersion"]] <- getPackageVersion()
  if (is.character(file) && !endsWith(file, ".json")) {
    file <- paste0(file, ".json")
  }
  opts <- putClassAttributAsListEntry(opts)
  jsonlite::write_json(opts, file, pretty = TRUE, digits = 8, auto_unbox = TRUE)
}


#' @export
readOpts <- function(file, optsClass = NULL, .fill = TRUE, removeUnderscoreEntries = TRUE) {
  opts <- readOptsBare(file, removeUnderscoreEntries)
  if (is.null(optsClass)) {
    optsClass <- getOptsClass(opts)
  }
  stopifnot(!is.null(optsClass))
  asOpts(opts, optsClass, .fill = .fill)
}

#' @export
readOptsBare <- function(file, removeUnderscoreEntries = TRUE) {
  fileContent <- jsonlite::read_json(
    file,
    simplifyVector = TRUE,
    simplifyMatrix = TRUE,
    simplifyDataFrame = FALSE)
  opts <- putListEntryClassAsAttribute(fileContent)
  if (removeUnderscoreEntries) {
    opts <- opts[!startsWith(names(opts), "_")]
  }
  return(opts)
}

putListEntryClassAsAttribute <- function(lst) {
  if ("_class" %in% names(lst)) {
    oldClass(lst) <- lst[["_class"]]
    lst[["_class"]] <- NULL
  }
  for (i in seq_along(lst)) {
    if (is.list(lst[[i]]))
      lst[[i]] <- putListEntryClassAsAttribute(lst[[i]])
  }
  return(lst)
}

putClassAttributAsListEntry <- function(obj) {
  classLst <- list()
  classLst[["_class"]] <- oldClass(obj)
  obj <- c(classLst, unclass(obj))
  for (i in seq_along(obj)) {
    if (is.list(obj[[i]]))
      obj[[i]] <- putClassAttributAsListEntry(obj[[i]])
  }
  return(obj)
}

getPackageVersion <- function() {
  format(utils::packageVersion(utils::packageName()))
}
