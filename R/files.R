#' Write an Opts object as a json file.
#'
#' @param opts An object of S3 class Opts.
#' @param file A path to the output file or `NULL`.
#' @param dir `NULL` or a path to the output directory. In the latter case, the
#'   output file name will be created automatically from the class.
#' @param addMetaInfo Should a time stamp and package versions be added before
#'   writing the object?
#' @param digits max number of decimal digits to write for numeric values. Use
#'   `I()` to specify significant digits. Use `NA` for max precision.
#' @param warn Should warnings be signaled if, e.g., a file will be overwritten?
#'
#' @export
writeOpts <- function(opts, file = NULL, dir = NULL, addMetaInfo = TRUE, digits = NA, warn = TRUE) {
  if ((is.null(file) && is.null(dir)) || (!is.null(file) && !is.null(dir))) {
    stop("Exactly one of the arguments `dir` and `file` must be non-NULL.")
  }
  validateOpts(opts, filled=FALSE)
  if (addMetaInfo) {
    opts[["_timeStamp"]] <- date()
    pInfo <- getPackageInfo()
    opts[paste0("_", names(pInfo))] <- pInfo
  }
  if (is.null(file)) {
    file <- file.path(dir, paste(rev(oldClass(opts)), collapse="_"))
  }
  if (is.character(file) && !endsWith(file, ".json")) {
    file <- paste0(file, ".json")
  }
  opts <- putClassAttributAsListEntry(opts)
  if (warn) {
    if (file.exists(file)) warning(file, " will be overwritten")
  }
  jsonlite::write_json(opts, file, pretty = TRUE, digits = digits, auto_unbox = TRUE)
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

getPackageInfo <- function() {
  info <- list()
  info$packageNames <- unique(sapply(sys.frames(), methods::getPackageName))
  info$packageVersions <- sapply(info$packageNames, function(pn) {
    tryCatch(
      {
        format(utils::packageVersion(pn))
      },
      error = function(cond) "")
    }
  )
  return(info)
}


#' @export
getAvailableOptsClasses <- function() {
  defaultPath <- getOption("ConfigOpts.pathDefaults")
  fileNames <-
    defaultPath |>
    lapply(list.files, pattern = "^Opts_.*\\.json$") |>
    unlist() |>
    unique()
  splited <- strsplit(fileNames, "_|\\.")
  lapply(
    splited,
    \(x) rev(x[2:(length(x)-1)])
  )
}
