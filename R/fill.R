fillWithDefaultOpts <- function(opts, optsClass = NULL, fillThis = TRUE, fillSub = TRUE, fillList = TRUE) {
  opts <- asOpts(opts, optsClass, .fill = FALSE)
  optsClass <- getOptsClass(opts)
  if (fillThis) {
    opts <- fillOpts(opts, getDefaultOpts(optsClass))
  }
  if (fillSub) {
    for (i in seq_along(opts)) {
      if (isOpts(opts[[i]])) {
        opts[[i]] <- fillWithDefaultOpts(opts[[i]])
      }
    }
  }
  if (fillList && isListOpts(opts)) {
    for (j in seq_along(opts$list)) {
      opts$list[[j]] <- fillWithDefaultOpts(opts$list[[j]])
    }
  }
  return(opts)
}

fillOpts <- function(opts, filler) {
  if (!"name" %in% names(opts) && "name" %in% names(filler)) {
    opts$name <- filler$name
  }
  allEntries <- names(filler)
  allOptsEntries <- allEntries[!startsWith(allEntries, "_")]
  if ("name" %in% names(opts)) {
    optsName <- opts$name
    suffix <- "_used_by"
    usedByEntries <- allEntries[startsWith(allEntries, "_") & endsWith(allEntries, suffix)]
    if (length(usedByEntries) == 0) {
      fillableEntries <- allOptsEntries
    } else {
      usedByOptsEntries <- substr(usedByEntries, start = 2, stop = nchar(usedByEntries) - nchar(suffix))
      unrestrictedEntries <- allOptsEntries[!allOptsEntries %in% usedByOptsEntries]
      restrictedEntries <- allOptsEntries[allOptsEntries %in% usedByOptsEntries]
      used <- sapply(restrictedEntries, \(nm) optsName %in% filler[[paste0("_", nm, suffix)]])
      fillableEntries <- c(unrestrictedEntries, restrictedEntries[used])
    }
  } else {
    fillableEntries <- allOptsEntries
  }

  for (nm in fillableEntries) {
    if (nm %in% names(opts)) next
    opts[[nm]] <- filler[[nm]]
  }
  return(opts)
}
