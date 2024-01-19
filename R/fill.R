fillWithDefaultOpts <- function(opts, optsClass = NULL, fillThis = TRUE, fillSub = TRUE, fillList = TRUE) {
  if ("_defaultInstance" %in% names(opts)) {
    opts <- opts[["_defaultInstance"]]
  }
  if (is.null(optsClass)) {
    optsClass <- oldClass(opts)
    optsClass <- optsClass[-length(optsClass)]
  }
  if (fillThis) {
    opts <- fillOpts(opts, getDefaultOpts(optsClass))
  }
  if (fillSub) {
    for (i in seq_along(opts)) {
      if (inherits(opts[[i]], "Opts")) {
        opts[[i]] <- fillWithDefaultOpts(opts[[i]])
      }
    }
  }
  if (fillList && inherits(opts, c("List", "Opts"))) {
    for (j in seq_along(opts$list)) {
      opts$list[[j]] <- fillWithDefaultOpts(opts$list[[j]])
    }
  }
  return(opts)
}

fillOpts <- function(opts, filler) {
  newNames <- setdiff(names(filler), names(opts))
  opts[newNames] <- unclass(filler)[newNames]
  if (all(oldClass(opts) %in% oldClass(filler))) {
    oldClass(opts) <- oldClass(filler)
  }
  return(opts)
}
