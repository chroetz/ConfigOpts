#' @export
makeSequence <- function(opts) {
  opts <- asOpts(opts, "Sequence")
  type <- getClassAt(opts, 2)
  switch(
    type,
    ByStepCount = seq(opts$range[1], opts$range[2], length.out = opts$steps),
    stop("Unknown Sequence type: ", type)
  )
}
