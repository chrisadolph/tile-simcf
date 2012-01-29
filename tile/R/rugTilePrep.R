rugTilePrep <-
function(trace) {

    ## Attach to requested axes

  trace$attachToX <- FALSE
  trace$attachToY <- FALSE
  trace$attachToTop <- FALSE
  trace$attachToRight <- FALSE
  if (!is.null(trace$x)) {
    trace$attachToX <- TRUE
    trace$x <- na.omit(trace$x)
  }
  if (!is.null(trace$y)) {
    trace$attachToY <- TRUE
    trace$y <- na.omit(trace$y)
  }
  if (!is.null(trace$top)) {
    trace$attachToTop <- TRUE
    trace$top <- na.omit(trace$top)
  }
  if (!is.null(trace$right)) {
    trace$attachToRight <- TRUE
    trace$right <- na.omit(trace$right)
  }
  if (sum(trace$attachToX,trace$attachToY,trace$attachToTop,trace$attachToRight)!=1)
    stop("Rug trace must contain data for exactly one input:  x, y, top, or right.")

    # Return trace to processtraces
    trace
}

