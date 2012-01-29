lineplotTilePrep <-
function(trace) {

    ## Attach to requested axes

    # Horizontal data
    trace$attachToX <- TRUE
    trace$attachToY <- TRUE
    trace$attachToTop <- FALSE
    trace$attachToRight <- FALSE
    
    if (!is.null(trace$x)) {
        if (is.null(trace$lower0)) trace$lower0 <- trace$xlower
        if (is.null(trace$upper0)) trace$upper0 <- trace$xupper
    }
    
    if (is.null(trace$x)&&!is.null(trace$top)) {
        trace$x <- trace$top
        if (is.null(trace$lower0)) trace$lower0 <- trace$toplower
        if (is.null(trace$upper0)) trace$upper0 <- trace$topupper
        trace$attachToTop <- TRUE
        trace$attachToX <- FALSE
    }
    
    if (is.null(trace$x)&&is.null(trace$top)) {
        if (!is.null(trace$toplower)||!is.null(trace$topupper)) {
            trace$lower0 <- trace$toplower
            trace$upper0 <- trace$topupper
            trace$attachToTop <- TRUE
            trace$attachToX <- FALSE
        }
        if (!is.null(trace$xlower)||!is.null(trace$xupper)) {
            trace$lower0 <- trace$xlower
            trace$upper0 <- trace$xupper
        }

    }


    # Vertical data
       
    if (!is.null(trace$y)) {
        if (is.null(trace$lower)) trace$lower <- trace$ylower
        if (is.null(trace$upper)) trace$upper <- trace$yupper
    }

    if (is.null(trace$y)&&!is.null(trace$right)) {
        trace$y <- trace$right
        if (is.null(trace$lower)) trace$lower <- trace$rightlower
        if (is.null(trace$upper)) trace$upper <- trace$rightupper
        trace$attachToRight <- TRUE
        trace$attachToY <- FALSE
    }

    if (is.null(trace$y)&&is.null(trace$right)) {
        if (!is.null(trace$rightlower)||!is.null(trace$rightlower)) {
            trace$lower <- trace$rightlower
            trace$upper <- trace$rightupper
            trace$attachToRight <- TRUE
            trace$attachToY <- FALSE
        }
        if (!is.null(trace$ylower)||!is.null(trace$yupper)) {
            trace$lower <- trace$ylower
            trace$upper <- trace$yupper
        }
    }
    
    # Return trace to processtraces
    trace
}

