pointsTilePrep <-
function(trace) {

    ## Attach to requested axes

    # Horizontal data
    trace$attachToX <- TRUE
    trace$attachToY <- TRUE
    trace$attachToTop <- FALSE
    trace$attachToRight <- FALSE
    
 
    
    if (is.null(trace$x)&&!is.null(trace$top)) {
        trace$x <- trace$top
        trace$attachToTop <- TRUE
        trace$attachToX <- FALSE
    }

    if (is.null(trace$x)&&is.null(trace$top)) {
       stop("Points trace contains no horizontal coordinates (both x and top are NULL)")
    }
    
   

    # Vertical data

    if (is.null(trace$y)&&!is.null(trace$right)) {
        trace$y <- trace$right
        trace$attachToRight <- TRUE
        trace$attachToY <- FALSE
    }

    if (is.null(trace$y)&&is.null(trace$right)) {
        stop("Points trace contains no vertical coordinates (both y and right are NULL)")
    }
    
    # Return trace to processtraces
    trace
}

