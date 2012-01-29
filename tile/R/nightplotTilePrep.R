nightplotTilePrep <- function(trace) {

     
    ## Attach to requested axes

    # Horizontal data
    trace$attachToX <- TRUE
    trace$attachToY <- TRUE
    trace$attachToTop <- FALSE
    trace$attachToRight <- FALSE
    
    if (!is.null(trace$x)) {
        if (is.null(trace$hlower)) trace$hlower <- trace$xlower
        if (is.null(trace$hupper)) trace$hupper <- trace$xupper
    }
    
    if (is.null(trace$x)&&!is.null(trace$top)) {
        trace$x <- trace$top
        if (!is.null(trace$truetop)) {trace$truex <- trace$truetop}
        if (is.null(trace$hlower)) trace$hlower <- trace$toplower
        if (is.null(trace$hupper)) trace$hupper <- trace$topupper
        trace$attachToTop <- TRUE
        trace$attachToX <- FALSE
    }
    
    if (is.null(trace$x)&&is.null(trace$top)) {
        if (!is.null(trace$toplower)||!is.null(trace$topupper)) {
            trace$hlower <- trace$toplower
            trace$hupper <- trace$topupper
            trace$attachToTop <- TRUE
            trace$attachToX <- FALSE
        }
        if (!is.null(trace$xlower)||!is.null(trace$xupper)) {
            trace$hlower <- trace$xlower
            trace$hupper <- trace$xupper
        }

    }


    # Vertical data
       
    if (!is.null(trace$y)) {
        if (is.null(trace$lower)) trace$lower <- trace$ylower
        if (is.null(trace$upper)) trace$upper <- trace$yupper
    }

    if (is.null(trace$y)&&!is.null(trace$right)) {
        trace$y <- trace$right
        if (!is.null(trace$trueright)) {trace$truey <- trace$trueright}
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

    # Fillout true data
    if (!is.null(trace$truex)&&is.null(trace$truey))
        trace$truey <- trace$y
    if (!is.null(trace$truey)&&is.null(trace$truex))
        trace$truex <- trace$x
    if (is.null(trace$truex))
        trace$truex <- rep(NA,length(trace$x))
    if (is.null(trace$truey))
        trace$truey <- rep(NA,length(trace$y))
    
                           

  
    # Four styles
    if ((!is.null(trace$y))&&(!is.null(trace$x)))
        trace$boundtype <- "xy"
    if ((!is.null(trace$y))&&(!is.null(trace$hlower))&&(!is.null(trace$hupper)))
        trace$boundtype <- "xby"
    if ((!is.null(trace$x))&&(!is.null(trace$lower))&&(!is.null(trace$upper)))
        trace$boundtype <- "xyb"
    if ((!is.null(trace$hlower))&&(!is.null(trace$hupper))&&(!is.null(trace$lower))&&(!is.null(trace$upper)))
        trace$boundtype <- "xbyb"
    if (is.null(trace$boundtype)) {
        stop("Error in nightplot: Insufficient input \n You must provide some horizontal axis data (either x or top themselves or xlower & xupper or toplower & topupper) and some data on the vertical axis (either y or right themslves or ylower & yupper or rightlower & rightupper).")
    }

  trace
}
