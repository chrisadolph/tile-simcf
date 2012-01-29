riceplotTilePrep <- function(trace) {

                        
  # Update objects
    rl.def <- list(x=NULL,
                   y=NULL,
                   tilt=NULL,
                   thick=NULL,
                   color=NULL,
                   colorbreaks=NULL,
                   mark=list(length=1,
                     var=1,
                     thickness=50,
                     center=NULL)
                   )
#
#    length=NULL,
#    thickness=NULL,
#    angle=NULL,
#    hue=NULL,
#    saturation=NULL,
#    value=NULL,
#    size=NULL,

#    lengthlimits=NULL,
#    thicknesslimits=NULL,
#    anglelimits=NULL,    

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
    

    # Create length and width from data
    
    #lengthlimit
    #widthlimit

          # Convert to plot units
    
    # Create angle from data

    #anglelimit


    trace$tiltrate <- 2
    trace$tiltcenter <- 1
    trace$length <- 0.05
    
    
    theta <- pi/2 * 1/(1+exp( trace$tiltrate*(-trace$angle + trace$tiltcenter )))


    
    # Create paint chip vertices
    a <- trace$thickness/2 * cos(theta)
    b <- trace$thickness/2 * sin(theta)
    c <- trace$length/2 * cos(theta)
    d <- trace$length/2 * sin(theta)

    #a <- trace$length/2 * cos(theta)
    #b <- trace$length/2 * sin(theta)
    #c <- trace$thickness/2 * cos(theta)
    #d <- trace$thickness/2 * sin(theta)

    x1 <- trace$x + a + d
    x2 <- trace$x - a + d
    x3 <- trace$x - a - d
    x4 <- trace$x + a - d

    y1 <- trace$y + b - c
    y2 <- trace$y - b - c
    y3 <- trace$y - b + c
    y4 <- trace$y + b + c

    trace$x <- cbind(x1,x2,x3,x4,trace$x)
    trace$y <- cbind(y1,y2,y3,y4,trace$y)

    trace
}
