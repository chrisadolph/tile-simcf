ropeladderTilePrep <- function(trace) {

    # Make sure labels are character
    trace$labels <- as.character(trace$labels)
    
    ## Attach to requested axes

    # Horizontal data
    trace$attachToX <- TRUE
    trace$attachToY <- TRUE
    trace$attachToTop <- FALSE
    trace$attachToRight <- FALSE

    # Check for data in x
    if (!is.null(trace$x)||(!is.null(trace$xlower)&&!is.null(trace$xupper))) {
       
        x <- trace$x
        trace$plotrotation <- 0
        if (is.null(trace$lower)) trace$lower <- trace$xlower
        if (is.null(trace$upper)) trace$upper <- trace$xupper
        if (trace$mirrorlabels) {
        
            specialaxis <- "right"
        } else {
            specialaxis <- "y"
        }
        trace$dataaxis <- "x"
        trace$labelaxis <- specialaxis
        
    } else {
        # Check for data in y
        if (!is.null(trace$y)||(!is.null(trace$ylower)&&!is.null(trace$yupper))) {
            x <- trace$y
            trace$plotrotation <- 90
            if (is.null(trace$lower)) trace$lower <- trace$ylower
            if (is.null(trace$upper)) trace$upper <- trace$yupper
            if (trace$mirrorlabels) {
              
                specialaxis <- "top"
            } else {
                specialaxis <- "x"
            }
            trace$dataaxis <- "y"
            trace$labelaxis <- specialaxis
        } else {
            # Check for data in top
            if (!is.null(trace$top)||(!is.null(trace$toplower)&&!is.null(trace$topupper))) {
                x <- trace$top
                trace$plotrotation <- 180
                if (is.null(trace$lower)) trace$lower <- trace$toplower
                if (is.null(trace$upper)) trace$upper <- trace$topupper
                trace$attachToX <- FALSE
                trace$attachToTop <- TRUE
                if (trace$mirrorlabels) {
                    trace$attachToY <- FALSE
                    trace$attachToRight <- TRUE
                    specialaxis <- "right"
                } else {
                    specialaxis <- "y"
                }
                trace$dataaxis <- "top"
                trace$labelaxis <- specialaxis
            } else {
                # Check for data in right
                if (!is.null(trace$right)||(!is.null(trace$rightlower)&&!is.null(trace$rightupper))) {
                    x <- trace$right
                    trace$plotrotation <- 270
                    if (is.null(trace$lower)) trace$lower <- trace$rightlower
                    if (is.null(trace$upper)) trace$upper <- trace$rightupper
                    trace$attachToY <- FALSE
                    trace$attachToRight <- TRUE
                    if (trace$mirrorlabels) {
                        trace$attachToX <- FALSE
                        trace$attachToTop <- TRUE
                        specialaxis <- "top"
                    } else {
                        specialaxis <- "x"
                    }
                    trace$dataaxis <- "right"
                    trace$labelaxis <- specialaxis
                } else {
                    warning("Ropeladder trace contains no data for plotting")
                }
            }
        }
    }
 
    # Create trace
    trace$nentries <- length(x);
    #incr <- 1/(trace$nentries + 1);
    #if ((trace$plotrotation==0)||(trace$plotrotation==180)) {
    #    y <- rev(seq(incr,1-incr,by=incr))
    #}
    #if ((trace$plotrotation==90)||(trace$plotrotation==270)) {
    #    y <- seq(incr,1-incr,by=incr)
    #}

    incr <- 1/(trace$nentries + 1);
    if ((trace$plotrotation==0)||(trace$plotrotation==180)) {
        y <- (1-trace$spaceAbove-trace$spaceBelow)*(trace$nentries:1 - 0.5)/trace$nentries + trace$spaceBelow
    }
    if ((trace$plotrotation==90)||(trace$plotrotation==270)) {
       y <- (1-trace$spaceAbove-trace$spaceBelow)*(1:trace$nentries - 0.5)/trace$nentries + trace$spaceBelow
    }
    
    trace[[trace$dataaxis]] <- x
    trace[[trace$labelaxis]] <- y
 
    trace$special$axislabels$attach <- specialaxis
    trace$special$axislabels$labels <- trace$labels
    trace$special$axislabels$cex <- trace$cex
    trace$special$axislabels$fontsize <- trace$fontsize
    trace$special$axislabels$font <- trace$font
    trace$special$axislabels$fontfamily <- trace$fontfamily
    
    
   

    # Shadowbox
    if (!is.null(trace$shadowbox)) {
        if (identical(trace$shadowbox,TRUE)) {
            trace$shadowbox <- "gray"
        }
    }
 
    # Shadowrow
    if (!is.null(trace$shadowrow)) {
        if (identical(trace$shadowrow,TRUE)) {
            trace$shadowrow <- "gray"
        }
    }
   
    
    if (trace$plotrotation==0) {
 
        trace$special$height$plot <- trace$entryheight*trace$nentries               # generalize
        trace$special$limits <- c(NA,NA,0,1,NA,NA,NA,NA)
    }
    if (trace$plotrotation==180) {
    
        trace$special$height$plot <- trace$entryheight*trace$nentries               # generalize
        trace$special$limits <- c(NA,NA,0,1,NA,NA,0,1)
    }
                                     
    if (trace$plotrotation==90) {
     
        trace$special$width$plot <- trace$entryheight*trace$nentries
        trace$special$limits <- c(0,1,NA,NA,NA,NA,NA,NA)
    }
    if (trace$plotrotation==270) {
      
        trace$special$width$plot <- trace$entryheight*trace$nentries
        trace$special$limits <- c(0,1,NA,NA,0,1,NA,NA)
    }

    

  trace

}

