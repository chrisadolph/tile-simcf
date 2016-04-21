#' Redraw a tile graphical object to the current device
#' 
#' Draws a tile graphical object to the current device.  Because \code{tile}
#' automatically draws the object to a file, \code{tileDraw} is usually
#' superfluous.
#' 
#' This internal function draws a tile graphical object to the current device.
#' It is mainly of use of an internal function; most users should simply use
#' \code{\link{tile}} to create and save tile graphics to a file.
#' 
#' This function is provided for completeness, and is not necessary to create
#' tile graphics with detailed annotations, which can be done through careful
#' construction of traces prior to calling \code{\link{tile}}.
#' 
#' However, if users wish to add to or modify an existing tile grob, they may
#' do so by directly editing the grob returned by \code{tile}, then sending it
#' to \code{tileDraw} to place it on the current device.  Care should be taken
#' to set up the current device to have appropriate height, width, and
#' pointsize.
#' 
#' @param grob A tile or grid object; e.g., the value returned by \code{tile}
#' @param defaultlayer The layer on which to draw any sub-elements of
#' \code{grob} which lack a preassigned layer.  Layers with higher numbers are
#' drawn first, then lower numbers are drawn on top.
#' @return \code{tileDraw} is mainly called for the side-effect of drawing a
#' tile grob.  It invisibly returns the grob after drawing.
#' @author Christopher Adolph <\email{cadolph@@uw.edu}>
#' @seealso \code{\link{tile}}
#' @keywords hplot
#' @export tileDraw
tileDraw <- function(grob,defaultlayer=10) {

    if (any(class(grob)=="tile")) {
        grob <- grob$grob
    }
    
    # get all levels
    layers <- tileDrawChildren(grob,NULL,draw=FALSE,defaultlayer)
    layers[is.na(layers)] <- defaultlayer
    ulayers <- rev(sort(unique(layers)))
    
    # loop over unique levels
    for (i in 1:length(ulayers)) 
        tileDrawChildren(grob,ulayers[i],draw=TRUE,defaultlayer)
    invisible(grob)
}
