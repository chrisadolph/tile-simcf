tileSetLayer <- function(gl,layer=NA,defaultlayer=10) {
    # Check to see if a layer has been set by user
    if (is.na(layer)) {
        layer <- defaultlayer
    }
    
    # Get last element of gList
    i <- length(gl)

    # Modify this grob's layer
    gl[[i]]$layer <- layer
    gl
}
