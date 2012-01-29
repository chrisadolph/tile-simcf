tileDrawGrob <- function(grob,layer,draw=TRUE,defaultlayer=10) {
    if (draw) {
            # Draw if the grob is not a gTree and is current layer
        if (any(names(grob)=="layer")) {
            if (grob$layer==layer) {
                grid.draw(grob)
            }
        } else {
            if (layer==defaultlayer)
                grid.draw(grob)
        }
    } else {
        if (any(names(grob)=="layer")) {
            layer <- c(layer,grob$layer)
        } else {
            layer <- c(layer,NA)
        }
    }
    layer
}
