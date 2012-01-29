tileDrawChildren <- function(grob,layer,draw=TRUE,defaultlayer=10) {
    if (any(class(grob)=="gTree")&&!any(class(grob)=="axis")) {
        # Work way through all branches of the gTree,
        # recursively calling tileDrawChildren
        children <- childNames(grob)
        nkids <- length(children)
        if (nkids>0) {
            for (i in 1:nkids) {
                subgrob <- getGrob(grob,children[i])
                layer <- tileDrawChildren(subgrob,layer,draw,defaultlayer)
            }
        }
    } else {
        if (any(class(grob)=="gList")) {
            for (i in 1:length(grob)) {
                layer <- tileDrawGrob(grob[[i]],layer,draw,defaultlayer)
            }
        } else {
            if (any(class(grob)=="axis")) {
                layer <- tileDrawGrob(grob,layer,draw,defaultlayer)
            } else {
                if (any(class(grob)=="grob")) {
                    layer <- tileDrawGrob(grob,layer,draw,defaultlayer)
                }
            }
        }

    }
    invisible(layer)   
}
