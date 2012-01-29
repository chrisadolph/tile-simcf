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
