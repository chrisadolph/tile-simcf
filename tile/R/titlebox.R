titlebox <- function(currvpTree,currgList) {

    currgList <- gList(currgList,
                           rectGrob(name=paste("titlebox"),
                                    vp=currvpTree
                                    )
                       )
    
    currgList
}
