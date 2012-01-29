tilegrobNest <- function(tc,name) {
    currgTree <- gTree(name = name,
                       children = tc$currgList)
    tc$currgList <- NULL
    tc$grob <- addGrob(tc$grob,currgTree)  
    tc
}
