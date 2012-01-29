"gridlines" <- function(gridlines,currvp,currgList) {
        
    gridlines.def <- list(type=NULL,  # or "x" or "y" or "xy"
                          lwd=0.15,
                          col="gray50",
                          lty="solid",
                          xat=NULL,
                          yat=NULL,
                          tat=NULL,
                          rat=NULL,
                          limits=NULL,           # length 4
                          edges=FALSE
                          )
    gridlines <- mergelist(gridlines,gridlines.def)
    
    if (length(grep("x",gridlines$type))) {
        for (j in 1:length(gridlines$xat)) {
            if (((gridlines$xat[j]>gridlines$limits[1]) && (gridlines$xat[j]<gridlines$limits[2]))||gridlines$edges) {
                currgList <- gList(currgList,
                                   linesGrob(x=unit(c(gridlines$xat[j],
                                             gridlines$xat[j]),"native"),
                                             y=unit(gridlines$limits[3:4],"native"),
                                             gp=gpar(lwd=gridlines$lwd,
                                             col=gridlines$col,
                                             lty=gridlines$lty),
                                             name=paste("xgridlines",j,sep=""),
                                             vp=currvp
                                             )
                                   )
            }
        }
    }
    
    if (length(grep("y",gridlines$type))) {
        for (j in 1:length(gridlines$yat)) {
            if (((gridlines$yat[j]>gridlines$limits[3]) && (gridlines$yat[j]<gridlines$limits[4]))||gridlines$edges) {
                currgList <- gList(currgList,
                                   linesGrob(x=unit(gridlines$limits[1:2],"native"),
                                             y=unit(c(gridlines$yat[j],
                                             gridlines$yat[j]),"native"),
                                             gp=gpar(lwd=gridlines$lwd,
                                             col=gridlines$col,
                                             lty=gridlines$lty),
                                             name=paste("ygridlines",j,sep=""),
                                             vp=currvp
                                             )
                                   )
            }
        }
    }

    if (length(grep("t",gridlines$type))) {
        for (j in 1:length(gridlines$tat)) {
            if (((gridlines$tat[j]>gridlines$limits[1]) && (gridlines$tat[j]<gridlines$limits[2]))||gridlines$edges) {
                currgList <- gList(currgList,
                                   linesGrob(x=unit(c(gridlines$tat[j],
                                             gridlines$tat[j]),"native"),
                                             y=unit(gridlines$limits[3:4],"native"),
                                             gp=gpar(lwd=gridlines$lwd,
                                             col=gridlines$col,
                                             lty=gridlines$lty),
                                             name=paste("topgridlines",j,sep=""),
                                             vp=currvp
                                             )
                                   )
            }
        }
    }
    
    if (length(grep("r",gridlines$type))) {
        for (j in 1:length(gridlines$rat)) {
            if (((gridlines$rat[j]>gridlines$limits[3]) && (gridlines$rat[j]<gridlines$limits[4]))||gridlines$edges) {
                currgList <- gList(currgList,
                                   linesGrob(x=unit(gridlines$limits[1:2],"native"),
                                             y=unit(c(gridlines$rat[j],
                                             gridlines$rat[j]),"native"),
                                             gp=gpar(lwd=gridlines$lwd,
                                             col=gridlines$col,
                                             lty=gridlines$lty),
                                             name=paste("rightgridlines",j,sep=""),
                                             vp=currvp
                                             )
                                   )
            }
        }
    }
    
    currgList
}

