riceplotTilePlot <- function(tc) {
    currgList <- NULL
    ct <- tc$traces[[tc$itrace]]
    n <- nrow(ct$x)
    
    # Check attachments
    if (ct$attachToTop) {
        xmincol <- 5
        xmaxcol <- 6
        hatype <- "topaxis"
    } else {
        xmincol <- 1
        xmaxcol <- 2
        hatype <- "xaxis"
    }
    if (ct$attachToRight) {
        ymincol <- 7
        ymaxcol <- 8
        vatype <- "rightaxis"
    } else {
        ymincol <- 3
        ymaxcol <- 4
        vatype <- "yaxis"
    }   

    # Obtain needed limits
    curxmin <- tc$limits[tc$iplot,xmincol]
    curxmax <- tc$limits[tc$iplot,xmaxcol]
    curymin <- tc$limits[tc$iplot,ymincol]
    curymax <- tc$limits[tc$iplot,ymaxcol]
    
    # Push viewport
    currvp <- viewport(layout.pos.col=tc$curplotcol,
                       layout.pos.row=tc$curplotrow,
                       xscale=c(curxmin,curxmax),
                       yscale=c(curymin,curymax),
                       gp=gpar(fontsize=tc$fsize*6),                                            # Remove 6?
                       name=paste("plot",tc$iplot,"trace",tc$itrace,"clipped",sep=""),
                       clip="on"
                       )
    currvpTree <- vpTree(tc$mainvp,
                       vpList(currvp)
                       )
    pushViewport(currvpTree)

    ## Plot elements

    # Create multi-polygon vectors

    Xall <- cbind(ct$x[,1:4])
    Xall <- as.vector(t(Xall))
    
    Yall <- cbind(ct$y[,1:4])
    Yall <- as.vector(t(Yall))

    # Plot polygons
    #for (i in 1:nrow(ct$x)) {
    currgList <- gList(currgList,
                       polygonGrob(Xall,#ct$x[i,],
                                   Yall,#ct$y[i,],
                                   id.length=rep(4,n),
                                   gp=tileSetgpar(ct,1:n), # i                                     
                                   default.units="native",
                                   name=paste("trace",tc$itrace,sep=""),#"grain",i,sep=""),
                                   vp=currvpTree
                                   )                           
                       )
                                        #}

    # Create no clip viewport (not currently used)
    upViewport(0)
    currvp <- viewport(
                       layout.pos.col=tc$curplotcol,
                       layout.pos.row=tc$curplotrow,
                       xscale=c(curxmin,curxmax),
                       yscale=c(curymin,curymax),
                       gp=gpar(fontsize=tc$fsize*6),
                       name=paste("plot",tc$iplot,"trace",tc$itrace,"clipped",sep=""),
                       clip="off"
                       )

    currvpTree <- vpTree(tc$mainvp,
                       vpList(currvp)
                       )
    pushViewport(currvpTree)
    upViewport(0)


    # Return result
    tc$currgList <- gList(tc$currgList,currgList)    
    tc    
}
