linesTilePlot <-
function(tc=list(),...) {

    currgList <- NULL

    ct <- tc$traces[[tc$itrace]]
    n <- length(ct$x)
    
   # addArrow=addArrow,
   # angleArrow=angleArrow,
   # lengthArrow=lengthArrow,
   # endsArrow=endsArrow,
   # typeArrow=typeArrow
    
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
    #at.x.first <- min(na.omit(tc[[hatype]]$at[tc$iplot,]))
    #at.x.last <- max(na.omit(tc[[hatype]]$at[tc$iplot,]))
    #at.y.first <- min(na.omit(tc[[vatype]]$at[tc$iplot,]))
    #at.y.last <- max(na.omit(tc[[vatype]]$at[tc$iplot,]))
    
    # Push viewport
    currvp <- viewport(layout.pos.col=tc$curplotcol,
                       layout.pos.row=tc$curplotrow,
                       xscale=c(curxmin,curxmax),
                       yscale=c(curymin,curymax),
                       gp=gpar(fontsize=tc$fsize*6),                                            # Remove 6?
                       name=paste("plot",tc$iplot,"trace",tc$itrace,sep=""),
                       clip=ct$clip
                       )
    currvpTree <- vpTree(tc$mainvp,
                       vpList(currvp)
                       )
    pushViewport(currvpTree)

    ## Plot elements

    # Plot lines
    currgList <- gList(currgList,
                       linesGrob(x=ct$x,
                                 y=ct$y,
                                 gp=tileSetgpar(ct,1),
                                 default.units="native",
                                 name=paste("line",sep=""),
                                 vp=currvpTree
                                 )
                       )
     currgList <- tileSetLayer(currgList,ct$layer)
 
    upViewport(0)

    # Return result
    tc$currgList <- gList(tc$currgList,currgList)    
    tc    
}

