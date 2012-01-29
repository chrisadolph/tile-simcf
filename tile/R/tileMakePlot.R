tileMakePlot <- function(tc) {
  
    tc$currgList <- NULL

    # Measure borders
    upViewport(0)
    pushViewport(tc$mainvp)

    tc$height$spacer.units <- maxUnit(tc$height$spacer.units, units="char")
    tc$height$spacer.npcmain <- convertHeight(tc$height$spacer.units,"npc",valueOnly=TRUE)
    tc$height$topborder.npcmain <- convertHeight(tc$height$topborder.units,"npc",valueOnly=TRUE)
    tc$height$bottomborder.npcmain <- convertHeight(tc$height$bottomborder.units,"npc",valueOnly=TRUE)
    tc$width$spacer.units <- maxUnit(tc$width$spacer.units, units="char")
    tc$width$spacer.npcmain <- convertWidth(tc$width$spacer.units,"npc",valueOnly=TRUE)
    tc$width$leftborder.npcmain <- convertWidth(tc$width$leftborder.units,"npc",valueOnly=TRUE)
    tc$width$rightborder.npcmain <- convertWidth(tc$width$rightborder.units,"npc",valueOnly=TRUE)
    upViewport(0)

    # Add X-axis
    if (tc$xaxis$add[tc$iplot]) {
        tc <- tileAxis("xaxis",tc)
    }
    
    # Add Y-axis
    if (tc$yaxis$add[tc$iplot]) {
        tc <- tileAxis("yaxis",tc) 
    }
    
    # Add Top-axis
    if (tc$topaxis$add[tc$iplot]) {
        tc <- tileAxis("topaxis",tc)
    }

    # Add Right-axis
    if (tc$rightaxis$add[tc$iplot]) {
        tc <- tileAxis("rightaxis",tc)
    }

    # Add Frame
    if (tc$frame[tc$iplot]&&tc$useplot[tc$iplot]) {
        currvp <- viewport(layout.pos.col=tc$curplotcol,
                           layout.pos.row=tc$curplotrow,
                           xscale=c(0,1),
                           yscale=c(0,1),
                           gp=gpar(fontsize=tc$fsize*6),                                            # Remove 6?
                           name=paste("plot",tc$iplot,"frame",sep=""),
                           clip="off"
                           )
        currvpTree <- vpTree(tc$mainvp,
                             vpList(currvp)
                             )
        pushViewport(currvpTree)       
        frame.grob <- rectGrob(name=paste("frame"),
                               vp=currvpTree
                               )        
        tc$currgList <- gList(tc$currgList,frame.grob)
        tc$currgList <- tileSetLayer(tc$currgList,(tc$defaults$layer-8))
        upViewport(0)
    }

    tc
}
