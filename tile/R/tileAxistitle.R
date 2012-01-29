"tileAxistitle" <-
function(ttype,tc=list(),...) {

      # Axis specific information
  if (ttype=="xaxis") {
    currdim <- tc$currrow
    altdim <- tc$currcol
    widthMag <- tc$width$xaxistitle[tc$iplot]
    heightMag <- tc$height$xaxistitle[tc$iplot]
  }
  if (ttype=="yaxis") {
    currdim <- tc$currcol
    altdim <- tc$currrow
    widthMag <- tc$width$yaxistitle[tc$iplot]
    heightMag <- tc$height$yaxistitle[tc$iplot]
  }
  if (ttype=="topaxis") {
    currdim <- tc$currrow
    altdim <- tc$currcol
    widthMag <- tc$width$topaxistitle[tc$iplot]
    heightMag <- tc$height$topaxistitle[tc$iplot]
  }
  if (ttype=="rightaxis") {
    currdim <- tc$currcol
    altdim <- tc$currrow
    widthMag <- tc$width$rightaxistitle[tc$iplot]
    heightMag <- tc$height$rightaxistitle[tc$iplot]
  }
  
  ttype <- paste(ttype,"title",sep="")
  
  currgList <- NULL
  
  maketitles <- 0
  layout.pos.col <- tc[[ttype]]$layout.pos.col
  layout.pos.row <- tc[[ttype]]$layout.pos.row
  
  if (tc[[ttype]]$type=="all") {
    icur <- tc$iplot
    if (!suppressWarnings(is.na(tc[[ttype]]$labels[icur]))&&(!identical(tc[[ttype]]$labels[icur],""))) maketitles <- 1
    fs <- tc[[ttype]]$fontsize[1]
  }
  if ((tc[[ttype]]$type=="row")&&((ttype=="xaxistitle")||(ttype=="topaxistitle"))) {
    icur <- tc$currrow
    layout.pos.col <- (tc$rowtitle$add+1):(tc$overlay$ncol-1)
    fs <- tc[[ttype]]$fontsize[2]
    if ((tc$newrow)) maketitles <- 1
  }
  if ((tc[[ttype]]$type=="row")&&((ttype=="yaxistitle")||(ttype=="rightaxistitle"))) {
    icur <- tc$currrow
    fs <- tc[[ttype]]$fontsize[2]
    if ((tc$newrow)&&(ttype=="yaxistitle")) maketitles <- 1
    if ((tc$currcol==tc$RxC[2])&&(ttype=="rightaxistitle")) maketitles <- 1
  }
  if (tc[[ttype]]$type=="first") {
    icur <- tc$iplot
    fs <- tc[[ttype]]$fontsize[1]
    if (tc$iplot==1) maketitles <- 1
  }
  
  if (maketitles) {
    currvp <- viewport(layout.pos.col=layout.pos.col,
                       layout.pos.row=layout.pos.row,
                       gp=gpar(fontsize=fs)
                       )
    currvpTree <- vpTree(tc$mainvp,
                         vpList(currvp)
                         )
    
    pushViewport(currvpTree)
    
    if (tc$titleboxes)
      currgList <- titlebox(currvpTree,currgList)
    
    if (suppressWarnings(is.na(tc[[ttype]]$labels[icur])))
      printlabel <- ""
    else
      printlabel <- tc[[ttype]]$labels[icur]
    
    titleGrob <- textGrob(printlabel,
                          x=tc[[ttype]]$x[icur],
                          y=tc[[ttype]]$y[icur],
                          rot=tc[[ttype]]$rot[icur],
                          gp=gpar(cex=tc[[ttype]]$cex[icur],
                            col=tc[[ttype]]$col[icur],
                            fontface=tc[[ttype]]$fontface[icur]
                            ),
                          name=ttype,
                          vp=currvpTree
                          )
    currgList <- gList(currgList,
                       titleGrob
                       )
    ttypenpc <- paste(ttype,".npc",sep="")
    ttypeunits <- paste(ttype,".strunit",sep="")
    
    tc$height[[ttypenpc]][tc$currrow] <- max(tc$height[[ttypenpc]][tc$currrow],
                                             heightMag*convertHeight(grobHeight(titleGrob), "npc",valueOnly=TRUE),
                                             na.rm=TRUE)
   
    tc$width[[ttypenpc]][tc$currcol] <- max(tc$width[[ttypenpc]][tc$currcol],
                                            widthMag*convertWidth(grobWidth(titleGrob), "npc",valueOnly=TRUE),
                                            na.rm=TRUE)
  }
  
  upViewport(0)
  
  if (maketitles) {
    pushViewport(tc$mainvp)
    ttypenpcmain <- paste(ttype,".npcmain",sep="")
    ttypeunits <- paste(ttype,".strunit",sep="")
    tc$height[[ttypenpcmain]][tc$currrow] <- max(tc$height[[ttypenpcmain]][tc$currrow],
                                                 heightMag*convertHeight(grobHeight(titleGrob), "npc",valueOnly=TRUE),
                                                 na.rm=TRUE)
    
    tc$width[[ttypenpcmain]][tc$currcol] <- max(tc$width[[ttypenpcmain]][tc$currcol],
                                                widthMag*convertWidth(grobWidth(titleGrob), "npc",valueOnly=TRUE),
                                                na.rm=TRUE)
    
    upViewport(0)
  }
  
  tc$currgList <- currgList
  tc
}

