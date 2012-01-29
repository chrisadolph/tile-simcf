"tileTitle" <-
function(ttype,tc=list(),...) {
  
  # If style is NULL, improvise
  # else if style is "allplots", do below
  # else if style is "eachrow", set viewport to every column between 1+rtitles and end-1
  # else if style is "firstplot", do below only if iplot==1
  # else if style is "firstrow", set viewport to every column between 1+rtitles and end-1, only if iplot==1

# need last row option for undertitles

  currgList <- NULL
  
  maketitles <- 0
  layout.pos.col <- tc[[ttype]]$layout.pos.col
  layout.pos.row <- tc[[ttype]]$layout.pos.row

  if (ttype=="undertitle") {
    widthMag <- tc$width$undertitle[tc$iplot]
    heightMag <- tc$height$undertitle[tc$iplot]
  }
  if (ttype=="plottitle") {
    widthMag <- tc$width$plottitle[tc$iplot]
    heightMag <- tc$height$plottitle[tc$iplot]
  }
  if (ttype=="columntitle") {
    widthMag <- tc$width$columntitle[tc$iplot]
    heightMag <- tc$height$columntitle[tc$iplot]
  }
  if (ttype=="rowtitle") {
    widthMag <- tc$width$rowtitle[tc$iplot]
    heightMag <- tc$height$rowtitle[tc$iplot]
  }
  if (ttype=="maintitle") {
    widthMag <- tc$width$maintitle[tc$iplot]
    heightMag <- tc$height$maintitle[tc$iplot]
  }

    if (is.null(tc[[ttype]]$type)) {

        if (ttype=="undertitle") {                         # Fix
            if (length(tc[[ttype]]$labels)==1)
                tc[[ttype]]$type <- "last"
            if (length(tc[[ttype]]$labels)==tc$nplots)
                tc[[ttype]]$type <- "all"
            if (length(tc[[ttype]]$labels)==tc$RxC[1])
                tc[[ttype]]$type <- "lastrow"
            if (is.null(tc[[ttype]]$type))
                tc[[ttype]]$type <- "all"
        }
        if (ttype=="plottitle") {                          # Fix
            if (length(tc[[ttype]]$labels)==1)
                tc[[ttype]]$type <- "first"
            if (length(tc[[ttype]]$labels)==tc$nplots)
                tc[[ttype]]$type <- "all"
            if (length(tc[[ttype]]$labels)==tc$RxC[1])
                tc[[ttype]]$type <- "firstrow"
            if (is.null(tc[[ttype]]$type))
                tc[[ttype]]$type <- "all"

        }
        if (ttype=="columntitle") {
           # if (length(tc[[ttype]]$labels)==1)
          tc[[ttype]]$type <- "firstrow"
        }
        if (ttype=="rowtitle") {
            #if (length(tc[[ttype]]$labels)==1)
            
          tc[[ttype]]$type <- "firstcolumn"
            }
        if (ttype=="maintitle") {
            layout.pos.row <- 3
            layout.pos.col <- 3
            if (is.null(tc[[ttype]]$type))
                tc[[ttype]]$type <- "firstrowmerge"#"firstrow"
        }
    }   
    
    if (is.null(tc[[ttype]]$type)) {
        tc[[ttype]]$type <- "all"
    }
    
    if (tc[[ttype]]$type=="all") {
        icur <- tc$iplot
        maketitles <- TRUE
        fs <- tc[[ttype]]$fontsize[1]
    }
    
    if (tc[[ttype]]$type=="row") {
        icur <- tc$currrow
        layout.pos.col <- (tc$rowtitle$add+1):(tc$overlay$ncol-1)
        fs <- tc[[ttype]]$title$fontsize[2]
        if ((tc$newrow)) maketitles <- 1
    }
    
    if (tc[[ttype]]$type=="column") {
        icur <- tc$currcol
        layout.pos.col <- (tc$maintitle$add+tc$columntitle$add+1):(tc$overlay$nrow-1)
        fs <- tc[[ttype]]$title$fontsize[2]
        if ((tc$newcol)) maketitles <- 1
    }
    
    if (tc[[ttype]]$type=="first") {
        icur <- tc$iplot
        fs <- tc[[ttype]]$fontsize[1]
        if (tc$iplot==1) maketitles <- 1
    }
    
    if (tc[[ttype]]$type=="firstrow") {
        icur <- tc$currcol
        layout.pos.col <- tc$curplotcol
        fs <- tc[[ttype]]$fontsize[2]
        
        if (tc$currrow==1) maketitles <- 1
    }
    if (tc[[ttype]]$type=="lastrow") {
        icur <- tc$currcol
        layout.pos.col <- tc$curplotcol
        fs <- tc[[ttype]]$fontsize[2]
        if (tc$currrow==tc$RxC[1]) maketitles <- 1
    }
    
    if (tc[[ttype]]$type=="firstcolumn") {
        icur <- tc$currrow
        layout.pos.row <- tc$curplotrow
        fs <- tc[[ttype]]$fontsize[2]
        if (tc$currcol==1) maketitles <- 1
    }
    if (tc[[ttype]]$type=="lastcolumnm") {
        icur <- tc$currrow
        layout.pos.row <- tc$curplotrow
        fs <- tc[[ttype]]$fontsize[2]
        if (tc$currcol==tc$RxC[2]) maketitles <- 1
    }
    
    
    if (tc[[ttype]]$type=="firstrowmerge") {
        icur <- 1#tc$iplot
        layout.pos.col <- (tc$rowtitle$add+1):(tc$overlay$ncol-1)
        fs <- tc[[ttype]]$fontsize[2]        
        if (tc$iplot==1) maketitles <- 1
    }
    if (tc[[ttype]]$type=="lastrowmerge") {
        icur <- 1#tc$iplot
        layout.pos.col <- (tc$rowtitle$add+1):(tc$overlay$ncol-1)
        fs <- tc[[ttype]]$fontsize[2]
        if (tc$iplot==tc$nplot) maketitles <- 1
    }
    
    if (tc[[ttype]]$type=="firstcolumnmerge") {
        icur <- 1#tc$iplot
        layout.pos.row <- (tc$maintitle$add+tc$columntitle$add+1):(tc$overlay$nrow-1)
        fs <- tc[[ttype]]$fontsize[2]
        if (tc$iplot==1) maketitles <- 1
    }
    if (tc[[ttype]]$type=="lastcolumnmerge") {
        icur <- 1#tc$iplot
        layout.pos.row <- (tc$maintitle$add+tc$rowtitle$add+1):(tc$overlay$nrow-1)
        fs <- tc[[ttype]]$fontsize[2]
        if (tc$iplot==tc$nplot) maketitles <- 1
    }
    
    if (maketitles) {
      
        currvp <- viewport(layout.pos.col=layout.pos.col,
                           layout.pos.row=layout.pos.row,
                           gp=gpar(fontsize=tc[[ttype]]$fontsize[1]),
                           name=paste(ttype,tc$iplot,sep="")
                           )
        
        currvpTree <- vpTree(tc$mainvp,
                             vpList(currvp)
                             )
        
        pushViewport(currvpTree)
      
        if (tc$titleboxes)
          currgList <- titlebox(currvpTree,currgList)

        specifictitle <- paste("labels",icur,sep="")#
        if (is.null(tc[[ttype]][[specifictitle]])) {
          if (is.na(tc[[ttype]]$labels[icur]))
            printlabel <- ""
          else
            printlabel <- tc[[ttype]]$labels[icur]
        } else {
           if (is.na(tc[[ttype]][[specifictitle]][1]))
            printlabel <- ""
          else
            printlabel <- tc[[ttype]][[specifictitle]][1]
        }

        titleGrob <- textGrob(printlabel,
                              x=tc[[ttype]]$x[icur],
                              y=tc[[ttype]]$y[icur],
                              rot=tc[[ttype]]$rot[icur],
                              gp=gpar(cex=tc[[ttype]]$cex[icur],
                                col=tc[[ttype]]$col[icur],
                                fontface=tc[[ttype]]$fontface[icur],
                                fontsize=tc[[ttype]]$fontsize[1]
                                ),
                              name=ttype,
                              vp=currvpTree
                              )
        currgList <- gList(currgList,
                           titleGrob
                           )
      
        ttypenpc <- paste(ttype,"npc",sep=".")
        ttypeunits <- paste(ttype,"strunit",sep=".")

          tc$height[[ttypenpc]][tc$currrow] <-  max(tc$height[[ttypenpc]][tc$currrow],
                                                  heightMag*convertHeight(grobHeight(titleGrob), "npc",valueOnly=TRUE),
                                                  na.rm=TRUE)

          tc$width[[ttypenpc]][tc$currcol] <-  max(tc$width[[ttypenpc]][tc$currcol],
                                                 widthMag*convertWidth(grobWidth(titleGrob), "npc",valueOnly=TRUE),
                                                 na.rm=TRUE)
    }
    upViewport(0) 

    if (maketitles) {
        pushViewport(tc$mainvp)
        ttypenpcmain <- paste(ttype,"npcmain",sep=".")  
        ttypeunits <- paste(ttype,"strunit",sep=".")        
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

