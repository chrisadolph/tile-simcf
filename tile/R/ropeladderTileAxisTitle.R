"ropeladderTileAxisTitle" <-
function(ttype,tc=list(),...) {
  
       # Axis specific information
    if (ttype=="xaxis") {
        currdim <- tc$currrow
        widthMag <- tc$width$xaxistitle[tc$iplot]
        heightMag <- tc$height$xaxistitle[tc$iplot]
    }
    if (ttype=="yaxis") {
        currdim <- tc$currcol
        widthMag <- tc$width$yaxistitle[tc$iplot]
        heightMag <- tc$height$yaxistitle[tc$iplot]
    }
    if (ttype=="topaxis") {
        currdim <- tc$currrow
        widthMag <- tc$width$topaxistitle[tc$iplot]
        heightMag <- tc$height$topaxistitle[tc$iplot]
    }
    if (ttype=="rightaxis") {
        currdim <- tc$currcol
        widthMag <- tc$width$rightaxistitle[tc$iplot]
        heightMag <- tc$height$rightaxistitle[tc$iplot]
    }
       
    tctrl <- paste(ttype,"titleControl", sep="")

    currgList <- NULL
    
    maketitles <- 0
    
    layout.pos.col <- tc[[paste(ttype,"title",sep="")]]$layout.pos.col
    layout.pos.row <- tc[[paste(ttype,"title",sep="")]]$layout.pos.row

    if (is.null(tc$special[[tctrl]]$type)) {
        tc$special[[tctrl]]$type <- "all"
    }   
        
    if (tc$special[[tctrl]]$type=="all") {
        icur <- tc$iplot
        maketitles <- 1
        fs <- tc$special[[tctrl]]$fontsize[1]
    }

    if ((tc$special[[tctrl]]$type=="column")&&((ttype=="xaxis")||(ttype=="topaxis"))) {
        icur <- tc$currrow
        layout.pos.col <- (tc$rowtitle$add+1):(tc$overlay$ncol-1)
        fs <- tc$special[[tctrl]]$fontsize[2]
        if ((tc$newrow)) maketitles <- 1
    }
        
    if ((tc$special[[tctrl]]$type=="row")&&((ttype=="yaxis")||(ttype=="rightaxis"))) {
        icur <- tc$currrow
        fs <- tc$special[[tctrl]]$fontsize[2]
        if ((tc$newrow)&&(ttype=="yaxis")) maketitles <- 1
        if ((tc$currcol==tc$RxC[2])&&(ttype=="rightaxis")) maketitles <- 1                                                
    }
    if (tc$special[[tctrl]]$type=="first") {
        icur <- tc$iplot
        fs <- tc$special[[tctrl]]$fontsize[1]
        if (tc$iplot==1) maketitles <- 1
    }

    if (is.null(tc$special[[tctrl]][[tc$iplot]]$labels) || (length(tc$special[[tctrl]][[tc$iplot]]$labels)==0))
        maketitles <- 0

    if (maketitles) {

        
        currvp <- viewport(layout.pos.col=layout.pos.col,
                           layout.pos.row=layout.pos.row,
                           gp=gpar(fontsize=tc$special$fontsizeRopeladder[tc$iplot])
                           )
        currvpTree <- vpTree(tc$mainvp,
                             vpList(currvp)
                             )
        
        pushViewport(currvpTree)

        if (tc$titleboxes) 
          currgList <- titlebox(currvpTree,currgList)
  
        # Add text labels to plotting object       
        tc$special[[tctrl]]$labels <- as.matrix(tc$special[[tctrl]][[tc$iplot]]$labels)
        maxwidth <- maxheight <- 0
        if (length(tc$special[[tctrl]][[tc$iplot]]$labels)>0) {
            for (i in 1:length(tc$special[[tctrl]][[tc$iplot]]$labels)) {
                if (!is.na(tc$special[[tctrl]][[tc$iplot]]$labels[i])) {
                    if (ttype=="xaxis") {
                        currx <- unit(tc$special[[tctrl]][[tc$iplot]]$x[i],"npc")
                        curry <- unit(0.5,"npc")
                        rot <- 90
                    }
                    if (ttype=="yaxis") {
                        curry <- unit(tc$special[[tctrl]][[tc$iplot]]$y[i],"npc")
                        currx <- unit(0.5,"npc")
                        rot <- 0
                    }
                    if (ttype=="topaxis") {
                        currx <- unit(tc$special[[tctrl]][[tc$iplot]]$top[i],"npc")
                        curry <- unit(0.5,"npc")
                        rot <- 90
                    }
                    if (ttype=="rightaxis") {
                        curry <- unit(tc$special[[tctrl]][[tc$iplot]]$right[i],"npc")
                        currx <- unit(0.5,"npc")
                        rot <- 0
                    }

                    titleGrob <- textGrob(tc$special[[tctrl]][[tc$iplot]]$labels[i],
                                          x=currx,
                                          y=curry,
                                          rot=rot,
                                          gp=gpar(#cex=tc$special[[tctrl]][[tc$iplot]]$cex[i],
                                            col="black",#tc$special[[tctrl]][[tc$iplot]]$col[i],  # or labcol ?
                                            fontface=tc$special[[tctrl]][[tc$iplot]]$fontface[i]#,
                                                #fontsize=
                                            ),
                                          name=paste("title",i,sep=""),
                                          vp=currvpTree
                                          )

                    currgList <- gList(currgList,
                                       titleGrob
                                       )

                    if (i==1) {
                      tallestGrob <- widestGrob <- titleGrob
                    }
                    
                    newwidth <- convertWidth(grobWidth(titleGrob), 
                                             "npc", valueOnly = TRUE)
                    if (newwidth > maxwidth) {
                      maxwidth <- newwidth
                      widestGrob <- titleGrob
                    }
                    newheight <- convertHeight(grobHeight(titleGrob), 
                                               "npc", valueOnly = TRUE)
                    
                    if (newheight > maxheight) {
                      maxheight <- newheight
                      tallestGrob <- titleGrob
                    } 
                  }
              }
        
        # measure widths
                ttypenpc <- paste(ttype,"title.npc",sep="")
                ttypeunits <- paste(ttype,"title.strunit",sep="")
      
                #if ((rot==0)||(rot==180)) {
                  tc$height[[ttypenpc]][currdim] <- max(tc$height[[ttypenpc]][currdim],
                                                        convertHeight(unit(2,"char"),"npc",valueOnly=TRUE) +
                                                        #heightMag*
                                                        
                                                        convertHeight(grobHeight(tallestGrob), "npc",valueOnly=TRUE),
                                                        #newheightnpc,
                                                        na.rm=TRUE)
                
                  tc$width[[ttypenpc]][currdim] <- max(tc$width[[ttypenpc]][currdim],
                                                       convertWidth(unit(2,"char"),"npc",valueOnly=TRUE) +
                                                       #widthMag*
                                                       
                                                       convertWidth(grobWidth(widestGrob), "npc",valueOnly=TRUE),
                                                       #newwidthnpc,
                                                       na.rm=TRUE)  # Width

                upViewport(0)
                
                if (maketitles) {
                  pushViewport(tc$mainvp)
                  ttypenpcmain <- paste(ttype,"title.npcmain",sep="")
                  ttypeunits <- paste(ttype,"title.strunit",sep="")
                  
                  tc$height[[ttypenpcmain]][currdim] <- max(tc$height[[ttypenpcmain]][currdim],
                                                            convertHeight(unit(2,"char"),"npc",valueOnly=TRUE) +
                                                            #heightMag*
                                                            convertHeight(grobHeight(tallestGrob), "npc",valueOnly=TRUE),
                                                            #newheightnpcmain,
                                                            na.rm=TRUE)
                  
                  tc$width[[ttypenpcmain]][currdim] <- max(tc$width[[ttypenpcmain]][currdim],
                                                           convertWidth(unit(2,"char"),"npc",valueOnly=TRUE) +
                                                           #widthMag*
                                                           convertWidth(grobWidth(widestGrob), "npc",valueOnly=TRUE),
                                                           #newwidthnpcmain,
                                                           na.rm=TRUE)  # Width
              
                  upViewport(0)
                }
                
              }
      }
    
    tc$currgList <- currgList
    tc
  }

