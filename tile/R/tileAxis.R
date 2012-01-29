"tileAxis" <- function(atype,tc=list()) {
  
    currgList <- NULL
    
    # Axis specific information
    if (atype=="xaxis") {
        xmincol <- 1
        xmaxcol <- 2
        altmincol <- 5
        altmaxcol <- 6
        atypef <- "xaxis"
        main <- TRUE
        
        if (!is.na(tc$limits[tc$iplot,xmincol])&&!is.na(tc$limits[tc$iplot,xmaxcol])) {
            curxmin <- tc$limits[tc$iplot,xmincol]
            curxmax <- tc$limits[tc$iplot,xmaxcol]
        } else {            
            if (!is.na(tc$limits[tc$iplot,altmincol])&&!is.na(tc$limits[tc$iplot,altmaxcol])) {
                curxmin <- tc$limits[tc$iplot,altmincol]
                curxmax <- tc$limits[tc$iplot,altmaxcol]
            } else {
                curxmin <- 0
                curxmax <- 1
            }
        }        
        curymin <- 0
        curymax <- 1
        limits <- c(curxmin,curxmax)
        currdim <- tc$currrow
        altdim <- tc$currcol
    }
    if (atype=="yaxis") {
        xmincol <- 3
        xmaxcol <- 4
        altmincol <- 7
        altmaxcol <- 8
        atypef <- "yaxis"
        main <- TRUE
        curxmin <- 0
        curxmax <- 1

        if (!is.na(tc$limits[tc$iplot,xmincol])&&!is.na(tc$limits[tc$iplot,xmaxcol])) {
            curymin <- tc$limits[tc$iplot,xmincol]
            curymax <- tc$limits[tc$iplot,xmaxcol]
        } else {
            if (!is.na(tc$limits[tc$iplot,altmincol])&&!is.na(tc$limits[tc$iplot,altmaxcol])) {
                curymin <- tc$limits[tc$iplot,altmincol]
                curymax <- tc$limits[tc$iplot,altmaxcol]
            } else {
                curymin <- 0
                curymax <- 1
            }
        }
        limits <- c(curymin,curymax)
        currdim <- tc$currcol
        altdim <- tc$currrow
    }
    if (atype=="topaxis") {
        xmincol <- 5
        xmaxcol <- 6
        altmincol <- 1
        altmaxcol <- 2
        atypef <- "xaxis"
        main <- FALSE
        
        if (!is.na(tc$limits[tc$iplot,xmincol])&&!is.na(tc$limits[tc$iplot,xmaxcol])) {
            curxmin <- tc$limits[tc$iplot,xmincol]
            curxmax <- tc$limits[tc$iplot,xmaxcol]
        } else {
            if (!is.na(tc$limits[tc$iplot,altmincol])&&!is.na(tc$limits[tc$iplot,altmaxcol])) {
                curxmin <- tc$limits[tc$iplot,altmincol]
                curxmax <- tc$limits[tc$iplot,altmaxcol]
            } else {
                curxmin <- 0
                curxmax <- 1
            }
        }
        curymin <- 0
        curymax <- 1
        limits <- c(curxmin,curxmax)
        currdim <- tc$currrow
        altdim <- tc$currcol
    }
    if (atype=="rightaxis") {
        xmincol <- 7
        xmaxcol <- 8
        altmincol <- 3
        altmaxcol <- 4
        atypef <- "yaxis"
        main <- FALSE
        curxmin <- 0
        curxmax <- 1
        
        if (!is.na(tc$limits[tc$iplot,xmincol])&&!is.na(tc$limits[tc$iplot,xmaxcol])) {
            curymin <- tc$limits[tc$iplot,xmincol]
            curymax <- tc$limits[tc$iplot,xmaxcol]
        } else {
            if (!is.na(tc$limits[tc$iplot,altmincol])&&!is.na(tc$limits[tc$iplot,altmaxcol])) {
                curymin <- tc$limits[tc$iplot,altmincol]
                curymax <- tc$limits[tc$iplot,altmaxcol]
            } else {
                curymin <- 0
                curymax <- 1
            }
        }
        limits <- c(curymin,curymax)
        currdim <- tc$currcol
        altdim <- tc$currrow
    }

    # Check for rug
    add.rug <- tc[[atype]]$rug[tc$iplot]
    
    # Push viewport
    currvp <- viewport(layout.pos.col=tc$curplotcol,
                       layout.pos.row=tc$curplotrow,
                       xscale=c(curxmin,curxmax),
                       yscale=c(curymin,curymax),
                       gp=gpar(cex=tc[[atype]]$cex[tc$iplot],
                         fontsize=tc[[atype]]$fontsize[tc$iplot]
                         ),
                       name=paste("plot",tc$iplot,atype,sep=""),
                       clip="off"
                       )

    currvpTree <- vpTree(tc$mainvp,
                       vpList(currvp)
                       ) 

    pushViewport(currvpTree)    
    
    # Get at & labels
    add.labels <- TRUE
    delete.labels <- FALSE
    if (nrow(tc[[atype]]$at)>1) {
        at <- na.omit(as.vector(tc[[atype]]$at[tc$iplot,]))
        labels <- na.omit(as.vector(tc[[atype]]$labels[tc$iplot,]))
    } else {
        at <- na.omit(as.vector(tc[[atype]]$at))
        labels <- na.omit(as.vector(tc[[atype]]$labels))
    }
    if (length(labels)==0) {
      labels <- NULL
      add.labels <- FALSE
    }
    if (all(labels==""))
      delete.labels <- TRUE

    if (tc[[atype]]$major[tc$iplot]) {
      delete.major <- FALSE
    } else {
      delete.major <- TRUE
    }

    # Create axis
    axis.grob <- eval(call(paste(atypef,"Grob",sep=""),
                           at=at,
                           gp=gpar(lwd=tc[[atype]]$lwd[tc$iplot],
                             cex=tc[[atype]]$cex[tc$iplot],
                             fontsize=tc[[atype]]$fontsize[tc$iplot],
                             col=tc[[atype]]$col[tc$iplot]),                          
                           vp=currvpTree,  # currvp
                           main=main
                           )
                      )

    # Add labels to axis
    #axis.grob <- editGrob(axis.grob,
    #                      gPath("labels"),
    #                      label=labels)

    
    if (add.labels) 
      axis.grob <- editGrob(axis.grob,
                            gPath("labels"),
                            label=tc[[atype]]$labels[tc$iplot,])

    if (delete.labels)
      axis.grob <- removeGrob(axis.grob,
                              gPath("labels"))

    if (tc[[atype]]$tick.length[tc$iplot]<0)
      tickdir <- "out"
    else
      tickdir <- "in"
    if (main) {
      ticklength <- tc[[atype]]$tick.length[tc$iplot]
    } else {
      ticklength <- -tc[[atype]]$tick.length[tc$iplot]
    }

    if (!delete.labels)
      axis.grob <- editGrob(axis.grob,
                            gPath("labels"),
                            rot=tc[[atype]]$rot[tc$iplot])

    
    if ((atype=="xaxis")||(atype=="topaxis")) {
      if (!is.null(tc$gridlines$type)&&length(grep(substring(atype,1,1),tc$gridlines$type[tc$iplot]))) {
        add.gridlines <- TRUE
        axis.grob <- editGrob(axis.grob,
                              gPath("ticks"),
                              y1=(as.numeric(main==FALSE)*unit(1,"npc") + unit(0.0,"lines")))
            
        gridlines <- list(type=substring(atype,1,1),  # or "x" or "y" or "xy" or "tr" etc
                          lwd=tc$gridlines$lwd[tc$iplot],
                          col=tc$gridlines$col[tc$iplot],
                          lty=tc$gridlines$lty[tc$iplot],
                          limits=c(curxmin,curxmax,curymin,curymax),
                          edges=tc$gridlines$edges[tc$iplot]
                          )            
        gridlines[[paste(substring(atype,1,1),"at",sep="")]] <- at

            # remove overlap with vertmarks    TO ADD

        currgList <- gridlines(gridlines,currvpTree,currgList)
        
      } else {
        add.gridlines <- FALSE
        axis.grob <- editGrob(axis.grob,
                              gPath("ticks"),
                              y1= as.numeric(main==FALSE)*unit(1,"npc") + unit(ticklength,"lines"))
      }
      
        # Revise this section:

        # Measure ticks or thickest rug
        # Measure half of label width/height
        # Add a small buffer between them (parameter:  should be same as width of axis title?)
        # Combined width/height is yloc

      if (main) {  # X axis
          # Baseline for main axes
        yloc <- unit(0,"npc")
          
        if (add.rug) {
          # Create space for thickest rug
          yloc <- yloc - unit(tc$height$xaxis.rug[tc$iplot], "char")   #"lines"
        } else {
          # Create space for outfacing ticks
          if ((tickdir=="out")&&!add.gridlines) 
            yloc <- yloc + unit(ticklength,"lines")
        }

        if (!delete.labels) {
          # Add space for labels if present
          if ((tc[[atype]]$rot[tc$iplot]==90)||(tc[[atype]]$rot[tc$iplot]==270)) {
            tempheight <- max(convertHeight(unit(rep(1,length(labels)),"strheight",as.list(as.character(labels))), "lines", valueOnly=TRUE))
            tempwidth <- max(convertHeight(unit(rep(1,length(labels)),"strwidth",as.list(as.character(labels))), "lines", valueOnly=TRUE))
            yloc <- yloc - 0.5*unit(tempwidth,"lines") - tc$height$xaxis.labelspace[tc$iplot]*unit(tempheight,"lines")
          } else {
            tempheight <- max(convertHeight(unit(rep(1,length(labels)),"strheight",as.list(as.character(labels))), "lines", valueOnly=TRUE))
            yloc <- yloc - (tc$height$xaxis.labelspace[tc$iplot] + 0.5)*unit(tempheight,"lines")
          }
          axis.grob <- editGrob(axis.grob,
                                gPath("labels"),
                                y=yloc)
        }
        
      } else {    # Top axis
          # Baseline for mirrored axes
        yloc <- unit(0,"npc") #unit(1,"npc")

        if (add.rug) {
            # Create space for thickest rug
            yloc <- yloc + unit(tc$height$topaxis.rug[tc$iplot], "char")
        } else {
            # Create space for outfacing ticks
          if ((tickdir=="out")&&!add.gridlines) 
            yloc <- yloc + unit(ticklength,"lines")
        }

        if (!delete.labels) {
            # Add space for labels if present
          if ((tc[[atype]]$rot[tc$iplot]==90)||(tc[[atype]]$rot[tc$iplot]==270)) {
            tempheight <- max(convertHeight(unit(rep(1,length(labels)),"strheight",as.list(as.character(labels))), "lines", valueOnly=TRUE))
            tempwidth <- max(convertHeight(unit(rep(1,length(labels)),"strwidth",as.list(as.character(labels))), "lines", valueOnly=TRUE))
            yloc <- yloc + 0.5*unit(tempwidth,"lines") + tc$height$xaxis.labelspace[tc$iplot]*unit(tempheight,"lines")
          } else {
            tempheight <- max(convertHeight(unit(rep(1,length(labels)),"strheight",as.list(as.character(labels))), "lines", valueOnly=TRUE))
            yloc <- yloc + (tc$height$topaxis.labelspace[tc$iplot] + 0.5)*unit(tempheight,"lines")
          }
            axis.grob <- editGrob(axis.grob,
                                gPath("labels"),
                                y=unit(1,"npc")+yloc)
        }
      }
    
      axis.grob <- editGrob(axis.grob,
                            gPath("major"),
                            x=unit(c(0,1),"npc"))
    } else {
      
      if (!is.null(tc$gridlines$type)) {
        add.gridlines <- TRUE
        axis.grob <- editGrob(axis.grob,
                              gPath("ticks"),
                              x1=(as.numeric(main==FALSE)*unit(1,"npc") + unit(0.0,"lines")))
        
        gridlines <- list(type=substring(atype,1,1),  # or "x" or "y" or "xy" or "tr" etc
                          lwd=tc$gridlines$lwd[tc$iplot],
                          col=tc$gridlines$col[tc$iplot],
                          lty=tc$gridlines$lty[tc$iplot],
                          limits=c(curxmin,curxmax,curymin,curymax),
                          edges=tc$gridlines$edges[tc$iplot]
                          )            
        gridlines[[paste(substring(atype,1,1),"at",sep="")]] <- at

            # remove overlap with vertmarks    TO ADD

        currgList <- gridlines(gridlines,currvpTree,currgList)        
      } else {
        add.gridlines <- FALSE
        axis.grob <- editGrob(axis.grob,
                              gPath("ticks"),
                              x1= as.numeric(main==FALSE)*unit(1,"npc") + unit(ticklength,"lines")
                              #x1= as.numeric(main==FALSE)*unit(1,"npc") + unit(tc[[atype]]$tickslength[tc$iplot]*ticksdir,"lines")
                              )   
      }

      if (main) {  # Y axis
        # Baseline for main axes
        xloc <- unit(0,"npc")
          
        if (add.rug) {
          # Create space for thickest rug
          xloc <- xloc - convertWidth(unit(tc$width$yaxis.rug[tc$iplot], "char"), "npc")  # lines?  char -> npc ?
        } else {
            # Create space for outfacing ticks
          if ((tickdir=="out")&&!add.gridlines) 
            xloc <- xloc + unit(ticklength,"lines")
        }

        if (!delete.labels) {
            # Add space for labels if present
          if ((tc[[atype]]$rot[tc$iplot]==90)||(tc[[atype]]$rot[tc$iplot]==270)) {
            tempwidth <- max(convertHeight(unit(rep(1,length(labels)),"strheight",as.list(as.character(labels))), "lines", valueOnly=TRUE))
            xloc <- xloc - (tc$width$yaxis.labelspace[tc$iplot] + 0.5)*unit(tempwidth,"lines")
          } else {
            tempheight <- max(convertHeight(unit(rep(1,length(labels)),"strheight",as.list(as.character(labels))), "lines", valueOnly=TRUE))
            tempwidth <- max(convertHeight(unit(rep(1,length(labels)),"strwidth",as.list(as.character(labels))), "lines", valueOnly=TRUE))
            xloc <- xloc - 0.5*unit(tempwidth,"lines") - tc$width$yaxis.labelspace[tc$iplot]*unit(tempheight,"lines")
            # Previously:  likely bug
            #xloc <- xloc - 0.5*unit(tempwidth,"lines") - tc$height$xaxis.labelspace[tc$iplot]*unit(tempheight,"lines")
          }
          axis.grob <- editGrob(axis.grob ,
                                gPath("labels"),
                                x=xloc)
        }
      } else {    # Right axis
        # Baseline for mirrored axes
        xloc <- unit(0,"npc")#unit(1,"npc")

        if (add.rug) {
          # Create space for thickest rug
          xloc <- xloc + unit(tc$width$rightaxis.rug[tc$iplot], "char")
            
        } else {
          # Create space for outfacing ticks
          if ((tickdir=="out")&&!add.gridlines) 
            xloc <- xloc + unit(ticklength,"lines")
        }

        if (!delete.labels) {
            # Add space for labels if present
          if ((tc[[atype]]$rot[tc$iplot]==90)||(tc[[atype]]$rot[tc$iplot]==270)) {
            tempwidth <- max(convertHeight(unit(rep(1,length(labels)),"strheight",as.list(as.character(labels))), "lines", valueOnly=TRUE))
            xloc <- xloc + (tc$width$rightaxis.labelspace[tc$iplot] + 0.5)*unit(tempwidth,"lines")
          } else {
            tempheight <- max(convertHeight(unit(rep(1,length(labels)),"strheight",as.list(as.character(labels))), "lines", valueOnly=TRUE))
            tempwidth <- max(convertHeight(unit(rep(1,length(labels)),"strwidth",as.list(as.character(labels))), "lines", valueOnly=TRUE))
            xloc <- xloc + 0.5*unit(tempwidth,"lines") + tc$width$rightaxis.labelspace[tc$iplot]*unit(tempheight,"lines")
          }
          
          #tempwidth <- max(convertHeight(unit(rep(1,length(labels)),"strheight",as.list(as.character(labels))), "lines", valueOnly=TRUE))
          #xloc <- xloc + (tc$width$rightaxis.labelspace[tc$iplot] + 0.5)*unit(tempwidth,"lines")
          axis.grob <- editGrob(axis.grob ,
                                gPath("labels"),
                                x=unit(1,"npc") + xloc)
        }
     
      axis.grob <- editGrob(axis.grob,
                            gPath("major"),
                            y=unit(c(0,1),"npc"))
      }
    }
    
    if (add.rug) {      
      # Remove axis line and ticks
      axis.grob <- removeGrob(axis.grob,
                              gPath("major"))
      
      axis.grob <- removeGrob(axis.grob,
                              gPath("ticks"))
    }

    if (delete.major)
      axis.grob <- removeGrob(axis.grob,
                              gPath("major"), warn=FALSE)    
      
    atypenpc <- paste(atype,".npc",sep="")
    atypeunits <- paste(atype,".lines",sep="")
    
    # Measure height of axis:  units
    if (atype=="xaxis") {
      if (delete.labels) {
        tempheight <- abs(convertHeight(yloc, "lines", valueOnly=TRUE))
        tc$height[[atypeunits]][tc$currrow] <- unit(tempheight, "lines") 
      } else {
        tempheight <- abs(convertHeight(getGrob(axis.grob,gPath("labels"))$y, "lines", valueOnly=TRUE))
        if ((tc[[atype]]$rot[tc$iplot]==90)||(tc[[atype]]$rot[tc$iplot]==270))
          tempheight <- tempheight + max(convertWidth(unit(rep(1,length(labels)),"strwidth",as.list(as.character(labels))), "lines", valueOnly=TRUE))
        else
          tempheight <- tempheight + 0.5*convertHeight(grobHeight(getGrob(axis.grob,gPath("labels"))), "lines",valueOnly=TRUE)
        tc$height[[atypeunits]][tc$currrow] <- unit(tempheight, "lines")
      }
    }
    if (atype=="topaxis") {
      if (delete.labels) {
        tempheight <- abs(convertHeight(yloc, "lines", valueOnly=TRUE)) - convertHeight(unit(1,"npc"),"lines",valueOnly=TRUE)
        tc$height[[atypeunits]][tc$currrow] <- unit(tempheight, "lines") 
      } else {
        tempheight <- abs(convertHeight(getGrob(axis.grob,gPath("labels"))$y, "lines", valueOnly=TRUE)) - convertHeight(unit(1,"npc"),"lines",valueOnly=TRUE)
        if ((tc[[atype]]$rot[tc$iplot]==90)||(tc[[atype]]$rot[tc$iplot]==270))
          tempheight <- tempheight + max(convertWidth(unit(rep(1,length(labels)),"strwidth",as.list(as.character(labels))), "lines", valueOnly=TRUE))
        else
          tempheight <- tempheight + 0.5*convertHeight(grobHeight(getGrob(axis.grob,gPath("labels"))), "lines",valueOnly=TRUE)
        tc$height[[atypeunits]][tc$currrow] <- unit(tempheight, "lines")       
      }
    }
    if (atype=="yaxis") { 
      tc$height[[atypeunits]][tc$currrow] <- convertHeight(unit(1, units="grobheight", list(axis.grob)), "lines")
    }
    if (atype=="rightaxis") { 
      tc$height[[atypeunits]][tc$currrow] <- convertHeight(unit(1, units="grobheight", list(axis.grob)), "lines")
    }

    # Convert height of axis to npc
    tc$height[[atypenpc]][tc$currrow] <- max(tc$height[[atypenpc]][tc$currrow],
                                             convertHeight(tc$height[[atypeunits]][tc$currrow],
                                                           "npc",valueOnly=TRUE)
                                             )    
    # Measure width of axis: units
    if (atype=="xaxis") { 
      tc$width[[atypeunits]][tc$currcol] <- convertWidth(unit(1, units="grobwidth", list(axis.grob)), "lines")
    }
    if (atype=="topaxis") { 
      tc$width[[atypeunits]][tc$currcol] <- convertWidth(unit(1, units="grobwidth", list(axis.grob)), "lines")
    }
    if (atype=="yaxis") {
      if (delete.labels) {
        tempwidth <- abs(convertWidth(xloc, "lines", valueOnly=TRUE))
        tc$width[[atypeunits]][tc$currrow] <- unit(tempwidth, "lines") 
      } else {
        tempwidth <- abs(convertWidth(xloc, "lines", valueOnly=TRUE))
        if ((tc[[atype]]$rot[tc$iplot]==90)||(tc[[atype]]$rot[tc$iplot]==270))
          tempwidth <- tempwidth + 0.5*convertWidth(grobWidth(getGrob(axis.grob,gPath("labels"))), "lines",valueOnly=TRUE)
        else {
          tempwidth <- tempwidth + max(convertWidth(unit(rep(1,length(labels)),"strwidth",as.list(as.character(labels))), "lines", valueOnly=TRUE))
        }
        tc$width[[atypeunits]][tc$currcol] <- unit(tempwidth, "lines")
      }
    }
    if (atype=="rightaxis") {
      if (delete.labels) {
        tempwidth <- abs(convertWidth(xloc, "lines", valueOnly=TRUE)) - convertWidth(unit(1,"npc"),"lines",valueOnly=TRUE)
        tc$width[[atypeunits]][tc$currrow] <- unit(tempwidth, "lines") 
      } else {
        tempwidth <- abs(convertWidth(getGrob(axis.grob,gPath("labels"))$x, "lines", valueOnly=TRUE)) - convertWidth(unit(1,"npc"),"lines",valueOnly=TRUE)
        if ((tc[[atype]]$rot[tc$iplot]==90)||(tc[[atype]]$rot[tc$iplot]==270))
          tempwidth <- tempwidth + 0.5*convertWidth(grobWidth(getGrob(axis.grob,gPath("labels"))), "lines",valueOnly=TRUE)
        else
          tempwidth <- tempwidth + convertWidth(grobWidth(getGrob(axis.grob,gPath("labels"))), "lines",valueOnly=TRUE)
        tc$width[[atypeunits]][tc$currcol] <- unit(tempwidth, "lines")       
      }
    }

    # Convert width of axis to npc
    tc$width[[atypenpc]][tc$currcol] <- max(tc$width[[atypenpc]][tc$currcol],
                                            convertWidth(tc$width[[atypeunits]][tc$currcol],
                                                     "npc",valueOnly=TRUE)
                                            )
    
    # Measure height and width on npcmain
    upViewport(0)
    pushViewport(tc$mainvp)
    atypenpcmain <- paste(atype,".npcmain",sep="")
    tc$height[[atypenpcmain]][tc$currrow] <- max(tc$height[[atypenpcmain]][tc$currrow],
                                                 convertHeight(tc$height[[atypeunits]][tc$currrow],
                                                               "npc",valueOnly=TRUE)
                                                 )


    tc$width[[atypenpcmain]][tc$currcol] <- max(tc$width[[atypenpcmain]][tc$currcol],
                                                convertWidth(tc$width[[atypeunits]][tc$currcol],
                                                             "npc",valueOnly=TRUE)
                                                )
 
    upViewport(0)
    

    # Optional boxes around axis regions
    if (tc$axisboxes) {
    # Push intended axis region

    
    
    # Push axis viewport
    curplotrow <- tc$curplotrow
    curplotcol <- tc$curplotcol
    if (atype=="xaxis") {
      curplotrow <- curplotrow + 1
      }
    if (atype=="topaxis") {
      curplotrow <- curplotrow - 1
      }
    if (atype=="yaxis") {
      curplotcol <- curplotcol - 1
      }
    if (atype=="rightaxis") {
      curplotcol <- curplotcol + 1
      }
    
    currvp <- viewport(layout.pos.col=curplotcol,
                       layout.pos.row=curplotrow,
                       xscale=c(0,1),
                       yscale=c(0,1),
                       #gp=gpar(fontsize=tc$fsize*6),
                       name=paste(atype,tc$iplot,sep=""),
                       clip="on"
                       )

    currvpTree <- vpTree(tc$mainvp,
                       vpList(currvp)
                       ) 

    pushViewport(currvpTree)    

    currgList <- gList(currgList,
                       textGrob(label=paste(atype,"Loc",sep=" "),
                                gp=gpar(col="red",cex=0.5),
                                name=paste(atype,"titleboxLabel",sep="."),
                                vp=currvpTree
                                )
                       )
    
    currgList <- gList(currgList,
                       rectGrob(gp=gpar(col="red"),
                                name=paste(atype,"titlebox",sep="."),
                                vp=currvpTree
                                )
                       )

    upViewport(0)
  }

    tc$currgList <- gList(tc$currgList,currgList,axis.grob)
    tc
}

