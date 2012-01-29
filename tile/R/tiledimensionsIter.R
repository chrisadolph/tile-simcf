tiledimensionsIter <- function(tc) {
    
    # prep dimensions
    tc$nullswide <- sum(tc$width$plotmax[1:tc$RxC[2]])
    tc$fsize <- 1
    
    # Prep layout heights
    for (irow in 1:tc$RxC[1]) {
        currplot <- getplotnums("row",irow,tc)
        firstinrow <- ((irow-1)*tc$RxC[2]+1)
        
        # Prep layout heights
        lh <- tc$height$plot.units <- unit(tc$height$plotmax[firstinrow],"null")

        # X-axis
        if (any(tc$xaxis$add[currplot])) { 
            lh <- unit.c(lh,unit(tc$height$xaxis.npcmain[irow],"npc"))
        }
          
        # Top axis
        if (any(tc$topaxis$add[currplot])) {        
            lh <- unit.c(unit(tc$height$topaxis.npcmain[irow],"npc"),lh)
        }
         
        #X-axis titles
        if (any(tc$xaxistitle$add[currplot])) {
            if ((irow>1)&&((tc$xaxistitle$type=="firstrow")||(tc$xaxistitle$type=="first"))) {
                lh <- unit.c(lh,unit(0,"npc"))
            } else {
                lh <- unit.c(lh,unit(tc$height$xaxistitle.npcmain[irow],"npc"))
            }
            tc$height$xaxistitle.npcmainSaved[irow] <- tc$height$xaxistitle.npcmain[irow]
            tc$height$xaxistitle.npcmain[irow] <- 0
        }

        # Top-axis titles
        if (any(tc$topaxistitle$add[currplot])) {
            if ((irow>1)&&((tc$topaxistitle$type=="firstrow")||(tc$topaxistitle$type=="first"))) {
                lh <- unit.c(unit(0,"npc"),lh)
            } else {
                lh <- unit.c(unit(tc$height$topaxistitle.npcmain[irow],"npc"),lh)
            }
            tc$height$topaxistitle.npcmainSaved[irow] <- tc$height$topaxistitle.npcmain[irow] 
            tc$height$topaxistitle.npcmain[irow] <- 0
        }
        
        # Plot titles
        if (any(tc$plottitle$add[currplot])) {
            lh <- unit.c(unit(tc$height$plottitle.npcmain[irow],"npc"),lh)
            tc$height$plottitle.npcmainSaved[irow] <- tc$height$plottitle.npcmain[irow]
            tc$height$plottitle.npcmain[irow] <- 0
        }

        # Under-titles
        if (any(tc$undertitle$add[currplot])) {
         
            lh <- unit.c(lh,unit(tc$height$undertitle.npcmain[irow],"npc"))
            tc$height$undertitle.npcmainSaved[irow] <- tc$height$undertitle.npcmain[irow]
            tc$height$undertitle.npcmain[irow] <- 0

        }
        
      # Add Column titles
      if (tc$columntitle$add&&(irow==1)) {
       
        lh <- unit.c(unit(max(tc$height$columntitle.npcmain),"npc"),lh)
        tc$height$columntitle.npcmainSaved[irow] <- tc$height$columntitle.npcmain[irow]
        tc$height$columntitle.npcmain[irow] <- 0
      }


      # Main title        
      if (tc$maintitle$add&&(irow==1)) {
          if ((irow>1)&&(!is.null(tc$maintitle$type))&&((tc$maintitle$type=="firstrow"))) {
              lh <- unit.c(unit(0,"npc"),lh)
          } else {
              lh <- unit.c(unit(tc$height$maintitle.npcmain[1],"npc"),lh)
          }
          tc$height$maintitle.npcmainSaved <- tc$height$maintitle.npcmain
          tc$height$maintitle.npcmain <- 0
      }

      # Add topborder if needed
      if (irow==1) {
        tc$height$topborder.units <- convertHeight(unit(tc$height$topborder,"char"), "npc")
        lh <- unit.c(tc$height$topborder.units,lh)
      }
        
      # Add spacing where needed
        if ((irow==1)&&(irow<tc$RxC[1])) {
            #tc$height$spacer.units <- unit(tc$height$spacer[irow],"char")
            tc$height$spacer.units <- convertHeight(unit(tc$height$spacer[irow],"char"), "npc")
            lh <- unit.c(lh,tc$height$spacer.units[irow])
        } else {
            if (irow<tc$RxC[1]) {            
                #tc$height$spacer.units <- unit.c(tc$height$spacer.units,
                #                                 unit(tc$height$spacer[irow],"char"))
                tc$height$spacer.units <- unit.c(tc$height$spacer.units,
                                                 convertHeight(unit(tc$height$spacer[irow],"char"), "npc"))
                lh <- unit.c(lh,tc$height$spacer.units[irow])
            } else {
                #if (irow==1) tc$height$spacer.units <- unit(0,"char")
                #tc$height$bottomborder.units <- unit(tc$height$bottomborder,"char")
                if (irow==1) tc$height$spacer.units <- convertHeight(unit(0,"char"), "npc")
                tc$height$bottomborder.units <- convertHeight(unit(tc$height$bottomborder,"char"), "npc")
                lh <- unit.c(lh,tc$height$bottomborder.units)
            }
        }
    

      # Add to layout
      if (irow==1)
        tc$height$layout <- lh
      else
        tc$height$layout <- unit.c(tc$height$layout,lh)

    }
    
    # Prep layout widths
    for (iwidth in 1:tc$RxC[2]) {

        currplot <- getplotnums("column",iwidth,tc)
        
        tc$width$plot.units <- unit(tc$width$plotmax[iwidth],"null")
        lw <- tc$width$plot.unit
        
      # Y-axis
        if (any(tc$yaxis$add[currplot])) {
            lw <- unit.c(unit(tc$width$yaxis.npcmain[iwidth],"npc"),lw)            
        }
      
               
      # Y-axis titles
         if (any(tc$yaxistitle$add[currplot])) {             
             if ((iwidth>1)&&((tc$yaxistitle$type=="row")||(tc$yaxistitle$type=="first"))) {      
                lw <- unit.c(unit(0,"npc"),lw)
              } else {
                lw <- unit.c(unit(tc$width$yaxistitle.npcmain[iwidth],"npc"),lw)
              }             
             tc$width$yaxistitle.npcmainSaved[iwidth] <- tc$width$yaxistitle.npcmain[iwidth]
             tc$width$yaxistitle.npcmain[iwidth] <- 0
        }
        
        
      # Right-axis        
        if (any(tc$rightaxis$add[currplot])) {
            lw <- unit.c(lw,unit(tc$width$rightaxis.npcmain[iwidth],"npc"))
        }
      
        
      # Right-axis titles
          if (any(tc$rightaxistitle$add[currplot])) {
            if ((iwidth>1)&&((tc$rightaxistitle$type=="row")||(tc$rightaxistitle$type=="first"))) {
              lw <- unit.c(lw,unit(0,"npc"))
            } else {
              lw <- unit.c(lw,unit(tc$width$rightaxistitle.npcmain[iwidth],"npc"))
            }
            tc$width$rightaxistitle.npcmainSaved[iwidth] <- tc$width$rightaxistitle.npcmain[iwidth]
            tc$width$rightaxistitle.npcmain[iwidth] <- 0
         }      

        
      # Add Row titles
      if (tc$rowtitle$add&&(iwidth==1)) {
        lw <- unit.c(unit(max(tc$width$rowtitle.npcmain),"npc"),lw)
        tc$width$rowtitle.npcmainSaved[iwidth] <- tc$width$rowtitle.npcmain[iwidth]
        tc$width$rowtitle.npcmain[iwidth] <- 0
      }

      # Add leftborder if needed
      if (iwidth==1) {
        tc$width$leftborder.units <- convertWidth(unit(tc$width$leftborder,"char"), "npc")
        lw <- unit.c(tc$width$leftborder.units,lw)
      }  

      # Add spacing where needed
        if ((iwidth==1)&&(iwidth<tc$RxC[2])) {
          #tc$width$spacer.units <- unit(tc$width$spacer[iwidth],"char")
          tc$width$spacer.units <- convertWidth(unit(tc$width$spacer[iwidth],"char"), "npc")
            lw <- unit.c(lw,tc$width$spacer.units[iwidth])
        } else {
            if (iwidth<tc$RxC[2]) {
               #tc$width$spacer.units <- unit.c(tc$width$spacer.units,
               #                                  unit(tc$width$spacer[iwidth],"char"))
                tc$width$spacer.units <- unit.c(tc$width$spacer.units,
                                                 convertWidth(unit(tc$width$spacer[iwidth],"char"), "npc"))
                lw <- unit.c(lw,tc$width$spacer.units[iwidth])
            } else {
              #if (iwidth==1) tc$width$spacer.units <- unit(0,"char")
              #  tc$width$rightborder.units <- unit(tc$width$rightborder,"char")
                if (iwidth==1) tc$width$spacer.units <- convertWidth(unit(0,"char"), "npc")
                tc$width$rightborder.units <- convertWidth(unit(tc$width$rightborder,"char"), "npc")
                lw <- unit.c(lw,tc$width$rightborder.units)
            }
        }

      # Add to layout
      if (iwidth==1)
        tc$width$layout <- lw
      else
        tc$width$layout <- unit.c(tc$width$layout,lw)
    }

    
    # Create layout
    nrow <- 1 + tc$columntitle$add + tc$maintitle$add  # 1 is top border
    ncol <- 1 + tc$rowtitle$add                        # 1 is left border
    for (i in 1:tc$RxC[1]) {
        currplot <- getplotnums("row",i,tc)
        nrow <- (nrow 
                 + any(tc$plottitle$add[currplot])
                 + any(tc$topaxistitle$add[currplot])
                 + any(tc$topaxis$add[currplot])
                 + 1                              # the graphic
                 + any(tc$xaxis$add[currplot])
                 + any(tc$xaxistitle$add[currplot])
                 + any(tc$undertitle$add[currplot])
                 + 1                              # a spacer (except last row)
                 )
    }
    for (i in 1:tc$RxC[2]) {
        currplot <- getplotnums("column",i,tc)
        ncol <- (ncol
                 + any(tc$yaxistitle$add[currplot])
                 + any(tc$yaxis$add[currplot])
                 + 1                              # the graphic
                 + any(tc$rightaxis$add[currplot])
                 + any(tc$rightaxistitle$add[currplot])
                 + 1                              # a spacer (except last row) 
                 )
    }

    tc$overlay <- grid.layout(nrow=nrow,
                              ncol=ncol,
                              widths=tc$width$layout,
                              heights=tc$height$layout,
                              respect=TRUE)


    # Set new output width
    if (tc$output$widthNotGiven) {
      sumnpc <- sum(as.numeric(tc$width$layout[grep("npc",as.character(tc$width$layout))]))
      sumnull <- sum(as.numeric(tc$width$layout[grep("null",as.character(tc$width$layout))]))
      tc$output$wide <- tc$output$wide*sumnpc + tc$width$null*sumnull
     
    }

    tc
}
