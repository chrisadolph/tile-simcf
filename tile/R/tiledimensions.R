tiledimensions <- function(tc) {
    
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
            tc$height$xaxis.lines[irow] <- unit(max(tc$height$xaxis[currplot])*tc$fsize,"lines")     
            lh <- unit.c(lh,tc$height$xaxis.lines[irow])
        }
        
      # Top axis
        if (any(tc$topaxis$add[currplot])) {
            tc$height$topaxis.lines[irow] <- unit(max(tc$height$topaxis[currplot])*tc$fsize,"lines")            
            lh <- unit.c(tc$height$topaxis.lines[irow],lh)
        } else {
            
        }
        
      #X-axis titles
        if (any(tc$xaxistitle$add[currplot])) {
            if ((irow>1)&&((tc$xaxistitle$type=="firstrow")||(tc$xaxistitle$type=="first"))) {
                tc$height$xaxistitle.strunit <- unit.c(tc$height$xaxistitle.strunit,
                                                       unit(1,"strwidth",as.list("")))
            } else {                
                ch <- unit(1,"strheight","")                
                for (iplot in currplot) {                
                    if (tc$special$xaxistitle[iplot]!="none") {
                        currlabs <- tc$special$xaxistitleControl[[iplot]]$labels
                    } else {
                        currlabs <- tc$xaxistitle$labels[iplot]
                    }
                    if (is.null(currlabs)||(length(currlabs)==0)) currlabs <- ""

                    if (tc$height$xaxistitle.useStrheight[iplot]) {
                        ch <- unit.c(ch,
                                     unit(rep(tc$height$xaxistitle[iplot],length(currlabs)), "strheight", as.list(currlabs))
                                     )                                            
                    } else {
                        ch <- unit.c(ch,
                                     unit(rep(tc$height$xaxistitle[iplot],length(currlabs)), "strwidth", as.list(currlabs))
                                     )
                    }
        
                }
                if (irow==1) {
                    tc$height$xaxistitle.strunit <- max(ch)*tc$fsize
                } else {
                    tc$height$xaxistitle.strunit <- unit.c(tc$height$xaxistitle.strunit,
                                                           max(ch)*tc$fsize)
                }
            }
            lh <- unit.c(lh,tc$height$xaxistitle.strunit[irow])
        } else {
            if (irow==1) {
                tc$height$xaxistitle.strunit <- unit(1,"strwidth",as.list(""))
            } else {
                tc$height$xaxistitle.strunit <- unit.c(tc$height$xaxistitle.strunit,
                                                       unit(1,"strwidth",as.list("")))
            }
        }

      # Top-axis titles
        if (any(tc$topaxistitle$add[currplot])) {
            if ((irow>1)&&((tc$topaxistitle$type=="firstrow")||(tc$topaxistitle$type=="first"))) {
                tc$height$topaxistitle.strunit <- unit.c(tc$height$topaxistitle.strunit,
                                                         unit(1,"strwidth",as.list("")))
            } else {

                ch <- unit(1,"strheight",as.list(""))
                for (iplot in currplot) {
                    
                    if (tc$special$topaxistitle[iplot]!="none") {
                        currlabs <- tc$special$topaxistitleControl[[iplot]]$labels
                    } else {
                        currlabs <- tc$topaxistitle$labels[iplot]
                    }
                    if (is.null(currlabs)||(length(currlabs)==0)) currlabs <- ""
                    
                    if (tc$height$topaxistitle.useStrheight[iplot]) {
                        ch <- unit.c(ch,
                                     unit(rep(tc$height$topaxistitle[iplot],length(currlabs)), "strheight", as.list(currlabs))
                                     )
                    } else {
                        ch <- unit.c(ch,
                                     unit(rep(tc$height$topaxistitle[iplot],length(currlabs)), "strwidth", as.list(currlabs))
                                     )
                    }
                    
                }
                if (irow==1) {
                    tc$height$topaxistitle.strunit <- max(ch)*tc$fsize
                } else {
                    tc$height$topaxistitle.strunit <- unit.c(tc$height$topaxistitle.strunit,
                                                             max(ch)*tc$fsize)
                }
            }
            lh <- unit.c(tc$height$topaxistitle.strunit[irow],lh)
        } else {
            if (irow==1) {
                tc$height$topaxistitle.strunit <- unit(1,"strwidth",as.list(""))
            } else {
                tc$height$topaxistitle.strunit <- unit.c(tc$height$topaxistitle.strunit,
                                                         unit(1,"strwidth",as.list("")))
            }
        }
      
        # Plot titles
        if (any(tc$plottitle$add[currplot])) {
            if(irow>1) {
                tc$width$plottitle.strunit <- unit.c(tc$width$plottitle.strunit,
                                                     max(unit(tc$width$plottitle[currplot]
                                                              ,"strwidth", as.list(tc$plottitle$labels[currplot])
                                                              ) 
                                                         ) * tc$fsize
                                                     )
                tc$height$plottitle.strunit <- unit.c(tc$height$plottitle.strunit,
                                                      max(unit(tc$height$plottitle[currplot]
                                                               ,"strheight", as.list(tc$plottitle$labels[currplot])
                                                               ) 
                                                          ) * tc$fsize
                                                      )
            } else {
                tc$width$plottitle.strunit <- max(unit(tc$width$plottitle[currplot]
                                                       ,"strwidth", as.list(tc$plottitle$labels[currplot])
                                                       ) 
                                                  ) * tc$fsize
                tc$height$plottitle.strunit <- max(unit(tc$height$plottitle[currplot]
                                                         ,"strheight", as.list(tc$plottitle$labels[currplot])
                                                         ) 
                                                    ) * tc$fsize
            }
            lh <- unit.c(tc$height$plottitle.strunit[irow],lh)
        } else {
            if(irow>1) {
                tc$width$plottitle.strunit <- unit.c(tc$width$plottitle.strunit,
                                                     unit(1,"strwidth",as.list(""))
                                                     )
                tc$height$plottitle.strunit <- unit.c(tc$height$plottitle.strunit,
                                                      unit(1,"strheight",as.list(""))
                                                      )
            } else {
                tc$width$plottitle.strunit <- unit(1,"strwidth","")
                tc$height$plottitle.strunit <- unit(1,"strheight","")
            }

        }

        # Under-titles
        if (any(tc$undertitle$add[currplot])) {
            if(irow>1) {
                tc$width$undertitle.strunit <- unit.c(tc$width$undertitle.strunit,
                                                     max(unit(tc$width$undertitle[currplot]
                                                              ,"strwidth", as.list(tc$undertitle$labels[currplot])
                                                              ) 
                                                         ) * tc$fsize
                                                     )
                tc$height$undertitle.strunit <- unit.c(tc$height$undertitle.strunit,
                                                      max(unit(tc$height$undertitle[currplot]
                                                               ,"strheight", as.list(tc$undertitle$labels[currplot])
                                                               ) 
                                                          ) * tc$fsize
                                                      )
            } else {
                tc$width$undertitle.strunit <- max(unit(tc$width$undertitle[currplot]
                                                       ,"strwidth", as.list(tc$undertitle$labels[currplot])
                                                       ) 
                                                  ) * tc$fsize
                tc$height$undertitle.strunit <- max(unit(tc$height$undertitle[currplot]
                                                         ,"strheight", as.list(tc$undertitle$labels[currplot])
                                                         ) 
                                                    ) * tc$fsize
            }
            lh <- unit.c(lh,tc$height$undertitle.strunit[irow])
        } else {
            if(irow>1) {
                tc$width$undertitle.strunit <- unit.c(tc$width$undertitle.strunit,
                                                     unit(1,"strwidth","")
                                                     )
                tc$height$undertitle.strunit <- unit.c(tc$height$undertitle.strunit,
                                                      unit(1,"strheight","")
                                                      )
            } else {
                tc$width$undertitle.strunit <- unit(1,"strwidth","")
                tc$height$undertitle.strunit <- unit(1,"strheight","")
            }

        }
        
      # Add Column titles
      if (tc$columntitle$add&&(irow==1)) {
          ctr <- 0
          for (iplot in currplot) {
              ctr <- ctr+1
              if ((tc$columntitle$rot[iplot]==0)||(tc$columntitle$rot[iplot]==180)) {               
                  if (ctr==1) {
                      tc$width$columntitle.strunit <- unit(tc$width$columntitle[ctr],"strwidth",tc$columntitle$labels[ctr])
                      tc$height$columntitle.strunit <- unit(tc$height$columntitle[ctr],"strheight",tc$columntitle$labels[ctr])
                  } else {
                      tc$width$columntitle.strunit <- unit.c(tc$width$columntitle.strunit,
                                                          unit(tc$width$columntitle[ctr],"strwidth",tc$columntitle$labels[ctr]))
                      tc$height$columntitle.strunit <- unit.c(tc$height$columntitle.strunit,
                                                           unit(tc$height$columntitle[ctr],"strheight",tc$columntitle$labels[ctr]))
                  }
              } else {
                  if (ctr==1) {
                      tc$width$columntitle.strunit <- unit(tc$width$columntitle[ctr],"strheight",tc$columntitle$labels[ctr])
                      tc$height$columntitle.strunit <- unit(tc$height$columntitle[ctr],"strwidth",tc$columntitle$labels[ctr])
                  } else {
                      tc$width$columntitle.strunit <- unit.c(tc$width$columntitle.strunit,
                                                          unit(tc$width$columntitle[ctr],"strheight",tc$columntitle$labels[ctr]))
                      tc$height$columntitle.strunit <- unit.c(tc$height$columntitle.strunit,
                                                           unit(tc$height$columntitle[ctr],"strwidth",tc$columntitle$labels[ctr]))
                  }
              }
              tc$width$columntitle.strunit <- max(tc$width$columntitle.strunit) * tc$fsize
              tc$height$columntitle.strunit <- max(tc$height$columntitle.strunit) * tc$fsize
                                                                         
          }
        lh <- unit.c(tc$width$columntitle.strunit,lh)
      }
        

      # Main title        
      if (tc$maintitle$add&&(irow==1)) {
          tc$width$maintitle.strunit <- tc$width$maintitle*max(unit(rep(1, length(tc$maintitle$labels))
                                                                  ,"strwidth", as.list(tc$maintitle$labels)
                                                                  ) 
                                                             ) * tc$fsize
          tc$height$maintitle.strunit <- tc$height$maintitle*max(unit(rep(1, length(tc$maintitle$labels))
                                                                ,"strheight", as.list(tc$maintitle$labels)
                                                                ) 
                                                           ) * tc$fsize
          lh <- unit.c(tc$height$maintitle.strunit,lh)
      }

      # Add topborder if needed
      if (irow==1) {
        tc$height$topborder.units <- unit(tc$height$topborder,"char")
        lh <- unit.c(tc$height$topborder.units,lh)
      }

      # Add spacing where needed
        if ((irow==1)&&(irow<tc$RxC[1])) {
            tc$height$spacer.units <- unit(tc$height$spacer[irow],"char")
            lh <- unit.c(lh,tc$height$spacer.units[irow])
        } else {
            if (irow<tc$RxC[1]) {            
                tc$height$spacer.units <- unit.c(tc$height$spacer.units,
                                                 unit(tc$height$spacer[irow],"char"))
                lh <- unit.c(lh,tc$height$spacer.units[irow])
            } else {
                if (irow==1) tc$height$spacer.units <- unit(0,"char")
                tc$height$bottomborder.units <- unit(tc$height$bottomborder,"char")
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
            tc$width$yaxis.lines[iwidth] <- unit(max(tc$width$yaxis[currplot])*tc$fsize,"lines")
            lw <- unit.c(tc$width$yaxis.lines[iwidth],lw)
        }
        
      # Y-axis titles
         if (any(tc$yaxistitle$add[currplot])) {
            if ((iwidth>1)&&((tc$yaxistitle$type=="row")||(tc$yaxistitle$type=="first"))) {
                tc$width$yaxistitle.strunit <- unit.c(tc$width$yaxistitle.strunit,
                                                       unit(1,"strwidth",as.list("")))
            } else {
                cw <- unit(1,"strwidth",as.list(""))
                for (iplot in currplot) {
                    
                    if (tc$special$yaxistitle[iplot]!="none") {
                        currlabs <- tc$special$yaxistitleControl[[iplot]]$labels
                    } else {
                        currlabs <- tc$yaxistitle$labels[iplot]
                    }
                    if (is.null(currlabs)||(length(currlabs)==0)) currlabs <- ""

                    if (tc$width$yaxistitle.useStrheight[iplot]) {
                        cw <- unit.c(cw,
                                     unit(rep(tc$width$yaxistitle[iplot],length(currlabs)), "strheight", as.list(currlabs))
                                     )
                    } else {
                        cw <- unit.c(cw,
                                     unit(rep(tc$width$yaxistitle[iplot],length(currlabs)), "strwidth", as.list(currlabs))
                                     )
                    }
                    
                }
                if (iwidth==1) {
                    tc$width$yaxistitle.strunit <- max(cw)*tc$fsize
                } else {
                    tc$width$yaxistitle.strunit <- unit.c(tc$width$yaxistitle.strunit,
                                                           max(cw)*tc$fsize)
                }
            }            
            lw <- unit.c(tc$width$yaxistitle.strunit[iwidth],lw)
        } else {
            if (iwidth==1) {
                tc$width$yaxistitle.strunit <- unit(1,"strwidth",as.list(""))
            } else {
                tc$width$yaxistitle.strunit <- unit.c(tc$width$yaxistitle.strunit,
                                                      unit(1,"strwidth",as.list("")))
            }
        }
       
      # Right-axis
        
        if (any(tc$rightaxis$add[currplot])) {
            tc$width$rightaxis.lines[iwidth] <- unit(max(tc$width$rightaxis[currplot])*tc$fsize,"lines")
            lw <- unit.c(lw,tc$width$rightaxis.lines[iwidth])
        }
      
      # Right-axis titles
          if (any(tc$rightaxistitle$add[currplot])) {
            if ((iwidth>1)&&((tc$rightaxistitle$type=="row")||(tc$rightaxistitle$type=="first"))) {
                tc$width$rightaxistitle.strunit <- unit.c(tc$width$rightaxistitle.strunit,
                                                       unit(1,"strwidth",as.list("")))
            } else {
                cw <- unit(1,"strwidth",as.list(""))
                for (iplot in currplot) {                
                    if (tc$special$rightaxistitle[iplot]!="none") {
                        currlabs <- tc$special$rightaxistitleControl[[iplot]]$labels
                    } else {
                        currlabs <- tc$rightaxistitle$labels[iplot]
                    }
                     if (is.null(currlabs)||(length(currlabs)==0)) currlabs <- ""
                    
                    if (tc$width$rightaxistitle.useStrheight[iplot]) {
                        cw <- unit.c(cw,
                                     unit(rep(tc$width$rightaxistitle[iplot],length(currlabs)), "strheight", as.list(currlabs))
                                     )
                    } else {
                        cw <- unit.c(cw,
                                     unit(rep(tc$width$rightaxistitle[iplot],length(currlabs)), "strwidth", as.list(currlabs))
                                     )
                    }
                    
                }
                if (iwidth==1) {
                    tc$width$rightaxistitle.strunit <- max(cw)*tc$fsize
                } else {
                    tc$width$rightaxistitle.strunit <- unit.c(tc$width$rightaxistitle.strunit,
                                                           max(cw)*tc$fsize)
                }
            }
            lw <- unit.c(lw,tc$width$rightaxistitle.strunit[iwidth])
        } else {
             if (iwidth==1) {
                tc$width$rightaxistitle.strunit <- unit(1,"strwidth",as.list(""))
            } else {
                tc$width$rightaxistitle.strunit <- unit.c(tc$width$rightaxistitle.strunit,
                                                          unit(1,"strwidth",as.list("")))
            }
         }      

        #length(gregexpr("\n",label,fixed=TRUE)[[1]])
        
      # Add Row titles
      if (tc$rowtitle$add&&(iwidth==1)) {
          ctr <- 0
          for (iplot in 1:tc$RxC[1]) {
              ctr <- ctr+1
              if ((tc$rowtitle$rot[iplot]==0)||(tc$rowtitle$rot[iplot]==180)) {               
                  if (ctr==1) {
                      tc$width$rowtitle.strunit <- unit(tc$width$rowtitle[ctr],"strwidth",tc$rowtitle$labels[ctr])
                      tc$height$rowtitle.strunit <- unit(tc$height$rowtitle[ctr],"strheight",tc$rowtitle$labels[ctr])
                  } else {
                      tc$width$rowtitle.strunit <- unit.c(tc$width$rowtitle.strunit,
                                                          unit(tc$width$rowtitle[ctr],"strwidth",tc$rowtitle$labels[ctr]))
                      tc$height$rowtitle.strunit <- unit.c(tc$height$rowtitle.strunit,
                                                           unit(tc$height$rowtitle[ctr],"strheight",tc$rowtitle$labels[ctr]))
                  }
              } else {
                  if (ctr==1) {
                      tc$width$rowtitle.strunit <- unit(tc$width$rowtitle[ctr],"strheight",tc$rowtitle$labels[ctr])
                      tc$height$rowtitle.strunit <- unit(tc$height$rowtitle[ctr],"strwidth",tc$rowtitle$labels[ctr])
                  } else {
                      tc$width$rowtitle.strunit <- unit.c(tc$width$rowtitle.strunit,
                                                          unit(tc$width$rowtitle[ctr],"strheight",tc$rowtitle$labels[ctr]))
                      tc$height$rowtitle.strunit <- unit.c(tc$height$rowtitle.strunit,
                                                           unit(tc$height$rowtitle[ctr],"strwidth",tc$rowtitle$labels[ctr]))
                  }
              }
              
              tc$width$rowtitle.strunit <- max(tc$width$rowtitle.strunit) #* (tc$rowtitle$fontsize[1]/tc$output$pointsize)#tc$fsize
              tc$height$rowtitle.strunit <- max(tc$height$rowtitle.strunit) #* (tc$rowtitle$fontsize[1]/tc$output$pointsize)#tc$fsize
                                                                         
          }
        lw <- unit.c(tc$width$rowtitle.strunit,lw)
      }    

      # Add leftborder if needed
      if (iwidth==1) {
        tc$width$leftborder.units <- unit(tc$width$leftborder,"char")
        lw <- unit.c(tc$width$leftborder.units,lw)
      }
        
      # Add spacing where needed
        if ((iwidth==1)&&(iwidth<tc$RxC[2])) {
            tc$width$spacer.units <- unit(tc$width$spacer[iwidth],"char")
            lw <- unit.c(lw,tc$width$spacer.units[iwidth])
        } else {
            if (iwidth<tc$RxC[2]) {            
                tc$width$spacer.units <- unit.c(tc$width$spacer.units,
                                                 unit(tc$width$spacer[iwidth],"char"))
                lw <- unit.c(lw,tc$width$spacer.units[iwidth])
            } else {
                if (iwidth==1) tc$width$spacer.units <- unit(0,"char")
                tc$width$rightborder.units <- unit(tc$width$rightborder,"char")
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

    nrow <- 1 + tc$columntitle$add + tc$maintitle$add  # 1 is topborder
    ncol <- 1 + tc$rowtitle$add                        # 1 is leftborder
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

    tc
}
