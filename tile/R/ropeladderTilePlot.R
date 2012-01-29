ropeladderTilePlot <-
function(tc=list(),...) {
    
    currgList <- NULL

    ct <- tc$traces[[tc$itrace]]
    n <- length(ct$x)
    
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

    
    if ((ct$plotrotation==0)||(ct$plotrotation==180)) {
        at.first <- curxmin
        at.last <- curxmax
    }
    if ((ct$plotrotation==90)||(ct$plotrotation==270)) {
        at.first <- curymin
        at.last <- curymax
    }
    
    # Push viewport
    currvp <- viewport(layout.pos.col=tc$curplotcol,
                       layout.pos.row=tc$curplotrow,
                       xscale=c(curxmin,curxmax),
                       yscale=c(curymin,curymax),
                       gp=gpar(fontsize=1),                                            # Remove 6?
                       name=paste("plot",tc$iplot,"trace",tc$itrace,"clipped",sep=""),
                       #clip="on"
                       clip="off"
                       )
    currvpTree <- vpTree(tc$mainvp,
                       vpList(currvp)
                       )
    pushViewport(currvpTree)


#currgList <- titlebox(currvpTree,currgList)

    
    ## Plot elements

    if ((!is.null(ct$shadowbox))&&(!is.na(ct$shadowbox))) {
        if ((ct$plotrotation==0)||(ct$plotrotation==180)) {
            currx <- c(max(min(na.omit(ct[[ct$dataaxis]])),at.first),
                       min(max(na.omit(ct[[ct$dataaxis]])),at.last),
                       min(max(na.omit(ct[[ct$dataaxis]])),at.last),
                       max(min(na.omit(ct[[ct$dataaxis]])),at.first))
            curry <- c(min(na.omit(ct[[ct$labelaxis]]))/2,
                       min(na.omit(ct[[ct$labelaxis]]))/2,
                       (1+max(na.omit(ct[[ct$labelaxis]])))/2,
                       (1+max(na.omit(ct[[ct$labelaxis]])))/2)
            
        }
        if ((ct$plotrotation==90)||(ct$plotrotation==270)) {
            curry <- c(max(min(na.omit(ct[[ct$dataaxis]])),at.first),
                       min(max(na.omit(ct[[ct$dataaxis]])),at.last),
                       min(max(na.omit(ct[[ct$dataaxis]])),at.last),
                       max(min(na.omit(ct[[ct$dataaxis]])),at.first))
            currx <- c(min(na.omit(ct[[ct$labelaxis]]))/2,
                       min(na.omit(ct[[ct$labelaxis]]))/2,
                       (1+max(na.omit(ct[[ct$labelaxis]])))/2,
                       (1+max(na.omit(ct[[ct$labelaxis]])))/2)
        }
        
        currgList <- gList(currgList,
                           polygonGrob(x=currx,
                                       y=curry,
                                       gp=gpar(fill=ct$shadowbox,col=NA),
                                       default.units="native",
                                       name=paste("trace",tc$itrace,"shadowbox",sep=""),
                                       vp=currvpTree)
                           )
        currgList <- tileSetLayer(currgList,(ct$layer+4))
    }


 
    # Add row shadow boxes if desired
    if ((!is.null(ct$sr))&&(!is.na(ct$sr))&&(!is.null(ct$multitrace))&&ct$multitrace) {
      ally <- allx <- allid <- NULL
      for (i in 1:length(ct$groupdata)) {
        if ((ct$plotrotation==0)||(ct$plotrotation==180)) {
          currx <- c(max(min(na.omit(ct$groupdata[[i]])),at.first),
                     min(max(na.omit(ct$groupdata[[i]])),at.last),
                     min(max(na.omit(ct$groupdata[[i]])),at.last),
                     max(min(na.omit(ct$groupdata[[i]])),at.first))
          margin <- sort(na.omit(ct$grouplabel[[i]]))
          margin <- (abs(margin[1]-margin[2]))
          curry <- c(min(na.omit(ct$grouplabel[[i]])) - margin,
                     min(na.omit(ct$grouplabel[[i]])) - margin,
                     max(na.omit(ct$grouplabel[[i]])) + margin,
                     max(na.omit(ct$grouplabel[[i]])) + margin)

        }
        if ((ct$plotrotation==90)||(ct$plotrotation==270)) {
          curry <- c(max(min(na.omit(ct$groupdata[[i]])),at.first),
                     min(max(na.omit(ct$groupdata[[i]])),at.last),
                     min(max(na.omit(ct$groupdata[[i]])),at.last),
                     max(min(na.omit(ct$groupdata[[i]])),at.first))
          
          margin <- sort(na.omit(ct$grouplabel[[i]]))
          margin <- (abs(margin[1]-margin[2]))
          currx <- c(min(na.omit(ct$grouplabel[[i]])) - margin,
                     min(na.omit(ct$grouplabel[[i]])) - margin,
                     max(na.omit(ct$grouplabel[[i]])) + margin,
                     max(na.omit(ct$grouplabel[[i]])) + margin)
        }
        ally <- c(ally,curry)
        allx <- c(allx,currx)
        allid <- c(allid,rep(i,length(curry)))        
      }
      
      currgList <- gList(currgList,
                         polygonGrob(x=allx,
                                     y=ally,
                                     id=allid,
                                     gp=gpar(fill=ct$sr,col=NA),
                                     default.units="native",
                                     name=paste("trace",tc$itrace,"shadowrow",sep=""),
                                     vp=currvpTree)
                         )
      currgList <- tileSetLayer(currgList,(ct$layer+3))
    }
    
   # if (!is.null(tc$gridlines$type)) {
   #     if ((ct$plotrotation==0)||(ct$plotrotation==180)) {
   #         currat <- list(x=setdiff(na.omit(tc$xaxis$at[tc$iplot,]),
   #                        intersect(na.omit(tc$xaxis$at[tc$iplot,]),
   #                                  ct$vertmark$at)
   #                        )
   #                        )
   #         currlimits <- c(tc$limits[ct$plot,1:2],0,1)
   #     }
   #     if ((ct$plotrotation==90)||(ct$plotrotation==270)) {
   #         currat <- list(y=setdiff(na.omit(tc$xaxis$at[tc$iplot,]),
   #                        intersect(na.omit(tc$xaxis$at[tc$iplot,]),
   #                                  ct$vertmark$at)
   #                        )
   #                        )
   #         currlimits <- c(0,1,tc$limits[ct$plot,1:2])
   #     }
   #     gridlines.def <- list(style=NULL,  # or "x" 
   #                           lwd=0.15,
   #                           col="gray50",
   #                           lty="solid",
   #                           at=currat,
   #                           limits=currlimits,
   #                           edges=TRUE
   #                           )
        # removes overlap with vertmarks
  
    #    tc$gridlines <- mergelist(tc$gridlines,gridlines.def)
    #    currgList <- gridlines(tc$gridlines,currvpTree,currgList)
    #}
  
    if (!is.null(ct$vertmark$at)) {
        if ((ct$plotrotation==0)||(ct$plotrotation==180)) {
            currx <- c(tc$rl$vertmark$at,tc$rl$vertmark$at)
            curry <- c(0,1)
        }
        if ((ct$plotrotation==90)||(ct$plotrotation==270)) {
            currx <- c(0,1)
            curry <- c(ct$vertmark$at,ct$vertmark$at)
        }
                                        #for (i in 1:length(tc$rl$vertmark$at[tc$iplot]))
        currgList <- gList(currgList,
                           linesGrob(x=currx,
                                     y=curry,
                                     gp=gpar(lty=ct$vertmark$lty,#[XXX],
                                     #lwd=tc$lwd,
                                     col=ct$vertmark$col),#[XXX]),
                                     default.units="native",
                                     name="vertmark",
                                     vp=currvpTree)
                           )
        currgList <- tileSetLayer(currgList,(ct$layer+2))
    }

    # Plot trace
    if (ct$type==1) {
        for (i in 1:ct$nentries) {

            if (ct$extrapolate$control[i]) {
                #lty <- "dotdash"
                col <- lighten(ct$col[i],pct=ct$lighten)
            } else {
                #lty <- "dashed"
                col <- ct$col[i]
            }
            

            
            if (ct$arrowsout) {        
                if (!is.na(ct[[ct$dataaxis]][i])&&(ct[[ct$dataaxis]][i]>=at.first)&&(ct[[ct$dataaxis]][i]<=at.last)) {
                    if ((ct$plotrotation==0)||(ct$plotrotation==180)) {
                        currx <- ct[[ct$dataaxis]][i]
                        curry <- ct[[ct$labelaxis]][i]
                    }
                    if ((ct$plotrotation==90)||(ct$plotrotation==270)) {
                        currx <- ct[[ct$labelaxis]][i]
                        curry <- ct[[ct$dataaxis]][i]
                    }
                    currgList <- gList(currgList,
                                       pointsGrob(x=currx,
                                                  y=curry,
                                                  pch=ct$pch[i],
                                                  size=unit(ct$size[i],"char"),
                                                  gp=tileSetgpar(ct,i,col=col),
                                                  name=paste("trace",tc$itrace,"points",i,sep=""),
                                                  vp=currvpTree
                                                  )
                                       )
                    currgList <- tileSetLayer(currgList,(ct$layer))

                    if (!is.null(ct$sublabels)) {
                      currgList <- gList(currgList,
                                         textGrob(label=ct$sublabels[i],
                                                  x=ct$sublabelsxloc[i],
                                                  y=ct$sublabelsyloc[i],
                                                  gp=tileSetgpar(ct,i,col=col,fontsize=ct$sublabelsfontsize[i]),
                                                  name=paste("trace",tc$itrace,"labels",i,sep=""),
                                                  vp=currvpTree
                                                  )
                                         )
                      currgList <- tileSetLayer(currgList,(ct$layer-1))
                    }
                    
                }
                if (!is.null(ct$lower)) {
                    if (!is.na(ct$lower[i]) & !is.na(ct$upper[i])) {
                        if ((ct$lower[i]<at.first) & (ct$upper[i]>at.last)) {
                            if ((ct$plotrotation==0)||(ct$plotrotation==180)) {
                                currx <- c(at.first,at.last)
                                curry <- ct[[ct$labelaxis]][i]
                            }
                            if ((ct$plotrotation==90)||(ct$plotrotation==270)) {
                                currx <- ct[[ct$labelaxis]][i]
                                curry <- c(at.first,at.last)
                            }
                            currgList <- gList(currgList,
                                               linesGrob(x=currx,
                                                         y=curry,
                                                         gp=tileSetgpar(ct,i,col=col,lwd=ct$lwd,lty=ct$lty),
                                                         arrow=arrow(length=unit(0.4,"char"),#unit(0.04*(at.last-at.first),"npc"),
                                                         ends="both",
                                                         type="open"),
                                                         default.units="native",
                                                         name=paste("trace",tc$itrace,"ci",i,sep=""),
                                                         vp=currvpTree
                                                         )
                                               )
                            currgList <- tileSetLayer(currgList,(ct$layer))
                        } else {
                            if (ct$lower[i]<at.first) {
                                if ((ct$plotrotation==0)||(ct$plotrotation==180)) {
                                    currx <- c(at.first,ct$upper[i])
                                    curry <- ct[[ct$labelaxis]][i]
                                }
                                if ((ct$plotrotation==90)||(ct$plotrotation==270)) {
                                    currx <- ct[[ct$labelaxis]][i]                                    
                                    curry <- c(at.first,ct$upper[i])
                                }
                                currgList <- gList(currgList,
                                                   linesGrob(x=currx,#c(at.first,ct$upper[i]),
                                                             y=curry,#ct[[ct$labelaxis]][i],
                                                             gp=tileSetgpar(ct,i,col=col,lwd=ct$lwd,lty=ct$lty),
                                                             arrow=arrow(length=unit(0.4,"char"),#unit(0.04*(at.last-at.first),"npc"),
                                                             ends="first",
                                                             type="open"),
                                                             default.units="native",
                                                             name=paste("trace",tc$itrace,"ci",i,sep=""),
                                                             vp=currvpTree
                                                             )
                                                   )
                                currgList <- tileSetLayer(currgList,(ct$layer))
                            } else {
                                if (ct$upper[i]>at.last) {
                                    if ((ct$plotrotation==0)||(ct$plotrotation==180)) {
                                        currx <- c(ct$lower[i],at.last)
                                        curry <- ct[[ct$labelaxis]][i]
                                    }
                                    if ((ct$plotrotation==90)||(ct$plotrotation==270)) {
                                        currx <- ct[[ct$labelaxis]][i]
                                        curry <- c(ct$lower[i],at.last)
                                    }
                                    currgList <- gList(currgList,
                                                       linesGrob(x=currx,
                                                                 y=curry,
                                                                 gp=tileSetgpar(ct,i,col=col,lwd=ct$lwd,lty=ct$lty),
                                                                 arrow=arrow(length=unit(0.4,"char"),#unit(0.04*(at.last-at.first),"npc"),
                                                                 ends="last",
                                                                 type="open"),
                                                                 default.units="native",                                  
                                                                 name=paste("trace",tc$itrace,"ci",i,sep=""),
                                                                 vp=currvpTree
                                                                 )
                                                       )
                                    currgList <- tileSetLayer(currgList,(ct$layer))
                                } else {
                                    if ((ct$plotrotation==0)||(ct$plotrotation==180)) {
                                        currx <- c(ct$lower[i],ct$upper[i])
                                        curry <- c(ct[[ct$labelaxis]][i],ct[[ct$labelaxis]][i])
                                    }
                                    if ((ct$plotrotation==90)||(ct$plotrotation==270)) {
                                        currx <- c(ct[[ct$labelaxis]][i],ct[[ct$labelaxis]][i])
                                        curry <- c(ct$lower[i],ct$upper[i])
                                    }
                                    currgList <- gList(currgList,
                                                       linesGrob(x=currx,
                                                                 y=curry,
                                                                 gp=tileSetgpar(ct,i,col=col,lwd=ct$lwd,lty=ct$lty),
                                                                 default.units="native",
                                                                 name=paste("trace",tc$itrace,"ci",i,sep=""),
                                                                 vp=currvpTree
                                                                 )
                                                       )
                                    currgList <- tileSetLayer(currgList,(ct$layer))
                                }
                            }
                        }
                    }
                }
            } else {
                if ((ct$plotrotation==0)||(ct$plotrotation==180)) {
                    currx <- ct[[ct$dataaxis]][i]
                    curry <- ct[[ct$labelaxis]][i]
                }
                if ((ct$plotrotation==90)||(ct$plotrotation==270)) {
                    currx <- ct[[ct$labelaxis]][i]
                    curry <- ct[[ct$dataaxis]][i]
                }
                currgList <- gList(currgList,
                                   pointsGrob(x=currx,
                                              y=curry,
                                              pch=ct$pch[i],
                                              size=unit(ct$size[i],"char"),
                                              gp=tileSetgpar(ct,i,col=col),
                                              name=paste("trace",tc$itrace,"points",i,sep=""),
                                              vp=currvpTree
                                              )
                                   )
                currgList <- tileSetLayer(currgList,(ct$layer))

                if (!is.null(ct$sublabels)) {

                  currgList <- gList(currgList,
                                     textGrob(label=ct$sublabels[i],
                                              x=ct$sublabelsxloc[i],
                                              y=ct$sublabelsyloc[i],
                                              gp=tileSetgpar(ct,i,col=col,fontsize=ct$sublabelsfontsize[i]),
                                              name=paste("trace",tc$itrace,"labels",i,sep=""),
                                              vp=currvpTree
                                              )
                                     )
                  currgList <- tileSetLayer(currgList,(ct$layer-1))
                }
                
                if ((ct$plotrotation==0)||(ct$plotrotation==180)) {
                    currx <- c(ct$lower[i],ct$upper[i])
                    curry <- c(ct[[ct$labelaxis]][i],ct[[ct$labelaxis]][i])
                }
                if ((ct$plotrotation==90)||(ct$plotrotation==270)) {
                    currx <- c(ct[[ct$labelaxis]][i],ct[[ct$labelaxis]][i])
                    curry <- c(ct$lower[i],ct$upper[i])
                }
                currgList <- gList(currgList,
                                   linesGrob(x=currx,
                                             y=curry,
                                             gp=tileSetgpar(ct,i,col=col,lwd=ct$lwd,lty=ct$lty),
                                             default.units="native",
                                             name=paste("trace",tc$itrace,"ci",i,sep=""),
                                             vp=currvpTree
                                             )
                                   )
                currgList <- tileSetLayer(currgList,(ct$layer))
            }
        }
    } else {
        if (ct$type==2) {

            if (!is.null(ct$baseline$at)&&is.na(ct$baseline$at))
                if ((ct$plotrotation==0)||(ct$plotrotation==180)) {
                    ct$baseline$at <- curxmin
                } else {
                    ct$baseline$at <- curymin
                }
            
            if ((ct$plotrotation==0)||(ct$plotrotation==180)) {
                currx <- c(ct$baseline$at,ct$baseline$at)
                curry <- c(0,1)
            }
            if ((ct$plotrotation==90)||(ct$plotrotation==270)) {
                currx <- c(0,1)
                curry <- c(ct$baseline$at,ct$baseline$at)
            }
            
            currgList <- gList(currgList,
                               linesGrob(x=currx,
                                         y=curry,
                                         gp=gpar(col=ct$baseline$col,
                                                 lwd=ct$baseline$lwd,
                                                 lty=ct$baeline$lty
                                                 ),
                                         default.units="native",
                                         name=paste("trace",tc$itrace,"baseline",sep=""),
                                         vp=currvpTree
                                         )
                               )
            currgList <- tileSetLayer(currgList,(ct$layer+1))

            for (i in 1:ct$nentries) {
                
                if (ct$extrapolate$control[i]) {
                #lty <- "dotdash"
                    col <- lighten(ct$col[i],pct=ct$lighten)
                } else {
                #lty <- "dashed"
                    col <- ct$col[i]
                }
                            
                if (!is.na(ct[[ct$dataaxis]][i])) {
                    if ((ct$plotrotation==0)||(ct$plotrotation==180)) {
                        currx <- ct[[ct$dataaxis]][i]
                        curry <- ct[[ct$labelaxis]][i]
                    }
                    if ((ct$plotrotation==90)||(ct$plotrotation==270)) {
                        currx <- ct[[ct$labelaxis]][i]
                        curry <- ct[[ct$dataaxis]][i]
                    }
                    currgList <- gList(currgList,
                                       pointsGrob(x=currx,
                                                  y=curry,
                                                  pch=ct$pch[i],
                                                  size=unit(ct$size[i],"char"),
                                                  gp=tileSetgpar(ct,i,col=col),
                                                  name=paste("trace",tc$itrace,"points",i,sep=""),
                                                  vp=currvpTree
                                                  )
                                       )
                    currgList <- tileSetLayer(currgList,(ct$layer))
                    if ((ct$plotrotation==0)||(ct$plotrotation==180)) {
                        currx <- c(ct$baseline$at,ct[[ct$dataaxis]][i])
                        curry <- c(ct[[ct$labelaxis]][i],ct[[ct$labelaxis]][i])
                    }
                    if ((ct$plotrotation==90)||(ct$plotrotation==270)) {
                        currx <- c(ct[[ct$labelaxis]][i],ct[[ct$labelaxis]][i])
                        curry <- c(ct$baseline$at,ct[[ct$dataaxis]][i])
                    }
                    currgList <- gList(currgList,
                                       linesGrob(x=currx,
                                                 y=curry,
                                                 gp=tileSetgpar(ct,i,col=col),
                                                 default.units="native",
                                                 name=paste("trace",tc$itrace,"distance",i,sep=""),
                                                 vp=currvpTree
                                                 )
                                       )
                    currgList <- tileSetLayer(currgList,(ct$layer))
                }
                
                if (!is.null(ct$lower)) {
                    if ((ct$plotrotation==0)||(ct$plotrotation==180)) {
                        currx <- ct$lower[i]
                        curry <- ct[[ct$labelaxis]][i]
                        rot <- 0
                    }
                    if ((ct$plotrotation==90)||(ct$plotrotation==270)) {
                        currx <- ct[[ct$labelaxis]][i]
                        curry <- ct$lower[i]
                        rot <- 90
                    }
                    currgList <- gList(currgList,
                                       textGrob(x=currx,
                                                y=curry,
                                                label="(",
                                                rot = rot,
                                                gp=tileSetgpar(ct,i,col=col),
                                                default.units="native",
                                                name=paste("trace",tc$itrace,"lower",i,sep=""),
                                                vp=currvpTree
                                                )
                                       )
                    currgList <- tileSetLayer(currgList,(ct$layer))
                    if ((ct$plotrotation==0)||(ct$plotrotation==180)) {
                        currx <- ct$upper[i]
                        curry <- ct[[ct$labelaxis]][i]
                        rot <- 0
                    }
                    if ((ct$plotrotation==90)||(ct$plotrotation==270)) {
                        currx <- ct[[ct$labelaxis]][i]
                        curry <- ct$upper[i]
                        rot <- 90
                    }
                    currgList <- gList(currgList,
                                       textGrob(x=currx,
                                                y=curry,
                                                label=")",
                                                rot = rot,
                                                gp=tileSetgpar(ct,i,col=col),
                                                default.units="native",
                                                name=paste("trace",tc$itrace,"upper",i,sep=""),
                                                vp=currvpTree
                                                )
                                       )
                    currgList <- tileSetLayer(currgList,(ct$layer))
                }
            }
        }
    }

    
    # Create no clip viewport (not currently used)
    upViewport(0)
    currvp <- viewport(
                       layout.pos.col=tc$curplotcol,
                       layout.pos.row=tc$curplotrow,
                       xscale=c(curxmin,curxmax),
                       yscale=c(curymin,curymax),
                       name=paste("plot",tc$iplot,"trace",tc$itrace,"clipoff",sep=""),,
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

