scatterTilePlot <-
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
                       gp=gpar(fontsize=tc$fsize),                                            # Remove 6?
                       name=paste("plot",tc$iplot,"trace",tc$itrace,"clipped",sep=""),
                       clip="on"
                       )
    currvpTree <- vpTree(tc$mainvp,
                       vpList(currvp)
                       )
    pushViewport(currvpTree)

    ## Plot elements
    
    # Plot fit
    # Add fits to traces  NEED extrapolate
    if (!is.null(ct$fit$method)) {
        for (m in 1:length(ct$fit$method)) {

            # Do fitting
            if (ct$fit$method[m]=="wls") {
              fit <- eval(call(paste("tilefit",ct$fit$method[m],sep=""),
                               ct$y,
                               ct$x,
                               ct$fit$ci,
                               ct$fit$weights)
                          )
            } else {
              fit <- eval(call(paste("tilefit",ct$fit$method[m],sep=""),
                               ct$y,
                               ct$x,
                               ct$fit$ci)
                          )
            }
            
            if (!is.null(fit)) {
            # Plot fit CI
              if (!is.null(ct$fit$ci)&&!is.null(fit$lower)) {
                for (k in 1:length(ct$fit$ci)) {
                  if (ct$fit$mark[k]=="dashed") {
                    currgList <- gList(currgList,
                                       linesGrob(x=fit$x,
                                                 y=fit$lower[,k],
                                                 gp=gpar(lty="dashed",
                                                   col=ct$fit$col[m]),
                                                 default.units="native",
                                                 name=paste("trace",tc$itrace,"fit",m,"lower",k,sep=""),
                                                 vp=currvpTree
                                                 )
                                       )
                    currgList <- tileSetLayer(currgList,ct$layer)
                    currgList <- gList(currgList,
                                       linesGrob(x=fit$x,
                                                 y=fit$upper[,k],
                                                 gp=gpar(lty="dashed",
                                                   col=ct$fit$col[m]),
                                                 default.units="native",
                                                 name=paste("trace",tc$itrace,"fit",m,"upper",k,sep=""),
                                                 vp=currvpTree
                                                 )
                                       )
                    currgList <- tileSetLayer(currgList,ct$layer)
                  } else {
                    if (ct$fit$mark[k]=="shaded") {
                      xpoly <- c(fit$x,rev(fit$x),fit$x[1])
                      ypoly <- c(fit$lower[,k],rev(fit$upper[,k]),fit$lower[1,k])
                      pcol <- lighten(ct$fit$col[m])
                      currgList <- gList(currgList,
                                         polygonGrob(x=xpoly,
                                                     y=ypoly,
                                                     gp=gpar(col=pcol,
                                                       border=FALSE,
                                                       fill=pcol,
                                                       alpha=ct$polyalpha
                                                       ),
                                                     default.units="native",
                                                     name=paste("trace",tc$itrace,"fit",m,"ci",k,sep=""),
                                                     vp=currvpTree                                              
                                                     )
                                         )
                      currgList <- tileSetLayer(currgList,(ct$layer+4))
                    }
                  }
                }
              }

            # Plot fit line
              currgList <- gList(currgList,
                                 linesGrob(x=fit$x,
                                           y=fit$y,
                                           gp=gpar(lty="solid",
                                             col=ct$fit$col[m]),
                                           default.units="native",
                                           name=paste("trace",tc$itrace,"fit",m,sep=""),
                                           vp=currvpTree
                                           )
                                 )
              currgList <- tileSetLayer(currgList,ct$layer)    
            }
          }
      }
    
    #TO FINISH:  DOES UPPER NEED TO BE GENERALIZED FOR NON-X?
    # Plot CIs
    if (length(ct$lower)>0) {
      ct$lower <- as.matrix(ct$lower)
      ct$upper <- as.matrix(ct$upper)
      for (k in 1:length(ct$ci$levels)) {
        
        blocktype <- 1 + as.numeric(!ct$extrapolate$control)
        if ((ct$ci$mark[k]=="lines")||(ct$ci$mark[k]=="line")) {
          col <- lty <- rep(NA,n)
          for (i in 1:n) {            
            if (blocktype[i]==1) {
              lty[i] <- ct$lty[i]
              col[i] <- lighten(ct$col[i],pct=ct$lighten-0.1)
            }
            if (blocktype[i]==2) {
              lty[i] <- ct$lty[i]
              col[i] <- ct$col[i]
            }
          }
          polylineX <- as.vector(rbind(ct$x,ct$x))
          polylineY <- as.vector(rbind(ct$lower[,k],ct$upper[,k]))

          currgList <- gList(currgList,
                             polylineGrob(x=unit(polylineX,"native"),
                                          y=unit(polylineY,"native"),
                                          id.length=rep(2,n),
                                          gp=tileSetgpar(ct,i,col=col,lty=lty,
                                            lwd=(1-(k-1)/length(ct$ci$levels))*ct$lwd,
                                            ),
                                          default.units="native",
                                          name=paste("trace",tc$itrace,"cilevel",k,"line",sep=""),
                                          vp=currvpTree
                                          )
                             )
          currgList <- tileSetLayer(currgList,ct$layer)
          
        } else {
          if (ct$ci$mark[k]=="") {
            
          }
        }
      }
    }


 #if (length(ct$lower)>0) {
 #     ct$lower <- as.matrix(ct$lower)
 #     ct$upper <- as.matrix(ct$upper)
 #     for (k in 1:length(ct$ci$levels)) {
 #       
 #       blocktype <- 1 + as.numeric(!ct$extrapolate$control)
 #       if ((ct$ci$mark[k]=="lines")||(ct$ci$mark[k]=="line")) {
 #         for (i in 1:n) {
 #           
 #           if (blocktype[i]==1) {
 #             lty <- ct$lty[i]
 #             col <- lighten(ct$col[i],pct=ct$lighten-0.1)
 #           }
 #           if (blocktype[i]==2) {
 #             lty=ct$lty[i]
 #             col <- ct$col[i]
 #           }
 #           currgList <- gList(currgList,
 #                              linesGrob(x=unit(c(ct$x[i],ct$x[i]),"native"),
 #                                        unit(c(ct$lower[i],ct$upper[i]),"native"),   
 #                                        gp=tileSetgpar(ct,i,col=col,lty=lty,
 #                                          lwd=(1-(k-1)/length(ct$ci$levels))*ct$lwd[i],
 #                                          ),
 #                                        default.units="native",
 #                                        name=paste("trace",tc$itrace,"cilevel",k,"line",i,sep=""),
 #                                        vp=currvpTree
 #                                        )
 #                              )
 #           currgList <- tileSetLayer(currgList,ct$layer)
 #         }
 #       } else {
 #         if (ct$ci$mark[k]=="") {
 #           
 #         }
 #       }
 #     }
 #   }
    
    

    # Plot vmarks

    # Plot hmarks
        

    # Plot lines
    if (ct$connect&&(length(ct$x)>0)) {

        # Get blocks
        blocktype <- 1 + as.numeric(!ct$extrapolate$control)
        blocks <- tileSplitBlocks(x=ct$x,lower=ct$y,upper=ct$y,blocktype=blocktype)

         # Loop over blocks and plot
        for (ib in 1:length(blocks)) {
            if ((blocks[[ib]]$blocktype>0)&&(!ct$extrapolate$omit.extrapolated||(blocks[[ib]]$blocktype==2))) {
                if (blocks[[ib]]$blocktype==1) {
                    lty <- "dashed"
                    col <- lighten(ct$col[length(ct$col)],pct=ct$lighten-0.1)
                }
                if (blocks[[ib]]$blocktype==2) {
                    lty <- ct$lty
                    col <- ct$col[length(ct$col)]
                }
                
                currgList <- gList(currgList,
                                   linesGrob(x=blocks[[ib]]$x,
                                             y=blocks[[ib]]$lower,   #blocks[[ib]]$lower,
                                             gp=tileSetgpar(ct,1,lty=lty,col=col),
                                             default.units="native",
                                             name=paste("trace",tc$itrace,"line","block",ib,sep=""),
                                             vp=currvpTree
                                             )
                                   )
                
                currgList <- tileSetLayer(currgList,ct$layer)
            }
        }
    }
    


    # Plot markers
    blocktype <- 1 + as.numeric(!ct$extrapolate$control)
    addmarkers <- ct$markers
    if ((addmarkers)&&(length(ct$x)>0)) {
      col <- rep(NA,n)
      for (i in 1:n) {
        if ((blocktype[i]>0)&&(!ct$extrapolate$omit.extrapolated||(blocktype[i]==2))) {
          if (ct$extrapolate$control[i])
            col[i] <- lighten(ct$col[i],pct=ct$lighten-0.1)
          else
            col[i] <- ct$col[i]
        } 
      }
      currgList <- gList(currgList,
                         pointsGrob(x=ct$x,
                                    y=ct$y,
                                    pch=ct$pch,
                                    size=ct$size,   # why commented out?
                                    gp=tileSetgpar(ct,1:n,col=col),
                                    default.units="native",
                                    name=paste("trace",tc$itrace,"points",sep=""),
                                    vp=currvpTree                                         
                                    )
                         )
      currgList <- tileSetLayer(currgList,ct$layer)
    }


    
    # Plot markers
    #blocktype <- 1 + as.numeric(!ct$extrapolate$control)
    #addmarkers <- ct$markers
    #if ((addmarkers)&&(length(ct$x)>0)) {
    #    for (i in 1:n) {
    #        if ((blocktype[i]>0)&&(!ct$extrapolate$omit.extrapolated||(blocktype[i]==2))) {
    #            if (ct$extrapolate$control[i])
    #                col <- lighten(ct$col[i],pct=ct$lighten-0.1)
    #            else
    #                col <- ct$col[i]
    #            currgList <- gList(currgList,
    #                               pointsGrob(x=ct$x[i],
    #                                          y=ct$y[i],
    #                                          pch=ct$pch[i],
    #                                          size=ct$size[i],   
    #                                          gp=tileSetgpar(ct,i,col=col),
    #                                          default.units="native",
    #                                          name=paste("trace",tc$itrace,"points",i,sep=""),
    #                                          vp=currvpTree                                         
    #                                          )
    #                               )
    #            currgList <- tileSetLayer(currgList,(ct$layer+ct$extrapolate$control[i]))
    #        }
    #    }
    #}

  
    # Plot text labels
    addlabels <- (!is.null(ct$labels)
                  )
    if ((addlabels)&&(length(ct$x)>0)) {
        for (i in 1:n) {
            if ((blocktype[i]>0)&&(!ct$extrapolate$omit.extrapolated||(blocktype[i]==2))) {
                if (ct$extrapolate$control[i])
                    col <- lighten(ct$col[i],pct=ct$lighten-0.1)
                else
                    col <- ct$col[i]
                currgList <- gList(currgList,
                                   textGrob(x=unit(ct$x[i],"native")+unit(ct$labelsxoffset[i],"npc"),
                                            y=unit(ct$y[i],"native")+unit(ct$labelsyoffset[i],"npc"),
                                            label=ct$labels[i],
                                            just=ct$just[i],
                                            hjust=ct$hjust[i],
                                            vjust=ct$vjust[i],
                                            rot=ct$rot[i],
                                            check.overlap=ct$check.overlap[i],
                                            gp=tileSetgpar(ct,i,col=col),
                                            default.units="native",
                                            name=paste("trace",tc$itrace,"labels",i,sep=""),
                                            vp=currvpTree
                                            )
                                   )
                currgList <- tileSetLayer(currgList,(ct$layer-3))
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
                       gp=gpar(fontsize=tc$fsize),
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

