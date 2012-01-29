lineplotTilePlot <-
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
                       gp=gpar(fontsize=1),#tc$fsize*6),                                            # Remove 6?
                       name=paste("plot",tc$iplot,"trace",tc$itrace,sep=""),
                       clip=ct$clip
                       )
    currvpTree <- vpTree(tc$mainvp,
                       vpList(currvp)
                       )
    pushViewport(currvpTree)


#currgList <- titlebox(currvpTree,currgList)


    
    #currgList <- tileSetLayer(currgList,5)

  
    ## Plot elements
    
    # Plot fit
    # Add fits to traces  NEED extrapolate
    if (!is.null(ct$fit$method)) {
        for (m in 1:length(ct$fit$method)) {

            # Do fitting
            fit <- eval(call(paste("tilefit",ct$fit$method[m],sep=""),
                             ct$y,
                             ct$x,
                             ct$fit$ci)
                        )
            if (!is.null(fit)) {
            # Plot fit CI
              if (!is.null(ct$fit$ci)) {
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
                                                 ),
                                       linesGrob(x=fit$x,
                                                 y=fit$upper[,k],
                                                 gp=gpar(lty="dashed",
                                                   col=ct$fit$col[m]),
                                                 default.units="native",
                                                 name=paste("trace",tc$itrace,"fit",m,"upper",k,sep=""),
                                                 vp=currvpTree
                                                 )
                                       )                          
                  } else {
                    if (ct$fit$mark[k]=="shaded") {
                      xpoly <- c(fit$x,rev(fit$x),fit$x[1])
                      ypoly <- c(fit$lower[,k],rev(fit$upper[,k]),fit$lower[1,k])
                      pcol <- lighten(ct$fit$col[m],ct$lighten)
                      currgList <- gList(currgList,
                                         polygonGrob(x=xpoly,
                                                     y=ypoly,
                                                     gp=gpar(col=NA,#pcol,
                                        #border=FALSE,
                                                       fill=pcol,
                                                       lineend="butt",
                                                       linejoin="mitre",
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
            }
          }
      }

    # Plot CIs
    if (length(ct$lower)>0) {
       
        
        if (length(ct$lower)>length(ct$x)) {
          ct$lower <- matrix(ct$lower,nrow=length(ct$x),ncol=length(ct$lower)/length(ct$x))
          ct$upper <- matrix(ct$upper,nrow=length(ct$x),ncol=length(ct$upper)/length(ct$x))          
        } else {
          ct$lower <- as.matrix(ct$lower)
          ct$upper <- as.matrix(ct$upper)
        }
          

        
        for (k in 1:ncol(ct$lower)) {
            # Get blocks
            blocktype <- 1 + as.numeric(!ct$extrapolate$control)
            blocks <- tileSplitBlocks(x=ct$x,lower=ct$lower[,k],upper=ct$upper[,k],blocktype=blocktype)
            
          
            # Loop over blocks and plot
            for (ib in 1:length(blocks)) {
                if ((blocks[[ib]]$blocktype>0)&&(!ct$extrapolate$omit.extrapolated||(blocks[[ib]]$blocktype==2))) {
                    if (ct$ci$mark[k]=="dashed") {
                        if (blocks[[ib]]$blocktype==1) {
                            lty <- "dotdash"
                            col <- lighten(ct$col[length(ct$col)],pct=ct$lighten)
                        }
                        if (blocks[[ib]]$blocktype==2) {
                            lty <- "dashed"
                            col <- ct$col[length(ct$col)]
                        }
                        currgList <- gList(currgList,
                                           linesGrob(x=blocks[[ib]]$x,
                                                     y=blocks[[ib]]$lower,
                                                     gp=gpar(lty=lty,
                                                     col=col),                            
                                                     default.units="native",
                                                     name=paste("trace",tc$itrace,"lower",k,"block",ib,sep=""),
                                                     vp=currvpTree
                                                     )
                                           )
                        currgList <- tileSetLayer(currgList,ct$layer)
                        
                        currgList <- gList(currgList,
                                           linesGrob(x=blocks[[ib]]$x,
                                                     y=blocks[[ib]]$upper,
                                                     gp=gpar(lty=lty,
                                                     col=col),
                                                     default.units="native",
                                                     name=paste("trace",tc$itrace,"upper",k,"block",ib,sep=""),
                                                     vp=currvpTree
                                                     )
                                           )
                        currgList <- tileSetLayer(currgList,ct$layer)
                        
                    } else {
                        if (ct$ci$mark[k]=="shaded") {
                            if (blocks[[ib]]$blocktype==1) {
                                col <- lighten(ct$col[length(ct$col)],pct=min((ct$lighten+0.1),0.95))
                            }
                            if (blocks[[ib]]$blocktype==2) {       
                                col <- lighten(ct$col[length(ct$col)],pct=ct$lighten)
                            }
                            currgList <- gList(currgList,
                                               polygonGrob(x=c(blocks[[ib]]$x,rev(blocks[[ib]]$x)),
                                                           y=c(blocks[[ib]]$lower,rev(blocks[[ib]]$upper)),
                                                           gp=gpar(col=NA,
                                                                   #border=FALSE,
                                                                   lty="solid",
                                                                   fill=col,
                                                                   alpha=ct$polyalpha
                                                                   ),
                                                           default.units="native",
                                                           name=paste("trace",tc$itrace,"ci",k,"block",ib,sep=""),
                                                           vp=currvpTree
                                                           )
                                               )
                            currgList <- tileSetLayer(currgList,(ct$layer+4))
                        }
                    }
                }
            }
        }
    }
    
    # Plot vmarks

    # Plot hmarks

    # Plot lines
    if ((length(ct$x)>0)&&(length(ct$y)>0)) {
      
        # Get blocks
        blocktype <- 1 + as.numeric(!ct$extrapolate$control)
        blocks <- tileSplitBlocks(x=ct$x,lower=ct$y,upper=ct$y,blocktype=blocktype)

         # Loop over blocks and plot
            for (ib in 1:length(blocks)) {
                if ((blocks[[ib]]$blocktype>0)&&(!ct$extrapolate$omit.extrapolated||(blocks[[ib]]$blocktype==2))) {
                    if (blocks[[ib]]$blocktype==1) {
                        lty <- "longdash"
                        col <- lighten(ct$col[length(ct$col)],pct=ct$lighten)
                    }
                    if (blocks[[ib]]$blocktype==2) {
                        lty <- ct$lty
                        col <- ct$col[length(ct$col)]
                    }

                    currgList <- gList(currgList,
                                       linesGrob(x=blocks[[ib]]$x,
                                                 y=blocks[[ib]]$lower,
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
    addmarkers <- (ct$markers#||
                   #is.null(ct$labels)||
                   #is.null(ct$labeloffset[1:2,icur])||
                   #is.na(ct$labeloffset[1:2,icur])
                   )
    if ((addmarkers)&&(length(ct$x)>0)) {
        for (i in 1:n) {
            if ((blocktype[i]>0)&&(!ct$extrapolate$omit.extrapolated||(blocktype[i]==2))) {
                if (ct$extrapolate$control[i])
                    col <- lighten(ct$col[i],pct=ct$lighten)
                else
                    col <- ct$col[i]
                currgList <- gList(currgList,
                                   pointsGrob(x=ct$x[i],
                                              y=ct$y[i],
                                              pch=ct$pch[i],
                                        #size=ct$size[i],
                                              gp=tileSetgpar(ct,i,col=col),
                                              default.units="native",
                                              name=paste("trace",tc$itrace,"points",i,sep=""),
                                              vp=currvpTree                                         
                                              )
                                   )
                currgList <- tileSetLayer(currgList,(ct$layer-1))
            }
        }
    }

    # Plot text labels
    addlabels <- (!is.null(ct$labels)
                  )
    if ((addlabels)&&(length(ct$x)>0)) {
         for (i in 1:n) {
             if ((blocktype[i]>0)&&(!ct$extrapolate$omit.extrapolated||(blocktype[i]==2))) {
                 if (ct$extrapolate$control[i])
                     col <- lighten(ct$col[i],pct=ct$lighten)
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

   
   
    upViewport(0)


    # Return result
    tc$currgList <- gList(tc$currgList,currgList)    
    tc    
}

