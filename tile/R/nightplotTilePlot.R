nightplotTilePlot <- function(tc=list(),...) {    
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

    # Calculate adjustment constant
    adjconst <- (abs(curymax-curymin)/abs(curxmax-curxmin)) * (tc$height$plot[tc$iplot]/tc$width$plot[tc$iplot])
    adjconst <- tc$height$plot[tc$iplot]/tc$width$plot[tc$iplot]

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
    
    if (ct$bw==FALSE) {

      truthcol <- "lightblue"
        currgList <- gList(currgList,
                           polygonGrob(x=c(curxmin,curxmin,curxmax,curxmax),
                                       y=c(curymin,curymax,curymax,curymin),
                                       gp=gpar(col="black",
                                       fill="black"),
                                       default.units="native",
                                       name=paste("trace",tc$itrace,"background",sep=""),
                                       vp=currvpTree
                                       )
                           )
        currgList <- tileSetLayer(currgList,ct$layer+10)
      } else {
        truthcol="blue"
      }
 
    
    if ( (ct$boundtype=="xyb")||(ct$boundtype=="xby") ) {

        if (ct$boundtype=="xyb") {
            data <- cbind(ct$x,
                          ct$y,
                          ct$lower,
                          ct$upper,
                          (ct$upper - ct$lower))

            data[,1] <- (data[,1]-curxmin)/(curxmax-curxmin)
            data[,2:4] <- (data[,2:4]-curymin)/(curymax-curymin)  
          #data[,5] <- (data[,5])/(curymax-curymin)
            data[,5] <- abs(data[,5])/abs(curymax-curymin)
            for (i in 1:nrow(data))
                data[i,5] <- min(data[i,5],1)
        } else {
            data <- cbind(ct$y,
                          ct$x,
                          ct$hlower,
                          ct$hupper,
                          (ct$hupper-ct$hlower))

            data[,2:4] <- (data[,2:4]-curxmin)/(curxmax-curxmin)
            data[,1] <- (data[,1]-curymin)/(curymax-curymin)
            data[,5] <- abs(data[,5])/abs(curxmax-curxmin)
            for (i in 1:nrow(data))
                data[i,5] <- min(data[i,5],1)
        }

        data <- sortmc(data,5,decreasing=TRUE)
        data <- na.omit(data)

        
        if (ct$bw) 
            colorp <- rgb(red=0.88 - 0.88*(1-data[,5]),
                          green=0.88 - 0.88*(1-data[,5]),
                          blue=0.88 - 0.88*(1-data[,5]),
                          maxColorValue=1)
        else
            colorp <- rgb(red=0.2 + 0.8*(1-data[,5]),
                          green=0.2 + 0.8*(1-data[,5]),
                          blue=0.2 +
                          0.8*(data[,5]<0.5)*data[,5] +
                          0.8*(data[,5]>=0.5)*(1-data[,5]),                          
                          maxColorValue=1)


    
        linearea <- ct$size[tc$iplot]*0.001*(tc$width$plot[tc$iplot]*tc$height$plot[tc$iplot])
    
        #linearea <- ct$size[tc$iplot]*0.001#*(curxmax-curxmin)*(curymax-curymin)
#               (tc$height$graph*tc$width$graph))

        radiusCartouche <- as.vector(data[,1])
        for (i in 1:nrow(data)) {
            r1 <- min(posquad(pi, 0, -linearea))
            r2 <- min(posquad(pi-4, 2*data[i,5], -linearea))
            if (is.na(r1)) {
                radiusCartouche[i] <- r2
            } else {
                if (is.na(r2)) {
                    radiusCartouche[i] <- r1
                } else {
                    if ((data[i,5]-2*r2)>0)
                        radiusCartouche[i] <- r2
                    else
                        radiusCartouche[i] <- r1
                }
            }
        }
        
        for (i in 1:nrow(data)){

            if (data[i,5]<(2*radiusCartouche[i]))
                yCartouche <-  c(mean(c(data[i,3],data[i,4])),
                                 mean(c(data[i,3],data[i,4]))
                                 )
            else
                yCartouche <- c(data[i,3]+radiusCartouche[i],data[i,4]-radiusCartouche[i])
             

            if (ct$boundtype=="xyb") {
                xplot <- c(data[i,1],data[i,1])
                yplot <- yCartouche
                xpoly <- c(xplot[1]-radiusCartouche[i],xplot[2]+radiusCartouche[i],xplot[2]+radiusCartouche[i],xplot[1]-radiusCartouche[i])
                ypoly <- c(yplot[1],yplot[1],yplot[2],yplot[2])
            } else {
                xplot <- yCartouche
                yplot <- c(data[i,1],data[i,1])
                xpoly <- c(xplot[1],xplot[2],xplot[2],xplot[1])
                ypoly <- c(yplot[1]-radiusCartouche[i],yplot[1]-radiusCartouche[i],yplot[2]+radiusCartouche[i],yplot[2]+radiusCartouche[i])
            }

            # Create circle dimensions
            if (ct$boundtype=="xyb") {
                circlewidth <- unit(radiusCartouche[i]*2,"npc")
                circleheight <- unit(radiusCartouche[i]*2/adjconst,"npc")
            } else {
                circlewidth <- unit(radiusCartouche[i]*2/adjconst,"npc")
                circleheight <- unit(radiusCartouche[i]*2,"npc")
            }
            
            
            # plot bottom circle
            upViewport(0)
            
            currvp0 <- viewport(x=unit(xplot[1],"npc"),
                                y=unit(yplot[1],"npc"),
                                width=circlewidth,
                                height=circleheight,                      
                                gp=gpar(fontsize=tc$fsize),                                            # Remove 6?
                                name=paste("plot",tc$iplot,"trace",tc$itrace,"cartouchebottom",i,sep=""),
                                clip="inherit"
                                )

            currvpTree0 <- vpTree(tc$mainvp,
                                  vpList(vpStack(currvp,
                                          currvp0))
                                  )

            pushViewport(currvpTree0)

            currgList <- gList(currgList,
                               circleGrob(name=paste("trace",tc$itrace,"cartouchebottom",i,sep=""),
                                          vp=currvpTree0,
                                          gp=gpar(col=NA,
                                                  fill=colorp[i],
                                          lwd=0)
                                          )#,
                               #rectGrob(name=paste("trace",tc$itrace,"cartouchebottombox",i,sep=""),
                               #          vp=currvpTree0,
                               #         gp=gpar(col="white")
                               #         )
                               )
            currgList <- tileSetLayer(currgList,ct$layer+3)
            upViewport(0)


            # plot top circle
            currvp0 <- viewport(x=unit(xplot[2],"npc"),
                                y=unit(yplot[2],"npc"),
                                width=circlewidth,
                                height=circleheight,
                        
                                gp=gpar(fontsize=tc$fsize),                                            # Remove 6?
                                name=paste("plot",tc$iplot,"trace",tc$itrace,"cartouchetop",i,sep=""),
                                clip="inherit"
                                )
           
            currvpTree0 <- vpTree(tc$mainvp,
                                  vpList(vpStack(currvp,
                                          currvp0))
                                  )
           
            pushViewport(currvpTree0)

            currgList <- gList(currgList,
                               circleGrob(name=paste("trace",tc$itrace,"cartouchetop",i,sep=""),
                                          vp=currvpTree0,
                                          gp=gpar(col=NA,
                                                  fill=colorp[i],
                                                  lwd=0)
                                          )
                               )
            currgList <- tileSetLayer(currgList,ct$layer+3)
            upViewport(0)
            
            # Restore plot viewport
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

            # plot rectangle
            currgList <- gList(currgList,
                               polygonGrob(xpoly,
                                           ypoly,
                                           name=paste("trace",tc$itrace,"cartouchemiddle",i,sep=""),
                                           vp=currvpTree,
                                           gp=gpar(lwd=0,
                                           col=NA,
                                           fill=colorp[i])
                                           )
                               )
            currgList <- tileSetLayer(currgList,ct$layer+3)
        }

        xplot <- yplot <- NULL
        for (i in 1:nrow(data)) {
          if (ct$boundtype=="xyb") {
            xplot <- c(xplot,data[i,1]-radiusCartouche[i],data[i,1]+radiusCartouche[i])
            yplot <- c(yplot,data[i,2],data[i,2])
          } else {
            xplot <- c(xplot,data[i,2],data[i,2])
            yplot <- c(yplot,data[i,1]-radiusCartouche[i],data[i,1]+radiusCartouche[i])
          }
        }
        currgList <- gList(currgList,
                           polylineGrob(x=xplot,
                                        y=yplot,
                                        id.length=rep(2,nrow(data)),
                                        gp=gpar(col=ct$col,  #"red"
                                          lwd=ct$lwd,  #.5
                                          lineend="butt"
                                          ),
                                        default.units="npc",
                                        name=paste("trace",tc$itrace,"center",sep=""),
                                        vp=currvpTree
                                        )
                           )
        currgList <- tileSetLayer(currgList,ct$layer)
        
            
            
        # Add red horiz lines at y.
        # width prop to 2*r
        # Brightness prop to 1/bndwd

        

        #for (i in 1:nrow(data)) {
        #    if (ct$boundtype=="xyb") {
        #        xplot <- c(data[i,1]-radiusCartouche[i],data[i,1]+radiusCartouche[i])
        #        yplot <- c(data[i,2],data[i,2])
        #    } else {
        #        xplot <- c(data[i,2],data[i,2])
        #        yplot <- c(data[i,1]-radiusCartouche[i],data[i,1]+radiusCartouche[i])
        #    }
        #    currgList <- gList(currgList,
        #                       linesGrob(x=xplot,
        #                                  y=yplot,
        #                                  gp=gpar(col=ct$col,  #"red"
        #                                  lwd=ct$lwd,  #.5
        #                                  lineend="butt"
        #                                  ),
        #                                  default.units="npc",
        #                                  name=paste("trace",tc$itrace,"center",i,sep=""),
        #                                  vp=currvpTree
        #                                  )
        #                       )
        #    currgList <- tileSetLayer(currgList,ct$layer)
        #}




        

        
        for (i in 1:nrow(data)) {
            if (!is.na(ct$truex[i])) {
                 # Create circle dimensions
                #radiusCartouche <- as.vector(data[,1])
                #for (i in 1:nrow(data)) {
                #    r1 <- posquad(pi, 2*ct$incr, -linearea)
                #    r2 <- posquad(pi-4, 2*data[i,5], ct$incr*data[i,5]-linearea)[1]
                #    if (is.na(r1)) radiusCartouche[i] <- r2
                 #   else {
                 #       if (is.na(r2)) radiusCartouche[i] <- r1
                 #       else
                 #           if ((-4*r2+2*r2*(data[i,5]-ct$incr)+ct$incr*data[i,5])>0) 
                 #               radiusCartouche[i] <- r1
                 #           else
                 #               radiusCartouche[i] <- r2
                 #   }
                                        #radiusCartouche[i] <- posquad(pi,2*(incr+data[i,5]),incr*data[i,5]-linearea)
                #}
                
                if (ct$boundtype=="xyb") {
                    circlewidth <- unit(radiusCartouche[i]*2,"npc")
                    circleheight <- unit(radiusCartouche[i]*2/adjconst,"npc")
                    plotx <- unit(ct$truex[i],"native")
                    ploty <- unit(ct$truey[i],"native")
                } else {
                    circlewidth <- unit(radiusCartouche[i]*2/adjconst,"npc")
                    circleheight <- unit(radiusCartouche[i]*2,"npc")
                    ploty <- unit(ct$truey[i],"native")
                    plotx <- unit(ct$truex[i],"native")
                }
                
                
                # plot truth circle
                upViewport(0)

                currvp0 <- viewport(x=plotx,
                                    y=ploty,
                                    width=circlewidth,
                                    height=circleheight,                      
                                    gp=gpar(fontsize=tc$fsize),                                            # Remove 6?
                                    name=paste("plot",tc$iplot,"trace",tc$itrace,"truth",i,sep=""),
                                    clip="on"
                                    )
                
                currvpTree0 <- vpTree(tc$mainvp,
                                      vpList(vpStack(currvp,
                                                     currvp0))
                                      )
                
                pushViewport(currvpTree0)
                
                currgList <- gList(currgList,
 
                                   circleGrob(name=paste("trace",tc$itrace,"truth",i,sep=""),
                                              vp=currvpTree0,
                                              gp=gpar(col=truthcol,
                                              fill=NA,
                                              lwd=2)
                                              )#,
                               #rectGrob(name=paste("trace",tc$itrace,"truthcirclebox",i,sep=""),
                               #          vp=currvpTree0,
                               #         gp=gpar(col="white")
                               #         )
                                   )
                currgList <- tileSetLayer(currgList,ct$layer)
                upViewport(0)
            }
        }

           # Restore plot viewport
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
        
  }  # End type xby xyb


 # type xyby

    if (ct$boundtype=="xbyb") {
    
        data <- cbind(ct$x,
                      ct$y,
                      ct$lower,
                      ct$upper,
                      ct$upper - ct$lower,
                      ct$hlower,
                      ct$hupper,
                      ct$hupper - ct$hlower)
        data[,c(1,6:8)] <- (data[,c(1,6:8)]-curxmin)/(curxmax-curxmin)
        data[,2:5] <- (data[,2:5]-curymin)/(curymax-curymin)
        data <- cbind(data,data[,5]*data[,8])
        data <- sortmc(data,9,decreasing=TRUE)
        data <- na.omit(data)
        
        if (ct$bw) 
            colorp <- rgb(red=1 - 0.95*(1-data[,9])^3,
                          green=1 - 0.95*(1-data[,9])^3,
                          blue=1 - 0.95*(1-data[,9])^3,
                          maxColorValue=1)
        else
            colorp <- rgb(red=0.05+ 0.95*(1-data[,9])^4,
                          green=0.05 + 0.95*(1-data[,9])^4,
                          blue=0.05+
                          0.95*(data[,9]<0.1)*(data[,9])^6 +
                          0.95*(data[,9]>=0.1)*(1-data[,9])^4,                          
                          maxColorValue=1)

      #print(tc$width$plot*tc$height$graph)
      linearea <- ct$size[tc$iplot]*0.001#*(tc$width$plot*tc$height$plot)
      
      for (i in 1:nrow(data)) {
        areabox <- data[i,5]*data[i,8]
        if (areabox<linearea)  # area of box
          {
            r1 <- posquad(pi,2*ct$incr,-linearea)
            r2 <- posquad(pi-4,2*data[i,5],ct$incr*data[i,5]-linearea)[1]
            if (is.na(r1)) radiusCartouche <- r2
            else {
              if (is.na(r2)) radiusCartouche <- r1
              else
                if ((-4*r2+2*r2*(data[i,5]-ct$incr)+ct$incr*data[i,5])>0) 
                  radiusCartouche <- r1
                else
                  radiusCartouche <- r2
            }

            polyvert <- tileCartouche(x=xplot,
                                      y=yplot,
                                      c = radiusCartouche[i],
                                      incr=ct$incr,
                                      arcsegments=ct$arcsegments)
            
            currgList <- gList(currgList,
                               polygonGrob(x=polyvert$x,
                                           y=polyvert$y,
                                           gp=gpar(col=colorp[i],
                                             lwd=0,
                                             fill=colorp[i]
                                             ),
                                           default.units="npc",
                                           name=paste("trace",tc$itrace,"cartouche",i,sep=""),
                                           vp=currvpTree
                                           )
                               )
            currgList <- tileSetLayer(currgList,ct$layer+3)
        } else {
            xplot <- c(data[i,6],data[i,6],data[i,7],data[i,7],data[i,6])
            yplot <- c(data[i,3],data[i,4],data[i,4],data[i,3],data[i,3])
            
            
            currgList <- gList(currgList,
                               polygonGrob(x=xplot,
                                           y=yplot,
                                           gp=gpar(col=colorp[i],
                                           lwd=0,
                                           fill=colorp[i]
                                           ),
                                           default.units="npc",
                                           name=paste("trace",tc$itrace,"cartouche",i,sep=""),
                                           vp=currvpTree
                                           )
                               )
            currgList <- tileSetLayer(currgList,ct$layer+3)
          }
      }
    }
  

 # type xy

  # circles of area as above


    # Create no clip viewport (not currently used)
    upViewport(0)
    currvp <- viewport(
                       layout.pos.col=tc$curplotcol,
                       layout.pos.row=tc$curplotrow,
                       xscale=c(curxmin,curxmax),
                       yscale=c(curymin,curymax),
                       gp=gpar(fontsize=tc$fsize),
                       name=paste("plot",tc$iplot,"trace",tc$itrace,"clipped",sep=""),,
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

  
