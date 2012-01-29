rugTilePlot <-
function(tc=list(),...) {
  
  currgList <- NULL
  
  ct <- tc$traces[[tc$itrace]]
  
  if (ct$attachToX) {

    data <- ct$x
    
    offset1 <- 0
    offset2 <- 0-ct$thickness

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
  if (ct$attachToTop) {

    data <- ct$top
    
    offset1 <- 1
    offset2 <- 1+ct$thickness
    
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
  if (ct$attachToY) {

    data <- ct$y
    
    offset1 <- 0
    offset2 <- 0-ct$thickness

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
  if (ct$attachToRight) {

    data <- ct$right
    
    offset1 <- 1
    offset2 <- 1+ct$thickness
    
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

  # Push viewport
  currvp <- viewport(layout.pos.col=tc$curplotcol,
                     layout.pos.row=tc$curplotrow,
                     xscale=c(curxmin,curxmax),
                     yscale=c(curymin,curymax),
                     #gp=gpar(fontsize=tc$fsize*6),                                            # Remove 6?
                     name=paste("plot",tc$iplot,"trace",tc$itrace,sep=""),
                     clip="off"
                     )
  
  currvpTree <- vpTree(tc$mainvp,
                       vpList(currvp)
                       ) 
  
  pushViewport(currvpTree)    

  # Adjust offsets
  offsetdiff <- convertHeight(unit(abs(offset2 - offset1), "char") , "npc", valueOnly = TRUE)
  if (offset2<offset1) offsetdiff <- -offsetdiff
  offset2 <- offset1 + offsetdiff

  ## Plot elements
  if (ct$type=="lines") {
    if (ct$attachToX||ct$attachToTop)  {
      xdata <- sort(c(data,data))
      ydata <- rep(c(offset1,offset2),length(data))
      id <- sort(c(seq(1:length(data)),seq(1:length(data))))     
      currgList <- gList(currgList,
                         polylineGrob(x=unit(xdata,"native"),
                                      y=unit(ydata,"npc"),
                                      id=id,
                                      gp=tileSetgpar(ct,1),
                                      default.units="native",
                                      name=paste("rug",sep=""),
                                      vp=currvpTree
                                      )
                         )
      currgList <- tileSetLayer(currgList,ct$layer)
      
    }
    if (ct$attachToY||ct$attachToRight)  {
      ydata <- sort(c(data,data))
      xdata <- rep(c(offset1,offset2),length(data))
      id <- sort(c(seq(1:length(data)),seq(1:length(data))))
      currgList <- gList(currgList,
                         polylineGrob(y=unit(ydata,"native"),
                                      x=unit(xdata,"npc"),
                                      id=id,
                                      gp=tileSetgpar(ct,1),
                                      default.units="native",
                                      name=paste("rug",sep=""),
                                      vp=currvpTree
                                      )
                         )
      currgList <- tileSetLayer(currgList,ct$layer)
    }
  }
  
  if (ct$type=="density") {
    
    if (ct$attachToX||ct$attachToTop)  {
      density <- density(na.omit(data),
                         kernel=ct$kernel[1],
                         from=limits[1],
                         to=limits[2])
                                        #density$x <- log.axis(density$x,tc[[atype]]$log)
      currgList <- gList(currgList,
                         polygonGrob(x=unit(c(0,density$x,1),"native"),
                                     y=unit(c(offset1,(density$y)*(offset2-offset1)+offset1,offset1),"npc"),
                                     gp=tileSetgpar(ct,1),
                                     default.units="native",
                                     name=paste("rug",sep=""),
                                     vp=currvpTree
                                     )
                         )
      currgList <- tileSetLayer(currgList,ct$layer)
    }
    
    if (ct$attachToY||ct$attachToRight)  {
      density <- density(na.omit(data),
                         kernel=ct$kernel[1],
                         from=limits[1],
                         to=limits[2])
                                        #density$x <- log.axis(density$x,tc[[atype]]$log)
      currgList <- gList(currgList,
                         polygonGrob(x=unit(c(offset1,(density$y)*(offset2-offset1)+offset1,offset1),"npc"),
                                     y=unit(c(0,density$x,1),"native"),
                                     gp=tileSetgpar(ct,1),
                                     default.units="native",
                                     name=paste("rug",sep=""),
                                     vp=currvpTree
                                     )
                         )
      currgList <- tileSetLayer(currgList,ct$layer)
    }
    
    
    
  }
  
  if (ct$type=="dots") {
    hist <- hist(data,plot=FALSE,breaks=50)
    maxcount <- max(hist$counts)
    for (i in 1:length(hist$counts)) {
      if (hist$counts[i]) {
        for (j in 1:hist$counts[i]) {
          if (ct$attachToX||ct$attachToTop)  {
            currgList <- gList(currgList,
                               pointsGrob(x=unit(hist$mids[i],"native"),
                                          y=unit(offset1 + j/maxcount*(offset2-offset1),
                                            "npc"),
                                          pch=".",
                                          gp=tileSetgpar(ct,1),
                                          default.units="native",
                                          name=paste("rug",i,".",j,sep=""),
                                          vp=currvpTree
                                          )
                               )
            currgList <- tileSetLayer(currgList,ct$layer)
          } else {
            currgList <- gList(currgList,
                               pointsGrob(y=unit(hist$mids[i],"native"),
                                          x=unit(offset1 + j/maxcount*(offset2-offset1),
                                            "npc"),
                                          pch=".",
                                          gp=tileSetgpar(ct,1),
                                          default.units="native",
                                          name=paste("rug",i,".",j,sep=""),
                                          vp=currvpTree
                                          )
                               )
            currgList <- tileSetLayer(currgList,ct$layer)
          }
        }
      }
    }
  }     
    
  if (ct$type=="jitter") {
    
    if (ct$attachToX||ct$attachToTop)  {
      currgList <- gList(currgList,
                         pointsGrob(x=unit(data,"native"),
                                    y=unit(runif(length(data),
                                      min=min(offset1,offset2),
                                      max=max(offset1,offset2)),"npc"),
                                    pch=".",
                                    gp=tileSetgpar(ct,1),
                                    default.units="native",
                                    name=paste("rug",sep=""),
                                    vp=currvpTree
                                    )
                         )
      currgList <- tileSetLayer(currgList,ct$layer)
    }
    
    if (ct$attachToY||ct$attachToRight)  {
      currgList <- gList(currgList,
                         pointsGrob(y=unit(data,"native"),
                                    x=unit(runif(length(data),
                                      min=min(offset1,offset2),
                                      max=max(offset1,offset2)),"npc"),
                                    pch=".",
                                    gp=tileSetgpar(ct,1),
                                    default.units="native",
                                    name=paste("rug",sep=""),
                                    vp=currvpTree
                                    )
                         )
      currgList <- tileSetLayer(currgList,ct$layer)
    }
  }
  
  if (ct$type=="histogram") {
    
  }
  
  
  upViewport(0)
  
  # Return result
  tc$currgList <- gList(tc$currgList,currgList)    
  tc    
}

