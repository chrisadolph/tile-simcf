tileHarmonizeAxes <- function(tc) {


     # Check for rugs
    for (j in 1:length(tc$traces)) {
      if (tc$traces[[j]]$graphic=="rug") {
        if (tc$traces[[j]]$attachToX) {
          tc[["xaxis"]]$rug[ tc$traces[[j]]$plot ] <- TRUE
          tc$height$xaxis.rug[tc$traces[[j]]$plot] <- max(tc$height$xaxis.rug[tc$traces[[j]]$plot], tc$traces[[j]]$thickness)
        }
        if (tc$traces[[j]]$attachToY) {
          tc[["yaxis"]]$rug[ tc$traces[[j]]$plot ] <- TRUE
          tc$width$yaxis.rug[tc$traces[[j]]$plot] <- max(tc$width$yaxis.rug[tc$traces[[j]]$plot], tc$traces[[j]]$thickness)
        }
        if (tc$traces[[j]]$attachToTop) {
          tc[["topaxis"]]$rug[ tc$traces[[j]]$plot ] <- TRUE
          tc$height$topaxis.rug[tc$traces[[j]]$plot] <- max(tc$height$topaxis.rug[tc$traces[[j]]$plot], tc$traces[[j]]$thickness)
        }
        if (tc$traces[[j]]$attachToRight) {
          tc[["rightaxis"]]$rug[ tc$traces[[j]]$plot ] <- TRUE
          tc$width$rightaxis.rug[tc$traces[[j]]$plot] <- max(tc$width$rightaxis.rug[tc$traces[[j]]$plot], tc$traces[[j]]$thickness)
        }
      }
    }
  
    atype <- c("xaxis","yaxis","topaxis","rightaxis")
    dtype <- c("x","y","x","y")
    attachtoaxis <- c("attachToX","attachToY","attachToTop","attachToRight")
    limitmin <- c(1,3,5,7)
    limitmax <- c(2,4,6,8)
    
    # Apply specials Loop over plots
    for (i in 1:tc$nplots) {

        # Initialize vectors for specials
        heightplot <- widthplot <- limits <- NULL
        
        # Loop over traces
        tracesadded <- 0
        for (j in 1:length(tc$traces)) {
            cond <- ((tc$traces[[j]]$plot==i) ) #&& tc$traces[[j]][[attachtoaxis[k]]] )
            
            # Check if add trace
            if (cond) {

                tc$useplot[i] <- TRUE
                
                # Check for special axis titles

                if (!is.null(tc$traces[[j]]$special$axislabels$attach)) {
                    
                    tc$special[[paste(tc$traces[[j]]$special$axislabels$attach,"axistitle",sep="")]][i] <-
                        tc$traces[[j]]$graphic
                    tc$special[[paste(tc$traces[[j]]$special$axislabels$attach,"axistitleControl",sep="")]][[i]]$x <-
                        tc$traces[[j]]$x
                    tc$special[[paste(tc$traces[[j]]$special$axislabels$attach,"axistitleControl",sep="")]][[i]]$y <-
                        tc$traces[[j]]$y
                    tc$special[[paste(tc$traces[[j]]$special$axislabels$attach,"axistitleControl",sep="")]][[i]]$top <-
                        tc$traces[[j]]$top
                    tc$special[[paste(tc$traces[[j]]$special$axislabels$attach,"axistitleControl",sep="")]][[i]]$right <-
                        tc$traces[[j]]$right

                    if (length(tc$traces[[j]]$labels)>0)
                      tc$special[[paste(tc$traces[[j]]$special$axislabels$attach,"axistitleControl",sep="")]][[i]]$labels <-
                        tc$traces[[j]]$labels
                  
                    tc$special[[paste(tc$traces[[j]]$special$axislabels$attach,"axistitleControl",sep="")]][[i]]$cex <-
                        tc$traces[[j]]$cex
                    tc$special[[paste(tc$traces[[j]]$special$axislabels$attach,"axistitleControl",sep="")]][[i]]$col <-
                        tc$traces[[j]]$col
                    tc$special[[paste(tc$traces[[j]]$special$axislabels$attach,"axistitleControl",sep="")]][[i]]$fontsize <-
                        tc$traces[[j]]$fontsize
                    tc$special[[paste(tc$traces[[j]]$special$axislabels$attach,"axistitleControl",sep="")]][[i]]$fontface <-
                        tc$traces[[j]]$fontface
                    tc$special[[paste(tc$traces[[j]]$special$axislabels$attach,"axistitleControl",sep="")]][[i]]$rot <-
                        tc$traces[[j]]$rot
                    tc$special[[paste(tc$traces[[j]]$special$axislabels$attach,"axistitleControl",sep="")]][[i]]$type <-
                        tc$traces[[j]]$type
                    tc[[paste(tc$traces[[j]]$special$axislabels$attach,"axistitle",sep="")]]$add[i] <- TRUE
                }
                

                # Create special height
                if (!is.null(tc$traces[[j]]$special$height$plot)) {
                    heightplot <- c(heightplot,tc$traces[[j]]$special$height$plot)
                }

                # Create special width
                if (!is.null(tc$traces[[j]]$special$width$plot)) {
                    widthplot <- c(widthplot,tc$traces[[j]]$special$width$plot)
                }

                # Create special limits
                if (!is.null(tc$traces[[j]]$special$limits)) {
                    limits <- rbind(limits,tc$traces[[j]]$special$limits)
                }
            }
        }

        # Modify if special

        if (!is.null(heightplot)) {
            tc$height$plot[i] <- max(heightplot)
        }
        if (!is.null(widthplot)) {
            tc$width$plot[i] <- max(widthplot)
        }
        if (!is.null(limits)) {
            for (j in c(1,3,5,7)) {
                if (length(na.omit(limits[,j]))>0) {
                    tc$limits[i,j] <- min(na.omit(limits[,j]))
                }
            }
            for (j in c(2,4,6,8)) {
                if (length(na.omit(limits[,j]))>0) {
                    tc$limits[i,j] <- max(na.omit(limits[,j]))
                }
            }
        }
        
    }
    
    #print(tc$limits)
    #stop()
    
    # Harmonize Axes:  Loop over plots
    for (i in 1:tc$nplots) {
        #Loop over all axes
        for (k in 1:4) {
            datamin <- datamax <- NA
           
            # Loop over traces
            tracesadded <- 0
            allgraph <- NULL
            xalldata <- yalldata <- rugdata <- NULL
            for (j in 1:length(tc$traces)) {
                #xalldata <- yalldata <- rugdata <- NULL
                cond <- ((tc$traces[[j]]$plot==i) && tc$traces[[j]][[attachtoaxis[k]]] )
                         
                # Check if add trace
                if (cond) {
                    tracesadded <- tracesadded + 1

                    # Collect graphic types
                    allgraph <- c(allgraph,tc$traces[[j]]$graphic)
                    
                    # Collect all relevant data
                    if (!is.null(tc$traces[[j]]$xdatalimits))
                        for (q in 1:length(tc$traces[[j]]$xdatalimits))
                            xalldata <- c(xalldata,as.numeric(tc$traces[[j]][[tc$traces[[j]]$xdatalimits[q]]]))
                    if (!is.null(tc$traces[[j]]$ydatalimits))
                        for (q in 1:length(tc$traces[[j]]$ydatalimits))
                            yalldata <- c(yalldata,as.numeric(tc$traces[[j]][[tc$traces[[j]]$ydatalimits[q]]]))
                    
                    # Modify trace for plotting if axis is transformed
                    if (is.logical(tc[[atype[k]]]$log[i])&&tc[[atype[k]]]$log[i]) {
                        if (dtype[k]=="x") {
                            if (!is.null(tc$traces[[j]]$xtransform))
                                for (q in 1:length(tc$traces[[j]]$xtransform))
                                    if (!is.null(tc$traces[[j]][[tc$traces[[j]]$xtransform[q]]])) 
                                        tc$traces[[j]][[tc$traces[[j]]$xtransform[q]]] <- log(tc$traces[[j]][[tc$traces[[j]]$xtransform[q]]])
                        }
                        if (dtype[k]=="y") {
                            if (!is.null(tc$traces[[j]]$ytransform))
                                for (q in 1:length(tc$traces[[j]]$ytransform))
                                    if (!is.null(tc$traces[[j]][[tc$traces[[j]]$ytransform[q]]]))
                                        tc$traces[[j]][[tc$traces[[j]]$ytransform[q]]] <- log(tc$traces[[j]][[tc$traces[[j]]$ytransform[q]]])
                        }                        
                    }
                    if (is.numeric(tc[[atype[k]]]$log[i])&&tc[[atype[k]]]$log[i]>0) {
                        if (dtype[k]=="x") {
                             if (!is.null(tc$traces[[j]]$xtransform))
                                for (q in 1:length(tc$traces[[j]]$xtransform))
                                    if (!is.null(tc$traces[[j]][[tc$traces[[j]]$xtransform[q]]]))
                                        tc$traces[[j]][[tc$traces[[j]]$xtransform[q]]] <-
                                            log(tc$traces[[j]][[tc$traces[[j]]$xtransform[q]]], base=tc[[atype[k]]]$log[i])
                        }
                        if (dtype[k]=="y") {
                            if (!is.null(tc$traces[[j]]$ytransform))
                                for (q in 1:length(tc$traces[[j]]$ytransform))
                                    if (!is.null(tc$traces[[j]][[tc$traces[[j]]$ytransform[q]]]))
                                        tc$traces[[j]][[tc$traces[[j]]$ytransform[q]]] <-
                                            log(tc$traces[[j]][[tc$traces[[j]]$ytransform[q]]], base=tc[[atype[k]]]$log)
                        }  
                    }                   
                }
            }
            
            # Get trace based limits
            if (dtype[k]=="x")
                alldata <- xalldata
            else
                alldata <- yalldata 
            if (!is.null(alldata)) {
                alldata <- na.omit(alldata)
                if (length(alldata)) {
                    datamax <- max(alldata)
                    datamin <- min(alldata)
                }
            }
            
            # Modify for presence of at
            if (length(na.omit(tc[[atype[k]]]$at[i,]))) {
                atdata <- na.omit(tc[[atype[k]]]$at[i,])
                datamax <- max(datamax,atdata)
                datamin <- min(datamin,atdata)
            }

            # Recommend limits if needed            
            if (is.na(tc$limits[i,limitmin[k]]) && is.na(tc$limits[i,limitmax[k]])) {
                datarange <- datamax-datamin
                datamin <- datamin - (datarange*tc$leeway/2)
                datamax <- datamax + (datarange*tc$leeway/2)
                tc$limits[i,limitmin[k]] <- datamin
                tc$limits[i,limitmax[k]] <- datamax
            } else {
                if (is.na(tc$limits[i,limitmin[k]])) {
                    datarange <- tc$limits[tc$iplot,limitmax[k]] - datamin
                    if (datarange<=0) {
                        datarange <- datamax-datamin
                        datamin <- datamin - (datarange*tc$leeway/2)
                        datamax <- datamax + (datarange*tc$leeway/2)
                        tc$limits[i,limitmin[k]] <- datamin
                        tc$limits[i,limitmax[k]] <- datamax
                    } else {
                        datamin <- datamin - (datarange*tc$leeway/2)
                        tc$limits[i,limitmin[k]] <- datamin                    
                    }
                }
                if (is.na(tc$limits[i,limitmax[k]])) {
                    datarange <- datamax-tc$limits[i,limitmin[k]]
                    if (datarange<=0) {
                        datarange <- datamax-datamin
                        datamin <- datamin - (datarange*tc$leeway/2)
                        datamax <- datamax + (datarange*tc$leeway/2)
                        tc$limits[i,limitmin[k]] <- datamin
                        tc$limits[i,limitmax[k]] <- datamax
                    } else {
                        datamax <- datamax + (datarange*tc$leeway/2)
                        tc$limits[i,limitmax[k]] <- datamax
                    }
                }
            }
            
            # Recommend at if needed
            if (length(na.omit(tc[[atype[k]]]$at[i,]))==0) {
                if (length(na.omit(tc$limits[i,limitmin[k]:limitmax[k]]))) {
                    tc[[atype[k]]]$at <- insertrow(grid.pretty(tc$limits[i,limitmin[k]:limitmax[k]]),
                                                   tc[[atype[k]]]$at,
                                                   i)               
                }
            }

            # Create labels if needed
            if (length(na.omit(tc[[atype[k]]]$labels[i,]))==0) {
                # Make labels
                tc[[atype[k]]]$labels <- insertrow(tc[[atype[k]]]$at[i,],
                                                   tc[[atype[k]]]$labels,
                                                   i)
            }
            
            # Modify at and limits for plotting if axis is transformed
            if (is.logical(tc[[atype[k]]]$log[i])&&tc[[atype[k]]]$log[i]) {
                tc$limits[i,limitmin[k]] <- log(tc$limits[i,limitmin[k]])
                tc$limits[i,limitmax[k]] <- log(tc$limits[i,limitmax[k]])
                tc[[atype[k]]]$at[i,] <- log(tc[[atype[k]]]$at[i,])
            }
            if (is.numeric(tc[[atype[k]]]$log[i])&&tc[[atype[k]]]$log[i]>0) {
                tc$limits[i,limitmin[k]] <- log(tc$limits[i,limitmin[k]],base=tc[[atype[k]]]$log[i])
                tc$limits[i,limitmax[k]] <- log(tc$limits[i,limitmax[k]],base=tc[[atype[k]]]$log[i])
                tc[[atype[k]]]$at[i,] <- log(tc[[atype[k]]]$at[i,],base=tc[[atype[k]]]$log[i])
            }    

            # Recommend add if needed
            tc[[atype[k]]]$recommendadd[i] <- (tracesadded>0)   # Any trace on this axis?
        }


        # SAVE ORIGINAL REC'S, APPLY HERE
        # If add is NA, swap in recommendations            FIX add to have 1 for each plot/axis
        for (k in 1:4) {
            if (is.na(tc[[atype[k]]]$add[i])) {
                tc[[atype[k]]]$add[i] <- tc[[atype[k]]]$recommendadd[i]
            }
        }   
    }

    # from fillout defaults
    tc <- tileAxistitleprep("xaxis",tc)
    tc <- tileAxistitleprep("yaxis",tc)
    tc <- tileAxistitleprep("topaxis",tc)
    tc <- tileAxistitleprep("rightaxis",tc)
    
    # Modify title heights to account for line breaks

    
    # Get number of line breaks
    listtitle <- c("plottitle","undertitle","maintitle","columntitle","rowtitle",
                   "xaxistitle","yaxistitle","topaxistitle","rightaxistitle")
    for (i in 1:length(listtitle)) {
        currtitle <- listtitle[i]
        for (j in 1:length(tc[[currtitle]]$labels)) {
            if (!is.null(tc[[currtitle]]$labels)) {
                if ((tc[[currtitle]]$rot[j]==0)||(tc[[currtitle]]$rot[j]==180))
                    tc$height[[currtitle]][j] <- adjlinebreaks(tc$height[[currtitle]][j],
                                                               tc[[currtitle]]$labels[j]
                                                               )
                else
                    tc$width[[currtitle]][j] <- adjlinebreaks(tc$width[[currtitle]][j],
                                                              tc[[currtitle]]$labels[j]
                                                              )
            }            
        }
    }


    
    
    
    # Harmonize Axes:  Graphic specific modifications
    for (i in 1:tc$nplots) {

        # Check for graphic types in this plot
        # Loop over traces
        allgraph <- NULL
        for (j in 1:length(tc$traces)) {
            cond <- ((tc$traces[[j]]$plot==i))
            # Check if add trace
            if (cond) {
                # Collect graphic types
                allgraph <- c(allgraph,tc$traces[[j]]$graphic)
            }
        }
                    
        # Use special harmonize functions as needed
        ugraph <- unique(allgraph)
        for (igraph in 1:length(ugraph)) {
            thg <- paste(ugraph[igraph], "TileHarmonize", sep="")
            if (exists(thg, mode="function"))
                tc <- eval(call(thg, tc, i))
        }
    }
        
    # Recalculate as needed?

    # Compute maxheight and maxwidth for each plot
    for (i in 1:tc$RxC[1]) {

        # Get this row plotnums
        plotnums <- getplotnums("row",i,tc)
        #print(plotnums)
        #layoutnums[i,]
        
        # Get this row plot heights; set max height to max of these
        tc$height$plotmax[plotnums] <- suppressWarnings(max(tc$height$plot[plotnums[tc$useplot[plotnums]]]))
    }

    for (i in 1:tc$RxC[2]) {

        # Get this column plotnums
        plotnums <- getplotnums("column",i,tc)
    
        # Get this column plot widths; set max width to max of these
        tc$width$plotmax[plotnums] <- suppressWarnings(max(tc$width$plot[plotnums[tc$useplot[plotnums]]]))
    }

    replaceInfWithDefault <- function(a, default) {
      for (i in 1:length(a)) {
        if ((a[i]==Inf)||(a[i]==-Inf)) a[i] <- default[i]
      }
      a
    }

    # Not needed
    replaceInfWithMean <- function(a, default=1) {
      A <- a[(a<Inf)&(a>-Inf)]
      if (length(A)==0) A <- default
      a[(a==Inf)|(a==-Inf)] <- mean(A)
      a
    }

    # Default below should become user settable throught width in tile?
    tc$height$plotmax <- replaceInfWithDefault(tc$height$plotmax, default=tc$height$plot)
    tc$width$plotmax <- replaceInfWithDefault(tc$width$plotmax, default=tc$width$plot)

    # Return tile control
    tc
}
