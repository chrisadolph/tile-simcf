"tile" <-
function(...,

         ## Plot options
         limits = NULL,
         frame = FALSE,
         gridlines = list(),
                  
         ## Axes
         xaxis = list(),
         yaxis = list(),
         topaxis = list(),
         rightaxis = list(),

         ## Axis titles
         xaxistitle = list(),
         yaxistitle = list(),
         topaxistitle = list(),
         rightaxistitle = list(),

         ## Titles
         maintitle = list(),
         rowtitle = list(), 
         columntitle = list(),
         plottitle = list(),
         undertitle = list(),
                 
         ## Layout options
         RxC=NULL,
         output = list(),
         width = list(),
         height = list(),

         ## Special tile controls
         leeway = 0,
         draw = FALSE,
         defaults = list(),
         layoutonly = FALSE,
         titleboxes = FALSE,
         axisboxes = FALSE,
         verbose = FALSE
         ) {

    if (verbose) print("Starting tile...")
    
    # Load needed packages
    require(grid)

    # Process traces
    if (verbose) print("Processing traces")
    traces <- tileProcessTraces(list(...))
    
    # Set defaults
    if (verbose) print("Setting tile defaults")
    tc <- tilesetdefaults(RxC = RxC,
                          output = output,
                          width = width,
                          height = height,
                          plottitle = plottitle,
                          maintitle = maintitle,
                          undertitle = undertitle,
                          rowtitle = rowtitle,
                          columntitle = columntitle,
                          xaxis = xaxis,
                          yaxis = yaxis,
                          topaxis = topaxis,
                          rightaxis = rightaxis,
                          xaxistitle = xaxistitle,
                          yaxistitle = yaxistitle,
                          topaxistitle = topaxistitle,
                          rightaxistitle = rightaxistitle,
                          gridlines = gridlines,
                          limits = limits,
                          frame = frame,
                          draw = draw,
                          leeway = leeway,                           
                          defaults = defaults,
                          layoutonly = layoutonly,
                          titleboxes = titleboxes,
                          axisboxes = axisboxes)

    
    # Add trace controls to main tile control
    tc$traces <- traces
  
    # Fillout inputs for each plot
    if (verbose) print("Filling out tile settings")
    tc <- tilefilloutinputs(tc)

    # Harmonize axes
    if (verbose) print("Harmonizing axes")
    tc <- tileHarmonizeAxes(tc)    
    
    # Initialize widths and heights of plot elements
    if (verbose) print("Initializing plotting dimensions")
    tc <- tileInitWH(tc)
    
    # Iterate until plot dimensions are stable
    done <- 0
    secondpass <- 0
    iternum <- 0
    while (!done) {
        
        iternum <- iternum+1
        if (verbose) print(paste("Plot layout iteration",iternum))
    
        if (secondpass) {

             #Get current plot dimensions
            tc <-  tiledimensionsIter(tc)
            
            # Close existing plot
            dev.off()
            
            # Set new dimensions
            output.high.last2 <- output.high.last
            output.high.last <- tc$output$high
        
            strheights0 <- (tc$maintitle$add * max(tc$height$maintitle.npcmainSaved,na.rm=TRUE)        
                            + tc$columntitle$add * max(tc$height$columntitle.npcmainSaved,na.rm=TRUE))   

            strheights1 <- 0
            for (irows in 1:tc$RxC[1]) {
                #currplot <- getplotnums("row",irows,tc)

                 strheights1 <- (strheights1
                                + tc$height$plottitle.npcmainSaved[irows]
                                + tc$height$xaxistitle.npcmainSaved[irows]
                                + tc$height$topaxistitle.npcmainSaved[irows]
                                + tc$height$xaxis.npcmainSaved[irows]
                                + tc$height$topaxis.npcmainSaved[irows]
                                + tc$height$undertitle.npcmainSaved[irows]
                                )
            }
            
            tc$nullshigh <- sum(tc$height$plotmax[getplotnums("col",1,tc)]) #+ sum(tc$height$spacer)  # what about omitted last spacer?
            
            strwidths0 <- max(tc$width$rowtitle.npcmainSaved,na.rm=TRUE)*tc$rowtitle$add

            strwidths1 <- 0
            for (icols in 1:tc$RxC[2]) {
                #currplot <- getplotnums("column",icols,tc)

                strwidths1 <- (strwidths1
                               + tc$width$yaxis.npcmainSaved[icols]
                               + tc$width$rightaxis.npcmainSaved[icols]
                               + tc$width$yaxistitle.npcmainSaved[icols]
                               + tc$width$rightaxistitle.npcmainSaved[icols]
                               )
            }

            
            spacerwidth <- tc$width$leftborder.npcmain + (tc$RxC[2]-1)*tc$width$spacer.npcmain + tc$width$rightborder.npcmain
            spacerheight <- tc$height$topborder.npcmain + (tc$RxC[1]-1)*tc$height$spacer.npcmain + tc$height$bottomborder.npcmain
    
            tc$output$high <- ( tc$output$wide*(1 - strwidths0 - strwidths1 - spacerwidth)*(tc$nullshigh/tc$nullswide)/
                               (1 - strheights1 - strheights0 - spacerheight) )

            #print("Output width")
            #print(tc$output$wide)
            #print("Widths")
            #print(c(strwidths0, strwidths1, spacerwidth))
            #print("Heights")
            #print(c(strheights0, strheights1, spacerheight))
            #print("Output height")
            #print(tc$output$high)
            #print("")
            
            # Check for convergence
            cond <- (abs(tc$output$high - output.high.last2)<tc$defaults$ccrit) + (iternum>=tc$defaults$maxiter)
            if (cond) done <- 1
            
            # Open new plot
            tileopendevice(tc,done)
        } else {

            # Get current plot dimeninsions
            tc <- tiledimensions(tc)
            
            # Prepare and open first plotting attempt
            output.high.last <- 0
            tc$output$high <- tc$output$wide
            tileopendevice(tc,FALSE)
        }

        # Create main viewport
        tc$mainvp <- viewport(layout=tc$overlay,name="mainvp",
                              gp=gpar(fontsize=12),#tc$output$pointsize),
                              )

        
        # Loop over all main plots
        tc$grob <- gTree(name="tile",
                         children=gList(NULL)
                         )
        tc$currcol <- 1
        tc$currrow <- 1
        for (iplot in 1:tc$nplots) {
            
            # Prep main plot region (axes, space for rugs, gridlines, frame)
            if (verbose) print("Create main plotting area")
            tc$iplot <- iplot
            tc <- tilefindplotarea(tc)
            tc <- tileMakePlot(tc)
            
            # Add current gList to main gTree
            tc <- tilegrobNest(tc,name=paste("plotarea",tc$iplot,sep=""))
            if (verbose) print("Add titles to plots")
        
            #  Add Row titles
            if (tc$newrow&&tc$rowtitle$add) {
                if (verbose) print("  Adding row titles")                
                if (tc$special$rowtitle!="none") { 
                    tc <- eval(call(paste("tile.",tc$special$rowtitle,".rowtitle",sep=""),tc)) }
                else {
                    tc$rowtitle$layout.pos.col <- 2
                    tc$rowtitle$layout.pos.row <- tc$curplotrow
                    tc$rowtitle$fontsize <- tc$defaults$rowtitle$fontsize
                    tc <- tileTitle("rowtitle",tc,...)             
                }    
                tc <- tilegrobNest(tc,name=paste("rowtitle",tc$iplot,sep=""))
            }

            #  Add Column titles
            if (tc$newcol&&tc$columntitle$add) {
                if (verbose) print("  Adding column titles")
                if (tc$special$columntitle!="none") { 
                    tc <- eval(call(paste("tile.",tc$special$columntitle,".columntitle",sep=""),tc)) }
                else {
                    tc$columntitle$layout.pos.col <- tc$curplotcol
                    tc$columntitle$layout.pos.row <- (tc$curplotrow
                                                      - any(tc$topaxistitle$add[tc$currrowplots])
                                                      - any(tc$topaxis$add[tc$currrowplots])
                                                      - any(tc$plottitle$add[tc$currrowplots])
                                                      - 1)
                    tc$columntitle$fontsize <- tc$defaults$columntitle$fontsize
                    tc <- tileTitle("columntitle",tc,...)
                }
                tc <- tilegrobNest(tc,name=paste("columntitle",tc$iplot,sep=""))
            }

          
            #  Add main titles 
            if ((iplot==1)&&tc$maintitle$add) {
                if (verbose) print("  Adding main titles")
                if (tc$special$maintitle!="none")
                    tc <- eval(call(paste("tile.",tc$special$maintitle,".maintitle",sep=""),tc))
                else {
                    tc$maintitle$layout.pos.col <- tc$curplotcol
                    tc$maintitle$layout.pos.row <- (tc$curplotrow
                                                    - any(tc$topaxistitle$add[tc$currrowplots])
                                                    - any(tc$topaxis$add[tc$currrowplots])
                                                    - any(tc$plottitle$add[tc$currrowplots])
                                                    - any(tc$columntitle$add)
                                                    - 1)

                    tc$maintitle$fontsize <- tc$defaults$maintitle$fontsize
                    tc <- tileTitle("maintitle",tc,...)
                }
                tc <- tilegrobNest(tc,name="maintitle")
            }

            #  Add plot titles
            if (tc$plottitle$add[tc$iplot]) {
                if (verbose) print("  Adding plot titles")
                if (tc$special$plottitle[tc$iplot]!="none")
                    tc <- eval(call(paste("tile.",tc$special$plottitle[tc$iplot],".plottitle",sep=""),tc))
                else {
                    tc$plottitle$layout.pos.col <- tc$curplotcol
                    tc$plottitle$layout.pos.row <- (tc$curplotrow
                                                    - any(tc$topaxistitle$add[tc$currrowplots])
                                                    - any(tc$topaxis$add[tc$currrowplots])
                                                    - 1)                
                    tc$plottitle$fontsize <- tc$defaults$plottitle$fontsize
                    tc <- tileTitle("plottitle",tc,...)
                }
                tc <- tilegrobNest(tc,name=paste("plottitle",tc$iplot,sep=""))
            }

           
            
            #  Add under titles
            if (tc$undertitle$add[tc$iplot]) {
                if (verbose) print("  Adding under titles")
                if (tc$special$undertitle[tc$iplot]!="none")
                    tc <- eval(call(paste("tile.",tc$special$undertitle[tc$iplot],".undertitle",sep=""),tc))
                else {
                    tc$undertitle$layout.pos.col <- tc$curplotcol
                    tc$undertitle$layout.pos.row <- (tc$curplotrow
                                                     + any(tc$xaxistitle$add[tc$currrowplots])
                                                     + any(tc$xaxis$add[tc$currrowplots])
                                                     + 1)
                    tc$undertitle$fontsize <- tc$defaults$undertitle$fontsize
                    tc <- tileTitle("undertitle",tc,...)
                }
                tc <- tilegrobNest(tc,name=paste("undertitle",tc$iplot,sep=""))
            }
            
            #  Add xaxis titles           
            if (tc$xaxistitle$add[tc$iplot]) {
                if (verbose) print("  Adding xaxis titles")
                tc$xaxistitle$layout.pos.col <- tc$curplotcol
                tc$xaxistitle$layout.pos.row <- (tc$curplotrow
                                                 + any(tc$xaxis$add[tc$currrowplots])
                                                 + 1)
                if (tc$special$xaxistitle[tc$iplot]!="none")
                    tc <- eval(call(paste(tc$special$xaxistitle[tc$iplot],"TileAxisTitle",sep=""),"xaxis",tc))
                else {
                    tc$xaxistitle$fontsize <- tc$defaults$xaxistitle$fontsize
                    tc <- tileAxistitleprep("xaxis",tc,...)
                    tc <- tileAxistitle("xaxis",tc,...)                   
                }
                tc <- tilegrobNest(tc,name=paste("xaxistitle",tc$iplot,sep=""))        
            }
        
            #  Add topaxis titles  
            if (tc$topaxistitle$add[tc$iplot]) {
                if (verbose) print("  Adding topaxis titles")
                tc$topaxistitle$layout.pos.col <- tc$curplotcol
                tc$topaxistitle$layout.pos.row <- (tc$curplotrow
                                                   - any(tc$topaxis$add[tc$currrowplots])
                                                   - 1)                
                if (tc$special$topaxistitle[tc$iplot]!="none")
                    tc <- eval(call(paste(tc$special$topaxistitle[tc$iplot],"TileAxisTitle",sep=""),"topaxis",tc))
                else {
                    tc$topaxistitle$fontsize <- tc$defaults$topaxistitle$fontsize
                    tc <- tileAxistitleprep("topaxis",tc,...)
                    tc <- tileAxistitle("topaxis",tc,...)
                }
                tc <- tilegrobNest(tc,name=paste("topaxistitle",tc$iplot,sep=""))
            }        
        
            #  Add yaxis titles
            if (tc$yaxistitle$add[tc$iplot]) {              
                if (verbose) print("  Adding yaxis titles")
                tc$yaxistitle$layout.pos.col <- (tc$curplotcol
                                                 - any(tc$yaxis$add[tc$currcolplots])
                                                 - 1)
                tc$yaxistitle$layout.pos.row <- tc$curplotrow
                if (tc$special$yaxistitle[tc$iplot]!="none") {
                    tc <- eval(call(paste(tc$special$yaxistitle[tc$iplot],"TileAxisTitle",sep=""),"yaxis",tc))
                } else {
                    tc$yaxistitle$fontsize <- tc$defaults$yaxistitle$fontsize
                    tc <- tileAxistitleprep("yaxis",tc,...)
                    tc <- tileAxistitle("yaxis",tc,...)
                }
                tc <- tilegrobNest(tc,name=paste("yaxistitle",tc$iplot,sep=""))
            }         
            
            #  Add rightaxis titles  
            if (tc$rightaxistitle$add[tc$iplot]) {
                if (verbose) print("  Adding rightaxis titles")
                tc$rightaxistitle$layout.pos.col <- (tc$curplotcol
                                                     + any(tc$rightaxis$add[tc$currcolplots])
                                                     + 1)
                tc$rightaxistitle$layout.pos.row <- tc$curplotrow
                if (tc$special$rightaxistitle[tc$iplot]!="none")
                    tc <- eval(call(paste(tc$special$rightaxistitle[tc$iplot],"TileAxisTitle",sep=""),"rightaxis",tc))
                else {
                    tc$rightaxistitle$fontsize <- tc$defaults$rightaxistitle$fontsize
                    tc <- tileAxistitleprep("rightaxis",tc,...)
                    tc <- tileAxistitle("rightaxis",tc,...)
                }
                tc <- tilegrobNest(tc,name=paste("rightaxistitle",tc$iplot,sep=""))
            }
        }

        # Loop over traces for plotting
        if (!tc$layoutonly&&done) {
            for (itrace in 1:tc$ntraces) {

                tc$itrace <- itrace
                tc$iplot <- tc$traces[[tc$itrace]]$plot
                tc <- tilefindplotarea(tc)
                
                if (verbose) print(paste("   Adding trace",itrace,"to plot"))
            
                # pass to XTilePlot
                tc <- eval(call(paste(tc$traces[[tc$itrace]]$graphic,"TilePlot",sep=""),tc))

                # Add current gList to main gTree
                tc <- tilegrobNest(tc,name=paste("trace",tc$itrace,sep=""))                
            }
        }
        
        # Draw graphic
        if (verbose&&done) print("Drawing graphic to output device")
        if (done) tileDraw(tc$grob)
        secondpass <- 1

    }   
    
    # Save file 
    if (!is.null(tc$output$outfile))
        dev.off()

    if (tc$draw) {
        if (verbose) print("Drawing second copy of graphic to default screen output")
        tileDraw(tc$grob)
    }
    
    if (verbose) print("Tile run complete.")

    invisible(tc)  
}

