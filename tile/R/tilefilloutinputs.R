tilefilloutinputs <- function(tc) {

    # Create scorecard and calculate nplots
    tc$ntraces <- length(tc$traces)

    scorecard <- maxplot <- NULL
    for (i in 1:tc$ntraces) {
        scorecard <- rbind(scorecard,c(tc$trace[[i]]$graphic,tc$trace[[i]]$plot))
        maxplot <- c(maxplot,tc$trace[[i]]$plot)
    }
    tc$scorecard <- xtabs(~scorecard[,1]+scorecard[,2])
    
    maxplot <- max(maxplot)

    
    # Determine RxC 
    if (is.null(tc$RxC)) {
        tc$nplots <- maxplot
        tc$RxC <- c(1,tc$nplots)        
    } else {
        tc$nplots <- max(maxplot,prod(tc$RxC))
    }
    if ((tc$RxC[1]*tc$RxC[2])<tc$nplots) {
        warning("RxC too small; setting RxC to {1,nplots}.")
        tc$RxC <- c(1,tc$nplots)
    }

    tc$useplot <- rep(FALSE,tc$nplots)
    for (i in 1:tc$nplots) {
      for (j in 1:length(tc$traces)) {
        cond <- ((tc$traces[[j]]$plot==i) )
        if (cond) {
          tc$useplot[i] <- TRUE
        }
      }
    }

    # Experimental initial output width
    if (tc$output$widthNotGiven) {
      tc$output$wide <- tc$RxC[2]*max(tc$width$null,na.rm=TRUE)*max(tc$width$plot,na.rm=TRUE)*4 + 5
    }
    
    # Fillout plot height and width requests
    if (length(tc$height$plot)==1) {
        tc$height$plot <- fillout(tc$height$plot,tc$nplots)
    } else {
        if (length(tc$height$plot)==tc$RxC[1]) {
            newheight <- NULL
            for (i in 1:tc$RxC[1]) {
                newheight <- c(newheight,rep(tc$height$plot[i]),tc$RxC[2])
            }
            tc$height$plot <- newheight
        } else {
            tc$height$plot <- fillout(tc$height$plot,tc$nplots)
        }
    }
    
    if (length(tc$width$plot)==1) {
        tc$width$plot <- fillout(tc$width$plot,tc$nplots)
    } else {
        if (length(tc$width$plot)==tc$RxC[2]) {
            for (i in 1:tc$RxC[1]) {
                newwidth <- NULL
                newwidth <- c(newwidth,tc$width$plot)
            }
            tc$width$plot <- newwidth
            tc$width$plot <- as.vector(tc$width$plot * matrix(1,nrow=tc$RxC[2],ncol=tc$RxC[1]))
        } else {
            tc$width$plot <- fillout(tc$width$plot,tc$nplots)
        }
    }

    for (i in 1:length(tc$height$plot)) {
        if (tc$height$plot[i]=="square") tc$height$plot[i] <- tc$width$plot[i]
        if (tc$height$plot[i]=="golden") tc$height$plot[i] <- as.numeric(tc$width$plot[i])/1.61803
        
    }

    tc$height$plot <- as.numeric(tc$height$plot)
    tc$width$plot <- as.numeric(tc$width$plot)

    tc$height$plotmax <- tc$height$plot
    tc$width$plotmax <-tc$width$plot
    
    #if (tc$height$plot=="square") tc$height$plot <- tc$width$plot
    #if (tc$height$plot=="golden") tc$height$plot <- tc$width$plot/1.61803
    
    #tc$height$plot <- fillout(tc$height$plot,tc$RxC[1])
    #tc$width$plot <- fillout(tc$width$plot,tc$RxC[2])

    # Fillout axis height and width requests
    # x-axis
    if (length(tc$height$xaxis)==1) {
        tc$height$xaxis <- fillout(tc$height$xaxis,tc$nplots)
    } else {
        if (length(tc$height$xaxis)==tc$RxC[1]) {
            newheight <- NULL
            for (i in 1:tc$RxC[1]) {
                newheight <- c(newheight,rep(tc$height$xaxis[i]),tc$RxC[2])
            }
            tc$height$xaxis <- newheight
        } else {
            tc$height$xaxis <- fillout(tc$height$xaxis,tc$nplots)
        }
    }
    if (length(tc$width$xaxis)==1) {
        tc$width$xaxis <- fillout(tc$width$xaxis,tc$nplots)
    } else {
        if (length(tc$width$xaxis)==tc$RxC[1]) {
            newwidth <- NULL
            for (i in 1:tc$RxC[1]) {
                newwidth <- c(newwidth,rep(tc$width$xaxis[i]),tc$RxC[2])
            }
            tc$width$xaxis <- newwidth
        } else {
            tc$width$xaxis <- fillout(tc$width$xaxis,tc$nplots)
        }
    }

    #topaxis
    if (length(tc$height$topaxis)==1) {
        tc$height$topaxis <- fillout(tc$height$topaxis,tc$nplots)
    } else {
        if (length(tc$height$topaxis)==tc$RxC[1]) {
            newheight <- NULL
            for (i in 1:tc$RxC[1]) {
                newheight <- c(newheight,rep(tc$height$topaxis[i]),tc$RxC[2])
            }
            tc$height$topaxis <- newheight
        } else {
            tc$height$topaxis <- fillout(tc$height$topaxis,tc$nplots)
        }
    }
    if (length(tc$width$topaxis)==1) {
        tc$width$topaxis <- fillout(tc$width$topaxis,tc$nplots)
    } else {
        if (length(tc$width$topaxis)==tc$RxC[1]) {
            newwidth <- NULL
            for (i in 1:tc$RxC[1]) {
                newwidth <- c(newwidth,rep(tc$width$topaxis[i]),tc$RxC[2])
            }
            tc$width$topaxis <- newwidth
        } else {
            tc$width$topaxis <- fillout(tc$width$topaxis,tc$nplots)
        }
    }

    # y-axis
    if (length(tc$width$yaxis)==1) {
        tc$width$yaxis <- fillout(tc$width$yaxis,tc$nplots)
    } else {
        if (length(tc$width$yaxis)==tc$RxC[2]) {
            for (i in 1:tc$RxC[1]) {
                newwidth <- NULL
                newwidth <- c(newwidth,tc$width$yaxis)
            }
            tc$width$yaxis <- newwidth
            tc$width$yaxis <- as.vector(tc$width$yaxis * matrix(1,nrow=tc$RxC[2],ncol=tc$RxC[1]))
        } else {
            tc$width$yaxis <- fillout(tc$width$yaxis,tc$nplots)
        }
    }
    
    if (length(tc$height$yaxis)==1) {
        tc$height$yaxis <- fillout(tc$height$yaxis,tc$nplots)
    } else {
        if (length(tc$height$yaxis)==tc$RxC[2]) {
            for (i in 1:tc$RxC[1]) {
                newheight <- NULL
                newheight <- c(newheight,tc$height$yaxis)
            }
            tc$height$yaxis <- newheight
            tc$height$yaxis <- as.vector(tc$height$yaxis * matrix(1,nrow=tc$RxC[2],ncol=tc$RxC[1]))
        } else {
            tc$height$yaxis <- fillout(tc$height$yaxis,tc$nplots)
        }
    }

    # right-axis
    if (length(tc$width$rightaxis)==1) {
        tc$width$rightaxis <- fillout(tc$width$rightaxis,tc$nplots)
    } else {
        if (length(tc$width$rightaxis)==tc$RxC[2]) {
            for (i in 1:tc$RxC[1]) {
                newwidth <- NULL
                newwidth <- c(newwidth,tc$width$rightaxis)
            }
            tc$width$rightaxis <- newwidth
            tc$width$rightaxis <- as.vector(tc$width$rightaxis * matrix(1,nrow=tc$RxC[2],ncol=tc$RxC[1]))
        } else {
            tc$width$rightaxis <- fillout(tc$width$rightaxis,tc$nplots)
        }
    }
    
    if (length(tc$height$rightaxis)==1) {
        tc$height$rightaxis <- fillout(tc$height$rightaxis,tc$nplots)
    } else {
        if (length(tc$height$rightaxis)==tc$RxC[2]) {
            for (i in 1:tc$RxC[1]) {
                newheight <- NULL
                newheight <- c(newheight,tc$height$rightaxis)
            }
            tc$height$rightaxis <- newheight
            tc$height$rightaxis <- as.vector(tc$height$rightaxis * matrix(1,nrow=tc$RxC[2],ncol=tc$RxC[1]))
        } else {
            tc$height$rightaxis <- fillout(tc$height$rightaxis,tc$nplots)
        }
    }
    


    # Fillout axis title height and width requests
    # x-axis title
    if (length(tc$height$xaxistitle)==1) {
        tc$height$xaxistitle <- fillout(tc$height$xaxistitle,tc$nplots)
    } else {
        if (length(tc$height$xaxistitle)==tc$RxC[1]) {
            newheight <- NULL
            for (i in 1:tc$RxC[1]) {
                newheight <- c(newheight,rep(tc$height$xaxistitle[i]),tc$RxC[2])
            }
            tc$height$xaxistitle <- newheight
        } else {
            tc$height$xaxistitle <- fillout(tc$height$xaxistitle,tc$nplots)
        }
    }
    if (length(tc$width$xaxistitle)==1) {
        tc$width$xaxistitle <- fillout(tc$width$xaxistitle,tc$nplots)
    } else {
        if (length(tc$width$xaxistitle)==tc$RxC[1]) {
            newwidth <- NULL
            for (i in 1:tc$RxC[1]) {
                newwidth <- c(newwidth,rep(tc$width$xaxistitle[i]),tc$RxC[2])
            }
            tc$width$xaxistitle <- newwidth
        } else {
            tc$width$xaxistitle <- fillout(tc$width$xaxistitle,tc$nplots)
        }
    }

    #topaxis title
    if (length(tc$height$topaxistitle)==1) {
        tc$height$topaxistitle <- fillout(tc$height$topaxistitle,tc$nplots)
    } else {
        if (length(tc$height$topaxistitle)==tc$RxC[1]) {
            newheight <- NULL
            for (i in 1:tc$RxC[1]) {
                newheight <- c(newheight,rep(tc$height$topaxistitle[i]),tc$RxC[2])
            }
            tc$height$topaxistitle <- newheight
        } else {
            tc$height$topaxistitle <- fillout(tc$height$topaxistitle,tc$nplots)
        }
    }
    if (length(tc$width$topaxistitle)==1) {
        tc$width$topaxistitle <- fillout(tc$width$topaxistitle,tc$nplots)
    } else {
        if (length(tc$width$topaxistitle)==tc$RxC[1]) {
            newwidth <- NULL
            for (i in 1:tc$RxC[1]) {
                newwidth <- c(newwidth,rep(tc$width$topaxistitle[i]),tc$RxC[2])
            }
            tc$width$topaxistitle <- newwidth
        } else {
            tc$width$topaxistitle <- fillout(tc$width$topaxistitle,tc$nplots)
        }
    }

    # y-axis title
    if (length(tc$width$yaxistitle)==1) {
        tc$width$yaxistitle <- fillout(tc$width$yaxistitle,tc$nplots)
    } else {
        if (length(tc$width$yaxistitle)==tc$RxC[2]) {
            for (i in 1:tc$RxC[1]) {
                newwidth <- NULL
                newwidth <- c(newwidth,tc$width$yaxistitle)
            }
            tc$width$yaxistitle <- newwidth
            tc$width$yaxistitle <- as.vector(tc$width$yaxistitle * matrix(1,nrow=tc$RxC[2],ncol=tc$RxC[1]))
        } else {
            tc$width$yaxistitle <- fillout(tc$width$yaxistitle,tc$nplots)
        }
    }
    
    if (length(tc$height$yaxistitle)==1) {
        tc$height$yaxistitle <- fillout(tc$height$yaxistitle,tc$nplots)
    } else {
        if (length(tc$height$yaxistitle)==tc$RxC[2]) {
            for (i in 1:tc$RxC[1]) {
                newheight <- NULL
                newheight <- c(newheight,tc$height$yaxistitle)
            }
            tc$height$yaxistitle <- newheight
            tc$height$yaxistitle <- as.vector(tc$height$yaxistitle * matrix(1,nrow=tc$RxC[2],ncol=tc$RxC[1]))
        } else {
            tc$height$yaxistitle <- fillout(tc$height$yaxistitle,tc$nplots)
        }
    }

    # right-axis title
    if (length(tc$width$rightaxistitle)==1) {
        tc$width$rightaxistitle <- fillout(tc$width$rightaxistitle,tc$nplots)
    } else {
        if (length(tc$width$rightaxistitle)==tc$RxC[2]) {
            for (i in 1:tc$RxC[1]) {
                newwidth <- NULL
                newwidth <- c(newwidth,tc$width$rightaxistitle)
            }
            tc$width$rightaxistitle <- newwidth
            tc$width$rightaxistitle <- as.vector(tc$width$rightaxistitle * matrix(1,nrow=tc$RxC[2],ncol=tc$RxC[1]))
        } else {
            tc$width$rightaxistitle <- fillout(tc$width$rightaxistitle,tc$nplots)
        }
    }
    
    if (length(tc$height$rightaxistitle)==1) {
        tc$height$rightaxistitle <- fillout(tc$height$rightaxistitle,tc$nplots)
    } else {
        if (length(tc$height$rightaxistitle)==tc$RxC[2]) {
            for (i in 1:tc$RxC[1]) {
                newheight <- NULL
                newheight <- c(newheight,tc$height$rightaxistitle)
            }
            tc$height$rightaxistitle <- newheight
            tc$height$rightaxistitle <- as.vector(tc$height$rightaxistitle * matrix(1,nrow=tc$RxC[2],ncol=tc$RxC[1]))
        } else {
            tc$height$rightaxistitle <- fillout(tc$height$rightaxistitle,tc$nplots)
        }
    }
    



    ############################


 #plot title
    if (length(tc$height$plottitle)<tc$nplots) {
        tc$height$plottitle <- fillout(tc$height$plottitle,tc$nplots)
    }
    if (length(tc$width$topaxistitle)<tc$nplots) {
        tc$width$plottitle <- fillout(tc$width$plottitle,tc$nplots)
    }


     #under title
    if (length(tc$height$undertitle)<tc$nplots) {
        tc$height$undertitle <- fillout(tc$height$undertitle,tc$nplots)
    }
    if (length(tc$width$undertitle)<tc$nplots) {
        tc$width$undertitle <- fillout(tc$width$undertitle,tc$nplots)
    }


    # row title
    #if (length(tc$height$rowtitle)<tc$RxC[1]) {
    #    tc$height$rowtitle <- fillout(tc$height$rowtitle,tc$nplots)
    #}
    #if (length(tc$width$rowtitle)<tc$RxC[1]) {
    #    tc$width$rowtitle <- fillout(tc$width$rowtitle,tc$nplots)
    #}


    # column title
    #if (length(tc$height$columntitle)<tc$RxC[2]) {
    #    tc$height$columntitle <- fillout(tc$height$columntitle,tc$nplots)
    #}
    #if (length(tc$width$columntitle)<tc$RxC[2]) {
    #tc$width$columntitle <- fillout(tc$width$columntitle,tc$nplots)
    #}
    
############################################################3



    
    #tc$width$spacer <- tc$width$spacer*median(c(tc$width$plot,tc$height$plot))
    #tc$height$spacer <- tc$height$spacer*median(c(tc$width$plot,tc$height$plot))  # width or height?  aesthetic question
    tc$width$spacer <- fillout(tc$width$spacer,tc$nplots)
    tc$height$spacer <- fillout(tc$height$spacer,tc$nplots)
    

    tc$width$xaxis.rug <- fillout(tc$width$xaxis.rug,tc$nplots)
    tc$height$xaxis.rug <- fillout(tc$height$xaxis.rug,tc$nplots)
    tc$width$yaxis.rug <- fillout(tc$width$yaxis.rug,tc$nplots)
    tc$height$yaxis.rug <- fillout(tc$height$yaxis.rug,tc$nplots)
    tc$width$topaxis.rug <- fillout(tc$width$topaxis.rug,tc$nplots)
    tc$height$topaxis.rug <- fillout(tc$height$topaxis.rug,tc$nplots)
    tc$width$rightaxis.rug <- fillout(tc$width$rightaxis.rug,tc$nplots)
    tc$height$rightaxis.rug <- fillout(tc$height$rightaxis.rug,tc$nplots)

    tc$height$xaxis.labelspace <- fillout(tc$height$xaxis.labelspace,tc$nplots)
    tc$width$yaxis.labelspace <- fillout(tc$width$yaxis.labelspace,tc$nplots)
    tc$height$topaxis.labelspace <- fillout(tc$height$topaxis.labelspace,tc$nplots)
    tc$width$rightaxis.labelspace <- fillout(tc$width$rightaxis.labelspace,tc$nplots)

    # Specific heights and widths
     # Experimental:  allow substitution by plot number
    components <- c("plot","spacer", "bottomborder", "xaxis",
                    "topaxis","xaxistitle","topaxistitle","xaxis.labelspace",
                    "xaxis.rug","topaxis.labelspace","topaxis.rug",
                    "maintitle","undertitle","plottitle",
                    "yaxistitle","rightaxistitle") 
    for (j in 1:length(components)) {
      for (i in 1:tc$nplots) {
        slot <- paste(components[j],i,sep="")
        if (!is.null(tc$height[[slot]]))
          tc$height[[components[j]]][i] <- tc$height[[slot]][1]
      }
    }

    components <- c("rowtitle")
    for (j in 1:length(components)) {
      for (i in 1:tc$RxC[1]) {
        slot <- paste(components[j],i,sep="")
        if (!is.null(tc$height[[slot]]))
          tc$height[[components[j]]][i] <- tc$height[[slot]][1]
      }
    }

    components <- c("columntitle")
    for (j in 1:length(components)) {
      for (i in 1:tc$RxC[2]) {
        slot <- paste(components[j],i,sep="")
        if (!is.null(tc$height[[slot]]))
          tc$height[[components[j]]][i] <- tc$height[[slot]][1]
      }
    }

    components <- c("plot","spacer","rightborder",
                    "yaxis","rightaxis","yaxistitle","rightaxistitle",
                    "xaxistitle","topaxistitle","yaxis.labelspace",
                    "yaxis.rug","rightaxis.labelspace","rightaxis.rug",
                    "maintitle","undertitle","plottitle") 
    for (j in 1:length(components)) {
      for (i in 1:tc$nplots) {
        slot <- paste(components[j],i,sep="")
        if (!is.null(tc$width[[slot]]))
          tc$width[[components[j]]][i] <- tc$width[[slot]][1]
      }
    }

    components <- c("rowtitle")
    for (j in 1:length(components)) {
      for (i in 1:tc$RxC[1]) {
        slot <- paste(components[j],i,sep="")
        if (!is.null(tc$width[[slot]]))
          tc$width[[components[j]]][i] <- tc$width[[slot]][1]
      }
    }

    components <- c("columntitle")
    for (j in 1:length(components)) {
      for (i in 1:tc$RxC[2]) {
        slot <- paste(components[j],i,sep="")
        if (!is.null(tc$width[[slot]]))
          tc$width[[components[j]]][i] <- tc$width[[slot]][1]
      }
    }

    
    

    l0 <- matrix(NA,nrow=tc$nplots,ncol=8)
    if (!is.null(tc$limits)) {
        tc$limits <- fillout.matrix(tc$limits,tc$nplots)
        l0[,1:ncol(tc$limits)] <- tc$limits
    }
    tc$limits <- l0
    
    tc$frame <- fillout(tc$frame,tc$nplots)

       
    # Check whether titles have been requested
    tc$plottitle$add <- !is.null(tc$plottitle$labels)
    tc$maintitle$add <- !is.null(tc$maintitle$labels)
    tc$undertitle$add <- !is.null(tc$undertitle$labels)
    tc$rowtitle$add <- !is.null(tc$rowtitle$labels)
    tc$columntitle$add <- !is.null(tc$columntitle$labels)
    
    tc$xaxistitle$add <- !is.null(tc$xaxistitle$labels)
    tc$yaxistitle$add <- !is.null(tc$yaxistitle$labels)
    tc$rightaxistitle$add <- !is.null(tc$rightaxistitle$labels)
    tc$topaxistitle$add <- !is.null(tc$topaxistitle$labels)

    # Remove unneeded borders
    if (tc$maintitle$add&&tc$height$topborderNotGiven)
      tc$height$topborder <- 0

    if (tc$rowtitle$add&&tc$width$leftborderNotGiven)
      tc$width$leftborder <- 0
    
    # Fillout axis
    tc <- fillout.axis("xaxis",tc$nplots,tc)
    tc <- fillout.axis("yaxis",tc$nplots,tc)
    tc <- fillout.axis("topaxis",tc$nplots,tc)
    tc <- fillout.axis("rightaxis",tc$nplots,tc)  
    #if (tc$plottitle$add)
    tc <- fillout.title("plottitle",tc$nplots,tc)
    #if (tc$maintitle$add)
    #    tc <- fillout.title("maintitle",tc$nplots,tc)
    #if (tc$undertitle$add)
    tc <- fillout.title("undertitle",tc$nplots,tc)
    #if (tc$rowtitle$add)
    tc <- fillout.title("rowtitle",tc$RxC[1],tc)
    #if (tc$columntitle$add)
    
    tc <- fillout.title("columntitle",tc$RxC[2],tc)
 
    tc$columntitle$add <- tc$columntitle$add[1]
    tc$rowtitle$add <- tc$rowtitle$add[1]

    # 
    #tc <- fillout.axistitle("xaxis",tc$nplots,tc)

    
#print(tc$yaxistitle$add)
#    stop()
    
    #if (tc$xaxistitle$add) {
        tc <- fillout.axistitle("xaxis",tc$nplots,tc)
        #tc <- tileAxistitleprep("xaxis",tc)
    #}
    #if (tc$yaxistitle$add) {
        tc <- fillout.axistitle("yaxis",tc$nplots,tc)
        #tc <- tileAxistitleprep("yaxis",tc)
    #}
    #if (tc$topaxistitle$add) {
        tc <- fillout.axistitle("topaxis",tc$nplots,tc)
        #tc <- tileAxistitleprep("topaxis",tc)
    #}
    #if (tc$rightaxistitle$add) {
        tc <- fillout.axistitle("rightaxis",tc$nplots,tc)
        #tc <- tileAxistitleprep("rightaxis",tc)
    #}


    # Modify title heights to account for line breaks

    
    # Get number of line breaks
    #listtitle <- c("plottitle","undertitle","maintitle","columntitle","rowtitle",
    #               "xaxistitle","yaxistitle","topaxistitle","rightaxistitle")
    #for (i in 1:length(listtitle)) {
    #    currtitle <- listtitle[i]
    #    for (j in 1:length(tc[[currtitle]]$labels)) {
    #        if (!is.null(tc[[currtitle]]$labels)) {
    #            if ((tc[[currtitle]]$rot[j]==0)||(tc[[currtitle]]$rot[j]==180))
    #                tc$height[[currtitle]][j] <- adjlinebreaks(tc$height[[currtitle]][j],
    #                                                           tc[[currtitle]]$labels[j]
    #                                                           )
    #            else
    #                tc$width[[currtitle]][j] <- adjlinebreaks(tc$width[[currtitle]][j],
    #                                                          tc[[currtitle]]$labels[j]
    #                                                          )
    #        }            
    #    }
    #}

    # Fillout gridlines
    if (!is.null(tc$gridlines$type)) tc$gridlines$type <- fillout(tc$gridlines$type,tc$nplots)
    tc$gridlines$lwd <- fillout(tc$gridlines$lwd,tc$nplots)
    tc$gridlines$lty <- fillout(tc$gridlines$lty,tc$nplots)
    tc$gridlines$col <- fillout(tc$gridlines$col,tc$nplots)
    tc$gridlines$edges <- fillout(tc$gridlines$edges,tc$nplots)

       
    # Experimental:  allow substitution by plot number
    components <- c("type", "lwd", "lty", "col", "edges")  
    for (j in 1:length(components)) {
      for (i in 1:tc$nplots) {
        slot <- paste(components[j],i,sep="")
        if (!is.null(tc$gridlines[[slot]]))
          tc$gridlines[[components[j]]][i] <- tc$gridlines[[slot]][1]
      }
    }


    # Fillout special
    tc$special$xaxistitle <- fillout(tc$special$xaxistitle,tc$nplots)
    tc$special$yaxistitle <- fillout(tc$special$yaxistitle,tc$nplots)
    tc$special$topaxistitle <- fillout(tc$special$topaxistitle,tc$nplots)
    tc$special$rightaxistitle <- fillout(tc$special$rightaxistitle,tc$nplots)
    tc$special$plottitle <- fillout(tc$special$plottitle,tc$nplots)
    tc$special$undertitle <- fillout(tc$special$undertitle,tc$nplots)

    tc$special$xaxistitleControl <- fillout.list(tc$special$xaxistitleControl,tc$nplots)
    tc$special$yaxistitleControl <- fillout.list(tc$special$yaxistitleControl,tc$nplots)
    tc$special$topaxistitleControl <- fillout.list(tc$special$topaxistitleControl,tc$nplots)
    tc$special$rightaxistitleControl <- fillout.list(tc$special$rightaxistitleControl,tc$nplots)
    tc$special$plottitleControl <- fillout.list(tc$special$plottitleControl,tc$nplots)
    tc$special$undertitleControl <- fillout.list(tc$special$undertitleControl,tc$nplots)

    tc$special$fontsizeRopeladder <- fillout(tc$special$fontsizeRopeladder,tc$nplots)
   
    tc
}
