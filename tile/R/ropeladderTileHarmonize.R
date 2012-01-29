ropeladderTileHarmonize <- function(tc,iplot) {

    #make sure is a trace$nentries

  # Assign special axis controls
      
      # Need a list "special" which has plot specific overrides

      # If a plot has an associated special, harmonizeAxes will override if possible
      # specials applied in order of traces (last special over first)

      # interaction across plots?  may not be necessary

    # check for ropeladder compatibility
    yright <- xtop <- 0
    ntraces <- ngroups <- 0
    groups <- NULL
    entryheight <- 1/6
    subentryheight <- 0.6
    for (j in 1:length(tc$traces)) {
        cond <- ( (tc$traces[[j]]$plot==iplot) && (tc$traces[[j]]$graphic=="ropeladder") )
        if (cond) {
            ct <- tc$traces[[j]]
            yright <- sum(yright,(ct$plotrotation==0),(ct$plotrotation==180))
            xtop <- sum(xtop,(ct$plotrotation==90),(ct$plotrotation==270))

            if (is.null(ct$group))
              ct$group <- ngroups + 1
            if (length(unique(c(groups,ct$group)))>length(unique(groups)))
              ngroups <- ngroups + 1
            groups <- c(groups,ct$group)
            
            ntraces <- ntraces + 1
            tc$special$fontsizeRopeladder[iplot] <- max(c(tc$special$fontsizeRopeladder[iplot], ct$fontsize))


            # Set entryheight & subentryheight
            if (tc$traces[[j]]$entryheight!=(1/6))
              entryheight <- tc$traces[[j]]$entryheight
            if (tc$traces[[j]]$subentryheight!=0.6)
              subentryheight <- tc$traces[[j]]$subentryheight
            
        }
    }

    
    
    if (yright&&xtop) {
        stop("Multiple ropeladder traces can only be added to the same plot if they have parallel orientations.  Modify your traces so that ropeladders on the same plot are all assign to x and/or top, or all assigned to y and/or right.")
    }

    # Harmomize axes

    if (yright) {
        tc$yaxis$add[iplot]  <-tc$rightaxis$add[iplot] <- FALSE
    } else {
        tc$xaxis$add[iplot] <-tc$topaxis$add[iplot]  <- FALSE
    }
 
  
    if (ntraces) {
        maxtraces <- 0
        for (i in 1:ntraces) {
            maxtraces <- max(maxtraces,tc$traces[[i]]$nentries)
        }
        if (ngroups==1) {
            newincr <- 0
        } else {
            if (ngroups==2) {
                width <- subentryheight
            }
            if (ngroups==3) {
                width <- subentryheight
            }
            if (ngroups>3) {
                width <- subentryheight
            }
            oldincr <- 1/(maxtraces + 1);
            prtincr <- 1/(ngroups + 1)
            newincr <- rev(seq(prtincr,1-prtincr,by=prtincr)-0.5)*width*oldincr
           
        }
    }
    
    ctr <- 0
    maxtracelength <- 0
    for (j in 1:length(tc$traces)) {
        cond <- ( (tc$traces[[j]]$plot==iplot) && (tc$traces[[j]]$graphic=="ropeladder") )
        if (cond) {
            ctr <- ctr + 1
            ct <- tc$traces[[j]]
            labelaxis <- tc$traces[[j]]$labelaxis
            dataaxis <- tc$traces[[j]]$dataaxis
            tc$traces[[j]][[labelaxis]] <- tc$traces[[j]][[labelaxis]]  + newincr[groups[ctr]] 
            maxtracelength <- max(maxtracelength, length(tc$traces[[j]][[labelaxis]]))

            # Create locations for sublabels if necessary
            
            if ((ct$plotrotation==0)||(ct$plotrotation==180)) {
              if (is.null(tc$traces[[j]]$sublabelsX)) {
                tc$traces[[j]]$sublabelsxloc <- unit(tc$traces[[j]][[dataaxis]],"native") + unit(tc$traces[[j]]$sublabelsxoffset, "npc")
              } else {
                tc$traces[[j]]$sublabelsxloc <- unit(tc$traces[[j]]$sublabelsX, "npc") + unit(tc$traces[[j]]$sublabelsxoffset, "npc")
              }
              
              if (is.null(tc$traces[[j]]$sublabelsY)) {
                tc$traces[[j]]$sublabelsyloc <- unit(tc$traces[[j]][[labelaxis]], "native") + unit(tc$traces[[j]]$sublabelsyoffset, "npc")
              } else {
                tc$traces[[j]]$sublabelsyloc <- unit(tc$traces[[j]]$sublabelsY, "npc") + unit(tc$traces[[j]]$sublabelsyoffset, "npc")
              }
              
              #tc$traces[[j]]$sublabelsyoffset <- tc$traces[[j]][[labelaxis]] + tc$traces[[j]]$sublabelsyoffset
            }
            if ((ct$plotrotation==90)||(ct$plotrotation==270)) {
              if (is.null(tc$traces[[j]]$sublabelsX)) {
                tc$traces[[j]]$sublabelsxloc <- unit(tc$traces[[j]][[labelaxis]],"native") + unit(tc$traces[[j]]$sublabelsxoffset, "npc")
              } else {
                tc$traces[[j]]$sublabelsxloc <- unit(tc$traces[[j]]$sublabelsX, "npc") + unit(tc$traces[[j]]$sublabelsxoffset, "npc")
              }
              
              if (is.null(tc$traces[[j]]$sublabelsY)) {
                tc$traces[[j]]$sublabelsyloc <- unit(tc$traces[[j]][[dataaxis]], "native") + unit(tc$traces[[j]]$sublabelsyoffset, "npc")
              } else {
                tc$traces[[j]]$sublabelsyloc <- unit(tc$traces[[j]]$sublabelsY, "npc") + unit(tc$traces[[j]]$sublabelsyoffset, "npc")
              }

              #tc$traces[[j]]$sublabelsyoffset <- tc$traces[[j]][[dataaxis]] 
              #tc$traces[[j]]$sublabelsxoffset <- tc$traces[[j]][[labelaxis]] + tc$traces[[j]]$sublabelsxoffset
            }

            # Add axes as needed by ropeladder; set new title metrics
            if (ct$plotrotation==0) {
                tc$xaxis$add[iplot] <- TRUE
                tc$yaxis$add[iplot] <- FALSE
                tc$rightaxis$add[iplot] <- FALSE
                if (tc$traces[[j]]$mirrorlabels)
                    tc$width$rightaxistitle.useStrheight[iplot] <- FALSE
                else
                    tc$width$yaxistitle.useStrheight[iplot] <- FALSE
            }
            if (ct$plotrotation==90) {
                tc$yaxis$add[iplot] <- TRUE
                tc$xaxis$add[iplot] <- FALSE
                tc$topaxis$add[iplot] <- FALSE
                if (tc$traces[[j]]$mirrorlabels)
                    tc$height$topaxistitle.useStrheight[iplot] <- FALSE
                else
                    tc$height$xaxistitle.useStrheight[iplot] <- FALSE
            }
            if (ct$plotrotation==180) {
                tc$topaxis$add[iplot] <- TRUE
                tc$yaxis$add[iplot] <- FALSE
                tc$rightaxis$add[iplot] <- FALSE
                if (tc$traces[[j]]$mirrorlabels)
                    tc$width$rightaxistitle.useStrheight[iplot] <- FALSE
                else
                    tc$width$yaxistitle.useStrheight[iplot] <- FALSE                
            }
            if (ct$plotrotation==270) {
                tc$rightaxis$add[iplot] <- TRUE
                tc$xaxis$add[iplot] <- FALSE
                tc$topaxis$add[iplot] <- FALSE
                if (tc$traces[[j]]$mirrorlabels)
                    tc$height$topaxistitle.useStrheight[iplot] <- FALSE
                else
                    tc$height$xaxistitle.useStrheight[iplot] <- FALSE
            }

        }
    }

    # Get data for each level for multitrace plots
    multitrace <- FALSE
    shadowgroup <- NULL
    ctr <- 0
    
    datalist <- labellist <- vector("list",maxtracelength) 
    for (j in 1:length(tc$traces)) {
      cond <- ( (tc$traces[[j]]$plot==iplot) && (tc$traces[[j]]$graphic=="ropeladder") )
        if (cond) {
          ctr <- ctr + 1
          if (ctr>1) multitrace <- TRUE          
          if (!is.null(tc$traces[[j]]$shadowrow))
            shadowgroup <- tc$traces[[j]]$shadowrow
          ct <- tc$traces[[j]]
          labelaxis <- tc$traces[[j]]$labelaxis
          dataaxis <- tc$traces[[j]]$dataaxis
          for (k in 1:length(tc$traces[[j]][[labelaxis]])) {              
            labellist[[k]] <- c(labellist[[k]],tc$traces[[j]][[labelaxis]][k])
          }
          for (k in 1:length(tc$traces[[j]][[dataaxis]])) {
            datalist[[k]] <- c(datalist[[k]],tc$traces[[j]][[dataaxis]][k])
          }  
        }
    }
    if (multitrace) {
      tc$traces[[ctr]]$multitrace <- multitrace
      tc$traces[[ctr]]$groupdata <- datalist
      tc$traces[[ctr]]$grouplabel <- labellist
      tc$traces[[ctr]]$sr <- shadowgroup
    }
    
    
    
    
    tc
    
}
