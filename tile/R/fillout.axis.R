"fillout.axis" <- function(axisname,tolength,tc) {
    
    tc[[axisname]]$recommendadd <- fillout(NA,tolength)
    tc[[axisname]]$tick.length <- fillout(tc[[axisname]]$tick.length,tolength)
    tc[[axisname]]$label.loc <- fillout(tc[[axisname]]$label.loc,tolength)
    tc[[axisname]]$ticks <- fillout(tc[[axisname]]$ticks,tolength)
    tc[[axisname]]$ntics <- fillout(tc[[axisname]]$ntics,tolength)
    tc[[axisname]]$major <- fillout(tc[[axisname]]$major,tolength)
    tc[[axisname]]$col <- fillout(tc[[axisname]]$col,tolength)
    tc[[axisname]]$lwd <- fillout(tc[[axisname]]$lwd,tolength)
    tc[[axisname]]$cex <- fillout(tc[[axisname]]$cex,tolength)
    tc[[axisname]]$rot <- fillout(tc[[axisname]]$rot,tolength)
    tc[[axisname]]$fontsize <- fillout(tc[[axisname]]$fontsize,tolength)
    tc[[axisname]]$at <- fillout.matrix(tc[[axisname]]$at,tolength)
    tc[[axisname]]$labels <- fillout.matrix(tc[[axisname]]$labels,tolength)
    tc[[axisname]]$log <- fillout(tc[[axisname]]$log,tolength)
    tc[[axisname]]$rug <- fillout(tc[[axisname]]$rug,tolength)
    if (!is.null(tc[[axisname]]$add)) {
      ladd <- length(tc[[axisname]]$add)
      tc[[axisname]]$add <- fillout(tc[[axisname]]$add,tolength)
      if (length(tc$useplot)>ladd)
        for (i in (ladd + 1):length(tc$useplot))
          if (!tc$useplot[i])
            tc[[axisname]]$add[i] <- FALSE
    } else {
        tc[[axisname]]$add <- fillout(NA,tolength)          # was FALSE
    }


    # Experimental:  allow substitution by plot number
    components <- c("tick.length", "label.loc", "ticks", "ntics", "major", "col",
                    "lwd", "cex", "fontsize", "log", "add")
    for (j in 1:length(components)) {
      for (i in 1:tolength) {
        slot <- paste(components[j],i,sep="")
        if (!is.null(tc[[axisname]][[slot]]))
          tc[[axisname]][[components[j]]][i] <- tc[[axisname]][[slot]][1]
        }
    }

    components <- c("at", "labels")
    for (j in 1:length(components)) {
      for (i in 1:tolength) {
        slot <- paste(components[j],i,sep="")
        if (!is.null(tc[[axisname]][[slot]])) {  
          ls <- length(tc[[axisname]][[slot]])
          cc <- ncol(as.matrix(tc[[axisname]][[components[j]]]))
          if (ls > cc) {
            tc[[axisname]][[components[j]]] <- cbind(tc[[axisname]][[components[j]]],
              matrix(NA, nrow=nrow(as.matrix(tc[[axisname]][[components[j]]])),
                     ncol= ls - cc))
            tc[[axisname]][[components[j]]][i,] <- tc[[axisname]][[slot]]
          } else {
            tc[[axisname]][[components[j]]][i,] <- c(tc[[axisname]][[slot]], rep(NA, cc-ls))
          }
        }
      }
    }
    
    tc
}

