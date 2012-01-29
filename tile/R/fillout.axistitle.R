"fillout.axistitle" <-
function(axisname,tolength,tc) {
    axisname <- paste(axisname,"title",sep="")
    tc[[axisname]]$cex <- fillout(tc[[axisname]]$cex,tolength)
    tc[[axisname]]$fontsize <- fillout(tc[[axisname]]$fontsize,tolength)
    tc[[axisname]]$fontface <- fillout(tc[[axisname]]$fontface,tolength)
    tc[[axisname]]$rot <- fillout(tc[[axisname]]$rot,tolength)
    tc[[axisname]]$x <- fillout(tc[[axisname]]$x,tolength)
    tc[[axisname]]$y <- fillout(tc[[axisname]]$y,tolength)
    tc[[axisname]]$col <- fillout(tc[[axisname]]$col,tolength)

    
    # Experimental:  allow substitution by plot number
    components <- c("cex", "fontsize", "fontface", "rot", "x", "y", "col", "labels")  #"labels" ?
    for (j in 1:length(components)) {
      for (i in 1:tolength) {
        slot <- paste(components[j],i,sep="")
        if (!is.null(tc[[axisname]][[slot]]))
          tc[[axisname]][[components[j]]][i] <- tc[[axisname]][[slot]][1]
      }
    }

    components <- c("labels") 
    for (j in 1:length(components)) {
      anylabels <- FALSE
      for (i in 1:tolength) {
        slot <- paste(components[j],i,sep="")
        if (!is.null(tc[[axisname]][[slot]]))
          anylabels <- TRUE
      }
      if (anylabels) {
        tc[[axisname]]$add <- TRUE
        if (is.null(tc[[axisname]][[components[j]]]))
          tc[[axisname]][[components[j]]] <- rep("",tolength)
        for (i in 1:tolength) {
          slot <- paste(components[j],i,sep="")
          if (!is.null(tc[[axisname]][[slot]]))
            tc[[axisname]][[components[j]]][i] <- tc[[axisname]][[slot]][1]
        }
      } else {
        if (is.null(tc[[axisname]]$type)||(tc[[axisname]]$type=="all")) {
          tc[[axisname]][[components[j]]] <- rep(tc[[axisname]][[components[j]]],tolength)
        }
      }
    }

    # fillout add
    if (!is.null(tc[[axisname]]$add)) {
      ladd <- length(tc[[axisname]]$add)
      tc[[axisname]]$add <- fillout(tc[[axisname]]$add,tolength)
      if (length(tc$useplot)>ladd)
        for (i in (ladd + 1):length(tc$useplot)) {
          if ((!tc$useplot[i])||is.null(tc[[axisname]]$labels)||suppressWarnings(is.na(tc[[axisname]]$labels[i]))||identical(tc[[axisname]]$labels[i],""))
            tc[[axisname]]$add[i] <- FALSE
          else
            tc[[axisname]]$add[i] <- TRUE
        }
    } else {
        tc[[axisname]]$add <- fillout(FALSE,tolength)          # was NA
    }
    
    if ((axisname=="xaxistitle")||(axisname=="topaxistitle")) {
        tc$height[[paste(axisname,"useStrheight",sep=".")]] <- fillout(TRUE,tolength)
        for (i in 1:tolength)
            if ((tc[[axisname]]$rot[i]==90)||(tc[[axisname]]$rot[i]==270))
                tc$height[[axisname]][[paste(axisname,"useStrheight",sep=".")]] <- FALSE
    } else {
        tc$height[[paste(axisname,"useStrheight",sep=".")]] <- fillout(FALSE,tolength)
        for (i in 1:tolength)
            if ((tc[[axisname]]$rot[i]==90)||(tc[[axisname]]$rot[i]==270))
                tc$height[[axisname]][[paste(axisname,"useStrheight",sep=".")]] <- TRUE
    }

    if ((axisname=="xaxistitle")||(axisname=="topaxistitle")) {
        tc$width[[paste(axisname,"useStrheight",sep=".")]] <- fillout(FALSE,tolength)
        for (i in 1:tolength)
            if ((tc[[axisname]]$rot[i]==90)||(tc[[axisname]]$rot[i]==270))
                tc$width[[axisname]][[paste(axisname,"useStrheight",sep=".")]] <- TRUE
    } else {
        tc$width[[paste(axisname,"useStrheight",sep=".")]] <- fillout(TRUE,tolength)
        for (i in 1:tolength)
            if ((tc[[axisname]]$rot[i]==90)||(tc[[axisname]]$rot[i]==270))
                tc$width[[axisname]][[paste(axisname,"useStrheight",sep=".")]] <- FALSE
    }
    
    tc
}

