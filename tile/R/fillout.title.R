"fillout.title" <-
function(titlename,tolength,tc) {
  tc[[titlename]]$cex <- fillout(tc[[titlename]]$cex,tolength)
  tc[[titlename]]$fontsize <- fillout(tc[[titlename]]$fontsize,tolength)
  tc[[titlename]]$fontface <- fillout(tc[[titlename]]$fontface,tolength)
  tc[[titlename]]$rot <- fillout(tc[[titlename]]$rot,tolength)
  tc[[titlename]]$x <- fillout(tc[[titlename]]$x,tolength)
  tc[[titlename]]$y <- fillout(tc[[titlename]]$y,tolength)
  tc[[titlename]]$col <- fillout(tc[[titlename]]$col,tolength)
 

  # Experimental:  allow substitution by plot number
  components <- c("cex", "fontsize", "fontface", "rot", "x", "y", "col") 
  for (j in 1:length(components)) {
    for (i in 1:tolength) {
      slot <- paste(components[j],i,sep="")
      if (!is.null(tc[[titlename]][[slot]]))
        tc[[titlename]][[components[j]]][i] <- tc[[titlename]][[slot]][1]
    }
  }

  components <- c("labels") 
  for (j in 1:length(components)) {
    anylabels <- FALSE
    for (i in 1:tolength) {
      slot <- paste(components[j],i,sep="")
      if (!is.null(tc[[titlename]][[slot]]))
       anylabels <- TRUE
    }
    if (anylabels) {
      tc[[titlename]]$add <- TRUE
      if (is.null(tc[[titlename]][[components[j]]]))
        tc[[titlename]][[components[j]]] <- rep("",tolength)
      for (i in 1:tolength) {
        slot <- paste(components[j],i,sep="")
        if (!is.null(tc[[titlename]][[slot]]))
          tc[[titlename]][[components[j]]][i] <- tc[[titlename]][[slot]][1]
      }
    }
  }


  ladd <- length(tc[[titlename]]$add)
  tc[[titlename]]$add <- fillout(tc[[titlename]]$add,tolength)
  
  if (length(tc$useplot)>ladd) 
    for (i in (ladd + 1):length(tc$useplot))
      if (!tc$useplot[i]) 
        tc[[titlename]]$add[i] <- FALSE
  #tc[[titlename]]$add <- fillout(tc[[titlename]]$add,tolength)
  
  tc
}

