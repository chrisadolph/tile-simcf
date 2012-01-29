"tileAxistitleprep" <-
function(ttype,tc=list(),...) {
    # If style is NULL, improvise
    # else if style is "all", do below
    # else if style is "row",
        # if x or t axis, set viewport to every column between 1+rtitles and end-1
        # if y or r, do below only if new row
    # else if style is "first", do below only if iplot==1
    atype <- ttype
    ttype <- paste(ttype,"title",sep="")
    if (is.null(tc[[ttype]]$type)) {
      if (length(tc[[ttype]]$labels)==1) {
        tc[[ttype]]$type <- "all"
      } else {
        if ((length(tc[[ttype]]$labels)==tc$nplots)||(length(tc[[ttype]]$labels)==0))
          tc[[ttype]]$type <- "all"
        else {

          if (((atype=="yaxis")||(atype=="topaxis"))&&(length(tc[[ttype]]$labels)==tc$RxC[1])) 
            tc[[ttype]]$type <- "row"

          if (((atype=="xaxis")||(atype=="rightaxis"))&&(length(tc[[ttype]]$labels)==tc$RxC[2])) 
            tc[[ttype]]$type <- "column"

          if (is.null(tc[[ttype]]$type)) {
            tc[[ttype]]$labels <- fillout(tc[[ttype]]$labels,tc$nplots)
            tc[[ttype]]$type <- "all"
          }
        }
      }
    } else {
      if (tc[[ttype]]$type=="all") {
        if (length(tc[[ttype]]$labels)&&(length(tc[[ttype]]$labels)<tc$nplots)) {
          tc[[ttype]]$labels <- fillout(tc[[ttype]]$labels,tc$nplots)
          for (i in 1:tc$nplots) {
            if (is.na(tc[[atype]]$add[i])||!tc[[atype]]$add[i])
              tc[[ttype]]$labels[i] <- NA 
          }
        }
      }
    }    
    tc
}

