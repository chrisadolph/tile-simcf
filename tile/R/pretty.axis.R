"pretty.axis" <-
function(tc,atype,iplot) {
    at <- tc[[atype]]$at[tc$iplot,]
    if (length(na.omit(at))==0) {
      if (atype=="xaxis"||atype=="topaxis")
        at <- grid.pretty(tc$limits[tc$iplot,1:2])#,n=tc[[atype]]$ntics[tc$iplot])
      else 
        at <- grid.pretty(tc$limits[tc$iplot,3:4])#,n=tc[[atype]]$ntics[tc$iplot])
    } else {
      at <- tc[[atype]]$at[tc$iplot,]
    }
  at
}

