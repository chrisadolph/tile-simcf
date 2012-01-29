"getplottraces" <-
function(tc,data,plotlist,current,axis) {
  axisa <- paste("attachto",axis,sep="")
  x <- as.numeric(tc[[axisa]]) * as.numeric(plotlist==current) * seq(1,length(plotlist))
  is.na(x) <- (x==0)
  x <- na.omit(x)
  data[,x,drop=FALSE]
}

