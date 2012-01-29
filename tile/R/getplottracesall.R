"getplottracesall" <-
function(tc,data,plotlist,axis) {
  axisa <- paste("attachto",axis,sep="")
  x <- as.numeric(tc[[axisa]]) * seq(1,length(plotlist))
  is.na(x) <- (x==0)
  x <- na.omit(x)
  data[,x,drop=FALSE]
}

