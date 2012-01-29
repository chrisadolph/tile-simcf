"polylinesTile" <-
function(...){  
  args <- list(...,graphic="polylines")
  class(args) <- c(class(args),"tileTrace","polylines")
  args
}

