"riceplot" <-
function(...){  
  args <- list(...,graphic="riceplot")
  class(args) <- c(class(args),"tileTrace","riceplot")
  args
}

