"linesTile" <-
function(...){  
  args <- list(...,graphic="lines")
  class(args) <- c(class(args),"tileTrace","lines")
  args
}

