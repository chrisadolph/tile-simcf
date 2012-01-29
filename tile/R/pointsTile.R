"pointsTile" <-
function(...){  
  args <- list(...,graphic="points")
  class(args) <- c(class(args),"tileTrace","points")
  args
}

