"scatter" <-
function(...){  
  args <- list(...,graphic="scatter")
  class(args) <- c(class(args),"tileTrace","scatter")
  args
}

