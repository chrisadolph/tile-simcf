"textTile" <-
function(...){  
  args <- list(...,graphic="text")
  class(args) <- c(class(args),"tileTrace","text")
  args
}

