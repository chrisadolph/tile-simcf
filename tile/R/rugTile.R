"rugTile" <-
function(...){  
  args <- list(...,graphic="rug")
  class(args) <- c(class(args),"tileTrace","rug")
  args
}

