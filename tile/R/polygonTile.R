"polygonTile" <-
function(...){  
  args <- list(...,graphic="polygon")
  class(args) <- c(class(args),"tileTrace","polygon")
  args
}

