"nightplot" <-
function(...){  
  args <- list(...,graphic="nightplot")
  class(args) <- c(class(args),"tileTrace","nightplot")
  args
}

