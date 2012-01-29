"lineplot" <-
function(...){  
  args <- list(...,graphic="lineplot")
  class(args) <- c(class(args),"tileTrace","lineplot")
  args
}

