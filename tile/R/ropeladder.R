"ropeladder" <-
function(...){  
  args <- list(...,graphic="ropeladder")
  class(args) <- c(class(args),"tileTrace","ropeladder")
  args
}
