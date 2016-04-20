logBound <- function(x, base=exp(1), forceAny=FALSE) {
  any <- as.numeric(x>0)
  log <- suppressWarnings(log(x,base=base))
  log[!any] <- 0
  if (forceAny|any(!any)) {
    return(cbind(any,log))
  } else {
    return(cbind(log))
  }
}
