logitBound <- function(x, base=exp(1), forceAny=FALSE, forceAll=FALSE) {

  # Can't include below because of simulation use of logitBound()
  #if (length(unique(na.omit(x)))==2) stop("You cannot logit transform, or even logitBound transform, a binary variable")
  #if (length(unique(na.omit(x)))==1) stop("You cannot logit transform, or even logitBound transform, a constant")

  any <- as.numeric(x>0)
  all <- as.numeric(x>=1)
  logit <- suppressWarnings(log(x/(1-x),base=base))
  logit[(any==0)|(all==1)] <- 0
  if (forceAny||any(!any)) {
    if (forceAll|sum(all)) {
      return(cbind(any,all,logit))
    } else {
      return(cbind(any,logit))
    }
  } else {
    if (sum(all)) {
      return(cbind(all,logit))
    } else {
      return(cbind(logit))
    }
  }
}