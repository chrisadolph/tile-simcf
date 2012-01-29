"fillout.matrix" <-
function(x,y,ncol=NULL) {
  if (is.null(ncol)) {
    if (is.vector(x))
      ncol <- length(x)
    if (is.matrix(x))
      ncol <- ncol(x)
  }
  if (is.vector(x)) {
    while (length(x)<ncol)
      x <- c(x,NA)
    x <- t(as.matrix(x[1:ncol]))
    while (nrow(x)<y)  x <- rbind(x,x)
  } else {
    if (is.matrix(x)) {
      while (ncol(x)<ncol)  x <- cbind(x,rep(NA,nrow(x)))
      x <- x[,1:ncol,drop=FALSE]
      while (nrow(x)<y)  x <- rbind(x,x)
    }
  }
  x <- x[1:y,,drop=FALSE]
  x
}

