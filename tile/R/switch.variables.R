"switch.variables" <-
function(x,y,a,b) {
  if (is.null(y)) {
    y <- fillout.matrix(x,a)
  } else {
    if (is.vector(y))
      y <- as.matrix(fillout(y,b))
    while (ncol(y)<a)
      y <- cbind(y,y)
    y <- y[,1:a,drop=FALSE]
  }
  y
}

