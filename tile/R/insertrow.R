"insertrow" <-
function(y,x,r) {
  if (is.vector(y))
    y <- t(as.matrix(y))
  if (is.null(x))
    x <- as.matrix(NA)
  if (ncol(x)<ncol(y))
    x <- cbind(x,
               matrix(data=NA,
                      nrow=nrow(x),
                      ncol=ncol(y)-ncol(x)
                      )
               )
  if (nrow(x)<max(r))
    x <- rbind(x,
               matrix(data=NA,
                      nrow=max(r)-nrow(x),
                      ncol=ncol(x)
                      )
               )
  x[r,1:ncol(y)] <- y
  x
}

