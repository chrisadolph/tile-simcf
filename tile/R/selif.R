"selif" <-
function(x,c)
{
  x <- cbind(x,t(t(c)));
  w <- x[x[,ncol(x)]!=0];
  dim(w) <- c(length(w)/ncol(x),ncol(x));
  w[,1:(ncol(w)-1),drop=FALSE];
}

