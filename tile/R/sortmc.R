"sortmc" <-
function(Mat, Sort, decreasing=FALSE) 
{
  if (decreasing) direction <- -1 else direction <- 1
  m <- do.call("order", as.data.frame(direction*Mat[, Sort,drop=FALSE]))
  Mat[m, ,drop=FALSE] 
}

