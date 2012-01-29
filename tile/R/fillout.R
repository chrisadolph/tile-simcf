"fillout" <-
function(x,y) {
  if (is.vector(x))
    while (length(x)<y)  x <- c(x,x)
  x[1:y]
}

