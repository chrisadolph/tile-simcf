"filloutunits" <-
function(x,y) {
  if (is.unit(x))
    while (length(x)<y)  x <- unit.c(x,x)
  x[1:y]
}

