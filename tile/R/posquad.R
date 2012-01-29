
posquad <- function(a,b,c) {
  if ((b^2 - 4*a*c)>=0) {
    x1 <- (-b + sqrt(b^2 - 4*a*c))/(2*a)
    x2 <- (-b - sqrt(b^2 - 4*a*c))/(2*a)
    out <- NULL
    if (x1>0) out <- c(out,x1)
    if (x2>0) out <- c(out,x2)
    if (is.null(out)) out <- NA
  } else {
    out <- NA
  }
  out
}
