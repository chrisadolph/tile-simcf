# Lag panel data
# Chris Adolph

# Usage:
#
# laggeddata <- lagpanel(x,  # An n x k matrix of data to be lagged;
                             # must be in time series order and stacked by
                             # unit
#                        c,  # An n x 1 vector of group numbers
#                        t,  # An n x 1 vector of periods
#                        lagnum, # The desired lag to report

lagpanel <- function(x,c,t,lagnum) {
  outclass <- "matrix"
  if (any(class(x)=="data.frame")) outclass <- "data.frame"
  x <- as.matrix(x)
  outnames <- colnames(x)
  c <- as.matrix(c)
  t <- as.matrix(t)
  listc <- unique(c)
  outmat <- matrix(NA,nrow=nrow(x),ncol=ncol(x))
  runningtotal <- 0
  for (i in 1:nrow(listc)) {
    numtper <- length(unique(t[c==listc[i,]]))
    xc <- as.matrix(x[c==listc[i,],])
    if (nrow(xc)>numtper)
      stop(paste("Duplicate time periods in data for unit", listc[i]))
    if (numtper>lagnum) {
      outmat[(runningtotal+1+lagnum):(runningtotal + numtper),] <-
        xc[1:(nrow(xc)-lagnum),]
    }
    runningtotal <- runningtotal + numtper
  }
  if (outclass=="data.frame") outmat <- as.data.frame(outmat)
  colnames(outmat) <- outnames
  outmat
}
