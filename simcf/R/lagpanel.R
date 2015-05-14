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





#' Lag a variable with panel structure
#' 
#' Correctly lags a variable which has both time series and cross-sectional
#' indexes
#' 
#' If you use lag() to attempt to create lags of panel data (i.e., time series
#' data for multiple units all contained in a single column of values), you
#' will induce massive measurement error by "lagging" the last period of unit i
#' over to the first period of unit i+1.  In contrast, lagpanel() correctly
#' lags panel data, introducing NAs where the previous value for each unit is
#' not contained in the dataset.  lagpanel can create more distant lags by
#' setting the lagnum argument appropriately.
#' 
#' Take care to provide lagpanel with data which have been sorted such that
#' each unit comes in a time-ordered block of observations.
#' 
#' Users of lagpanel may sometimes get unexpected results.  This is generally
#' because of miscoding of the data itself (e.g., some unit names are
#' inconsistent).  lagpanel demands that panel data be in proper order, and
#' thus is in some sense a decent check on whether such order is present.
#' 
#' @param x n x k matrix of data to be lagged; must be in time series order and
#' stacked by unit
#' @param c n x 1 vector of group numbers
#' @param t n x 1 vector of periods
#' @param lagnum Numeric, the desired lag to report
#' @return Returns an n x k matrix of data, lagged by t periods
#' @author Christopher Adolph <\email{cadolph@@u.washington.edu}>
#' @keywords design manip
#' @export
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
