\name{lagpanel}
\alias{lagpanel}
\title{Lag a variable with panel structure}
\description{
  Correctly lags a variable which has both time series and cross-sectional indexes }
\usage{
lagpanel(x, c, t, lagnum)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{n x k matrix of data to be lagged; must be in time series order and stacked by unit}
  \item{c}{n x 1 vector of group numbers}
  \item{t}{n x 1 vector of periods}
  \item{lagnum}{Numeric, the desired lag to report}

}
\details{If you use lag() to attempt to create lags of panel data (i.e., time series data for multiple units all contained in a single column of values), you will induce massive measurement error by "lagging" the last period of unit i over to the first period of unit i+1.  In contrast, lagpanel() correctly lags panel data, introducing NAs where the previous value for each unit is not contained in the dataset.  lagpanel can create more distant lags by setting the lagnum argument appropriately.

Take care to provide lagpanel with data which have been sorted such that each unit comes in a time-ordered block of observations.  

Users of lagpanel may sometimes get unexpected results.  This is generally because of miscoding of the data itself (e.g., some unit names are inconsistent).  lagpanel demands that panel data be in proper order, and thus is in some sense a decent check on whether such order is present.
 }
\value{
  Returns an n x k matrix of data, lagged by t periods
}
%\references{ ~put references to the literature/web site here ~ }
\author{Christopher Adolph <\email{cadolph@u.washington.edu}>}
%\seealso{\code{\link{probitsimev}}, \code{\link{mlogitsimev}}}
\examples{

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{design}
\keyword{manip}


