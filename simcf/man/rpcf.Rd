\name{rpcf}
\alias{rpcf}
\title{Ratio-preserving counterfactuals}
\description{
  Change one element of a vector with a fixed sum without changing the sum or the ratios of the other elements
}
\usage{
rpcf(x, c, delta = 1, constraint = 1, scale = "unit")
}

\arguments{
  \item{x}{n x p matrix of n compositions with p categories each.  All rows of x must sum to the \code{constraint}.}
  \item{c}{k vector of the numbers of columns to adjust by delta.  k must be less than p, the number of columns in x.  The requested adjustments must be logically possible; ie, respect the \code{constraint}.}
  \item{delta}{scalar or k vector of adjustments to make to the columns of listed in \code{c}.  The requested adjustments must be logically possible; ie, respect the \code{constraint}. The default is 1, which often will be an infeasibly large change.}
  \item{constraint}{scalar or k vector of logically required sums for each composition.  The default is 1.}
  \item{scale}{character, whether the adjustments in requested in \code{delta} are in units of x (\code{unit}, the default) or standard deviations of x (\code{sd}).}
}
\details{
 XXX
}
\value{
  
}
%\references{ ~put references to the literature/web site here ~ }
\author{Christopher Adolph <\email{cadolph@u.washington.edu}>}
\examples{

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{design}
\keyword{manip}



