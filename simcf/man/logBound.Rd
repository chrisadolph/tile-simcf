\name{logBound}
\alias{logBound}
\title{Log transform a variable, creating a dummy variable to record out of bound cases}
\description{
  If given a variable to log transform which contains values less than or equal to 0, logBound creates a dummy variable indicating the out of bound cases, and collects them in a matrix with the log transformed variable.}
\usage{
logBound(x, base=exp(1), forceAny=FALSE)
}

\arguments{
  \item{x}{A vector of data to be log tranformed.  Need not be restricted to the interval (0,Inf).}
  \item{base}{The logarithmic base for transformation}
  \item{forceAny}{Include a column of dummies for cases at or below the zero bound whether or not such cases are present in x.}
}
\details{ Computes the log transformation, log(x).  Instead of reporting out of bounds cases as -Inf or NaN, reports out of bounds cases as 0s, but includes a dummy variable indicating the lower boundary is breeched.  Including the output of logBound on the right-hand side of a regression model allows the user to log transform even variables which are exactly 0 by treating those cases as sui generis exceptions.  Analogous to treating x as a hurdle variable.

                          LogBound transformed variables will work in the other functions in the simcf package, but may fail when used in predict commands.                                     
                             
 
}
\value{
  A matrix including up to two columns.  The last is always the log transformed x, with 0s for any case outside the (0,Inf) bounds.  The optional initial column is 'any', which is 1 if the corresponding element of x is greater than 0.
}
%\references{ ~put references to the literature/web site here ~ }
\author{Christopher Adolph <\email{cadolph@u.washington.edu}>}
\seealso{\code{\link{logitBound}}}
%\examples{

%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}


