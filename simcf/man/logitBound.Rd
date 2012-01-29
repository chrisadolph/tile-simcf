\name{logitBound}
\alias{logitBound}
\title{Logit transform a variable, creating dummy variables to record out of bound cases}
\description{
  If given a variable to logit transform which contains values less than or equal to 0 or greater than or equal to 1, logitBound creates dummy variables indicating the out of bound cases, and collects them in a matrix with the logit transformed variable.}
\usage{
logitBound(x, base=exp(1), forceAny=FALSE, forceAll = FALSE)
}

\arguments{
  \item{x}{A vector of data to be logit tranformed.  Need not be restricted to the interval (0,1).}
  \item{base}{The logarithmic base for transformation}
  \item{forceAny}{Include a column of dummies for cases at or below the zero bound whether or not such cases are present in x.}
  \item{forceAll}{Include a column of dummies for cases at or above the unity bound whether or not such cases are present in x.}
}
\details{ Computes the logit transformation, ln(x/(1-x).  Instead of reporting out of bounds cases as -Inf or Inf, reports out of bounds cases as 0s, but includes dummy variables indicating which boundary is breeched.  Including the output of logitBound on the right-hand side of a regression model allows the user to logit transform even variables which are exactly 0 or 1 by treating those cases as sui generis exceptions.  Analogous to treating x as a hurdle variable.

         LogitBound transformed variables will work in the other functions in the simcf package, but may fail when used in predict commands.                                     
 
}
\value{
  A matrix including up to three columns.  The last is always the logit transformed x, with 0s for any case outside the (0,1) bound.  The optional initial columns are 'any', which is 1 if the corresponding element of x is greater than 0, and 'all', which is 1 if the corresponding element of x is equal to or greater than 1.
}
%\references{ ~put references to the literature/web site here ~ }
\author{Christopher Adolph <\email{cadolph@u.washington.edu}>}
\seealso{\code{\link{logBound}}}
%\examples{

%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}


