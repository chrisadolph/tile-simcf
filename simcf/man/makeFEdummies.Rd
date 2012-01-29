\name{makeFEdummies}
\alias{makeFEdummies}
\title{Create a matrix of dummy variables}
\description{
  Creates a matrix of dummy variables for use in model matrix of fixed effects specifications }
\usage{
makeFEdummies(unit,names=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{unit}{numeric or character vector of group numbers or names, one for each observation}
  \item{names}{character vector of group names, one for each unique group number in \code{unit}}

}
%\details{
% 
%}
\value{
  Returns a matrix of binary variables, one row for each observation, and one column for each group

}
%\references{ ~put references to the literature/web site here ~ }
\author{Christopher Adolph <\email{cadolph@u.washington.edu}>}
%\seealso{\code{\link{probitsimev}}, \code{\link{mlogitsimev}}}
\examples{

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{design}


