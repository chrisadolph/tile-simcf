\name{extractdata}
\alias{extractdata}
\title{Extract from a dataframe all variables used in a formula}
\description{
  Extracts from a dataframe all variables used in a formula, saves these variables to a new dataframe, adds additional variables if requested, and listwise deletes if desired.  Useful for creating a fully observed dataset without deleting on unused variables.}
\usage{
extractdata(formula, data, extra=NULL, na.rm = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{An \R formula object.}
  \item{data}{A dataframe containing all the variables used in \code{formula}.}
  \item{extra}{Either a dataframe with additional variables to add to the output, or a formula object containing additional variables to extract from \code{data}.}
  \item{na.rm}{If \code{TRUE}, listwise delete the newly created dataframe.  Default is \code{FALSE}.}
}
%\details{
% 
%}
\value{
  A data frame.
}
%\references{ ~put references to the literature/web site here ~ }
\author{Christopher Adolph <\email{cadolph@u.washington.edu}>}
%\seealso{\code{\link{probitsimev}}, \code{\link{mlogitsimev}}}
%\examples{

%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}


