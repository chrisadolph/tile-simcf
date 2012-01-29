\name{cfFactorial}
\alias{cfFactorial}
\title{Create a set of counterfactuals using a factorial design}
\description{
  Converts multiple sets of hypothetical covariate values into a single
  dataframe with all possible combinations of covariate values
}
\usage{
cfFactorial(...,formula=NULL,data=NULL,names=NULL,hull=FALSE,f="mean")
}
\arguments{
  \item{\dots}{vectors of hypothetical values for each of several
    covariates.  \bold{Must} be provided in \code{tag} = \code{value} format,
    where \code{tag} matches the name of the original (ie, factual)
    data}
  \item{formula}{forumla object, the model estimated}
  \item{data}{dataframe, the actual data used to estimate the
    original model.  Any covariates not specified in \dots will be
    assigned baseline values (e.g., their means)}
  \item{names}{string or string vector, the name(s) of the scenario(s),
    e.g., to label them on plots or tables; saved as the row name of the
    appropriate scenario}
   \item{hull}{logical, check for counterfactuals outside the convex hull
  using the WhatIf package}
  \item{f}{function, applied to covariates not specified in \dots to
    complete the
    counterfactuals; default is to take the mean of each covariate}
}
\details{
Useful for creating counterfactuals to input into the post-estimation simulation
functions of the \code{simcf} package or into \code{whatif} in the
\code{WhatIf} package. 
}
\value{
  A list with at least some of the following components:
\item{x}{A dataframe with columns corresponding to the provided variables, and
rows containing every possible combination of the given covariate
values, plus additional columns with the mean (or other summary value)
of the remaining covariates in the model formula, if any}
\item{xpre}{dataframe, as x}
\item{formula}{formula object, the model provided by user}
\item{extrapolatex}{vector, whether the x scenarios lie outside the
  convex hull (TRUE indicates extrapolation)}
\item{extrapolatexpre}{vector, whether the xpre scenarios lie outside the
  convex hull (TRUE indicates extrapolation)}
\item{extrapolatefd}{vector, whether either the x or xpre scenarios lie outside the
  convex hull (TRUE indicates extrapolation)}
}
\author{Christopher Adolph <\email{cadolph@u.washington.edu}>}
\examples{

# Simple example
xhyp <- c(1,2,3)
whyp <- c(TRUE, FALSE)
zhyp <- c("a","b","c")
scenarios <- cfFactorial(x = xhyp, w = whyp, z = zhyp)
print(scenarios$x)

# Complex example


}
\keyword{design}
\keyword{manip}


