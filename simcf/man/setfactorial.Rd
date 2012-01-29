\name{cfFactorial}
\alias{cfFactorial}
\title{Create a set of counterfactuals using a factorial design}
\description{
  Converts multiple sets of hypothetical covariate values into a single
  dataframe with all possible combinations of covariate values
}
\usage{
cfFactorial(...)
}
\arguments{
  \item{\dots}{vectors of hypothetical values for each of several
    covariates, usually provided in \code{tag} = \code{value} format,
    where \code{tag} matches the name of the original (ie, factual) data}
}
\details{
Useful for creating counterfactuals to input into the post-estimation simulation
functions of the \code{simcf} package or into \code{whatif} in the
\code{WhatIf} package. 
}
\value{
A dataframe with columns corresponding to the provided variables, and
rows containing every possible combination of the given covariate values
}
\author{Christopher Adolph <\email{cadolph@u.washington.edu}>}
\examples{
xhyp <- c(1,2,3)
whyp <- c(TRUE, FALSE)
zhyp <- c("a","b","c")
cfFactorial(x = xhyp, w = whyp, z = zhyp)
}
\keyword{design}
\keyword{manip}


