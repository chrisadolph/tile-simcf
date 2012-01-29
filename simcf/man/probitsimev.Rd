\name{probitsimev}
\alias{probitsimev}
\alias{probitsimfd}
\alias{probitsimrr}
\title{Simulate quantities of interest and confidence intervals for
  binary probit}
\description{
  Simulate and summarize uncertainty of conditional expected values, first
  differences and relative risks from
  estimated binary probit models
}
\usage{
probitsimev(x, b, ci=0.95, constant=1)

probitsimfd(x, b, ci=0.95, constant=1, xpre=NULL)

probitsimrr(x, b, ci=0.95, constant=1, xpre=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{list, a counterfactual object created by \code{cfMake}, or a vector or matrix of counterfactual values of the covariates, including multiple rows to simulate different counterfactual scenarios, and one column for each covariate}
  \item{b}{matrix, simulated parameters, one row per draw from the
    estimated model, and one column per parameter, including any constants}
  \item{ci}{vector, the requested intervals of the simulated quantity of
    interest to be reported}
  \item{constant}{scalar, the column of \code{b} containing the model
    constant, or \code{NA} for no constant}
  \item{xpre}{vector or matrix, counterfactual initial values of the
    covariates.  Rows must match \code{x}.  Not needed when \code{x} is a \code{counterfactual} object.}
}
\details{
  Given simulated parameters from an estimated probit model, and counterfactual
  values of the covariates, these functions calculate either the
  conditional expected value of the response (\code{probitsimev}), the conditional first
  difference (\code{probitsimfd}), or the relative risk (\code{probitsimrr}), and confidence intervals around that point estimate.  Use \code{cfMake} to initialize a \code{counterfactual} object containing \code{x} and \code{xpre}, or input them directly.

  If the function you used to estimate the model does not provide
  simulated parameter values, you can draw often them yourself, e.g., using
  functions such as \code{\link{vcov}} and \code{mvrnorm} in the
  \code{MASS} package, as shown below.

  zelig, in the package Zelig, offers similar features for a wide array of
  models and with automated handling of the simulation process.  These
  functions are offered as a simple alternative for users with
  simulations already in hand.
}
\value{
  Returns a list with three components
  \item{pe}{vector, the point estimate(s) of the requested quantity of interest}
  \item{lower}{matrix, the requested lower bounds around the quantity of
    interest; rows are scenarios, columns are intervals}
  \item{upper}{matrix, the requested upper bounds around the quantity of
    interest; rows are scenarios, columns are intervals}
}
%\references{ ~put references to the literature/web site here ~ }
\author{Christopher Adolph <\email{cadolph@u.washington.edu}>}
\seealso{\code{\link{logitsimev}}, \code{\link{mlogitsimev}}, \code{\link{cfMake}}, \code{\link{cfChange}}, \code{\link{cfName}}}
\examples{

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{models}


