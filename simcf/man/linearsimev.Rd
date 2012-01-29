\name{linearsimev}
\alias{linearsimev}
\alias{linearsimfd}
\alias{linearsimrr}
\title{Simulate quantities of interest and confidence intervals for
  linear models}
\description{
  Simulate and summarize uncertainty of conditional expected values, first
  differences and relative risks from
  estimated linear regression models
}
\usage{
linearsimev(x, b, ci=0.95, constant=1, sigma2=NULL, sims=10, save=FALSE)

linearsimfd(x, b, ci=0.95, constant=1, sigma2=NULL, sims=10, save=FALSE, xpre=NULL)

linearsimrr(x, b, ci=0.95, constant=1, sigma2=NULL, sims=10, save=FALSE, xpre=NULL)
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
  \item{sigma2}{scalar or vector, either the estimated sigma2 from the linear regression, or a vector of simulated sigma2's drawn from the model posterior.  Only required if predicted values are desired.}
  \item{sims}{scalar, the number of draws taken from the predictive distribution of the response.  Only used if sigma2 is provided.  Default is 10 draws.}
  \item{save}{logical, whether to save the simulated expected values (and predicted values, if any), in addition to the point estimates and requested intervals.  Default is FALSE.}
  \item{xpre}{vector or matrix, counterfactual initial values of the
    covariates.  Rows must match \code{x}.  Not needed when \code{x} is a \code{counterfactual} object.}
}
\details{
  Given simulated parameters from an estimated linear model, and counterfactual
  values of the covariates, these functions calculate either the
  conditional expected value of the response (\code{linearsimev}), the conditional first
  difference (\code{linearsimfd}), or the relative risk (\code{linearsimrr}), and confidence intervals around that point estimate (optionally, predictive intervals as well, taking into account the fundametal uncertainty in the response captured by sigma2).  

  Use \code{cfMake} to initialize a \code{counterfactual} object containing \code{x} and \code{xpre}, or input them directly.

  If the function you used to estimate the model does not provide
  simulated parameter values, you can draw often them yourself, e.g., using
  functions such as \code{\link{vcov}} and \code{\link{mvrnorm}} in the
  \pkg{MASS} package, as shown below.

  \code{\link{zelig}}, in the package \pkg{Zelig}, offers similar features for a wide array of
  models and with automated handling of the simulation process.  These
  functions are offered as a simple alternative for users with
  simulations already in hand.
}
\value{
  Returns a list with at least three components, and as many as seven:
  \item{pe}{vector, the point estimate(s) of the requested quantity of interest}
  \item{lower}{vector or matrix, the requested lower confidence bounds around the quantity of
    interest; rows are scenarios, columns are intervals}
  \item{upper}{vector or matrix, the requested upper confidence bounds around the quantity of
    interest; rows are scenarios, columns are intervals}
  \item{plower}{vector or matrix, the requested lower predictive bounds around the quantity of
    interest; rows are scenarios, columns are intervals (requires sigma2 be an input)}
  \item{pupper}{vector or matrix, the requested upper predictive bounds around the quantity of
    interest; rows are scenarios, columns are intervals (requires sigma2 be an input)}
  \item{ev}{vector or matrix, the simulated expected values; rows are scenarios, columns are simulations (only given if save is TRUE)}
  \item{pv}{vector or matrix, the simulated predicted values; rows are scenarios, columns are simulations (only given if sigma2 is provided and save is TRUE)}
}
%\references{ ~put references to the literature/web site here ~ }
\author{Christopher Adolph <\email{cadolph@u.washington.edu}>}
\seealso{\code{\link{cfMake}}, \code{\link{cfChange}}, \code{\link{cfName}}}
\examples{

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{models}


