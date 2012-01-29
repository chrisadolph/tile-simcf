\name{loglinsimev}
\alias{loglinsimev}
\alias{loglinsimfd}
\alias{loglinsimrr}
\title{Simulate quantities of interest and confidence intervals for
  loglinear models}
\description{
  Simulate and summarize uncertainty of conditional expected counts, first
  differences and relative rates from
  estimated loglinear models, such as the Poisson or Negative Binomial}
\usage{
loglinsimev(x, b, ci=0.95, constant=1, period=1, labels=NULL)

loglinsimfd(x, b, ci=0.95, constant=1, period=1, labels=NULL, xpre=NULL)

loglinsimrr(x, b, ci=0.95, constant=1, period=1, labels=NULL, xpre=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{vector, matrix, or list, counterfactual values of the covariates.
    Include multiple rows to simulate different counterfactual
    scenarios.  If a list, may contain \code{x} and \code{xpre} as dataframes, a
    formula object to apply to them, and names for each scenario given
    as the row names for component \code{x}}
  \item{b}{matrix, simulated parameters, one row per draw from the
    estimated model, and one column per parameter, including any constants}
  \item{ci}{vector, the requested intervals of the simulated quantity of
    interest to be reported}
  \item{constant}{scalar, the column of \code{b} containing the model
    constant, or NA for no constant}
  \item{period}{scalar or vector, the length of the period over which
    the count is simulated}
  \item{labels}{string vector, names of each scenario (overrides any
    names in \code{x})}
  \item{xpre}{vector or matrix, counterfactual final values of the
    covariates.  Include multiple rows to simulate
    different counterfactual scenarios; rows must match \code{x}.  Not
    needed is \code{x} is a list containing \code{xpre}}
}
\details{
  Given simulated parameters from an estimated loglinear model, and counterfactual
  values of the covariates, these functions calculate either the
  conditional expected count (\code{loglinsimev}), the conditional first
  difference (\code{loglinsimfd}), or the conditional relative rate of events (\code{loglinsimrr}) and confidence intervals around these point estimate.

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
  Returns a list with four components
  \item{pe}{vector, the point estimate(s) of the requested quantity of interest}
  \item{lower}{matrix, the requested lower bounds around the quantity of
    interest; rows are scenarios, columns are intervals}
  \item{upper}{matrix, the requested upper bounds around the quantity of
    interest; rows are scenarios, columns are intervals}
  \item{labels}{string vector, the names of each scenario (optional)}
}
%\references{ ~put references to the literature/web site here ~ }
\author{Christopher Adolph <\email{cadolph@u.washington.edu}>}
%\seealso{\code{\link{probitsimev}}, \code{\link{mlogitsimev}}}
\examples{

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{models}


