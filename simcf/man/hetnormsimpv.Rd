\name{hetnormsimpv}
\alias{hetnormsimpv}
\title{Simulate quantities of interest and predictive intervals for
  heteroskedastic linear models}
\description{
  Simulate and summarize uncertainty of conditional predicted values from
  estimated heteroskedastic linear models
}
\usage{
hetnormsimpv(x, b, z, g, ci=0.95, constant=1, varconstant=1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{list, a counterfactual object created by \code{cfMake}, or a vector or matrix of counterfactual values of the covariates, including multiple rows to simulate different counterfactual scenarios, and one column for each covariate of the mean systematic component}
  \item{b}{matrix, simulated parameters for the mean systematic component, one row per draw from the
    estimated model, and one column per parameter, including any constants}
  \item{z}{list, a counterfactual object created by \code{cfMake}, or a vector or matrix of counterfactual values of the covariates, including multiple rows to simulate different counterfactual scenarios, and one column for each covariate of the variance systematic component}
  \item{g}{matrix, simulated parameters for the variance systematic component, one row per draw from the
    estimated model, and one column per parameter, including any constants}
  \item{ci}{vector, the requested intervals of the simulated quantity of
    interest to be reported}
  \item{constant}{scalar, the column of \code{b} containing the
    constant for the mean systematic component, or \code{NA} for no constant}
  \item{varconstant}{scalar, the column of \code{b} containing the
    constant for the variance systematic component, or \code{NA} for no constant}
}
\details{

  In linear regression, the response variable is
   normally distributed with variable mean but fixed variance
  across all observations.  In contrast, the linear heteroskedastic
  model considers a normally distributed response variable whose mean
  and variance both vary across observations:

    \deqn{y_i \sim f_\mathrm{Normal}(\mu_i, \sigma_i^2)}{y_i ~ Normal(mu_i, sigma_i^2)}

  The model contains a mean systematic component,

  \deqn{\mu_i = x_ib}{mu_i = x_i * b}

  and a variance systematic component,

  \deqn{\sigma_i^2 = \mathrm{exp}(z_ig)}{sigma_i^2 = exp(z_i * g)}

  The heteroskedastic normal model allows covariates to explain both
  changes in the mean and changes in the variance of the response
  variable, and can be estimated by maximum likelihood.

  The functions documented above assume that the user has estimated
  such a model, and wishes to calculate predicted values of the
  response for hypothetical values of x and z.
  
  \code{hetnormsimpv} takes simulated parameters b and g from an
  estimated linear heteroskedastic model, and counterfactual values of
  the covariates for the mean and variance systemtic components (x and
  z), and calculates the conditional predicted value of the response,
  including uncertainty intervals around that estimate.

  You may use \code{cfMake} to initialize \code{counterfactual}
  objects for \code{x} and \code{z}, or input them directly as
  matrices.

  If the function you used to estimate the model does not provide
  simulated parameter values, you can often draw  them yourself, e.g., using
  functions such as \code{\link{vcov}} and \code{\link{mvrnorm}} in the
  \pkg{MASS} package, as shown below.

  \code{\link{zelig}}, in the package \pkg{Zelig}, offers similar features for a wide array of
  models and with automated handling of the simulation process.  These
  functions are offered as a simple alternative for users with
  simulations already in hand.
}
\value{
  Returns a list with components:
  \item{pe}{vector, the point estimate(s) of the requested quantity of interest}
  \item{lower}{vector or matrix, the requested lower confidence bounds around the quantity of
    interest; rows are scenarios, columns are intervals}
  \item{upper}{vector or matrix, the requested upper confidence bounds around the quantity of
    interest; rows are scenarios, columns are intervals}
}
%\references{ ~put references to the literature/web site here ~ }
\author{Christopher Adolph <\email{cadolph@u.washington.edu}>}
\seealso{\code{\link{cfMake}}, \code{\link{cfChange}}, \code{\link{cfName}}, \code{\link{linearsimev}}}
\examples{

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{models}


