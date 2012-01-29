\name{ldvsimev}
\alias{ldvsimev}
\alias{ldvsimfd}
\alias{ldvsimrr}
\alias{ldvsimpv}
\alias{ldvsimpd}
\alias{ldvsimpr}
\title{Simulate quantities of interest and confidence intervals for
  linear time series models including ARIMA or lagged dependent variable processes}
\description{
  Simulate and summarize uncertainty of iterated conditional expected
  values (ev), expected first differences (fd), expected relative risks (rr), predicted values (pv), predicted first differences (pd), and predicted relative risks (pr) from estimated ARIMA or lagged dependent variable time series linear models }
\usage{
ldvsimev(x, b, ci=0.95, constant=1, xpre=NULL, phi=NULL, lagY=NULL, 
         transform="none", initialY=NULL, cumulate=FALSE, discount=0)

ldvsimfd(x, b, ci=0.95, constant=1, xpre=NULL, phi=NULL, lagY=NULL, 
         transform="none", cumulate=FALSE, discount=0)

ldvsimrr(x, b, ci=0.95, constant=1, xpre=NULL, phi=NULL, lagY=NULL, 
         transform="none", cumulate=FALSE, discount=0)

ldvsimpv(x, b, ci=0.95, constant=1, xpre=NULL, phi=NULL, lagY=NULL, 
         rho=NULL, lagEps=NULL, sigma=NULL, transform="none", initialY=NULL,
         cumulate=FALSE, discount=0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{vector or matrix, counterfactual values of the covariates.
    Include multiple rows to simulate different counterfactual scenarios}
  \item{b}{matrix, simulated parameters, one row per draw from the
    estimated model, and one column per parameter, including any constants}
  \item{ci}{vector, the requested intervals of the simulated quantity of
    interest to be reported}
  \item{constant}{scalar, the column of \code{b} containing the model
    constant, or NA for no constant}
   \item{xpre}{vector or matrix, counterfactual initial values of the
    covariates.  Include multiple rows to simulate
    different counterfactual scenarios; rows must match \code{x};
    redundant and ignored if \code{x} is a \code{counterfactual object}}
  \item{phi}{scalar of point estimate, or matrix of simulated, AR or lagged DV parameters}
  \item{lagY}{scalar or vector, the prior levels of y or diff(y), 
    most recent first; must match number of columns of \code{phi}}
  \item{rho}{scalar of point estimate, or matrix of simulated, MA parameters}
  \item{lagY}{scalar or vector, the prior levels of the error term, 
    most recent first; must match number of columns of \code{rho}}
  \item{sigma}{scalar of point estimate, or matrix of simulated, white noise parameter or 
    standard error of the regression}	
  \item{transform}{string, transformation applied to the dependent
    variable in the original model; \code{log}, for the natural log, 
    \code{diff}, for differencing; \code{difflog}, for differencing of 
    a logged variable; \code{logit} for the logistic transformation; 
    \code{difflogit} for the differencing of a logit transformed variable;
    and \code{none} for no transformation (default)}
  \item{initialY}{vector, for differenced models, the original level of the response}
  \item{cumulate}{logical, whether to additionally report cumulative values of the quantity of interest (default is not to report).}
  \item{discount}{scalar, the compounding discount rate to apply to future periods in calculating cumulative quantities of interest; default is 0 for no discounting.  Set as a proportion, so that 0.05 would be a 5 percent discount rate.}
}
\details{
  Given simulated parameters from an estimated linear model with one or
  more lagged dependent variables, and counterfactual
  values of the covariates, these functions calculate either the
  conditional expected value of the response iterated over several
  periods, 
  or the conditional difference or risk ratio over those periods, and confidence intervals around that point estimate.  Alternatively, the last function calculates predicted values, which include random noise and moving averages of that noise, also iterated over time and with prediction intervals.  This function is thus suitable for forming most desired quantities of interest from time series linear models with ARIMA or lagged dependent variable processes.

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
  Returns a list with at least four and as many as eight components:
  \item{pe}{vector, the point estimate(s) of the requested quantity of interest}
  \item{lower}{matrix, the requested lower bounds around the quantity of
    interest; rows are scenarios, columns are intervals}
  \item{upper}{matrix, the requested upper bounds around the quantity of
    interest; rows are scenarios, columns are intervals}
  \item{se}{}
  \item{pe.cumulative}{}
  \item{lower.cumulative}{}
  \item{upper.cumulative}{}
  \item{se.cumulative}{}
}
%\references{ ~put references to the literature/web site here ~ }
\author{Christopher Adolph <\email{cadolph@uw.edu}>}
%\seealso{\code{\link{probitsimev}}, \code{\link{mlogitsimev}}}
\examples{

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{models}


