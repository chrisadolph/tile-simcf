\name{mlogitsimev}
\alias{mlogitsimev}
\alias{mlogitsimfd}
\alias{mlogitsimrr}
\title{Simulate quantities of interest and confidence intervals for
  multinomial logit}
\description{
  Simulate and summarize uncertainty of conditional expected values, first
  differences and relative risks from
  estimated multinomial logit models
}
\usage{
mlogitsimev(x, b, ci=0.95, constant=1, z=NULL, g=NULL)

mlogitsimfd(x, b, ci=0.95, constant=1, xpre=NULL,
            zpre=NULL, zpost=NULL, g=NULL)

mlogitsimrr(x, b, ci=0.95, constant=1, xpre=NULL,
            zpre=NULL, zpost=NULL, g=NULL)
}
\arguments{
  \item{x}{vector or matrix, counterfactual values of
    covariates with category-specific parameters.
    Include multiple rows to simulate different counterfactual scenarios}
  \item{b}{3d array, simulated parameters, one row per draw from the
    estimated model, one column per parameter, including any
    constants, and one "sheet" for all categories except the reference
    category.}
  \item{ci}{vector, the requested intervals of the simulated quantity of
    interest to be reported}
  \item{constant}{scalar, the column of \code{b} containing the model
    constant, or NA for no constant}
  \item{z}{3d array, counterfactual values of covariates specific to the
    response category.  Include one row for each counterfactual
    scenario, one column for each parameter, and one "sheet" for each
    category (do not omit any reference categories here)}
  \item{g}{3d array, simulated parameters, one row per draw from the
    estimated model, one column per parameter,
    and one "sheet" for all categories (do not omit any reference
    categories here)}
  \item{xpre}{vector or matrix, counterfactual \emph{initial} values of
    covariates with category-specific parameters.  Include multiple rows
    to simulate different counterfactual scenarios}
  \item{zpre}{3d array, counterfactual \emph{initial} values of
    covariates specific to the response category.  Include one row for each counterfactual
    scenario, one column for each parameter, and one "sheet" for each
    category (do not omit any reference categories here)}
  \item{zpost}{3d array, counterfactual \emph{final} values of covariates specific to the
    response category.  Include one row for each counterfactual
    scenario, one column for each parameter, and one "sheet" for each
    category (do not omit any reference categories here)}
}
\details{
  Given simulated parameters from an estimated multinomial logit model,
  and counterfactual values of the covariates, calculate either the
  conditional expected value of the response (\code{mlogitsimev}), the conditional first
  difference (\code{mlogitsimfd}), or the relative risk (\code{mlogitsimrr}), and confidence intervals around that point estimate.

  If the function you used to estimate the model does not provide
  simulated parameter values, you can draw often them yourself, e.g., using
  functions such as \code{\link{vcov}} and \code{mvrnorm} in the
  \code{MASS} package, as shown below.

  You must provide either observation-specific (\code{x}) or
  category-specific (\code{z}) covariates, or both, and the appropriate
  parameters (\code{b}, \code{g}, or both, respectively).  Include any
  observation-and-category-specific covariates in \code{z}.
  
  zelig, in the package Zelig, offers similar features for a wide array of
  models and with automated handling of the simulation process.  These
  functions are offered as a simple alternative for users with
  simulations already in hand.
}
\value{
  Returns a list with three components
  \item{pe}{matrix, the point estimates of the requested quantity of
  interest; rows are scenarios, and columns are categories}
  \item{lower}{array, the requested lower bounds around the quantity of
    interest; rows are scenarios, columns are the category of the response, and the third dimension is the confidence interval}
  \item{upper}{array, the requested upper bounds around the quantity of
    interest; rows are scenarios, columns are the category of the response, and the third dimension is the confidence interval}
}
%\references{ ~put references to the literature/web site here ~ }
\author{Christopher Adolph <\email{cadolph@u.washington.edu}>}
\seealso{\code{\link{logitsimev}}}
\examples{
# Multinomial Logistic Regression of alligator food
# See tile package function lineplot for graphical presentation of this example

# Load data and libraries
data(gator)
require(MASS)
require(nnet)

# Estimate MNL using the nnet library
mlogit.result <- multinom(food ~ size + female, Hess=TRUE)
pe <- mlogit.result$wts[c(6,7,8,10,11,12)]
                                      # point estimates
vc <- solve(mlogit.result$Hess)       # var-cov matrix

# Simulate parameters from predictive distributions
sims <- 10000
simbetas <- mvrnorm(sims,pe,vc)       # draw parameters, using MASS::mvrnorm
simb <- array(NA, dim = c(sims,3,2))  # re-arrange simulates to array format
simb[,,1] <- simbetas[,1:3]           #   for MNL simulation
simb[,,2] <- simbetas[,4:6]

# Create full factorial set of counterfactuals
sizerange <- seq(1,4,by=0.1)          # range of counterfactual sizes
femalerange <- c(0,1)                 # range of counterfactual sexes
xhyp <- cffactorial(size = sizerange, female = femalerange)
                                      
# Simulate expected probabilities
mlogit.qoi1 <- mlogitsimev(xhyp,simb,ci=0.67)
print(mlogit.qoi1)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{models}


