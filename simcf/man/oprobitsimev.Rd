\name{oprobitsimev}
\alias{oprobitsimev}
\alias{oprobitsimfd}
\alias{oprobitsimrr}
\title{Simulate quantities of interest and confidence intervals for
  ordered probit}
\description{
  Simulate and summarize uncertainty of conditional expected values, first
  differences and relative risks from
  estimated ordered probit models
}
\usage{
oprobitsimev(x, b, ci=0.95, constant=1, cat=3)

oprobitsimfd(x, b, ci=0.95, constant=1, cat=3, xpre=NULL)

oprobitsimrr(x, b, ci=0.95, constant=1, cat=3, xpre=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{list, a counterfactual object created by \code{cfMake}, or a vector or matrix of counterfactual values of the covariates, including multiple rows to simulate different counterfactual scenarios, and one column for each covariate}
  \item{b}{matrix, simulated parameters, one row per draw from the
    estimated model, and one column per parameter, including any constants.  The last \code{cat} - 2 columns should be the cutpoints.}
  \item{ci}{vector, the requested intervals of the simulated quantity of
    interest to be reported}
  \item{constant}{scalar, the column of \code{b} containing the model
    constant, or \code{NA} for no constant.  If the estimation was done in polr() 
    from the MASS library, you must set constant to NA.}
  \item{cat}{scalar, the number of categories in the response variable; must be at least 3 and an integer}
  \item{xpre}{vector or matrix, counterfactual initial values of the
    covariates.  Rows must match \code{x}.  Not needed when \code{x} is a \code{counterfactual} object.}
}
\details{
  Given simulated parameters from an estimated ordered probit model, and counterfactual
  values of the covariates, these functions calculate either the
  conditional expected value of the response (\code{oprobitsimev}), the conditional first
  difference (\code{oprobitsimfd}), or the relative risk (\code{oprobitsimrr}), and confidence intervals around these point estimates.  

Use \code{cfMake} to initialize a \code{counterfactual} object containing \code{x} and \code{xpre}, or input them directly.

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
  \item{pe}{matrix, the point estimates of the requested quantity of
  interest; rows are scenarios, and columns are categories}
  \item{lower}{array, the requested lower bounds around the quantity of
    interest; rows are scenarios, columns are the category of the response, and the third dimension is the confidence interval}
  \item{upper}{array, the requested upper bounds around the quantity of
    interest; rows are scenarios, columns are the category of the response, and the third dimension is the confidence interval}
}
%\references{ ~put references to the literature/web site here ~ }
\author{Christopher Adolph <\email{cadolph@u.washington.edu}>}
\seealso{\code{\link{logitsimev}}, \code{\link{mlogitsimev}}, \code{\link{cfMake}}, \code{\link{cfChange}}, \code{\link{cfName}}}
\examples{
require(MASS)

# Using housing data from MASS; convert to optim usable form
data(housing)
housingExpanded <- housing[rep(1:nrow(housing), housing$Freq),]
rownames(housingExpanded) <- 1:nrow(housingExpanded)
housingExpanded$Sat <- as.numeric(housingExpanded$Sat)
housingExpanded$Infl <- as.numeric(housingExpanded$Infl)
housingExpanded$Cont <- as.numeric(housingExpanded$Cont)
y <- housingExpanded$Sat
x <- cbind(housingExpanded$Infl, housingExpanded$Cont)
model <- Sat ~ Infl + Cont

# Estimate a p-category ordered probit using optim and llk.oprobit (from simcf library)
# This version of ordered probit has a constant and p-2 estimated cutpoints
ncut <- length(unique(housingExpanded$Sat)) - 2
ls.result <- lm(model, data=housingExpanded) # LS based starting values
stval <- c(ls.result$coefficients, 1:ncut)          # and 1, 2, ... for cutpoints
house.optim <- optim(stval, llk.oprobit, method="BFGS", y=y, x=x, hessian=TRUE)

# Construct counterfactual scenarios:  All combinations of Infl and Cont
xhyp <- cfFactorial(Infl = unique(housingExpanded$Infl), 
                    Cont = unique(housingExpanded$Cont))

# Simulate E(Sat) for each counterfactual
sims <- 10000
simbetas.optim <- mvrnorm(sims, house.optim$par, solve(house.optim$hessian) )
yhyp.optim <- oprobitsimev(xhyp, simbetas.optim, constant=1, cat=3)
print(yhyp.optim)

# Estimate a p-category ordered probit using polr (from MASS library)
# This version of ordered probit has no constant and p-1 estimated cutpoints
house.plr <- polr(as.factor(y) ~ x, method="probit")
house.plr$par <- c(house.plr$coefficients, house.plr$zeta)
simbetas.plr <- mvrnorm(sims, house.plr$par, vcov(house.plr) )

# Simulate E(Sat) for each counterfactual, polr version
#   Must set constant = NA to predict from polr
yhyp.plr <- oprobitsimev(xhyp, simbetas.plr, constant=NA, cat=3)
print(yhyp.plr)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{models}


