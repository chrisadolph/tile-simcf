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


# Multinomial Logistic Regression of alligator food;
# Tiled lineplots using factorial design of counterfactuals,
# with extrapolation outside the convex hull flagged
#
# For an alternative method, using cfMake and cfChange instead of cfFactorial,
# see help(lineplot) in the tile package

data(gator)
require(MASS)
require(nnet)
require(tile)
require(WhatIf)

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
xhyp <- cfFactorial(size = sizerange, female = femalerange)
                                      
# Simulate expected probabilities
mlogit.qoi1 <- mlogitsimev(xhyp,simb,ci=0.67)

# Create one trace for each predicted category of the response, and each sex
trace1 <- lineplot(x=xhyp$x$size[xhyp$x$female==0],
                   y=mlogit.qoi1$pe[xhyp$x$female==0,1],
                   lower=mlogit.qoi1$lower[xhyp$x$female==0,1,],
                   upper=mlogit.qoi1$upper[xhyp$x$female==0,1,],
                   ci=list(mark="shaded"),
                   extrapolate=list(data=cbind(size,female),
                                    cfact=xhyp$x[xhyp$x$female==0,],
                                    omit.extrapolated=FALSE),
                   col="blue",
                   plot=1
                   )

trace2 <- lineplot(x=xhyp$x$size[xhyp$x$female==0],
                   y=mlogit.qoi1$pe[xhyp$x$female==0,2],
                   lower=mlogit.qoi1$lower[xhyp$x$female==0,2,],
                   upper=mlogit.qoi1$upper[xhyp$x$female==0,2,],
                   ci=list(mark="shaded"),
                   extrapolate=list(data=cbind(size,female),
                                    cfact=xhyp$x[xhyp$x$female==0,],
                                    omit.extrapolated=FALSE),
                   col="red",
                   plot=1
                   )

trace3 <- lineplot(x=xhyp$x$size[xhyp$x$female==0],
                   y=mlogit.qoi1$pe[xhyp$x$female==0,3],
                   lower=mlogit.qoi1$lower[xhyp$x$female==0,3,],
                   upper=mlogit.qoi1$upper[xhyp$x$female==0,3,],
                   ci=list(mark="shaded"),
                   extrapolate=list(data=cbind(size,female),
                                    cfact=xhyp$x[xhyp$x$female==0,],
                                    omit.extrapolated=FALSE),
                   col="green",
                   plot=1
                   )

trace4 <- lineplot(x=xhyp$x$size[xhyp$x$female==1],
                   y=mlogit.qoi1$pe[xhyp$x$female==1,1],
                   lower=mlogit.qoi1$lower[xhyp$x$female==1,1,],
                   upper=mlogit.qoi1$upper[xhyp$x$female==1,1,],
                   ci=list(mark="shaded"),
                   extrapolate=list(data=cbind(size,female),
                                    cfact=xhyp$x[xhyp$x$female==1,],
                                    omit.extrapolated=FALSE),
                   col="blue",
                   plot=2
                   )

trace5 <- lineplot(x=xhyp$x$size[xhyp$x$female==1],
                   y=mlogit.qoi1$pe[xhyp$x$female==1,2],
                   lower=mlogit.qoi1$lower[xhyp$x$female==1,2,],
                   upper=mlogit.qoi1$upper[xhyp$x$female==1,2,],
                   ci=list(mark="shaded"),
                   extrapolate=list(data=cbind(size,female),
                                    cfact=xhyp$x[xhyp$x$female==1,],
                                    omit.extrapolated=FALSE),
                   col="red",
                   plot=2
                   )

trace6 <- lineplot(x=xhyp$x$size[xhyp$x$female==1],
                   y=mlogit.qoi1$pe[xhyp$x$female==1,3],
                   lower=mlogit.qoi1$lower[xhyp$x$female==1,3,],
                   upper=mlogit.qoi1$upper[xhyp$x$female==1,3,],
                   ci=list(mark="shaded"),
                   extrapolate=list(data=cbind(size,female),
                                    cfact=xhyp$x[xhyp$x$female==1,],
                                    omit.extrapolated=FALSE),
                   col="green",
                   plot=2
                   )

linelabels <- textTile(labels=c("Invertebrates",
                                 "Fish",
                                 "Other"),
                         x=  c(1.75,      3,         3),
                         y=  c(0.95,     0.95,      0.375),
                         col=c("blue", "green", "red"),
                         cex = 0.75,
                         plot=c(1,2)
                         )

at.x <- c(1,2,3,4)
at.y <- c(0,0.2,0.4,0.6,0.8,1)

# Plot traces using tile
tile(trace1,
     trace2,
     trace3,
     trace4,
     trace5,
     trace6,
     linelabels,
     RxC = c(1,2),
     limits = c(1,4,0,1),
     #output = list(outfile="gator2cf", width=5),
     xaxis = list(at=at.x),
     yaxis = list(at=at.y),
     xaxistitle = list(labels="Size of alligator"),
     yaxistitle = list(type="first",labels="Pr(Food preference)"),
     undertitle = list(labels=c("Male","Female")),
     maintitle = list(labels="Food choice by alligator size and sex", x=0.57),
     width = list(yaxistitle=3),
     height = list(maintitle=5, undertitle=3),
     gridlines = list(type="xy"),
     frame=TRUE
     )


}
\keyword{design}
\keyword{manip}


