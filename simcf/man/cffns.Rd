\name{cffns}
\alias{cfMake}
\alias{cfChange}
\alias{cfName}
\title{Create or modify a set of counterfactuals}
\description{
  Functions for creating and modifying sets of counterfactuals, usable
  as input for post-estimation prediction
}
\usage{
cfMake(formula=NULL, data, nscen=1, names=NULL, f=mean(), ...)

cfChange(xscen, covname, x, xpre=NULL, scen=1)

cfName(xscen, name, scen=1)
}
\arguments{
  \item{formula}{forumla object, the model estimated.  Compatible with \code{logBound()} and \code{logitBound()}.}
  \item{data}{matrix or dataframe, the actual data used to estimate the
    original model}
  \item{nscen}{scalar, number of counterfactual scenarios to initialize}
  \item{names}{string or string vector, the name(s) of the scenario(s),
    e.g., to label them on plots or tables; saved as the row name of the
  appropriate scenario}
  \item{f}{function, applied to columns of covariate data to produce the baseline
    counterfactual; default is to take the mean of each covariate}
  \item{\dots}{additional inputs to \code{f}}
  \item{xscen}{list object, containing an pre-initialized set of counterfactuals,
    as output by \code{cfMake} or modified by \code{cfChange} or \code{cfName}}
  \item{x}{scalar, a hypothetical value of a covariate}
  \item{covname}{string or scalar, the name or number of a column
    (covariate) in a counterfactual dataset which the user wishes to
    modify to the values given in \code{x} or \code{xpre}}
  \item{xpre}{scalar, a hypothetical initial value of a covariate, for
    calculating first differences}
  \item{scen}{scalar or string, the row (number or name) of the scenario
    to be modified} 
}
\details{
Useful for creating counterfactuals to input into the post-estimation simulation
functions of the \code{simcf} package (e.g., for plotting with the
\code{tile} package) or into \code{whatif} in the \code{WhatIf} package. 
}
\value{
  A list object containing the following components:
  \item{x}{A dataframe of counterfactuals, with \code{nscen} rows and a
    column for each column in \code{data}}
  \item{xpre}{A dataframe of initial counterfactuals for constructing
    first differences, with \code{nscen} rows and a
    column for each column in \code{data}}
  \item{model}{The model formula provided by the user (optional)}
}
\author{Christopher Adolph <\email{cadolph@u.washington.edu}>}
\examples{

# Linear, Poisson, and Negative Binomial regression using UScrime data

# Uses simcf and ropeladders to show how the expected crime rate varies 
# in response to changes in 7 covariates under each of four estimation 
# methods.  

# See example for ropeladder in package tile for more plots

# Load data and libraries; set up specification
require(tile)
require(MASS)
data(UScrime)
model <- (y ~ log(M) + So + log(Ed) + log(Po1) + log(Po2)
              + log(LF) + log(M.F) + log(Pop) + log(NW) +log(U1)
              + log(U2) + log(GDP) + log(Ineq) + log(Prob) +
              log(Time))

# Estimate Linear regression model
lm1.res <- lm(model, data = UScrime)
lm1.pe <- lm1.res$coefficients        # point estimates
lm1.vc <- vcov(lm1.res)               # var-cov matrix

# Estimate Robust and resistant regression model
mm1.res <- rlm(model, data = UScrime, method="MM")
mm1.pe <- mm1.res$coefficients        # point estimates
mm1.vc <- vcov(mm1.res)               # var-cov matrix

# Estimate Poisson model
po1.res <- glm(model, family=poisson, data = UScrime)
po1.pe <- po1.res$coefficients         # point estimates
po1.vc <- vcov(po1.res)                # var-cov matrix

# Estimate Negative Binomial model
nb1.res <- glm.nb(model, data = UScrime)
nb1.pe <- nb1.res$coefficients         # point estimates
nb1.vc <- vcov(nb1.res)                # var-cov matrix

# Use the simcf package to simulate first differences in crime rate
# given changes in each of the seven covariates

# Initialize 7 different scenarios to the mean values of the covariates
xscen <- cfMake(model, data=UScrime, nscen=7)

# Configure scenario 1:  Raise Probability of Imprisonment by 1/2 standard deviation
xscen <- cfName(xscen, "Pr(Prison) +0.5 sd", scen=1)
xscen <- cfChange(xscen, "Prob", x = mean(UScrime$Prob) +
0.5*sd(UScrime$Prob), scen=1)

# Configure scenario 2:  Raise Police Spending by 1/2 standard deviation
xscen <- cfName(xscen, "Police Spending +0.5 sd", scen=2)
xscen <- cfChange(xscen, "Po1", x = mean(UScrime$Po1) + 0.5*sd(UScrime$Po1),
scen=2)

# Configure scenario 3:  Raise Unemployment (Age 35-39)  by 1/2 standard deviation
xscen <- cfName(xscen, "Unemployment (t-2) +0.5 sd", scen=3)
xscen <- cfChange(xscen, "U2", x = mean(UScrime$U2) + 0.5*sd(UScrime$U2),
scen=3)

# Configure scenario 4:  Raise Non-white population by 1/2 standard deviation
xscen <- cfName(xscen, "Non-White Pop +0.5 sd", scen=4)
xscen <- cfChange(xscen, "NW", x = mean(UScrime$NW) + 0.5*sd(UScrime$NW), scen=4)

# Configure scenario 5:  Raise Male Pop by 1/2 standard deviation
xscen <- cfName(xscen, "Male Pop +0.5 sd", scen=5)
xscen <- cfChange(xscen, "M", x = mean(UScrime$M) + 0.5*sd(UScrime$M), scen=5)

# Configure scenario 6:  Raise Education by 1/2 standard deviation
xscen <- cfName(xscen, "Education +0.5 sd", scen=6)
xscen <- cfChange(xscen, "Ed", x = mean(UScrime$Ed) + 0.5*sd(UScrime$Ed), scen=6)

# Configure scenario 7:  Raise Inequality by 1/2 standard deviation
xscen <- cfName(xscen, "Inequality +0.5 sd", scen=7)
xscen <- cfChange(xscen, "Ineq", x = mean(UScrime$Ineq) +
0.5*sd(UScrime$Ineq), scen=7)

# Simulate conditional expectations for these counterfactuals
sims <- 10000

# Linear regression simulations
simbetas.lm <- mvrnorm(sims, lm1.pe, lm1.vc)       # draw parameters, using MASS::mvrnorm
lm1.qoi <- linearsimfd(xscen, simbetas.lm, ci=0.95)

# Robust regression simulations
simbetas.mm <- mvrnorm(sims, mm1.pe, mm1.vc)       # draw parameters, using MASS::mvrnorm
mm1.qoi <- linearsimfd(xscen, simbetas.mm, ci=0.95)

# Poisson simulations
simbetas.po <- mvrnorm(sims, po1.pe, po1.vc)       # draw parameters, using MASS::mvrnorm
po1.qoi <- loglinsimfd(xscen, simbetas.po, ci=0.95)

# Negative Binomial simulations
simbetas.nb <- mvrnorm(sims, nb1.pe, nb1.vc)       # draw parameters, using MASS::mvrnorm
nb1.qoi <- loglinsimfd(xscen, simbetas.nb, ci=0.95)

# Create ropeladder traces of first differences from each model
trace1 <- ropeladder(x=lm1.qoi$pe,
                     lower=lm1.qoi$lower,
                     upper=lm1.qoi$upper,
                     labels=row.names(xscen$x),
                     #extrapolate=list(model, data=UScrime, cfact=xscen$x),
                     plot=1
                     )

trace2 <- ropeladder(x=mm1.qoi$pe,
                     lower=mm1.qoi$lower,
                     upper=mm1.qoi$upper,
                     plot=2
                     )

trace3 <- ropeladder(x=po1.qoi$pe,
                     lower=po1.qoi$lower,
                     upper=po1.qoi$upper,                   
                     plot=3
                     )

trace4 <- ropeladder(x=nb1.qoi$pe,
                     lower=nb1.qoi$lower,
                     upper=nb1.qoi$upper,
                     plot=4
                     )

rug1 <- rugTile(x = UScrime$y - mean(UScrime$y),
                plot = 1:4
                )
                
vertmark <- linesTile(x = c(0,0),
                      y = c(0,1),
                      lty = "solid",
                      plot = 1:4
                      )

# Create plot
tc <- tile(trace1, trace2, trace3, trace4,
           rug1, vertmark,
           #output = list(file = "ropeladderEx1"),          
           xaxistitle = list(labels="E(crime rate per 100,000)"),
           topaxis= list(at = mean(UScrime$y)*c(0.5, 1, 1.5, 2) - mean(UScrime$y),
                         labels = c("0.5x","1x","1.5x","2x"),
                         add = rep(TRUE,4)
                         ),
           topaxistitle = list(labels="E(crime rate) / average"),
           plottitle = list(labels1 = "Linear",
                            labels2 = "Robust",
                            labels3 = "Poisson",
                            labels4 = "Neg Bin"),
           gridlines=list(type="t")
           )

}
\keyword{design}
\keyword{manip}


