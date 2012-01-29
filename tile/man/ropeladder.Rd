\name{ropeladder}
\alias{ropeladder}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Summarize inferences using ropeladders }

\description{Initializes a dotplot aimed at summarizing
inferences from regression models.  This plot may: include confidence intervals,
perhaps created from simulations; suppress cases outside the convex hull
to avoid unwarranted extrapolation; and box the range of point estimates across different scenarios. 
}
\usage{
ropeladder(...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{\dots}{Any
  number of arguments given below.   Must include exactly one
  dimension (\code{x}, \code{top}, \code{y}, or \code{right}), which contains the primary data to plot (e.g., point estimates).  All inputs should be identified by appropriate tags; i.e., use \code{ropeladder(x=myxvar)},
  \emph{not} \code{ropeladder(myxvar)}
}}
\details{
This function does no plotting; instead, it creates a
  \code{ropeladder} object, or \dfn{trace} of plotting data, to be drawn
  on one or more plots in a tiled arrangement of plots.  To complete
  the drawing include the \code{lineplot} object as an input to
  \code{\link{tile}}, from which users can set further options
  including plot and axis titles, axis scaling and titles.

  \code{ropeladder} offers many data processing and formatting options
  for the trace to be plotted.  Confidence intervals (shown as
  horizontal or vertical lines) can be calculated from simulations or
  posterior draws, or may be provided by the user.  Optionally,
  results outside the convex hull of the original data can be hidden
  or flagged.  Finally, the graphical parameters for each element of
  the plot can be adjusted, often on a point-by-point basis.

  Run through \code{tile}, output from \code{ropeladder} will yield a
  finished plot.  The plot cannot be easily modified after creation.
  Rather, users should include in the initial call to \code{tile}
  additional traces for all desired annotations (text, symbols, lines,
  or polygons) to the finished plot.


}

\section{Ropeladder-specific parameters}{

  A call to \code{ropeladder} \strong{must} provide a single data input to one of the following plot dimensions:

 \describe{
  \item{\code{x}}{coordinate vector of data to plot, attached to the \var{x}
   axis.  \code{x} may be plotted directly, or treated as
  simulation data to summarize (see parameter \code{simulates} below).}
  \item{\code{y}}{coordinate vector of data to plot, attached to the \var{y}
  axis; may be simulation data.}
  \item{\code{top}}{coordinate vector of data to plot, attached to the \var{top}
  axis; may be simulation data.}
  \item{\code{right}}{coordinate vector of data to plot, attached to
    the \var{right} axis; may be simulation data.}
  }


Users will usually wish to provide some of the following inputs:
\describe{
 
  \item{\code{lower}}{vector of same length as \code{x}, \code{y}, \code{top}, or \code{right},
  user-provided lower bounds on the plotted points; only used when simulates is \code{NULL}}
  
  \item{\code{upper}}{vector of same length as \code{x}, \code{y}, \code{top}, or \code{right},
  user-provided upper bounds on theplotted points; only used when simulates is \code{NULL}}
  
  \item{\code{simulates}}{A string identifying one of the variables
    (\code{x}, \code{y}, \code{top}, or \code{right}) as simulation
    data (by default is \code{NULL}, for no simulation data).  If
    \code{simulates} is set to one of the plot dimensions, the
    orthogonal dimension will be treated as scenario code grouping the
    simulations.  For example, to plot summaries of 1,000 simulates
    drawn from the conditional distribution of the response variable
    \var{y} for each of 5 different values of a particular covariate,
    stack all 5,000 simulates in a single vector \code{y}, then create
    a corresponding 5,000-vector \code{x} listing the values of
    \var{x} used to create each simulate.  \code{ropeladder} will then
    calculate confidence intervals each scenario, as requested in
    \code{ci} below.}
 
  \item{\code{plot}}{scalar or vector, the plot(s) in which this trace
    will be drawn; defaulting to the first plot.  Plots are numbered
    consecutively from the top left, row-by-row.  Thus in a 2 x 3
    tiling, the first plot in the second row is plot number 4.}  

}

  The next set of inputs are all optional, and control the major
  features of \code{ropeladder}:

  \describe{
  \item{\code{ci}}{list, parameters governing
    the appearance and calculation of confidence intervals from data in
    \code{lower} and \code{upper} or provided by the simulations defined
    in \code{simulates}:
    \describe{
    \item{\code{levels}}{vector of desired confidence intervals to
  calculate from the variable named by \code{simulates}; ignored if user
  provides bounds in \code{lower} and \code{upper}.  Default is
  c(0.67,0.95), which gives approximately 1- and 2-standard error bounds.}
    \item{\code{mark}}{vector of desired plotting styles for confidence
  intervals (either \code{shaded} regions or \code{dashed} lines).
  May have as many elements as there are columns in \code{lower} and
  \code{upper}, or elements in \code{ci$levels}}
  }}
  
  \item{\code{extrapolate}}{list, parameters governing
    the plotting of extrapolation outside the convex hull of the
    covariate data, using \code{whatif} in the \pkg{WhatIf} package:
    \describe{
    \item{\code{formula}}{optional formula object, used to specify the
    estimated model. Useful if the model contains
    functions of the covariates given in \code{data} below}
    \item{\code{data}}{matrix or dataframe, the actual values of all covariates used
    to estimate the model (omit the constant and response variable)}
    \item{\code{cfact}}{matrix or dataframe, the counterfactual values of all the
    covariates (omit the constant and response variable), one row for
    each scenario.  The order of colums must match \code{data}, and the
    order of rows must match the order of the scenarios.  If scenarios
    are calculated from simulates, then the rows must be listed from the
    scenario with the smallest factor level to the highest}
    \item{\code{omit.extrapolated}}{If \code{TRUE} (the default), then the plotted
    trace and CIs are clipped to the convex hull; if \code{FALSE}, then
    extrapolation outside the convex hull is printed in a lighter color
    or with dashed or dotted lines.}

}}

\item{\code{connect}}{XXX.  Default is \code{FALSE}.}

\item{\code{type}}{Numeric, set to 1 for a standard dot plot, and set to 2 for a stickplot with confidence intervals.}

\item{\code{baseline}}{List, controls for the baseline in barplots drawn when \code{type} is 2:

   \describe{
     \item{\code{at}}{Location of baseline for type 2 ropeladders.}
     \item{\code{col}}{Color of baseline, default is \code{black}.}
     \item{\code{lwd}}{Width of baseline, default is \code{0.5}.}
     \item{\code{lty}}{Type of baseline, default is \code{solid}.}
   }
}

\item{\code{shadowbox}}{If \code{TRUE} or a color, draw a polygon showing
the range from the lowest to highest plotted dots; if \code{NULL} or
\code{NA}, omit this polygon (default is to omit).  See \code{shadowrow} below for an alternative if your plot has multiple ropeladders.}

\item{\code{arrowsout}}{Logical, draws arrows to indicate confidence intervals extending outside the plot range.  The default is \code{TRUE}.}

\item{\code{entryheight}}{Numeric, controls the space between each entry. The default is 1/6.}

\item{\code{subentryheight}}{Numeric, controls the space between each subentry when multiple traces are plotted to the same plotting area. The default is 0.6.}

\item{\code{mirrorlabels}}{Logical, print duplicate labels on opposite side of plot. The default is \code{FALSE}.}
       
\item{\code{group}}{Numeric.  Multiple ropeladder traces sent to the same plot are, by default, plotted with slightly different alignments.  If group numbers of provided, ropeladder traces on the same plot with the same group number are printed overlapping rather than spaced.}
   
\item{\code{shadowrow}}{When multiple ropeladder traces are plotted to the same plot, setting \code{shadowrow} to \code{TRUE} or a color or a vector of colors will draw a series of polygons showing
the range from the lowest to highest plotted dots for each row of the ropeladders.  If \code{NULL} or
\code{NA}, omit these polygons (default is to omit)  See \code{shadowbox} above for an alternative if you have just one ropeladder per plot.}

\item{\code{sublabels}}{Character string.  When multiple ropeladder traces are plotted to the same plot, this label is printed near the points for this trace.}

\item{\code{sublabelsxoffset}}{Numeric.  When sublabels are printed, \code{sublabelxoffset} shifts the label along the \var{x}-axis, e.g., to avoid overlap with the plotted points.  Measured in \code{npc} units.}

\item{\code{sublabelsyoffset}}{Numeric.  When sublabels are printed, \code{sublabelyoffset} shifts the label along the \var{y}-axis, e.g., to avoid overlap with the plotted points.  Measured in \code{npc} units.}

\item{\code{sublabelsx}}{Numeric vector.  Override default locations of sublabels on \var{x}-axis.  Should have the number of entries as you have points to plot.  Measured in \code{npc} units.}

\item{\code{sublabelsy}}{Numeric vector.  Override default locations of sublabels on \var{y}-axis.  Should have the number of entries as you have points to plot.  Measured in \code{npc} units.}

\item{\code{spaceAbove}}{Scalar.  Create a header space above the plotted points and labels; e.g., for adding extra plot notation.  Default is 0.  As the range on the labels axis is 1, the sum of spaceAbove and spaceBelow should be considerably less than 1.}

\item{\code{spaceBelow}}{Scalar.  Create a footer space below the plotted points and labels; e.g., for adding extra plot notation.  Default is 0.  As the range on the labels axis is 1, the sum of spaceAbove and spaceBelow should be considerably less than 1.}

}
 
In addition to these \code{ropeladder}-specific parameters, users may provide any of the generic tile parameters documented in \code{\link{pointsTile}}.

}
\value{
A ropeladder object, used only as an input to \code{\link{tile}}.
}
%\references{ ~put references to the literature/web site here ~ }
\author{Christopher Adolph \email{cadolph@u.washington.edu}}
%\note{ ~~further notes~~ }
\seealso{\code{\link{tile}}}
\examples{

# Linear, Poisson, and Negative Binomial regression using UScrime data

# Uses ropeladders to show how the expected crime rate varies in
# response to changes in 7 covariates under each of four estimation 
# methods.  

# Plot 1 shows a four plot set up, one plot per method.  
# This approach highlights differences in effects across covariates

# Plot 2 squeezes all four ropeladders into a single plot.
# This approach gives equal attention to differences 
# across covariates and models

# Plot 3 creates a plot for each covariate.
# This approach highlights differences across models.

# Load data and libraries; set up specification
require(simcf)
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

# Configure scenario 2:  Raise Non-white population by 1/2 standard deviation
xscen <- cfName(xscen, "Non-White Pop +0.5 sd", scen=2)
xscen <- cfChange(xscen, "NW", x = mean(UScrime$NW) + 0.5*sd(UScrime$NW), scen=2)

# Configure scenario 3:  Raise Unemployment (Age 35-39)  by 1/2 standard deviation
xscen <- cfName(xscen, "Unemployment (t-2) +0.5 sd", scen=3)
xscen <- cfChange(xscen, "U2", x = mean(UScrime$U2) + 0.5*sd(UScrime$U2),
scen=3)

# Configure scenario 4:  Raise Male Pop by 1/2 standard deviation
xscen <- cfName(xscen, "Male Pop +0.5 sd", scen=4)
xscen <- cfChange(xscen, "M", x = mean(UScrime$M) + 0.5*sd(UScrime$M), scen=4)

# Configure scenario 5:  Raise Education by 1/2 standard deviation
xscen <- cfName(xscen, "Education +0.5 sd", scen=5)
xscen <- cfChange(xscen, "Ed", x = mean(UScrime$Ed) + 0.5*sd(UScrime$Ed), scen=5)

# Configure scenario 6:  Raise Police Spending by 1/2 standard deviation
xscen <- cfName(xscen, "Police Spending +0.5 sd", scen=6)
xscen <- cfChange(xscen, "Po1", x = mean(UScrime$Po1) + 0.5*sd(UScrime$Po1),
scen=6)

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

# Create Plot 1 (focus on covariates) and save to pdf
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

# Plot all four models together:  equal focus on covariates and models

# Revise traces to place on same plot
trace1$plot <- trace2$plot <- trace3$plot <- trace4$plot <- 1
vertmark$plot <- 1

# Revise traces to make symbols different
trace1$pch <- 19
trace2$pch <- 15
trace3$pch <- 17
trace4$pch <- 23

# Add sublabels to each trace
trace1$sublabels <- "linear"
trace2$sublabels <- "robust"
trace3$sublabels <- "poisson"
trace4$sublabels <- "negbin"

# Widen space between entries to make labels visible
trace1$entryheight <- 0.25

# Shift sublabels to left side of plot to avoid overlap
trace1$sublabelsX <- 0.07
trace2$sublabelsX <- 0.07
trace3$sublabelsX <- 0.07
trace4$sublabelsX <- 0.07

# Add boxes around the results for each covariate when traces are plotted 
# to the same graph (could add to any of the traces)
trace1$shadowrow <- TRUE

# Create Plot 2 and save to pdf 
tc <- tile(trace1, trace2, trace3, trace4,
           vertmark,
           limits = c(-230, 460),
           width=list(null=4),
           #output = list(file="ropeladderEx2"),          
           xaxistitle = list(labels="E(crime rate per 100,000)"),
           topaxis= list(at = mean(UScrime$y)*c(0.75, 1, 1.25, 1.5) - mean(UScrime$y),
                         labels = c("0.75x","1x","1.25x","1.5x"),
                         add = rep(TRUE,4)
                         ),
           topaxistitle = list(labels="E(crime rate) / average"),
           gridlines=list(type="t")
           )

# Plot by coefficient to show variation across models

# Collect in matrix form all first differences and confidence 
# intervals across models (columns) and covariates (rows)
allPE <- cbind(lm1.qoi$pe, mm1.qoi$pe, po1.qoi$pe, nb1.qoi$pe)
allLOWER <- cbind(lm1.qoi$lower, mm1.qoi$lower, po1.qoi$lower, nb1.qoi$lower)
allUPPER <- cbind(lm1.qoi$upper, mm1.qoi$upper, po1.qoi$upper, nb1.qoi$upper)

# Add a new model:  Negative Binomial estimation with 

# Create a trace for each covariate of the different models' estimates
# (Save these traces in a vector of traces; note double bracket indexing)
collectedtraces <- vector("list", nrow(allPE))
for (i in 1: nrow(allPE)) {
  collectedtraces[[i]] <- ropeladder(x = allPE[i,],
                                   lower = allLOWER[i,],
                                   upper = allUPPER[i,],
                                   shadowbox = TRUE,
                                   plot = i
                                   )
  }

# Add ropeladder labels to first and fifth plots
# (The first ropeladder in each row of plots; note double bracket indexing)
collectedtraces[[1]]$labels <- collectedtraces[[5]]$labels <- 
   c("Linear", "Robust & Resistant", "Poisson", "Negative Binomial")

# Revise vertical mark to plot on all seven plots
vertmark$plot <- 1:7

# Create Plot 3:  Focus on variation across models
tc <- tile(collectedtraces, vertmark,
           RxC = c(2,4),
           limits = c(-230, 460),
           width = list(spacer=5),
           #output = list(file="ropeladderEx3"),       
           xaxis = list(at = c(-200, 0, 200, 400)),   
           xaxistitle = list(labels="E(crime rate per 100,000)"),
           topaxis= list(at = mean(UScrime$y)*c(0.75, 1, 1.25, 1.5) - mean(UScrime$y),
                         labels = c(".75x","1x","1.25x","1.5x"),
                         add = rep(TRUE,4)
                         ),
           topaxistitle = list(labels="E(crime rate) / average"),
           plottitle = list(labels1 = "Pr(Prison)",
                            labels2 = "Police Spending",
                            labels3 = "Unemployment",
                            labels4 = "Non-White Pop",
                            labels5 = "Male Pop",
                            labels6 = "Education",
                            labels7 = "Inequality"),
           gridlines=list(type="top")
           )
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{dplot}
\keyword{list}% __ONLY ONE__ keyword per line
