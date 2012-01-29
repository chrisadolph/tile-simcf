\name{nightplot}
\alias{nightplot}
\title{Summarize bounded data using nightplots}

\description{Initializes a scatterplot of the relationship between one
 known variable and another variable known within an upper and lower
 bound.  All observations are represented by equal area lozenges which
 are stretched between the bounds on the uncertain variable.}

\usage{ nightplot(\dots)  }

\arguments{ 

\item{\dots}{Any number of arguments given below.  At a
minimum, inputs must include data for one known dimension (\code{x},
\code{top}, \code{top}, or \code{right}), and the bounds on an
orthogonal dimension.  All inputs should be identified by appropriate
tags; i.e., use \code{nightplot(x=myxvar, ylower=myylowervar, yupper=myyuppervar)}, \emph{not}
\code{nightplot(myxvar, myylowervar, myyuppervar)}} 

}

\details{ This function does no plotting; instead, it creates a
  \code{nightplot} object, or \dfn{trace} of plotting data, to be drawn
  on one or more plots in a tiled arrangement of plots.  To complete
  the drawing include the \code{nightplot} object as an input to
  \code{\link{tile}}, from which users can set further options
  including plot and axis titles, axis scaling and titles.

  Nightplots are an experimental new plot aimed at addressing a
  problem in usual displays of bounded information, which arises most
  often when an estimated quantity is plotted against a known
  covariate.  Two approaches---\dQuote{lines} and
  \dQuote{bubbles}---are most common.  The \dQuote{lines} approach
  draws a line between the lower and upper bound (see the first panel
  in the example below).  The \dQuote{bubbles} approach scales the
  plotted symbol---usually a circle---to be larger for more precisely
  known cases (see the second panel in the example below).  Neither
  approach is entirely satisfactory.  Lines between bounds give
  greatest visual emphasis to the least understood cases, and often
  hide the precisely known cases, even though these are exactly the
  most important observations for inference.  Scaled bubbles restore
  the proper emphasis, but completely lose the clear presentation of
  the upper and lower bounds given by the lines approach.

  Nightplots combine the virtues of each technique to draw a lozenge,
  or tablet shape, between the upper and lower bounds.  All lozenges
  are scaled to have the same area, so cases with wide bounds will be
  stretch into thin, less noticeable bands, while precisely known
  cases will become large tablets or even circles centered on the
  known value.  Additionally, more precisely known cases are plotted
  in bright yellow shades, while cases with wide bounds are shown as
  desaturated grays.  Finally, the best known cases are plotted
  on top of the least known cases.

  Run through \code{tile}, output from \code{nightplot} will yield a
  finished plot.  The plot cannot be easily modified after creation.
  Rather, users should include in the initial call to \code{tile}
  additional traces for all desired annotations (text, symbols, lines,
  or polygons) to the finished plot.  
}

\section{nightplot-specific parameters}{

  A call to \code{nightplot} \strong{must} provide data on at least one and possibly two orthogonal dimensions of the plot:

  \describe{

  \item{\code{x}}{coordinate vector of data to plot, attached to the
   \var{x} axis.  If \code{x} is provided without \code{xlower} and
   \code{xupper}, it will be treated as the known dimension.  If
   \code{xlower} and \code{xupper} are also provided, then \code{x} is
   optional, and if given will be treated as a bounded estimate on the
   \var{x} dimension.}

  \item{\code{y}}{coordinate vector of data to plot, attached to the
  \var{y} axis.  If \code{y} is provided without \code{ylower} and
  \code{yupper}, it will be treated as the known dimension.  If
  \code{ylower} and \code{yupper} are also provided, then \code{y} is
  optional, and if given will be treated as a bounded estimate on the
  \var{y} dimension.}

  \item{\code{top}}{coordinate vector of data to plot, attached to the
  \var{top} axis.  If \code{top} is provided without \code{toplower}
  and \code{topupper}, it will be treated as the known dimension.  If
  \code{toplower} and \code{topupper} are also provided, then
  \code{top} is optional, and if given will be treated as a bounded
  estimate on the \var{top} dimension.}

  \item{\code{right}}{coordinate vector of data to plot, attached to
    the \var{right} axis.  If \code{right} is provided without
    \code{rightlower} and \code{rightupper}, it will be treated as the
    known dimension.  If \code{rightlower} and \code{rightupper} are
    also provided, then \code{right} is optional, and if given will be
    treated as a bounded estimate on the \var{right} dimension.}

  }

  Additionally, a call to \code{nightplot} \strong{should} provide a
  set of bounds on each of the points in exactly one of the above
  dimensions:

\describe{
 
  \item{\code{xlower}}{vector of same length as \code{x} containing
  user-provided lower bounds.}
  
  \item{\code{xupper}}{vector of same length as \code{x} containing
  user-provided upper bounds.}

\item{\code{ylower}}{vector of same length as \code{y} containing
user-provided lower bounds.}
  
  \item{\code{yupper}}{vector of same length as \code{y} containing
  user-provided upper bounds.}

\item{\code{toplower}}{vector of same length as \code{top} containing
user-provided lower bounds.}
  
  \item{\code{topupper}}{vector of same length as \code{top}
  containing user-provided upper bounds.}

\item{\code{rightlower}}{vector of same length as \code{right}
containing user-provided lower bounds.}
  
  \item{\code{rightupper}}{vector of same length as \code{right}
  containing user-provided upper bounds.}

}

 The following inputs are all optional, and other
  features of \code{nightplot}. 

\describe{   

\item{\code{truex}}{vector of the same length of \code{x} containing
the true values of each case on the \var{x} dimension.  Only provide
this input if \var{x} is the unknown dimension (ie, you have also
provided \code{xlower} and \code{xupper}).}

\item{\code{truey}}{vector of the same length of \code{y} containing
the true values of each case on the \var{y} dimension.  Only provide
this input if \var{y} is the unknown dimension (ie, you have also
provided \code{ylower} and \code{yupper}).}


\item{\code{truetop}}{vector of the same length of \code{top} containing
the true values of each case on the \var{top} dimension.  Only provide
this input if \var{top} is the unknown dimension (ie, you have also
provided \code{toplower} and \code{topupper}).}


\item{\code{trueright}}{vector of the same length of \code{right} containing
the true values of each case on the \var{right} dimension.  Only provide
this input if \var{right} is the unknown dimension (ie, you have also
provided \code{rightlower} and \code{rightupper}).}


  \item{\code{bw}}{Logical, indicates whether to draw a gray-scale
  version of the nightplot, with a white background and black or gray
  lozenges (\code{TRUE}, or to draw the color version, with a black
  background and yellow or gray lozenges (\code{FALSE}).  Default is
  \code{FALSE}.}

  \item{\code{plot}}{scalar or vector, the plot(s) in which this trace
    will be drawn; defaulting to the first plot.  Plots are numbered
    consecutively from the top left, row-by-row.  Thus in a 2 x 3
    tiling, the first plot in the second row is plot number 4.}  


}

In addition to these \code{nightplot}-specific parameters, users may provide any of the generic tile parameters documented in \code{\link{pointsTile}}.

 }


\value{
  A \code{nightplot} object, used only as an input to \code{\link{tile}}.
}
%\references{ ~put references to the literature/web site here ~ }
\author{Christopher Adolph \email{cadolph@u.washington.edu}}
%\note{ ~~further notes~~ 
\seealso{\code{\link{tile}}, \code{\link{scatter}}}

\examples{
# Generate some data
n <- 100
x <- runif(n)
disturbance <- rnorm(n,sd=0.05)
y <- 0.25 + 0.5*x + disturbance
firsthalf <- 1:80
secondhalf <- 81:100

# Some bad "estimates" for the first half of the data
ylower <- yupper <- yhat <- rep(NA,n)
badbounds <- sort(runif(2*n))
ylower[firsthalf] <- badbounds[firsthalf]
yupper[firsthalf] <- rev(badbounds)[firsthalf]
yhat[firsthalf] <- apply(cbind(yupper[firsthalf], ylower[firsthalf]), 1, mean)

# Some "good" estimates for the second half of the data
res <- lm(y[secondhalf] ~ x[secondhalf])
fitted <- predict(res,interval="confidence")
yhat[secondhalf] <- fitted[,1]
ylower[secondhalf] <- fitted[,2]
yupper[secondhalf] <- fitted[,3]

# Lines approach to plotting bounded data
trace1 <- scatter(x=x,
                  y=yhat,
                  ylower=ylower,
                  yupper=yupper,
                  size = 0.5,
                  plot = 1
                  )

# Bubbles approach to plotting bounded data
inversebndwidth <- 1/(yupper - ylower)
trace2 <- pointsTile(x=x,
                     y=yhat,
                     pch=1,
                     size=0.25*inversebndwidth,
                     markers=TRUE,
                     plot=2
                     )

# Nightplot approach to plotting bounded data 
trace3 <- nightplot(x=x,
                    y=yhat,
                    ylower=ylower,
                    yupper=yupper,
                    plot = 3
                    )

# Nightplot approach (grayscale)
trace4 <- nightplot(x=x,
                    y=yhat,
                    ylower=ylower,
                    yupper=yupper,
                    truey=y,
                    bw=TRUE,
                    plot = 4
                    )

# Tile all four approaches
tile(trace1, trace2, trace3, trace4,
     RxC=c(2,2),
     limits=c(0,1,0,1),
     frame=TRUE,
     xaxistitle=list(labels="A precise quantity"),
     yaxistitle=list(type="row",labels=c("A bounded quantity",
                                         "A bounded quantity")),
     maintitle=list(labels="Plots for Bounded Variables"),
     plottitle=list(labels=c("Lines", "Bubbles", "Nightplot", "Grayscale Nightplot"))
     )        
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{dplot}
\keyword{list}
