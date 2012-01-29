\name{influencePlot}
\alias{influencePlot}
\title{Create an interactive plot of studentized residuals against hat-values}
\description{Assess outliers by plotting the discrepancy of each observation against its leverage, with optional interactive labeling of points.}
\usage{
influencePlot(object,names=NULL,identify=TRUE,pdf=NULL,main=NULL,box=FALSE,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{A regression object, e.g., as produced by \code{\link{lm}}.}
  \item{names}{An optional vector of observation names for interactively labeling points.}
  \item{identify}{Logical, allow interactive labeling of points.  If \code{identify} is TRUE, the \code{pdf} should be NULL.}
  \item{pdf}{Character, the name of a file to save the graphic as a pdf instead of plotting to a screen device.  Default is no pdf.}
  \item{main}{Character, option title for the plot.}
  \item{box}{Logical, draw a box around the plotting area.  Default is FALSE.}
  \item{...}{Other options passed to \code{\link{pdf}}.}
}
\details{The x-axis of the plot shows "standardized" hat-values, which are equivalent to the diagonal elements of the hat matrix divided by the total number of parameters in the model.  Higher hat-values imply greater leverage.  

The y-axis plots studentized residuals.  Values outside the [-2,2] range should occur by chance for only 1 in 20 observations. 
}
\value{
  Creates a plot; no object returned.
}
%\references{ ~put references to the literature/web site here ~ }
\author{Christopher Adolph <\email{cadolph@u.washington.edu}>}
%\seealso{\code{\link{}}, \code{\link{}}}
%\examples{

%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{hplot,robust}


