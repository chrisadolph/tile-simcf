\name{polylinesTile}
\alias{polylinesTile}

\title{ Add several unconnected line segments to a tile plot }
\description{
  Initializes a multiple line graphic for inclusion in a tile plot.  For more advanced features
  suitable for summarizing inference from a model, use instead
  \code{\link{lineplot}}.
}
\usage{
polylinesTile(...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{\dots}{Any
  number of arguments given below.   Must include exactly one horizontal
  dimension (\code{x} or \code{top}) and exactly one vertical dimension
  (\code{y} or \code{right}).  All inputs should be identified by
  appropriate tags; i.e., use \code{polylinesTile(x=myxvar, y=myyvar)},
  \emph{not} \code{polylinesTile(myxvar,myyvar)}}
}
\details{

This function does no plotting; instead, it creates a \code{polylinesTile}
  object, or \dfn{trace} of plotting data, to be drawn on one or more
  plots in a tiled arrangement of plots.  To complete the drawing
  include the object as an input to \code{\link{tile}}.  From
  \code{tile}, it is possible to set further options including plot
  and axis titles, axis ranges and labels, logged axes, and
  annotations to the plot.

  \code{polylinesTile} simply creates a series of disconnected lines
  or line segments to add to a \code{\link{tile}} plot.  It serves as
  the \pkg{tile} equivalent to the grid
  graphics \code{polylineGrob}, and is primarily useful for annotating
  plots which also use other traces.  If you need to draw a large
  number of disconnected line segments, creating a single trace made
  by \code{\link{polylinesTile}} will be much faster than creating a
  large number of \code{linesTile} traces.

  To plot lines to a location outside the plotting area, users might
  try combining \code{clip="off"}, with coordinates
  placing the lines in the desired spots.

}

\section{linesTile-specific parameters}{


 A call to \code{linesTile} \strong{must} provide an orthogonal pair of the
  following inputs:

  \describe{
  \item{\code{x}}{coordinate vector of data to plot, attached to the \var{x}
   axis.}
  \item{\code{y}}{coordinate vector of data to plot, attached to the \var{y}
  axis.}
  \item{\code{top}}{coordinate vector of data to plot, attached to the \var{top}
  axis.}
  \item{\code{right}}{coordinate vector of data to plot, attached to
    the \var{right} axis.}
  }

Additionally, users \strong{must} provide an assigment vector:

\describe{
\item{id}{vector of group numbers separating the data into different lines.}
}

Users will often wish to provide the following input:

\describe{
  \item{\code{plot}}{scalar or vector, the plot(s) in which this trace
    will be drawn; defaulting to the first plot.  Plots are numbered
    consecutively from the top left, row-by-row.  Thus in a 2 x 3
    tiling, the first plot in the second row is plot number 4.}  
}


In addition to these \code{polylinesTile}-specific parameters, users may provide any of the generic tile parameters documented in \code{\link{pointsTile}}.

}
  

\value{
  A \code{polylinesTile} object, used only as an input to \code{\link{tile}}.
}
%\references{ ~put references to the literature/web site here ~ }
\author{Christopher Adolph \email{cadolph@u.washington.edu}}

\seealso{\link{tile}, \code{\link{linesTile}}, \code{\link{lineplot}}}
%\examples{

%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{dplot}
\keyword{list}
