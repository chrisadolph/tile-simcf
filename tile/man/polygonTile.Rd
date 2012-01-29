\name{polygonTile}
\alias{polygonTile}


\title{ Add a polygon to a tile plot }
\description{
  Initializes a polygon graphic for inclusion in a tile plot. 
}
\usage{
polygonTile(\dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{\dots}{Any
  number of arguments given below.   Must include exactly one horizontal
  dimension (\code{x} or \code{top}) and exactly one vertical dimension
  (\code{y} or \code{right}).  All inputs should be identified by
  appropriate tags; i.e., use \code{polygonTile(x=myxvar, y=myyvar)},
  \emph{not} \code{polygonTile(myxvar,myyvar)}}
}
\details{

This function does no plotting; instead, it creates a \code{polygonTile}
  object, or \dfn{trace} of plotting data, to be drawn on one or more
  plots in a tiled arrangement of plots.  To complete the drawing
  include the object as an input to \code{\link{tile}}.  From
  \code{tile}, it is possible to set further options including plot
  and axis titles, axis ranges and labels, logged axes, and
  annotations to the plot.

  \code{polygonTile} simply creates a shaded region or polygon to add
  to a \code{\link{tile}} plot.  It serves as the \pkg{tile}
  equivalent to the base graphics \code{polygon} or grid graphics
  \code{polygonGrob}, and is primarily useful for annotating plots
  which also use other traces. 

  To plot polygons to a location outside the plotting area, users might
  try combining \code{clip="off"}, with coordinates
  placing the polygon in the desired spot.

}

\section{polygonTile-specific parameters}{


 A call to \code{polygonTile} \strong{must} provide an orthogonal pair of the
  following inputs:

  \describe{
  \item{\code{x}}{coordinate vector of vertices to plot, attached to the \var{x}
   axis.}
  \item{\code{y}}{coordinate vector of vertices to plot, attached to the \var{y}
  axis.}
  \item{\code{top}}{coordinate vector of vertices to plot, attached to the \var{top}
  axis.}
  \item{\code{right}}{coordinate vector of vertices to plot, attached to
    the \var{right} axis.}
  }

Users will often wish to provide the following input:

\describe{
  \item{\code{plot}}{scalar or vector, the plot(s) in which this trace
    will be drawn; defaulting to the first plot.  Plots are numbered
    consecutively from the top left, row-by-row.  Thus in a 2 x 3
    tiling, the first plot in the second row is plot number 4.}  
}


In addition to these \code{polygonTile}-specific parameters, users may provide any of the generic tile parameters documented in \code{\link{pointsTile}}.

}
  

\value{
  A \code{polygonTile} object, used only as an input to \code{\link{tile}}.
}
%\references{ ~put references to the literature/web site here ~ }
\author{Christopher Adolph \email{cadolph@u.washington.edu}}

\seealso{\link{tile}}
%\examples{

%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{dplot}
\keyword{list}

