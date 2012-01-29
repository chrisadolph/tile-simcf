\name{textTile}
\alias{textTile}
\title{ Add plotted text labels to a tile plot }
\description{
  Initializes a trace which plots text to a tile graphic.
}
\usage{
textTile(...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{\dots}{Any
  number of arguments given below.   Must include the text \code{labels}, exactly one horizontal
  dimension (\code{x} or \code{top}), and exactly one vertical dimension
  (\code{y} or \code{right}).  All inputs should be identified by
  appropriate tags; i.e., use \code{textTile(x=myxvar, y=myyvar)},
  \emph{not} \code{textTile(myxvar,myyvar)}}
}
\details{

This function does no plotting; instead, it creates a \code{textTile}
  object, or \dfn{trace} of plotting data, to be drawn on one or more
  plots in a tiled arrangement of plots.  To complete the drawing
  include the object as an input to \code{\link{tile}}.  From
  \code{tile}, it is possible to set further options including plot
  and axis titles, axis ranges and labels, logged axes, and
  annotations to the plot.

  \code{textTile} simply adds text at a specific location or locations
  to a \code{\link{tile}} plot.  It serves as the \pkg{tile}
  equivalent to the base graphics \code{text} or grid graphics
  \code{textGrob}, and is primarily useful for annotating plots which
  also use other traces.  Note that \code{textTile} traces must be
  created in advance and included in the call to \code{tile}, rather
  than added afterward.

  To plot text to a location outside the plotting area, users might
  try combining \code{clip="off"}, with coordinates
  placing the text in the desired spot.

}

\section{textTile-specific parameters}{

The primary input to \code{textTile} is the text itself:

\describe{
\item{\code{labels}}{The text to plot; may be scalar or vector.}
}

 A call to \code{textTile} \strong{must} provide an orthogonal pair of the
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

Users will often wish to provide the following input:

\describe{
  \item{\code{plot}}{scalar or vector, the plot(s) in which this trace
    will be drawn; defaulting to the first plot.  Plots are numbered
    consecutively from the top left, row-by-row.  Thus in a 2 x 3
    tiling, the first plot in the second row is plot number 4.}  
}


In addition to these \code{textTile}-specific parameters, users may provide any of the generic tile parameters documented in \code{\link{pointsTile}}.

}
  

\value{
  A \code{textTile} object, used only as an input to \code{\link{tile}}.
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
