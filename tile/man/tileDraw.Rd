\name{tileDraw}
\alias{tileDraw}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Redraw a tile graphical object to the current device}
\description{
Draws a tile graphical object to the current device.  Because \code{tile}
automatically draws the object to a file, \code{tileDraw} is usually superfluous.  
}
\usage{
tileDraw(grob, defaultlayer=10)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{grob}{A tile or grid object; e.g., the value returned by \code{tile}}
  \item{defaultlayer}{The layer on which to draw any sub-elements of
    \code{grob} which lack a preassigned layer.  Layers with higher
    numbers are drawn first, then lower numbers are drawn on top.}
}
\details{
This internal function draws a tile graphical object to the current
device.  It is mainly of use of an internal function; most users should
simply use \code{\link{tile}} to create and save tile graphics to a
file.
  
This function is provided for completeness, and is not necessary to
create tile graphics with detailed annotations, which can be done
through careful construction of traces prior to calling \code{\link{tile}}.

However, if users wish to add to or modify an existing tile grob, they
may do so by directly editing the grob returned by \code{tile}, then sending it
to \code{tileDraw} to place it on the current device.  Care should be
taken to set up the current device to have appropriate height, width,
and pointsize.
}
\value{
\code{tileDraw} is mainly called for the side-effect of drawing a tile
grob.  It invisibly returns the grob after drawing.
}
\author{Christopher Adolph <\email{cadolph@uw.edu}>}
\seealso{\code{\link{tile}}}
\keyword{hplot}

