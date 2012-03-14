\name{pointsTile}
\alias{pointsTile}
\title{ Add plotted points to a tile plot }
\description{
  Initializes a trace which plots points to a tile graphic.  For more advanced features
  suitable for detailed scatterplots or summarizing inference from a model, use instead
  \code{\link{scatter}}.
}
\usage{
pointsTile(\dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{\dots}{Any
  number of arguments given below.   Must include exactly one horizontal
  dimension (\code{x} or \code{top}), and exactly one vertical dimension
  (\code{y} or \code{right}).  All inputs should be identified by
  appropriate tags; i.e., use \code{pointsTile(x=myxvar, y=myyvar)},
  \emph{not} \code{textTile(myxvar,myyvar)}}
}
\details{

This function does no plotting; instead, it creates a \code{pointsTile}
  object, or \dfn{trace} of plotting data, to be drawn on one or more
  plots in a tiled arrangement of plots.  To complete the drawing
  include the object as an input to \code{\link{tile}}.  From
  \code{tile}, it is possible to set further options including plot
  and axis titles, axis ranges and labels, logged axes, and
  annotations to the plot.

  \code{pointsTile} simply adds a symbol at a specific location or locations
  to a \code{\link{tile}} plot.  It serves as the \pkg{tile}
  equivalent to the base graphics \code{points} or grid graphics
  \code{pointsGrob}, and is primarily useful for annotating plots which
  also use other traces.  Note that \code{pointsTile} traces must be
  created in advance and included in the call to \code{tile}, rather
  than added afterward.

  To plot symbols to a location outside the plotting area, users might
  try combining \code{clip="off"}, with coordinates
  placing the symbols in the desired spot.

}

\section{pointsTile-specific parameters}{

 A call to \code{pointsTile} \strong{must} provide an orthogonal pair of the
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


In addition to these \code{pointsTile}-specific parameters, users may provide any of the generic tile parameters documented below.

}
  

\section{Generic tile graphical parameters}{The inputs in this section may be given to any trace creating function
in the \pkg{tile} package, though in some cases the parameter will
have no effect for obvious reasons.  These inputs are documented in
three groups: 1.  Parameters mimicking the \pkg{grid} parameters, 2.
Parameters for sophisticated color selection, and 3. Other generic
\pkg{tile} trace parameters.

\enumerate{
\item{The following graphics parameters mostly mimic the
\code{\link{gpar}} parameters of the \pkg{grid} package, and can be
given as inputs to any \pkg{tile} trace-making function.  Starred (*)
parameters accept vector as well as scalar input, so that different
parameter values can be applied to different elements of the trace:

  \describe{
  \item{\code{fill}}{*Color for filling polygons; default is \code{transparent}.}

  \item{\code{col}}{*Color for lines, symbols, and text; default is \code{black}.}

  \item{\code{lty}}{*Line type; default is \code{solid}.}

  \item{\code{lwd}}{*Line width; default is 1.}

  \item{\code{cex}}{*Multiplier applied to fontsize; default is 1.}

  \item{\code{lex}}{*Multiplier applied to linewidth; default is 1.}

  \item{\code{fontsize}}{Size of the text in points; default is 12.}

  \item{\code{lineheight}}{Height of a line as a multple of the size of
    text; default is 1.2.}
  
  \item{\code{fontfamily}}{The font family; default is "".}

  \item{\code{fontface}}{The font face (\code{bold}, \code{italic}, etc.);
      default is \code{plain}.}
  
  \item{\code{alpha}}{Alpha channel for transparency; default is 0.8.}

  \item{\code{polyalpha}}{Alpha channel for transparency of polygons only; default is 0.8.}

  \item{\code{lineend}}{Line end style (\code{round}, \code{butt},
  \code{square}); default is \code{round}.}

  \item{\code{linejoin}}{Line join style (\code{round}, \code{mitre}, \code{bevel}); default
  is \code{"round"}.}

  \item{\code{linemitre}}{Line mitre limit, must be greater than 1; default is 10.}
  }
}


  \item{The color of plotted elements can be most simply controlled by
  providing a scalar or vector of colors to \code{col} above.  More detailed control is available:

  \describe{
  \item{\code{color}}{List, advanced color controls (overrides \code{col}).  Use these options if you wish to create colors with equal perceptual spacing (ie, select colors based on their location in the \code{\link{polarLUV}} space).  Normally, users will want to provide \code{data} and \code{colorset}, and possibly \code{bins} or \code{breaks}.  The remaining inputs allow very fine control of colors, but the defaults should work well in almost all cases:
  \describe{
    \item{\code{data}}{A vector of numeric data to be encoded on the plot through varied colors.}
    \item{\code{bins}}{Scalar, either the number of bins to sort \code{data} into, or the name of an algorithm used by \code{\link{hist}} to bin data.  Default is \code{Sturges}.  Not needed if \code{breaks} are provided.}
    \item{\code{breaks}}{Vector, the values separating the different bins of \code{data}.  Overrides \code{bins}.}

    \item{\code{colorset}}{Scalar, which schema governs color
    selection: \code{sequential}, \code{diverge}, \code{heatmap},
    \code{terrain}, \code{categories}, or \code{brewer}.  Most choices
    are documented in the package \pkg{colorspaces} under
    \code{\link{sequential_hcl}}.  \code{sequential} (the default)
    produces a range of colors with the same hue but progressively
    lighter colors by perceptually equal steps; \code{diverge}
    produces a range of colors centered on a neutral desaturated
    color, and ranging outwards to two fully saturated hues on either
    side by perceptually equal steps; \code{heatmap} ranges from a
    dark, colorful red to a light, desaturated yellow by perceptually
    equal steps; \code{terrain} ranges from a dark, colorful green to
    a light, desaturated brown by perceptually equal steps;
    \code{rainbow} produces colors of equal chroma and equally spaced
    hue; \code{categories} produces colors directly from
    \code{\link{hcl}}, and \code{brewer} returns selected colors for
    categorical displays as recommended by the package
    \pkg{RColorBrewer}.}

    \item{\code{hue}}{A scalar or vector of hue values between 0 and 360.  For
    \code{colorset="sequential"}, this is a scalar reflecting
    the common hue of all plotted colors.  For
    \code{colorset="diverge"},
    \code{colorset="heatmap"} or
    \code{colorset="terrain"}, \code{hue} is a two-vector of
    the two hues used for extreme values of \code{data}.  For
    \code{colorset="rainbow"}, \code{hue} is a two-vector
    giving range in which equally spaced hues are selected.  For
    \code{colorset="categories"}, \code{hue} is a vector of
    arbitrary length giving the hues desired for different categories
    of \code{data}.  \code{hue} has no effect if
    \code{colorset="brewer"}. Default varies by \code{colorset}.}

    \item{\code{chroma}}{A scalar or vector of chroma values between 0 and 100.  For
    \code{colorset="sequential"}, \code{colorset="heatmap"} or
    \code{colorset="terrain"}, this is a two vector reflecting
    the range of chroma used from low to high values of plotted data.  For
    \code{colorset="diverge"},
    \code{chroma} is a scalar of
    the chroma used for extreme values of \code{data}.  For
    \code{colorset="rainbow"}, \code{chroma} is a scalar
    giving the common chroma for all plotted colors.  For
    \code{colorset="categories"}, \code{chroma} is a scalar or vector of
    arbitrary length giving the chroma desired for different categories
    of \code{data}.  \code{chroma} has no effect if
    \code{colorset="brewer"}. Default varies by \code{colorset}.}

        \item{\code{luminance}}{A scalar or vector of luminance values between 0 and 100.  For
    \code{colorset="sequential"}, \code{colorset="diverge"}, \code{colorset="heatmap"} or
    \code{colorset="terrain"}, this is a two vector reflecting
    the range of luminance used from low to high values of plotted data. For
    \code{colorset="rainbow"}, \code{chroma} is a scalar
    giving the common luminance for all plotted colors.  For
    \code{colorset="categories"}, \code{luminance} is a scalar or vector of
    arbitrary length giving the luminance desired for different categories
    of \code{data}.  \code{luminance} has no effect if
    \code{colorset="brewer"}. Default varies by \code{colorset}.}

\item{\code{power}}{Control parameter determining how chroma and luminance should be increased (1 for linear, 2 for quadratic, etc.).  Default varies by \code{colorset}.}

\item{\code{gamma}}{Scalar, gamma value of display.  Default is 2.4.}

\item{\code{fixup}}{Logical, corrected colors to a valid RGB value.  Default is \code{TRUE}.}
  }}}}

  \item{These final graphical parameters are specific to \pkg{tile} plots,
  and tweak the appearance of traces.  Starred (*) parameters accept
  vector as well as scalar input, to apply different parameters to
  different elements of the trace:

  \describe{
  \item{\code{markers}}{Logical, plot symbols at data points; default is
  \code{FALSE} for no plotting}

  \item{\code{labels}}{*Text to plot at datapoints; default is to plot no
  text.}

  \item{\code{labelxoffset}}{*Horizontal offset for labels, to avoid
  overplotting with symbols; default is 0.}

  \item{\code{labelyoffset}}{*Vertical offset for labels, to avoid
  overplotting with symbols; default is 0.}

  \item{\code{size}}{*Size of symbols to plot at datapoints if \code{markers} is
  \code{TRUE}; default is 1.}

  \item{\code{pch}}{*Symbol(s) to plot at datapoints if \code{markers}
  is \code{TRUE}; default is 1 (run \code{example(points)} to see symbols).}

  \item{\code{addArrow}}{Adds an arrowhead to the end of the plotted
  line; deault is \code{FALSE}.}

  \item{\code{angleArrow}}{The angle of the arrow head in degrees
  (smaller numbers produce narrower, pointier arrows); default is 30.}

  \item{\code{lengthArrow}}{A unit specifying the length of the arrow
  head (from tip to base); default is \code{unit(0.25, "inches")}.}

  \item{\code{endsArrow}}{One of \code{last}, \code{first}, or
  \code{both}, indicating which ends of the line to draw arrow heads;
  default is \code{last}.}

  \item{\code{typeArrow}}{One of \code{open} or \code{closed} indicating whether the arrow head should be a closed triangle.}

  \item{\code{just}}{XXX}

  \item{\code{hjust}}{XXX}

  \item{\code{vjust}}{XXX}

  \item{\code{rot}}{XXX}

  \item{\code{check.overlap}}{XXX}

  \item{\code{clip}}{Clip graphic results to the plotting area(s)
  (\code{on} or \code{off}); default is \code{on}.}

  \item{\code{lighten}}{XXX}

  \item{\code{layer}}{Graphical elements with lower \code{layer} will
    be drawn later in the plotting process, and hence appear on top of
    elements with higher \code{layer}.  By default, \code{layer} is 10, though polygons in
    this trace will be plotted at \code{layer + X}, and text labels and
    markers at \code{layer - X}.}
}
}
}
}


\value{
  A \code{pointsTile} object, used only as an input to \code{\link{tile}}.
}
%\references{ ~put references to the literature/web site here ~ }
\author{Christopher Adolph \email{cadolph@u.washington.edu}}

\seealso{\link{tile}, \link{scatter}}
%\examples{

%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{dplot}
\keyword{list}
