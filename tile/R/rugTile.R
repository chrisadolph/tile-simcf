#' Add a rug to a tile plot
#' 
#' Initializes plots of marginal distributions along axes for tile plots.
#' 
#' 
#' This function does no plotting; instead, it creates a \code{rugTile} object,
#' or \dfn{trace} of plotting data, to be drawn on one or more plots in a tiled
#' arrangement of plots.  To complete the drawing include the object as an
#' input to \code{\link{tile}}.  From \code{tile}, it is possible to set
#' further options including plot and axis titles, axis ranges and labels,
#' logged axes, and annotations to the plot.
#' 
#' \code{rugTile} creates a rug, or a plot of a marginal distribution along the
#' axis of a two-dimensional plot, to add to a \code{\link{tile}} plot.  It
#' serves as the \pkg{tile} equivalent to the base graphics \code{rug}, and is
#' primarily useful for annotating plots which also use other traces.  Note
#' that you may need multiple rug traces to complete your plot: if, for
#' example, the \var{x} axes in different plots have different marginal
#' distributions, each will need a separate trace containing the rug data for
#' that plot and axis.
#' 
#' Also note that multiple rugs can be plotted to the same axis and plot, if
#' different marginal distributions are to be compared.
#' 
#' @param \dots Any number of arguments given below.  Must include exactly one
#' dimension (\code{x}, \code{top}, \code{y} or \code{right}).  All inputs
#' should be identified by appropriate tags; i.e., use
#' \code{rugTile(x=myxvar)}, \emph{not} \code{rugTile(myxvar)}
#' @return A \code{rugTile} object, used only as an input to
#' \code{\link{tile}}.
#' @section rugTile-specific parameters:
#' 
#' A call to \code{rugTile} \strong{must} provide (only) one of the following
#' inputs:
#' 
#' \describe{ \item{list("x")}{vector of data to plot to a rug along the
#' \var{x} axis.} \item{list("y")}{vector of data to plot to a rug along the
#' \var{y} axis.} \item{list("top")}{vector of data to plot to a rug along the
#' \var{top} axis.} \item{list("right")}{vector of data to plot to a rug along
#' the \var{right} axis.} }
#' 
#' Users will often wish to provide the following inputs: \describe{
#' \item{type}{String indicating the type of rug to draw: \code{lines} for thin
#' lines marking each datapoint; \code{dots} for a histogram made of dots for
#' each datapoint; \code{jitter} for a strip filled with jittered dots for each
#' datapoint; \code{density} for a smoothed histogram (experimental).  Default
#' is \code{lines}.} \item{kernel}{The smoothing kernel for \code{density}
#' rugs; default is \code{gaussian}.  See \code{\link{density}} for other
#' options.} \item{thickness}{Scalar, the height of rugs attached to horizontal
#' axes, or the width of rugs attached to vertical axes, in multiples of
#' current character size (\code{char}).  Default is 1.}
#' \item{list("plot")}{scalar or vector, the plot(s) in which this rug will be
#' drawn; defaulting to the first plot.  Plots are numbered consecutively from
#' the top left, row-by-row.  Thus in a 2 x 3 tiling, the first plot in the
#' second row is plot number 4.} }
#' 
#' In addition to these \code{rugTile}-specific parameters, users may provide
#' any of the generic tile parameters documented in \code{\link{pointsTile}}.
#' @author Christopher Adolph \email{cadolph@@u.washington.edu}
#' @seealso \link{tile}
#' @keywords dplot list
#' @examples
#' 
#' 
#' x <- runif(100)
#' y <- x + rnorm(100)
#' 
#' trace0 <- scatter(top=x,
#'                   y=y,
#'                   col="red",
#'                   pch=1,
#'                   fit=list(method="linear"),
#'                   cex=0.25,
#'                   lwd=0.5,
#'                   plot = 1)
#' 
#' trace0a <- scatter(x=x,
#'                   right=y,
#'                   col="blue",
#'                   pch=5,
#'                   fit=list(method="mmest"),
#'                   cex=0.25,
#'                   lwd=0.5,
#'                   plot = 2)
#' 
#' 
#' rug1top <- rugTile(top=x, type="jitter",col="black", plot=1)
#' 
#' rug1y <- rugTile(y=y, type="lines", col="gray40", plot=1)
#' 
#' rug2x <- rugTile(x=x, type="dots",col="black", plot=2)
#' 
#' rug2right <- rugTile(right=y, type="density", col="gray40", plot=2)
#' 
#' tile(trace0,trace0a,rug1top,rug1y,rug2x,rug2right,
#'      frame=TRUE,gridlines=list(type=c("xytr")),
#'      xaxistitle=list(labels2="X axis label"),
#'      topaxistitle=list(labels1="Top axis label"),
#'      rightaxistitle=list(labels2="Right axis label"),
#'      yaxistitle=list(labels1="Y axis label"),
#'      plottitle=list(labels1="Plot title 1", labels2="Plot title 2")
#'      )
#'      
#' 
#' @export rugTile
"rugTile" <-
function(...){  
  args <- list(...,graphic="rug")
  class(args) <- c(class(args),"tileTrace","rug")
  args
}

