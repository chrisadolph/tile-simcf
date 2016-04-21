#' Add a connected line segment to a tile plot
#' 
#' Initializes a simple line graphic for inclusion in a tile plot.  For more
#' advanced features suitable for summarizing inference from a model, use
#' instead \code{\link{lineplot}}.
#' 
#' 
#' This function does no plotting; instead, it creates a \code{linesTile}
#' object, or \dfn{trace} of plotting data, to be drawn on one or more plots in
#' a tiled arrangement of plots.  To complete the drawing include the object as
#' an input to \code{\link{tile}}.  From \code{tile}, it is possible to set
#' further options including plot and axis titles, axis ranges and labels,
#' logged axes, and annotations to the plot.
#' 
#' \code{linesTile} simply creates a line or series of connected line segments
#' to add to a \code{\link{tile}} plot.  It serves as the \pkg{tile} equivalent
#' to the base graphics \code{lines} or grid graphics \code{linesGrob}, and is
#' primarily useful for annotating plots which also use other traces.  If you
#' need to draw a large number of disconnected line segments, creating a single
#' trace made by \code{\link{polylinesTile}} will be much faster than creating
#' a large number of \code{linesTile} traces.
#' 
#' To plot lines to a location outside the plotting area, users might try
#' combining \code{clip="off"}, with coordinates placing the lines in the
#' desired spot.
#' 
#' @param \dots Any number of arguments given below.  Must include exactly one
#' horizontal dimension (\code{x} or \code{top}) and exactly one vertical
#' dimension (\code{y} or \code{right}).  All inputs should be identified by
#' appropriate tags; i.e., use \code{linesTile(x=myxvar, y=myyvar)}, \emph{not}
#' \code{linesTile(myxvar,myyvar)}
#' @return A \code{linesTile} object, used only as an input to
#' \code{\link{tile}}.
#' @section linesTile-specific parameters:
#' 
#' A call to \code{linesTile} \strong{must} provide an orthogonal pair of the
#' following inputs:
#' 
#' \describe{ \item{list("x")}{coordinate vector of data to plot, attached to
#' the \var{x} axis.} \item{list("y")}{coordinate vector of data to plot,
#' attached to the \var{y} axis.} \item{list("top")}{coordinate vector of data
#' to plot, attached to the \var{top} axis.} \item{list("right")}{coordinate
#' vector of data to plot, attached to the \var{right} axis.} }
#' 
#' Users will often wish to provide the following input:
#' 
#' \describe{ \item{list("plot")}{scalar or vector, the plot(s) in which this
#' trace will be drawn; defaulting to the first plot.  Plots are numbered
#' consecutively from the top left, row-by-row.  Thus in a 2 x 3 tiling, the
#' first plot in the second row is plot number 4.} }
#' 
#' In addition to these \code{linesTile}-specific parameters, users may provide
#' any of the generic tile parameters documented in \code{\link{pointsTile}}.
#' @author Christopher Adolph \email{cadolph@@u.washington.edu}
#' @seealso \link{tile}, \code{\link{polylinesTile}}, \code{\link{lineplot}}
#' @keywords dplot list
#' @export linesTile
"linesTile" <-
function(...){  
  args <- list(...,graphic="lines")
  class(args) <- c(class(args),"tileTrace","lines")
  args
}

