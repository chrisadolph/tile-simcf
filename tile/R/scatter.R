#' Summarize inferences using scatterplots
#' 
#' Initializes a scatterplot aimed at summarizing inferences from regression
#' models.  This plot may: include confidence intervals, perhaps created from
#' simulations; be clipped to the convex hull to avoid unwarranted
#' extrapolation; and include simple linear or robust fits to the data.  If you
#' simply want to draw points on a \pkg{tile} plot, use
#' \code{\link{pointsTile}} instead.
#' 
#' This function does no plotting; instead, it creates a \code{scatter} object,
#' or \dfn{trace} of plotting data, to be drawn on one or more plots in a tiled
#' arrangement of plots.  To complete the drawing include the \code{scatter}
#' object as an input to \code{\link{tile}}, from which users can set further
#' options including plot and axis titles, axis scaling and titles.
#' 
#' \code{scatter} offers many data processing and formatting options for the
#' trace to be plotted.  Confidence intervals (shown as horizontal or vertical
#' lines, or both) can be calculated from simulations or posterior draws, or
#' may be provided by the user.  Alternatively, \code{scatter} can add simple
#' fit lines and confidence intervals to the plotted data (e.g., a linear,
#' robust, or loess fit).  Optionally, results outside the convex hull of the
#' original data can be hidden or flagged.  Finally, the graphical parameters
#' for each element of the scatter (including symbols, confidence intervals, or
#' text) can be adjusted, often on a point-by-point basis.
#' 
#' Run through \code{tile}, output from \code{scatter} will yield a finished
#' plot.  The plot cannot be easily modified after creation.  Rather, users
#' should include in the initial call to \code{tile} additional traces for all
#' desired annotations (text, symbols, lines, or polygons) to the finished
#' plot.
#' 
#' @param \dots Any number of arguments given below.  Must include exactly one
#' horizontal dimension (\var{x} or \var{top}) and exactly one vertical
#' dimension (\var{y} or \var{right}).  All inputs should be identified by
#' appropriate tags; i.e., use \code{scatter(x=myxvar, y=myyvar)}, \emph{not}
#' \code{scatter(myxvar,myyvar)}
#' @return A \code{scatter} object, used only as an input to
#' \code{\link{tile}}.
#' @section scatter-specific parameters:
#' 
#' A call to \code{scatter} \strong{must} provide an orthogonal pair of the
#' following inputs:
#' 
#' \describe{ \item{list("x")}{coordinate vector of data to plot, attached to
#' the \var{x} axis.  \code{x} may be plotted directly, or treated as
#' simulation data to summarize (see parameter \code{simulates} below).}
#' \item{list("y")}{coordinate vector of data to plot, attached to the \var{y}
#' axis; may be simulation data.} \item{list("top")}{coordinate vector of data
#' to plot, attached to the \var{top} axis; may be simulation data.}
#' \item{list("right")}{coordinate vector of data to plot, attached to the
#' \var{right} axis; may be simulation data.} }
#' 
#' The following inputs are all optional, and control the major features of
#' \code{scatter}.  It is usually best to use either \code{ci} or \code{fit},
#' but not both.
#' 
#' \describe{
#' 
#' \item{list("xlower")}{vector of same length as \code{x} containing
#' user-provided lower bounds; only used when \code{simulates} is \code{NULL}}
#' 
#' \item{list("xupper")}{vector of same length as \code{x} containing
#' user-provided upper bounds; only used when \code{simulates} is \code{NULL}}
#' 
#' \item{list("ylower")}{vector of same length as \code{y} containing
#' user-provided lower bounds; only used when \code{simulates} is \code{NULL}}
#' 
#' \item{list("yupper")}{vector of same length as \code{y} containing
#' user-provided upper bounds; only used when \code{simulates} is \code{NULL}}
#' 
#' \item{list("toplower")}{vector of same length as \code{top} containing
#' user-provided lower bounds; only used when \code{simulates} is \code{NULL}}
#' 
#' \item{list("topupper")}{vector of same length as \code{top} containing
#' user-provided upper bounds; only used when \code{simulates} is \code{NULL}}
#' 
#' \item{list("rightlower")}{vector of same length as \code{right} containing
#' user-provided lower bounds; only used when \code{simulates} is \code{NULL}}
#' 
#' \item{list("rightupper")}{vector of same length as \code{right} containing
#' user-provided upper bounds; only used when \code{simulates} is \code{NULL}}
#' 
#' \item{list("simulates")}{A string identifying one of the variables
#' (\code{x}, \code{y}, \code{top}, or \code{right}) as simulation data (by
#' default is \code{NULL}, for no simulation data).  If \code{simulates} is set
#' to one of the plot dimensions, the orthogonal dimension will be treated as
#' scenario code grouping the simulations.  For example, to plot summaries of
#' 1,000 simulates drawn from the conditional distribution of the response
#' variable \var{y} for each of 5 different values of a particular covariate,
#' stack all 5,000 simulates in a single vector \code{y}, then create a
#' corresponding 5,000-vector \code{x} listing the values of \var{x} used to
#' create each simulate.  \code{scatter} will then calculate confidence
#' intervals each scenario, as requested in \code{ci} below.}
#' 
#' \item{list("plot")}{scalar or vector, the plot(s) in which this trace will
#' be drawn; defaulting to the first plot.  Plots are numbered consecutively
#' from the top left, row-by-row.  Thus in a 2 x 3 tiling, the first plot in
#' the second row is plot number 4.}
#' 
#' \item{list("ci")}{list, parameters governing the appearance and calculation
#' of confidence intervals from data in \code{lower} and \code{upper} or
#' provided by the simulations defined in \code{simulates}: \describe{
#' \item{list("levels")}{scalar or vector of desired confidence intervals to
#' calculate from the variable named by \code{simulates}; ignored if user
#' provides bounds in \code{lower} and \code{upper}.  Default is 0.95, which
#' gives approximately 2-standard error bounds.} \item{list("mark")}{vector of
#' desired plotting styles for confidence intervals.  The default and only
#' current option is \code{lines}.} }}
#' 
#' \item{list("fit")}{list, parameters governing the appearance and calculation
#' of simple fits to the two plotted dimensions: \describe{
#' \item{list("method")}{The type of fit to apply: \code{linear} (default) fits
#' a bivariate linear regression; \code{wls} fits a weighted linear regression;
#' \code{robust} fits a robust regression using an M-estimator; \code{mmest}
#' fits a robust regression using an MM-estimator; \code{loess} fits a loess
#' smoother fits a loess smoother.} \item{list("ci")}{vector of requested
#' levels of confidence intervals for best fit line; default is 0.95.  Set to
#' \code{NA} for no confidence intervals.} \item{list("mark")}{vector of
#' desired plotting styles for confidence intervals (either \code{shaded}
#' regions or \code{dashed} lines) for best fit line; default is
#' \code{shaded}.} \item{list("col")}{color of best fit line; default is
#' \code{black}.} \item{list("span")}{bandwith parameter for loess; default is
#' 0.95.} \item{list("weights")}{vector of weights for \code{wls} fits.} } }
#' 
#' \item{list("extrapolate")}{list, parameters governing the plotting of
#' extrapolation outside the convex hull of the covariate data, using
#' \code{whatif} in the \pkg{WhatIf} package: \describe{
#' \item{list("formula")}{optional formula object, used to specify the
#' estimated model. Useful if the model contains functions of the covariates
#' given in \code{data} below} \item{list("data")}{matrix or dataframe, the
#' actual values of all covariates used to estimate the model (omit the
#' constant and response variable)} \item{list("cfact")}{matrix or dataframe,
#' the counterfactual values of all the covariates (omit the constant and
#' response variable), one row for each scenario.  The order of colums must
#' match \code{data}, and the order of rows must match the order of the
#' scenarios.  If scenarios are calculated from simulates, then the rows must
#' be listed from the scenario with the smallest factor level to the highest}
#' \item{list("omit.extrapolated")}{If \code{TRUE} (the default), then the
#' plotted trace and CIs are clipped to the convex hull; if \code{FALSE}, then
#' extrapolation outside the convex hull is printed in a lighter color or with
#' dashed or dotted lines.} }}
#' 
#' \item{labelsxoffset}{Scalar, horizontal offset for text labels.  Default is
#' 0.} \item{labelsyoffset}{Scalar, vertical offset for text labels.  Default
#' is 0.}
#' 
#' }
#' 
#' In addition to these \code{scatter}-specific parameters, users may provide
#' any of the generic tile parameters documented in \code{\link{pointsTile}}.
#' 
#' @author Christopher Adolph \email{cadolph@@u.washington.edu}
#' @seealso \code{\link{tile}}, \code{\link{pointsTile}}
#' @keywords dplot list
#' @examples
#' 
#' 
#' # Example:  Duncan's Occupational Prestige Data
#' 
#' # Load data
#' require(car)
#' data(Duncan)
#' attach(Duncan)
#' 
#' # Convert job classes to numerical codes
#' jobclass <- (type=="prof")*1 + (type=="wc")*2 + (type=="bc")*3
#' 
#' # Make some nice colors for job classes (not run)
#' # require(RColorBrewer)
#' # col <- brewer.pal(3, "Dark2")
#' 
#' # The colors brewer.pal would produce in this case:
#' col <- c("#1B9E77", "#D95F02", "#7570B3")
#' 
#' # Pick some symbols for job classes 
#' # (run example(points) for meaning of symbol codes)
#' pch <- c(17, 15, 16)
#' 
#' # Create labels, symbols, and colors for points
#' labels <- as.vector(row.names(Duncan))
#' markers <- pch[jobclass]
#' colors <- col[jobclass]
#' 
#' # Create scatterplot trace
#' prestigeXeducation <- scatter(x = education,
#'                               y = prestige,
#'                               labels = labels,
#'                               pch = markers,
#'                               col = colors,
#'                               size = 1,
#'                               cex = 0.75,
#'                               labelsyoffset = -0.035,
#'                               plot = 1,
#'                               fit = list(method="mmest")
#'                               )
#' 
#' # Create legend traces
#' legendtitle <- textTile(labels="1950 US Occupations (Duncan, 1961)",
#'                         x=20, y=98,
#'                         col="black",
#'                         fontface="bold",
#'                         cex = 0.75,
#'                         plot = 1
#'                         )
#' 
#' legendsymbols <- pointsTile(x=  c(2,      2,       2),
#'                             y=  c(88,     82,      77),
#'                             pch = pch,
#'                             col = col,
#'                             size = 1,
#'                             cex = 0.75,
#'                             plot=1
#'                             )
#' 
#' legendlabels <- textTile(labels=c("Professional",
#'                                   "White collar",
#'                                   "Blue collar"),
#'                          x=  c(11,      11,       11),
#'                          y=  c(88,     82,      77),
#'                          pch= pch,
#'                          col= col,
#'                          cex = 0.75,
#'                          plot=1
#'                          )
#' 
#' # Create rug traces
#' xrug <- rugTile(x=education, type="dots", plot=1)
#' 
#' yrug <- rugTile(y=prestige, type="dots", plot=1)
#' 
#' # Plot all traces using tile
#' tile(prestigeXeducation,
#'      legendtitle,
#'      legendsymbols,
#'      legendlabels,
#'      xrug, yrug,
#' 
#'      width = list(null=5),      # widen plot area for visibility
#'      #output = list(file="ScatterplotExample"),
#'      limits = c(0,100,0,100),
#'      xaxistitle = list(labels=
#'                       "Income (% of males making > $3500 in $1950)"),
#'      yaxistitle = list(labels=
#'                       "Prestige (% rated good or excellent by survey)"),
#'      height=list(plot="golden")
#'      )
#' 
#' 
#' @export scatter
"scatter" <-
function(...){  
  args <- list(...,graphic="scatter")
  class(args) <- c(class(args),"tileTrace","scatter")
  args
}

