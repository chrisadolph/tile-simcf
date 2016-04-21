#' Plot a tiled graphical layout
#' 
#' A generic layout function for plotting tiled arrangements of graphic styles
#' found in the tile package, including scatterplots, lineplots, dotplots and
#' more specialized formats.  Includes fine control of titles, axes, axis
#' transformation, and rugs for all plots.
#' 
#' 
#' With the \pkg{tile} package, you can not only create elegant graphical
#' displays of data, but also of the quantitative implications of fitted
#' regression models.  In particular, the package makes it easy to repeat the
#' same graphic in a tiled arrangement (\dfn{small multiples}), and always
#' allows for the inclusion of confidence intervals around any plotted
#' quantity.
#' 
#' A typical script producing a \pkg{tile} graphic follows three steps:
#' 
#' \enumerate{
#' 
#' \item \strong{Obtain data to plot.} These data may be the underlying
#' variables of interest, or quantities inferred from models.  (For this step,
#' you should use your own modeling code, or a package for extracting
#' counterfactual inferences from a model, such as \pkg{Zelig} or \pkg{simcf}.)
#' For example, you may have a set of expected values of a response variable
#' calculated conditional on different hypothetical values of covariates, and
#' may wish to plot those expected values along with their confidence
#' intervals.  Or you might have the actual draws from posterior distributions,
#' and decide to leave the summary of those draws by confidence intervals up to
#' graphing function.
#' 
#' \item \strong{Create traces from the data.} A \dfn{trace} is a single set of
#' data to be added to the plot, and may be as simple as a text label or a line
#' segment, or as complex as a set of points combined their best fit line and
#' shaded confidence region.  All the parameters affecting the construction and
#' appearance of the trace are set using one of the many functions in the
#' \pkg{tile} package.  These include several functions for creating primitives
#' (analogous to \pkg{grid} functions):
#' 
#' \tabular{ll}{ \code{\link{linesTile}} \tab Plot a set of connected line
#' segments \cr \code{\link{pointsTile}} \tab Plot a set of points \cr
#' \code{\link{polygonTile}} \tab Plot a shaded region \cr
#' \code{\link{polylinesTile}} \tab Plot a set of unconnected line segments \cr
#' \code{\link{textTile}} \tab Plot text labels \cr }
#' 
#' And several functions for constructing complex traces for model or data
#' exploration:
#' 
#' \tabular{ll}{ \code{\link{lineplot}} \tab Plot lines with confidence
#' intervals, extrapolation warnings \cr \code{\link{nightplot}} \tab
#' (Experimental) Plot bounded quantites using equal-area lozenges \cr
#' \code{\link{riceplot}} \tab (Experimental) Plot up to five dimensions to a
#' 2D scatter \cr \code{\link{ropeladder}} \tab Plot dotplots with confidence
#' intervals, extrapolation warnings, \cr \tab and shaded ranges \cr
#' \code{\link{scatter}} \tab Plot scatterplots with text and symbol markers,
#' \cr \tab fit lines, and confidence intervals \cr }
#' 
#' This is the point to tweak the look of the plotted data and
#' annotations---essentially, anything inside the plotted areas of the tiled
#' plots.  (Rugs, which technically lie along the outer borders of plots, also
#' get set up at this stage.  To add a rug, create a trace using
#' \code{\link{rugTile}}.)
#' 
#' \item \strong{Create the tiled graphic.} Finally, give all the traces as
#' inputs to a single call to \code{tile}.  At this point, you can customize
#' the look of the plotting framework: inputs to \code{tile} let you change the
#' rows and columns of tiled plots; include, scale, and label ticks for any
#' axes on any plots; add and format titles of axes, plots, rows of plots, or
#' columns of plots; and add and customize gridlines and frames around plots.
#' This is also the time to select the graphical device on which to save the
#' graph.
#' 
#' }
#' 
#' \pkg{tile} follows a draw-at-once philosophy.  No drawing takes place until
#' the user provides all the traces to \code{tile}.  Considering the entire
#' graphic at once, \code{tile} determines how to allocate space in the graphic
#' to each of the tiled plots, axes, and titles.  Only then are the elements in
#' the graphic drawn, in the order specified by the \code{layer} of each
#' graphical object.  As a result, it is not easy to modify or annotate an
#' existing graphic from the R command line.  Instead, if you desire to modify
#' a plot, you should modify the traces used to create the graphic, and re-run
#' \code{tile} to create the graphic anew.
#' 
#' %Explain axes, including transformation, attachment, and automatic limits
#' 
#' %Explain rugs, and warn some styles may not make sense with logged axes
#' 
#' %Explain titles
#' 
#' %Explain widths and heights
#' 
#' %Explain annotation across plots
#' 
#' %Note possibility of extension, and contact
#' 
#' @param \dots Any number of \dQuote{traces}, or lists of traces, as supplied
#' by \pkg{tile} graphic functions such as \code{\link{lineplot}},
#' \code{\link{scatter}}, \code{\link{ropeladder}}, \code{\link{nightplot}},
#' \code{\link{riceplot}}, \code{\link{linesTile}}, \code{\link{pointsTile}},
#' \code{\link{textTile}}, \code{\link{polylinesTile}}, or
#' \code{\link{polygonTile}}
#' @param limits A vector or matrix giving the limits of the plotting region,
#' similar to \code{usr} for base graphics, but generalized to the four axis
#' set-up of \code{tile}.  The usual case is an 8-vector, \code{c(xmin, xmax,
#' ymin, ymax, topmin, topmax, rightmin, rightmax)}.  If no plots have right
#' axes, this could be a 6-vector; if none have right or top axes, it could be
#' a 4-vector, and so on.  If an element of \code{limits} is \code{NA}, then
#' that limit is automatically computed based on the data plotted.  If no
#' limits are provided, all limits are computed.  To set different limits for
#' each plot, provide a matrix with one row of limits for each plot; otherwise,
#' the same limits are plotted (and, if necessary, computed) for all plots.
#' @param frame Logical; draw a frame around plots (use a vector to set for
#' specific plots).  Default is \code{FALSE}.
#' @param gridlines A list controlling the printing of gridlines in the
#' plotting regions.  Each input may be a scalar (to set the parameter globally
#' for all plots) or a vector (to set the parameter plot by plot).
#' Alternatively, for multiplot layouts you may specify, e.g., separate types
#' for plot 1 by providing \code{type1}, separate colors for plot 3 using
#' \code{col3}, etc.  Plot specific values given in this way override the
#' generic inputs.
#' 
#' \describe{ \item{list("type")}{A string containing the first letters of each
#' axis which should have gridlines attached to its ticks; e.g., \dQuote{xy}
#' for gridlines on the \var{x} and \var{y} axes, \dQuote{t} for gridlines on
#' the \var{top} axis only, and \dQuote{xytr} for all four axes.  Default is no
#' gridlines.} \item{list("lwd")}{Width of gridlines.  Default is 0.15.}
#' \item{list("col")}{Color of gridlines.  Default is \code{gray50}.}
#' \item{list("lty")}{Line type of gridlines.  Default is \code{solid}.} }
#' @param xaxis A list; controls the plotting and format of the \var{x}-axes.
#' The inputs below are generic, and apply to all plots.  Alternatively, for
#' multiplot layouts you may specify, e.g., separate labels for plot 1 by
#' providing \code{labels1}, separate colors for plot 3 using \code{col3}, etc.
#' Plot specific values given in this way override the generic inputs.  This is
#' especially helpful for changing just one plot out of a large layout, or for
#' specifying different numbers of labels or tick locations for different
#' plots.
#' 
#' \describe{ \item{list("at")}{Vector or matrix of locations to draw ticks.
#' If a matrix, \code{at} should have one row for each plot; \code{NA}s are
#' ignored.  Default is to have \code{tile} select \code{at} automatically.}
#' \item{list("labels")}{Vector or matrix of labels to apply at tick locations.
#' If a matrix, \code{labels} should contain one row for each plot.  Default is
#' to use \code{at}.} \item{list("tick.length")}{Length of ticks in
#' \code{lines}.  For ticks facing into the plot, use positive values.  For
#' ticks facing out of the plot, use negative values.  Default is -0.5.}
#' \item{list("label.loc")}{Location of tick labels relative to axis baseline.
#' Default is 1.25.} \item{list("ticks")}{Show tick marks.  Default is
#' \code{TRUE}.} \item{list("ntics")}{If ticks are automatic, how many to draw?
#' Default is 5.} \item{list("major")}{Logical, indicating whether to plot the
#' main axis line.  Default is \code{TRUE}.} \item{list("col")}{Color for all
#' axis elements.  Default is \code{black}.} \item{list("lwd")}{Line width of
#' axis elements.  Default is 0.5.} \item{list("cex")}{Degree of text and
#' symbol magnification.  Default is 1.} \item{list("rot")}{Rotation of tick
#' labels in degrees.  Default is 0, for no rotation.  90 and 270 should also
#' work well.  Non-right angles may work, but might lead to unexpected errors
#' in alignment of plot elements.} \item{list("fontsize")}{Fontsize of tick
#' labels.  Default is XXX.} \item{list("add")}{Logical indicating whether this
#' axis be drawn.  May be a vector with values for each plot.  Default is for
#' tile to include an axis in a plot only if data are plotted to that axis.}
#' \item{list("log")}{Logical or numeric which indicates whether this axis
#' should be logged.  May be \code{FALSE}, for no logging, \code{TRUE}, for
#' natural log scaling, or a positive number, which indicates the logathrmic
#' base for a scale other than the natural log.  Default is \code{FALSE}.} }
#' @param yaxis A list; controls the plotting and format of the \var{y}-axes;
#' see \code{xaxis} for parameters.
#' @param topaxis A list; controls the plotting and format of the
#' \var{top}-axes; see \code{xaxis} for parameters.
#' @param rightaxis A list; controls the plotting and format of the
#' \var{right}-axes; see \code{xaxis} for parameters.
#' @param xaxistitle A list; places titles for the \var{x}-axes and controls
#' their format.  These inputs should be scalar if the same treatment is
#' desired for the titles of different plots, or vector if different results
#' are desired for each title (\code{type}, of course, must be scalar, as it
#' applies to the whole layout).  Alternatively, for multiplot layouts you may
#' specify, e.g., separate labels for plot 1 by providing \code{labels1},
#' separate colors for plot 3 using \code{col3}, etc.  Plot specific values
#' given in this way override the generic inputs.  This is especially helpful
#' for changing just one plot out of a large layout.
#' 
#' \describe{ \item{list("labels")}{The text labels for each title.  User may
#' provide a single title, which will then be repeated for each plot (unless
#' the user also sets \code{type} to something other than \code{"all"}.)  Or
#' user may provide a vector of titles, one for each plot.  Default is not to
#' print titles.} \item{list("cex")}{Magnification of title text.  Default is
#' 1.} \item{list("col")}{The color of the titles.  Default is \code{black}.}
#' \item{list("fontsize")}{The fontsize of the title.  Default for \var{x}-axis
#' titles is XXX.  Varies by kind of title.} \item{list("fontface")}{The
#' fontface of the title.  Default is \code{plain}.}
#' \item{list("rot")}{Rotation of the title; one of 0, 90, 180, or 270.
#' Default varies by type of axis.} \item{list("x")}{Horizontal position of the
#' title relative to the title viewport, in npc.  Default is 0.5, the middle of
#' the viewport.} \item{list("y")}{Vertical position of the title relative to
#' the title viewport, in npc.  Default is 0.5, the middle of the viewport.}
#' \item{list("type")}{Character; indicates whether the titles should appear on
#' \code{all} plots, only the \code{first} plot, only in the first plot of each
#' \code{row} of plots, or only the first plot of each \code{column} of plots.
#' The default depends on the length of \code{label}: if 1 or as long as the
#' number of plots, each plot gets a title; if \code{label} is as long as the
#' number of rows or columns, then only first row or column gets a title.}
#' 
#' \item{list("add")}{Logical indicating whether the title should be plotted.
#' Will be set to \code{TRUE} if labels are given.  A vector input can
#' selectively turn on and off titles by plot.}
#' 
#' }
#' @param yaxistitle A list; places titles for the \var{y}-axes and controls
#' their format; see \code{xaxistitle} for parameters.
#' @param topaxistitle A list; places titles for the \var{top}-axes and
#' controls their format; see \code{xaxistitle} for parameters.
#' @param rightaxistitle A list; places titles for the \var{right}-axes and
#' controls their format; see \code{xaxistitle} for parameters.
#' @param maintitle A list; places a single title above the entire graphic and
#' controls its format. See \code{xaxistitle} for parameters.
#' @param rowtitle A list; places titles to the left of each row of plots and
#' controls their format. See \code{xaxistitle} for parameters.
#' @param columntitle A list; places titles above each column of plots and
#' controls their format. See \code{xaxistitle} for parameters.
#' @param plottitle A list; places titles above each plot and controls their
#' format. See \code{xaxistitle} for parameters.
#' @param undertitle A list; places titles below each plot and controls their
#' format. See \code{xaxistitle} for parameters.
#' @param RxC A 2-vector giving the number of rows and columns of the tiling of
#' plots. Default is to place all plots in a single row.
#' @param output A list controlling the output device: \describe{
#' \item{list("file")}{Name of the file where the graphic is to be saved.  An
#' appropriate extension is applied automatically.}
#' 
#' \item{list("width")}{Width of the entire graphic, in inches.  If provided,
#' plot height is calculated automatically based on \code{width} and the
#' provided tiling and titles.  If \code{width} is not provided, \code{tile}
#' will automatically select a width for the graphic by allocating a fixed
#' number of inches of width for each graphic (see option \code{null} under the
#' main input list \code{width} below).}
#' 
#' \item{list("pointsize")}{Pointsize for the device.  Default is 12.}
#' 
#' \item{list("family")}{Font family for the device.  Default is
#' \code{Helvetica}.}
#' 
#' \item{list("type")}{Device type: choices include \code{pdf},
#' \code{postscript}, \code{png}, \code{jpeg}, \code{bmp}, \code{bitmap},
#' \code{win.metafile}, \code{xfig}, \code{pictex}, \code{CairoPDF},
#' \code{CairoPS} (the latter two require the \code{Cairo} package be
#' installed).  Leave as \code{NULL} to plot to the default device (usually the
#' screen).  Default is \code{pdf}.  See \code{\link{Devices}} in package
#' \pkg{grDevices} for details.} \item{list("res")}{The resolution of raster
#' graphics (\code{jpeg}, \code{bmp}, \code{png}) in dpi.  Default is 300.}
#' 
#' \item{list("units")}{The units of \code{width}.  Default is inches.}}
#' @param width A list of widths of various plot elements.  Modify these to
#' improve the look of spacing of plots and titles, or to expand the width of
#' plotting areas.
#' 
#' \describe{
#' 
#' \item{list("null")}{Width of plotting areas in inches per null.  Default is
#' 2.  Multiplies by the width in nulls for each plot.  Setting this parameter
#' ensures a fixed width in inches for each plot in the tiling, and can help if
#' your plotting areas appear too small or too large.  This parameter is
#' overriden if the user fixes the overall width of the graphic using the
#' \code{width} argument to \code{output}, in which case the plotting area
#' widths are calculated automatically from the space left over after titles
#' are created.}
#' 
#' \item{list("plot")}{Width of the plotting areas in nulls (see
#' \code{\link{unit}}).  May be a vector.  Default is 1.  Higher values stretch
#' plots horizontally; lower values to squeeze them relative to titles and
#' layout elements.  The aspect ratio of the plots is ratio of the plot height
#' to this number.  Ordinarily, this parameter should only be changed if users
#' want different widths for different plots (in which case this should be a
#' vector); otherwise, use the plot height to adjust the aspect ratio, and the
#' width of a null (immediately above) to set the width of plots in inches.}
#' 
#' \item{list("spacer")}{Width of the space between each column of plots, or
#' between the \var{right}-axis titles and \var{y}-axis titles, if present.
#' Default is 2.  Measured in \code{char} units (see \code{\link{unit}}).
#' Higher values increase the whitespace between plots and can remove overlap
#' between stray tick labels.}
#' 
#' \item{list("leftborder")}{Width of the space before the first column of
#' plots, or before the row titles, if present.  Default is 2.  Measured in
#' \code{char} units (see \code{\link{unit}}).  Higher values increase the
#' whitespace before the first column and can eliminate clipping of tick labels
#' on the edge of the graphic.}
#' 
#' \item{list("rightborder")}{Width of the space after the final column of
#' plots, or after the final column's \var{right}-axis titles, if present.
#' Default is 2.  Measured in \code{char} units (see \code{\link{unit}}).
#' Higher values increase the whitespace after the final column and can
#' eliminate clipping of tick labels on the edge of the graphic.}
#' 
#' \item{list("rowtitle")}{Width of the column of rowtitles, as a multiple of
#' the widest rowtitle string width. Default is 1.25.}
#' 
#' \item{list("yaxistitle")}{Width of \var{y}-axis titles, as a multiple of the
#' width of the widest title.  Default is 2.  Because \var{y}-axis titles are
#' usually rotated 90 degrees, this usually multiplies the string height.}
#' 
#' \item{list("rightaxistitle")}{Width of \var{right}-axis titles, as a
#' multiple of the width of the widest title.  Default is 2.  Because
#' \var{right}-axis titles are usually rotated 90 degrees, this usually
#' multiplies the string height.}
#' 
#' \item{list("yaxis.labelspace")}{Width of space between \var{y}-axis tick
#' marks and tick labels, as a multiple of the width of the tick labels.
#' Default is 0.5.}
#' 
#' \item{list("rightaxis.labelspace")}{Width of space between \var{right}-axis
#' tick marks and tick labels, as a multiple of the width of the tick labels.
#' Default is 0.5.}
#' 
#' }
#' @param height A list of heights of various plot elements.  Modify these to
#' improve the look of spacing of plots and titles, or to change the aspect
#' ratio of plotting areas.
#' 
#' \describe{
#' 
#' \item{list("plot")}{Height of the plotting areas in nulls (see
#' \code{\link{unit}}); if the width of the plot is set to 1 null (the
#' default), then this parameter directly sets the aspect ratio.  May be a
#' vector.  Default is 1.  Higher values stretch plots vertically; lower values
#' to squeeze them relative to titles and layout elements, but may be
#' non-numeric: \dQuote{\code{square}} will set the aspect ratio to be 1:1
#' regardless of the plot width, and \dQuote{\code{golden}} will set plotting
#' area to be a golden rectangle.}
#' 
#' \item{list("spacer")}{Height of the space between each row of plots, or
#' between the \var{x}-axis titles and \var{top}-axis titles, if present.
#' Default is 2.  Measured in \code{char} units (see \code{\link{unit}}).
#' Higher values increase the whitespace between plots and can remove overlap
#' between stray tick labels.}
#' 
#' \item{list("topborder")}{Width of the space before the first row of plots,
#' or before the main title, if present.  Default is 2.  Measured in
#' \code{char} units (see \code{\link{unit}}).  Higher values increase the
#' whitespace before the first row and can eliminate clipping of tick labels on
#' the edge of the graphic.}
#' 
#' \item{list("bottomborder")}{Width of the space after the final row of plots,
#' or after the final row's under titles, if present.  Default is 2.  Measured
#' in \code{char} units (see \code{\link{unit}}).  Higher values increase the
#' whitespace after the final row and can eliminate clipping of tick labels on
#' the edge of the graphic.}
#' 
#' \item{list("maintitle")}{Height of the main title, as a multiple of the
#' maintitle string height. Default is 3.}
#' 
#' \item{list("columntitle")}{Height of the row of column titles, as a multiple
#' of the tallest columntitle string height. Default is 1.5.}
#' 
#' \item{list("plottitle")}{Height of the plot titles, as a multiple of the
#' tallest plot title string height. Default is 1.5.}
#' 
#' \item{list("undertitle")}{Height of the under titles, as a multiple of the
#' tallest under title string height. Default is 1.5.}
#' 
#' \item{list("xaxistitle")}{Height of \var{x}-axis titles, as a multiple of
#' the height of the tallest title.  Default is 2.}
#' 
#' \item{list("topaxistitle")}{Height of \var{top}-axis titles, as a multiple
#' of the height of the tallest title.  Default is 2.}
#' 
#' \item{list("xaxis.labelspace")}{Height of space between \var{x}-axis tick
#' marks and tick labels, as a multiple of the height of the tick labels.
#' Default is 0.75.}
#' 
#' \item{list("topaxis.labelspace")}{Height of space between \var{top}-axis
#' tick marks and tick labels, as a multiple of the height of the tick labels.
#' Default is 0.75.}
#' 
#' }
#' @param leeway When finding plot region limits automatically, add a little
#' leeway beyond data limits.  Default is 0.
#' @param draw Logical indicating whether a copy of the plot should be plotted
#' to the current screen device, in addition to the device specified in
#' \code{output}.  Default is \code{FALSE}.  Will plot over any content on
#' current screen.
#' @param defaults List object with default settings for title heights and
#' widths, and other tile parameters. Users will not normally need to changes
#' these settings.
#' @param layoutonly Logical.  If \code{TRUE}, \code{tile} will draw only axes,
#' titles, and frames, but no data.  Intended only as a diagnostic.  Default is
#' \code{FALSE}.
#' @param titleboxes Logical.  Draws boxes around title plotting areas.
#' Helpful as a diagnostic for poorly behavied layouts; not intended for final
#' plots.  Default is \code{FALSE}.
#' @param axisboxes Logical.  Draws boxes around axis plotting areas.  Helpful
#' as a diagnostic for poorly behavied layouts; not intended for final plots.
#' Default is \code{FALSE}.
#' @param verbose Logical.  If \code{TRUE}, \code{tile} displays a progress
#' report for error checking.  Default is \code{FALSE}.
#' @return \code{tile} is mainly called for the side-effect of saving a graphic
#' to the requested device.  However, it does return a list containing all the
#' user inputs, default settings, and numerous internal values.  This list
#' contains a copy of the entire plot in the slot \code{grob}.  To draw this
#' graphical object to the current device, give the output from tile as input
#' to \code{\link{tileDraw}}.
#' @author Christopher Adolph \email{cadolph@@uw.edu}
#' @seealso \code{\link{lineplot}}, \code{\link{scatter}},
#' \code{\link{ropeladder}}, \code{\link{nightplot}}, \code{\link{riceplot}},
#' \code{\link{rugTile}}, \code{\link{linesTile}}, \code{\link{pointsTile}},
#' \code{\link{textTile}}, \code{\link{polylinesTile}},
#' \code{\link{polygonTile}}
#' @references Murrell, Paul. (2006) \emph{R Graphics.} Chapman \& Hall/CRC.
#' @keywords hplot
#' @examples
#' 
#' 
#' # Example 1:  A tile layout template
#' 
#' trace1 <- textTile("Plot Area 1", x=500, y=50, plot=1)
#' trace2 <- textTile("Plot Area 2", top=500, right=50, plot=2)
#' trace3 <- textTile("Plot Area 3", x=500, y=50, plot=3)
#' trace4 <- textTile("Plot Area 4", top=500, right=50, plot=4)
#' trace5 <- textTile("Plot Area 5", x=500, y=50, plot=5)
#' trace6 <- textTile("Plot Area 6", top=500, right=50, plot=6)
#' 
#' at.x <- c(1,5,10,20,35)
#' at.y <- c(0,0.2,0.4,0.6,0.8,1)
#' 
#' tc <- tile(trace1,
#'            trace2,
#'            trace3,
#'            trace4,
#'            trace5,
#'            trace6,
#' 
#'            # Plotting options
#'            RxC=c(2,3),             # Tile 2 rows & 3 columns of plots
#'            
#'            gridlines=list(type="xytr"),
#' 
#'            limits=c(0.1,1000,        # min(x), max(x)
#'                     0,100,           # min(y), max(y)
#'                     0.1,1000,        # min(top), max(top)
#'                     0,100),          # min(right), max(right)
#'            
#'            xaxis=list(            # User provided tick locations
#'              log1=10,
#'              log3=10,
#'              log5=10
#'              ),
#'            
#'            xaxistitle=list(labels1="X Axis 1",
#'              labels3="X Axis 3",
#'              labels5="X Axis 5"),
#' 
#'            topaxis=list(at=at.x,            # User provided tick locations
#'                         log1=10,
#'                         log3=10,
#'                         log5=10
#'                         ),
#'  
#'            topaxistitle=list(labels2="Top Axis 2",
#'              labels4="Top Axis 4",
#'              labels6="Top Axis 6"),
#' 
#'            yaxistitle=list(labels1="Y Axis 1",
#'              labels3="Y Axis 3",
#'              labels5="Y Axis 5"),
#'  
#'            rightaxistitle=list(labels2="Right Axis 2",
#'              labels4="Right Axis 4",
#'              labels6="Right Axis 6"),
#'            
#'            plottitle=list(fontface="bold",
#'                           labels=c("Plot Title 1",
#'                                    "Plot Title 2",
#'                                    "Plot Title 3",
#'                                    "Plot Title 4",
#'                                    "Plot Title 5",
#'                                    "Plot Title 6")
#'                           ),
#'            
#'            maintitle=list(fontface="bold",
#'                           labels=c("Main title")
#'                           ),
#'            
#'            rowtitle=list(labels=c("Row Title 1",
#'                                   "Row Title 2")
#'                          ),
#'            
#'            columntitle=list(labels=c("Column Title 1",
#'                                      "Column Title 2",
#'                                      "Column Title 3")
#'                                      ),
#'            
#'            undertitle=list(fontface="italic",
#'                            labels=c("Under Title 1",
#'                                     "Under Title 2",
#'                                     "Under Title 3",
#'                                     "Under Title 4",
#'                                     "Under Title 5",
#'                                     "Under Title 6")
#'                            ),
#' 
#'            frame=TRUE,
#'            output = list(file="tile2x3layout")
#'            )
#' 
#' 
#' # Example 2:  Small multiples
#' 
#' 
#' # Example 3:  A multi-trace layout
#' 
#' 
#' 
#' 
#' @export tile
"tile" <-
function(...,

         ## Plot options
         limits = NULL,
         frame = FALSE,
         gridlines = list(),
                  
         ## Axes
         xaxis = list(),
         yaxis = list(),
         topaxis = list(),
         rightaxis = list(),

         ## Axis titles
         xaxistitle = list(),
         yaxistitle = list(),
         topaxistitle = list(),
         rightaxistitle = list(),

         ## Titles
         maintitle = list(),
         rowtitle = list(), 
         columntitle = list(),
         plottitle = list(),
         undertitle = list(),
                 
         ## Layout options
         RxC=NULL,
         output = list(),
         width = list(),
         height = list(),

         ## Special tile controls
         leeway = 0,
         draw = FALSE,
         defaults = list(),
         layoutonly = FALSE,
         titleboxes = FALSE,
         axisboxes = FALSE,
         verbose = FALSE
         ) {

    if (verbose) print("Starting tile...")
    
    # Load needed packages
    require(grid)

    # Process traces
    if (verbose) print("Processing traces")
    traces <- tileProcessTraces(list(...))
    
    # Set defaults
    if (verbose) print("Setting tile defaults")
    tc <- tilesetdefaults(RxC = RxC,
                          output = output,
                          width = width,
                          height = height,
                          plottitle = plottitle,
                          maintitle = maintitle,
                          undertitle = undertitle,
                          rowtitle = rowtitle,
                          columntitle = columntitle,
                          xaxis = xaxis,
                          yaxis = yaxis,
                          topaxis = topaxis,
                          rightaxis = rightaxis,
                          xaxistitle = xaxistitle,
                          yaxistitle = yaxistitle,
                          topaxistitle = topaxistitle,
                          rightaxistitle = rightaxistitle,
                          gridlines = gridlines,
                          limits = limits,
                          frame = frame,
                          draw = draw,
                          leeway = leeway,                           
                          defaults = defaults,
                          layoutonly = layoutonly,
                          titleboxes = titleboxes,
                          axisboxes = axisboxes)

    
    # Add trace controls to main tile control
    tc$traces <- traces
  
    # Fillout inputs for each plot
    if (verbose) print("Filling out tile settings")
    tc <- tilefilloutinputs(tc)

    # Harmonize axes
    if (verbose) print("Harmonizing axes")
    tc <- tileHarmonizeAxes(tc)    
    
    # Initialize widths and heights of plot elements
    if (verbose) print("Initializing plotting dimensions")
    tc <- tileInitWH(tc)
    
    # Iterate until plot dimensions are stable
    done <- 0
    secondpass <- 0
    iternum <- 0
    while (!done) {
        
        iternum <- iternum+1
        if (verbose) print(paste("Plot layout iteration",iternum))
    
        if (secondpass) {

             #Get current plot dimensions
            tc <-  tiledimensionsIter(tc)
            
            # Close existing plot
            dev.off()
            
            # Set new dimensions
            output.high.last2 <- output.high.last
            output.high.last <- tc$output$high
        
            strheights0 <- (tc$maintitle$add * max(tc$height$maintitle.npcmainSaved,na.rm=TRUE)        
                            + tc$columntitle$add * max(tc$height$columntitle.npcmainSaved,na.rm=TRUE))   

            strheights1 <- 0
            for (irows in 1:tc$RxC[1]) {
                #currplot <- getplotnums("row",irows,tc)

                 strheights1 <- (strheights1
                                + tc$height$plottitle.npcmainSaved[irows]
                                + tc$height$xaxistitle.npcmainSaved[irows]
                                + tc$height$topaxistitle.npcmainSaved[irows]
                                + tc$height$xaxis.npcmainSaved[irows]
                                + tc$height$topaxis.npcmainSaved[irows]
                                + tc$height$undertitle.npcmainSaved[irows]
                                )
            }
            
            tc$nullshigh <- sum(tc$height$plotmax[getplotnums("col",1,tc)]) #+ sum(tc$height$spacer)  # what about omitted last spacer?
            
            strwidths0 <- max(tc$width$rowtitle.npcmainSaved,na.rm=TRUE)*tc$rowtitle$add

            strwidths1 <- 0
            for (icols in 1:tc$RxC[2]) {
                #currplot <- getplotnums("column",icols,tc)

                strwidths1 <- (strwidths1
                               + tc$width$yaxis.npcmainSaved[icols]
                               + tc$width$rightaxis.npcmainSaved[icols]
                               + tc$width$yaxistitle.npcmainSaved[icols]
                               + tc$width$rightaxistitle.npcmainSaved[icols]
                               )
            }

            
            spacerwidth <- tc$width$leftborder.npcmain + (tc$RxC[2]-1)*tc$width$spacer.npcmain + tc$width$rightborder.npcmain
            spacerheight <- tc$height$topborder.npcmain + (tc$RxC[1]-1)*tc$height$spacer.npcmain + tc$height$bottomborder.npcmain
    
            tc$output$high <- ( tc$output$wide*(1 - strwidths0 - strwidths1 - spacerwidth)*(tc$nullshigh/tc$nullswide)/
                               (1 - strheights1 - strheights0 - spacerheight) )

            #print("Output width")
            #print(tc$output$wide)
            #print("Widths")
            #print(c(strwidths0, strwidths1, spacerwidth))
            #print("Heights")
            #print(c(strheights0, strheights1, spacerheight))
            #print("Output height")
            #print(tc$output$high)
            #print("")
            
            # Check for convergence
            cond <- (abs(tc$output$high - output.high.last2)<tc$defaults$ccrit) + (iternum>=tc$defaults$maxiter)
            if (cond) done <- 1
            
            # Open new plot
            tileopendevice(tc,done)
        } else {

            # Get current plot dimeninsions
            tc <- tiledimensions(tc)
            
            # Prepare and open first plotting attempt
            output.high.last <- 0
            tc$output$high <- tc$output$wide
            tileopendevice(tc,FALSE)
        }

        # Create main viewport
        tc$mainvp <- viewport(layout=tc$overlay,name="mainvp",
                              gp=gpar(fontsize=12),#tc$output$pointsize),
                              )

        
        # Loop over all main plots
        tc$grob <- gTree(name="tile",
                         children=gList(NULL)
                         )
        tc$currcol <- 1
        tc$currrow <- 1
        for (iplot in 1:tc$nplots) {
            
            # Prep main plot region (axes, space for rugs, gridlines, frame)
            if (verbose) print("Create main plotting area")
            tc$iplot <- iplot
            tc <- tilefindplotarea(tc)
            tc <- tileMakePlot(tc)
            
            # Add current gList to main gTree
            tc <- tilegrobNest(tc,name=paste("plotarea",tc$iplot,sep=""))
            if (verbose) print("Add titles to plots")
        
            #  Add Row titles
            if (tc$newrow&&tc$rowtitle$add) {
                if (verbose) print("  Adding row titles")                
                if (tc$special$rowtitle!="none") { 
                    tc <- eval(call(paste("tile.",tc$special$rowtitle,".rowtitle",sep=""),tc)) }
                else {
                    tc$rowtitle$layout.pos.col <- 2
                    tc$rowtitle$layout.pos.row <- tc$curplotrow
                    tc$rowtitle$fontsize <- tc$defaults$rowtitle$fontsize
                    tc <- tileTitle("rowtitle",tc,...)             
                }    
                tc <- tilegrobNest(tc,name=paste("rowtitle",tc$iplot,sep=""))
            }

            #  Add Column titles
            if (tc$newcol&&tc$columntitle$add) {
                if (verbose) print("  Adding column titles")
                if (tc$special$columntitle!="none") { 
                    tc <- eval(call(paste("tile.",tc$special$columntitle,".columntitle",sep=""),tc)) }
                else {
                    tc$columntitle$layout.pos.col <- tc$curplotcol
                    tc$columntitle$layout.pos.row <- (tc$curplotrow
                                                      - any(tc$topaxistitle$add[tc$currrowplots])
                                                      - any(tc$topaxis$add[tc$currrowplots])
                                                      - any(tc$plottitle$add[tc$currrowplots])
                                                      - 1)
                    tc$columntitle$fontsize <- tc$defaults$columntitle$fontsize
                    tc <- tileTitle("columntitle",tc,...)
                }
                tc <- tilegrobNest(tc,name=paste("columntitle",tc$iplot,sep=""))
            }

          
            #  Add main titles 
            if ((iplot==1)&&tc$maintitle$add) {
                if (verbose) print("  Adding main titles")
                if (tc$special$maintitle!="none")
                    tc <- eval(call(paste("tile.",tc$special$maintitle,".maintitle",sep=""),tc))
                else {
                    tc$maintitle$layout.pos.col <- tc$curplotcol
                    tc$maintitle$layout.pos.row <- (tc$curplotrow
                                                    - any(tc$topaxistitle$add[tc$currrowplots])
                                                    - any(tc$topaxis$add[tc$currrowplots])
                                                    - any(tc$plottitle$add[tc$currrowplots])
                                                    - any(tc$columntitle$add)
                                                    - 1)

                    tc$maintitle$fontsize <- tc$defaults$maintitle$fontsize
                    tc <- tileTitle("maintitle",tc,...)
                }
                tc <- tilegrobNest(tc,name="maintitle")
            }

            #  Add plot titles
            if (tc$plottitle$add[tc$iplot]) {
                if (verbose) print("  Adding plot titles")
                if (tc$special$plottitle[tc$iplot]!="none")
                    tc <- eval(call(paste("tile.",tc$special$plottitle[tc$iplot],".plottitle",sep=""),tc))
                else {
                    tc$plottitle$layout.pos.col <- tc$curplotcol
                    tc$plottitle$layout.pos.row <- (tc$curplotrow
                                                    - any(tc$topaxistitle$add[tc$currrowplots])
                                                    - any(tc$topaxis$add[tc$currrowplots])
                                                    - 1)                
                    tc$plottitle$fontsize <- tc$defaults$plottitle$fontsize
                    tc <- tileTitle("plottitle",tc,...)
                }
                tc <- tilegrobNest(tc,name=paste("plottitle",tc$iplot,sep=""))
            }

           
            
            #  Add under titles
            if (tc$undertitle$add[tc$iplot]) {
                if (verbose) print("  Adding under titles")
                if (tc$special$undertitle[tc$iplot]!="none")
                    tc <- eval(call(paste("tile.",tc$special$undertitle[tc$iplot],".undertitle",sep=""),tc))
                else {
                    tc$undertitle$layout.pos.col <- tc$curplotcol
                    tc$undertitle$layout.pos.row <- (tc$curplotrow
                                                     + any(tc$xaxistitle$add[tc$currrowplots])
                                                     + any(tc$xaxis$add[tc$currrowplots])
                                                     + 1)
                    tc$undertitle$fontsize <- tc$defaults$undertitle$fontsize
                    tc <- tileTitle("undertitle",tc,...)
                }
                tc <- tilegrobNest(tc,name=paste("undertitle",tc$iplot,sep=""))
            }
            
            #  Add xaxis titles           
            if (tc$xaxistitle$add[tc$iplot]) {
                if (verbose) print("  Adding xaxis titles")
                tc$xaxistitle$layout.pos.col <- tc$curplotcol
                tc$xaxistitle$layout.pos.row <- (tc$curplotrow
                                                 + any(tc$xaxis$add[tc$currrowplots])
                                                 + 1)
                if (tc$special$xaxistitle[tc$iplot]!="none")
                    tc <- eval(call(paste(tc$special$xaxistitle[tc$iplot],"TileAxisTitle",sep=""),"xaxis",tc))
                else {
                    tc$xaxistitle$fontsize <- tc$defaults$xaxistitle$fontsize
                    tc <- tileAxistitleprep("xaxis",tc,...)
                    tc <- tileAxistitle("xaxis",tc,...)                   
                }
                tc <- tilegrobNest(tc,name=paste("xaxistitle",tc$iplot,sep=""))        
            }
        
            #  Add topaxis titles  
            if (tc$topaxistitle$add[tc$iplot]) {
                if (verbose) print("  Adding topaxis titles")
                tc$topaxistitle$layout.pos.col <- tc$curplotcol
                tc$topaxistitle$layout.pos.row <- (tc$curplotrow
                                                   - any(tc$topaxis$add[tc$currrowplots])
                                                   - 1)                
                if (tc$special$topaxistitle[tc$iplot]!="none")
                    tc <- eval(call(paste(tc$special$topaxistitle[tc$iplot],"TileAxisTitle",sep=""),"topaxis",tc))
                else {
                    tc$topaxistitle$fontsize <- tc$defaults$topaxistitle$fontsize
                    tc <- tileAxistitleprep("topaxis",tc,...)
                    tc <- tileAxistitle("topaxis",tc,...)
                }
                tc <- tilegrobNest(tc,name=paste("topaxistitle",tc$iplot,sep=""))
            }        
        
            #  Add yaxis titles
            if (tc$yaxistitle$add[tc$iplot]) {              
                if (verbose) print("  Adding yaxis titles")
                tc$yaxistitle$layout.pos.col <- (tc$curplotcol
                                                 - any(tc$yaxis$add[tc$currcolplots])
                                                 - 1)
                tc$yaxistitle$layout.pos.row <- tc$curplotrow
                if (tc$special$yaxistitle[tc$iplot]!="none") {
                    tc <- eval(call(paste(tc$special$yaxistitle[tc$iplot],"TileAxisTitle",sep=""),"yaxis",tc))
                } else {
                    tc$yaxistitle$fontsize <- tc$defaults$yaxistitle$fontsize
                    tc <- tileAxistitleprep("yaxis",tc,...)
                    tc <- tileAxistitle("yaxis",tc,...)
                }
                tc <- tilegrobNest(tc,name=paste("yaxistitle",tc$iplot,sep=""))
            }         
            
            #  Add rightaxis titles  
            if (tc$rightaxistitle$add[tc$iplot]) {
                if (verbose) print("  Adding rightaxis titles")
                tc$rightaxistitle$layout.pos.col <- (tc$curplotcol
                                                     + any(tc$rightaxis$add[tc$currcolplots])
                                                     + 1)
                tc$rightaxistitle$layout.pos.row <- tc$curplotrow
                if (tc$special$rightaxistitle[tc$iplot]!="none")
                    tc <- eval(call(paste(tc$special$rightaxistitle[tc$iplot],"TileAxisTitle",sep=""),"rightaxis",tc))
                else {
                    tc$rightaxistitle$fontsize <- tc$defaults$rightaxistitle$fontsize
                    tc <- tileAxistitleprep("rightaxis",tc,...)
                    tc <- tileAxistitle("rightaxis",tc,...)
                }
                tc <- tilegrobNest(tc,name=paste("rightaxistitle",tc$iplot,sep=""))
            }
        }

        # Loop over traces for plotting
        if (!tc$layoutonly&&done) {
            for (itrace in 1:tc$ntraces) {

                tc$itrace <- itrace
                tc$iplot <- tc$traces[[tc$itrace]]$plot
                tc <- tilefindplotarea(tc)
                
                if (verbose) print(paste("   Adding trace",itrace,"to plot"))
            
                # pass to XTilePlot
                tc <- eval(call(paste(tc$traces[[tc$itrace]]$graphic,"TilePlot",sep=""),tc))

                # Add current gList to main gTree
                tc <- tilegrobNest(tc,name=paste("trace",tc$itrace,sep=""))                
            }
        }
        
        # Draw graphic
        if (verbose&&done) print("Drawing graphic to output device")
        if (done) tileDraw(tc$grob)
        secondpass <- 1

    }   
    
    # Save file 
    if (!is.null(tc$output$outfile))
        dev.off()

    if (tc$draw) {
        if (verbose) print("Drawing second copy of graphic to default screen output")
        tileDraw(tc$grob)
    }
    
    if (verbose) print("Tile run complete.")

    invisible(tc)  
}

