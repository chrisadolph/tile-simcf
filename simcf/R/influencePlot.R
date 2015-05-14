#' Create an interactive plot of studentized residuals against hat-values
#' 
#' Assess outliers by plotting the discrepancy of each observation against its
#' leverage, with optional interactive labeling of points.
#' 
#' The x-axis of the plot shows "standardized" hat-values, which are equivalent
#' to the diagonal elements of the hat matrix divided by the total number of
#' parameters in the model.  Higher hat-values imply greater leverage.
#' 
#' The y-axis plots studentized residuals.  Values outside the [-2,2] range
#' should occur by chance for only 1 in 20 observations.
#' 
#' @param object A regression object, e.g., as produced by \code{\link{lm}}.
#' @param names An optional vector of observation names for interactively
#' labeling points.
#' @param identify Logical, allow interactive labeling of points.  If
#' \code{identify} is TRUE, the \code{pdf} should be NULL.
#' @param pdf Character, the name of a file to save the graphic as a pdf
#' instead of plotting to a screen device.  Default is no pdf.
#' @param main Character, option title for the plot.
#' @param box Logical, draw a box around the plotting area.  Default is FALSE.
#' @param ... Other options passed to \code{\link{pdf}}.
#' @return Creates a plot; no object returned.
#' @author Christopher Adolph <\email{cadolph@@u.washington.edu}>
#' @keywords hplot,robust
influencePlot <- function(res,names=NULL,identify=TRUE,pdf=NULL,main=NULL,box=FALSE,...) {
  hatscore <- hatvalues(res)/mean(hatvalues(res))
  rstu <- rstudent(res)
  if (is.null(names))
    names <- 1:length(hatscore)
  if (!is.null(pdf)) {
    pdf(file=pdf,...)
  }
  plot.new()
  ux <- max(max(hatscore),5)
  ly <- min(min(rstu),-3.5)
  uy <- max(max(rstu),3.5)
  usr <- c(0,ux,ly,uy)
  par(usr=usr,tcl=-0.1,mgp=c(2,0.35,0))  
  axis(2)
  par(usr=usr,tcl=-0.1,mgp=c(2,0.15,0))
  if (identical(ux,5)) {
    axis(1,at=c(0,1,2,3,4,5))
  } else {
    axis(1)
  }
  title(xlab="Standardized hat-values",ylab="Studentized residuals")
  if (!is.null(main))
    title(main=main)
  points(hatscore,rstu, col = "blue")
  lines(c(usr[1],usr[2]),c(-2,-2),lty="dashed")
  lines(c(usr[1],usr[2]),c(2,2),lty="dashed")
  lines(c(2,2),c(usr[3],usr[4]),lty="dashed")
  lines(c(3,3),c(usr[3],usr[4]),lty="dotted")
  if (box) box()
  if (identify) identify(hatscore,rstu,names)
  if (!is.null(pdf)) dev.off()
  invisible()
}
         
