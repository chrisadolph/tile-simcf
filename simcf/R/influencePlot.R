
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
         
