tilesetdefaults <- function(RxC=NULL,
                            output = list(),
                            width = list(),
                            height = list(),
                            plottitle = list(),
                            maintitle = list(),
                            undertitle = list(),
                            rowtitle = list(),
                            columntitle = list(),
                            xaxis = list(),
                            yaxis = list(),
                            topaxis = list(),
                            rightaxis = list(),
                            xaxistitle = list(),
                            yaxistitle = list(),
                            topaxistitle = list(),
                            rightaxistitle = list(),
                            gridlines = list(),
                            limits = c(NA,NA,NA,NA,NA,NA,NA,NA),   # NULL?
                            frame = FALSE,
                            draw= FALSE,
                            leeway = 0,              
                            defaults = list(),
                            layoutonly = FALSE,
                            titleboxes = FALSE,
                            ...) {

    # Generic preparation (Fill in defaults where needed)

    if (!is.null(output$width))
      output$wide <- output$width
    if (!is.null(output$file))
      output$outfile <- output$file
    
    output.def <- list(outfile=NULL,
                       high=NULL,
                       width=7.5,
                       pointsize=12,
                       family="Helvetica",
                       type="pdf",
                       res=300,
                       units="in"
                       )
    
    output <- mergelist(output,output.def)

    output$wide <- output$width
    
    width.def <- list(plot=1,
                      spacer=1,   #3
                      rightborder=2,
                      rowtitle=1.25,
                      yaxis=1.5,
                      rightaxis=1.5,
                      yaxistitle=1,
                      rightaxistitle=1,
                      xaxistitle=1,
                      topaxistitle=1,
                      yaxis.rug=1,
                      rightaxis.rug=1,
                      maintitle=1,
                      undertitle=1,
                      plottitle=1,
                      columntitle=1
                      )
    
    width <- mergelist(width,width.def)
    
    height.def <- list(plot=1,
                       spacer=1,   #3
                       bottomborder=1,   #2
                       xaxis=1,
                       topaxis=1,
                       xaxistitle=1.05,   #  1.75
                       topaxistitle=1.05,  # 1.75
                       xaxis.rug=1,
                       topaxis.rug=1,
                       maintitle=1.5,         # 7
                       undertitle=1.25,     # 3
                       plottitle=1.25,   # 3
                       columntitle=1.25,  # 5.5
                       rowtitle=1,    #3.5,
                       yaxistitle=1,
                       rightaxistitle=1
                       )
    
    
    height <- mergelist(height,height.def)
    
    
    title.def <- list(labels=NULL,
                      cex=1,
                      col="black",
                      fontsize=c(12),  #12
                      fontface="plain",
                      rot=NULL,
                      x=0.5,
                      y=0.5,
                      type=NULL,
                      add=FALSE)

    cfs <- output$pointsize
    defaults$rowtitle$fontsize <-               c(cfs,       cfs + 2)
    defaults$columntitle$fontsize <-            c(cfs,       cfs + 2)
    defaults$plottitle$fontsize <-              c(cfs+2,     cfs + 2)
    defaults$maintitle$fontsize <-              c(cfs+4,     cfs + 3)
    defaults$undertitle$fontsize <-             c(cfs+2,     cfs + 2)  
    defaults$xaxistitle$fontsize <-             c(cfs,       cfs + 1)
    defaults$yaxistitle$fontsize <-             c(cfs,       cfs + 1)
    defaults$topaxistitle$fontsize <-           c(cfs,       cfs + 1)
    defaults$rightaxistitle$fontsize <-         c(cfs,       cfs + 1)
    defaults$layer <-                           10
    defaults$ccrit <-                           0.01
    defaults$maxiter <-                         10

    rowtitle.def <- columntitle.def <- plottitle.def <- maintitle.def <-
        undertitle.def <- xaxistitle.def <- yaxistitle.def <- topaxistitle.def <-
            rightaxistitle.def <- title.def

    rowtitle.def$fontsize <- defaults$rowtitle$fontsize
    columntitle.def$fontsize <- defaults$columntitle$fontsize
    plottitle.def$fontsize <- defaults$plottitle$fontsize
    maintitle.def$fontsize <- defaults$maintitle$fontsize
    undertitle.def$fontsize <- defaults$undertitle$fontsize
    xaxistitle.def$fontsize <- defaults$xaxistitle$fontsize
    yaxistitle.def$fontsize <- defaults$yaxistitle$fontsize
    topaxistitle.def$fontsize <- defaults$topaxistitle$fontsize
    rightaxistitle.def$fontsize <- defaults$rightaxistitle$fontsize
    
    plottitle <- mergelist(plottitle,plottitle.def) 
    undertitle <- mergelist(undertitle,undertitle.def)
    maintitle <- mergelist(maintitle,maintitle.def)
    rowtitle <- mergelist(rowtitle,rowtitle.def)
    columntitle <- mergelist(columntitle,columntitle.def)
    xaxistitle <- mergelist(xaxistitle,xaxistitle.def)
    yaxistitle <- mergelist(yaxistitle,yaxistitle.def)
    topaxistitle <- mergelist(topaxistitle,topaxistitle.def)
    rightaxistitle <- mergelist(rightaxistitle,rightaxistitle.def)
    if(is.null(plottitle$rot)) {plottitle$rot <- 0}
    if(is.null(maintitle$rot)) {maintitle$rot <- 0}
    if(is.null(rowtitle$rot)) {rowtitle$rot <- 0}
    if(is.null(undertitle$rot)) {undertitle$rot <- 0}
    if(is.null(columntitle$rot)) {columntitle$rot <- 0}    
    if(is.null(xaxistitle$rot)) {xaxistitle$rot <- 0}
    if(is.null(yaxistitle$rot)) {yaxistitle$rot <- 90}
    else if ((is.null(yaxistitle$x))||((yaxistitle$rot)==0)) {yaxistitle$y=0.8}
    if(is.null(topaxistitle$rot)) {topaxistitle$rot <- 0}
    if(is.null(rightaxistitle$rot)) {rightaxistitle$rot <- 270}
    else if ((is.null(rightaxistitle$x))||((rightaxistitle$rot)==0)) {rightaxistitle$y=0.8}
    
    
    axis.def <- list(at=NA,
                     labels=NA,
                     tick.length=-0.5,
                     label.loc=-1.25,
                     ticks=TRUE,
                     ntics=5,
                     major=TRUE,
                     col="black",
                     lwd=0.5,
                     cex=1,
                     fontsize=cfs,
                     add=NULL,
                     log=FALSE,
                     rug=FALSE)
    xaxis <- mergelist(xaxis,axis.def)
    yaxis <- mergelist(yaxis,axis.def)
    topaxis <- mergelist(topaxis,axis.def)
    rightaxis <- mergelist(rightaxis,axis.def)

    #limits <- c(limits,rep(NA,8))[1:8]
    
    # gridline defaults
    gridlines.def <- list(type=NULL,  # or "x" or "y" or "xy"
                          lwd=0.15,
                          col="gray50",
                          lty="solid",
                          xat=NULL,
                          yat=NULL,
                          tat=NULL,
                          yat=NULL,
                          limits=NULL,           # length 4
                          edges=FALSE
                          )
    gridlines <- mergelist(gridlines,gridlines.def)

    
    xaxistitleControl <- list(labels=NULL,
                             cex=1,
                             col="black",
                             fontsize=cfs,
                             fontface="plain",
                             rot=NULL,
                             x=0.5,
                             y=0.5,
                             type=NULL,
                             add=FALSE
                             )
    yaxistitleControl <- list(labels=NULL,
                             cex=1,
                             col="black",
                             fontsize=cfs,
                             fontface="plain",
                             rot=NULL,
                             x=0.5,
                             y=0.5,
                             type=NULL,
                             add=FALSE
                             )
    topaxistitleControl <- list(labels=NULL,
                             cex=1,
                             col="black",
                             fontsize=cfs,
                             fontface="plain",
                             rot=NULL,
                             x=0.5,
                             y=0.5,
                             type=NULL,
                             add=FALSE
                             )
    rightaxistitleControl <- list(labels=NULL,
                             cex=1,
                             col="black",
                             fontsize=cfs,
                             fontface="plain",
                             rot=NULL,
                             x=0.5,
                             y=0.5,
                             type=NULL,
                             add=FALSE
                             )
    plottitleControl <- list(labels=NULL,
                             cex=1,
                             col="black",
                             fontsize=cfs,
                             fontface="plain",
                             rot=NULL,
                             x=0.5,
                             y=0.5,
                             type=NULL,
                             add=FALSE
                             )
    undertitleControl <- list(labels=NULL,
                             cex=1,
                             col="black",
                             fontsize=cfs,
                             fontface="plain",
                             rot=NULL,
                             x=0.5,
                             y=0.5,
                             type=NULL,
                             add=FALSE
                             )
    
    special <- list(rowtitle="none",
                    columntitle="none",
                    plottitle="none",
                    maintitle="none",
                    undertitle="none",
                    xaxistitle="none",
                    yaxistitle="none",
                    topaxistitle="none",
                    rightaxistitle="none",
                    plottitleControl=plottitleControl,
                    undertitleControl=undertitleControl,
                    xaxistitleControl=xaxistitleControl,
                    yaxistitleControl=yaxistitleControl,
                    topaxistitleControl=topaxistitleControl,
                    rightaxistitleControl=rightaxistitleControl,
                    fontsizeRopeladder=0)
        
    grob <- NULL
    mainvp <- NULL
    currvp <- NULL
    
    tc <- list(RxC=RxC,
               output=output,
               width=width,
               height=height,
               rowtitle=rowtitle,
               columntitle=columntitle,
               plottitle=plottitle,
               maintitle=maintitle,
               undertitle=undertitle,
               xaxis=xaxis,
               yaxis=yaxis,
               topaxis=topaxis,
               rightaxis=rightaxis,
               xaxistitle=xaxistitle,
               yaxistitle=yaxistitle,
               topaxistitle=topaxistitle,
               rightaxistitle=rightaxistitle,
               gridlines=gridlines,
               limits=limits,
               frame=frame,
               draw=draw,
               leeway=leeway,
               defaults=defaults,
               layoutonly=layoutonly,
               titleboxes=titleboxes,
               special=special,
               grob=grob,
               mainvp=mainvp,
               currvp=currvp
               )
    
    #if (tc$height$plot=="square") tc$height$plot <- tc$width$plot
    #if (tc$height$plot=="golden") tc$height$plot <- tc$width$plot/1.61803
    
    #tc$width$spacer <- tc$width$spacer*min(c(tc$width$plot,tc$height$plot))
    #tc$height$spacer <- tc$height$spacer*min(c(tc$width$plot,tc$height$plot))  # width or height?  aesthetic question

    class(tc) <- c(class(tc),"tile")
    
    tc
}
