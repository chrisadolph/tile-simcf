riceplotTileTraceFillout <- function(trace,tracedefault) {

    if (is.null(trace$layer))
        trace$layer <- 10#tracedefault$defaultlayer
  
  if (!is.null(trace$color)) {
    trace$color <- mergelist(trace$color,tracedefault$color)
    if (!is.null(trace$color$data)) {
      trace$fill <- tileColorcode(data=trace$color$data,
                                 n=trace$color$bins,
                                 breaks=trace$color$breaks,
                                 colorset=trace$color$colorset,
                                 hue=trace$color$hue,
                                 chroma=trace$color$chroma,
                                 luminance=trace$color$luminance,
                                 power=trace$color$power,
                                 gamma=trace$color$gamma,
                                 fixup=trace$color$fixup)
    } else {
      trace$fill <- tileColorcode(data=1,
                                 n=trace$color$bins,
                                 breaks=trace$color$breaks,
                                 colorset=trace$color$set,
                                 hue=trace$color$hue,
                                 chroma=trace$color$chroma,
                                 luminance=trace$color$luminance,
                                 power=trace$color$power,
                                 gamma=trace$color$gamma,
                                 fixup=trace$color$fixup)
    }
  }
  
  trace$fit <- mergelist(trace$fit,tracedefault$fitdefault)
  if (!is.null(trace$method)) {
    trace$fit$col <- fillout(trace$fit$col,length(trace$fit$method))
    trace$fit$mark <- c(trace$fit$mark,fillout("shaded",length(trace$fit$ci)))[1:length(trace$fit$ci)]
  }
  
  trace$ci <- mergelist(trace$ci,tracedefault$cidefault)
  
  trace$ci$mark <- c(trace$ci$mark,fillout("lines",length(trace$ci$levels)))[1:length(trace$ci$levels)]

        trace$extrapolate <- mergelist(trace$extrapolate,tracedefault$extrapolatedefault)
  
  trace <- mergelist(trace,tracedefault)

    # Check for and process simulate data
  trace$factors <- setdiff(trace$factors,trace$simulates)    
  trace <- tilesimulates(trace)
  
  maxtrace <- max(length(trace$x),
                  length(trace$y),
                  length(trace$top),
                  length(trace$right)
                  )
  
  if (maxtrace) {
    trace$fill <- fillout(trace$fill,maxtrace)
    trace$col <- fillout(trace$col,maxtrace)
    trace$lty <- fillout(trace$lty,maxtrace)
    trace$lwd <- fillout(trace$lwd,maxtrace)
    trace$cex <- fillout(trace$cex,maxtrace)
    trace$fontsize <- fillout(trace$fontsize,maxtrace)
    trace$lineheight <- fillout(trace$lineheight,maxtrace)
    trace$font <- fillout(trace$font,maxtrace)
    trace$fontfamily <- fillout(trace$fontfamily,maxtrace)
    trace$fontface <- fillout(trace$fontface,maxtrace)
    trace$alpha <- fillout(trace$alpha,maxtrace)
    trace$lineend <- fillout(trace$lineend,maxtrace)
    trace$linejoin <- fillout(trace$linejoin,maxtrace)
    trace$linemitre <- fillout(trace$linemitre,maxtrace)
    trace$lex <- fillout(trace$lex,maxtrace)
    trace$pch <- fillout(trace$pch,maxtrace)
    trace$size <- fillout(trace$size,maxtrace)
    trace$addArrow <- fillout(trace$addArrow,maxtrace)
    trace$angleArrow <- fillout(trace$angleArrow,maxtrace)
    trace$lengthArrow <- filloutunits(trace$lengthArrow,maxtrace)
    trace$endsArrow <- fillout(trace$endsArrow,maxtrace)
    trace$typeArrow <- fillout(trace$typeArrow,maxtrace)
    trace$just <- fillout(trace$just,maxtrace)
    trace$hjust <- fillout(trace$hjust,maxtrace)
    trace$vjust <- fillout(trace$vjust,maxtrace)
    trace$rot <- fillout(trace$rot,maxtrace)
    trace$check.overlap <- fillout(trace$check.overlap,maxtrace)
    trace$labelsxoffset <- fillout(trace$labelsxoffset,maxtrace)
    trace$labelsyoffset <- fillout(trace$labelsyoffset,maxtrace)
    trace$angle <- fillout(trace$angle,maxtrace)
    trace$thickness <- fillout(trace$thickness,maxtrace)
  }

          # Check for and process extrapolation data
        if (!is.null(trace$extrapolate$control)&&!any(is.na(trace$extrapolate$control))) {
            trace$extrapolate$control <- fillout(trace$extrapolate$control,maxtrace)
        } else {
            if (!is.null(trace$extrapolate$data)&&!is.null(trace$extrapolate$cfact)) {
                trace$extrapolate <- tileWhatIf(trace$extrapolate)
            } else {
                trace$extrapolate$control <- fillout(FALSE,maxtrace)
            }
        }
    
  trace
}
