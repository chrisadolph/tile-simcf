rugTileTraceFillout <- function(trace,tracedefault) {

  if (!is.null(trace$color)) {
      trace$color <- mergelist(trace$color,tracedefault$color)
      if (!is.null(trace$color$data)) {
        trace$col <- tileColorcode(data=trace$color$data,
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
        trace$col <- tileColorcode(data=1,
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
  
    trace <- mergelist(trace,tracedefault)
    
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
        trace$kernel <- fillout(trace$kernel,maxtrace)
    }
    trace
}
