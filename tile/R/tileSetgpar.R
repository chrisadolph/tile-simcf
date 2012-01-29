tileSetgpar <- function(ct,
                        index=1,
                        fill=NULL,
                        col=NULL,
                        lty=NULL,
                        lwd=NULL,
                        cex=NULL,
                        fontsize=NULL,
                        lineheight=NULL,
                        font=NULL,
                        fontfamily=NULL,
                        alpha=NULL,
                        lineend=NULL,
                        linejoin=NULL,
                        linemitre=NULL,
                        lex=NULL
                        ) {


    if (is.null(fill))
        fill <- ct$fill[index]    
    if (is.null(col))
        col <- ct$col[index]
    if (is.null(lty))
        lty <- ct$lty[index]
    if (is.null(cex))
        cex <- ct$cex[index]
    if (is.null(fontsize))
        fontsize <- ct$fontsize[index]
    if (is.null(lineheight))
        lineheight <- ct$lineheight[index]
    if (is.null(font))
        font <- ct$font[index]
    if (is.null(fontfamily))
        fontfamily <- ct$fontfamily[index]
    if (is.null(alpha))
        alpha <- ct$alpha[index]
    if (is.null(lineend))
        lineend <- ct$lineend[index]
    if (is.null(linejoin))        
        linejoin <- ct$linejoin[index]
    if (is.null(linemitre))
        linemitre <- ct$linemitre[index]
    if (is.null(lex))
        lex <- ct$lex[index]
    
    cg <- gpar(fill=fill,
               col=col,
               lty=lty,
               lwd=lwd,
               cex=cex,
               fontsize=fontsize,
               lineheight=lineheight,
               font=font,
               fontfamily=fontfamily,                                     
               alpha=alpha,
               lineend=lineend,
               linejoin=linejoin,
               linemitre=linemitre,
               lex=lex
               )
cg
}
