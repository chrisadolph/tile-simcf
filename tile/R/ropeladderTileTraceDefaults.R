ropeladderTileTraceDefaults <- function(x=NULL,
                                        y=NULL,
                                        top=NULL,
                                        right=NULL,
                                        xlower=NULL,
                                        xupper=NULL,
                                        ylower=NULL,
                                        yupper=NULL,
                                        toplower=NULL,
                                        topupper=NULL,
                                        rightlower=NULL,
                                        rightupper=NULL,
                                        attachToTop=NULL,
                                        attachToRight=NULL,
                                        simulates=NULL,
                                        factors=c("x","y","top","right"),
                                        xdatalimits=c("x","lower","upper"),
                                        ydatalimits=c("y","lower","upper"),
                                        xtransform=c("x","lower","upper"),
                                        ytransform=c("y","lower","upper"),
                                        extrapolate=NULL,
                                        graphic="ropeladder",
                                        plot=NULL,
                                        layer=10,
                                        defaultlayer=10,
                                        fit=NULL,
                                        upper=NULL,
                                        lower=NULL,
                                        se=NULL,
                                        markers=TRUE,
                                        labels=NULL,
                                        labelsxoffset=0,
                                        labelsyoffset=0,
                                        sublabels=NULL,
                                        sublabelsX = NULL,
                                        sublabelsY = NULL,
                                        sublabelsxoffset=0,
                                        sublabelsyoffset=0,
                                        color=color,
                                        fill="transparent",
                                        col="black",
                                        lty="solid",
                                        lwd=1,
                                        cex=1,
                                        fontsize=12,
                                        lineheight=1.2,
                                        font=1,
                                        fontfamily="",
                                        fontface="plain",
                                        alpha=1,#0.8,                   # only if pdf
                                        lineend="round",
                                        linejoin="round",
                                        linemitre=10,
                                        lex=1,
                                        pch=16,
                                        size=0.4,
                                        addArrow=FALSE,
                                        angleArrow=30,
                                        lengthArrow=unit(0.25, "inches"),
                                        endsArrow="last",
                                        typeArrow="open",
                                        just="centre",
                                        hjust=NULL,
                                        vjust=NULL,
                                        rot=0,
                                        check.overlap=FALSE,
                                        clip="on",
                                        lighten=0.75,
                                        
                                       
                                        connect=FALSE,
                                        type=1,
                                        baseline=NULL,                   
                                        shadowbox=NULL,
                                        arrowsout=TRUE,
                                        entryheight=1/6,
                                        subentryheight=0.6,
                                        sublabelsfontsize=8,
                                        mirrorlabels=FALSE,
                                        group=NULL,
                                        shadowrow=NULL,
                                        spaceAbove=0,
                                        spaceBelow=0
                                        
                                        ) {


  if (type==1) {
    baselinedefault <- list(at=NULL,
                            lty="dashed",
                            col="black",
                            lwd=lwd
                            )
  }
  if (type==2) {
    baselinedefault <- list(at=NA,
                            lty="solid",
                            col=col,
                            lwd=0
                            )
  }

    axislabels <- list(attach=NULL,
                       labels=NULL,
                       cex=NULL,
                       fontsize=NULL,
                       font=NULL,
                       fontfamily=NULL
                       )
    height <- list(plot=NULL)
    width <- list(width=NULL)
    
    special <- list(height=height,
                    width=width,
                    limits=NULL,
                    axislabels=axislabels
                    )
    
    fitdefault <- list(method=NULL,
                      ci=0.95,
                      mark="shaded",
                      col="black"                      
                      )

    cidefault <- list(levels=c(0.95),
                      mark=c("lines")
                      )

        color <- list(data=NULL,
                  bins=NULL,
                  breaks=NULL,
                  colorset="sequential",# rainbow, diverge, heatmap, terrain, categories
                  hue=NULL,
                  chroma=NULL,
                  luminance=NULL,
                  power=NULL,
                  gamma=2.4,
                  fixup=TRUE
                  )


    extrapolatedefault <- list(formula=NULL,
                               data=NULL,
                               cfact=NULL,
                               control=NULL,
                               omit.extrapolated=FALSE
                               )


    defaults <- list(x=x,
                     y=y,
                     top=top,
                     right=right,
                     xlower=xlower,
                     xupper=xupper,
                     ylower=ylower,
                     yupper=yupper,
                     toplower=toplower,
                     topupper=topupper,
                     rightlower=rightlower,
                     rightupper=rightupper,
                     attachToTop=attachToTop,
                     attachToRight=attachToRight,
                     simulates=simulates,
                     factors=factors,
                     xdatalimits=xdatalimits,
                     ydatalimits=ydatalimits,
                     xtransform=xtransform,
                     ytransform=ytransform,
                     extrapolatedefault=extrapolatedefault,
                     cidefault=cidefault,
                     graphic=graphic,
                     plot=plot,
                     layer=layer,
                     defaultlayer=defaultlayer,
                     fitdefault=fitdefault,
                     upper=upper,
                     lower=lower,
                     se=se,
                     markers=markers,
                     labels=labels,
                     labelsxoffset=labelsxoffset,
                     labelsyoffset=labelsyoffset,
                     sublabels=sublabels,
                     sublabelsX=sublabelsX,
                     sublabelsX=sublabelsY,
                     sublabelsxoffset=sublabelsxoffset,
                     sublabelsyoffset=sublabelsyoffset,
                     color=color,
                     fill=fill,
                     col=col,
                     lty=lty,
                     lwd=lwd,
                     cex=cex,
                     fontsize=fontsize,
                     lineheight=lineheight,
                     font=font,
                     fontfamily=fontfamily,
                     fontface=fontface,
                     alpha=alpha,
                     lineend=lineend,
                     linejoin=linejoin,
                     linemitre=linemitre,
                     lex=lex,
                     pch=pch,
                     size=size,
                     addArrow=addArrow,
                     angleArrow=angleArrow,
                     lengthArrow=lengthArrow,
                     endsArrow=endsArrow,
                     typeArrow=typeArrow,
                     just=just,
                     hjust=hjust,
                     vjust=vjust,
                     rot=rot,
                     check.overlap=check.overlap,
                     clip=clip,
                     lighten=lighten,
                     special=special,

                    
                     connect=connect,
                     type=type,
                     baselinedefault=baselinedefault,
                     shadowbox=shadowbox,
                     arrowsout=arrowsout,
                     entryheight=entryheight,
                     subentryheight=subentryheight,
                     sublabelsfontsize=sublabelsfontsize,
                     mirrorlabels=mirrorlabels,
                     group=group,
                     shadowrow=shadowrow,
                     spaceAbove=spaceAbove,
                     spaceBelow=spaceBelow
                     )
    
    defaults

}
    


