getplotnums <- function(rc,num,tc) {  # rc is row, col, or RxC

    plotmatrix <- matrix(c(1:tc$nplots,rep(NA,prod(tc$RxC)))[1:prod(tc$RxC)],
                         nrow=tc$RxC[1],ncol=tc$RxC[2],byrow=TRUE)
    
    if ((rc=="row")||(rc=="rows")) {
        plots <- na.omit(plotmatrix[num,])
    }
    if ((rc=="col")||(rc=="column")||(rc=="columns")) {
        plots <- na.omit(plotmatrix[,num])
    }
    if (rc=="RxC") {
        plots <- na.omit(plotmatrix[num[1],num[2]])
    }
    
    plots
}
