tilefindplotarea <- function(tc) {

    tc$currrow <- ceiling(tc$iplot/tc$RxC[2])
    tc$currcol <- (tc$iplot%%tc$RxC[2]) + as.numeric((tc$iplot%%tc$RxC[2])==0)*tc$RxC[2]
    if ((tc$iplot %% tc$RxC[2])==1)
        tc$newrow <- TRUE
    else
        tc$newrow <- FALSE

    if (tc$iplot<=tc$RxC[2])
        tc$newcol <- TRUE
    else
        tc$newcol <- FALSE


    tc$curplotcol <- 1 + tc$rowtitle$add  # 1 is left border
    for (i in 1:(tc$currcol)) {
        currplot <- getplotnums("column",i,tc)
        if (i<tc$currcol) {
            tc$curplotcol <- (tc$curplotcol
                              + any(tc$yaxistitle$add[currplot])
                              + any(tc$yaxis$add[currplot])
                              + 1  # the graphic
                              + any(tc$rightaxis$add[currplot])
                              + any(tc$rightaxistitle$add[currplot])
                              + 1  # a spacer
                              )
        } else {
            tc$curplotcol <- (tc$curplotcol
                              + any(tc$yaxistitle$add[currplot])
                              + any(tc$yaxis$add[currplot])
                              + 1  # the graphic
                              )

        }     
    }
    
    tc$curplotrow <- 1 + tc$maintitle$add + tc$columntitle$add  # 1 is top border
    for (i in 1:(tc$currrow)) {
        currplot <- getplotnums("row",i,tc)
        if (i<tc$currrow) {
            tc$curplotrow <- (tc$curplotrow
                              + any(tc$plottitle$add[currplot])
                              + any(tc$topaxistitle$add[currplot])
                              + any(tc$topaxis$add[currplot])
                              + 1  # the graphic
                              + any(tc$xaxis$add[currplot])
                              + any(tc$xaxistitle$add[currplot])
                              + any(tc$undertitle$add[currplot])
                              + 1  # a spacer
                              )
        } else {
            tc$curplotrow <- (tc$curplotrow
                              + any(tc$plottitle$add[currplot])
                              + any(tc$topaxistitle$add[currplot])
                              + any(tc$topaxis$add[currplot])
                              + 1  # the graphic
                              )

        }     
    }

  
    tc$currrowplots <- getplotnums("row",tc$currrow,tc)
   
    tc$currcolplots <- getplotnums("column",tc$currcol,tc)
    
    tc
}
