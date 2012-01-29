
    adjlinebreaks <- function(height,label) {
        a <- 1+ sum(gregexpr("\n",label,fixed=TRUE)[[1]]>0)
        height*(2*a-1)
    }
