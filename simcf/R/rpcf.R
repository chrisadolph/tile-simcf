rpcf <- function(x,   # Data matrix
                 c,   # Column(s) to adjust
                 delta=1,  # Adjustment
                 scale="unit" # or "sd"
                 ) {

    if (!any(class(x)=="matrix")) x <- matrix(x,ncol=length(x))
    y <- apply(x,2,mean,na.rm=TRUE)
    xc <- y[c]
    if (identical(scale,"sd")) {
        delta <- delta*apply(x[,c,drop=FALSE],2,sd,na.rm=TRUE)
    }
    xcd <- xc + delta
    if (sum(xcd<0) + sum(xcd>1) + (sum(xcd)>1)) {
        stop("Error in rpcf:  hypothetical components are logically impossible (exceed [0,1] bounds).")
    }
    y <- y*(1 - sum(delta)/(1 - sum(xc)))
    y[c] <- xcd
    y
}
