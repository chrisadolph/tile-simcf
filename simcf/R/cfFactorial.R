cfFactorial <- function(...,formula=NULL,data=NULL,names=NULL,hull=FALSE,f="mean") {
                  #,trim.exptrapolated=FALSE,data=NULL) {

    increment <- function(ctr,maxctr,nexttoincr) {
        if (all(ctr[1:nexttoincr]==maxctr[1:nexttoincr])) {
            nexttoincr <- nexttoincr + 1
            ctr[nexttoincr] <- ctr[nexttoincr] + 1
            for (j in 1:(nexttoincr-1)) ctr[j] <- 1
        } else {
            j <- 1
            incred <- FALSE
            while ((!incred)&&(j<nexttoincr)) {
                if (ctr[j]<maxctr[j]) {
                    ctr[j] <- ctr[j] + 1
                    incred <- TRUE
                    if (j>1) for (k in 1:(j-1)) ctr[k] <- 1
                }
                j <- j+1
            }
            if (!incred) {
                ctr[nexttoincr] <- ctr[nexttoincr]+1
                #if ((j-1)>1) for (k in 1:(nexttoincr-1)) ctr[k] <- 1
                if (nexttoincr>1) for (k in 1:(nexttoincr-1)) ctr[k] <- 1
            }
        }
        list(ctr=ctr,nexttoincr=nexttoincr)
    }
    
    factors <- list(...)
    vars <- names(factors)
    
    nfact <- length(factors)
    if (nfact>1) {
        ufact <- list()
        maxctr <- NULL
        for (i in 1:nfact) {
            ufact[[i]] <- sort(unique(factors[[i]]))
            maxctr <- c(maxctr,length(ufact[[i]]))
        }
        cfact <- as.data.frame(t(rep(NA,length(vars))))
        colnames(cfact) <- vars    
        ctr <- rep(1,nfact)
        nexttoincr <- 1
        done <- all(ctr==maxctr)
        while (!done) {
            newcfact <- NULL
            for (i in 1:nfact) newcfact <- c(newcfact, ufact[[i]][ctr[i]])        
            cfact <- rbind(cfact,newcfact)
            done <- all(ctr==maxctr)
            if (!done) {
                res <- increment(ctr,maxctr,nexttoincr)
                ctr <- res$ctr
                nexttoincr <- res$nexttoincr
            }
        }
        cfact <- cfact[2:nrow(cfact),]
    } else {
        cfact <- as.data.frame(factors[[1]])
        colnames(cfact) <- vars   
    }
        row.names(cfact) <- seq(1:nrow(cfact))
    if (!is.null(names)) {
        row.names(cfact)[1:length(names)] <- names
    }

    # Find all vars in data but not in cfact; apply f and repeat
    cfact2 <- NULL
    if (!is.null(data)) {
        if (!is.null(formula)) {
            sdata <- data[,all.vars(formula)]
        }
        svar <- colnames(sdata)
        unused <- setdiff(svar,vars)
        if (length(unused)>0) {
            sdata <- sdata[,unused]
            sdata <- na.omit(sdata)
            xmean <- apply(sdata,2,f)
            cfact2 <- as.data.frame(matrix(data=xmean,nrow=nrow(cfact),ncol=ncol(sdata),byrow=TRUE))
            colnames(cfact2) <- colnames(sdata)   
        }
    }

    # Construct list    
    xscen <- list(x=NULL,xpre=NULL)
    if (is.null(cfact2))
        xscen$x <- cfact
    else
        xscen$x <- cbind(cfact,cfact2)
    xscen$xpre <- xscen$x
    if (!is.null(formula)) {
        xscen$model <- formula
    }
    class(xscen) <- c("list","counterfactual")

    # Check for extrapolation
    if (hull&&(!is.null(formula))&&(!is.null(data))) {
        require(WhatIf)
        wi <- whatif(formula=formula, data=data, cfact=xscen$x)
        xscen$extrapolatex <- !wi$in.hull
        wi <- whatif(formula=formula, data=data, cfact=xscen$xpre)
        xscen$extrapolatexpre <- !wi$in.hull
        xscen$extrapolatefd <- xscen$extrapolatex|xscen$extrapolatexpre
        xscen$data <- data
        if (any(c(xscen$extrapolatex,xscen$extrapolatexpre,xscen$extrapolatefd)==FALSE)) {
            warning("Some counterfactuals involve extrapolation outside the convex hull")
            if (any(xscen$extrapolatex==FALSE)) {
                print(c("x scenarios:  ",row.names(x)[xscen$extrapolatex]))
            }
            if (any(xscen$extrapolatexpre==FALSE)) {
                print(c("xpre scenarios:  ",row.names(xpre)[xscen$extrapolatexpre]))
            }
            if (any(xscen$extrapolatefd==FALSE)) {
                print(c("first diff scenarios:  ",row.names(x)[xscen$extrapolatefd]))
            }
        }
    }
    xscen
}