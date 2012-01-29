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

cfMake <- function(formula=NULL,data,nscen=1,names=NULL,hull=FALSE,f="mean",...) {
    if (!is.null(formula)) {
        #resploc <- attr(terms(formula),"response")
        #data <- data[,all.vars(formula)[-resploc]]
        data <- data[,all.vars(formula)]
    }
    data <- na.omit(data)
    xmean <- apply(data,2,f,...)
    xscen <- list(x=NULL,xpre=NULL)
    xscen$x <- as.data.frame(matrix(data=xmean,nrow=nscen,ncol=ncol(data),byrow=TRUE))
    xscen$xpre <- xscen$x
    colnames(xscen$x) <- names(data)
    colnames(xscen$xpre) <- names(data)
    if (!is.null(names)) {
        row.names(xscen$x) <- names
        row.names(xscen$xpre) <- names
    }
    if (!is.null(formula)) {

      # Get terms attribute
      tl <- attributes(terms(formula))$term.labels
      
      # Loop over terms
      for (i in 1:length(tl)) {
        tlCur <- tl[i]
      
        # Check for logitBound transformations
        if (substr(tlCur,1,11)=="logitBound(") {
          # if found, check number of terms needed.
          varname <- substr(tlCur,start=12,stop=nchar(tlCur)-1)
          subform <- as.formula(paste("~",varname,"-1"))
          toLT <- as.vector(model.matrix(subform,data=data))
          testLT <- as.matrix(logitBound(toLT))
        
          # revise formula so logitBound() call includes "forceAny" and/or "forceAll" as needed
          if (any(colnames(testLT)=="any")) {
            tlCur <- paste(substr(tlCur,start=1,stop=nchar(tlCur)-1), ", forceAny=TRUE)",sep="")
          }
          if (any(colnames(testLT)=="all")) {
            print(testLT)
            tlCur <- paste(substr(tlCur,start=1,stop=nchar(tlCur)-1), ", forceAll=TRUE)",sep="")
          }
          tl[i] <- tlCur
          rhs <- paste(tl, collapse = " + ")
          newform <- as.formula(paste("lhs ~", rhs), env=.GlobalEnv)
          newform[[2L]] <- formula[[2L]]
          formula <- newform
        }    

        # Check for logBound transformations
        if (substr(tlCur,1,9)=="logBound(") {
          # if found, check number of terms needed.
          varname <- substr(tlCur,start=10,stop=nchar(tlCur)-1)
          subform <- as.formula(paste("~",varname,"-1"))          
          toLT <- as.vector(model.matrix(subform,data=data))
          testLT <- as.matrix(logitBound(toLT))
          # revise formula so logBound() call includes "forceAny" as needed
          if (any(colnames(testLT)=="any")) {
            tlCur <- paste(substr(tlCur,start=1,stop=nchar(tlCur)-1), ", forceAny=TRUE)",sep="")
          }
          tl[i] <- tlCur
          rhs <- paste(tl, collapse = " + ")
          newform <- as.formula(paste("lhs ~", rhs), env=.GlobalEnv)
          newform[[2L]] <- formula[[2L]]
          formula <- newform
        }
      }
      
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

cfChange <- function(xscen,covname,x=NULL,xpre=NULL,scen=1) {
    if (!is.null(x))
        xscen$x[scen,covname] <- x
    if (!is.null(xpre))
        xscen$xpre[scen,covname] <- xpre
    if (!is.null(xscen$extrapolatex)) {
        require(WhatIf)
        wi <- whatif(formula=xscen$model, data=xscen$data, cfact=xscen$x)
        xscen$extrapolatex <- !wi$in.hull
        wi <- whatif(formula=xscen$model, data=xscen$data, cfact=xscen$xpre)
        xscen$extrapolatexpre <- !wi$in.hull
        xscen$extrapolatefd <- xscen$extrapolatex|xscen$extrapolatexpre
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

cfName <- function(xscen,name,scen=1) {
    if (!is.null(xscen$x))
        row.names(xscen$x)[scen] <- name
    if (!is.null(xscen$xpre))
        row.names(xscen$xpre)[scen] <- name
    xscen
}


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


# Make a simcf function to do all this from forumla, etc)
extractdata <- function(formula, data, extra=NULL, na.rm = FALSE) {
  subdata <- get_all_vars(formula,data)
  if (!is.null(extra)) {
    if (is.data.frame(extra)) {
      subdata <- cbind(subdata,extra)
    } else {
      if (any(class(extra)=="formula")) {
        subdata <- cbind(subdata, get_all_vars(extra,data) )
      } else {
                                        # to implement
        stop("Extra must be a dataframe or formula object")
      }
    }
  }
  if (na.rm)
    subdata <- na.omit(subdata)
  subdata
}



logBound <- function(x, base=exp(1), forceAny=FALSE) {
  any <- as.numeric(x>0)
  log <- suppressWarnings(log(x,base=base))
  log[!any] <- 0
  if (forceAny|any(!any)) {
    return(cbind(any,log))
  } else {
    return(cbind(log))
  }
}

logitBound <- function(x, base=exp(1), forceAny=FALSE, forceAll=FALSE) {

  # Can't include below because of simulation use of logitBound()
  #if (length(unique(na.omit(x)))==2) stop("You cannot logit transform, or even logitBound transform, a binary variable")
  #if (length(unique(na.omit(x)))==1) stop("You cannot logit transform, or even logitBound transform, a constant")

  any <- as.numeric(x>0)
  all <- as.numeric(x>=1)
  logit <- suppressWarnings(log(x/(1-x),base=base))
  logit[(any==0)|(all==1)] <- 0
  if (forceAny||any(!any)) {
    if (forceAll|sum(all)) {
      return(cbind(any,all,logit))
    } else {
      return(cbind(any,logit))
    }
  } else {
    if (sum(all)) {
      return(cbind(all,logit))
    } else {
      return(cbind(logit))
    }
  }
}

glue <- function (...){paste(...,sep="")}
