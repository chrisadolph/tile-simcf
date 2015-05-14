#' Create a set of counterfactuals using a factorial design
#' 
#' Converts multiple sets of hypothetical covariate values into a single
#' dataframe with all possible combinations of covariate values
#' 
#' Useful for creating counterfactuals to input into the post-estimation
#' simulation functions of the \code{simcf} package or into \code{whatif} in
#' the \code{WhatIf} package.
#' 
#' @param \dots vectors of hypothetical values for each of several covariates.
#' \bold{Must} be provided in \code{tag} = \code{value} format, where
#' \code{tag} matches the name of the original (ie, factual) data
#' @param formula forumla object, the model estimated
#' @param data dataframe, the actual data used to estimate the original model.
#' Any covariates not specified in \dots{} will be assigned baseline values
#' (e.g., their means)
#' @param names string or string vector, the name(s) of the scenario(s), e.g.,
#' to label them on plots or tables; saved as the row name of the appropriate
#' scenario
#' @param hull logical, check for counterfactuals outside the convex hull using
#' the WhatIf package
#' @param f function, applied to covariates not specified in \dots{} to
#' complete the counterfactuals; default is to take the mean of each covariate
#' @return A list with at least some of the following components: \item{x}{A
#' dataframe with columns corresponding to the provided variables, and rows
#' containing every possible combination of the given covariate values, plus
#' additional columns with the mean (or other summary value) of the remaining
#' covariates in the model formula, if any} \item{xpre}{dataframe, as x}
#' \item{formula}{formula object, the model provided by user}
#' \item{extrapolatex}{vector, whether the x scenarios lie outside the convex
#' hull (TRUE indicates extrapolation)} \item{extrapolatexpre}{vector, whether
#' the xpre scenarios lie outside the convex hull (TRUE indicates
#' extrapolation)} \item{extrapolatefd}{vector, whether either the x or xpre
#' scenarios lie outside the convex hull (TRUE indicates extrapolation)}
#' @author Christopher Adolph <\email{cadolph@@u.washington.edu}>
#' @keywords design manip
#' @examples
#' 
#' 
#' # Simple example
#' xhyp <- c(1,2,3)
#' whyp <- c(TRUE, FALSE)
#' zhyp <- c("a","b","c")
#' scenarios <- cfFactorial(x = xhyp, w = whyp, z = zhyp)
#' print(scenarios$x)
#' 
#' # Complex example
#' 
#' 
#' # Multinomial Logistic Regression of alligator food;
#' # Tiled lineplots using factorial design of counterfactuals,
#' # with extrapolation outside the convex hull flagged
#' #
#' # For an alternative method, using cfMake and cfChange instead of cfFactorial,
#' # see help(lineplot) in the tile package
#' 
#' data(gator)
#' require(MASS)
#' require(nnet)
#' require(tile)
#' require(WhatIf)
#' 
#' # Estimate MNL using the nnet library
#' mlogit.result <- multinom(food ~ size + female, Hess=TRUE)
#' pe <- mlogit.result$wts[c(6,7,8,10,11,12)]
#'                                       # point estimates
#' vc <- solve(mlogit.result$Hess)       # var-cov matrix
#' 
#' # Simulate parameters from predictive distributions
#' sims <- 10000
#' simbetas <- mvrnorm(sims,pe,vc)       # draw parameters, using MASS::mvrnorm
#' simb <- array(NA, dim = c(sims,3,2))  # re-arrange simulates to array format
#' simb[,,1] <- simbetas[,1:3]           #   for MNL simulation
#' simb[,,2] <- simbetas[,4:6]
#' 
#' # Create full factorial set of counterfactuals
#' sizerange <- seq(1,4,by=0.1)          # range of counterfactual sizes
#' femalerange <- c(0,1)                 # range of counterfactual sexes
#' xhyp <- cfFactorial(size = sizerange, female = femalerange)
#'                                       
#' # Simulate expected probabilities
#' mlogit.qoi1 <- mlogitsimev(xhyp,simb,ci=0.67)
#' 
#' # Create one trace for each predicted category of the response, and each sex
#' trace1 <- lineplot(x=xhyp$x$size[xhyp$x$female==0],
#'                    y=mlogit.qoi1$pe[xhyp$x$female==0,1],
#'                    lower=mlogit.qoi1$lower[xhyp$x$female==0,1,],
#'                    upper=mlogit.qoi1$upper[xhyp$x$female==0,1,],
#'                    ci=list(mark="shaded"),
#'                    extrapolate=list(data=cbind(size,female),
#'                                     cfact=xhyp$x[xhyp$x$female==0,],
#'                                     omit.extrapolated=FALSE),
#'                    col="blue",
#'                    plot=1
#'                    )
#' 
#' trace2 <- lineplot(x=xhyp$x$size[xhyp$x$female==0],
#'                    y=mlogit.qoi1$pe[xhyp$x$female==0,2],
#'                    lower=mlogit.qoi1$lower[xhyp$x$female==0,2,],
#'                    upper=mlogit.qoi1$upper[xhyp$x$female==0,2,],
#'                    ci=list(mark="shaded"),
#'                    extrapolate=list(data=cbind(size,female),
#'                                     cfact=xhyp$x[xhyp$x$female==0,],
#'                                     omit.extrapolated=FALSE),
#'                    col="red",
#'                    plot=1
#'                    )
#' 
#' trace3 <- lineplot(x=xhyp$x$size[xhyp$x$female==0],
#'                    y=mlogit.qoi1$pe[xhyp$x$female==0,3],
#'                    lower=mlogit.qoi1$lower[xhyp$x$female==0,3,],
#'                    upper=mlogit.qoi1$upper[xhyp$x$female==0,3,],
#'                    ci=list(mark="shaded"),
#'                    extrapolate=list(data=cbind(size,female),
#'                                     cfact=xhyp$x[xhyp$x$female==0,],
#'                                     omit.extrapolated=FALSE),
#'                    col="green",
#'                    plot=1
#'                    )
#' 
#' trace4 <- lineplot(x=xhyp$x$size[xhyp$x$female==1],
#'                    y=mlogit.qoi1$pe[xhyp$x$female==1,1],
#'                    lower=mlogit.qoi1$lower[xhyp$x$female==1,1,],
#'                    upper=mlogit.qoi1$upper[xhyp$x$female==1,1,],
#'                    ci=list(mark="shaded"),
#'                    extrapolate=list(data=cbind(size,female),
#'                                     cfact=xhyp$x[xhyp$x$female==1,],
#'                                     omit.extrapolated=FALSE),
#'                    col="blue",
#'                    plot=2
#'                    )
#' 
#' trace5 <- lineplot(x=xhyp$x$size[xhyp$x$female==1],
#'                    y=mlogit.qoi1$pe[xhyp$x$female==1,2],
#'                    lower=mlogit.qoi1$lower[xhyp$x$female==1,2,],
#'                    upper=mlogit.qoi1$upper[xhyp$x$female==1,2,],
#'                    ci=list(mark="shaded"),
#'                    extrapolate=list(data=cbind(size,female),
#'                                     cfact=xhyp$x[xhyp$x$female==1,],
#'                                     omit.extrapolated=FALSE),
#'                    col="red",
#'                    plot=2
#'                    )
#' 
#' trace6 <- lineplot(x=xhyp$x$size[xhyp$x$female==1],
#'                    y=mlogit.qoi1$pe[xhyp$x$female==1,3],
#'                    lower=mlogit.qoi1$lower[xhyp$x$female==1,3,],
#'                    upper=mlogit.qoi1$upper[xhyp$x$female==1,3,],
#'                    ci=list(mark="shaded"),
#'                    extrapolate=list(data=cbind(size,female),
#'                                     cfact=xhyp$x[xhyp$x$female==1,],
#'                                     omit.extrapolated=FALSE),
#'                    col="green",
#'                    plot=2
#'                    )
#' 
#' linelabels <- textTile(labels=c("Invertebrates",
#'                                  "Fish",
#'                                  "Other"),
#'                          x=  c(1.75,      3,         3),
#'                          y=  c(0.95,     0.95,      0.375),
#'                          col=c("blue", "green", "red"),
#'                          cex = 0.75,
#'                          plot=c(1,2)
#'                          )
#' 
#' at.x <- c(1,2,3,4)
#' at.y <- c(0,0.2,0.4,0.6,0.8,1)
#' 
#' # Plot traces using tile
#' tile(trace1,
#'      trace2,
#'      trace3,
#'      trace4,
#'      trace5,
#'      trace6,
#'      linelabels,
#'      RxC = c(1,2),
#'      limits = c(1,4,0,1),
#'      #output = list(outfile="gator2cf", width=5),
#'      xaxis = list(at=at.x),
#'      yaxis = list(at=at.y),
#'      xaxistitle = list(labels="Size of alligator"),
#'      yaxistitle = list(type="first",labels="Pr(Food preference)"),
#'      undertitle = list(labels=c("Male","Female")),
#'      maintitle = list(labels="Food choice by alligator size and sex", x=0.57),
#'      width = list(yaxistitle=3),
#'      height = list(maintitle=5, undertitle=3),
#'      gridlines = list(type="xy"),
#'      frame=TRUE
#'      )
#' 
#' 
#' @export
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

#' Create or modify a set of counterfactuals
#' 
#' Functions for creating and modifying sets of counterfactuals, usable as
#' input for post-estimation prediction
#' 
#' Useful for creating counterfactuals to input into the post-estimation
#' simulation functions of the \code{simcf} package (e.g., for plotting with
#' the \code{tile} package) or into \code{whatif} in the \code{WhatIf} package.
#' 
#' @param formula forumla object, the model estimated.  Compatible with
#' \code{logBound()} and \code{logitBound()}.
#' @param data matrix or dataframe, the actual data used to estimate the
#' original model
#' @param nscen scalar, number of counterfactual scenarios to initialize
#' @param names string or string vector, the name(s) of the scenario(s), e.g.,
#' to label them on plots or tables; saved as the row name of the appropriate
#' scenario
#' @param f function, applied to columns of covariate data to produce the
#' baseline counterfactual; default is to take the mean of each covariate
#' @param \dots additional inputs to \code{f}
#' @param xscen list object, containing an pre-initialized set of
#' counterfactuals, as output by \code{cfMake} or modified by \code{cfChange}
#' or \code{cfName}
#' @param x scalar, a hypothetical value of a covariate
#' @param covname string or scalar, the name or number of a column (covariate)
#' in a counterfactual dataset which the user wishes to modify to the values
#' given in \code{x} or \code{xpre}
#' @param xpre scalar, a hypothetical initial value of a covariate, for
#' calculating first differences
#' @param scen scalar or string, the row (number or name) of the scenario to be
#' modified
#' @return A list object containing the following components: \item{x}{A
#' dataframe of counterfactuals, with \code{nscen} rows and a column for each
#' column in \code{data}} \item{xpre}{A dataframe of initial counterfactuals
#' for constructing first differences, with \code{nscen} rows and a column for
#' each column in \code{data}} \item{model}{The model formula provided by the
#' user (optional)}
#' @author Christopher Adolph <\email{cadolph@@u.washington.edu}>
#' @keywords design manip
#' @examples
#' 
#' # Linear, Poisson, and Negative Binomial regression using UScrime data
#' 
#' # Uses simcf and ropeladders to show how the expected crime rate varies 
#' # in response to changes in 7 covariates under each of four estimation 
#' # methods.  
#' 
#' # See example for ropeladder in package tile for more plots
#' 
#' # Load data and libraries; set up specification
#' require(tile)
#' require(MASS)
#' data(UScrime)
#' model <- (y ~ log(M) + So + log(Ed) + log(Po1) + log(Po2)
#'               + log(LF) + log(M.F) + log(Pop) + log(NW) +log(U1)
#'               + log(U2) + log(GDP) + log(Ineq) + log(Prob) +
#'               log(Time))
#' 
#' # Estimate Linear regression model
#' lm1.res <- lm(model, data = UScrime)
#' lm1.pe <- lm1.res$coefficients        # point estimates
#' lm1.vc <- vcov(lm1.res)               # var-cov matrix
#' 
#' # Estimate Robust and resistant regression model
#' mm1.res <- rlm(model, data = UScrime, method="MM")
#' mm1.pe <- mm1.res$coefficients        # point estimates
#' mm1.vc <- vcov(mm1.res)               # var-cov matrix
#' 
#' # Estimate Poisson model
#' po1.res <- glm(model, family=poisson, data = UScrime)
#' po1.pe <- po1.res$coefficients         # point estimates
#' po1.vc <- vcov(po1.res)                # var-cov matrix
#' 
#' # Estimate Negative Binomial model
#' nb1.res <- glm.nb(model, data = UScrime)
#' nb1.pe <- nb1.res$coefficients         # point estimates
#' nb1.vc <- vcov(nb1.res)                # var-cov matrix
#' 
#' # Use the simcf package to simulate first differences in crime rate
#' # given changes in each of the seven covariates
#' 
#' # Initialize 7 different scenarios to the mean values of the covariates
#' xscen <- cfMake(model, data=UScrime, nscen=7)
#' 
#' # Configure scenario 1:  Raise Probability of Imprisonment by 1/2 standard deviation
#' xscen <- cfName(xscen, "Pr(Prison) +0.5 sd", scen=1)
#' xscen <- cfChange(xscen, "Prob", x = mean(UScrime$Prob) +
#' 0.5*sd(UScrime$Prob), scen=1)
#' 
#' # Configure scenario 2:  Raise Police Spending by 1/2 standard deviation
#' xscen <- cfName(xscen, "Police Spending +0.5 sd", scen=2)
#' xscen <- cfChange(xscen, "Po1", x = mean(UScrime$Po1) + 0.5*sd(UScrime$Po1),
#' scen=2)
#' 
#' # Configure scenario 3:  Raise Unemployment (Age 35-39)  by 1/2 standard deviation
#' xscen <- cfName(xscen, "Unemployment (t-2) +0.5 sd", scen=3)
#' xscen <- cfChange(xscen, "U2", x = mean(UScrime$U2) + 0.5*sd(UScrime$U2),
#' scen=3)
#' 
#' # Configure scenario 4:  Raise Non-white population by 1/2 standard deviation
#' xscen <- cfName(xscen, "Non-White Pop +0.5 sd", scen=4)
#' xscen <- cfChange(xscen, "NW", x = mean(UScrime$NW) + 0.5*sd(UScrime$NW), scen=4)
#' 
#' # Configure scenario 5:  Raise Male Pop by 1/2 standard deviation
#' xscen <- cfName(xscen, "Male Pop +0.5 sd", scen=5)
#' xscen <- cfChange(xscen, "M", x = mean(UScrime$M) + 0.5*sd(UScrime$M), scen=5)
#' 
#' # Configure scenario 6:  Raise Education by 1/2 standard deviation
#' xscen <- cfName(xscen, "Education +0.5 sd", scen=6)
#' xscen <- cfChange(xscen, "Ed", x = mean(UScrime$Ed) + 0.5*sd(UScrime$Ed), scen=6)
#' 
#' # Configure scenario 7:  Raise Inequality by 1/2 standard deviation
#' xscen <- cfName(xscen, "Inequality +0.5 sd", scen=7)
#' xscen <- cfChange(xscen, "Ineq", x = mean(UScrime$Ineq) +
#' 0.5*sd(UScrime$Ineq), scen=7)
#' 
#' # Simulate conditional expectations for these counterfactuals
#' sims <- 10000
#' 
#' # Linear regression simulations
#' simbetas.lm <- mvrnorm(sims, lm1.pe, lm1.vc)       # draw parameters, using MASS::mvrnorm
#' lm1.qoi <- linearsimfd(xscen, simbetas.lm, ci=0.95)
#' 
#' # Robust regression simulations
#' simbetas.mm <- mvrnorm(sims, mm1.pe, mm1.vc)       # draw parameters, using MASS::mvrnorm
#' mm1.qoi <- linearsimfd(xscen, simbetas.mm, ci=0.95)
#' 
#' # Poisson simulations
#' simbetas.po <- mvrnorm(sims, po1.pe, po1.vc)       # draw parameters, using MASS::mvrnorm
#' po1.qoi <- loglinsimfd(xscen, simbetas.po, ci=0.95)
#' 
#' # Negative Binomial simulations
#' simbetas.nb <- mvrnorm(sims, nb1.pe, nb1.vc)       # draw parameters, using MASS::mvrnorm
#' nb1.qoi <- loglinsimfd(xscen, simbetas.nb, ci=0.95)
#' 
#' # Create ropeladder traces of first differences from each model
#' trace1 <- ropeladder(x=lm1.qoi$pe,
#'                      lower=lm1.qoi$lower,
#'                      upper=lm1.qoi$upper,
#'                      labels=row.names(xscen$x),
#'                      #extrapolate=list(model, data=UScrime, cfact=xscen$x),
#'                      plot=1
#'                      )
#' 
#' trace2 <- ropeladder(x=mm1.qoi$pe,
#'                      lower=mm1.qoi$lower,
#'                      upper=mm1.qoi$upper,
#'                      plot=2
#'                      )
#' 
#' trace3 <- ropeladder(x=po1.qoi$pe,
#'                      lower=po1.qoi$lower,
#'                      upper=po1.qoi$upper,                   
#'                      plot=3
#'                      )
#' 
#' trace4 <- ropeladder(x=nb1.qoi$pe,
#'                      lower=nb1.qoi$lower,
#'                      upper=nb1.qoi$upper,
#'                      plot=4
#'                      )
#' 
#' rug1 <- rugTile(x = UScrime$y - mean(UScrime$y),
#'                 plot = 1:4
#'                 )
#'                 
#' vertmark <- linesTile(x = c(0,0),
#'                       y = c(0,1),
#'                       lty = "solid",
#'                       plot = 1:4
#'                       )
#' 
#' # Create plot
#' tc <- tile(trace1, trace2, trace3, trace4,
#'            rug1, vertmark,
#'            #output = list(file = "ropeladderEx1"),          
#'            xaxistitle = list(labels="E(crime rate per 100,000)"),
#'            topaxis= list(at = mean(UScrime$y)*c(0.5, 1, 1.5, 2) - mean(UScrime$y),
#'                          labels = c("0.5x","1x","1.5x","2x"),
#'                          add = rep(TRUE,4)
#'                          ),
#'            topaxistitle = list(labels="E(crime rate) / average"),
#'            plottitle = list(labels1 = "Linear",
#'                             labels2 = "Robust",
#'                             labels3 = "Poisson",
#'                             labels4 = "Neg Bin"),
#'            gridlines=list(type="t")
#'            )
#' 
#' 
#' @rdname cfMake
#' @name cfMake
#' @export
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

#' @rdname cfMake
#' @export
cfChange <- function(xscen,covname,x=NULL,xpre=NULL,scen=1) {
    if (!is.null(x))
        xscen$x[scen,covname] <- x
    if (!is.null(xpre))
        xscen$xpre[scen,covname] <- xpre
    if (!is.null(xscen$extrapolatex)) {
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

#' @rdname cfMake
#' @export
cfName <- function(xscen,name,scen=1) {
    if (!is.null(xscen$x))
        row.names(xscen$x)[scen] <- name
    if (!is.null(xscen$xpre))
        row.names(xscen$xpre)[scen] <- name
    xscen
}


#' Ratio-preserving counterfactuals
#' 
#' Change one element of a vector with a fixed sum without changing the sum or
#' the ratios of the other elements
#' 
#' XXX
#' 
#' @param x n x p matrix of n compositions with p categories each.  All rows of
#' x must sum to the \code{constraint}.
#' @param c k vector of the numbers of columns to adjust by delta.  k must be
#' less than p, the number of columns in x.  The requested adjustments must be
#' logically possible; ie, respect the \code{constraint}.
#' @param delta scalar or k vector of adjustments to make to the columns of
#' listed in \code{c}.  The requested adjustments must be logically possible;
#' ie, respect the \code{constraint}. The default is 1, which often will be an
#' infeasibly large change.
#' @param constraint scalar or k vector of logically required sums for each
#' composition.  The default is 1.
#' @param scale character, whether the adjustments in requested in \code{delta}
#' are in units of x (\code{unit}, the default) or standard deviations of x
#' (\code{sd}).
#' @author Christopher Adolph <\email{cadolph@@u.washington.edu}>
#' @keywords design manip
#' @export 
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




#' Extract from a dataframe all variables used in a formula
#' 
#' Extracts from a dataframe all variables used in a formula, saves these
#' variables to a new dataframe, adds additional variables if requested, and
#' listwise deletes if desired.  Useful for creating a fully observed dataset
#' without deleting on unused variables.
#' 
#' 
#' @param formula An formula object.
#' @param data A dataframe containing all the variables used in \code{formula}.
#' @param extra Either a dataframe with additional variables to add to the
#' output, or a formula object containing additional variables to extract from
#' \code{data}.
#' @param na.rm If \code{TRUE}, listwise delete the newly created dataframe.
#' Default is \code{FALSE}.
#' @return A data frame.
#' @author Christopher Adolph <\email{cadolph@@u.washington.edu}>
#' @keywords manip
#' @export
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







#' Log transform a variable, creating a dummy variable to record out of bound
#' cases
#' 
#' If given a variable to log transform which contains values less than or
#' equal to 0, logBound creates a dummy variable indicating the out of bound
#' cases, and collects them in a matrix with the log transformed variable.
#' 
#' Computes the log transformation, log(x).  Instead of reporting out of bounds
#' cases as -Inf or NaN, reports out of bounds cases as 0s, but includes a
#' dummy variable indicating the lower boundary is breeched.  Including the
#' output of logBound on the right-hand side of a regression model allows the
#' user to log transform even variables which are exactly 0 by treating those
#' cases as sui generis exceptions.  Analogous to treating x as a hurdle
#' variable.
#' 
#' LogBound transformed variables will work in the other functions in the simcf
#' package, but may fail when used in predict commands.
#' 
#' @param x A vector of data to be log tranformed.  Need not be restricted to
#' the interval (0,Inf).
#' @param base The logarithmic base for transformation
#' @param forceAny Include a column of dummies for cases at or below the zero
#' bound whether or not such cases are present in x.
#' @return A matrix including up to two columns.  The last is always the log
#' transformed x, with 0s for any case outside the (0,Inf) bounds.  The
#' optional initial column is 'any', which is 1 if the corresponding element of
#' x is greater than 0.
#' @author Christopher Adolph <\email{cadolph@@u.washington.edu}>
#' @seealso \code{\link{logitBound}}
#' @keywords manip
#' @export
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





#' Logit transform a variable, creating dummy variables to record out of bound
#' cases
#' 
#' If given a variable to logit transform which contains values less than or
#' equal to 0 or greater than or equal to 1, logitBound creates dummy variables
#' indicating the out of bound cases, and collects them in a matrix with the
#' logit transformed variable.
#' 
#' Computes the logit transformation, ln(x/(1-x).  Instead of reporting out of
#' bounds cases as -Inf or Inf, reports out of bounds cases as 0s, but includes
#' dummy variables indicating which boundary is breeched.  Including the output
#' of logitBound on the right-hand side of a regression model allows the user
#' to logit transform even variables which are exactly 0 or 1 by treating those
#' cases as sui generis exceptions.  Analogous to treating x as a hurdle
#' variable.
#' 
#' LogitBound transformed variables will work in the other functions in the
#' simcf package, but may fail when used in predict commands.
#' 
#' @param x A vector of data to be logit tranformed.  Need not be restricted to
#' the interval (0,1).
#' @param base The logarithmic base for transformation
#' @param forceAny Include a column of dummies for cases at or below the zero
#' bound whether or not such cases are present in x.
#' @param forceAll Include a column of dummies for cases at or above the unity
#' bound whether or not such cases are present in x.
#' @return A matrix including up to three columns.  The last is always the
#' logit transformed x, with 0s for any case outside the (0,1) bound.  The
#' optional initial columns are 'any', which is 1 if the corresponding element
#' of x is greater than 0, and 'all', which is 1 if the corresponding element
#' of x is equal to or greater than 1.
#' @author Christopher Adolph <\email{cadolph@@u.washington.edu}>
#' @seealso \code{\link{logBound}}
#' @keywords manip
#' @export
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





#' Paste without separators
#' 
#' Paste without separators
#' 
#' Calls paste with sep="".
#' 
#' @param ... character strings to be pasted together, or numeric data to be
#' coerced to character
#' @author Christopher Adolph <\email{cadolph@@uw.edu}>
#' @keywords manip
#' @export
glue <- function (...){paste(...,sep="")}
