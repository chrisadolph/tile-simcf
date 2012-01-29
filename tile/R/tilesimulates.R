# Compile simulated data into means and CIs for a given trace
tilesimulates <- function(trace) {
    
    # Check for simulates
    if (!is.null(trace$simulates)) {

        # Create holders for results
        for (i in 1:length(trace$simulates)) {
            assign(paste(trace$simulates[i],sep=""),
                   NULL)
            assign(paste(trace$simulates[i],"se",sep=""),
                   NULL)
            assign(paste(trace$simulates[i],"lower",sep=""),
                   NULL)
            assign(paste(trace$simulates[i],"upper",sep=""),
                   NULL)
        }

        # Create lists of factors and their levels
        factors <- ufactors <- flength <- NULL
        for (i in 1:length(trace$factors)) {
            if (!is.null(trace[[trace$factors[i]]])&&!is.na(unique(trace[[trace$factors[i]]]))) {
                assign(paste("unique",trace$factors[i],sep=""),
                       unique(trace[[trace$factors[i]]]))
                ufactors <- c(ufactors,paste("unique",trace$factors[i],sep=""))
                factors <- c(factors,trace$factors[i])               
                flength <- c(flength,length(get(ufactors[length(ufactors)])))

                # Create holder for results
                assign(trace$factors[i],
                       NULL)              
            }
        }
        
        # Loop over scenarios (factorial combinations)
        nfact <- length(factors)
        ctr <- rep(1,nfact)
        done <- FALSE
        while (!done) {

            # Loop over factors
            cond <- TRUE
            for (i in 1:nfact) {
                # Obtain rows of simulates relevant to current scenario
                cond <- cond&(trace[[factors[i]]]==get(ufactors[i])[ctr[i]])
            }
            
            # Loop over simulated variables; summarize
            outpe <- outse <- rep(NA,length(trace$simulates))
            outlow <- outup <- matrix(nrow=length(trace$simulates),ncol=length(trace$ci$levels))
            for (i in 1:length(trace$simulates)) {

                # Select relevant simulation data
                sy <- na.omit(trace[[trace$simulates[i]]][cond])
                lengthsy <- length(sy)

                havedata <- FALSE
                if (lengthsy>0) {

                    # Data is available for this scenario
                    havedata <- TRUE
                    
                    # Sort data for quantiles
                    sy <- sort(sy)
                
                    # Compact simulate data to requested CI's
                    lowsy <- upsy <- NULL
                    for (k in 1:length(trace$ci$levels)) {
                        lowsy <- c(lowsy,sy[trunc((1-trace$ci$levels[k])/2*lengthsy)])
                        upsy <- c(upsy,sy[trunc((1-(1-trace$ci$levels[k])/2)*lengthsy)])
                    }

                    
                    # Add results & factor levels to temporary variables
                    outpe[i] <- mean(sy)
                    outse[i] <- sd(sy)
                    outlow[i,] <- lowsy
                    outup[i,] <- upsy            
                }                
            }

            # Collect scenario data if available
            if (havedata) {
                for (i in 1:length(trace$simulates)) {
                    # Create temporary variable names
                    cvstr <- paste(trace$simulates[i],sep="")
                    cvsestr <- paste(trace$simulates[i],"se",sep="")
                    cvlowerstr <- paste(trace$simulates[i],"lower",sep="")
                    cvupperstr <- paste(trace$simulates[i],"upper",sep="")
                    
                    # Add results & factor levels to temporary variables
                    assign(cvstr,
                           c(get(cvstr),outpe[i]))
                    assign(cvsestr,
                           c(get(cvsestr),outse[i]))
                    assign(cvlowerstr,
                           rbind(get(cvlowerstr),outlow[i,]))
                    assign(cvupperstr,
                           rbind(get(cvupperstr),outup[i,]))                      
                }

                for (i in 1:nfact) {

                    # Add current levels of factors to temporary variables
                    assign(factors[i],
                           c(get(factors[i]),get(ufactors[i])[ctr[i]]))
                }
            }
            
        
            # Increment highest counter not at its max
            f1 <- nfact
            doneinner <- FALSE
            while (!doneinner) {
                
                # Check next highest factor & increment if possible
                if (ctr[f1]<flength[f1]) {
                    ctr[f1] <- ctr[f1] + 1
                    if (f1<nfact)
                        ctr[(f1+1):nfact] <- 1
                    doneinner <- TRUE                    
                } else {
                    # Decrement factor (inner loop)
                    f1 <- f1 - 1
                }
            
                # Reached end of scenarios?
                if (f1==0) {
                    done <- doneinner <- TRUE
                }
            }
            
        }

       
        # Replace original data with simulation summaries; store original ___ in ___Simulates
        for (i in 1:length(trace$simulates)) {
            #trace[[paste(trace$simulates[i],"Simulates",sep="")]] <- trace[[trace$simulates[i]]]
            trace[[trace$simulates[i]]] <- get(trace$simulates[i])
            trace[[paste(trace$simulates[i],"se",sep="")]] <- get(paste(trace$simulates[i],"se",sep=""))
            trace[[paste(trace$simulates[i],"lower",sep="")]] <- get(paste(trace$simulates[i],"lower",sep=""))
            trace[[paste(trace$simulates[i],"upper",sep="")]] <- get(paste(trace$simulates[i],"upper",sep=""))
        }
        for (i in 1:nfact) {
            #trace[[paste(factors[i],"Simulates",sep="")]] <- trace[[factors[i]]]
            trace[[factors[i]]] <- get(factors[i])
        }            
    }
    
    # Return trace 
    trace
}
