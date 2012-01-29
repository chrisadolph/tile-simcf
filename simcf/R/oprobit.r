
# Likelihood for ordered probit
llk.oprobit <- function(param, x, y) {
  # preliminaries
    x <- as.matrix(x)
    os <- rep(1, nrow(x))
    x <- cbind(os, x)  
    b <- param[1:ncol(x)]
    tau <- c(0, param[(ncol(x)+1) : length(param)])
    max.y <- max(y)
  
    # probabilities and penalty function
    xb <- x%*%b
    prob <- matrix(NA, nrow=length(os), ncol=max.y)
    prob[,1] <- log(pnorm(-xb))
  
    for (i in 2:(max.y-1)) {
      if (tau[i] <= tau[i-1]) prob[,i] <- -(abs(tau[i])*1000000)    # penalty function
      else prob[,i] <- log( pnorm(tau[i]-xb) - pnorm(tau[i-1]-xb)     )
    }
  
    prob[,max.y] <- log(1-pnorm(tau[max.y-1]-xb)) 

    # bind y's
    ybinary <- NULL
    for (i in 1:max.y)
      ybinary <- cbind(ybinary,y==i)
    
    # -1 * log likelihood (optim is a minimizer)  
    -sum(ybinary * prob)
  }
