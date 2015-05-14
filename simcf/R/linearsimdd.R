#adding inputs for scens to be differenced

#' @export
linearsimdd <- function (x, xd, b, ci = 0.95, constant = 1, xpre = NULL, xdpre=NULL) 
{
    if (any(class(x) == "counterfactual") && !is.null(x$model)) {
        xpre <- model.matrix(x$model, x$xpre)
        x <- model.matrix(x$model, x$x)
    }
    else {
        if (any(class(x) == "list")) 
            x <- x$x
        if (any(class(x) == "list")) 
            xpre <- x$xpre
        if (is.data.frame(x)) 
            x <- as.matrix(x)
        if (is.data.frame(xpre)) 
            x <- as.matrix(xpre)
        if (!is.matrix(x)) {
            if (is.matrix(b)) {
                x <- t(x)
                if (!is.na(constant)) {
                  x <- append(x, 1, constant - 1)
                }
            }
            else {
                x <- as.matrix(x)
                if (!is.na(constant)) {
                  x <- appendmatrix(x, rep(1, nrow(x)), constant)
                }
            }
        }
        else {
            if (!is.na(constant)) {
                x <- appendmatrix(x, rep(1, nrow(x)), constant)
            }
        }
        if (!is.matrix(xpre)) {
            if (is.matrix(b)) {
                xpre <- t(xpre)
                if (!is.na(constant)) {
                  xpre <- append(xpre, 1, constant - 1)
                }
            }
            else {
                xpre <- as.matrix(xpre)
                if (!is.na(constant)) {
                  xpre <- appendmatrix(xpre, rep(1, nrow(xpre)), 
                    constant)
                }
            }
        }
        else {
            if (!is.na(constant)) {
                xpre <- appendmatrix(xpre, rep(1, nrow(xpre)), 
                  constant)
            }
        }
    }
    
    
################
    # Repeating for the second scenario (to be differenced)
################
    
    if (any(class(xd) == "counterfactual") && !is.null(xd$model)) {
        xdpre <- model.matrix(xd$model, xd$xpre)
        xd <- model.matrix(xd$model, xd$x)
    }
    else {
        if (any(class(x) == "list")) 
            xd <- xd$x
        if (any(class(x) == "list")) 
            xdpre <- xd$xdpre
        if (is.data.frame(x)) 
            xd <- as.matrix(xd)
        if (is.data.frame(xdpre)) 
            xd <- as.matrix(xdpre)
        if (!is.matrix(xd)) {
            if (is.matrix(b)) {
                xd <- t(xd)
                if (!is.na(constant)) {
                  xd <- append(xd, 1, constant - 1)
                }
            }
            else {
                xd <- as.matrix(xd)
                if (!is.na(constant)) {
                  xd <- appendmatrix(xd, rep(1, nrow(xd)), constant)
                }
            }
        }
        else {
            if (!is.na(constant)) {
                xd <- appendmatrix(xd, rep(1, nrow(xd)), constant)
            }
        }
        if (!is.matrix(xdpre)) {
            if (is.matrix(b)) {
                xdpre <- t(xdpre)
                if (!is.na(constant)) {
                  xdpre <- append(xdpre, 1, constant - 1)
                }
            }
            else {
                xdpre <- as.matrix(xdpre)
                if (!is.na(constant)) {
                  xdpre <- appendmatrix(xdpre, rep(1, nrow(xdpre)), 
                    constant)
                }
            }
        }
        else {
            if (!is.na(constant)) {
                xdpre <- appendmatrix(xdpre, rep(1, nrow(xdpre)), 
                  constant)
            }
        }
    }
    
    esims <- nrow(as.matrix(b))
    nscen <- nrow(x)
    nci <- length(ci)
    res <- list(pe = rep(NA, nscen), lower = matrix(NA, nrow = nscen, 
        ncol = nci), upper = matrix(NA, nrow = nscen, ncol = nci))
    
#################
    # inserting differencing differences part into simulations: simulated ys produced are difs
#################
    
    for (i in 1:nscen) {
        simmu1 <- b %*% xpre[i, ]
        simmu2 <- b %*% x[i, ]
        simmud1 <- b %*% xdpre[i,] 
        simmud2 <- b %*% xd[i,]
        simy <- (simmu2 - simmu1) - (simmud2 - simmud1)
        res$pe[i] <- mean(simy)
        for (k in 1:nci) {
            cint <- quantile(simy, probs = c((1 - ci[k])/2, (1 - 
                (1 - ci[k])/2)))
            res$lower[i, k] <- cint[1]
            res$upper[i, k] <- cint[2]
        }
    }
    res$lower <- drop(res$lower)
    res$upper <- drop(res$upper)
    res
}
