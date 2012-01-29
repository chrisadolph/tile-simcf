"rbindfill" <-
function(...) {
    x <- list(...)
    rx <- rep(0,length(x))
    for (i in 1:length(x)) {
        if (!is.matrix(x[[i]])) {
            x[[i]] <- t(as.matrix(x[[i]]))
            rx[i] <- ncol(as.matrix(x[[i]]))
        }
    }
    mx <- max(rx)
    y <- NULL
    for (i in 1:length(x)) {
        if (rx[i]<mx)
            xnew <- cbind(as.matrix(x[[i]]),
                          matrix(NA,
                                 nrow=nrow(as.matrix(x[[i]])),
                                 ncol=(mx-rx[i])
                                 )
                          )
        else
            xnew <- x[[i]]
        y <- rbind(y,xnew)
    }
    y
  }

