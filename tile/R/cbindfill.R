"cbindfill" <-
function(...) {
    x <- list(...)
    rx <- rep(0,length(x))
    for (i in 1:length(x)) {
        rx[i] <- nrow(as.matrix(x[[i]]))
    }
    mx <- max(rx)
    y <- NULL
    for (i in 1:length(x)) {
        if (rx[i]<mx)
            xnew <- rbind(as.matrix(x[[i]]),
                          matrix(NA,
                                 ncol=ncol(as.matrix(x[[i]])),
                                 nrow=(mx-rx[i])
                                 )
                          )
        else
            xnew <- x[[i]]
        y <- cbind(y,xnew)
    }
    y
  }

