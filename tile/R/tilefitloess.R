tilefitloess <-
function(y,x,ci,span=0.95) {
  
  dat <- na.omit(cbind(y,x))
  if (length(dat)>2) {
    dat <- sortmc(dat,2,decreasing=FALSE)
    x <- dat[,2]
    y <- dat[,1]
    result <- loess(y~x,span=span)

    fit <- list(x=x)
    if (!is.null(ci)&&(length(na.omit(ci))>0)) {
      fit$y <- predict(result,se=TRUE)$fit
      for (i in 1:length(ci)) {
        fit$lower <- cbind(fit$lower, fit$y + qt((1-ci[i])/2,10000)*predict(result,se=TRUE)$se.fit )
        fit$upper <- cbind(fit$upper, fit$y - qt((1-ci[i])/2,10000)*predict(result,se=TRUE)$se.fit )
      }
    } else {
      fit$y <- predict(result)
    }
  } else {
    warning("Error in loess fit.  Insufficient datapoints for bivariate fit.")
    fit <- NULL
  }

  fit
}

