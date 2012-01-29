tilefitwls <-
function(y,x,ci,weights) {
  dat <- na.omit(cbind(y,x,weights))
  if (length(dat)>2) {
    dat <- sortmc(dat,2,decreasing=FALSE)
    x <- dat[,2]
    y <- dat[,1]
    weights <- dat[,3]
    result <- lm(y~x,weights=weights)
    fit <- list(x=x)
    fit$y <- result$fitted.values
    fit$lower <- fit$upper <- NULL
    if (!is.null(ci)&&(length(na.omit(ci))>0))
      for (i in 1:length(ci)) {
        pred <- predict(result,interval="confidence",level=ci[i])
        fit$lower <- cbind(fit$lower,pred[,2])
        fit$upper <- cbind(fit$upper,pred[,3])
      }
  } else {
    warning("Error in wls fit.  Insufficient datapoints for bivariate fit.")
    fit <- NULL
  }
  fit
}

