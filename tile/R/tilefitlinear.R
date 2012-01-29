tilefitlinear <-
function(y,x,ci) {
  dat <- na.omit(cbind(y,x))
  if (length(dat)>2) {
    dat <- sortmc(dat,2,decreasing=FALSE)
    x <- dat[,2]
    y <- dat[,1]
    result <- lm(y~x)
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
    warning("Error in linear fit.  Insufficient datapoints for bivariate fit.")
    fit <- NULL
  }
  fit
}

