#' Prediction-based goodness of fit measures for categorical models
#'
#' Compute percent correctly predicted and concordance indexes for logistic
#' regression, ordered probit, and similar models.
#'
#' To add.
#'
#' @param res A glm object, e.g., as produced by \code{\link{glm}}.
#' @param y A numeric vector containing the categorical response variable.  Not
#' implemented for factors.
#' @param type Character, whether the function should return the goodness of
#' fit for the estimated \code{model}, the goodness of fit for a \code{null}
#' model, or the percentage \code{improve}-ment from the latter to the former.
#' @param x The matrix of covariates.
#' @param b The vector of estimated parameters, with cutpoints at the end.
#' There should be k-2 cutpoints for a model with k response categories.  Not
#' currently compatible with \code{polr}.
#' @param constant Numeric, the position of the element of \code{b} containing
#' the constant term; set to \code{NA} for no constant.
#' @param ncat Number of response categories.
#' @return Returns either the percent correctly predicted, the concordance
#' index, or the percent improvement in these goodness of fit measures.
#' @author Christopher Adolph <\email{cadolph@@u.washington.edu}>
#' @keywords htest
#' @name pcp.glm
#' @rdname pcp.glm
#' @export
pcp.glm <- function(res, y, type="model") { # other types:  null, improve
  pcp <- mean(round(predict(res,type="response"))==y)
  pcpNull <- max(mean(y), mean(1-y))
  pcpImprove <- (pcp-pcpNull)/(1-pcpNull)

  if (type=="pcp")
    return(pcp)
  if (type=="null")
    return(pcpNull)
  if (type=="improve")
    return(pcpImprove)
}

#' @export
#' @rdname pcp.glm
pcp.oprobit <- function(x, y, b, constant=1, ncat=3, type="model") { # other types:  null, improve
  b <- matrix(b,nrow=100,ncol=length(b),byrow=TRUE)
  simy <- oprobitsimev(x, b, constant=constant, cat=ncat)

  cats <- sort(unique(y))

  predcatN <- cats[rev(order(table(y)))][1]

  n <- length(y)
  pcp <- pcpNull <- predcatM <- rep(NA,n)
  for (i in 1:n) {
    predcatM[i] <- cats[rev(order(simy$pe[i,]))][1]
    pcp[i] <- predcatM[i]==y[i]
    pcpNull[i] <- predcatN==y[i]
  }

  pcp <- mean(pcp)
  pcpNull <- mean(pcpNull)
  pcpImprove <- (pcp-pcpNull)/(1-pcpNull)

  if (type=="model")
    return(pcp)
  if (type=="null")
    return(pcpNull)
  if (type=="improve")
    return(pcpImprove)

}

#' @export
#' @rdname pcp.glm
concord.glm <- function(res, y, type="model") { # other types:  null, improve
  if (type=="model") {
    yhat <- predict(res,type="response")
    concord <- roc.area(y, yhat)$A
  }
  if (type=="null") {
    yhat <- rep(max(mean(y), mean(1-y)), length(y))
    concord <- roc.area(y, yhat)$A
  }
  if (type=="improve") {
    yhat <- predict(res,type="response")
    model <- roc.area(y, yhat)$A
    yhat <- rep( max(mean(y), mean(1-y)), length(y))
    null <- roc.area(y, yhat)$A
    concord <- (model-null)/(1-null)
  }
  concord
}

#' @export
#' @rdname pcp.glm
concord.oprobit <- function(x, y, b, constant=1, ncat=3, type="model") { # other types:  null, improve
  b <- matrix(b,nrow=100,ncol=length(b),byrow=TRUE)
  simy <- oprobitsimev(x, b, constant=constant, cat=ncat)
  cats <- sort(unique(y))

  if (type=="model") {
    model <- rep(NA,ncat)
    for (j in 1:ncat) {
      yhat <- simy$pe[,j]
      model[j] <- roc.area(as.numeric(y==cats[j]), yhat)$A
    }
    concord <- mean(model)
  }

  if (type=="null") {
    null <- rep(NA,ncat)
    for (j in 1:ncat) {
      probs <- rep(mean(y==cats[j]), length(y))
      null[j] <- roc.area(as.numeric(y==cats[j]), probs)$A
    }
    concord <- mean(null)
  }

  if (type=="improve") {
    improve <- rep(NA,ncat)
    for (j in 1:ncat) {
      probs <- rep(mean(y==cats[j]), length(y))
      null <- roc.area(as.numeric(y==cats[j]), probs)$A
      yhat <- simy$pe[,j]
      model <- roc.area(as.numeric(y==cats[j]), yhat)$A
      improve[j] <- (model-null)/(1-null)
    }
    concord <- mean(improve)
  }

  concord
}
