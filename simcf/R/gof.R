
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

pcp.oprobit <- function(x, y, b, constant=1, ncat=3, type="model") { # other types:  null, improve

  require(simcf)
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


concord.glm <- function(res, y, type="model") { # other types:  null, improve
  require(verification)
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


concord.oprobit <- function(x, y, b, constant=1, ncat=3, type="model") { # other types:  null, improve
  require(simcf)
  require(verification)
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
