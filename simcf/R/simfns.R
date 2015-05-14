# Other sim functions to add:

#  TVC Cox PH                            lp/rp
#  panel ARCH/GARCH?                     lp/rp
#  ologit                                rp
#  ZIP/ZINB                              rp
#  Compositional data (RHS/ LHS)         rp/tern
#  Moving windows                        lp

# Append a vector into a matrix
appendmatrix <- function(x,values,after=ncol(x)) {
    nrowx <- nrow(x)
    ncolx <- ncol(x)
    after <- nrowx*(after-1)
    x <- as.vector(as.matrix(x))
    x <- append(x,values,after)
    matrix(x,nrow=nrowx,ncol=ncolx+1,byrow=FALSE)
}

# Simulate expected probabilities for linear models




#' Simulate quantities of interest and confidence intervals for linear models
#'
#' Simulate and summarize uncertainty of conditional expected values, first
#' differences and relative risks from estimated linear regression models
#'
#' Given simulated parameters from an estimated linear model, and
#' counterfactual values of the covariates, these functions calculate either
#' the conditional expected value of the response (\code{linearsimev}), the
#' conditional first difference (\code{linearsimfd}), or the relative risk
#' (\code{linearsimrr}), and confidence intervals around that point estimate
#' (optionally, predictive intervals as well, taking into account the
#' fundametal uncertainty in the response captured by sigma2).
#'
#' Use \code{cfMake} to initialize a \code{counterfactual} object containing
#' \code{x} and \code{xpre}, or input them directly.
#'
#' If the function you used to estimate the model does not provide simulated
#' parameter values, you can draw often them yourself, e.g., using functions
#' such as \code{\link{vcov}} and \code{\link{mvrnorm}} in the \pkg{MASS}
#' package, as shown below.
#'
#' \code{\link[Zelig]{zelig}}, in the package \pkg{Zelig}, offers similar features for
#' a wide array of models and with automated handling of the simulation
#' process.  These functions are offered as a simple alternative for users with
#' simulations already in hand.
#'
#' @aliases linearsimev linearsimfd linearsimrr
#' @param x list, a counterfactual object created by \code{cfMake}, or a vector
#' or matrix of counterfactual values of the covariates, including multiple
#' rows to simulate different counterfactual scenarios, and one column for each
#' covariate
#' @param b matrix, simulated parameters, one row per draw from the estimated
#' model, and one column per parameter, including any constants
#' @param ci vector, the requested intervals of the simulated quantity of
#' interest to be reported
#' @param constant scalar, the column of \code{b} containing the model
#' constant, or \code{NA} for no constant
#' @param sigma2 scalar or vector, either the estimated sigma2 from the linear
#' regression, or a vector of simulated sigma2's drawn from the model
#' posterior.  Only required if predicted values are desired.
#' @param sims scalar, the number of draws taken from the predictive
#' distribution of the response.  Only used if sigma2 is provided.  Default is
#' 10 draws.
#' @param save logical, whether to save the simulated expected values (and
#' predicted values, if any), in addition to the point estimates and requested
#' intervals.  Default is FALSE.
#' @param xpre vector or matrix, counterfactual initial values of the
#' covariates.  Rows must match \code{x}.  Not needed when \code{x} is a
#' \code{counterfactual} object.
#' @return Returns a list with at least three components, and as many as seven:
#' \item{pe}{vector, the point estimate(s) of the requested quantity of
#' interest} \item{lower}{vector or matrix, the requested lower confidence
#' bounds around the quantity of interest; rows are scenarios, columns are
#' intervals} \item{upper}{vector or matrix, the requested upper confidence
#' bounds around the quantity of interest; rows are scenarios, columns are
#' intervals} \item{plower}{vector or matrix, the requested lower predictive
#' bounds around the quantity of interest; rows are scenarios, columns are
#' intervals (requires sigma2 be an input)} \item{pupper}{vector or matrix, the
#' requested upper predictive bounds around the quantity of interest; rows are
#' scenarios, columns are intervals (requires sigma2 be an input)}
#' \item{ev}{vector or matrix, the simulated expected values; rows are
#' scenarios, columns are simulations (only given if save is TRUE)}
#' \item{pv}{vector or matrix, the simulated predicted values; rows are
#' scenarios, columns are simulations (only given if sigma2 is provided and
#' save is TRUE)}
#' @author Christopher Adolph <\email{cadolph@@u.washington.edu}>
#' @seealso \code{\link{cfMake}}, \code{\link{cfChange}}, \code{\link{cfName}}
#' @keywords models
#' @export
linearsimev <- function(x,b,ci=0.95,constant=1,sigma2=NULL,sims=10,save=FALSE,nscen=1) {
  if (is.null(x)) {
    if (is.na(constant))
      stop("either x must be given, or a constant specified")
    else
      x <- matrix(1,nrow=nscen,ncol=1)
  } else {
    if (any(class(x)=="counterfactual")&&!is.null(x$model)) {
      x <- model.matrix(x$model,x$x)
    } else {
      if (any(class(x)=="list")) x <- x$x
      if (is.data.frame(x)) x <- as.matrix(x)
      if (!is.matrix(x)) {
        if (is.matrix(b)) {
          x <- t(x)
          if (!is.na(constant)) {
            x <- append(x,1,constant-1)
          }
        } else {
          x <- as.matrix(x)
          if (!is.na(constant)) {
            x <- appendmatrix(x,rep(1,nrow(x)),constant)
          }
        }
      } else {
        if (!is.na(constant)) {
          x <- appendmatrix(x,rep(1,nrow(x)),constant)
        }
      }
    }
  }
  esims <- nrow(as.matrix(b))

  if (!is.null(sigma2)) {
    predict <- TRUE
    sigma <- sqrt(sigma2)
  } else
    predict <- FALSE

  nscen <- nrow(x)
  nci <- length(ci)
  res <- list(pe = rep(NA, nscen),
              lower = matrix(NA,nrow=nscen,ncol=nci),
              upper = matrix(NA,nrow=nscen,ncol=nci))
  if (predict) {
    res$plower <- matrix(NA,nrow=nscen,ncol=nci)
    res$pupper <- matrix(NA,nrow=nscen,ncol=nci)
  }
  if (save) {
    res$ev <- matrix(NA,nrow=nscen,ncol=esims)
    if (predict) {
      res$pv <- matrix(NA,nrow=nscen,ncol=esims*sims)
    }
  }
  for (i in 1:nscen) {
    simmu <- b%*%x[i,]
    if (save) res$ev[i,] <- simmu
    res$pe[i] <- mean(simmu)
    for (k in 1:nci) {
      cint <- quantile(simmu,probs=c( (1-ci[k])/2, (1-(1-ci[k])/2) ) )
      res$lower[i,k] <- cint[1]
      res$upper[i,k] <- cint[2]
    }

    # Simulate predicted values if requested
    if (predict) {
      pv <- rnorm(sims*esims,mean=simmu,sd=sigma)
      if (save)
        res$pv[i,] <- pv
      for (k in 1:nci) {
        cint <- quantile(pv,probs=c( (1-ci[k])/2, (1-(1-ci[k])/2) ) )
        res$plower[i,k] <- cint[1]
        res$pupper[i,k] <- cint[2]
      }
    }
  }
  res$lower <- drop(res$lower)
  res$upper <- drop(res$upper)
  if (predict) {
    res$plower <- drop(res$plower)
    res$pupper <- drop(res$pupper)
  }
  res
}


#' @export
linearsimfd <- function(x,b,ci=0.95,constant=1,xpre=NULL) {
  if (any(class(x)=="counterfactual")&&!is.null(x$model)) {
    xpre <- model.matrix(x$model,x$xpre)
    x <- model.matrix(x$model,x$x)
  } else {
    if (any(class(x)=="list")) x <- x$x
    if (any(class(x)=="list")) xpre <- x$xpre
    if (is.data.frame(x)) x <- as.matrix(x)
    if (is.data.frame(xpre)) x <- as.matrix(xpre)
    if (!is.matrix(x)) {
      if (is.matrix(b)) {
        x <- t(x)
        if (!is.na(constant)) {
          x <- append(x,1,constant-1)
        }
      } else {
        x <- as.matrix(x)
        if (!is.na(constant)) {
          x <- appendmatrix(x,rep(1,nrow(x)),constant)
        }
      }
    } else {
      if (!is.na(constant)) {
        x <- appendmatrix(x,rep(1,nrow(x)),constant)
      }
    }
    if (!is.matrix(xpre)) {
      if (is.matrix(b)) {
        xpre <- t(xpre)
        if (!is.na(constant)) {
          xpre <- append(xpre,1,constant-1)
        }
      } else {
        xpre <- as.matrix(xpre)
        if (!is.na(constant)) {
          xpre <- appendmatrix(xpre,rep(1,nrow(xpre)),constant)
        }
      }
    } else {
      if (!is.na(constant)) {
        xpre <- appendmatrix(xpre,rep(1,nrow(xpre)),constant)
      }
    }
  }

  esims <- nrow(as.matrix(b))

  nscen <- nrow(x)
  nci <- length(ci)
  res <- list(pe = rep(NA, nscen),
              lower = matrix(NA,nrow=nscen,ncol=nci),
              upper = matrix(NA,nrow=nscen,ncol=nci))
  for (i in 1:nscen) {
    simmu1 <- b%*%xpre[i,]
    simmu2 <- b%*%x[i,]
    simy <- simmu2 - simmu1
    res$pe[i] <- mean(simy)
    for (k in 1:nci) {
      cint <- quantile(simy,probs=c( (1-ci[k])/2, (1-(1-ci[k])/2) ) )
      res$lower[i,k] <- cint[1]
      res$upper[i,k] <- cint[2]
    }
  }
  res$lower <- drop(res$lower)
  res$upper <- drop(res$upper)
  res
}



#' @export
linearsimrr <- function(x,b,ci=0.95,constant=1,xpre=NULL) {
  if (any(class(x)=="counterfactual")&&!is.null(x$model)) {
    xpre <- model.matrix(x$model,x$xpre)
    x <- model.matrix(x$model,x$x)
  } else {
    if (any(class(x)=="list")) x <- x$x
    if (any(class(x)=="list")) xpre <- x$xpre
    if (is.data.frame(x)) x <- as.matrix(x)
    if (is.data.frame(xpre)) x <- as.matrix(xpre)
    if (!is.matrix(x)) {
      if (is.matrix(b)) {
        x <- t(x)
        if (!is.na(constant)) {
          x <- append(x,1,constant-1)
        }
      } else {
        x <- as.matrix(x)
        if (!is.na(constant)) {
          x <- appendmatrix(x,rep(1,nrow(x)),constant)
        }
      }
    } else {
      if (!is.na(constant)) {
        x <- appendmatrix(x,rep(1,nrow(x)),constant)
      }
    }
    if (!is.matrix(xpre)) {
      if (is.matrix(b)) {
        xpre <- t(xpre)
        if (!is.na(constant)) {
          xpre <- append(xpre,1,constant-1)
        }
      } else {
        xpre <- as.matrix(xpre)
        if (!is.na(constant)) {
          xpre <- appendmatrix(xpre,rep(1,nrow(xpre)),constant)
        }
      }
    } else {
      if (!is.na(constant)) {
        xpre <- appendmatrix(xpre,rep(1,nrow(xpre)),constant)
      }
    }
  }

  esims <- nrow(as.matrix(b))

  nscen <- nrow(x)
  nci <- length(ci)
  res <- list(pe = rep(NA, nscen),
              lower = matrix(NA,nrow=nscen,ncol=nci),
              upper = matrix(NA,nrow=nscen,ncol=nci))
  for (i in 1:nscen) {
    simmu1 <- b%*%xpre[i,]
    simmu2 <- b%*%x[i,]
    simy <- simmu2/simmu1
    res$pe[i] <- mean(simy)
    for (k in 1:nci) {
      cint <- quantile(simy,probs=c( (1-ci[k])/2, (1-(1-ci[k])/2) ) )
      res$lower[i,k] <- cint[1]
      res$upper[i,k] <- cint[2]
    }
  }
  res$lower <- drop(res$lower)
  res$upper <- drop(res$upper)
  res
}



# Simulate expected probabilities for logit




#' Simulate quantities of interest and confidence intervals for binary logit
#'
#' Simulate and summarize uncertainty of conditional expected values, first
#' differences and relative risks from estimated binary logit models
#'
#' Given simulated parameters from an estimated logit model, and counterfactual
#' values of the covariates, these functions calculate either the conditional
#' expected value of the response (\code{logitsimev}), the conditional first
#' difference (\code{logitsimfd}), or the relative risk (\code{logitsimrr}),
#' and confidence intervals around that point estimate.  Use \code{cfMake} to
#' initialize a \code{counterfactual} object containing \code{x} and
#' \code{xpre}, or input them directly.
#'
#' If the function you used to estimate the model does not provide simulated
#' parameter values, you can draw often them yourself, e.g., using functions
#' such as \code{\link{vcov}} and \code{mvrnorm} in the \code{MASS} package, as
#' shown below.
#'
#' zelig, in the package Zelig, offers similar features for a wide array of
#' models and with automated handling of the simulation process.  These
#' functions are offered as a simple alternative for users with simulations
#' already in hand.
#'
#' @aliases logitsimev logitsimfd logitsimrr
#' @param x list, a counterfactual object created by \code{cfMake}, or a vector
#' or matrix of counterfactual values of the covariates, including multiple
#' rows to simulate different counterfactual scenarios, and one column for each
#' covariate
#' @param b matrix, simulated parameters, one row per draw from the estimated
#' model, and one column per parameter, including any constants
#' @param ci vector, the requested intervals of the simulated quantity of
#' interest to be reported
#' @param constant scalar, the column of \code{b} containing the model
#' constant, or \code{NA} for no constant
#' @param xpre vector or matrix, counterfactual initial values of the
#' covariates.  Rows must match \code{x}.  Not needed when \code{x} is a
#' \code{counterfactual} object.
#' @return Returns a list with three components \item{pe}{vector, the point
#' estimate(s) of the requested quantity of interest} \item{lower}{matrix, the
#' requested lower bounds around the quantity of interest; rows are scenarios,
#' columns are intervals} \item{upper}{matrix, the requested upper bounds
#' around the quantity of interest; rows are scenarios, columns are intervals}
#' @author Christopher Adolph <\email{cadolph@@u.washington.edu}>
#' @seealso \code{\link{probitsimev}}, \code{\link{mlogitsimev}},
#' \code{\link{cfMake}}, \code{\link{cfChange}}, \code{\link{cfName}}
#' @keywords models
#' @export
logitsimev <- function(x,b,ci=0.95,constant=1) {
    if (any(class(x)=="counterfactual")&&!is.null(x$model)) {
        x <- model.matrix(x$model,x$x)
    } else {
        if (any(class(x)=="list")) x <- x$x
        if (is.data.frame(x)) x <- as.matrix(x)
        if (!is.matrix(x)) {
          if (is.matrix(b)) {
            x <- t(x)
            if (!is.na(constant)) {
              x <- append(x,1,constant-1)
            }
          } else {
            x <- as.matrix(x)
            if (!is.na(constant)) {
              x <- appendmatrix(x,rep(1,nrow(x)),constant)
            }
          }
        } else {
          if (!is.na(constant)) {
            x <- appendmatrix(x,rep(1,nrow(x)),constant)
          }
        }
      }

  esims <- nrow(as.matrix(b))

    nscen <- nrow(x)
    nci <- length(ci)
    res <- list(pe = rep(NA, nscen),
                lower = matrix(NA,nrow=nscen,ncol=nci),
                upper = matrix(NA,nrow=nscen,ncol=nci))
    for (i in 1:nscen) {
      simmu <- b%*%x[i,]
      simy <- 1/(1+ exp(-simmu))
      res$pe[i] <- mean(simy)
      for (k in 1:nci) {
        cint <- quantile(simy,probs=c( (1-ci[k])/2, (1-(1-ci[k])/2) ) )
        res$lower[i,k] <- cint[1]
        res$upper[i,k] <- cint[2]
      }
    }
    res$lower <- drop(res$lower)
    res$upper <- drop(res$upper)
    res
}


# Simulate first difference of expected probabilities for logit
#' @export
logitsimfd <- function(x,b,ci=0.95,constant=1,xpre=NULL) {
  if (any(class(x)=="counterfactual")&&!is.null(x$model)) {
    xpre <- model.matrix(x$model,x$xpre)
    x <- model.matrix(x$model,x$x)
  } else {
    if (any(class(x)=="list")) x <- x$x
    if (any(class(x)=="list")) xpre <- x$xpre
    if (is.data.frame(x)) x <- as.matrix(x)
    if (is.data.frame(xpre)) x <- as.matrix(xpre)
    if (!is.matrix(x)) {
      if (is.matrix(b)) {
        x <- t(x)
        if (!is.na(constant)) {
          x <- append(x,1,constant-1)
        }
      } else {
        x <- as.matrix(x)
        if (!is.na(constant)) {
          x <- appendmatrix(x,rep(1,nrow(x)),constant)
        }
      }
    } else {
      if (!is.na(constant)) {
        x <- appendmatrix(x,rep(1,nrow(x)),constant)
      }
    }
    if (!is.matrix(xpre)) {
      if (is.matrix(b)) {
        xpre <- t(xpre)
        if (!is.na(constant)) {
          xpre <- append(xpre,1,constant-1)
        }
      } else {
        xpre <- as.matrix(xpre)
        if (!is.na(constant)) {
          xpre <- appendmatrix(xpre,rep(1,nrow(xpre)),constant)
        }
      }
    } else {
      if (!is.na(constant)) {
        xpre <- appendmatrix(xpre,rep(1,nrow(xpre)),constant)
      }
    }
  }

  esims <- nrow(as.matrix(b))

  nscen <- nrow(x)
  nci <- length(ci)
  res <- list(pe = rep(NA, nscen),
              lower = matrix(NA,nrow=nscen,ncol=nci),
              upper = matrix(NA,nrow=nscen,ncol=nci))
  for (i in 1:nscen) {
    simmu1 <- b%*%xpre[i,]
    simmu2 <- b%*%x[i,]
    simy <- 1/(1+ exp(-simmu2)) - 1/(1+ exp(-simmu1))
    res$pe[i] <- mean(simy)
    for (k in 1:nci) {
      cint <- quantile(simy,probs=c( (1-ci[k])/2, (1-(1-ci[k])/2) ) )
      res$lower[i,k] <- cint[1]
      res$upper[i,k] <- cint[2]
    }
  }
  res$lower <- drop(res$lower)
  res$upper <- drop(res$upper)
  res
}



# Simulate relative risk of expected probabilities for logit
#' @export
logitsimrr <- function(x,b,ci=0.95,constant=1,xpre=NULL) {
  if (any(class(x)=="counterfactual")&&!is.null(x$model)) {
    xpre <- model.matrix(x$model,x$xpre)
    x <- model.matrix(x$model,x$x)
  } else {
    if (any(class(x)=="list")) x <- x$x
    if (any(class(x)=="list")) xpre <- x$xpre
    if (is.data.frame(x)) x <- as.matrix(x)
    if (is.data.frame(xpre)) x <- as.matrix(xpre)
    if (!is.matrix(x)) {
      if (is.matrix(b)) {
        x <- t(x)
        if (!is.na(constant)) {
          x <- append(x,1,constant-1)
        }
      } else {
        x <- as.matrix(x)
        if (!is.na(constant)) {
          x <- appendmatrix(x,rep(1,nrow(x)),constant)
        }
      }
    } else {
      if (!is.na(constant)) {
        x <- appendmatrix(x,rep(1,nrow(x)),constant)
      }
    }
    if (!is.matrix(xpre)) {
      if (is.matrix(b)) {
        xpre <- t(xpre)
        if (!is.na(constant)) {
          xpre <- append(xpre,1,constant-1)
        }
      } else {
        xpre <- as.matrix(xpre)
        if (!is.na(constant)) {
          xpre <- appendmatrix(xpre,rep(1,nrow(xpre)),constant)
        }
      }
    } else {
      if (!is.na(constant)) {
        xpre <- appendmatrix(xpre,rep(1,nrow(xpre)),constant)
      }
    }
  }

  esims <- nrow(as.matrix(b))

  nscen <- nrow(x)
  nci <- length(ci)
  res <- list(pe = rep(NA, nscen),
              lower = matrix(NA,nrow=nscen,ncol=nci),
              upper = matrix(NA,nrow=nscen,ncol=nci))
  for (i in 1:nscen) {
    simmu1 <- b%*%xpre[i,]
    simmu2 <- b%*%x[i,]
    simy <- (1/(1+ exp(-simmu2))) / (1/(1+ exp(-simmu1)))
    res$pe[i] <- mean(simy)
    for (k in 1:nci) {
      cint <- quantile(simy,probs=c( (1-ci[k])/2, (1-(1-ci[k])/2) ) )
      res$lower[i,k] <- cint[1]
      res$upper[i,k] <- cint[2]
    }
  }
  res$lower <- drop(res$lower)
  res$upper <- drop(res$upper)
  res
}



# Simulate expected probabilities for probit




#' Simulate quantities of interest and confidence intervals for binary probit
#'
#' Simulate and summarize uncertainty of conditional expected values, first
#' differences and relative risks from estimated binary probit models
#'
#' Given simulated parameters from an estimated probit model, and
#' counterfactual values of the covariates, these functions calculate either
#' the conditional expected value of the response (\code{probitsimev}), the
#' conditional first difference (\code{probitsimfd}), or the relative risk
#' (\code{probitsimrr}), and confidence intervals around that point estimate.
#' Use \code{cfMake} to initialize a \code{counterfactual} object containing
#' \code{x} and \code{xpre}, or input them directly.
#'
#' If the function you used to estimate the model does not provide simulated
#' parameter values, you can draw often them yourself, e.g., using functions
#' such as \code{\link{vcov}} and \code{mvrnorm} in the \code{MASS} package, as
#' shown below.
#'
#' zelig, in the package Zelig, offers similar features for a wide array of
#' models and with automated handling of the simulation process.  These
#' functions are offered as a simple alternative for users with simulations
#' already in hand.
#'
#' @aliases probitsimev probitsimfd probitsimrr
#' @param x list, a counterfactual object created by \code{cfMake}, or a vector
#' or matrix of counterfactual values of the covariates, including multiple
#' rows to simulate different counterfactual scenarios, and one column for each
#' covariate
#' @param b matrix, simulated parameters, one row per draw from the estimated
#' model, and one column per parameter, including any constants
#' @param ci vector, the requested intervals of the simulated quantity of
#' interest to be reported
#' @param constant scalar, the column of \code{b} containing the model
#' constant, or \code{NA} for no constant
#' @param xpre vector or matrix, counterfactual initial values of the
#' covariates.  Rows must match \code{x}.  Not needed when \code{x} is a
#' \code{counterfactual} object.
#' @return Returns a list with three components \item{pe}{vector, the point
#' estimate(s) of the requested quantity of interest} \item{lower}{matrix, the
#' requested lower bounds around the quantity of interest; rows are scenarios,
#' columns are intervals} \item{upper}{matrix, the requested upper bounds
#' around the quantity of interest; rows are scenarios, columns are intervals}
#' @author Christopher Adolph <\email{cadolph@@u.washington.edu}>
#' @seealso \code{\link{logitsimev}}, \code{\link{mlogitsimev}},
#' \code{\link{cfMake}}, \code{\link{cfChange}}, \code{\link{cfName}}
#' @keywords models
#' @export
probitsimev <- function(x,b,ci=0.95,constant=1) {
    if (any(class(x)=="counterfactual")&&!is.null(x$model)) {
        x <- model.matrix(x$model,x$x)
    } else {
        if (any(class(x)=="list")) x <- x$x
        if (is.data.frame(x)) x <- as.matrix(x)
        if (!is.matrix(x)) {
          if (is.matrix(b)) {
            x <- t(x)
            if (!is.na(constant)) {
              x <- append(x,1,constant-1)
            }
          } else {
            x <- as.matrix(x)
            if (!is.na(constant)) {
              x <- appendmatrix(x,rep(1,nrow(x)),constant)
            }
          }
        } else {
          if (!is.na(constant)) {
            x <- appendmatrix(x,rep(1,nrow(x)),constant)
          }
        }
      }

  esims <- nrow(as.matrix(b))

    nscen <- nrow(x)
    nci <- length(ci)
    res <- list(pe = rep(NA, nscen),
                lower = matrix(NA,nrow=nscen,ncol=nci),
                upper = matrix(NA,nrow=nscen,ncol=nci))
    for (i in 1:nscen) {
      simmu <- b%*%x[i,]
      simy <- pnorm(simmu)
      res$pe[i] <- mean(simy)
      for (k in 1:nci) {
        cint <- quantile(simy,probs=c( (1-ci[k])/2, (1-(1-ci[k])/2) ) )
        res$lower[i,k] <- cint[1]
        res$upper[i,k] <- cint[2]
      }
    }
    res$lower <- drop(res$lower)
    res$upper <- drop(res$upper)
    res
}


# Simulate first difference of expected probabilities for probit
#' @export
probitsimfd <- function(x,b,ci=0.95,constant=1,xpre=NULL) {
    if (any(class(x)=="counterfactual")&&!is.null(x$model)) {
      xpre <- model.matrix(x$model,x$xpre)
      x <- model.matrix(x$model,x$x)
    } else {
        if (any(class(x)=="list")) x <- x$x
        if (any(class(x)=="list")) xpre <- x$xpre
        if (is.data.frame(x)) x <- as.matrix(x)
        if (is.data.frame(xpre)) x <- as.matrix(xpre)
        if (!is.matrix(x)) {
          if (is.matrix(b)) {
            x <- t(x)
            if (!is.na(constant)) {
              x <- append(x,1,constant-1)
            }
          } else {
            x <- as.matrix(x)
            if (!is.na(constant)) {
              x <- appendmatrix(x,rep(1,nrow(x)),constant)
            }
          }
        } else {
          if (!is.na(constant)) {
            x <- appendmatrix(x,rep(1,nrow(x)),constant)
          }
        }
        if (!is.matrix(xpre)) {
          if (is.matrix(b)) {
            xpre <- t(xpre)
            if (!is.na(constant)) {
              xpre <- append(xpre,1,constant-1)
            }
          } else {
            xpre <- as.matrix(xpre)
            if (!is.na(constant)) {
              xpre <- appendmatrix(xpre,rep(1,nrow(xpre)),constant)
            }
          }
        } else {
          if (!is.na(constant)) {
            xpre <- appendmatrix(xpre,rep(1,nrow(xpre)),constant)
          }
        }
      }

  esims <- nrow(as.matrix(b))

    nscen <- nrow(x)
    nci <- length(ci)
    res <- list(pe = rep(NA, nscen),
                lower = matrix(NA,nrow=nscen,ncol=nci),
                upper = matrix(NA,nrow=nscen,ncol=nci))
    for (i in 1:nscen) {
      simmu1 <- b%*%xpre[i,]
      simmu2 <- b%*%x[i,]
      simy <- pnorm(simmu2) - pnorm(simmu1)
      res$pe[i] <- mean(simy)
      for (k in 1:nci) {
        cint <- quantile(simy,probs=c( (1-ci[k])/2, (1-(1-ci[k])/2) ) )
        res$lower[i,k] <- cint[1]
        res$upper[i,k] <- cint[2]
      }
    }
    res$lower <- drop(res$lower)
    res$upper <- drop(res$upper)
    res
  }

# Simulate relative risk of expected probabilities for probit
#' @export
probitsimrr <- function(x,b,ci=0.95,constant=1,xpre=NULL) {
    if (any(class(x)=="counterfactual")&&!is.null(x$model)) {
      xpre <- model.matrix(x$model,x$xpre)
      x <- model.matrix(x$model,x$x)
    } else {
        if (any(class(x)=="list")) x <- x$x
        if (any(class(x)=="list")) xpre <- x$xpre
        if (is.data.frame(x)) x <- as.matrix(x)
        if (is.data.frame(xpre)) x <- as.matrix(xpre)
        if (!is.matrix(x)) {
          if (is.matrix(b)) {
            x <- t(x)
            if (!is.na(constant)) {
              x <- append(x,1,constant-1)
            }
          } else {
            x <- as.matrix(x)
            if (!is.na(constant)) {
              x <- appendmatrix(x,rep(1,nrow(x)),constant)
            }
          }
        } else {
          if (!is.na(constant)) {
            x <- appendmatrix(x,rep(1,nrow(x)),constant)
          }
        }
        if (!is.matrix(xpre)) {
          if (is.matrix(b)) {
            xpre <- t(xpre)
            if (!is.na(constant)) {
              xpre <- append(xpre,1,constant-1)
            }
          } else {
            xpre <- as.matrix(xpre)
            if (!is.na(constant)) {
              xpre <- appendmatrix(xpre,rep(1,nrow(xpre)),constant)
            }
          }
        } else {
          if (!is.na(constant)) {
            xpre <- appendmatrix(xpre,rep(1,nrow(xpre)),constant)
          }
        }
    }

  esims <- nrow(as.matrix(b))

    nscen <- nrow(x)
    nci <- length(ci)
    res <- list(pe = rep(NA, nscen),
                lower = matrix(NA,nrow=nscen,ncol=nci),
                upper = matrix(NA,nrow=nscen,ncol=nci))
    for (i in 1:nscen) {
      simmu1 <- b%*%xpre[i,]
      simmu2 <- b%*%x[i,]
      simy <- pnorm(simmu2) / pnorm(simmu1)
      res$pe[i] <- mean(simy)
      for (k in 1:nci) {
        cint <- quantile(simy,probs=c( (1-ci[k])/2, (1-(1-ci[k])/2) ) )
        res$lower[i,k] <- cint[1]
        res$upper[i,k] <- cint[2]
      }
    }
    res$lower <- drop(res$lower)
    res$upper <- drop(res$upper)
    res
}

#' Likelihood for ordered probit
#'
#' @export
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
      if (tau[i] <= tau[i-1]) prob[,i] <- -(abs(tau[i])*10000)    # penalty function
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


# Simulate expected probabilities for ordered probit




#' Simulate quantities of interest and confidence intervals for ordered probit
#'
#' Simulate and summarize uncertainty of conditional expected values, first
#' differences and relative risks from estimated ordered probit models
#'
#' Given simulated parameters from an estimated ordered probit model, and
#' counterfactual values of the covariates, these functions calculate either
#' the conditional expected value of the response (\code{oprobitsimev}), the
#' conditional first difference (\code{oprobitsimfd}), or the relative risk
#' (\code{oprobitsimrr}), and confidence intervals around these point
#' estimates.
#'
#' Use \code{cfMake} to initialize a \code{counterfactual} object containing
#' \code{x} and \code{xpre}, or input them directly.
#'
#' If the function you used to estimate the model does not provide simulated
#' parameter values, you can draw often them yourself, e.g., using functions
#' such as \code{\link{vcov}} and \code{mvrnorm} in the \code{MASS} package, as
#' shown below.
#'
#' zelig, in the package Zelig, offers similar features for a wide array of
#' models and with automated handling of the simulation process.  These
#' functions are offered as a simple alternative for users with simulations
#' already in hand.
#'
#' @aliases oprobitsimev oprobitsimfd oprobitsimrr
#' @param x list, a counterfactual object created by \code{cfMake}, or a vector
#' or matrix of counterfactual values of the covariates, including multiple
#' rows to simulate different counterfactual scenarios, and one column for each
#' covariate
#' @param b matrix, simulated parameters, one row per draw from the estimated
#' model, and one column per parameter, including any constants.  The last
#' \code{cat} - 2 columns should be the cutpoints.
#' @param ci vector, the requested intervals of the simulated quantity of
#' interest to be reported
#' @param constant scalar, the column of \code{b} containing the model
#' constant, or \code{NA} for no constant.  If the estimation was done in
#' polr() from the MASS library, you must set constant to NA.
#' @param cat scalar, the number of categories in the response variable; must
#' be at least 3 and an integer
#' @param xpre vector or matrix, counterfactual initial values of the
#' covariates.  Rows must match \code{x}.  Not needed when \code{x} is a
#' \code{counterfactual} object.
#' @return Returns a list with three components \item{pe}{matrix, the point
#' estimates of the requested quantity of interest; rows are scenarios, and
#' columns are categories} \item{lower}{array, the requested lower bounds
#' around the quantity of interest; rows are scenarios, columns are the
#' category of the response, and the third dimension is the confidence
#' interval} \item{upper}{array, the requested upper bounds around the quantity
#' of interest; rows are scenarios, columns are the category of the response,
#' and the third dimension is the confidence interval}
#' @author Christopher Adolph <\email{cadolph@@u.washington.edu}>
#' @seealso \code{\link{logitsimev}}, \code{\link{mlogitsimev}},
#' \code{\link{cfMake}}, \code{\link{cfChange}}, \code{\link{cfName}}
#' @keywords models
#' @examples
#'
#' require(MASS)
#'
#' # Using housing data from MASS; convert to optim usable form
#' data(housing)
#' housingExpanded <- housing[rep(1:nrow(housing), housing$Freq),]
#' rownames(housingExpanded) <- 1:nrow(housingExpanded)
#' housingExpanded$Sat <- as.numeric(housingExpanded$Sat)
#' housingExpanded$Infl <- as.numeric(housingExpanded$Infl)
#' housingExpanded$Cont <- as.numeric(housingExpanded$Cont)
#' y <- housingExpanded$Sat
#' x <- cbind(housingExpanded$Infl, housingExpanded$Cont)
#' model <- Sat ~ Infl + Cont
#'
#' # Estimate a p-category ordered probit using optim and llk.oprobit (from simcf library)
#' # This version of ordered probit has a constant and p-2 estimated cutpoints
#' ncut <- length(unique(housingExpanded$Sat)) - 2
#' ls.result <- lm(model, data=housingExpanded) # LS based starting values
#' stval <- c(ls.result$coefficients, 1:ncut)          # and 1, 2, ... for cutpoints
#' house.optim <- optim(stval, llk.oprobit, method="BFGS", y=y, x=x, hessian=TRUE)
#'
#' # Construct counterfactual scenarios:  All combinations of Infl and Cont
#' xhyp <- cfFactorial(Infl = unique(housingExpanded$Infl),
#'                     Cont = unique(housingExpanded$Cont))
#'
#' # Simulate E(Sat) for each counterfactual
#' sims <- 10000
#' simbetas.optim <- mvrnorm(sims, house.optim$par, solve(house.optim$hessian) )
#' yhyp.optim <- oprobitsimev(xhyp, simbetas.optim, constant=1, cat=3)
#' print(yhyp.optim)
#'
#' # Estimate a p-category ordered probit using polr (from MASS library)
#' # This version of ordered probit has no constant and p-1 estimated cutpoints
#' house.plr <- polr(as.factor(y) ~ x, method="probit")
#' house.plr$par <- c(house.plr$coefficients, house.plr$zeta)
#' simbetas.plr <- mvrnorm(sims, house.plr$par, vcov(house.plr) )
#'
#' # Simulate E(Sat) for each counterfactual, polr version
#' #   Must set constant = NA to predict from polr
#' yhyp.plr <- oprobitsimev(xhyp, simbetas.plr, constant=NA, cat=3)
#' print(yhyp.plr)
#'
#' @export
oprobitsimev <- function(x,b,ci=0.95,constant=1,cat=3) {
  if (cat<3) { stop("cat must be at least 3 for ordered probit") }
    if (any(class(x)=="counterfactual")&&!is.null(x$model)) {
      if (is.na(constant)) {
        reviseModel <- as.character(x$model)
        reviseModel <- as.formula(paste(reviseModel[2],reviseModel[1],paste(reviseModel[3],"-1")))
        x$model <- reviseModel
      }
      x <- model.matrix(x$model,x$x)

    } else {
        if (any(class(x)=="list")) x <- x$x
        if (is.data.frame(x)) x <- as.matrix(x)
        if (!is.matrix(x)) {
            if (is.matrix(b)) {
                x <- t(x)
                if (!is.na(constant)) {
                    x <- append(x,1,constant-1)
                }
            } else {
                x <- as.matrix(x)
                if (!is.na(constant)) {
                    x <- appendmatrix(x,rep(1,nrow(x)),constant)
                }
            }
        } else {
          if (!is.na(constant)) {
            x <- appendmatrix(x,rep(1,nrow(x)),constant)
          }
        }
    }

  sims <- nrow(as.matrix(b))
  res <- list(pe=NULL,
              lower=array(0, dim = c(nrow(x), cat, length(ci) )),
              upper=array(0, dim = c(nrow(x), cat, length(ci) ))
              )

    for (i in 1:nrow(x)) {
      if (is.na(constant)) {
        simbeta <- b[,1:(ncol(b)-cat+1)]
        simtau <- b[,(ncol(b)-cat+2):ncol(b),drop=FALSE]
      } else {
        simbeta <- b[,1:(ncol(b)-cat+2)]
        simtau <- b[,(ncol(b)-cat+3):ncol(b),drop=FALSE]
      }

      if (is.vector(simbeta)) {
	simmu = as.matrix(simbeta) %*% x[i]
      }
      else {
	simmu <- simbeta%*%x[i,]
      }

      simy <- matrix(NA,nrow=sims,ncol=cat)
      if (!is.na(constant)) {
        simy[,1] <- pnorm(0,simmu)
        for (j in 2:(cat-1)) {
          simy[,j] <- pnorm(simtau[,(j-1)],simmu) - apply(simy[,1:(j-1),drop=FALSE],1,sum)
        }
      } else {
        simy[,1] <- pnorm(simtau[,1],simmu)
        for (j in 2:(cat-1)) {
          simy[,j] <- pnorm(simtau[,j],simmu) - apply(simy[,1:(j-1),drop=FALSE],1,sum)
        }
      }
      simy[,cat] <- 1-apply(simy[,1:(cat-1),drop=FALSE],1,sum)
      for (j in 1:ncol(simy)) {
        simy[,j] <- sort(simy[,j])
      }
      res$pe <- rbind(res$pe,
                      apply(simy,2,mean))
      length.simy <- nrow(simy)
      low <- up <- NULL
      for (k in 1:length(ci)) {
        res$lower[i,,k] <- cbind( low, simy[trunc((1-ci[k])/2*length.simy),] )
        res$upper[i,,k]  <- cbind( up, simy[trunc((1-(1-ci[k])/2)*length.simy),] )
      }
    }
  if (length(ci)==1) {
    if (nrow(x)==1) {
      res$pe <- as.numeric(res$pe)
      res$lower <- as.numeric(res$lower)
      res$upper <- as.numeric(res$upper)
    } else {
      res$lower <- res$lower[,,1,drop=TRUE]
      res$upper <- res$upper[,,1,drop=TRUE]
    }
  }
  res
}


# Simulate first differences for ordered probit
#' @export
oprobitsimfd <- function(x,b,ci=0.95,constant=1,cat=3,xpre=NULL) {
  if (cat<3) { stop("cat must be at least 3 for ordered probit") }

  if (any(class(x)=="counterfactual")&&!is.null(x$model)) {
      xpre <- model.matrix(x$model,x$xpre)
      x <- model.matrix(x$model,x$x)
    } else {
        if (any(class(x)=="list")) x <- x$x
        if (any(class(x)=="list")) xpre <- x$xpre
        if (is.data.frame(x)) x <- as.matrix(x)
        if (is.data.frame(xpre)) x <- as.matrix(xpre)
        if (!is.matrix(x)) {
          if (is.matrix(b)) {
            x <- t(x)
            if (!is.na(constant)) {
              x <- append(x,1,constant-1)
            }
          } else {
            x <- as.matrix(x)
            if (!is.na(constant)) {
              x <- appendmatrix(x,rep(1,nrow(x)),constant)
            }
          }
        } else {
          if (!is.na(constant)) {
            x <- appendmatrix(x,rep(1,nrow(x)),constant)
          }
        }
        if (!is.matrix(xpre)) {
          if (is.matrix(b)) {
            xpre <- t(xpre)
            if (!is.na(constant)) {
              xpre <- append(xpre,1,constant-1)
            }
          } else {
            xpre <- as.matrix(xpre)
            if (!is.na(constant)) {
              xpre <- appendmatrix(xpre,rep(1,nrow(xpre)),constant)
            }
          }
        } else {
          if (!is.na(constant)) {
            xpre <- appendmatrix(xpre,rep(1,nrow(xpre)),constant)
          }
        }
      }

  sims <- nrow(as.matrix(b))

  res <- list(pe=NULL,
              lower=array(0, dim = c(nrow(x), cat, length(ci) )),
              upper=array(0, dim = c(nrow(x), cat, length(ci) ))
              )

  for (i in 1:nrow(x)) {
    if (is.na(constant)) {
      simbeta <- b[,1:(ncol(b)-cat+1)]
      simtau <- b[,(ncol(b)-cat+2):ncol(b),drop=FALSE]
    } else {
      simbeta <- b[,1:(ncol(b)-cat+2)]
      simtau <- b[,(ncol(b)-cat+3):ncol(b),drop=FALSE]
    }
    simy <- matrix(NA,nrow=sims,ncol=cat)

    if (is.vector(simbeta)) {
      simmu1 <- as.matrix(simbeta) %*% xpre[i]
      simmu2 <- as.matrix(simbeta) %*% x[i]
    }
    else {
      simmu1 <- simbeta%*%xpre[i,]
      simmu2 <- simbeta%*%x[i,]
    }

    simy1 <- matrix(NA,nrow=sims,ncol=cat)
    if (!is.na(constant)) {
      simy1[,1] <- pnorm(0,simmu1)
      for (j in 2:(cat-1)) {
        simy1[,j] <- pnorm(simtau[,(j-1)],simmu1) - apply(simy1[,1:(j-1),drop=FALSE],1,sum)
      }
    } else {
      simy1[,1] <- pnorm(simtau[,1],simmu1)
      for (j in 2:(cat-1)) {
        simy1[,j] <- pnorm(simtau[,j],simmu1) - apply(simy1[,1:(j-1),drop=FALSE],1,sum)
      }
    }
    simy1[,cat] <- 1-apply(simy1[,1:(cat-1),drop=FALSE],1,sum)

    simy2 <- matrix(NA,nrow=sims,ncol=cat)
    if (!is.na(constant)) {
      simy2[,1] <- pnorm(0,simmu2)
      for (j in 2:(cat-1)) {
        simy2[,j] <- pnorm(simtau[,(j-1)],simmu2) - apply(simy2[,1:(j-1),drop=FALSE],1,sum)
      }
    } else {
      simy2[,1] <- pnorm(simtau[,1],simmu2)
      for (j in 2:(cat-1)) {
        simy2[,j] <- pnorm(simtau[,j],simmu2) - apply(simy2[,1:(j-1),drop=FALSE],1,sum)
      }
    }
    simy2[,cat] <- 1-apply(simy2[,1:(cat-1),drop=FALSE],1,sum)

    for (j in 1:ncol(simy)) {
      simy[,j] <- sort(simy2[,j] - simy1[,j])
    }

    res$pe <- rbind(res$pe,
                    apply(simy,2,mean))

    length.simy <- nrow(simy)
    low <- up <- NULL
    for (k in 1:length(ci)) {
      res$lower[i,,k] <- cbind( low, simy[trunc((1-ci[k])/2*length.simy),] )
      res$upper[i,,k]  <- cbind( up, simy[trunc((1-(1-ci[k])/2)*length.simy),] )
    }
  }
  if (length(ci)==1) {
    if (nrow(x)==1) {
      res$pe <- as.numeric(res$pe)
      res$lower <- as.numeric(res$lower)
      res$upper <- as.numeric(res$upper)
    } else {
      res$lower <- res$lower[,,1,drop=TRUE]
      res$upper <- res$upper[,,1,drop=TRUE]
    }
  }
  res
}


# Simulate risk ratio for ordered probit
#' @export
oprobitsimrr <- function(x,b,ci=0.95,constant=1,cat=3,xpre=NULL) {
  if (cat<3) { stop("cat must be at least 3 for ordered probit") }

  if (any(class(x)=="counterfactual")&&!is.null(x$model)) {
      xpre <- model.matrix(x$model,x$xpre)
      x <- model.matrix(x$model,x$x)
    } else {
        if (any(class(x)=="list")) x <- x$x
        if (any(class(x)=="list")) xpre <- x$xpre
        if (is.data.frame(x)) x <- as.matrix(x)
        if (is.data.frame(xpre)) x <- as.matrix(xpre)
        if (!is.matrix(x)) {
          if (is.matrix(b)) {
            x <- t(x)
            if (!is.na(constant)) {
              x <- append(x,1,constant-1)
            }
          } else {
            x <- as.matrix(x)
            if (!is.na(constant)) {
              x <- appendmatrix(x,rep(1,nrow(x)),constant)
            }
          }
        } else {
          if (!is.na(constant)) {
            x <- appendmatrix(x,rep(1,nrow(x)),constant)
          }
        }
        if (!is.matrix(xpre)) {
          if (is.matrix(b)) {
            xpre <- t(xpre)
            if (!is.na(constant)) {
              xpre <- append(xpre,1,constant-1)
            }
          } else {
            xpre <- as.matrix(xpre)
            if (!is.na(constant)) {
              xpre <- appendmatrix(xpre,rep(1,nrow(xpre)),constant)
            }
          }
        } else {
          if (!is.na(constant)) {
            xpre <- appendmatrix(xpre,rep(1,nrow(xpre)),constant)
          }
        }
    }

  sims <- nrow(as.matrix(b))
  res <- list(pe=NULL,
              lower=array(0, dim = c(nrow(x), cat, length(ci) )),
              upper=array(0, dim = c(nrow(x), cat, length(ci) ))
              )

  for (i in 1:nrow(x)) {
    if (is.na(constant)) {
      simbeta <- b[,1:(ncol(b)-cat+1)]
      simtau <- b[,(ncol(b)-cat+2):ncol(b),drop=FALSE]
    } else {
      simbeta <- b[,1:(ncol(b)-cat+2)]
      simtau <- b[,(ncol(b)-cat+3):ncol(b),drop=FALSE]
    }
    simy <- matrix(NA,nrow=sims,ncol=cat)
    if (is.vector(simbeta)) {
      simmu1 <- as.matrix(simbeta) %*% xpre[i]
      simmu2 <- as.matrix(simbeta) %*% x[i]
    }
    else {
      simmu1 <- simbeta%*%xpre[i,]
      simmu2 <- simbeta%*%x[i,]
    }

    simy1 <- matrix(NA,nrow=sims,ncol=cat)
    if (!is.na(constant)) {
      simy1[,1] <- pnorm(0,simmu1)
      for (j in 2:(cat-1)) {
        simy1[,j] <- pnorm(simtau[,(j-1)],simmu1) - apply(simy1[,1:(j-1),drop=FALSE],1,sum)
      }
    } else {
      simy1[,1] <- pnorm(simtau[,1],simmu1)
      for (j in 2:(cat-1)) {
        simy1[,j] <- pnorm(simtau[,j],simmu1) - apply(simy1[,1:(j-1),drop=FALSE],1,sum)
      }
    }
    simy1[,cat] <- 1-apply(simy1[,1:(cat-1),drop=FALSE],1,sum)

    simy2 <- matrix(NA,nrow=sims,ncol=cat)
    if (!is.na(constant)) {
      simy2[,1] <- pnorm(0,simmu2)
      for (j in 2:(cat-1)) {
        simy2[,j] <- pnorm(simtau[,(j-1)],simmu2) - apply(simy2[,1:(j-1),drop=FALSE],1,sum)
      }
    } else {
      simy2[,1] <- pnorm(simtau[,1],simmu2)
      for (j in 2:(cat-1)) {
        simy2[,j] <- pnorm(simtau[,j],simmu2) - apply(simy2[,1:(j-1),drop=FALSE],1,sum)
      }
    }
    simy2[,cat] <- 1-apply(simy2[,1:(cat-1),drop=FALSE],1,sum)

    for (j in 1:ncol(simy)) {
      simy[,j] <- sort(simy2[,j] / simy1[,j])
    }


      res$pe <- rbind(res$pe,
                      apply(simy,2,mean))
      length.simy <- nrow(simy)
      low <- up <- NULL
      for (k in 1:length(ci)) {
        res$lower[i,,k] <- cbind( low, simy[trunc((1-ci[k])/2*length.simy),] )
        res$upper[i,,k]  <- cbind( up, simy[trunc((1-(1-ci[k])/2)*length.simy),] )
      }
    }
  if (length(ci)==1) {
    if (nrow(x)==1) {
      res$pe <- as.numeric(res$pe)
      res$lower <- as.numeric(res$lower)
      res$upper <- as.numeric(res$upper)
    } else {
      res$lower <- res$lower[,,1,drop=TRUE]
      res$upper <- res$upper[,,1,drop=TRUE]
    }
  }
  res
}




# Simulate expected count for a log-linear model




#' Simulate quantities of interest and confidence intervals for loglinear
#' models
#'
#' Simulate and summarize uncertainty of conditional expected counts, first
#' differences and relative rates from estimated loglinear models, such as the
#' Poisson or Negative Binomial
#'
#' Given simulated parameters from an estimated loglinear model, and
#' counterfactual values of the covariates, these functions calculate either
#' the conditional expected count (\code{loglinsimev}), the conditional first
#' difference (\code{loglinsimfd}), or the conditional relative rate of events
#' (\code{loglinsimrr}) and confidence intervals around these point estimate.
#'
#' If the function you used to estimate the model does not provide simulated
#' parameter values, you can draw often them yourself, e.g., using functions
#' such as \code{\link{vcov}} and \code{mvrnorm} in the \code{MASS} package, as
#' shown below.
#'
#' zelig, in the package Zelig, offers similar features for a wide array of
#' models and with automated handling of the simulation process.  These
#' functions are offered as a simple alternative for users with simulations
#' already in hand.
#'
#' @aliases loglinsimev loglinsimfd loglinsimrr
#' @param x vector, matrix, or list, counterfactual values of the covariates.
#' Include multiple rows to simulate different counterfactual scenarios.  If a
#' list, may contain \code{x} and \code{xpre} as dataframes, a formula object
#' to apply to them, and names for each scenario given as the row names for
#' component \code{x}
#' @param b matrix, simulated parameters, one row per draw from the estimated
#' model, and one column per parameter, including any constants
#' @param ci vector, the requested intervals of the simulated quantity of
#' interest to be reported
#' @param constant scalar, the column of \code{b} containing the model
#' constant, or NA for no constant
#' @param period scalar or vector, the length of the period over which the
#' count is simulated
#' @param labels string vector, names of each scenario (overrides any names in
#' \code{x})
#' @param xpre vector or matrix, counterfactual final values of the covariates.
#' Include multiple rows to simulate different counterfactual scenarios; rows
#' must match \code{x}.  Not needed is \code{x} is a list containing
#' \code{xpre}
#' @return Returns a list with four components \item{pe}{vector, the point
#' estimate(s) of the requested quantity of interest} \item{lower}{matrix, the
#' requested lower bounds around the quantity of interest; rows are scenarios,
#' columns are intervals} \item{upper}{matrix, the requested upper bounds
#' around the quantity of interest; rows are scenarios, columns are intervals}
#' \item{labels}{string vector, the names of each scenario (optional)}
#' @author Christopher Adolph <\email{cadolph@@u.washington.edu}>
#' @keywords models
#' @export
loglinsimev <- function(x,b,ci=0.95,constant=1,period=1) {
  if (any(class(x)=="counterfactual")&&!is.null(x$model)) {
    x <- model.matrix(x$model,x$x)
  } else {
    if (any(class(x)=="list")) x <- x$x
    if (is.data.frame(x)) x <- as.matrix(x)
    if (!is.matrix(x)) {
      if (is.matrix(b)) {
        x <- t(x)
        if (!is.na(constant)) {
          x <- append(x,1,constant-1)
        }
      } else {
        x <- as.matrix(x)
        if (!is.na(constant)) {
          x <- appendmatrix(x,rep(1,nrow(x)),constant)
        }
      }
    } else {
      if (!is.na(constant)) {
        x <- appendmatrix(x,rep(1,nrow(x)),constant)
      }
    }
  }

  esims <- nrow(as.matrix(b))

  while (length(period)<nrow(x)) period <- c(period,period)
    res <- list()
    for (i in 1:nrow(x)) {
        simmu <- b%*%x[i,]
        simy <- exp(period[i]*simmu)
        simy <- sort(simy)
        res$pe <- c(res$pe,mean(simy))
        length.simy <- length(simy)
        low <- up <- NULL
        for (k in 1:length(ci)) {
            low <- c( low,simy[trunc((1-ci[k])/2*length.simy)] )
            up  <- c( up, simy[trunc((1-(1-ci[k])/2)*length.simy)] )
        }
        res$lower <- rbind(res$low,low)
        res$upper <- rbind(res$up,up)
    }
  res
}


# Simulate first difference of expected counts for a log-linear model
#' @export
loglinsimfd <- function(x,b,ci=0.95,constant=1,xpre=NULL,period=1,labels=NULL) {
    if (any(class(x)=="counterfactual")&&!is.null(x$model)) {
        if (is.null(labels))
            labels <- row.names(x$x)
        xpre <- model.matrix(x$model,x$xpre)
        x <- model.matrix(x$model,x$x)
    } else {
        if (any(class(x)=="list")) x <- x$x
        if (any(class(x)=="list")) xpre <- x$xpre
        if (is.data.frame(x)) x <- as.matrix(x)
        if (is.data.frame(xpre)) x <- as.matrix(xpre)
        if (!is.matrix(x)) {
          if (is.matrix(b)) {
            x <- t(x)
            if (!is.na(constant)) {
              x <- append(x,1,constant-1)
            }
          } else {
            x <- as.matrix(x)
            if (!is.na(constant)) {
              x <- appendmatrix(x,rep(1,nrow(x)),constant)
            }
          }
        } else {
          if (!is.na(constant)) {
            x <- appendmatrix(x,rep(1,nrow(x)),constant)
          }
        }
        if (!is.matrix(xpre)) {
          if (is.matrix(b)) {
            xpre <- t(xpre)
            if (!is.na(constant)) {
              xpre <- append(xpre,1,constant-1)
            }
          } else {
            xpre <- as.matrix(xpre)
            if (!is.na(constant)) {
              xpre <- appendmatrix(xpre,rep(1,nrow(xpre)),constant)
            }
          }
        } else {
          if (!is.na(constant)) {
            xpre <- appendmatrix(xpre,rep(1,nrow(xpre)),constant)
          }
        }
    }

  esims <- nrow(as.matrix(b))

    while (length(period)<nrow(x)) period <- c(period,period)
    res <- list()
    for (i in 1:nrow(xpre)) {
        simmu1 <- b%*%xpre[i,]
        simmu2 <- b%*%x[i,]
        simy <- exp(period[i]*simmu2) - exp(period[i]*simmu1)
        simy <- sort(simy)
        res$pe <- c(res$pe,mean(simy))
        length.simy <- length(simy)
        low <- up <- NULL
        for (k in 1:length(ci)) {
            low <- c( low,simy[trunc((1-ci[k])/2*length.simy)] )
            up  <- c( up, simy[trunc((1-(1-ci[k])/2)*length.simy)] )
        }
        res$lower <- rbind(res$low,low)
        res$upper <- rbind(res$up,up)
    }
    if (is.null(labels))
        labels <- 1:length(res$pe)
    res$labels <- labels
    res
}


# Simulate relative rate of expected counts for a log-linear model
#' @export
loglinsimrr <- function(x,b,ci=0.95,constant=1,xpre=NULL,period=1,labels=NULL) {
    if (any(class(x)=="counterfactual")&&!is.null(x$model)) {
        if (is.null(labels))
            labels <- row.names(x$x)
        xpre <- model.matrix(x$model,x$xpre)
        x <- model.matrix(x$model,x$x)
    } else {
        if (any(class(x)=="list")) x <- x$x
        if (any(class(x)=="list")) xpre <- x$xpre
        if (is.data.frame(x)) x <- as.matrix(x)
        if (is.data.frame(xpre)) x <- as.matrix(xpre)
        if (!is.matrix(x)) {
          if (is.matrix(b)) {
            x <- t(x)
            if (!is.na(constant)) {
              x <- append(x,1,constant-1)
            }
          } else {
            x <- as.matrix(x)
            if (!is.na(constant)) {
              x <- appendmatrix(x,rep(1,nrow(x)),constant)
            }
          }
        } else {
          if (!is.na(constant)) {
            x <- appendmatrix(x,rep(1,nrow(x)),constant)
          }
        }
        if (!is.matrix(xpre)) {
          if (is.matrix(b)) {
            xpre <- t(xpre)
            if (!is.na(constant)) {
              xpre <- append(xpre,1,constant-1)
            }
          } else {
            xpre <- as.matrix(xpre)
            if (!is.na(constant)) {
              xpre <- appendmatrix(xpre,rep(1,nrow(xpre)),constant)
            }
          }
        } else {
          if (!is.na(constant)) {
            xpre <- appendmatrix(xpre,rep(1,nrow(xpre)),constant)
          }
        }
      }

  esims <- nrow(as.matrix(b))

    while (length(period)<nrow(x)) period <- c(period,period)
    res <- list()
    for (i in 1:nrow(xpre)) {
        simmu1 <- b%*%xpre[i,]
        simmu2 <- b%*%x[i,]
        simy <- exp(period[i]*simmu2) / exp(period[i]*simmu1)
        simy <- sort(simy)
        res$pe <- c(res$pe,mean(simy))
        length.simy <- length(simy)
        low <- up <- NULL
        for (k in 1:length(ci)) {
            low <- c( low,simy[trunc((1-ci[k])/2*length.simy)] )
            up  <- c( up, simy[trunc((1-(1-ci[k])/2)*length.simy)] )
        }
        res$lower <- rbind(res$low,low)
        res$upper <- rbind(res$up,up)
    }
    if (is.null(labels))
        labels <- 1:length(res$pe)
    res$labels <- labels
    res
}



# Simulate expected probabilities for multinomial logit




#' Simulate quantities of interest and confidence intervals for multinomial
#' logit
#'
#' Simulate and summarize uncertainty of conditional expected values, first
#' differences and relative risks from estimated multinomial logit models
#'
#' Given simulated parameters from an estimated multinomial logit model, and
#' counterfactual values of the covariates, calculate either the conditional
#' expected value of the response (\code{mlogitsimev}), the conditional first
#' difference (\code{mlogitsimfd}), or the relative risk (\code{mlogitsimrr}),
#' and confidence intervals around that point estimate.
#'
#' If the function you used to estimate the model does not provide simulated
#' parameter values, you can draw often them yourself, e.g., using functions
#' such as \code{\link{vcov}} and \code{mvrnorm} in the \code{MASS} package, as
#' shown below.
#'
#' You must provide either observation-specific (\code{x}) or category-specific
#' (\code{z}) covariates, or both, and the appropriate parameters (\code{b},
#' \code{g}, or both, respectively).  Include any
#' observation-and-category-specific covariates in \code{z}.
#'
#' zelig, in the package Zelig, offers similar features for a wide array of
#' models and with automated handling of the simulation process.  These
#' functions are offered as a simple alternative for users with simulations
#' already in hand.
#'
#' @aliases mlogitsimev mlogitsimfd mlogitsimrr
#' @param x vector or matrix, counterfactual values of covariates with
#' category-specific parameters.  Include multiple rows to simulate different
#' counterfactual scenarios
#' @param b 3d array, simulated parameters, one row per draw from the estimated
#' model, one column per parameter, including any constants, and one "sheet"
#' for all categories except the reference category.
#' @param ci vector, the requested intervals of the simulated quantity of
#' interest to be reported
#' @param constant scalar, the column of \code{b} containing the model
#' constant, or NA for no constant
#' @param z 3d array, counterfactual values of covariates specific to the
#' response category.  Include one row for each counterfactual scenario, one
#' column for each parameter, and one "sheet" for each category (do not omit
#' any reference categories here)
#' @param g 3d array, simulated parameters, one row per draw from the estimated
#' model, one column per parameter, and one "sheet" for all categories (do not
#' omit any reference categories here)
#' @param xpre vector or matrix, counterfactual \emph{initial} values of
#' covariates with category-specific parameters.  Include multiple rows to
#' simulate different counterfactual scenarios
#' @param zpre 3d array, counterfactual \emph{initial} values of covariates
#' specific to the response category.  Include one row for each counterfactual
#' scenario, one column for each parameter, and one "sheet" for each category
#' (do not omit any reference categories here)
#' @param zpost 3d array, counterfactual \emph{final} values of covariates
#' specific to the response category.  Include one row for each counterfactual
#' scenario, one column for each parameter, and one "sheet" for each category
#' (do not omit any reference categories here)
#' @return Returns a list with three components \item{pe}{matrix, the point
#' estimates of the requested quantity of interest; rows are scenarios, and
#' columns are categories} \item{lower}{array, the requested lower bounds
#' around the quantity of interest; rows are scenarios, columns are the
#' category of the response, and the third dimension is the confidence
#' interval} \item{upper}{array, the requested upper bounds around the quantity
#' of interest; rows are scenarios, columns are the category of the response,
#' and the third dimension is the confidence interval}
#' @author Christopher Adolph <\email{cadolph@@u.washington.edu}>
#' @seealso \code{\link{logitsimev}}
#' @keywords models
#' @examples
#'
#' # Multinomial Logistic Regression of alligator food
#' # See tile package function lineplot for graphical presentation of this example
#'
#' # Load data and libraries
#' data(gator)
#' require(MASS)
#' require(nnet)
#'
#' # Estimate MNL using the nnet library
#' mlogit.result <- multinom(food ~ size + female, Hess=TRUE)
#' pe <- mlogit.result$wts[c(6,7,8,10,11,12)]
#'                                       # point estimates
#' vc <- solve(mlogit.result$Hess)       # var-cov matrix
#'
#' # Simulate parameters from predictive distributions
#' sims <- 10000
#' simbetas <- mvrnorm(sims,pe,vc)       # draw parameters, using MASS::mvrnorm
#' simb <- array(NA, dim = c(sims,3,2))  # re-arrange simulates to array format
#' simb[,,1] <- simbetas[,1:3]           #   for MNL simulation
#' simb[,,2] <- simbetas[,4:6]
#'
#' # Create full factorial set of counterfactuals
#' sizerange <- seq(1,4,by=0.1)          # range of counterfactual sizes
#' femalerange <- c(0,1)                 # range of counterfactual sexes
#' xhyp <- cfFactorial(size = sizerange, female = femalerange)
#'
#' # Simulate expected probabilities
#' mlogit.qoi1 <- mlogitsimev(xhyp,simb,ci=0.67)
#' print(mlogit.qoi1)
#'
#' @export
mlogitsimev <- function(x,b,ci=0.95,constant=1,z=NULL,g=NULL,predict=FALSE,sims=10) {
    if (!is.array(b)) {
        stop("b must be an array")
    }
    if (any(class(x)=="counterfactual")&&!is.null(x$model)) {
        x <- model.matrix(x$model,x$x)
        x <- array(x,dim=c(nrow(x),ncol(x),dim(b)[3]))
    } else {
        if (any(class(x)=="list")) x <- x$x
        if (is.data.frame(x)) x <- as.matrix(x)
        if (!is.array(x)) {
            if (!is.matrix(x)) {
                x <- t(x)
            }
            x <- array(x,dim=c(nrow(x),ncol(x),dim(b)[3]))
        } else {
            x <- array(x,dim=c(nrow(x),ncol(x),dim(b)[3]))
        }
        if (!is.na(constant)) {
            xnew <- array(NA,dim=c(nrow(x),(ncol(x)+1),dim(b)[3]))
            for (i in 1:dim(x)[3]) {
                xnew[,,i] <- appendmatrix(x[,,i,drop=FALSE],rep(1,dim(x)[1]),constant)
            }
            x <- xnew
        }
    }

    #if (is.data.frame(z)) x <- as.matrix(z)

    if (!is.null(g)) {
        usegamma <- TRUE
    } else {
        usegamma <- FALSE
    }

    if (usegamma&&!is.array(z)) {
        stop("if g is provided, z must be an array with dimension 3 equal to the number of categories")
    }

  esims <- nrow(as.matrix(b))

    res <- list(lower=array(0, dim = c(dim(x)[1],  (dim(x)[3]+1), length(ci) )),
                upper=array(0, dim = c(dim(x)[1],  (dim(x)[3]+1), length(ci) ))
                )

    if (predict) res$pv <- NULL

    for (iscen in 1:dim(x)[1]) {

        # Create denominator for MNL
        simdenom <- 0
        for (icat in 1:(dim(b)[3])) {
            if (usegamma) {
                newdenom <- exp(b[,,icat]%*%x[iscen,,icat] + g%*%z[iscen,,icat])
            } else {
                newdenom <- exp(b[,,icat]%*%x[iscen,,icat])
            }
            simdenom <- simdenom + newdenom
        }
        if (usegamma) {
            simdenom <- simdenom + exp(g%*%z[iscen,,dim(z)[3]])
        } else {
            simdenom <- simdenom + 1
        }


        # Set up placeholders for simulated response
        simy <- matrix(NA,nrow=dim(b)[1],ncol=(dim(b)[3]+1))

        # Calculate simulated probabilities for this scenario
        for (icat in 1:dim(x)[3]) {
            if (usegamma)
                simy[,icat] <- exp(b[,,icat]%*%x[iscen,,icat] + g%*%z[iscen,,icat] )/simdenom
            else
                simy[,icat] <- exp(b[,,icat]%*%x[iscen,,icat])/simdenom
        }
        if (usegamma)
            simy[,ncol(simy)] <- exp(g%*%z[iscen,,dim(g)[3]])/simdenom
        else
            simy[,ncol(simy)] <- 1/simdenom

        # Calculate pe and CI of these probabilities
        simy <- apply(simy,2,sort)
        res$pe <- rbind(res$pe,apply(simy,2,mean))
        length.simy <- nrow(simy)
        low <- up <- NULL

        for (k in 1:length(ci)) {
          for (icat in 1:(dim(b)[3]+1)) {
            res$lower[iscen, icat, k] <- rbind(low, quantile(simy[,icat],
                probs = (1 - ci[k])/2))
            res$upper[iscen, icat, k] <- rbind(up, quantile(simy[,icat],
                probs = (1 - (1 - ci[k])/2)))
          }
        }

        # Simulate predicted values if requested
        if (predict) {
          pv <- NULL
          for (ipred in 1:dim(b)[1]) {
            pv <- c(pv,sample(1:dim(simy)[2], size=sims, prob=simy[ipred,],replace=TRUE))
          }
          res$pv <- rbind(res$pv,pv)

          low <- up <- NULL
          for (k in 1:length(ci)) {
            for (icat in 1:(dim(b)[3]+1)) {
              res$plower[iscen, icat, k] <- rbind(low, quantile(pv[,icat],
                                                                probs = (1 - ci[k])/2))
              res$pupper[iscen, icat, k] <- rbind(up, quantile(pv[,icat],
                                                               probs = (1 - (1 - ci[k])/2)))
            }
          }
        }
      }
    res
}

# Simulate first differences for multinomial logit
#' @export
mlogitsimfd <- function(x,b,ci=0.95,constant=1,xpre=NULL,
                        z=NULL,zpre=NULL,g=NULL) {

    # NEED to add x null case

    if (!is.array(b)) {
        stop("b must be an array")
    }
    if (any(class(x)=="counterfactual")&&!is.null(x$model)) {
      xpre <- model.matrix(x$model,x$xpre)
      x <- model.matrix(x$model,x$x)
      xpre <- array(xpre,dim=c(nrow(xpre),ncol(xpre),dim(b)[3]))
      x <- array(x,dim=c(nrow(x),ncol(x),dim(b)[3]))
    } else {
        if (any(class(x)=="list")) x <- x$x
        if (any(class(x)=="list")) xpre <- x$xpre
        if (is.data.frame(x)) x <- as.matrix(x)
        if (is.data.frame(xpre)) x <- as.matrix(xpre)
        if (!is.array(x)) {
            if (!is.matrix(x)) {
                x <- t(x)
            }
            x <- array(x,dim=c(nrow(x),ncol(x),dim(b)[3]))
        } else {
            x <- array(x,dim=c(nrow(x),ncol(x),dim(b)[3]))
        }
        if (!is.na(constant)) {
            xnew <- array(NA,dim=c(nrow(x),(ncol(x)+1),dim(b)[3]))
            for (i in 1:dim(x)[3]) {
                xnew[,,i] <- appendmatrix(x[,,i,drop=FALSE],rep(1,dim(x)[1]),constant)
            }
            x <- xnew
        }
        if (!is.array(xpre)) {
            if (!is.matrix(xpre)) {
                xpre <- t(xpre)
            }
            xpre <- array(xpre,dim=c(nrow(xpre),ncol(xpre),dim(b)[3]))
        } else {
            xpre <- array(xpre,dim=c(nrow(xpre),ncol(xpre),dim(b)[3]))
        }
        if (!is.na(constant)) {
            xnew <- array(NA,dim=c(nrow(xpre),(ncol(xpre)+1),dim(b)[3]))
            for (i in 1:dim(x)[3]) {
                xnew[,,i] <- appendmatrix(xpre[,,i],rep(1,dim(xpre)[1]),constant)
            }
            xpre <- xnew
        }

    }
    #if (is.data.frame(z)) x <- as.matrix(z)

    if (!is.null(g)) {
        usegamma <- TRUE
    } else {
        usegamma <- FALSE
    }

    if (usegamma&&!is.array(z)) {
        stop("if g is provided, z must be an array with dimension 3 equal to the number of categories")
    }

    if (usegamma&&!is.array(zpre)) {
        stop("if g is provided, zpre must be an array with dimension 3 equal to the number of categories")
    }

    res <- list(lower=array(0, dim = c(dim(x)[1], (dim(x)[3]+1), length(ci) )),
                upper=array(0, dim = c(dim(x)[1], (dim(x)[3]+1), length(ci) ))
                )

    for (iscen in 1:dim(x)[1]) {

        # Create denominator for MNL
        simdenom <- 0
        for (icat in 1:(dim(b)[3])) {
            if (usegamma) {
                newdenom <- exp(b[,,icat]%*%x[iscen,,icat] + g%*%z[iscen,,icat])
            } else {
                newdenom <- exp(b[,,icat]%*%x[iscen,,icat])
            }
            simdenom <- simdenom + newdenom
        }
        if (usegamma) {
            simdenom <- simdenom + exp(g%*%z[iscen,,dim(z)[3]])
        } else {
            simdenom <- simdenom + 1
        }

        # Create pre denominator for MNL
        simdenom0 <- 0
        for (icat in 1:(dim(b)[3])) {
            if (usegamma) {
                newdenom0 <- exp(b[,,icat]%*%xpre[iscen,,icat] + g%*%zpre[iscen,,icat])
            } else {
                newdenom0 <- exp(b[,,icat]%*%xpre[iscen,,icat])
            }
            simdenom0 <- simdenom0 + newdenom0
        }
        if (usegamma) {
            simdenom0 <- simdenom0 + exp(g%*%zpre[iscen,,dim(zpre)[3]])
        } else {
            simdenom0 <- simdenom0 + 1
        }

        # Set up placeholders for simulated response
        simy <- simy0 <- matrix(NA,nrow=dim(b)[1],ncol=(dim(b)[3]+1))

        # Calculate simulated probabilities for this scenario
        for (icat in 1:dim(x)[3]) {
            if (usegamma) {
                simy[,icat] <- exp(b[,,icat]%*%x[iscen,,icat] + g%*%z[iscen,,icat] )/simdenom
                simy0[,icat] <- exp(b[,,icat]%*%xpre[iscen,,icat] + g%*%zpre[iscen,,icat] )/simdenom0
            } else {
                simy[,icat] <- exp(b[,,icat]%*%x[iscen,,icat] + g%*%z[iscen,,icat] )/simdenom
                simy0[,icat] <- exp(b[,,icat]%*%xpre[iscen,,icat] + g%*%zpre[iscen,,icat] )/simdenom0
            }
        }
        if (usegamma) {
            simy[,ncol(simy)] <- exp(g%*%z[iscen,,dim(g)[3]])/simdenom
            simy0[,ncol(simy0)] <- exp(g%*%zpre[iscen,,dim(g)[3]])/simdenom0
        } else {
            simy[,ncol(simy)] <- 1/simdenom
            simy0[,ncol(simy0)] <- 1/simdenom0
        }

        # Calculate pe and CI of these probabilities
        simy <- simy - simy0
        simy <- apply(simy,2,sort)
        res$pe <- rbind(res$pe,apply(simy,2,mean))
        length.simy <- nrow(simy)
        low <- up <- NULL

        for (k in 1:length(ci)) {
          for (icat in 1:(dim(b)[3]+1)) {
            res$lower[iscen, icat, k] <- rbind(low, quantile(simy[,icat],
                probs = (1 - ci[k])/2))
            res$upper[iscen, icat, k] <- rbind(up, quantile(simy[,icat],
                probs = (1 - (1 - ci[k])/2)))
          }
        }
    }
    res
}

# Simulate relative risks for multinomial logit
#' @export
mlogitsimrr <- function(x,b,ci=0.95,constant=1,xpre=NULL,
                        z=NULL,zpre=NULL,g=NULL) {

    # NEED to add x null case

    if (!is.array(b)) {
        stop("b must be an array")
    }
    if (any(class(x)=="counterfactual")&&!is.null(x$model)) {
      xpre <- model.matrix(x$model,x$xpre)
      x <- model.matrix(x$model,x$x)
      xpre <- array(xpre,dim=c(nrow(xpre),ncol(xpre),dim(b)[3]))
      x <- array(x,dim=c(nrow(x),ncol(x),dim(b)[3]))
    } else {
        if (any(class(x)=="list")) x <- x$x
        if (any(class(x)=="list")) xpre <- x$xpre
        if (is.data.frame(x)) x <- as.matrix(x)
        if (is.data.frame(xpre)) x <- as.matrix(xpre)
        if (!is.array(x)) {
            if (!is.matrix(x)) {
                x <- t(x)
            }
            x <- array(x,dim=c(nrow(x),ncol(x),dim(b)[3]))
        } else {
            x <- array(x,dim=c(nrow(x),ncol(x),dim(b)[3]))
        }
        if (!is.na(constant)) {
            xnew <- array(NA,dim=c(nrow(x),(ncol(x)+1),dim(b)[3]))
            for (i in 1:dim(x)[3]) {
                xnew[,,i] <- appendmatrix(x[,,i,drop=FALSE],rep(1,dim(x)[1]),constant)
            }
            x <- xnew
        }
        if (!is.array(xpre)) {
            if (!is.matrix(xpre)) {
                xpre <- t(xpre)
            }
            xpre <- array(xpre,dim=c(nrow(xpre),ncol(xpre),dim(b)[3]))
        } else {
            xpre <- array(xpre,dim=c(nrow(xpre),ncol(xpre),dim(b)[3]))
        }
        if (!is.na(constant)) {
            xnew <- array(NA,dim=c(nrow(xpre),(ncol(xpre)+1),dim(b)[3]))
            for (i in 1:dim(x)[3]) {
                xnew[,,i] <- appendmatrix(xpre[,,i],rep(1,dim(xpre)[1]),constant)
            }
            xpre <- xnew
        }

    }
    #if (is.data.frame(z)) x <- as.matrix(z)

    if (!is.null(g)) {
        usegamma <- TRUE
    } else {
        usegamma <- FALSE
    }

    if (usegamma&&!is.array(z)) {
        stop("if g is provided, z must be an array with dimension 3 equal to the number of categories")
    }

    if (usegamma&&!is.array(zpre)) {
        stop("if g is provided, zpre must be an array with dimension 3 equal to the number of categories")
    }

    res <- list(lower=array(0, dim = c(dim(x)[1], (dim(x)[3]+1),length(ci) )),
                upper=array(0, dim = c(dim(x)[1], (dim(x)[3]+1),length(ci) ))
                )

    for (iscen in 1:dim(x)[1]) {

        # Create denominator for MNL
        simdenom <- 0
        for (icat in 1:(dim(b)[3])) {
            if (usegamma) {
                newdenom <- exp(b[,,icat]%*%x[iscen,,icat] + g%*%z[iscen,,icat])
            } else {
                newdenom <- exp(b[,,icat]%*%x[iscen,,icat])
            }
            simdenom <- simdenom + newdenom
        }
        if (usegamma) {
            simdenom <- simdenom + exp(g%*%z[iscen,,dim(z)[3]])
        } else {
            simdenom <- simdenom + 1
        }

        # Create pre denominator for MNL
        simdenom0 <- 0
        for (icat in 1:(dim(b)[3])) {
            if (usegamma) {
                newdenom0 <- exp(b[,,icat]%*%xpre[iscen,,icat] + g%*%zpre[iscen,,icat])
            } else {
                newdenom0 <- exp(b[,,icat]%*%xpre[iscen,,icat])
            }
            simdenom0 <- simdenom0 + newdenom0
        }
        if (usegamma) {
            simdenom0 <- simdenom0 + exp(g%*%zpre[iscen,,dim(zpre)[3]])
        } else {
            simdenom0 <- simdenom0 + 1
        }

        # Set up placeholders for simulated response
        simy <- simy0 <- matrix(NA,nrow=dim(b)[1],ncol=(dim(b)[3]+1))

        # Calculate simulated probabilities for this scenario
        for (icat in 1:dim(x)[3]) {
            if (usegamma) {
                simy[,icat] <- exp(b[,,icat]%*%x[iscen,,icat] + g%*%z[iscen,,icat] )/simdenom
                simy0[,icat] <- exp(b[,,icat]%*%xpre[iscen,,icat] + g%*%zpre[iscen,,icat] )/simdenom0
            } else {
                simy[,icat] <- exp(b[,,icat]%*%x[iscen,,icat] + g%*%z[iscen,,icat] )/simdenom
                simy0[,icat] <- exp(b[,,icat]%*%xpre[iscen,,icat] + g%*%zpre[iscen,,icat] )/simdenom0
            }
        }
        if (usegamma) {
            simy[,ncol(simy)] <- exp(g%*%z[iscen,,dim(g)[3]])/simdenom
            simy0[,ncol(simy0)] <- exp(g%*%zpre[iscen,,dim(g)[3]])/simdenom0
        } else {
            simy[,ncol(simy)] <- 1/simdenom
            simy0[,ncol(simy0)] <- 1/simdenom0
        }

        # Calculate pe and CI of these probabilities
        simy <- simy / simy0
        simy <- apply(simy,2,sort)
        res$pe <- rbind(res$pe,apply(simy,2,mean))
        length.simy <- nrow(simy)
        low <- up <- NULL

        for (k in 1:length(ci)) {
          for (icat in 1:(dim(b)[3]+1)) {
            res$lower[iscen, icat, k] <- rbind(low, quantile(simy[,icat],
                probs = (1 - ci[k])/2))
            res$upper[iscen, icat, k] <- rbind(up, quantile(simy[,icat],
                probs = (1 - (1 - ci[k])/2)))
          }
        }
    }
    res
}


# FIX THIS BELOW XXX
# Simulate expected values from heteroskedastic normal
#
#' @export
hetnormsimev <- function(x,b,z,g,ci=0.95,constant=1,varconstant=1, predict=TRUE, sims=XXX) {
    if (any(class(x)=="counterfactual")&&!is.null(x$model)) {
        x <- model.matrix(x$model,x$x)
    } else {
        if (any(class(x)=="list")) x <- x$x
        if (is.data.frame(x)) x <- as.matrix(x)
        if (!is.matrix(x)) {
          if (is.matrix(b)) {
            x <- t(x)
            if (!is.na(constant)) {
              x <- append(x,1,constant-1)
            }
          } else {
            x <- as.matrix(x)
            if (!is.na(constant)) {
              x <- appendmatrix(x,rep(1,nrow(x)),constant)
            }
          }
        } else {
          if (!is.na(constant)) {
            x <- appendmatrix(x,rep(1,nrow(x)),constant)
          }
        }
      }

    if (any(class(z)=="counterfactual")&&!is.null(z$model)) {
        z <- model.matrix(z$model,z$x)
    } else {
        if (any(class(z)=="list")) z <- z$x
        if (is.data.frame(z)) z <- as.matrix(z)
        if (!is.matrix(z)) {
          if (is.matrix(g)) {
            z <- t(z)
            if (!is.na(varconstant)) {
              z <- append(z,1,varconstant-1)
            }
          } else {
            z <- as.matrix(z)
            if (!is.na(varconstant)) {
              z <- appendmatrix(z,rep(1,nrow(z)),varconstant)
            }
          }
        } else {
          if (!is.na(constant)) {
            z <- appendmatrix(z,rep(1,nrow(z)),varconstant)
          }
        }
      }

    res <- list()
    for (i in 1:nrow(x)) {
        simmu <- b%*%x[i,]
        simsigma2 <- exp(g%*%z[i,])
        simy <- sqrt(simsigma2)*rnorm(psims)+simmu
        simy <- sort(simy)
        res$pe <- c(res$pe,mean(simy))
        length.simy <- length(simy)
        low <- up <- NULL
        for (k in 1:length(ci)) {
            low <- c( low,simy[trunc((1-ci[k])/2*length.simy)] )
            up  <- c( up, simy[trunc((1-(1-ci[k])/2)*length.simy)] )
        }
        res$lower <- rbind(res$low,low)
        res$upper <- rbind(res$up,up)
    }
  res
}



# Simulate predicted values from heteroskedastic normal


#' Simulate quantities of interest and predictive intervals for heteroskedastic
#' linear models
#'
#' Simulate and summarize uncertainty of conditional predicted values from
#' estimated heteroskedastic linear models
#'
#'
#' In linear regression, the response variable is normally distributed with
#' variable mean but fixed variance across all observations.  In contrast, the
#' linear heteroskedastic model considers a normally distributed response
#' variable whose mean and variance both vary across observations:
#'
#' \deqn{y_i \sim f_\mathrm{Normal}(\mu_i, \sigma_i^2)}{y_i ~ Normal(mu_i,
#' sigma_i^2)}
#'
#' The model contains a mean systematic component,
#'
#' \deqn{\mu_i = x_ib}{mu_i = x_i * b}
#'
#' and a variance systematic component,
#'
#' \deqn{\sigma_i^2 = \mathrm{exp}(z_ig)}{sigma_i^2 = exp(z_i * g)}
#'
#' The heteroskedastic normal model allows covariates to explain both changes
#' in the mean and changes in the variance of the response variable, and can be
#' estimated by maximum likelihood.
#'
#' The functions documented above assume that the user has estimated such a
#' model, and wishes to calculate predicted values of the response for
#' hypothetical values of x and z.
#'
#' \code{hetnormsimpv} takes simulated parameters b and g from an estimated
#' linear heteroskedastic model, and counterfactual values of the covariates
#' for the mean and variance systemtic components (x and z), and calculates the
#' conditional predicted value of the response, including uncertainty intervals
#' around that estimate.
#'
#' You may use \code{cfMake} to initialize \code{counterfactual} objects for
#' \code{x} and \code{z}, or input them directly as matrices.
#'
#' If the function you used to estimate the model does not provide simulated
#' parameter values, you can often draw them yourself, e.g., using functions
#' such as \code{\link{vcov}} and \code{\link{mvrnorm}} in the \pkg{MASS}
#' package, as shown below.
#'
#' \code{\link[Zelig]{zelig}}, in the package \pkg{Zelig}, offers similar features for
#' a wide array of models and with automated handling of the simulation
#' process.  These functions are offered as a simple alternative for users with
#' simulations already in hand.
#'
#' @param x list, a counterfactual object created by \code{cfMake}, or a vector
#' or matrix of counterfactual values of the covariates, including multiple
#' rows to simulate different counterfactual scenarios, and one column for each
#' covariate of the mean systematic component
#' @param b matrix, simulated parameters for the mean systematic component, one
#' row per draw from the estimated model, and one column per parameter,
#' including any constants
#' @param z list, a counterfactual object created by \code{cfMake}, or a vector
#' or matrix of counterfactual values of the covariates, including multiple
#' rows to simulate different counterfactual scenarios, and one column for each
#' covariate of the variance systematic component
#' @param g matrix, simulated parameters for the variance systematic component,
#' one row per draw from the estimated model, and one column per parameter,
#' including any constants
#' @param ci vector, the requested intervals of the simulated quantity of
#' interest to be reported
#' @param constant scalar, the column of \code{b} containing the constant for
#' the mean systematic component, or \code{NA} for no constant
#' @param varconstant scalar, the column of \code{b} containing the constant
#' for the variance systematic component, or \code{NA} for no constant
#' @return Returns a list with components: \item{pe}{vector, the point
#' estimate(s) of the requested quantity of interest} \item{lower}{vector or
#' matrix, the requested lower confidence bounds around the quantity of
#' interest; rows are scenarios, columns are intervals} \item{upper}{vector or
#' matrix, the requested upper confidence bounds around the quantity of
#' interest; rows are scenarios, columns are intervals}
#' @author Christopher Adolph <\email{cadolph@@u.washington.edu}>
#' @seealso \code{\link{cfMake}}, \code{\link{cfChange}}, \code{\link{cfName}},
#' \code{\link{linearsimev}}
#' @keywords models
#' @export
hetnormsimpv <- function(x,b,z,g,ci=0.95,constant=1,varconstant=1) {
    if (any(class(x)=="counterfactual")&&!is.null(x$model)) {
        x <- model.matrix(x$model,x$x)
    } else {
        if (any(class(x)=="list")) x <- x$x
        if (is.data.frame(x)) x <- as.matrix(x)
        if (!is.matrix(x)) {
          if (is.matrix(b)) {
            x <- t(x)
            if (!is.na(constant)) {
              x <- append(x,1,constant-1)
            }
          } else {
            x <- as.matrix(x)
            if (!is.na(constant)) {
              x <- appendmatrix(x,rep(1,nrow(x)),constant)
            }
          }
        } else {
          if (!is.na(constant)) {
            x <- appendmatrix(x,rep(1,nrow(x)),constant)
          }
        }
      }

    if (any(class(z)=="counterfactual")&&!is.null(z$model)) {
        z <- model.matrix(z$model,z$x)
    } else {
        if (any(class(z)=="list")) z <- z$x
        if (is.data.frame(z)) z <- as.matrix(z)
        if (!is.matrix(z)) {
          if (is.matrix(g)) {
            z <- t(z)
            if (!is.na(varconstant)) {
              z <- append(z,1,varconstant-1)
            }
          } else {
            z <- as.matrix(z)
            if (!is.na(varconstant)) {
              z <- appendmatrix(z,rep(1,nrow(z)),varconstant)
            }
          }
        } else {
          if (!is.na(constant)) {
            z <- appendmatrix(z,rep(1,nrow(z)),varconstant)
          }
        }
      }

    res <- list()
    for (i in 1:nrow(x)) {
        simmu <- b%*%x[i,]
        simsigma2 <- exp(g%*%z[i,])
        simy <- sqrt(simsigma2)*rnorm(sims)+simmu
        simy <- sort(simy)
        res$pe <- c(res$pe,mean(simy))
        length.simy <- length(simy)
        low <- up <- NULL
        for (k in 1:length(ci)) {
            low <- c( low,simy[trunc((1-ci[k])/2*length.simy)] )
            up  <- c( up, simy[trunc((1-(1-ci[k])/2)*length.simy)] )
        }
        res$lower <- rbind(res$low,low)
        res$upper <- rbind(res$up,up)
    }
  res
}


# Simulate lagged dependent variable models out to t periods




#' Simulate quantities of interest and confidence intervals for linear time
#' series models including ARIMA or lagged dependent variable processes
#'
#' Simulate and summarize uncertainty of iterated conditional expected values
#' (ev), expected first differences (fd), expected relative risks (rr),
#' predicted values (pv), predicted first differences (pd), and predicted
#' relative risks (pr) from estimated ARIMA or lagged dependent variable time
#' series linear models
#'
#' Given simulated parameters from an estimated linear model with one or more
#' lagged dependent variables, and counterfactual values of the covariates,
#' these functions calculate either the conditional expected value of the
#' response iterated over several periods, or the conditional difference or
#' risk ratio over those periods, and confidence intervals around that point
#' estimate.  Alternatively, the last function calculates predicted values,
#' which include random noise and moving averages of that noise, also iterated
#' over time and with prediction intervals.  This function is thus suitable for
#' forming most desired quantities of interest from time series linear models
#' with ARIMA or lagged dependent variable processes.
#'
#' If the function you used to estimate the model does not provide simulated
#' parameter values, you can draw often them yourself, e.g., using functions
#' such as \code{\link{vcov}} and \code{mvrnorm} in the \code{MASS} package, as
#' shown below.
#'
#' zelig, in the package Zelig, offers similar features for a wide array of
#' models and with automated handling of the simulation process.  These
#' functions are offered as a simple alternative for users with simulations
#' already in hand.
#'
#' @aliases ldvsimev ldvsimfd ldvsimrr ldvsimpv ldvsimpd ldvsimpr
#' @param x vector or matrix, counterfactual values of the covariates.  Include
#' multiple rows to simulate different counterfactual scenarios
#' @param b matrix, simulated parameters, one row per draw from the estimated
#' model, and one column per parameter, including any constants
#' @param ci vector, the requested intervals of the simulated quantity of
#' interest to be reported
#' @param constant scalar, the column of \code{b} containing the model
#' constant, or NA for no constant
#' @param xpre vector or matrix, counterfactual initial values of the
#' covariates.  Include multiple rows to simulate different counterfactual
#' scenarios; rows must match \code{x}; redundant and ignored if \code{x} is a
#' \code{counterfactual object}
#' @param phi scalar of point estimate, or matrix of simulated, AR or lagged DV
#' parameters
#' @param lagY scalar or vector, the prior levels of y or diff(y), most recent
#' first; must match number of columns of \code{phi}
#' @param rho scalar of point estimate, or matrix of simulated, MA parameters
#' @param lagY scalar or vector, the prior levels of the error term, most
#' recent first; must match number of columns of \code{rho}
#' @param sigma scalar of point estimate, or matrix of simulated, white noise
#' parameter or standard error of the regression
#' @param transform string, transformation applied to the dependent variable in
#' the original model; \code{log}, for the natural log, \code{diff}, for
#' differencing; \code{difflog}, for differencing of a logged variable;
#' \code{logit} for the logistic transformation; \code{difflogit} for the
#' differencing of a logit transformed variable; and \code{none} for no
#' transformation (default)
#' @param initialY vector, for differenced models, the original level of the
#' response
#' @param cumulate logical, whether to additionally report cumulative values of
#' the quantity of interest (default is not to report).
#' @param discount scalar, the compounding discount rate to apply to future
#' periods in calculating cumulative quantities of interest; default is 0 for
#' no discounting.  Set as a proportion, so that 0.05 would be a 5 percent
#' discount rate.
#' @return Returns a list with at least four and as many as eight components:
#' \item{pe}{vector, the point estimate(s) of the requested quantity of
#' interest} \item{lower}{matrix, the requested lower bounds around the
#' quantity of interest; rows are scenarios, columns are intervals}
#' \item{upper}{matrix, the requested upper bounds around the quantity of
#' interest; rows are scenarios, columns are intervals} \item{se}{}
#' \item{pe.cumulative}{} \item{lower.cumulative}{} \item{upper.cumulative}{}
#' \item{se.cumulative}{}
#' @author Christopher Adolph <\email{cadolph@@uw.edu}>
#' @keywords models
#' @export
ldvsimev <- function(x,                  # A counterfactual object, or the matrix of hypothetical x's,
                                         #    one for each period
                     b,                  # The matrix of simulated parameters
                     ci=0.95,            # Desired confidence interval
                     constant=1,         # Column containing the constant
                                         # set to NA for no constant
                     phi=NULL,           # Matrix of lag parameters; ncol must match length lagY
                     lagY=NULL,          # prior values of y or diff(y), most recent first;
                                         # length must match ncol of phi
                     transform="none",   # "log" to undo log transformation
                                         # "diff" to convert differenced models to cumulative change
                                         #  from initial period
                                         # "difflog" for both (give initial Y unlogged)
                     initialY=NULL,      # for differenced models, the original *level* of Y:
                                         #  omitting initialY for differenced models will
                                         #  calculate cumulative change over t periods;
                                         #  including initial Y will calculate the new level of Y
                     cumulate=FALSE,     # Record cumulative QoIs as well?
                     discount=0,          # Discount rate to apply to cumulation
                     nscen=1             # ignored unless x is NULL; else number of periods to iterate
                     ) {
  if (is.null(x)) {
    if (is.na(constant))
      stop("either x must be given, or a constant specified")
    else
      x <- matrix(1,nrow=nscen,ncol=1)
  } else {
    if (any(class(x)=="counterfactual")&&!is.null(x$model)) {
      x <- model.matrix(x$model,x$x)
    } else {
      if (any(class(x)=="list")) x <- x$x
      if (is.data.frame(x)) x <- as.matrix(x)
      if (!is.matrix(x)) {
        if (is.matrix(b)) {
          x <- t(x)
          if (!is.na(constant)) {
            x <- append(x,1,constant-1)
          }
        } else {
          x <- as.matrix(x)
          if (!is.na(constant)) {
            x <- appendmatrix(x,rep(1,nrow(x)),constant)
          }
        }
      } else {
        if (!is.na(constant)) {
          x <- appendmatrix(x,rep(1,nrow(x)),constant)
        }
      }
    }
  }

  sims <- nrow(b)
  if (!is.null(phi)) {
    if (is.vector(phi)&&(length(phi)!=sims)) {
      phi <- matrix(phi,nrow=sims,ncol=length(phi),byrow=TRUE)
    }
  }
  t <- nrow(x)


  # Create storage matrices
  yhyp <- seyhyp <- upyhyp <- lwyhyp <- matrix(data=0,nrow=t,ncol=1)
  if (cumulate) {
    ycum <- secum <- upcum <- lwcum <- matrix(data=0,nrow=t,ncol=1)
  }

  os <- matrix(data=1,nrow=sims,ncol=1)
  simy <- matrix(data=NA,nrow=sims,ncol=1)
  if (!is.null(lagY))
    lagY <- matrix(lagY,nrow=sims,ncol=length(lagY),byrow=TRUE)

  if (cumulate)
    cum <- rep(0,sims)

  simysave <- NULL
  countt <- 1
  while (countt<=t) {

    # Update lags
    if (countt>1)
      if (!is.null(phi)&&!is.null(lagY))
        lagY <- cbind(simmu,lagY)[,1:ncol(lagY),drop=FALSE]

    # Compute new simulated Y's
    if (!is.null(phi)&&(!is.null(lagY)))
        arcomponent <- apply(phi * lagY, 1, sum)
    else
      arcomponent <- 0
    simmu <- b%*%t(x[countt,,drop=FALSE]) + arcomponent


    # Transformations of response
    if (transform=="none")
      simy <- simmu

    if (transform=="log") {
      simy <- exp(simmu)
    }

    if (transform=="diff") {
      if (countt>1)
        simy <- simysave[,1] + simmu
      else {
        if (!is.null(initialY))
          simy <- initialY + simmu
        else
          simy <- simmu
      }
    }

    if (transform=="difflog") {
      if (countt>1)
        simy <- exp(log(simysave[,1]) + simmu)
      else
        if (!is.null(initialY))
          simy <- exp(log(initialY) + simmu)
        else
          simy <- exp(simmu)
    }

    if (transform=="logit") {
      simy <- 1/(1+exp(-simmu))
    }

    if (transform=="difflogit") {
      if (countt>1)
        simy <- 1/(1+exp(-log(simysave[,1]/(1-simysave[,1])) -simmu))
      else
        if (!is.null(initialY))
          simy <- 1/(1+exp(-log(initialY/(1-initialY)) -simmu))
        else
          simy <- 1/(1+exp(-simmu))
    }

    if (cumulate) cum <- cum + simy*(1-discount)^countt

    # Save this period result
    simysave <- cbind(simy,simysave)
    simysort <- sort(simy)
    yhyp[countt] <- mean(simy)
    seyhyp[countt] <- sd(simy)
    upyhyp[countt] <- simysort[trunc(sims*((1+ci)/2))]
    lwyhyp[countt] <- simysort[trunc(sims*((1-ci)/2))]

    # Save accumulated result, if requested
    if (cumulate) {
      cumsort <- sort(cum)
      ycum[countt] <- mean(cumsort)
      secum[countt] <- sd(cumsort)
      upcum[countt] <- cumsort[trunc(sims*((1+ci)/2))]
      lwcum[countt] <- cumsort[trunc(sims*((1-ci)/2))]
    }

    # Increment counter
    countt <- countt+1
  }

  # output
  if (cumulate) {
    output <- list(pe=yhyp,
                   lower=lwyhyp,
                   upper=upyhyp,
                   se=seyhyp,
                   pe.cumulative=ycum,
                   lower.cumulative=lwcum,
                   upper.cumulative=upcum,
                   se.cumulative=secum
                   )
  } else {
    output <- list(pe=yhyp,
                   lower=lwyhyp,
                   upper=upyhyp,
                   se=seyhyp
                   )
  }

  output
}


#' @export
ldvsimfd <- function(x,                  # A counterfactual object, or the matrix of hypothetical x's
                                         #   one row per period to simulate
                     b,                  # The matrix of simulated betas
                     ci=0.95,            # Desired confidence interval
                     constant=1,         # Column containing the constant
                                         # set to NA for no constant
                     xpre=NULL,          # The matrix of hypothetical x0's (ignored if x is a counterfactual object)
                     phi=NULL,           # Matrix of simulated AR parameters; ncol must match length lagY
                     lagY=NULL,          # prior values of y or diff(y), most recent first;
                                         # length must match ncol of phi
                     transform="none",   # "log" to undo log transformation
                                         # "diff" to compute difference-in-difference for models of differenced Y
                                         # "difflog" for both (log and diff)
                                         # "logit" if y is logit transformed
                                         # "difflogit" if y is logit transformed and differenced
                     cumulate=FALSE,     # Record cumulative QoIs as well?
                     discount=0          # Discount rate to apply to cumulation
                     ) {

  if (any(class(x)=="counterfactual")&&!is.null(x$model)) {
    xpre <- model.matrix(x$model,x$xpre)
    x <- model.matrix(x$model,x$x)
  } else {
    if (any(class(x)=="list")) x <- x$x
    if (any(class(x)=="list")) xpre <- x$xpre
    if (is.data.frame(x)) x <- as.matrix(x)
    if (is.data.frame(xpre)) x <- as.matrix(xpre)
    if (!is.matrix(x)) {
      if (is.matrix(b)) {
        x <- t(x)
        if (!is.na(constant)) {
          x <- append(x,1,constant-1)
        }
      } else {
        x <- as.matrix(x)
        if (!is.na(constant)) {
          x <- appendmatrix(x,rep(1,nrow(x)),constant)
        }
      }
    } else {
      if (!is.na(constant)) {
        x <- appendmatrix(x,rep(1,nrow(x)),constant)
      }
    }
    if (!is.matrix(xpre)) {
      if (is.matrix(b)) {
        xpre <- t(xpre)
        if (!is.na(constant)) {
          xpre <- append(xpre,1,constant-1)
        }
      } else {
        xpre <- as.matrix(xpre)
        if (!is.na(constant)) {
              xpre <- appendmatrix(xpre,rep(1,nrow(xpre)),constant)
            }
      }
    } else {
      if (!is.na(constant)) {
        xpre <- appendmatrix(xpre,rep(1,nrow(xpre)),constant)
      }
    }
  }

  res <- list()
  sims <- nrow(b)
  if (!is.null(phi)) {
    if (is.vector(phi)&&(length(phi)!=sims)) {
      phi <- matrix(phi,nrow=sims,ncol=length(phi),byrow=TRUE)
    }
  }
  t <- nrow(x)

    # Create storage matrices
  yhyp <- seyhyp <- upyhyp <- lwyhyp <- matrix(data=0,nrow=t,ncol=1)
  if (cumulate) {
    ycum <- secum <- upcum <- lwcum <- matrix(data=0,nrow=t,ncol=1)
  }
  os <- matrix(data=1,nrow=sims,ncol=1)
  simy <- simy0 <- matrix(data=NA,nrow=sims,ncol=1)
  if (!is.null(lagY)) {
    lagY <- matrix(lagY,nrow=sims,ncol=length(lagY),byrow=TRUE)
    lagY0 <- lagY
  }

  if (cumulate)
    cum <- rep(0,sims)

  simysave <- NULL
  countt <- 1
  while (countt<=t) {

                                        # Update lags
    if (countt>1)
      if (!is.null(phi)&&!is.null(lagY)) {
        lagY <- cbind(simmu,lagY)[,1:ncol(lagY),drop=FALSE]
        lagY0 <- cbind(simmu0,lagY0)[,1:ncol(lagY0),drop=FALSE]
      }

                                        # Compute new simulated Y's
    if (!is.null(phi)&&(!is.null(lagY))) {
      arcomponent <- apply(phi * lagY, 1, sum)
      arcomponent0 <- apply(phi * lagY0, 1, sum)
    } else {
      arcomponent <- 0
      arcomponent0 <- 0
    }
    simmu <- b%*%t(x[countt,,drop=FALSE]) + arcomponent
    simmu0 <- b%*%t(xpre[countt,,drop=FALSE]) + arcomponent0

    # transformation of response
    if (transform=="none")
      simy <- simmu - simmu0

    if (transform=="log") {
      simy <- exp(simmu) - exp(simmu0)
    }

    if (transform=="diff") {
      if (countt>1)
        simy <- simmu - simmu0
      else
        simy <- simmu - simmu0
    }

    if (transform=="difflog") {
      if (countt>1)
        simy <- exp(simmu) - exp(simmu0)
      else
        simy <- exp(simmu) - exp(simmu0)
    }

    if (transform=="logit") {
      simy <- 1/(1+exp(-simmu)) - 1/(1+exp(-simmu0))
    }

    if (transform=="difflogit") {
      if (countt>1)
        simy <- 1/(1+exp(-simmu)) - 1/(1+exp(-simmu0))
      else
       simy <- 1/(1+exp(-simmu)) - 1/(1+exp(-simmu0))
    }

    if (cumulate) cum <- cum + simy*(1-discount)^countt

    # Save this period result
    simysave <- cbind(simy,simysave)
    simysort <- sort(simy)
    yhyp[countt] <- mean(simy)
    seyhyp[countt] <- sd(simy)
    upyhyp[countt] <- simysort[trunc(sims*((1+ci)/2))]
    lwyhyp[countt] <- simysort[trunc(sims*((1-ci)/2))]

    # Save accumulated result, if requested
    if (cumulate) {
      cumsort <- sort(cum)
      ycum[countt] <- mean(cumsort)
      secum[countt] <- sd(cumsort)
      upcum[countt] <- cumsort[trunc(sims*((1+ci)/2))]
      lwcum[countt] <- cumsort[trunc(sims*((1-ci)/2))]
    }

    # Increment counter
    countt <- countt+1
  }

  # output
  if (cumulate) {
    output <- list(pe=yhyp,
                   lower=lwyhyp,
                   upper=upyhyp,
                   se=seyhyp,
                   pe.cumulative=ycum,
                   lower.cumulative=lwcum,
                   upper.cumulative=upcum,
                   se.cumulative=secum
                   )
  } else {
    output <- list(pe=yhyp,
                   lower=lwyhyp,
                   upper=upyhyp,
                   se=seyhyp
                   )
  }

  output
}




#' @export
ldvsimrr <- function(x,                  # A counterfactual object, or the matrix of hypothetical x's
                                         #   one row per period to simulate
                     b,                  # The matrix of simulated betas
                     ci=0.95,            # Desired confidence interval
                     constant=1,         # Column containing the constant
                                         # set to NA for no constant
                     xpre=NULL,          # The matrix of hypothetical x0's (ignored if x is a counterfactual object)
                     phi=NULL,           # Matrix of simulated AR parameters; ncol must match length lagY
                     lagY=NULL,          # prior values of y or diff(y), most recent first;
                                         # length must match ncol of phi
                     transform="none",   # "log" to undo log transformation
                                         # "diff" to compute difference-in-difference for models of differenced Y
                                         # "difflog" for both (log and diff)
                                         # "logit" if y is logit transformed
                                         # "difflogit" if y is logit transformed and differenced
                     cumulate=FALSE,     # Record cumulative QoIs as well?
                     discount=0          # Discount rate to apply to cumulation
                     ) {

  if (any(class(x)=="counterfactual")&&!is.null(x$model)) {
    xpre <- model.matrix(x$model,x$xpre)
    x <- model.matrix(x$model,x$x)
  } else {
        if (any(class(x)=="list")) x <- x$x
        if (any(class(x)=="list")) xpre <- x$xpre
        if (is.data.frame(x)) x <- as.matrix(x)
        if (is.data.frame(xpre)) x <- as.matrix(xpre)
        if (!is.matrix(x)) {
          if (is.matrix(b)) {
            x <- t(x)
            if (!is.na(constant)) {
              x <- append(x,1,constant-1)
            }
          } else {
            x <- as.matrix(x)
            if (!is.na(constant)) {
              x <- appendmatrix(x,rep(1,nrow(x)),constant)
            }
          }
        } else {
          if (!is.na(constant)) {
            x <- appendmatrix(x,rep(1,nrow(x)),constant)
          }
        }
        if (!is.matrix(xpre)) {
          if (is.matrix(b)) {
            xpre <- t(xpre)
            if (!is.na(constant)) {
              xpre <- append(xpre,1,constant-1)
            }
          } else {
            xpre <- as.matrix(xpre)
            if (!is.na(constant)) {
              xpre <- appendmatrix(xpre,rep(1,nrow(xpre)),constant)
            }
          }
        } else {
          if (!is.na(constant)) {
            xpre <- appendmatrix(xpre,rep(1,nrow(xpre)),constant)
          }
        }
      }

  res <- list()
  sims <- nrow(b)
  if (!is.null(phi)) {
    if (is.vector(phi)&&(length(phi)!=sims)) {
      phi <- matrix(phi,nrow=sims,ncol=length(phi),byrow=TRUE)
    }
  }
  t <- nrow(x)

  # Create storage matrices
  yhyp <- seyhyp <- upyhyp <- lwyhyp <- matrix(data=0,nrow=t,ncol=1)
  if (cumulate) {
    ycum <- secum <- upcum <- lwcum <- matrix(data=0,nrow=t,ncol=1)
  }
  os <- matrix(data=1,nrow=sims,ncol=1)
  simy <- simy0 <- matrix(data=NA,nrow=sims,ncol=1)
  if (!is.null(lagY)) {
    lagY <- matrix(lagY,nrow=sims,ncol=length(lagY),byrow=TRUE)
    lagY0 <- lagY
  }

  if (cumulate)
    cum <- cum0 <- rep(0,sims)

  simysave <- NULL
  countt <- 1
  while (countt<=t) {

                                        # Update lags
    if (countt>1)
      if (!is.null(phi)&&!is.null(lagY)) {
        lagY <- cbind(simmu,lagY)[,1:ncol(lagY),drop=FALSE]
        lagY0 <- cbind(simmu0,lagY0)[,1:ncol(lagY0),drop=FALSE]
      }

                                        # Compute new simulated Y's
    if (!is.null(phi)&&(!is.null(lagY))) {
      arcomponent <- apply(phi * lagY, 1, sum)
      arcomponent0 <- apply(phi * lagY0, 1, sum)
    } else {
      arcomponent <- 0
      arcomponent0 <- 0
    }
    simmu <- b%*%t(x[countt,,drop=FALSE]) + arcomponent
    simmu0 <- b%*%t(xpre[countt,,drop=FALSE]) + arcomponent0

    # transformation of response
    if (transform=="none") {
      simy <- simmu / simmu0
      if (cumulate) {
        simyPE <- simmu
        simyPE0 <- simmu0
      }
    }

    if (transform=="log") {
      simy <- exp(simmu) / exp(simmu0)
      if (cumulate) {
        simyPE <- exp(simmu)
        simyPE0 <- exp(simmu0)
      }
    }

    if (transform=="diff") {
      if (countt>1) {
        simy <- (simysave[,1] + simmu) / (simysave[,1] + simmu0)
        if (cumulate) {
          simyPE <- simysave[,1] + simmu
          simyPE0 <- simysave[,1] + simmu0
        }
      } else {
        simy <- simmu / simmu0
        if (cumulate) {
          simyPE <- simmu
          simyPE0 <- simmu0
        }
      }
    }

    if (transform=="difflog") {
      if (countt>1) {
        simy <- (simysave[,1] + exp(simmu)) / (simysave[,1] + exp(simmu0))
        if (cumulate) {
          simyPE <- simysave[,1] + exp(simmu)
          simyPE0 <- simysave[,1] + exp(simmu0)
        }
      } else {
        simy <- exp(simmu) / exp(simmu0)
        if (cumulate) {
          simyPE <- exp(simmu)
          simyPE0 <- exp(simmu0)
        }
      }
    }

    if (transform=="logit") {
      simy <- 1/(1+exp(-simmu)) / 1/(1+exp(-simmu0))
      if (cumulate) {
        simyPE <- 1/(1+exp(-simmu))
        simyPE0 <- 1/(1+exp(-simmu0))
      }
    }

    if (transform=="difflogit") {
      if (countt>1) {
        simy <- (simysave[,1] + 1/(1+exp(-simmu))) / (simysave[,1] + 1/(1+exp(-simmu0)))
        if (cumulate) {
          simyPE <- simysave[,1] + 1/(1+exp(-simmu))
          simyPE0 <- simysave[,1] + 1/(1+exp(-simmu0))
        }
      } else {
        simy <- 1/(1+exp(-simmu)) / 1/(1+exp(-simmu0))
        if (cumulate) {
          simyPE <- 1/(1+exp(-simmu))
          simyPE0 <- 1/(1+exp(-simmu0))
        }
      }
    }

    if (cumulate) {
      cum <- cum + simyPE*(1-discount)^countt
      cum0 <- cum0 + simyPE0*(1-discount)^countt
    }

    # Save this period result
    simysave <- cbind(simy,simysave)
    simysort <- sort(simy)
    yhyp[countt] <- mean(simy)
    seyhyp[countt] <- sd(simy)
    upyhyp[countt] <- simysort[trunc(sims*((1+ci)/2))]
    lwyhyp[countt] <- simysort[trunc(sims*((1-ci)/2))]

    # Save accumulated result, if requested
    if (cumulate) {
      cumsort <- sort(cum/cum0)
      ycum[countt] <- mean(cumsort)
      secum[countt] <- sd(cumsort)
      upcum[countt] <- cumsort[trunc(sims*((1+ci)/2))]
      lwcum[countt] <- cumsort[trunc(sims*((1-ci)/2))]
    }

    # Increment counter
    countt <- countt+1
  }

  # output
  if (cumulate) {
    output <- list(pe=yhyp,
                   lower=lwyhyp,
                   upper=upyhyp,
                   se=seyhyp,
                   pe.cumulative=ycum,
                   lower.cumulative=lwcum,
                   upper.cumulative=upcum,
                   se.cumulative=secum
                   )
  } else {
    output <- list(pe=yhyp,
                   lower=lwyhyp,
                   upper=upyhyp,
                   se=seyhyp
                   )
  }

  output
}



#' @export
ldvsimpv <- function(x,                  # A counterfactual object, or the matrix of hypothetical x's,
                                         #    one for each period
                     b,                  # The matrix of simulated parameters
                     ci=0.95,            # Desired confidence interval
                     constant=1,         # Column containing the constant
                                         # set to NA for no constant
                     phi=NULL,           # Matrix of AR parameters; ncol must match length lagY
                     lagY=NULL,          # prior values of y or diff(y), most recent first;
                                         # length must match ncol of phi
                     rho=NULL,           # Matrix of MA parameters; ncol must match length lagEps
                     lagEps=NULL,        # prior values of error term, most recent first;
                                         # length much match ncol of rho
                     sigma=NULL,         # scalar; the std err of the regression
                     transform="none",   # "log" to undo log transformation
                                         # "diff" to convert differenced models to cumulative change
                                         #  from initial period
                                         # "difflog" for both (give initial Y unlogged)
                     initialY=NULL,      # for differenced models, the original *level* of Y:
                                         #  omitting initialY for differenced models will
                                         #  calculate cumulative change over t periods;
                                         #  including initial Y will calculate the new level of Y
                     cumulate=FALSE,     # Record cumulative QoIs as well?
                     discount=0,          # Discount rate to apply to cumulation
                     nscen=1             # ignored unless x is NULL; else number of periods to iterate
                     ) {
  if (is.null(x)) {
    if (is.na(constant))
      stop("either x must be given, or a constant specified")
    else
      x <- matrix(1,nrow=nscen,ncol=1)
  } else {
    if (any(class(x)=="counterfactual")&&!is.null(x$model)) {
      x <- model.matrix(x$model,x$x)
    } else {
      if (any(class(x)=="list")) x <- x$x
      if (is.data.frame(x)) x <- as.matrix(x)
      if (!is.matrix(x)) {
        if (is.matrix(b)) {
          x <- t(x)
          if (!is.na(constant)) {
            x <- append(x,1,constant-1)
          }
        } else {
          x <- as.matrix(x)
          if (!is.na(constant)) {
            x <- appendmatrix(x,rep(1,nrow(x)),constant)
          }
        }
      } else {
        if (!is.na(constant)) {
          x <- appendmatrix(x,rep(1,nrow(x)),constant)
        }
      }
    }
  }

  sims <- nrow(b)
  if (!is.null(phi)) {
    if (is.vector(phi)&&(length(phi)!=sims)) {
      phi <- matrix(phi,nrow=sims,ncol=length(phi),byrow=TRUE)
    }
  }
  if (!is.null(rho)) {
    if (is.vector(rho)&&(length(rho)!=sims)) {
      rho <- matrix(rho,nrow=sims,ncol=length(rho),byrow=TRUE)
    }
  }
  t <- nrow(x)


  # Create storage matrices
  yhyp <- seyhyp <- upyhyp <- lwyhyp <- matrix(data=0,nrow=t,ncol=1)
  if (cumulate) {
    ycum <- secum <- upcum <- lwcum <- matrix(data=0,nrow=t,ncol=1)
  }
  os <- matrix(data=1,nrow=sims,ncol=1)
  simy <- matrix(data=NA,nrow=sims,ncol=1)
  if (!is.null(lagY))
    lagY <- matrix(lagY,nrow=sims,ncol=length(lagY),byrow=TRUE)
  if (!is.null(lagEps))
    lagEps <- matrix(lagEps,nrow=sims,ncol=length(lagEps),byrow=TRUE)

  if (cumulate)
    cum <- rep(0,sims)

  simysave <- NULL
  countt <- 1
  while (countt<=t) {

    # Update lags
    if (countt>1) {
      if (!is.null(phi)&&!is.null(lagY))
        lagY <- cbind(simmu,lagY)[,1:ncol(lagY),drop=FALSE]
      if (!is.null(rho)&&!is.null(lagEps))
        lagEps <- cbind(simsigma,lagEps)[,1:ncol(lagEps),drop=FALSE]
    }

    # Compute new simulated Y's
    if (!is.null(phi)&&(!is.null(lagY)))
        arcomponent <- apply(phi * lagY, 1, sum)
    else
      arcomponent <- 0
    if (!is.null(rho)&&(!is.null(lagEps)))
        macomponent <- apply(rho * lagEps, 1, sum)
    else
      macomponent <- 0
    simsigma <- rnorm(sims,sd=sigma)
    simmu <- b%*%t(x[countt,,drop=FALSE]) + arcomponent + macomponent + simsigma


    # Transformations of response
    if (transform=="none")
      simy <- simmu

    if (transform=="log") {
      simy <- exp(simmu)
    }

    if (transform=="diff") {
      if (countt>1)
        simy <- simysave[,1] + simmu
      else {
        if (!is.null(initialY))
          simy <- initialY + simmu
        else
          simy <- simmu
      }
    }

    if (transform=="difflog") {
      if (countt>1)
        simy <- exp(log(simysave[,1]) + simmu)
      else
        if (!is.null(initialY))
          simy <- exp(log(initialY) + simmu)
        else
          simy <- exp(simmu)
    }

    if (transform=="logit") {
      simy <- 1/(1+exp(-simmu))
    }

    if (transform=="difflogit") {
      if (countt>1)
        simy <- 1/(1+exp(-log(simysave[,1]/(1-simysave[,1])) -simmu))
      else
        if (!is.null(initialY))
          simy <- 1/(1+exp(-log(initialY/(1-initialY)) -simmu))
        else
          simy <- 1/(1+exp(-simmu))
    }

    if (cumulate) cum <- cum + simy*(1-discount)^countt

    # Save this period result
    simysave <- cbind(simy,simysave)
    simysort <- sort(simy)
    yhyp[countt] <- mean(simy)
    seyhyp[countt] <- sd(simy)
    upyhyp[countt] <- simysort[trunc(sims*((1+ci)/2))]
    lwyhyp[countt] <- simysort[trunc(sims*((1-ci)/2))]

    # Save accumulated result, if requested
    if (cumulate) {
      cumsort <- sort(cum)
      ycum[countt] <- mean(cumsort)
      secum[countt] <- sd(cumsort)
      upcum[countt] <- cumsort[trunc(sims*((1+ci)/2))]
      lwcum[countt] <- cumsort[trunc(sims*((1-ci)/2))]
    }

    # Increment counter
    countt <- countt+1
  }

  # output
  if (cumulate) {
    output <- list(pe=yhyp,
                   lower=lwyhyp,
                   upper=upyhyp,
                   se=seyhyp,
                   pe.cumulative=ycum,
                   lower.cumulative=lwcum,
                   upper.cumulative=upcum,
                   se.cumulative=secum
                   )
  } else {
    output <- list(pe=yhyp,
                   lower=lwyhyp,
                   upper=upyhyp,
                   se=seyhyp
                   )
  }

  output
}

