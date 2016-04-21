# Test code for function below
#baseline <- 1
#limits <- matrix(c(0.00001, 0.0001, 0.001, 0.01, 0.1, 0.2, 0.5, 0.7, 0.8, 0.9, 0.95,
#                   100000,  10000,  1000,  100,  10,  2, 1.5, 1.3, 1.2, 1.1, 1.05),
#                 ncol=2)

#for (i in 1:nrow(limits))
#  print(c(limits[i,],"Mirror RR", mirrorRR(limits[i,],baseline)))



#' Label a relative risk axis on a plot of probabilities
#' 
#' If given probabilities which the user wishes to plot, suggests tick marks
#' and labels for axes showing relative risks across the range of probabilities
#' with respect to a reference level
#' 
#' 
#' @param limits
#' @param reference
#' @param type
#' @param fd
#' @param cut
#' @param base
#' @return A list object containing \code{at}, a vector of locations for the
#' relative risk tick marks, and \code{labels}, a vector of labels to plot at
#' those tick marks.
#' @author Christopher Adolph \email{cadolph@@uw.edu}
#' @seealso \link{tile}
#' @keywords dplot
#' @export mirrorRR
mirrorRR <- function(limits, reference, type="auto", fd=TRUE,
                     # types:  hundredths, tenths, quarters, halves, units, logscale, powers
                     cut=c(1.175, 1.4, 2, 3.5, 11, 50, 500),
                     # note that log(log(cut,base=10),base=10) is linear in (1:6)
                     base=NULL) {

  # Demonstrate log-log principle behind cutpoints of dynamic range
  # plot(log(log(cut,base=10),base=10))
  
  if (is.null(base)) base <- exp(1)
  
  limits <- sort(limits)
  mag <- limits[2]/limits[1]
  if (mag<=0) stop("powersAxis requires strictly positive axis range")
  minFact <- limits[1]/reference
  maxFact <- limits[2]/reference

  if ((type=="hundredths")||(mag<cut[1])) {  # Mark by 0.01x changes
    startFact <- ceiling(minFact*100)/100
    endFact <- floor(maxFact*100)/100
    atFact <- seq(startFact,endFact,by=0.01)
  }

  if ((type=="twentieths")||((mag>=cut[1])&&(mag<cut[2]))) {  # Mark by 0.05x changes
    startFact <- ceiling(minFact*20)/20
    endFact <- floor(maxFact*20)/20
    atFact <- seq(startFact,endFact,by=0.05)
  }
  
  if ((type=="tenths")||((mag>=cut[2])&&(mag<cut[3]))) {  # Mark by 0.1x changes
    startFact <- ceiling(minFact*10)/10
    endFact <- floor(maxFact*10)/10
    atFact <- seq(startFact,endFact,by=0.1)
  }
  
  if ((type=="quarters")||((mag>=cut[3])&&(mag<cut[4]))) {  # Mark by 0.25x changes
    startFact <- ceiling(minFact*4)/4
    endFact <- floor(maxFact*4)/4
    atFact <- seq(startFact,endFact,by=0.25)
  }

  if ((type=="halves")||((mag>=cut[4])&&(mag<cut[5]))) {  # Mark by 0.5x changes
    startFact <- ceiling(minFact*2)/2
    endFact <- floor(maxFact*2)/2
    print(startFact)
    print(endFact)
    atFact <- seq(startFact,endFact,by=0.5)
  }

  if ((type=="units")||((mag>=cut[5])&&(mag<cut[6]))) {  # Mark by 1x changes
    startFact <- ceiling(minFact)
    endFact <- floor(maxFact)
    atFact <- seq(startFact,endFact,by=1)
  }


  if ((type=="logscale")||((mag>=cut[6])&&(mag<cut[7]))) {  # Mark by 1x, 2x, 5x changes
    atFact <- 1
    done <- FALSE
    currbase <- 1
    while (!done) {
      cand <- currbase/c(2,5,10)
      validcand <- cand[cand>=minFact]
      atFact <- c(sort(validcand),atFact)
      if (length(validcand)<3) done <- TRUE
      currbase <- min(atFact)
    }

    done <- FALSE
    currbase <- 1
    while (!done) {
      cand <- currbase*c(2,5,10)
      validcand <- cand[cand<=maxFact]
      atFact <- c(atFact,sort(validcand))
      if (length(validcand)<3) done <- TRUE
      currbase <- max(atFact)
    }
  }

  if ((type=="powers")||(mag>=cut[7])) {  # Mark by 1x, 10x, 100x changes  
    startFact <- ceiling(log(minFact,base=10))
    endFact <- floor(log(maxFact,base=10))
    atFact <- 10^seq(startFact,endFact,by=1)
  }

  if (fd) {
    at <- reference*atFact - reference
  } else {
    at <- reference*atFact
  }
  labels <- paste(atFact,"x",sep="")
  list(at=at,labels=labels)
}
