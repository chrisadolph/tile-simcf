library(Zelig)	
library(survey)

#We have different scenarios

#zeligtoRopeLadder

#There is a summary.zelig function which is hidden and can only be obtained from source code 
#that calculates the confidence intervals and returns the desired statistics from Zelig
#ZeligTile should to this in a more transparent way, since almost no one will ever use summary.zelig
#to change the defaults

#In principle, the way the Zelig manual is set up, first differences will often be calculated one at a time
#That is, there will be lots of different objects containing different simulations of first differences
#Zelig Tile should handle both this and when they are all simluated at once. I need HELP with this. I believe 
#I need the ... at the beginning because this is key to the functioning of tile but I thought i would ask
#for help up front

#can be used with or without simulates -- Needs to provide different functionality in the two cases
#can be used with one confidence interval or many

#Should work with auto.ropeladder for first differences?


boundsCI <- function(conf.int){
	  cip <- c((1-conf.int)/2, 1-(1-conf.int)/2)
	return(cip)
}

#need to work for arrays
calcCI <- function(data, conf.int){
	probs <- sort(do.call("boundsCI", list(conf.int)))
	simCI <- t(apply(data, MARGIN = 2, FUN = quantile, probs = probs))
	lower <- simCI[ , 1:(ncol(simCI)/2)]
	upper <- simCI[ , ((ncol(simCI)/2)+1):(ncol(simCI))]
	res <- list(lower=lower, upper=upper)
	return(res)	
}

zeligTile <- function(sims, conf.int = .95, names = NULL, type = "ev", simulates = FALSE){
		
	#determine all simulations are of class zelig
	#if they are extract means and confidence intervals when simulates aren't desired
	#otherwise return simulates
	#We can also return more than means if we think that is useful 
	
	if (any(class(sims) != "zelig") || is.null(sims)) stop("You must give zeligTile a object of class zelig")
	else {
		if (simulates == FALSE){
			pe <- apply(sims$qi[[type]], 2, mean)
			#res <- c(res, do.call(stats[i], list(x)))
			#print(means)
			#print(class(sims))
			cis <-  calcCI(sims$qi[[type]], conf.int = conf.int)
		}
	#what else do I return here/How do I deal with names 
	#would it be useful to be able to store the value sequence here
	res <- list(pe=pe, lower = cis[["lower"]], upper = cis[["upper"]])
	return(res)
	}	
}

#this is the Zelig code that is called by summary.zelig
summarize.default <- function(x, rows = NULL, cip, stats, subset = NULL) {
  res <- NULL
  if (is.numeric(x)) {
    for (i in 1:length(stats))
      res <- c(res, do.call(stats[i], list(x)))
    res <- c(res, quantile(x, cip, na.rm=TRUE))
    names(res) <- c(stats, paste(cip*100, "%", sep = ""))
  }
  else if (is.character(x)) {
    res <- c(table(x) / length(x))
  }
  res
}

#quantities of interest is a list itself and each part of it can be extractedF
sMulti$qi[["fd"]])

#test

#use some Zelig code
data(api, package = "survey")

z.out1 <- zelig(enroll ~ api99 + yr.rnd, model = "poisson.survey",	weights = ~pw, data = apistrat)
x.low <- setx(z.out1, api99 = quantile(apistrat$api99, 0.2))
x.high <- setx(z.out1, api99 = quantile(apistrat$api99, 0.8))
x.low1 <- setx(z.out1, api99 = quantile(apistrat$api99, 0.1))
x.high1 <- setx(z.out1, api99 = quantile(apistrat$api99, 0.9))

xmultiLow <- setx(z.out1, api99 = c(quantile(apistrat$api99, 0.2), quantile(apistrat$api99, 0.1)))
xmultiHigh <- setx(z.out1, api99 = c(quantile(apistrat$api99, 0.8), quantile(apistrat$api99, 0.9)))
sMulti <- sim(z.out1, x = xmultiLow, x1 = xmultiHigh)

s.out1 <- sim(z.out1, x = x.low, x1 = x.high)
s.out2 <- sim(z.out1, x = x.low1, x1 = x.high1)

sMulti <- sim(z.out1, x = xmultiLow, x1 = xmultiHigh)
xlineTile <- setx(z.out1, api99=383:890)
apiLineTile <- sim(z.out1, x = xlineTile)


data(sanction)
sanction$ncost <- factor(sanction$ncost, ordered = TRUE, levels = c("net gain", "little effect", "modest loss", "major loss"))
z.out <- zelig(ncost ~ mil + coop, model = "ologit", data = sanction)
x.out <- setx(z.out, coop =1:4)
s.out <- sim(z.out, x = x.out)

#test resutls
calcCI(sMulti$qi$fd, .95)
test <- zeligTile(sMulti, conf.int = c(.9, .95, .99))
zeligTile(s.out1, conf.int = c(.9, .95, .99))
zeligTile(apiLineTile)

trace1 <- 




#zeligtoLineplot

#zeligtoMatrix