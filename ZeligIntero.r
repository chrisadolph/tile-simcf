library(Zelig)	
library(survey)

#We have different scenarios

#zeligtoRopeLadder

#There is a summary.zelig function which is hidden and can only be obtained from source code 
#that calculates the confidence intervals and returns the desired statistics from Zelig
#ZeligTile should to this in a more transparent way, since almost no one will ever use summary.zelig
#to change the defaults
#currently there is a slight difference between what CalcCI returns and what summary.zelig returns. 
#I need to hunt this down. 

#In principle, the way the Zelig manual is set up, first differences will often be calculated one at a time
#That is, there will be lots of different objects containing different simulations of first differences
#Zelig Tile should handle both this and when they are all simluated at one. I need HELP with this. I believe 
#I need the ... at the beginning because this is key to the functioning of tile but I thought i would ask
#for help up pront

#can be used with or without simulates -- Needs to provide different functionality in the two cases
#can be used with one confidence interval or many unlike summary.zelig

#Should work with auto.ropeladder for first differences?


zeligTile <- function(sims, conf.int = .95, names = NULL, type = "fd", simulates = FALSE){
		
	#determine all simulations are of class zelig
	#if they are extract means and confidence intervals when simulates aren't desired
	#otherwise return simulates
	#We can also return more than means if we think that is useful 
	if (type == "fd" && simulates == FALSE){
		means <- apply(sims$qi$fd, 2, mean)
		print(means)
		print(class(sims))
		cis <-  calcCI(sims$qi$fd, conf.int = conf.int)
	}
	
	#what else do I return here
	res <- (cbind(means, cis))
	return(res)
		}


calcCI <- function(data, conf.int){
	cols <- ncol(data)
	rows <- nrow(data)
	cis <- length(conf.int)
	simCI <- as.data.frame(matrix(data = NA, nrow = cols, ncol = (cis*2)))
	for (n in 1:cols){	
		rp1 <- 1
		print(rp1)
		for (i in 1:cis) {
			print(paste("conf.int",i))
			lwr <- (1 - conf.int[i])/2
			colnames(simCI)[rp1] <- paste(100*lwr,"%", sep = "")
			upr <- conf.int[i] + lwr
			colnames(simCI)[rp1+1] <- paste(100*upr,"%", sep = "")
			simCI[n, rp1 ] <- 	 sort(data[  ,n])[round(lwr*rows)]
			simCI[n, (rp1 +1)] <- sort(data[  ,n])[round(upr*rows)]
			rp1 <- rp1+2
		}
	}
	return(simCI)	
}

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


#test restuls
calcCI(sMulti$qi$fd, .95)
zeligTile(sMulti, conf.int = c(.9, .95, .99))
zeligTile(s.out1, conf.int = c(.9, .95, .99))


#zeligtoLineplot

#zeligtoMatrix