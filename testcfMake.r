# A file to Test the new multilevel abilities of simcf

# OriginalChris Adolph (faculty.washington.edu/cadolph)
# 2 February 2012
#Update July 2012 Aaron Erlich

# For testing tile, need to load simcf and vice versa
rm(list=ls())
library(devtools)
library(simcf)

# Just quickly run all the test files in the inst/test folder of the package
quicktest <- function(pkg) {
  newpath <- paste(path,"/inst/tests",sep="")
  files <- list.files(newpath)
  for (i in 1:length(files)) {
    source(paste(newpath,files[i],sep="/"))
  }
  invisible(NULL)
}

# Load all parts of the package you are editing 



# Your test package path goes here
# Your path may differ from mine, 
# and if you're developing on a PC, may have a drive name, etc.
path <- "/Users/aaronerlich/Documents/git/tile-simcf/simcf"
pathT <- "/Users/aaronerlich/Documents/git/tile-simcf/tile"


load_all(path, TRUE)

require(MASS)
data(UScrime)

data <- UScrime
data$Prob[1] <- 0 #add a logit bounded variable
model <- (y ~ log(M) + So + log(Ed) + log(Po1) + log(Po2)
              + log(LF) + log(M.F) + log(Pop) + log(NW) +log(U1)
              + log(U2) + log(GDP) + log(Ineq) + logitbound(Prob) +
              log(Time))

data <- cbind(data, nas = rep(NA, nrow(data))) #add a whole column of NAs

modelNA <- (y ~ log(M) + So + log(Ed) + log(Po1) + log(Po2)
              + log(LF) + log(M.F) + log(Pop) + log(NW) +log(U1)
              + log(U2) + log(GDP) + log(Ineq) + logitbound(Prob) +
              log(Time) + nas)

#test cfMake Warning messages
tl = 1

cfMake(model, tl)
cfMake(formula = NULL, data = data) #should yield a warning
cfMake(modelNA, data) #should stop because no rows in data.frame


#Test the multiformula model
model1 <- (y ~ log(M) + So)
model2 <- (y ~ logitBound(Prob) + So)

znb <- list(model1, model2)
znbTest <- cfMake(formula = znb , data = data, nscen =5)
print(znbTest)

#use Gelman's arm smoking data for hierarchical example
#data can be downloaded from
smoke <- read.table("/Volumes/Optibay-1TB/Data/ARM_Data/smoking/smoke_pub.dat", header = TRUE)
smokeL1 <- aggregate(smoke[, c("parsmk", "sex.1.F.")], by = list(newid = smoke$newid), head, 1)
smokeL2 <- smoke[, c("newid", "wave", "smkreg")]

smokeForm <- list((~ parsmk + sex.1.F.), (smkreg ~ wave))
smokeData <- list(smokeL1, smokeL2)

hlmTest <- cfMake(formula = smokeForm, data = smokeData, nscen =5)
print(hlmTest)

#test different lengths
cfMake(formula = smokeForm, data = list(smokeL1))


#git push -u git@github.com:aserlich/tile-simcf.git  master