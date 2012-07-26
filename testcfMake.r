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

model1 <- (y ~ log(M) + So)
model2 <- (y ~ logitBound(Prob) + So)
model3 <- (y ~ Prob + So)

#test cfMake Warning messages
tl = 1

notDFTest <- try(cfMake(model, tl)) # should yield a stop() that tl is not a data frame
cfMake(formula = NULL, data = data[ ,(1:(ncol(data)-1))]) #should yield a warning but work 
cfMake(modelNA, data) #should stop because no rows in data.frame
cfMake(model1, data) #should work
cfMake(model2, data) #should work with logitBound adjustment

#Test the multiformula model
znb1 <- list(model1, model2) #with logitBound transformation
znb2 <- list(model1, model3) #with no transformation
znbError <- list(model1, model2, 5) #with an incorrectly specificed formula
cfMake(formula = znb1 , data = data, nscen =5)
cfMake(formula = znb2 , data = data, nscen =5)
#should return an error saying one object of the list was not a formula but was of class numeric
cfMake(formula = znbError , data = data, nscen =5)

#use Gelman's arm smoking data for hierarchical example
#data can be downloaded from
smoke <- read.table("/Volumes/Optibay-1TB/Data/ARM_Data/smoking/smoke_pub.dat", header = TRUE)
smokeL1 <- aggregate(smoke[, c("parsmk", "sex.1.F.")], by = list(newid = smoke$newid), head, 1)
smokeL2 <- smoke[, c("newid", "wave", "smkreg")]

smokeForm <- list((~ parsmk + sex.1.F.), (smkreg ~ wave))
smokeData <- list(smokeL1, smokeL2)

scenNam <- paste("scen", seq(1,5))
scenNam2 <- paste("Hierarchical scen", seq(1,5))


#Test normal functionality of  hierarchical cfChange and cfName
hlmTest <- cfMake(formula = smokeForm, data = smokeData, nscen = c(8, 6))
hlmTest <- cfChange(hlmTest, covname = "smkreg" , x =.8 , xpre = .3, scen = 2)
hlmTest <- cfName(hlmTest, name = "some scen", scen =2,  df =1)
hlmTest <- cfName(hlmTest, name = "some other scen", scen =3,  df =2)
print(hlmTest)

hlmTest2 <- hlmTest
colnames(hlmTest2$x[[1]])[1] <- "smkreg"
#should yield an error two covariates can't have the same name
cfChange(hlmTest2, covname = "smkreg" , x =.8 , xpre = .3, scen = 2) 

#test different lengths should yield an error
cfMake(formula = smokeForm, data = list(smokeL1))


#git push -u git@github.com:aserlich/tile-simcf.git  master