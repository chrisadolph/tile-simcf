# ================
# = Aaron Erlich
# = first updated July 27, 2012
# =
# ================
library(simcf)

cfpe <- function(cf, scen=NULL, cat=NULL, period=NULL eq=NULL) {
	if (cf$cfMake.call[["eqtype"]]) %in% c("simple")) {
		if (is.vector(cf[["pe"]]) cf[["pe"]] <- as.matrix(cf[["pe"]]) #dim doesn't work on vectors. Should make this a matrix please
		peDim <- dim(cf[["pe"]])
		if(is.null(scen))  assign("scen", paste(1, ":", 3))
		pe.out <-   apply(function(i) i[scen, cat])
	}
	
}

# 
# would indicate scenarios 5 to 10 in the second category of a categorical outcome variable, and only in periods 5 to 10 of forecasts into the future, and only in the second equation of a multi-equation model. (Obviously not all of these arguments will apply in a specific case; and eq is there for future compatibility.) lower() and upper() also will have a ci argument.
# 
# The extractors will look in the cf object to learn the dimensions of the it's simulations, then return the requested segments in whatever format (vector, matrix, array). The cf argument will also provide information of what levels each confidence interval corresponds to.
# 
# These extractor functions can be used by end users, but also by tile traces when they encounter a cf object.


 po1.qoi[["pe"]] <- as.matrix(po1.qoi[["pe"]])
dim(po1.qoi[["pe"]])
dim(yhyp.plr[["pe"]])