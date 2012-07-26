cfMake <- function(formula=NULL,data,nscen=1,names=NULL,hull=FALSE,f="mean",...) {
#There are three scenarios for model
#1 one formula, one data frame
#2 many formulas, one data frame
#3 many formulas, equal number of data frames
#if there are multiple formulas, they all need to be passed to the model part of the scenario
#but if there is one data set and multiple formulas, those parts of the model need to be extracted and combined to create one set of counterfactuals
#and then then the scenario is made up of all of those parts. This leads to three scenarios for how to deal with models
	cfMake.call <- match.call(expand.dots = TRUE)
	
	if((any(class(formula)=="formula") || is.null(formula)) && !is.data.frame(data)) {
		stop("You must provide one and only one data.frame object for non-hierachical models")
	}
	
	#one formula, one data frame 
	if((any(class(formula)=="formula") || is.null(formula)) && is.data.frame(data)) {
		checkScenF(formula)
		data.out <- constDF(formula, data) #deal with null formulae
		checkScenD(data.out)
		xscen <- initScen(data.out, nscen, names, f, ...)
		xscen$model <- fixModel(formula, data.out)
		cfMake.call$model.type <- "simple"
	}
	
	#more than one formula, one data frame -- works with null formulae in list
	if (is.list(formula) && is.data.frame(data)) {
		sapply(formula, checkScenF)
		combVars <- unique(unlist(lapply(formula, all.vars))) #get all variables in all formulas
		combVars <- as.formula(paste(combVars[1], " ~ ", paste(combVars[2:length(combVars)], collapse= "+")))
		data.out <- constDF(combVars, data)
		checkScenD(data.out) #error checking
		xscen <- initScen(data.out, nscen, names, f, ...) #initiate scenario with all formulas
		#there is some environment issue with model.matrix it works if I reboot R but there is some conflict
		xscen$model <- sapply(formula, fixModel, data = data.out) #attach formulas to object
		cfMake.call$model.type <- "multiformula"
	}
	
	#hierarchical - many data frames, many scenarios and assumes datasets are in line with formula 
	#we could allow the initiation of variable numbers of scenarios as well
	#we need to decide if we want the name of the data set attached if present
	#names would have to be a list of different scenarios and hence a list
	if (is.list(formula) && !is.data.frame(data) && is.list(data)) {
		
		if (length(formula)!=length(data)) {
			stop("The number of formula in a list must equal the number of data.frames for hierarchical models")
		}
		if(is.null(names)) {
			names <- lapply(data, function(i) rep(NULL, 1))
		} else if (length(names)!=length(data)) {
			stop("You must either leave names as NULL or specify a vector of names for each level in your model with the same number of names as there are scenarios in the level")
		}
		if(length(nscen)==1) {
			nscen <- rep(nscen, length(data))
		} else if (length(nscen)!=length(data)) {
			stop(paste("You must either initiate the same number of scenarios for all levels of data or 
			there should be a number of scenarios specified for each level of data. You have intiated", length(nscen), "different numbers for scenarios for", length(data), "data.frames"))
		}

		xscen <- list(x=list(),xpre=list())
		for (levs in 1:length(formula)) {
			sapply(formula, checkScenF)
			sapply(data, checkScenD)
			dataTemp <-  constDF(formula[[levs]], data[[levs]])   
			xScenTemp <- initScen(dataTemp, nscen[levs], names[[levs]], f,...)
			xscen$x[[levs]] <- xScenTemp$x
			xscen$xpre[[levs]] <- xScenTemp$xpre
			xscen$model <- sapply(formula, fixModel, data = data) #attach formulas to object
		}
		cfMake.call$model.type <- "multilevel"	
	}
	xscen$cfMake.call <- as.call(cfMake.call)
	return(xscen)
}

constDF <- function(formula, data) {
	if(is.null(formula)) {
		data.out <- data
		} else {
			data.out <- data[ ,all.vars(formula)]
		}
		return(data.out)
	}

checkScenF <- function(formula) {	
	if (is.null(formula)) {
		warning("A NULL was given for a formula provided.")
	} 	else if (!("formula" %in% class(formula))) {
		stop(paste("An object that was neither NULL or of class formula was given as a formula. You gave an object of class", class(formula), sep = " "))
	}
}

checkScenD <- function(data) {
	if(nrow(extractdata(data, na.rm=TRUE)) ==0) {
		stop("Data provided must be data.frame object with at least one row of data not omitted")
	}
}

initScen <- function(data, nscen, names, f,...) {
	data <- na.omit(data)
    xmean <- apply(data,2,f,...)
    xscen <- list(x=NULL,xpre=NULL)
    xscen$x <- xscen$xpre <- as.data.frame(matrix(data=xmean,nrow=nscen,ncol=ncol(data),byrow=TRUE))
    colnames(xscen$x) <- colnames(xscen$xpre) <- names(data)

    if (!is.null(names)) {
		#we could have an error message for when the number of names don't equal the number of scenarios
        row.names(xscen$x) <- row.names(xscen$xpre) <- names
	}
	return(xscen)
}	
	
fixModel <- function(formula, data){
    if (!is.null(formula)) {
      # Get terms attribute
      tl <- attributes(terms(formula))$term.labels
      # Loop over terms
      for (i in 1:length(tl)) {
        tlCur <- tl[i]
        # Check for logitBound transformations
        if (substr(tlCur,1,11)=="logitBound(") {
          # if found, check number of terms needed.
          varname <- substr(tlCur,start=12,stop=nchar(tlCur)-1) #extracts variable name
          subform <- as.formula(paste("~",varname,"-1")) #
		  toLT <- as.vector(model.matrix(subform,data=data))
          testLT <- as.matrix(logitBound(toLT))
          # revise formula so logitBound() call includes "forceAny" and/or "forceAll" as needed
          if (any(colnames(testLT)=="any")) {
            tlCur <- paste(substr(tlCur,start=1,stop=nchar(tlCur)-1), ", forceAny=TRUE)",sep="")
          }
          if (any(colnames(testLT)=="all")) {
            print(testLT)
            tlCur <- paste(substr(tlCur,start=1,stop=nchar(tlCur)-1), ", forceAll=TRUE)",sep="")
          }
          tl[i] <- tlCur
          rhs <- paste(tl, collapse = " + ")
          newform <- as.formula(paste("lhs ~", rhs), env=.GlobalEnv)
          newform[[2L]] <- formula[[2L]]
          formula <- newform
        }    

        # Check for logBound transformations
        if (substr(tlCur,1,9)=="logBound(") {
          # if found, check number of terms needed.
          varname <- substr(tlCur,start=10,stop=nchar(tlCur)-1)
          subform <- as.formula(paste("~",varname,"-1"))          
          toLT <- as.vector(model.matrix(subform,data=data))
          testLT <- as.matrix(logitBound(toLT))
          # revise formula so logBound() call includes "forceAny" as needed
          if (any(colnames(testLT)=="any")) {
            tlCur <- paste(substr(tlCur,start=1,stop=nchar(tlCur)-1), ", forceAny=TRUE)",sep="")
          }
          tl[i] <- tlCur
          rhs <- paste(tl, collapse = " + ")
          newform <- as.formula(paste("lhs ~", rhs), env=.GlobalEnv)
          newform[[2L]] <- formula[[2L]]
          formula <- newform
        }
      }
      
      #xscen$model <- formula	  
    }
	return(formula)
}
###what to do with this?
    # 
    # # Check for extrapolation
    # if (hull&&(!is.null(formula))&&(!is.null(data))) {
    #     require(WhatIf)
    #     wi <- whatif(formula=formula, data=data, cfact=xscen$x)        
    #     xscen$extrapolatex <- !wi$in.hull
    #     wi <- whatif(formula=formula, data=data, cfact=xscen$xpre)
    #     xscen$extrapolatexpre <- !wi$in.hull
    #     xscen$extrapolatefd <- xscen$extrapolatex|xscen$extrapolatexpre
    #     xscen$data <- data
    #     if (any(c(xscen$extrapolatex,xscen$extrapolatexpre,xscen$extrapolatefd)==FALSE)) {
    #         warning("Some counterfactuals involve extrapolation outside the convex hull")
    #         if (any(xscen$extrapolatex==FALSE)) {
    #             print(c("x scenarios:  ",row.names(x)[xscen$extrapolatex]))
    #         }
    #         if (any(xscen$extrapolatexpre==FALSE)) {
    #             print(c("xpre scenarios:  ",row.names(xpre)[xscen$extrapolatexpre]))
    #         }
    #         if (any(xscen$extrapolatefd==FALSE)) {
    #             print(c("first diff scenarios:  ",row.names(x)[xscen$extrapolatefd]))
    #         }
    #     }
    # }
    # 
    #   
    #   
#Nothing special needs to be do for cfChange in a multiformula model
#We do need to do something when there are hierarchical models
#There are two approachs. One is to use an if statement to do something differe
#Another is to tag the types of simulators and then use that to split
#i.e. -- xscen$type = "multi-level"
cfChange <- function(xscen,covname,x=NULL,xpre=NULL,scen=1) {
	if(xscen$cfMake.call[["model.type"]] %in% c("simple", "multiformula")) {
		if (!is.null(x)) xscen$x[scen,covname] <- x
    	if (!is.null(xpre)) xscen$xpre[scen,covname] <- xpre
    	if (!is.null(xscen$extrapolatex)) {
        	require(WhatIf)
        	wi <- whatif(formula=xscen$model, data=xscen$data, cfact=xscen$x)
        	xscen$extrapolatex <- !wi$in.hull
        	wi <- whatif(formula=xscen$model, data=xscen$data, cfact=xscen$xpre)
        	xscen$extrapolatexpre <- !wi$in.hull
        	xscen$extrapolatefd <- xscen$extrapolatex|xscen$extrapolatexpre
        	if (any(c(xscen$extrapolatex,xscen$extrapolatexpre,xscen$extrapolatefd)==FALSE)) {
            	warning("Some counterfactuals involve extrapolation outside the convex hull")
            	if (any(xscen$extrapolatex==FALSE)) {
                	print(c("x scenarios:  ",row.names(x)[xscen$extrapolatex]))
            	}
            	if (any(xscen$extrapolatexpre==FALSE)) {
                	print(c("xpre scenarios:  ",row.names(xpre)[xscen$extrapolatexpre]))
            	}
            	if (any(xscen$extrapolatefd==FALSE)) {
                	print(c("first diff scenarios:  ",row.names(x)[xscen$extrapolatefd]))
            	}
        	}
		}
	} else if (xscen$cfMake.call[["model.type"]] %in% c("multilevel")) {
		locs <- sapply(xscen$x, function(i) covname %in% names(i))
		mtches <- locs[locs == TRUE]
		if(length(mtches)>1) {
			stop(paste("You have the same variable (column) <", covname, "> name in more than one level of your hierarchical model. All variables MUST have unique names"))
		} else if (length(mtches)==0) {
			stop(paste("The specified covname <", covname, "> does not exist in any of the data.frames provided"))
		} else if (length(mtches)==1){
			covLoc <- match(TRUE, locs)
			if (!is.null(x)) xscen$x[[covLoc]][scen,covname] <- x
    		if (!is.null(xpre)) xscen$xpre[[covLoc]][scen,covname] <- xpre
    		#what does extraplating outside of the convex hull in multilevel models look like?
		}
	} else {
		stop("Somehow the model.type is incorrect or not specified")
	}
	return(xscen)
}


#the user should specify the data set where they want to name the scenario
#the user could combine different names of different scenarios at the different levels
cfName <- function(xscen,name,scen=1, df = 1) {
	if(xscen$cfMake.call[["model.type"]] %in% c("simple", "multiformula")) {
    	if (!is.null(xscen$x)) row.names(xscen$x)[scen] <- name
    	if (!is.null(xscen$xpre)) row.names(xscen$xpre)[scen] <- name
	} else if (xscen$cfMake.call[["model.type"]] %in% c("multilevel")) {
		if (!is.null(xscen$x[[df]])) row.names(xscen$x[[df]])[scen] <- name
		if (!is.null(xscen$xpre[[df]])) row.names(xscen$xpre[[df]])[scen] <- name
	}
 	return(xscen)
}


