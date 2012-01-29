tileSplitBlocks <- function(x,
                            lower,
                            upper,
                            blocktype) {

    blocks <- tileGetBlocks(x=x,lower=lower,upper=upper,blocktype=blocktype)
    blocksOrig <- blocks

    # Possibility of only one block and only one entry

    if (length(blocks)==1) {
        if (length(blocks[[1]]$x)==1) {
            fractionplot <- convertWidth(unit(0.05,"npc"),
                                     "native",
                                     valueOnly=TRUE
                                     )
            charwidth <- convertWidth(unit(1,"strwidth",as.list("X")),
                                      "native",
                                      valueOnly=TRUE
                                      )
            
            xadj <- max(fractionplot,charwidth)/2
            newx <- blocks[[1]]$x[1] - xadj#curr
            #newx <- blocks[[cb]]$x[1] - xadj#curr
            blocks[[1]]$x <- c(blocks[[1]]$x-xadj,blocks[[1]]$x,blocks[[1]]$x+xadj)
            blocks[[1]]$lower <- rep(blocks[[1]]$lower,3)
            blocks[[1]]$upper <- rep(blocks[[1]]$upper,3)

            #blocks[[cb]]$x <- c(blocks[[1]]$x-xadj,blocks[[1]]$x,blocks[[1]]$x+xadj)
            #blocks[[cb]]$lower <- rep(blocks[[1]]$lower,3)
            #blocks[[cb]]$upper <- rep(blocks[[1]]$upper,3)
        }
        
        continue <- FALSE
    } else {
        cb <- 1
        nb <- 2
        nextblock <- (length(blocks)>0)
        continue <- (cb<=length(blocks))
    }
  
    while (continue) {

        ctype <- blocks[[cb]]$blocktype
  
        # Extend back as necessary
        if (cb>1) {
            lb <- cb-1
            ltype <- blocks[[lb]]$blocktype
            
            # widen by character unless overlap
            # in which case widen by half of distance
            # or 1/10 of plot, whichever smaller
            xall <- blocks[[cb]]$x[1] - blocks[[lb]]$x[length(blocks[[lb]]$x)]
            qtrdist <- 0.25*xall
            fractionplot <- convertWidth(unit(0.05,"npc"),
                                     "native",
                                     valueOnly=TRUE
                                     )
            charwidth <- convertWidth(unit(1,"strwidth",as.list("X")),
                                      "native",
                                      valueOnly=TRUE
                                      )
            maxwidth <- max(fractionplot,charwidth)
            xadj <- min(qtrdist,maxwidth)           
            xadjp <- xall#blocks[[lb]]$x[length(blocks[[lb]]$x)]#abs(xall-xadj)

            if (ltype==0) {
                if (ctype>ltype) {
                                        # Expand back a little
                    xadjcurr <- xadj
                }
                if (ctype<ltype) {
                                        # Extend back a lot
                    xadjcurr <- xadjp
                }
            } else {
                xadjcurr <- xall
            }
            currentlower <- blocks[[cb]]$lower[1]
            currentupper <- blocks[[cb]]$upper[1]
            if (ltype==0) {
                if ((lb-1)>0) {
                    lastlower <- blocks[[lb-1]]$lower[length(blocks[[lb-1]]$lower)]
                    lastupper <- blocks[[lb-1]]$upper[length(blocks[[lb-1]]$upper)]
                    xall <- blocks[[cb]]$x[1] - blocks[[lb-1]]$x[length(blocks[[lb-1]]$x)] 
                } else {
                    lastlower <- blocks[[cb]]$lower[length(blocks[[cb]]$lower)]
                    lastupper <- blocks[[cb]]$upper[length(blocks[[cb]]$upper)]
                }
            } else {    
                lastlower <- blocks[[lb]]$lower[length(blocks[[lb]]$lower)]
                lastupper <- blocks[[lb]]$upper[length(blocks[[lb]]$upper)]
            }

            newlower <- currentlower - (currentlower - lastlower)*(xadjcurr/xall)
            newupper <- currentupper - (currentupper - lastupper)*(xadjcurr/xall)
            newx <- blocks[[cb]]$x[1] - xadjcurr
            blocks[[cb]]$x <- c(newx,blocks[[cb]]$x)
            blocks[[cb]]$lower <- c(newlower,blocks[[cb]]$lower)
            blocks[[cb]]$upper <- c(newupper,blocks[[cb]]$upper)
        }

        if (((cb+1)<=length(blocks))) {
             ntype <- blocks[[nb]]$blocktype
            # Look forward
            xall <- blocks[[nb]]$x[1] - blocks[[cb]]$x[length(blocks[[cb]]$x)]
            qtrdist <- 0.25*xall
            fractionplot <- convertWidth(unit(0.05,"npc"),
                                     "native",
                                     valueOnly=TRUE
                                     )
            charwidth <- convertWidth(unit(1,"strwidth",as.list("X")),
                                      "native",
                                      valueOnly=TRUE
                                      )
            maxwidth <- max(fractionplot,charwidth)
            xadj <- min(qtrdist,maxwidth)           
            xadjp <- abs(xall-xadj)
            if (ctype>ntype) {
            # Extend forward a little
                xadjcurr <- xadj
            }       
            if (ctype<ntype) {
            # Extend forward a lot
                xadjcurr <- xadjp
            }
            
            currentlower <- blocks[[cb]]$lower[length(blocks[[cb]]$lower)]
            currentupper <- blocks[[cb]]$upper[length(blocks[[cb]]$upper)]
            if (ntype==0) {
                if (length(blocks)>=(nb+1)) {
                    nextlower <- blocks[[nb+1]]$lower[1]
                    nextupper <- blocks[[nb+1]]$upper[1]
                    xall <- blocks[[nb+1]]$x[1] - blocks[[cb]]$x[length(blocks[[cb]]$x)]
                } else {
                    nextlower <- blocks[[cb]]$lower[length(blocks[[cb]]$lower)]
                    nextupper <- blocks[[cb]]$upper[length(blocks[[cb]]$upper)]
                }
            } else {    
                nextlower <- blocks[[nb]]$lower[1]
                nextupper <- blocks[[nb]]$upper[1]
            }
            newlower <- currentlower + (nextlower - currentlower)*(xadjcurr/xall)
            newupper <- currentupper + (nextupper - currentupper)*(xadjcurr/xall)
            newx <- blocks[[cb]]$x[length(blocks[[cb]]$x)] + xadjcurr
            blocks[[cb]]$x <- c(blocks[[cb]]$x,newx)
            blocks[[cb]]$lower <- c(blocks[[cb]]$lower,newlower)
            blocks[[cb]]$upper <- c(blocks[[cb]]$upper,newupper)
        }
        
        cb <- cb+1
        nb <- nb+1
        continue <- (cb<=length(blocks))
    }

    

    blocks
}


    
    
