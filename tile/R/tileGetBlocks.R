tileGetBlocks <- function(...,blocktype=NULL) {

    x <- list(...)
    xnames <- names(x)
    nx <- length(x)
    nc <- NULL
    for (ix in 1:nx)
        nc <- c(nc,length(x[[ix]]))
    if (!is.null(blocktype))
        nc <- c(nc,length(blocktype))
    nc <- unique(nc)
    if (length(nc)!=1)
        stop("tileGetBlocks:  inputs must have identical length")
    if (is.null(blocktype)) {
        blocktype <- rep(1,nc)
    }
    
    dataleft <- (nc>0)
    blkctr <- 0
    block <- list()
    while (dataleft) {
        # Check for missings and block type
        sumNA <- 0
        for (ix in 1:nx) {
            sumNA <- sumNA + is.na(x[[xnames[ix]]][1])
        }
        if (sumNA) 
            currblock <- 0
        else
            currblock <- blocktype[1]
        
        # Start new block?
        if ((blkctr==0)||(currblock!=lastblock)) {
            lastblock <- currblock
            newblock <- TRUE  
        } else {
            newblock <- FALSE
        }
        if (newblock) {
            blkctr <- blkctr + 1
            block[[blkctr]] <- list()
            for (ix in 1:nx)  {
                block[[blkctr]][[xnames[ix]]] <- NULL
            }
            block[[blkctr]]$blocktype <- currblock 
        }

        # Move data from x to current block
        if (length(blocktype)>1) {
            blocktype <- blocktype[2:length(blocktype)]
        }
        for (ix in 1:nx) {
            block[[blkctr]][[xnames[ix]]] <- c(block[[blkctr]][[xnames[ix]]],
                                               x[[xnames[ix]]][1])
            if (length(x[[xnames[ix]]])>1) {
                x[[xnames[ix]]] <- x[[ix]][2:length(x[[xnames[ix]]])]                
            } else {
                x[[xnames[ix]]] <- NULL
            }
        }
        dataleft <- (length(x[[xnames[1]]])>0)
    }
    # If no data, return NA
    if (length(block)==0) {
        block$blocktype <- NA
        for (ix in 1:nx) {
            block[[blkctr]][[xnames[ix]]] <- NA
        }
    }
    block
}
