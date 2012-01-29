# Create an n-vector of (identical) lists seeded by l
fillout.list <- function(l,n) {    
    lout <- vector("list",n)
    for (i in 1:n) {
        lout[[i]] <- l
    }
    lout
}
