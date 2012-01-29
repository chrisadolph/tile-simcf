pretty.logaxis <- function (usr, base = NULL, axp3 = 3) {
    if (is.null(base)) 
        base <- exp(1)
    usr <- usr^base
    axp <- c(usr, axp3)
    iC <- as.integer(axp[3])
    ii <- round(log10(axp[1:2]))
    x10 <- 10^((ii[1] - (iC >= 2)):ii[2])
    r <- switch(iC, x10, c(outer(c(1, 5), x10))[-1], c(outer(c(1, 
        2, 5), x10))[-1])
    at <- r[usr[1] <= r & r <= usr[2]]
    at <- log(at, base = base)
    at
}
