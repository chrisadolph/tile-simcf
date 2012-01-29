tileInitWH <- function(tc) {

    shrow <- rep(unit(1,"strheight",""),tc$RxC[1])
    shcol <- rep(unit(1,"strheight",""),tc$RxC[2])
    lnrow <- rep(unit(0,"lines"),tc$RxC[1])
    lncol <- rep(unit(0,"lines"),tc$RxC[2])
    npcrow <- rep(0,tc$RxC[1])
    npccol <- rep(0,tc$RxC[2])
    npplots <- rep(0,tc$nplots)
 
    # NPC:  By col & row
    curraxes <- c("xaxis","yaxis","topaxis","rightaxis",
                  "xaxistitle","yaxistitle","topaxistitle","rightaxistitle",
                  "plottitle","undertitle","columntitle","rowtitle","maintitle"
                  )
    currunits <- c("npc","npcmain", "npcmainSaved")
    for (i in 1:length(curraxes)) {
        for (j in 1:length(currunits)) {
            tc$width[[paste(curraxes[i],currunits[j],sep=".")]] <- npplots
            tc$height[[paste(curraxes[i],currunits[j],sep=".")]] <- npplots
        }
    }
   
    # Lines:  By col & row
    curraxes <- c("xaxis","yaxis","topaxis","rightaxis",
                  "xaxistitle","yaxistitle","topaxistitle","rightaxistitle",
                  "plottitle","undertitle","columntitle","rowtitle","maintitle"
                  )
    currunits <- c("lines")
    for (i in 1:length(curraxes)) {
        for (j in 1:length(currunits)) {
            tc$width[[paste(curraxes[i],currunits[j],sep=".")]] <- lncol
            tc$height[[paste(curraxes[i],currunits[j],sep=".")]] <- lnrow
        }
    }

    # Strings:  By col & row
    curraxes <- c("xaxis","yaxis","topaxis","rightaxis",
                  "xaxistitle","yaxistitle","topaxistitle","rightaxistitle",
                  "plottitle","undertitle","columntitle","rowtitle","maintitle"
                  )
    currunits <- c("strunit")
    for (i in 1:length(curraxes)) {
        for (j in 1:length(currunits)) {
            tc$width[[paste(curraxes[i],currunits[j],sep=".")]] <- shcol
            tc$height[[paste(curraxes[i],currunits[j],sep=".")]] <- shrow
        }
    }
    
    tc
}
