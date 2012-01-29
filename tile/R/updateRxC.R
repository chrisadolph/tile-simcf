"updateRxC" <-
function(tc) {
  if (tc$currcol==tc$RxC[2]) {
    tc$currcol <- 1
    tc$currrow <- tc$currrow + 1
  } else {
    tc$currcol <- tc$currcol + 1
  }
tc
}

