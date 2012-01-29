"lighten" <-
function(col,
         pct=0.75,
         alpha=1){
  if (abs(pct)>1) {
    print("Warning:  Error in Lighten; invalid pct")
    pcol <- col2rgb(col)/255
  } else {
    col <- col2rgb(col)/255
    if (pct>0) {
      pcol <- col + pct*(1-col)
    } else {
      pcol <- col*pct
    }
  }
  pcol <- rgb(pcol[1],pcol[2],pcol[3],alpha)
  pcol
}

