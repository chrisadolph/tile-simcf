"stacked" <-
function(x=NULL,
         y=NULL,
         start=NULL,
         end=NULL,
         by=NULL,
         labels=NULL,
         labeloffset=NULL,
         markers=NULL,
         clpoints=NULL,
         sizes=NULL,
         thick=NULL,
         consistent=NULL,
         autoscale=NULL,
         plotlist=NULL,
         fit=NULL,
         ...){
  
  rl <- list(x=x,
             y=y,
             start=start,
             end=end,
             by=by,
             labels=labels,
             labeloffset=labeloffset,
             markers=markers,
             clpoints=clpoints,
             sizes=sizes,
             thick=thick,
             consistent=consistent,
             autoscale=autoscale,
             plotlist=plotlist,
             fit=fit
             )
  tile(graphic="stacked",rl=rl,...)
}

