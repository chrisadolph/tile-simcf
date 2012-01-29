tileProcessTraces <- function(traces) {

    # Check for mimimal requirements

    # Make new list

    # Unbundle any traces bundled into lists
    ntraces <- length(traces)
    nnewtraces <- 0
    for (i in 1:ntraces) {
      if (any(class(traces[[i]])=="tileTrace")) {
        nnewtraces <- nnewtraces + 1
      } else {
        if (is.list(traces[[i]])) {
          for (j in 1:length(traces[[i]])) {
            if (any(class(traces[[i]])=="tileTrace")) {
              nnewtraces <- nnewtraces + 1              
            }            
          }
        }
      }
    }
    newtraces <- vector("list",nnewtraces)
    int <- 1
    for (i in 1:ntraces) {
      if (any(class(traces[[i]])=="tileTrace")) {
        newtraces[[int]] <- traces[[i]]
        int <- int + 1
      } else {
        if (is.list(traces[[i]])) {
          for (j in 1:length(traces[[i]])) {
            if (any(class(traces[[i]][[j]])=="tileTrace")) {
              newtraces[[int]] <- traces[[i]][[j]]
              int <- int + 1          
            }            
          }
        }
      }
    }
    ntraces <- length(newtraces)
    
    # Loop over all traces; fill in defaults; determine number of trace-plots    
    ntp <- 0
    for (i in 1:ntraces) {

        # Set trace default options
        tracedefault <- eval(call(paste(newtraces[[i]]$graphic,"TileTraceDefaults",sep="")))

        # Fillout trace options
        newtraces[[i]] <- eval(call(paste(newtraces[[i]]$graphic,"TileTraceFillout",sep=""),newtraces[[i]],tracedefault))

        if (is.null(newtraces[[i]]$plot))
            newtraces[[i]]$plot <- i
        
        ntp <- ntp + length(newtraces[[i]]$plot)
    }
    tracesout <- vector("list",ntp)
    
    # Loop over traces; process for plotting
    ip <- 0
    for (i in 1:ntraces) {

        # Optional special treatment based on graphic type
        newtraces[[i]] <- eval(call(paste(newtraces[[i]]$graphic,"TilePrep",sep=""),newtraces[[i]]))
        
        # Create one copy of the trace for each plot it appears in
        currplots <- newtraces[[i]]$plot
        for (k in 1:length(currplots)) {
            ip <- ip+1
            tracesout[[ip]] <- newtraces[[i]]
            tracesout[[ip]]$plot <- currplots[k]
        }
    }
    
    tracesout
}
