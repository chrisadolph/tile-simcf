tileColorcode <- function(data,
                          n=NULL,
                          breaks=NULL,
                          colorset="sequential", # rainbow, diverge, heatmap, terrain, categories
                          hue=NULL,
                          chroma=NULL,
                          luminance=NULL,
                          power=NULL,
                          gamma=2.4,
                          fixup=TRUE
                          ) {
  
  # Create color bins, assign data to bin
  if (is.null(n)&&is.null(breaks)) {
    n <- "Sturges"
  }

  if (is.null(breaks)) {
    breaks <- hist(data,breaks=n,plot=FALSE)$breaks
    n <- length(breaks) - 1
  } else {
    mindata <- min(na.omit(data))
    maxdata <- max(na.omit(data))
    breaks <- sort(breaks)
    if (mindata<min(breaks))
      breaks <- c(mindata,breaks)
    if (maxdata>max(breaks))
      breaks <- c(breaks,maxdata)
  }

  # Sort data into bins
  lower <- breaks[1:(length(breaks)-1)]
  upper <- breaks[2:length(breaks)]
  ubins <- 1:n
  binned <- data
  for (i in 1:length(data))
    if (!is.na(data[i]))
        binned[i] <- ubins[(lower<data[i]) & (data[i]<=upper)]

  # Create corresponding color space
  colors <- tileColorspaces(n,colorset=colorset,hue=hue,chroma=chroma,
                            luminance=luminance,power=power,gamma=gamma,fixup=fixup)

  # Return vector of used colors
  colors[binned]
}
