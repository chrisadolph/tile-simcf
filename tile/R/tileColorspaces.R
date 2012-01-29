tileColorspaces <- function(n=5,
                            colorset="sequential", # rainbow, diverge, heatmap, terrain, categories, brewer
                            hue=NULL,
                            chroma=NULL,
                            luminance=NULL,
                            power=NULL,
                            gamma=2.4,
                            fixup=TRUE
                            ) {

  require(colorspace)
  
  if (colorset=="brewer") {
    colors <-  c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33", "#A65628", "#F781BF",
                 "#999999", "#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854", "#FFD92F", "#E5C494",
                 "#B3B3B3", "#8DD3C7", "#FFFFB3", "#BEBADA", "#FB8072", "#80B1D3", "#FDB462", "#B3DE69",
                 "#FCCDE5", "#D9D9D9", "#BC80BD", "#CCEBC5", "#FFED6F")
    if (n<=length(colors))
      colors <- colors[1:n]
    else
      stop("More colors requested than available from RColorBrewer")
  }
  
  if (is.null(hue)) {
    if (colorset=="sequential")
      hue <- 260
    if (colorset=="diverge")
      hue <- c(260,0)
    if (colorset=="rainbow")
      hue <- c(0,360*(n-1)/n)
    if (colorset=="heatmap")
      hue <- c(0,90)
    if (colorset=="terrain")
      hue <- c(130,0)
    if (colorset=="categories") {
      hue <- c("red","blue","green","yellow","purple","orange","brown","pink","aquamarine")
      if (n<=length(hue))
        hue <- hue[1:n]
      else
        stop("More colors requested than available from categories option in tileColorspaces")
    }
  }
  if (is.character(hue)) {
    hue <- rgb2hsv(col2rgb(c(hue)))[1,]
  }
  
  
  if (is.null(luminance)) {
    if (colorset=="sequential")
      luminance <- c(30,90)
    if (colorset=="diverge")
      luminance <- c(30,90)
    if (colorset=="rainbow")
      luminance <- 70
    if (colorset=="heatmap")
      luminance <- c(50,90)
    if (colorset=="terrain")
      luminance <- c(60,95)
    if (colorset=="categories")
      luminance <- 85
  }

  if (is.null(chroma)) {
    if (colorset=="sequential")
      chroma <- c(80,0)
    if (colorset=="diverge")
      chroma <- 80
    if (colorset=="rainbow")
      chroma <- 50
    if (colorset=="heatmap")
      chroma <- c(100,30)
    if (colorset=="terrain")
      chroma <- c(80,0)
    if (colorset=="categories")
      chroma <- 35
  }

  if (is.null(power)) {
    if (colorset=="sequential")
      power <- 1.5
    if (colorset=="diverge")
      power <- 1.5
    if (colorset=="rainbow")
      power <- NULL
    if (colorset=="heatmap")
      power <- c(1/5, 1)
    if (colorset=="terrain")
      power <- c(1/10, 1)
    if (colorset=="categories")
      power <- NULL
  }

  if (colorset=="sequential")
    colors <- sequential_hcl(n, h = hue, c. = chroma, l = luminance, power = power,
                             gamma = gamma, fixup = fixup)
  if (colorset=="diverge")
    colors <- diverge_hcl(n, h = hue, c = chroma, l = luminance, power = power,
                          gamma = gamma, fixup = fixup)
  if (colorset=="rainbow")
    colors <- rainbow_hcl(n, c = chroma, l = luminance, start = hue[1], end = hue[2],
                          gamma = gamma, fixup = fixup)
  if (colorset=="heatmap")
    colors <- heat_hcl(n, h = hue, c. = chroma, l = luminance, power = power,
                       gamma = gamma, fixup = fixup)
  if (colorset=="terrain")
    colors <- terrain_hcl(n, h = hue, c. = chroma, l = luminance, power = power,
                          gamma = gamma, fixup = fixup)
  if (colorset=="categories")
    colors <- hcl(h=hue, c=chroma, l=luminance, fixup=fixup)

  colors
}
