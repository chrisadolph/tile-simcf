"tileCartouche" <-
function(x,
         y,
         c,   # wants a default
         arcsegments=50,
         incr=0.0000001,
         unique=TRUE,
         ...) {
    
    if (unique) {
        data <- unique(cbind(x,y))
        x <- as.vector(data[,1])
        y <- as.vector(data[,2])
    }

print(x)
    print(y)
  
    
    xl <- yl <- NULL
    nvert <- length(x)
    
    
    if (nvert==2) {
        theta <- atan2(y[2]-y[1],x[2]-x[1])
        phi <- theta-(pi/2)
        ynew1 <-  y[1] - incr*sin(phi)
        xnew1 <-  x[1] - incr*cos(phi)
        
        theta <- atan2(y[1]-y[2],x[1]-x[2])
        phi <- theta-(pi/2)
        ynew2 <- y[2] + incr*sin(phi)
        xnew2 <- x[2] + incr*cos(phi)
        
        x <- c(xnew2,xnew1,x[1],x[2])
        y <- c(ynew2,ynew1,y[1],y[2])
    }
    if (nvert==1) {
        y <- c(y,y-incr,y,y+incr)
        x <- c(x+incr,x,x-incr,x)
    }

    print(xnew1)
    print(ynew1)
    print(xnew2)
    print(ynew2)
    stop()
    
    nvert <- length(x)
    for (i in 1:nvert) {
        xarc <- yarc <- NULL
        curr <- i
        if (i<nvert) new <- i+1 else new <- 1
        if (i>1) last <- i-1 else last <- nvert
        
        dist <- sqrt((x[new]-x[curr])^2 + (y[new]-y[curr])^2)
        xnew0 <- -c*(x[new]-x[curr])/dist + x[curr]
        ynew0 <- -c*(y[new]-y[curr])/dist + y[curr]
        dist <- sqrt((x[curr]-x[last])^2 + (y[curr]-y[last])^2)
        xnew1 <- c*(x[curr]-x[last])/dist + x[curr]
        ynew1 <- c*(y[curr]-y[last])/dist + y[curr]
        
        
        lowtheta <- atan2(ynew0-y[curr],xnew0-x[curr])
        uptheta <- atan2(ynew1-y[curr],xnew1-x[curr])
        
   # print(c(ynew1,xnew1,lowtheta,uptheta))
    
        if (uptheta>lowtheta) 
            theta <- seq(lowtheta,uptheta,-(lowtheta-uptheta)/arcsegments)
        else 
            theta <- c(seq(lowtheta,pi,-(lowtheta-pi)/arcsegments),
                       seq(-pi,uptheta,-(-pi-uptheta)/arcsegments))
        xarc <- x[curr] + c*cos(theta)
        yarc <- y[curr] + c*sin(theta)
        
        xl <- c(xl,xnew0,xarc,xnew1)
        yl <- c(yl,ynew0,yarc,ynew1)
    }

    print(xl)
    print(yl)
    stop()
    list(x=xl,y=yl)
}

