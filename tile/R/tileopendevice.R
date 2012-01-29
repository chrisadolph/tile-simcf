tileopendevice <- function(tc,final) {

      if (is.null(tc$output$outfile)) {
        if (final) {
          if(dev.cur() == 1) {
            getOption("device")(width=tc$output$wide,
                      height=tc$output$high)
          } else {
            dev.new(width=tc$output$wide,
                      height=tc$output$high)
          }
        } else {
          pdf(paste("tileTemporaryFile",tc$output$type,sep="."),
              pointsize = tc$output$pointsize,   #14
              width=tc$output$wide,
              height=tc$output$high,
              onefile = TRUE,
              family = tc$output$family,
              version = "1.4"          # needed for semitransparency
              )
        }
      } else {
          if ((tc$output$type=="ps")||(tc$output$type=="eps")||(tc$output$type=="postscript"))
              postscript(paste(tc$output$outfile,tc$output$type,sep="."),
                         pointsize = tc$output$pointsize,   #14
                         width=tc$output$wide,
                         height=tc$output$high,
                         horizontal = FALSE,
                         onefile = TRUE,
                         family = tc$output$family)
          if (tc$output$type=="pdf")
              pdf(paste(tc$output$outfile,tc$output$type,sep="."),
                  pointsize = tc$output$pointsize,   #14
                  width=tc$output$wide,
                  height=tc$output$high,
                  onefile = TRUE,
                  family = tc$output$family,
                  version = "1.4"          # needed for semitransparency
                  )
          if ((tc$output$type=="wmf")||(tc$output$type=="win.metafile"))
              win.metafile(filename =paste(tc$output$outfile,"wmf",sep="."),
                           pointsize = tc$output$pointsize,
                           width=tc$output$wide,
                           height=tc$output$high)

          if (tc$output$type=="png") {
            if (capabilities("png")) {
              png(filename=paste(tc$output$outfile,tc$output$type,sep="."),
                  pointsize = tc$output$pointsize,
                  width=tc$output$wide,
                  height=tc$output$high,
                  units=tc$output$units,
                  res=tc$output$res)
            } else {
              warning("png not available; using default graphical device")
              if(dev.cur() == 1) getOption("device")(width=tc$output$wide,
                          height=tc$output$high)
            }
          }
          
          if (tc$output$type=="bmp")
              bmp(filename=paste(tc$output$outfile,tc$output$type,sep="."),
                  pointsize = tc$output$pointsize,
                  width=tc$output$wide,
                  height=tc$output$high,
                  units=tc$output$units,
                  res=tc$output$res)


          if ((tc$output$type=="jpeg")||(tc$output$type=="jpg")) {
            if (capabilities("jpeg")) {
              jpeg(filename=paste(tc$output$outfile,tc$output$type,sep="."),
                  pointsize = tc$output$pointsize,
                  width=tc$output$wide,
                  height=tc$output$high,
                  units=tc$output$units,
                  res=tc$output$res)
            } else {
              warning("jpeg not available; using default graphical device")
              if(dev.cur() == 1) getOption("device")(width=tc$output$wide,
                          height=tc$output$high)
            }
          }


          if (tc$output$type=="tiff") {
            if (capabilities("jpeg")) {
              tiff(filename=paste(tc$output$outfile,tc$output$type,sep="."),
                  pointsize = tc$output$pointsize,
                  width=tc$output$wide,
                  height=tc$output$high,
                  units=tc$output$units,
                  res=tc$output$res)
            } else {
              warning("tiff not available; using default graphical device")
              if(dev.cur() == 1) getOption("device")(width=tc$output$wide,
                          height=tc$output$high)
            }
          }

          if (tc$output$type=="pictex")
              pictex(paste(tc$output$outfile,tc$output$type,sep="."),
                     width=tc$output$wide,
                     height=tc$output$high
                     )

          if (tc$output$type=="xfig")
              xfig(paste(tc$output$outfile,tc$output$type,sep="."),
                   pointsize = tc$output$pointsize,   #14
                   width=tc$output$wide,
                   height=tc$output$high,
                   horizontal = FALSE,
                   onefile = TRUE,
                   family = tc$output$family
                   )

          if (tc$output$type=="bitmap")
            bitmap(paste(tc$output$outfile,tc$output$type,sep="."),
                   pointsize = tc$output$pointsize,   #14
                   width=tc$output$wide,
                   height=tc$output$high)

          if (tc$output$type=="CairoPDF") {
            require(Cairo)
            CairoPDF(paste(tc$output$outfile,"pdf",sep="."),
                     pointsize = tc$output$pointsize,   #14
                     width=tc$output$wide,
                     height=tc$output$high,
                     onefile = TRUE,
                     family = tc$output$family,
                     version = "1.4"          # needed for semitransparency
                     )            
          }

          if (tc$output$type=="CairoPS") {
            require(Cairo)
            CairoPS(paste(tc$output$outfile,"ps",sep="."),
                     pointsize = tc$output$pointsize,   #14
                     width=tc$output$wide,
                     height=tc$output$high,
                     onefile = TRUE,
                     family = tc$output$family
                    )            
          }
        
        }
    }

