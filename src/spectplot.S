#############################################################################
#                                                                           #
#   copyright            : (C) 2000 SHLRC, Macquarie University             #
#   email                : Steve.Cassidy@mq.edu.au			    #
#   url			 : http://www.shlrc.mq.edu.au/emu		    #
#									    #
#   This program is free software; you can redistribute it and/or modify    #
#   it under the terms of the GNU General Public License as published by    #
#   the Free Software Foundation; either version 2 of the License, or       #
#   (at your option) any later version.                                     #
#									    #
#############################################################################


"spectplot" <- function(segs, track = "samples",  bandwidth="w",
			psfile="", rate=20000, scale=5, height=7,
			frange=c(0,rate/2),rms=F,
			ncolors=16, factor=4.0, zeropt=35,
			smoothformants=F,
                  	formants="fm", ...)
{
  makespec <- paste(emu.options("bindir"), "makespec", 
	            sep=emu.options("dirsep"))

  tmpfile <- emu.tempfile("spec")
  template <- attributes(segs)$database
  uttname <- utt(segs[1,])
  start <- mustart(segs)[1]
  end <- muend(segs)[nrow(segs)]

  command <- paste( makespec, template, 
		   start/1000, end/1000, uttname, bandwidth,
 		   frange[1], frange[2], factor, zeropt,  "> ", tmpfile)
  exitscript <- ""
  exitscript <- paste( exitscript, "unlink(", tmpfile , ");", sep="")
  cat(command)
  emu.system(command)


  spec <- as.matrix(read.table( tmpfile ))
  sgram.width <- dim(spec)[2]

  ## there is a bug giving a fake line at the top of the sgram and
  ## an artifact at the bottom, cut them off here
  spec <- spec[,6:sgram.width-1]
  # and modify the frange accordingly
  tmp <- frange[2]-frange[1]
  frange <- c( frange[1]+(6/sgram.width)*tmp, frange[2]-(1/sgram.width)*tmp )


  if( psfile != ""){
    ps.options(rasters=600)
    ps.options(image.colors=0:ncolors/ncolors) 
    ps.options(colors=c(0.0, 0.6, 0.3, 0.9, 0.4, 0.7, 0.1, 0.5, 0.8, 0.2) ) 
    setps(psfile, w=scale*sum(mudur(segs))/1000, h=height, type="char")
  }

  ## set bottom margin to zero on the gui plot
  oldmai <- par("mai")
  exitscript <- paste( exitscript, "par(mai=", oldmai , ");", sep="")
  mai <- oldmai
  mai[1] <- 0
  par(mai=mai)

  ## this needs to come here since the par() call above needs to happen
  ## before dev.off()
  if( psfile != "") 
      exitscript <- paste( exitscript, "dev.off();", sep="")
  on.exit( exitscript )
  
  frame()
  
  if( rms ) {
    par(fig=c(0,1,0.8,1))
    uttplot(segs, "rms", xlab="", colour=F, axes=F)
    box()

    mai <- oldmai
    mai[1] <- 0
    mai[3] <- 0
    par(mai=mai)
    fig <- c(0,1,0.3,0.8)
    formdolabels <- F
  } else {
    fig <- c(0,1,0.3,1)
    formdolabels <- T
  }

  par(fig=fig)
  if (is.R()) {
    image(z=spec, axes=F, col=gray(1:16/16))
  } else {
    image(spec, axes=F)
  }
  par(new=T)
  uttplot(segs, formants, ylim=frange, xlab="", zeros=T,ylab="frequency (Hz)",
	  defcolour=0, colour=F, axes=F, dolabels=formdolabels, 
	  smoothing=smoothformants)
  axis(2)
  box()
  par(new=T)
  mai <- oldmai
  mai[3] <- 0
  par(mai=mai)
  par(fig=c(0,1,0,0.3))
  uttplot(segs, track, colour=F, dolabels=F,axes=F,...)
  axis(1)
  box()

  par(fig=c(0,1,0,1))
}


# Local Variables:
# mode:S
# S-temp-buffer-p:t
# End:
