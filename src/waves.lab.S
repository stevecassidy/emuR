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


"waves.lab"<- function(segs, dir = NULL, extn = ".ilab")
{
# writes out Waves label files from any segment
# list. There will be one label file per utterance
# in segs. The Waves label files are created in the
# directory above the current .Data directory.
# The label files have an extension of .ilab. This
# can be set to any extension name by specifying
# extn= ".lab", for example, to create .lab files
# Note: this extension would overwrite any existing
# .lab files in the directory
# segs: a segment list
# dir: specify a directory where you want the waves files to be written to
# extn: what extension do you want for the Waves label files (
# defaults to .ilab)
  t1 <- "signal unnamed1"
  t2 <- "type 0"
  t3 <- "color 125"
  t4 <- "separator ;"
  t5 <- "nfields 1"
  t6 <- "#"
  template <- rbind(t1, t2, t3, t4, t5, t6)
  utts <- utt(segs)
  for(j in unique(utts)) {
    temp <- utts == j
    xsegs <- segs[temp,  ]
    if(is.matrix(xsegs))
      xsegs <- xsegs[sort.list(as.numeric(xsegs[, 1])),  ]
    xlabs <- label(xsegs)
    initlab <- "H#"
    inittime <- as.numeric(xsegs[1])/1000
    initval <- cbind(format(round(inittime, 6)), 125, initlab)
    if(!is.null(dir)) {
      uttnum <- splitstring(j,"/")
      uttnum <- uttnum[length(uttnum)]
      uttid <- paste(dir, "/", uttnum, extn, sep = "")
    }
    else 
      uttid <- paste(j, extn, sep = "")
    times <- muend(xsegs)/1000
    colcode <- rep(125, length(xlabs))
    mat <- cbind(format(round(times, 6)), colcode, xlabs)
    mat <- rbind(initval, mat)
    write(t(template), uttid)
    write(t(mat), uttid, 3, append = T)
  }
}


# Local Variables:
# mode:S
# S-temp-buffer-p:t
# End:
