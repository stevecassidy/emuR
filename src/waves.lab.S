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
  header <- rbind(t1, t2, t3, t4, t5, t6)
  utts <- utt(segs)

  for(utt in unique(utts)) {

    temp <- utts == utt
    utt.segs <- segs[temp,  ]
    
    ## sort the segments on their start times
    if(is.matrix(utt.segs))
      utt.segs <- utt.segs[sort.list(as.numeric(mustart(utt.segs))),  ]

    ## work out where to write this utterance
    ## this should involve asking Emu where files
    ## with this extension should live
    if(!is.null(dir)) {
      filename <- paste(dir, "/", utt, extn, sep = "")
    } else {
      filename <- paste(utt, extn, sep = "")
    }
    
    utt.labs <- c("H#", label(utt.segs))
    times <- c(mustart(utt.segs)[1], muend(utt.segs))/1000
    colcode <- rep(125, length(utt.labs))

    mat <- cbind(format(round(times, 6)), colcode, utt.labs)

    cat("Writing to ", filename, "\n" )

    write(t(header), filename)
    write(t(mat), filename, 3, append = T)
  }
}


# Local Variables:
# mode:S
# S-temp-buffer-p:t
# End:
