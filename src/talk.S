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


"utt.talk"<- function(utt.dat, internalspeaker = F)
{
# interactive playback on any waveform produced with
# uttplot with talk=T. left button twice to define
# the interval, middle button to quit
# example: mu.sub37a
# written by Jonathan Harrington, 1992
  
  samrate _ utt.dat$samrate

  cat("Use left mouse button to define left and right boundaries\n")
  cat("To quit, click on the middle mouse button\n")


  repeat {
    times.a <- locator(1)
    if(!any(times.a$x)) break
    abline(v = times.a$x)
    times.b <- locator(1)
    abline(v = times.b$x)
    if(times.a$x[1] < utt.dat$ftime[1, 1])
      left <- 1
    else 
      left <- round((times.a$x[1] - utt.dat$ftime[1, 1])/(1000/samrate)) + 1

    if(times.b$x[1] > utt.dat$ftime[nrow(utt.dat$ftime), 2])
      right <- length(utt.dat$data)
    else 
      right <- round((times.b$x[1] - utt.dat$ftime[1, 1])/(1000/samrate)) + 1

    play(utt.dat$data[left:right], samrate, internalspeaker = internalspeaker)
    abline(v = times.a$x, col = 0)
    abline(v = times.b$x, col = 0)
    box()
    invisible()
  }
}

# Local Variables:
# mode:S
# S-temp-buffer-p:t
# End:
