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


## play fn using the c-coded play written for xlispstat
## and hopefully compatible with Splus/R

"play"<- function(playdata, rate )
{
  datarange _ range(playdata)

  ## scale to 8 bits
  playdata _ 256*(playdata - datarange[1])/(datarange[2] - datarange[1])
  
  invisible(.C("xlplay",
               as.integer(playdata),
               as.integer(length(playdata)),
               as.integer(rate)))
}


# Local Variables:
# mode:S
# S-temp-buffer-p:t
# End:
