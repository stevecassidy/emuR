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

"mu.legend"<- function(legn, xlim, ylim)
{
  fudge.x _ (xlim[2]-xlim[1])/5
  fudge.y _ (ylim[2]-ylim[1])/5
  if(legn=="tl")
    return(list(x=xlim[1], y=ylim[2]))
  if(legn=="tr")
    return(list(x=xlim[2]-fudge.x, y=ylim[2]))
  if(legn=="br")
    return(list(x=xlim[2]-fudge.x, y=ylim[1]+fudge.y))
  if(legn=="bl")
    return(list(x=xlim[1], y=ylim[1]+fudge.y))
  if(legn=="loc")
    return(locator(1))
  stop("Unknown legend locator in mu.legend")
}


# Local Variables:
# mode:S
# S-temp-buffer-p:t
# End:
