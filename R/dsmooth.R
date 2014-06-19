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


"dsmooth"<- function(dataset)
{
  ## dataset: a list, as returned by track
  ## separately smooths the segments corresponding to the data
  dapply(dataset, dsmooth.sub)
}


"dsmooth.sub" <- function(data, ftime)
{
  if(is.matrix(data)){
    if(nrow(data)>5)
      data <- apply(data, 2, smooth)
  } else {
    if(length(data)>5)
      data <- smooth(data)
  }
  return( list(data=data, ftime=ftime) )
}


# Local Variables:
# mode:S
# S-temp-buffer-p:t
# End:
