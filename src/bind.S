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


"bind"<- function(a,...)
{
  UseMethod("bind")
}

  ## default is just to use rbind
"bind.default" <- function(...)
{
  rbind(...)
}

"bind.trackdata"<- function(...)
{
  ## function to combine datasets into one single datasets
  ## any number of datasets accepted e.g. dcombine(x, y, z)
  ## where x, y, z are lists of the form $data, $index, $ftime
  mat <- NULL
  for(j in list(...)) {
    if(is.matrix(j$data))
      mat$data <- rbind(mat$data, j$data)
    else mat$data <- c(mat$data, j$data)
    mat$index <- rbind(mat$index, j$index)
    if(!is.null(j$ftime))
      mat$ftime <- rbind(mat$ftime, j$ftime)
  }
  ## readjust the index times
  diffinds <- mat$index[, 2] - mat$index[, 1] + 1
  right <- cumsum(diffinds)
  first.left <- diffinds - 1
  left <- right - first.left
  mat$index <- cbind(left, right)
  if( version$major >= 5 ) {
    oldClass(mat) <- "trackdata"
  } else {
    class(mat) <- "trackdata"
  }
  mat
}



# Local Variables:
# mode:S
# S-temp-buffer-p:t
# End:
