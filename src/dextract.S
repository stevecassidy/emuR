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

dextract <- function(dataset, start, end) {
  if((start < 0) | (start > 1)) {
    stop("proportional duration must be between 0 and 1")
  }
  if(!missing(end)) {
    if((end < 0) | (end > 1)) {
      stop("proportional duration must be between 0 and 1")
    }
    if(start > end) {
      stop("proportional start must be less than proportional end")
    }
  }

  if(missing(end)) {
    leftin <- dataset$index[, 1]
    rightin <- dataset$index[, 2]
    scalein <- round((rightin - leftin) * start)
    outin <- leftin + scalein
    if(is.matrix(dataset$data))
      return(dataset$data[outin,  ])
    else return(dataset$data[outin])
  }
  else {
    dapply(dataset, dextract.sub, start, end)
  }
}

dextract.sub <- function(data, ftime, start, end) {
  if(is.matrix(data)) {
    len <- nrow(data)
    start <- floor(start * len)
    end <- ceiling(end * len)
    data <- data[start:end,  ]
  }
  else {
    len <- length(data)
    start <- floor(start * len)
    end <- ceiling(end * len)
    data <- data[start:end]
  }
  times <- seq(ftime[1], ftime[2], length=len)
  ftime <- times[c(start,end)]
  list(data = data, ftime = ftime)
}


# Local Variables:
# mode:S
# S-temp-buffer-p:t
# End:
