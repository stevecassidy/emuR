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


"linear"<- function(dataset, n = 20)
{
  ## perform linear time normalisation of a data set as returned
  ## from track(), and including $data and $index
  outvals <- NULL
  if(!is.matrix(dataset$data))
    dataset$data <- cbind(dataset$data)
  for(j in 1:nrow(dataset$index)) {
    left <- dataset$index[j, 1]
    right <- dataset$index[j, 2]
    vec <- dataset$data[left:right,  ]
    if(!is.matrix(vec))
      vec <- cbind(vec)
    mat <- NULL
    for(k in 1:ncol(vec)) {
      linvec <- approx(vec[, k], n = n)
      mat <- cbind(mat, linvec$y)
    }
    outvals$data <- rbind(outvals$data, mat)
  }
  rightin <- seq(n, n * nrow(dataset$index), length = nrow(dataset$index))
  leftin <- rightin - n + 1
  outvals$index <- cbind(leftin, rightin)
  if(ncol(outvals$data) == 1)
    outvals$data <- c(outvals$data)
  outvals
}


"linear.av"<- function(dataset, labs)
{
  finmat <- NULL
  mat <- NULL
  if(!is.matrix(dataset$data))
    dataset$data <- cbind(dataset$data)
  for(j in 1:ncol(dataset$data)) {
    vec <- matrix(c(dataset$data[, j]), nrow(dataset$index), byrow
                  = TRUE)
    outmat <- NULL
    for(k in unique(labs)) {
      temp <- labs == k
      labvec <- vec[temp,  ]
      outval <- apply(labvec, 2, mean)
      outmat <- c(outmat, outval)
    }
    mat <- cbind(mat, outmat)
  }
  tnum <- nrow(mat)
  tlab <- length(unique(labs))
  rightin <- seq(tnum/tlab, tnum, length = tlab)
  leftin <- rightin - (tnum/tlab) + 1
  finmat$data <- mat
  finmat$index <- cbind(leftin, rightin)
  finmat$lab <- unique(labs)
  finmat
}

# Local Variables:
# mode:S
# S-temp-buffer-p:t
# End:
