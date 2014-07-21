library(stats)

"euclidean"<- function(data, m = 1, n = ncol(data))
{
  ## returns  a vector of Euclidean distances between adjacent
  ## pairs i.e. the Euclidean distance from data[1,] to
  ## data[2,], then data[3,] to data[4,] etc. data 
  ## must of course be a matrix of any number of dims
  ## It  makes use of the Splus program dist
  ## m and n are the columns of data over which the euclidean
  ## distances are to be calculated (defaults to all the columns)
  data <- data[, m:n]
  lengths <- nrow(data)
  downstep <- seq((lengths - 1), 2, -1)
  values <- c(1, 1 + cumsum(downstep))
  dist(data)[values]
}


# Local Variables:
# mode:S
# S-temp-buffer-p:t
# End:
