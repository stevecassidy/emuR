


"frames.time" <- function(dataset, datanum)
{
  ## return the time and the number of the segment element
  ## that the datanum refers to
  if(is.matrix(dataset$ftime) == FALSE) dataset$ftime <- rbind(dataset$ftime)
  if(is.matrix(dataset$index) == FALSE)  dataset$index <- rbind(dataset$index)
  nums <- seq(1, nrow(dataset$ftime))
  incl <- dataset$index[, 1] <= datanum & dataset$index[, 2] >= datanum
  retv <- NULL
  segnum <- nums[incl]
  percent <- (datanum - dataset$index[segnum, 1])/
             (dataset$index[segnum, 2] - dataset$index[segnum, 1])
  retv$segnum <- segnum
  retv$time <- dataset$ftime[segnum, 1] + 
               percent * (dataset$ftime[segnum, 2] - dataset$ftime[segnum, 1])
  retv
}


"get.time.element"<- function(timeval, dataset)
{
  ## timeval: a time in milliseconds
  ## dataset: a data structure consisting of $data, $ftime, $index
  ## returns the element number of dataset$data corresponding to timeval
  numrows <- nrow(dataset$ftime)
  left <- dataset$ftime[1, 1]
  right <- dataset$ftime[numrows, 2]
  left.i <- dataset$index[1, 1]
  right.i <- dataset$index[numrows, 2]
  round(((timeval - left)/(right - left)) * (right.i - left.i)) + 1
}



# Local Variables:
# mode:S
# S-temp-buffer-p:t
# End:
