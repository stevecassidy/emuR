
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
  } else {
    dapply(dataset, dextract.sub, start, end)
  }
}

# helper function for use via dapply, returns a new
# trackdata element cut at start/end proportions
"dextract.sub" <- function (data, ftime, start, end) 
{
  len <- nrow(data)
  start <- floor(start * (len - 1) + 1)
  end <- ceiling(end * (len - 1) + 1)
  
  newdata <- data[start:end, ]
  times <- seq(ftime[1], ftime[2], length = len)
  newftime <- times[c(start, end)]
  return(list(data = newdata, ftime = newftime))
}


# Local Variables:
# mode:S
# S-temp-buffer-p:t
# End:
