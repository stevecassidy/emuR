
"dextract.lab"<- function(dataset, labs, labtype = unique(labs))
{
  # extract data values from a dataset ($data, $index, $ftime)
  # according to labtype (e.g. "i:", c("i:", "u:").
  # labs is parallel to dataset$index; labtype are
  # the label types for which the values in dataset are
  # to be extracted
  mat <- NULL
  lvals <- dataset$index[, 2] - dataset$index[, 1] + 1
  newlabs <- rep(labs, lvals)
  temp <- muclass(newlabs, labtype)
  if(is.matrix(dataset$data))
    vals <- dataset$data[temp,  ]
  else 
    vals <- dataset$data[temp]
  
  temp.lab <- muclass(labs, labtype)
  
  if(!is.null(dataset$ftime))
    ftimes <- dataset$ftime[temp.lab,  ]
  
  finds <- dataset$index[temp.lab,  ]	
  ## readjust the indeces
  diffinds <- finds[, 2] - finds[, 1] + 1
  right <- cumsum(diffinds)
  first.left <- diffinds - 1
  left <- right - first.left
  finds <- cbind(left, right)
  mat$data <- vals
  mat$index <- finds
  if(!is.null(dataset$ftime))
    mat$ftime <- ftimes
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
