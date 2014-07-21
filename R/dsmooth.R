

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
