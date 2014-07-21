"buildtrack" <- function(mylist, ftime=NULL, trackname="")
{
  # convert a list that is usually output from by() into
  # a trackdata object
  
  # examples
  # p = by(vowlax.fdat[1:3,2], diff)
  # m = trackfromlist(p)
  # p = by(vowlax.fdat[1:3,], apply, 2, diff)
  # o = trackfromlist(p)
  # r = by(vowlax.fdat[1:3,2], smooth)
  # should break because no ftimes
  # trackfromlist(r)
  # e = trackfromlist(r, ftime=vowlax.fdat[1:3,]$ftime)
  # m = by(vowlax.fdat[1:3,], apply, 2, smooth)
  # e = trackfromlist(m, ftime=vowlax.fdat[1:3,]$ftime)
  
  res <- NULL
  for(j in 1:length(mylist)){
    if(!is.matrix(mylist[[j]]))
      mylist[[j]] <- cbind(mylist[[j]])
    N <- nrow(mylist[[j]])
    if(is.null(ftime))
    {
      if(is.null(dimnames(mylist[[j]])[[1]]))
        stop("can't find any ftime values")
      times <- as.numeric(dimnames(mylist[[j]])[[1]])
      times <- c(times[1], times[N])
    }
    else 
      times <- ftime[j,]
    res$data <- rbind(res$data, cbind(mylist[[j]]))
    res$index <- c(res$index, N)
    res$ftime <- rbind(res$ftime, times)
  }
  # build indices
  n <- res$index
  right <- cumsum(n)
  left <- c(1, right+1)
  left <- left[-length(left)]
  res$index <- cbind(left, right)
  as.trackdata(res$data, res$index, res$ftime, trackname)
}

