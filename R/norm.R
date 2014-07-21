

"norm"<- function(data, speakerlabs, type = "gerst", rescale = FALSE)
{
  ## data: a matrix of data. Can be
  ## an n-columned matrix or a list as returned by track()
  ## speakerlabs, a parallel
  ## vector of speaker labels
  ## type: which normalisation do you want?
  ## type can be "nearey", "cen", "lob", "gerst" (default)
  ## for normalisation according to Nearey, centroid method
  ## Lobanov, Gerstman.
  ## rescale: do you want to rescale? 
  ## for all normalisations except nearey, rescaling works by factoring
  ## in the corresponding data across all talkers - e.g. for
  ## lobanov, s and m, the standard deviation and mean
  ## are calculated across all talkers; the lobanov
  ## normalised data are then multiplied by s to which m is added;
  ## for nearey, the normalised values are rescaled linearly
  ## in the ranges of the parameters across all the talkers; 
  ## e.g. if the range on the first parameter across all talkers
  ## is 200 to 1000, then the first nearey-scaled parameter
  ## is rescaled linearly within this range
  flag <- FALSE
  if(is.list(data)) {
    flag <- TRUE
    indvals <- data$index
    tvals <- data$ftime
    speakerlabs <- expand_labels(indvals, speakerlabs)
    data <- data$data
  }
  if(!is.matrix(data))
    data <- cbind(data)
  if(rescale) {
    if(type == "lob") {
      mvals <- apply(data, 2, mean)
      sdvals <- sqrt(apply(data, 2, var))
    }
    if(type == "nearey")
      neardata <- data
    if(type == "gerst") {
      mind <- apply(data, 2, min)
      maxd <- apply(data, 2, max)
      ranged <- maxd - mind
    }
  }
  if(type == "cen")
    cendata <- data
  for(j in unique(speakerlabs)) {
    temp <- speakerlabs == j
    vals <- data[temp,  ]
    if(type == "gerst")
      nvals <- gerst.sub(vals)
      else if(type == "lob")
	nvals <- lob.sub(vals)
	else if(type == "nearey")
	  nvals <- nearey.sub(vals)
	  else if(type == "cen")
	    nvals <- cen.sub(vals)
    data[temp,  ] <- nvals
  }
  if(rescale) {
    if(type == "lob")
      data <- rescale.lob(data, mvals, sdvals)
    if(type == "nearey")
      data <- rescale.nearey(data, neardata)
    if(type == "gerst")
      data <- rescale.gerst(data, mind, ranged)
    #if(type == "cen")
    # data <- rescale.cen(data, cendata)
  }
  if(flag) {
    vec <- data
    data <- NULL
    data$data <- vec
    data$index <- indvals
    data$ftime <- tvals
  }
  data
}

"gerst.sub"<- function(data)
{
  minvals <- apply(data, 2, min)
  maxvals <- apply(data, 2, max)
  rvals <- maxvals - minvals
  vec1 <- rep(minvals, nrow(data))
  vecmat <- matrix(vec1, nrow(data), byrow = TRUE)
  rvec1 <- rep(rvals, nrow(data))
  rvecmat <- matrix(rvec1, nrow(data), byrow = TRUE)
  (data - vecmat)/rvecmat
}

"lob.sub"<- function(data)
{
  if(!is.matrix(data))
    data <- cbind(data)
  meanvals <- apply(data, 2, mean)
  rvals <- sqrt(apply(data, 2, var))
  vec1 <- rep(meanvals, nrow(data))
  vecmat <- matrix(vec1, nrow(data), byrow = TRUE)
  rvec1 <- rep(rvals, nrow(data))
  rvecmat <- matrix(rvec1, nrow(data), byrow = TRUE)
  (data - vecmat)/rvecmat
}


"nearey.sub"<- function(data)
{
  if(!is.matrix(data))
    data <- cbind(data)
  ldata <- log(data)
  meanvals <- apply(ldata, 2, mean)
  tmean <- mean(meanvals)
  ldata - tmean
}

"cen.sub"<- function(data)
{
  if(!is.matrix(data))
    data <- cbind(data)
  mat <- NULL
  meanvals <- apply(data, 2, mean)
  for(j in 1:ncol(data)) {
    newdat <- data[, j] - meanvals[j]
    mat <- cbind(mat, newdat)
  }
  mat
}

"expand_labels"<- function(indvals, labs)
{
  ## indvals is the index component returned by frames/track
  ## labs is the parallel label vector
  ## returns a vector of labels, one for each row in the data matrix
  ## that corresponds to indvals
  mat <- NULL
  for(j in 1:nrow(indvals)) {
    rightin <- indvals[j, 2]
    leftin <- indvals[j, 1]
    num <- rightin - leftin + 1
    vec <- rep(labs[j], num)
    mat <- c(mat, vec)
  }
  mat
}


"rescale.lob"<- function(data, mvals, sdvals)
{
# rescales the Lobanov normalised data. mvals is the
# mean of the raw data, sdvals the standard dev. of
# the raw data
	if(!is.matrix(data)) data <- mvals + (data * sdvals) else mat <- NULL
	{
		for(j in 1:ncol(data)) {
			vec <- data[, j] * sdvals[j]
			mvec <- vec + mvals[j]
			mat <- cbind(mat, mvec)
		}
	}
	mat
}


"rescale.gerst"<- function(data, mind, ranged)
{
	for(j in 1:ncol(data)) {
		data[, j] <- data[, j] * ranged[j] + mind[j]
	}
	data
}



"rescale.nearey"<- function(data, neardata)
{
	if(!is.matrix(data))
		data <- rbind(data)
	for(j in 1:ncol(data)) {
		rval <- max(neardata[, j]) - min(neardata[, j])
		mindata <- min(data[, j])
		maxdata <- max(data[, j])
		rangedata <- maxdata - mindata
		data[, j] <- min(neardata[, j]) + 
		  (((data[, j] - mindata)/rangedata) * rval)
	}
	data
}


# Local Variables:
# mode:S
# S-temp-buffer-p:t
# End:
