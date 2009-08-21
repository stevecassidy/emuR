"is.spectral" <-
  function(dat)
{
  if(!is.trackdata(dat))
    return(any(class(dat) %in% "spectral"))
  else
    return(any(class(dat$data) %in% "spectral"))
}



"as.spectral" <-
  function(trackdata, fs)
{
  if(is.trackdata(trackdata)){

    if(is.spectral(trackdata$data)) {
      warning("matrix is already of class spectral")
      return(trackdata)
      }
    N <- ncol(trackdata$data)
    if(missing(fs))
      fs <- 0: (ncol(trackdata$data)-1)
    else{
      if(length(fs)==1)
        {
          fs <- fs/2
          fs <- seq(0, fs, length=N)
        }
    }
    attr(trackdata$data, "fs") <- fs
    class(trackdata$data) <- c(class(trackdata$data), "spectral")
  }

  else if (is.matrix(trackdata)){
    if(is.spectral(trackdata)) {
      warning("matrix is already of class spectral")
      return(trackdata)
      }
    N <- ncol(trackdata)
    if(missing(fs))
      fs <- 0: (ncol(trackdata)-1)
    else{
      if(length(fs)==1)
        {
          fs <- fs/2
          fs <- seq(0, fs, length=N)
        }
    }
    attr(trackdata, "fs") <- fs
    class(trackdata) <- c(class(trackdata), "spectral")
  }
  else
    {

      if(is.spectral(trackdata)){
        warning("matrix is already of class spectral")
        return(trackdata)
      }
      N <- length(trackdata)
      if(missing(fs))
        fs <- 0: (length(trackdata)-1)
      else{
        if(length(fs)==1)
          {
            fs <- fs/2
            fs <- seq(0, fs, length=N)
          }
      }
      attr(trackdata, "fs") <- fs
      class(trackdata) <- c(class(trackdata), "spectral")
    }
  trackdata
}




`plot.spectral` <-
  function (x, labs, ylim, xlim,  col, lty, lwd, fun, 
            freq, type = "l", power = FALSE, powcoeffs = c(10, 10), dbnorm = FALSE, 
            dbcoeffs = c(0, 0), legend = TRUE, axes=TRUE,  ...) 
{
  specdata = x
  if (is.trackdata(specdata)) 
    specdata <- specdata$data
  if (!is.spectral(specdata)) 
    stop("specdata must be of class spectral")
  if (dbnorm) 
    specdata <- dbnorm(specdata, dbcoeffs[1], dbcoeffs[2])
  if (missing(freq)) 
    f <- trackfreq(specdata)
  else f <- freq
  if (is.matrix(specdata)) 
    N <- nrow(specdata)
  else {
    N <- 1
    specdata <- rbind(specdata)
  }
  if (missing(labs)) 
    labs <- rep(".", N)
  if (!missing(fun)) {
    if (power) 
      specdata <- dbtopower(specdata, powcoeffs[1], powcoeffs[2])
    mat <- list(NULL)
    for (j in unique(labs)) {
      temp <- labs == j
      v <- apply(specdata[temp, ], 2, fun)
      mat$fn <- rbind(mat$fn, v)
      mat$l <- c(mat$l, j)
    }
    dimnames(mat$fn) <- list(mat$l, dimnames(specdata)[[2]])
    specdata <- mat$fn
    if (power) 
      specdata <- dbtopower(specdata, powcoeffs[1], powcoeffs[2], 
                            inv = TRUE)
    if (length(unique(labs)) > 1) 
      labs <- dimnames(specdata)[[1]]
    else {
      labs <- unique(labs)
      specdata <- rbind(specdata)
    }
  }
  if (missing(ylim)) 
    ylim <- range(specdata)
  if (missing(xlim)) 
    xlim <- range(f)
  if (missing(col)) 
    col <- TRUE
  if (missing(lty)) 
    lty <- FALSE
  if (missing(lwd)) 
    lwd <- NULL
  cols <- mu.colour(labs, col, lty, lwd)
  for (j in 1:nrow(specdata)) {
    plot(f, specdata[j, ], type = type, col = cols$colour[j], 
         lty = cols$linetype[j], lwd = cols$lwd[j], xlim = xlim, 
         ylim = ylim, xlab = "", ylab = "", axes = FALSE)
    par(new = TRUE)
  }
  if (is.logical(legend)) {
    if (legend & length(unique(labs)) > 1) {
      legend <- "topright"
      legend(legend, NULL, cols$legend$lab, col = cols$legend$col, 
             lty = as.numeric(cols$legend$lty), lwd = as.numeric(cols$legend$lwd))
    }
  }
  else legend(legend, NULL, cols$legend$lab, col = cols$legend$col, 
              lty = as.numeric(cols$legend$lty), lwd = as.numeric(cols$legend$lwd))
  if(axes)
    {
      axis(side = 1)
      axis(side = 2)
    }
  title(...)
  box(...)
}



`bark.spectral` <-
  function (f, ...) 
{
  specobject = f
  if (!is.trackdata(specobject)) {
    if (!is.matrix(specobject)) 
      specobject <- as.spectral(rbind(specobject), attr(specobject, 
                                                        "fs"))
  }
  f <- trackfreq(specobject)
  b <- bark(f)
  temp <- b < 0
  if (any(temp)) 
    specobject <- specobject[, !temp]
  f <- trackfreq(specobject)
  b <- bark(f)
  N <- length(b)
  ba <- seq(min(b), max(b), length = N)
  if (is.trackdata(specobject)) 
    spec <- specobject$data
  else if (is.matrix(specobject)) 
    spec <- specobject
  else spec <- as.spectral(rbind(specobject), attr(specobject, 
                                                   "fs"))
  res <- NULL
  for (j in 1:nrow(spec)) {
    v = approx(b, c(spec[j, ]), ba)
    res <- rbind(res, v$y)
  }
  if (is.trackdata(specobject)) {
    specobject$data <- res
    if (!is.null(tracktimes(spec))) 
      rownames(specobject$data) <- tracktimes(spec)
    specobject <- as.spectral(specobject, ba)
  }
  else {
    specobject <- res
    specobject <- as.spectral(specobject, ba)
  }
  specobject
}


`mel.spectral` <-
  function (a) 
{
  specobject = a
  if (!is.trackdata(specobject)) {
    if (!is.matrix(specobject)) 
      specobject <- as.spectral(rbind(specobject), attr(specobject, 
                                                        "fs"))
  }
  f <- trackfreq(specobject)
  b <- mel(f)
  N <- length(b)
  ba <- seq(min(b), max(b), length = N)
  if (is.trackdata(specobject)) 
    spec <- specobject$data
  else if (is.matrix(specobject)) 
    spec <- specobject
  else spec <- as.spectral(rbind(specobject), attr(specobject, 
                                                   "fs"))
  res <- NULL
  for (j in 1:nrow(spec)) {
    v = approx(b, c(spec[j, ]), ba)
    res <- rbind(res, v$y)
  }
  if (is.trackdata(specobject)) {
    specobject$data <- res
    if (!is.null(tracktimes(spec))) 
      rownames(specobject$data) <- tracktimes(spec)
    specobject <- as.spectral(specobject, ba)
  }
  else {
    specobject <- res
    specobject <- as.spectral(specobject, ba)
  }
  specobject
}




