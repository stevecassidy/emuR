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


"cepstrum"<- function(vals, points = 20, spectrum = F)
{
  ## vals:  a matrix of spectral values. Output of muspec
  ## points: number of cepstral coefficients; 
  ## roughly 20 for a sam. rate of 20 kHz
  ## spectrum: if T, a cepstrally smoothed spectrum is calculated
  ## that can then be plotted using the fft.plot() function
  ## the cepstral coefficients are stored in mat$coeff
  ## the cepstrally smoothed spectrum in mat$cep
  ## examples: mu.sub36
  ## written by Jonathan Harrington, 1992
  mat <- NULL

  if(!is.matrix(vals)) vals <- rbind(vals)

  z <- vals[, -ncol(vals)]
  x <- rev(t(z))

  ycol <- ncol(z)
  if(is.null(ycol)) ycol <- length(z)

  x <- matrix(x, ncol = ycol, byrow = T)
  x <- apply(x, 2, rev)
  if(!is.matrix(x)) x <- rbind(x)

  newvals <- cbind(vals, x)

  cepvals <- cepstrum.complex(newvals, points = points)

  mat$coeff <- t(cepvals[1:points,  ])

  if(spectrum) {
    results <- cep.db(cepvals)
    colnum <- (ncol(results)/2) + 1
    mat$cep <- results[, 1:colnum]
  }
  mat
}



"cepstrum.complex"<- function(logvals, points = NULL, normlen = T)
{
  ## logvals are in db and would usually be output from the
  ## Fft.db function. If logvals is a matrix, successive rows
  ## correspond to different segments. logvals can
  ## also be a vector corresponding to a single segment
  ## Note: logvals should
  ## be db values of the REFLECTED spectrum, i.e. all
  ## 200 columns for a 200 point fft. 
  ## points: the number of cepstral coefficients to be
  ## calculated. The output is a set of paired real and
  ## imaginary parts, with all values set to zero except
  ## the first 20 and last 19 values if points is set to 20
  ## If normlen is true, the inverse fft is divided by the
  ## square root of the number of points

  if(is.matrix(logvals) == F) logvals <- rbind(logvals)

  n <- ncol(logvals)

  ## apply inverse fft to each row of the data
  invlogvals <- apply(logvals, 1, fft, inverse = T)

  if(normlen) invlogvals <- invlogvals/sqrt(n)

  if(!missing(points)) {
    lowlim <- points + 1
    upplim <- n - points + 2
    invlogvals[lowlim:upplim,  ] <- 0
  }
  invlogvals
}


"cep.db"<- function(cep.mat)
{
# cep.mat: cepstral coefficients that are the output of
# the cepstrum.complex program. the nth columns of cep.mat is
# parallel to any segment list.
# the result returned is the spectrum of the cepstral coefficients
# such that the nth row is parallel to the segment list
	complexvals <- fft.complex(cep.mat)
	-(fft.mod(complexvals))
}




# Local Variables:
# mode:S
# S-temp-buffer-p:t
# End:
