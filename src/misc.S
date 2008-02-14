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








## return the index of the value in vec which is closest to val
"closest" <- function(vec, val)
{
	newval <- min(abs((vec - val)))
	z <- abs(vec - val)
	temp <- z == newval
	nums <- c(1:length(vec))
	nums[temp]
}



"label.num" <- function(labs)
{
  ## labs: a vector of labels
  ## convert labels to integers
  qq <- unique(labs)
  nums <- c(1:length(qq))
  for(j in 1:length(qq)) {
    temp <- labs == qq[j]
    labs[temp] <- nums[j]
  }
  as.numeric(labs)
}


## label.convert --
##  map one set of labels to another in a segment
##  list or label vector
##
"label.convert" <- function(segs.or.labels, match, replace)
{
  if (is.seglist( segs.or.labels ) ) {
    labs <- label(segs.or.labels)
  } else {
    labs <- segs.or.labels
  }
  if( length(match) != length(replace) ) {
    ## this is only ok if length(replace) == 1 so that we
    ## replace anything in match with replace
    if( length(replace) != 1 ) {
      stop("Lengths of match and replace vectors differ in label.convert")
    } 
  }

  if (length(replace) == 1) {
    temp <- muclass(labs, match)
    labs[temp] <- replace
  } else {
    for( i in 1:length(match) ) {
      labs[labs==match[i]] <- replace[i]
    }
  }
  if (is.seglist(segs.or.labels)) {
    return( modify.seglist(segs.or.labels, label=labs))
  } else {
    return( labs )
  }
}


"matscan"<- function(file, num.cols=count.fields(file)[1], what = 0, sk = 0)
{
  ## first make a template, a list of num.cols what's
  template <- as.list(rep(what, num.cols))
  data <- scan(file, template, skip=sk,quote = "")
  num.rows <- length(data[[1]])
  mat <- matrix(0, num.rows, num.cols)
  for(i in 1:num.cols) {
    mat[, i] <- data[[i]]
  }
  mat
}



"muclass"<- function(labels, class)
{
  !(is.na(match(labels, class)))
}

"randomise.segs" <-  function( segs, rand = T, bwd=F )
{
  if( bwd ){
    ## reverse the segment list
    segs[slen(segs):1,]
  } else {
    ## randomise the segment list
    segs[sample(1:slen(segs)),]
  }
}

"radians"<- function(degrees) (degrees * 2 * pi)/360

"sortmatrix" <- function(mat, labs = dimnames(mat)[[2]])
{
  ## labs can also be a vector of labels, which has
  ## the same number of columns as mat; e.g.
  ## if labs is "p" "t" "k", then mat
  ## will be sorted with these three labels in the first
  ## three columns
	b1 <- labs
	b2 <- dimnames(mat)[[1]]
	c1 <- match(b2, b1)
	d1 <- cbind(c1, mat)
	newmat <- d1[sort.list(d1[, 1]),  ]
	newmat <- newmat[, -1]
	b1 <- dimnames(newmat)[[1]]
	b2 <- dimnames(newmat)[[2]]
	c1 <- match(b2, b1)
	d1 <- rbind(c1, newmat)
	newmat2 <- d1[, (sort.list(d1[1,  ],  ))]
	newmat2[-1,  ]
}



"rad" <-
function(vec, samfreq = 20000, hz = T)
{
# hz: if T, vec is a vector in Hertz, otherwise it's radians
# convert from radians to Hz, or Hz to radians
	if(hz) vals <- (vec * 2 * pi)/samfreq else vals <- (vec * samfreq)/(2 * 
			pi)
	vals
}

"freqtoint" <-
function(trackdata, j){
# note to remove the dc offset, set j to -1
sg <- sign(j)
zerowhich <- sg==0
if(all(sg[!zerowhich] > 0))
sg[zerowhich] = 1
else sg[zerowhich] = -1
j <- abs(j)
fs <- trackfreq(trackdata)
N <- length(fs)
res <- 1+ (((j - fs[1]) * (N-1))/(fs[N] - fs[1]))
res[res < 1] = 1
res[res > N] = N
res <- sg * res
unique(round(res))
}


"dbnorm" <-
function(specdata, f=0, db=0)
{


if(is.trackdata(specdata))
dat <- specdata$data
else
dat <- specdata


# normalise to dbnorm
minfun <- function(specvals, f, db)
{
specvals <- specvals - specvals[f]+db
}
if(is.matrix(dat))
dat <- fapply(dat, minfun, f, db)
else
dat = dat-dat[f]+db

if(is.trackdata(specdata))
{
specdata$data <- dat
return(specdata)
}
else return(dat)

}



"dbtopower" <-
function(specdata, const = 10, base=10, inv=F)
{
# function for converting from db to power and back
if(is.trackdata(specdata))
dat <- specdata$data
else dat <- specdata
if(!inv)
result <- base^(dat/const)
else
result <- const * log(dat, base=base)
if(is.trackdata(specdata))
{
specdata$data <- result
return(specdata)
}
else return(result)
}


"shift" <-
function(x, delta = 1,  circular = T)
{
## converts x[n] into x[n-1] by multiplying the Fourier
## transform on x[n] by z^-1 i.e. by e^-iw
N <- length(x)
if(delta < 0)
delta <- N + delta
h <- c(rep(0, delta), 1)
if(!circular) {
N <- length(x) + length(h) - 1
x <- c(x, rep(0, N - length(x)))
}
filter(x, h, sides = 1, circular = T)[1:N]

}






# Local Variables:
# mode:S
# S-temp-buffer-p:t
# End:
