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


"mel" <- function(x)
{
	1/log(2) * (log(1 + (x/1000))) * 1000
}


"bark" <- function(x)
{
  Hz <- x/1000
  k1 <- (Hz/7.5) * (Hz/7.5)
  (13 * (atan(0.76 * Hz))) + (3.5 * (atan(k1)))
}

"zwicker"<-
structure(.Data = c(10, 100, 200, 300, 395, 510, 630, 765, 920, 1075, 1265, 
	1480, 1710, 1990, 2310, 2675, 3125, 3650, 4350, 5250, 6350, 7600, 9250, 
	11750, 90, 200, 300, 400, 505, 630, 770, 915, 1080, 1265, 1475, 1720, 
	1990, 2310, 2690, 3125, 3675, 4350, 5250, 6350, 7650, 9400, 11750, 
	15250), .Dim = c(24, 2))




## return the index of the value in vec which is closest to val
"closest" <- function(vec, val)
{
	newval <- min(abs((vec - val)))
	z <- abs(vec - val)
	temp <- z == newval
	nums <- c(1:length(vec))
	nums[temp]
}

"hamming" <- function(pts = 512)
{
  vec <- 0:(pts-1)
  0.54 - 0.46 * cos((2 * pi * vec)/(pts-1))
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


"splitmat"<- function(data, labs, filename = "file", labelfile = T)
{
# data: a vector or matrix of data; labs a parallel label
# file. Creates a new 
#  series of objects called obj.n etc. where n is an
# integer from 1 to n number of phonetic classes
# if labelfile is T, then a parallel label file is created
# called l.file.n
	newmat <- NULL
	labelfile <- NULL
	labelname <- NULL
	k <- 1
	if(!is.matrix(data))
		data <- rbind(data)
	for(j in unique(labs)) {
          temp <- muclass(labs, j)
          newdata <- data[temp,  ]
          outlab <- paste(filename, k, sep = ".")
          l.outlab <- paste("l.", outlab, sep = "")
          assign(outlab, newdata, w = 1)
          assign(l.outlab, rep(j, sum(temp)), w = 1)
          labelname <- c(labelname, j)
          newmat <- c(newmat, outlab)
          labelfile <- c(labelfile, l.outlab)
          k <- k + 1
	}
	z <- cbind(newmat, labelfile, labelname)
	headlabs <- c("filename", "label-file", "label-type")
	dimnames(z) <- list(NULL, headlabs)
	z
}


if( is.R() ) {

  splitstring <- function(str, char)  {
    return(strsplit(str, char)[[1]])
  }

} else {

splitstring <- function(str,char) {
  if(str == "")
    mat <- c(str)
  else {
    mat <- NULL
    ind <- 1
    cont <- T
    while(T) {
      ministr <- NULL
      length <- 0
      while(T) {
	ch <- substring(str, ind, ind)
	if(ch == char) {
	  ind <- ind + 1
	  break
	}
	if(ch == "") {
	  break
	}
	ministr <- c(ministr, ch)
	ind <- ind + 1
	length <- length + 1
      }
      ## now concatenate string
      if(length > 0)
	mat <- c(mat, paste(ministr, collapse = ""))
      if(ch == "")
	break
    }
  }
  mat
}

}

"uttnum"<- function(utt, str)
{
  temp <- muclass(utt, str)
  nums <- c(1:length(utt))
  nums[temp]
}

# Local Variables:
# mode:S
# S-temp-buffer-p:t
# End:
