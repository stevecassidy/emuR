\name{by}
\alias{by.trackdata}
\alias{by}

\title{A method of the generic function by for objects of class \'trackdata\'}
\description{
   A given function 'FUN' is applied to the data corresponding to each segment of data.
}
\usage{
   by(trackdata, fun, ..., simplify = FALSE)
}
\arguments{
  \item{trackdata}{a track data object }
  \item{fun}{a function that is applied to each segment}
  \item{\dots}{ arguments of the function fun}
  \item{simplify}{simplify = TRUE , output is a matrix; simplify = FALSE a list is returned}
}

\details{
  by.trackdata() is provides a way of  applying a function iteratively
to each segment of a trackdata object without the need
for using a for-loop. It can be used to calculate, for example,
the mean value of the data values of each segment separately.
Any function that can be applied sensibly to
trackdata[j]$data where j is a segment number can be
used as the fun argument to by.trackdata(). It is also possible
to write your own function and use by.trackdata() to apply
it separately to each segment.
Care needs to be taken in using by.trackdata() in the
following two ways. Firstly, the argument simplify=T should
only be set if it can be guaranteed that 
a vector of the same length or matrix of the same
number of rows as the number of segments in the trackdata
object is returned. For example, simplify=T can
be used in calculating the mean per segment of
a trackdata object, because there will only be one
value (the mean) per segment. However, simplify should
be set to F in calculating the range because here
two values are returned per segment. Similarly use simplify=F
n smoothing the data in which the number of values returned per segment
is different. 
Secondly, by.trackdata() only applies a function
to a single parameter; the function can be used to
apply to a function to multi-parameter trackdata such
as F1-F4, but then the function needs to be put inside
apply() - see examples below.
}
\value{
  list or vector
}

\author{Jonathan Harrington}


\seealso{ 
   \code{\link{by}},
   \code{\link{trackdata}}
   \code{\link{dapply}}
   \code{\link{smooth}} 
   \code{\link{apply}} 
   
}


\examples{
   data(vowlax)

   #segment list vowlax - first segment only 
   vowlax[1,]
   
   #F0 track data object for vowlax - first segment only 
   vowlax.fund[1,]

   #To get the maximum F0 value for each segment in the track data object
   maxF0 = by(vowlax.fund, max)
   
   #maxF0 - for the first ten segments only 
   maxF0[1:10]
   \dontshow{readline(prompt = "Press return to advance.")}

   #maxF0 is a list of length 410 because there are 410 segments in the track data object
   length(maxF0)
   nrow(vowlax.fund$index)
   \dontshow{readline(prompt = "Press return to advance.")}

   #get the the maximum F0 value for each segment in the track data object
   #this time return a vector 
   maxF0 = by(vowlax.fund, max, simplify=T)
   maxF0[1:10]
   \dontshow{readline(prompt = "Press return to advance.")}
   
   

   #another Example
   #---------------
   
   data(dip)
   #dip.fdat - formant F1, F2 track data of the segment list dip (see data(dip))
   #Apply mean() to each column (F1 F2) to each segment in the track data object, 
   #return a matrix 
   
   mean.dip.fdat = by(dip.fdat, apply, 2, mean, simplify=T)
   mean.dip.fdat[1:10,] 
   #the function in FUN is apply(X, MARGIN, FUN, ...), thus argument "2" and "mean"
   #are arguments of apply.



}

\keyword{methods}

