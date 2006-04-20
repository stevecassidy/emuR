\name{by.trackdata}
\alias{by.trackdata}
\alias{by}

\title{A method of the generic function by for objects of class \'trackdata\'}
\description{
   A given function 'FUN' is applied to the data corresponding to each segment of data.
}
\usage{
   by(trackdata, fun, ..., simplify = F)
}
\arguments{
  \item{trackdata}{a track data object }
  \item{fun}{a function that is applied to each segment}
  \item{\dots}{ arguments of the function fun}
  \item{simplify}{simplify = T , output is a matrix; simplify = F a list is returned}
}

\details{
  It's similar to dapply(). dapply() returns a track data object as by() returns a list or a vector.
  These lists or vectors can be rearranged to track data objects see example below.
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
\keyword{emu}
\keyword{methods}

