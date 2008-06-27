\name{trapply}
\alias{trapply}

\title{A method of the generic function by for objects of class \'trackdata\'}
\description{
   A given function 'FUN' is applied to the data corresponding to each segment of data.
}
\usage{
  trapply (trackdata, fun, \dots, simplify = FALSE , buildtrack = FALSE)
}
\arguments{
  \item{trackdata}{a track data object }
  \item{fun}{a function that is applied to each segment}
  \item{\dots}{ arguments of the function fun}
  \item{simplify}{simplify = TRUE , output is a matrix; simplify = FALSE a list is returned}
  \item{buildtrack}{buildtrack = TRUE , output is a track data object; buildtrack = FALSE a list or matrix is returned depending on simplify}
}

\details{
  trapply() is provides a way of  applying a function iteratively
to each segment of a trackdata object 
It can be used to calculate, for example,
the mean value of the data values of each segment separately.
Any function that can be applied sensibly to
trackdata[j]\$data where j is a segment number can be
used as the fun argument to trapply(). It is also possible
to write your own function and use trapply() to apply
it separately to each segment.
Care needs to be taken in using trapply() in the
following two ways. Firstly, the argument buildtrack=T should
only be set if it can be guaranteed that 
a vector of the same length or matrix of the same
number of rows as the number of segments in the trackdata
object is returned. For example, buildtrack=T can
be used in calculating the mean per segment of
a trackdata object, because there will only be one
value (the mean) per segment. However, simplify should
be set to F in calculating the range because here
two values are returned per segment. Similarly use simplify=F
n smoothing the data in which the number of values returned per segment
is different. 
Secondly, trapply() only applies a function
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
   maxF0 = trapply(vowlax.fund,max)
   
   #maxF0 - for the first ten segments only 
   maxF0[1:10]
   

   #maxF0 is a list of length 410 because there are 410 segments in the track data object
   length(maxF0)
   nrow(vowlax.fund$index)
   

   #Mean formant value per column (F1, F2, F3, F4) for the first 10 segments
   trapply(vowlax.fdat[1:10,], apply, 2, mean, simplify=TRUE)

   #get the the maximum F0 value for each segment in the track data object
   #this time return a vector 
   maxF0 = trapply(vowlax.fund, max, simplify=TRUE)
   maxF0[1:10]
   
   # get a track data object of DCT-smoothed (first two coefficients) F1 values
   vowlax.fdat.F1.dct = trapply(vowlax.fdat[,1], dct, 2, T, buildtrack=TRUE)
   
   

   
   
}

\keyword{manip}

