\name{by.trackdata}
\alias{by.trackdata}
\alias{by}

\title{A method of the generic function by for objects of class \'trackdata\'}
\description{
   A given function 'FUN' is applied to the data corresponding to each segment of data.
}
\usage{
  \method{by}{trackdata}(data, INDICES=NULL,FUN, \dots, simplify = FALSE
)
}
\arguments{
  \item{data}{a track data object }
  \item{INDICES} {a list of segment indices, like a label vector}
  \item{FUN}{a function that is applied to each segment}
  \item{\dots}{ arguments of the function fun}
  \item{simplify}{simplify = TRUE , output is a matrix; simplify = FALSE a list is returned}
}

\details{
It is the same as trackapply but with the extension to subsume calculation to groups of segments.
Note, if you do not want to apply the function fun to a special group of segments, use \link{trackapply} instead.
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
   \code{\link{trackapply}},
   \code{\link{by}},
   \code{\link{trackdata}}
   \code{\link{dapply}}
   \code{\link{smooth}} 
   \code{\link{apply}} 
   
}


\examples{
  data(demo.vowels)
  data(demo.vowels.fm)


   #mean F1 subsumed for each vowel
   ################################
   lab = label(demo.vowels)
   by(demo.vowels.fm[,1], lab ,mean ,simplify=FALSE)


   #mean F1 subsumed for segment onsets mids and offsets
   ##############################################
   data = demo.vowels.fm
   llabs = NULL
   for (ind in 1:dim(data$ftime)[1]) {
     seglabs = rep("mid",data$index[ind,2]-data$index[ind,1]+1)
     seglabs[1] = "on"
     seglabs[length(seglabs)] = "off"
     llabs = as.vector(c(llabs , seglabs))
   }

   by(demo.vowels.fm[,1], llabs , mean , simplify=FALSE)

   #mean F1 subsumed for segment onsets mids and offsets subsumed for each vowel
   #####################################################################
   by(demo.vowels.fm[,1], list(lab = lab, llabs = llabs) , mean , simplify=FALSE)


}

\keyword{methods}

