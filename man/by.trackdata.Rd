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
It is the same as trackapply but with the extension 
to subsume calculation to groups of segments.
Note, if you do not want to apply the function fun to a 
special group of segments, use \link{trackapply} instead.
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

