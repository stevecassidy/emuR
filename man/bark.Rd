\name{bark}
\alias{bark}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{Convert Hertz to Bark and Bark to Hertz}
\description{
 The calculation is done using the formulae Traunmüller (1990)
}
\usage{
bark(data, inv = F)
}
%- maybe also `usage' for other objects documented here.
\arguments{
 \item{data}{A vector or matrix of data}
 \item{inv}{A single element logical vector. If F, data are converted from Hertz to Bark, if T, data are converted from Bark to Hertz.}
 }
%\details{
% ~~ If necessary, more details than the __description__  above ~~
%}
\value{
  A vector of matrix of the same length and dimensions as data.
}
\references{  Traunmüller, H. (1990) "Analytical expressions for the tonotopic sensory scale" J. Acoust. Soc. Am. 88: 97-100.}
\author{Jonathan Harrington }
%\note{ ~~further notes~~ }

 ~Make other sections like WARNING with \section{WARNING }{....} ~

\seealso{ 
     \code{\link{zwicker}},  
     \code{\link{eplot}}
}

\examples{

   # convert Hertz values to Bark
   vec <- c(500, 1500, 2500)
   vec
   bark(vec)

   \dontshow{readline(prompt = "Press return to advance.")}
   # convert Hertz values to Bark and back to Hertz
   bark(bark(vec, inv=T))
   \dontshow{readline(prompt = "Press return to advance.")}

   # convert the $data values in a trackdata object to Bark
   # create a new track data object
   t1 <- dip.fdat
   t1[1]
   \dontshow{readline(prompt = "Press return to advance.")}
   # convert Hertz to Bark
   t1$data <- bark(t1$data)
   t1[1]
   \dontshow{readline(prompt = "Press return to advance.")}

}
\keyword{ emu}
