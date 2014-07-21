##' Track data object
##' 
##' A track data object is the result of emu.track().
##' 
##' 
##' @aliases trackdata Math.trackdata Math2.trackdata Ops.trackdata
##' Summary.trackdata
##' @format \describe{ \item{\$index}{a two columned matrix, each row keeps the
##' first and last index of the \$data rows that belong to one segment}
##' \item{\$ftime}{a two columned matrix, each row keeps the times marks of one
##' segment} \item{\$data}{a multi-columned matrix with the real track values
##' for each segment} }
##' @note The entire data track is retrieved for each segment in the segment
##' list. The amount of data returned will depend on the sample rate and number
##' of columns in the track requested.
##' @section Methods: The following generic methods are implemented for
##' trackdata obects.  \describe{ \item{list("Arith")}{\code{"+"}, \code{"-"},
##' \code{"*"}, \code{"^"}, \code{"%%"}, \code{"%/%"}, \code{"/"}}
##' \item{list("Compare")}{\code{"=="}, \code{">"}, \code{"<"}, \code{"!="},
##' \code{"<="}, \code{">="}} \item{list("Logic")}{\code{"&"}, \code{"|"}.  }
##' \item{list("Ops")}{\code{"Arith"}, \code{"Compare"}, \code{"Logic"}}
##' \item{list("Math")}{\code{"abs"}, \code{"sign"}, \code{"sqrt"},
##' \code{"ceiling"}, \code{"floor"}, \code{"trunc"}, X \code{"cummax"},
##' \code{"cummin"}, \code{"cumprod"}, \code{"cumsum"}, \code{"log"},
##' \code{"log10"}, \code{"log2"}, \code{"log1p"}, \code{"acos"},
##' \code{"acosh"}, \code{"asin"}, \code{"asinh"}, \code{"atan"},
##' \code{"atanh"}, \code{"exp"}, \code{"expm1"}, \code{"cos"}, \code{"cosh"},
##' \code{"sin"}, \code{"sinh"}, \code{"tan"}, \code{"tanh"}, \code{"gamma"},
##' \code{"lgamma"}, \code{"digamma"}, \code{"trigamma"} }
##' \item{list("Math2")}{\code{"round"}, \code{"signif"}}
##' \item{list("Summary")}{\code{"max"}, \code{"min"}, \code{"range"},
##' \code{"prod"}, \code{"sum"}, \code{"any"}, \code{"all"}} }
##' @seealso \code{\link{emu.track}}, \code{\link{demo.vowels.fm}}
##' \code{\link{demo.all.rms}}
##' @keywords classes
##' @examples
##' 
##'    data(demo.vowels.fm)
##'    data(demo.vowels)
##'    
##'    #Formant track data for the first segment of the segment list demo.vowels
##'    demo.vowels.fm[1]
##'   
##' 

##########################################

##' A method of the generic function dim for objects of class \'trackdata\'
##' 
##' The function returns the dimension attributes of a track data object.
##' 
##' The function returns the dimension attributes of a track data object as the
##' number of segments x number of tracks.  c(nrow(x$index), ncol(x$data))
##' 
##' @aliases dim.trackdata dim
##' @param x a track data object
##' @author Jonathan Harrington
##' @keywords methods
##' @examples
##' 
##'    #isol.fdat is the formant track of the segment list isol
##' 
##'    #write out the dimension of the track data object 
##'    dim.trackdata(isol.fdat)
##' 
##'    #because there are 13 segments
##'    isol.fdat$ftime
##' 
##'    #and there are 4 rows for each segment (see here for the first segment)
##'    isol.fdat$data[1,]
##' 
##' @export dim.trackdata
dim.trackdata <- function(x)
{
  # function returns the dimension attributes of
  # a trackdata object as the number of segments x number of tracks
  c(nrow(x$index), ncol(x$data))
}




##' Dimnames of trackdata object
##' 
##' returns dimension names of trackdata objects
##' 
##' 
##' @param x trackdata object
##' @keywords methods
##' @export dimnames.trackdata
"dimnames.trackdata" <- function(x)
{
  trackdata = x
  dimnames(trackdata$data)
}
