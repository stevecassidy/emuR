\name{emu.track}
\alias{emu.track}
\title{Retrieve Numerical Data}
\description{
  Takes the result of a database query and retrieves corresponding
  time-series data from the database. 
}
\usage{
emu.track(seglist, trackname, cut=NULL, npoints=NULL, template=attr(seglist, "database"))
}

\arguments{
  \item{seglist}{ An Emu segment list. }
  \item{trackname}{ The name of the data track to retrieve, a
    string. This must be a track name defined in the database template. }
  \item{cut}{ An optional cut time for segment data, ranges between 0
    and 1, a value of 0.5 will extract data only at the segment
    midpoint. }
  \item{npoints}{An optional number of points to retrieve for each
    segment or event. For segments this requires a cut= argument and
    data is extracted around the cut time. For events data is extracted
    around the event time.}
 \item{template}{ The database to retrieve the data from, this should
   not normally be set. }
}
\details{

  \code{emu.track} takes a segment list as input
  and retrieves associated numerical data from the corresponding
  database.  The segment list will usually be the result of a call
  to \code{\link{emu.query}} but could be constructed with the
  \code{\link{make.seglist}} function. The result is either a
  two dimensional array of data or an object of class
  \code{trackdata} which may contain multi-column data
  from many tokens.

}
\value{
  If only two arguments are supplied the entire data track is
  retrieved for each segment in the segment list. The amount of data
  returned will depend on the sample rate and number of columns in
  the track requested. The returned data is packaged up as a
  trackdata object.

  The optional \code{cut} argument specifies a cut
  point as a fraction of the duration of each segment in the segment
  list.  If this is specified the data at this single cutpoint will
  be extracted rather than that for the entire track.  The result of
  \code{emu.track} with the cut arguement is a two
  dimensional array of data with one row per segment in the original
  segment list and one column per column in the requested
  track. This array can be treated like any other array in Splus.

  If the input segment list is in fact an event list (ie. is
  derived from an annotation level defined as events in the database
  template) then the result of \code{emu.track} is the
  same as if the cut argument was specified.

  If the \code{npoints} argument is specified, it defines the number of
  points that will be returned for each segment or event.  The
  \code{cut} argument is required if the input is a segment list (as
  opposed to an event list).
}

\references{ See the Emu documentation at:
  \url{http://www.shlrc.mq.edu.au/emu} }
\author{ Steve Cassidy <Steve.Cassidy@mq.edu.au> }
\note{ This function calls an external program which is part of the Emu
  speech database system and so requires this system to be installed on
  your computer. See the Emu web site for details.  }

\seealso{ \code{\link{dplot}} \code{\link{dapply}} }

\examples{

## assumes a database called demo is available on your system and that
## the Emu system is installed. 

# find all Phonetic vowels in the database
segs <- emu.query("demo", "*", "Phonetic=vowel")

# get formant data at the midpoint, returns an array
data.mid <- emu.track( segs, "fm", cut=0.5 )
# get formant data for entire tracks, returns trackdata
data.all <- emu.track( segs, "fm" )
summary(data.all)


}
\keyword{misc}
