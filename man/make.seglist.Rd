\name{make.seglist}
\alias{make.seglist}
\title{ Make an Emu segment list from the various components }
\description{
  This is the appropriate way to make an Emu segment list and ensure
  that it has all of the required components.  
}
\usage{
make.seglist(labels, start, end, utts, query, type, database)
}
\arguments{
  \item{labels}{ A character vector of labels for each segment }
  \item{start}{ A vector of start times }
  \item{end}{ A vector of end times }
  \item{utts}{ A character vector of utterance names  }
  \item{query}{ A query string }
  \item{type}{ \code{segment} or \code{event}  }
  \item{database}{ The database name associated with the segment list }
}
\details{
  An Emu segment list is the result of a query to a speech database (see
  \code{\link{emu.query}}) and has one row per matching segment or event
  from the query. Each row   lists the label, start and end times (in
  milliseconds) and utterance   name for the segment.  This information
  is  used by \code{\link{emu.track}} and other functions to extract
  data corresponding to these segments.

  In order to ensure the proper format for segment lists and to ensure
  against future changes to the format, \code{make.seglist} should be
  used whenever you wish to create a segment list.   Another function,
  \code{\link{modify.seglist}} can be used to change some part of an
  existing segment list. The functions \code{\link{label}},
  \code{\link{mustart}}, \code{\link{muend}} and \code{\link{utt}} can
  be used to access the different columns of the segment list.
}
\value{
  An Emu segment list.
}
\author{ Steve Cassidy }
\seealso{   \code{\link{modify.seglist}}, \code{\link{label}},
  \code{\link{mustart}}, \code{\link{muend}}, \code{\link{utt}} }

\examples{

l <- c("A", "B", "C")
s <- 1:3
e <- 2:4
u <- c("u1", "u1", "u1")
segs <- make.seglist(l, s, e, u, "Fake Query", "segment", "fake")

## summary gives an overview of the data in the segment list
summary(segs)

# The following should be TRUE
label(segs) == l
mustart(segs) == s
muend(segs) == e
utt(segs) == u
emusegs.database(segs) == "fake"
emusegs.type(segs) == "segment"
emusegs.query(segs) == "Fake Query"

# segment durations should all be 1
mudur(segs) == c(1,1,1)

}
\keyword{misc}

