\name{modify.seglist}
\alias{modify.seglist}
\title{ Modify one of the components of an Emu segment list }
\description{
  This function can be used to modify one of the parts of an
  Emu segment list while leaving the other parts unchanged.
}
\usage{
modify.seglist(segs, labels, start, end, utts, query, type, database)
}
\arguments{
 \item{segs}{ A segment list to modify, a modified copy is returned }
 \item{labels}{ A new label vector }
 \item{start}{ A new start time vector }
 \item{end}{ A new end time vector }
 \item{utts}{ A new vector of utterance labels }
 \item{query}{ A new query string to associate with the segment list }
 \item{type}{ A new type string }
 \item{database}{ A new database name }
}
\details{
  An Emu segment list has a number of components and is stored as an R
  object of class \code{emusegs}.  This function can be used to modify a
  segment list while retaining all of the proper structures.

  Any new vectors passed to the function must have the same length as
  the segment list itself for this call to succeed.

  All arguments are optional and default to not modifying the segment
  list if not supplied.

  The original segment list is not modified, instead, a modified copy is
  returned.
}
\value{
  An Emu segment list.
}
\author{ Steve Cassidy }
\seealso{ \code{\link{emu.query}} }

\examples{
library(emu)
segs <- emu.query("demo","*","Phonetic=vowel")
# extend the start times by 10ms
newsegs <- modify.seglist( segs, start=mustart(segs)+10 )

# change the associated database name
# this will affect where emu.track looks to find data
newsegs <-  modify.seglist( segs, database="notdemo" )

}

\keyword{misc}
