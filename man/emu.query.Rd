\name{emu.query}
\alias{emu.query}
\title{Query an Emu Database}
\description{
Perform a query on an Emu speech database selecting segments for
subsequent analysis.  The return value is a segment list containing
the labels, start and end times and utterance name of each token
matching the query.  
}
\usage{
emu.query(template, pattern=NULL, query="")
}
\arguments{
 \item{template}{ The name of the Emu database to query (in quotes)  }
 \item{pattern}{ A pattern matching utterances to be searched from the
   database  }
 \item{query}{ a valid Emu query }
}
\details{
The Emu query language is described in the Emu
documentation. \code{template} must refer to a valid database template
on your system, ie. the template file name without the \code{.tpl}
extension. 
}
\value{
An object of type \code{emusegs} with one row per token matched by the
query and columns for the token label, start time, end time and
utterance name.  This can be passed to \code{\link{emu.track}} to extract
speech data corresponding to each token.
}
\references{ See the Emu documentation at:
  \url{http://www.shlrc.mq.edu.au/emu} }
\author{ Steve Cassidy <Steve.Cassidy@mq.edu.au> }
\note{ This function calls an external which are scripts via tcltk part of the Emu
  speech database system and so requires this system to be installed on
  your computer. See the Emu web site for details.  }

\seealso{ \code{\link{emu.track}}}

\examples{

## assumes a database called demo is available on your system and that
## the Emu system is installed. 
data(vowlax)
# find all Phonetic vowels in the database
\dontrun{segs <- emu.query("demo", "*", "Phonetic=vowel")}
\dontshow{segs = vowlax}
# display summary information on the segments found
summary(segs)
# get formant data at the midpoint and plot it
\dontrun{data <- emu.track( segs, "fm", cut=0.5 )}
\dontshow{data = vowlax.fdat.5}
eplot( data[,1:2], label( segs ), dopoints=TRUE )


}
\keyword{misc}

