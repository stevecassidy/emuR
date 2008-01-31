\name{read.emusegs}
\alias{read.emusegs}
\title{ Create an Emu segment list from a file }
\description{
  Create an Emu segment list from a file saved by the Emu query tools. 
}
\usage{
   read.emusegs(file)
}
\arguments{
 \item{file}{ The name of the file to read  }
}
\details{
  Reads segment lists created by programs external to R/Splus and stored
  in text files on disk.  
}
\value{
  An Emu segment list. 
}
\author{ Steve Cassidy }
\seealso{ \code{\link{emu.query}} }

\examples{
## create a segment list file and write it out
seglist.txt <- "database:demo\
query:Phonetic=vowel\
type:segment\
#\
@:	3059.65	3343.65	msdjc001\
e:	5958.55	6244.55	msdjc002\
@u	8984.75	9288.75	msdjc003\
A	11880.8	12184.8	msdjc004\
E	17188.3	17366.4	msdjc005\
ei	20315.2	20655.2	msdjc006"

cat(seglist.txt, file="seglist.txt")

# now read it back as a segment list
segs <- read.emusegs("seglist.txt")
segs
## and clean up
unlink("seglist.txt")

}

\keyword{IO}

