\name{palate}
\alias{palate}

\title{ Obtain a three-dimensional palatographic array }
\description{
Function to calculate a three-dimensional
palatographic array from.
}
\usage{ palate(epgdata) }

\arguments{
  \item{epgdata}{ An eight-columned EPG-compressed trackdata object
or an eight columned matrix of EPG-compressed trackdata.}
%  \item{weights}{A vector of five values  that are applied to
%EPG rows 1-5 respectively in epgai(). A vector of
%four values that are applied to columns 1 and 8, 
%to columns 2 and 7, columns 3 and 6, columns 4 and 5
%respectively. Defaults to the values given in Recasens & Pallares (2001). }
}

\details{
  An EPG compressed trackdata object that is output
from the Reading system contains eight columns of data
and each row value when converted to binary
numbers (after adding 1) gives the corresponding
EPG contact patterns. This function does
the conversion to binary values.
}

\value{
An array of three dimensions of 8 rows x 8 columns x n segments
where n is the number of segments in the trackdata object or
matrix. The rows and columns are given dimension names,
the dimension names of the third dimension contains
the times at which the palatograms occur.
}

\examples{
# convert an EPG-compressed trackdata object to palatograms
p <- palate(coutts.epg)

# convert an EPG-compressed matrix to palatograms
p <- palate(dcut(coutts.epg, 0, prop=T))

}

\author{ Jonathan Harrington }


\seealso{ 
\code{\link{epgcog}}
\code{\link{epggs}}
\code{\link{epgai}}
\code{\link{epgplot}}
}

\keyword{datagen}