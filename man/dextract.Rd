\name{dextract}
\alias{dextract}
\alias{dextract.sub}
\title{Extract a subset of data from a trackdata object}

\description{ A function that cuts up trackdata either at a proportional
 time or proportionally between two times. It is a subsidiary function
 of dplot() }

\usage{
dextract(dataset, start, end)
}

\arguments{
 \item{dataset}{A trackdata object}

 \item{start}{ A single valued numeric vector corresponding to a
 proportional time between zero (the onset of the trackdata) and one
 (the offset of the trackdata).}

 \item{end}{ As start, but optional}
}
\value{
  If both start and end are specified, a trackdata object
is returned, otherwise a vector if the original trackdata
is one-dimensional and the end argument
is not used, or a matrix if the original trackdata
has more than one dimension and the end argument is not used
}

\author{ Jonathan Harrington}

\seealso{\code{dcut}}

\examples{
library(emu)
# find all Phonetic vowels in the database
segs = emu.query("demo", "*", "Phonetic=A|E|I|O|U|V")
# get the formants
form = emu.track(segs, "fm")
# get the formants at the midpoint: f50 is a matrix
# same as dcut(form, .5, prop=T)
f50 = dextract(form, 0.5)
# get the formants between the 25% and 75% time points
# fcut is a trackdata object
# same as dcut(form, .25, .75, prop=T)
fcut = dextract(form, 0.25, 0.75)
# get  F0 at the midpoint. fzero50 is a vector
# same as dcut(fzero, .5, prop=T)
fzero = emu.track(segs, "F0")
fzero50 = dextract(fzero, 0.5)

}
\keyword{datagen}
