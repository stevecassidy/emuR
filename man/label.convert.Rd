\name{label.convert}
\alias{label.convert}
\title{
Translate labels in a label vector
}
\usage{
  label.convert(source, match, replace)
}
\arguments{
  \item{source}{
    An Emu segment list or a vector of character labels
  }
  \item{match}{
    A label or vector of labels to match in source 
  }
  \item{replace}{
    A label or vector of labels of the same length as \code{match} or
    a single character string.
  }
}
\value{
  A new Emu segment list or vector of labels (ie. the same kind of
  object as was passed in as \code{source}) with occurences of the
  labels in \code{match} replaced by the corresponding label in
  \code{replace}.  In the special case that \code{replace} is a single
  character string, any label in \code{match} is replaced by
  \code{replace}. 

  Note that you can achieve some more complicated mappings with the R
  builtin function \code{\link{sub}}.
}
\seealso{\code{\link{emu.query}}, \code{\link{sub}}}
\examples{

labs <- c("A", "E", "p", "I", "k", "A")
## lowercase some labels
label.convert(labs, c("A", "E", "I"), c("a", "e", "i"))
## special case, map all these to vv
label.convert(labs, c("A", "E", "I"), "vv")

## the same operations can be performed on segment lists
library(emu)
segs <- emu.query("demo", "*", "Phonetic=vowel")
newsegs <- label.convert(segs, c("A", "E", "I"), c("a", "e", "i"))
}

## Example of using sub, replace any label consisting of
## any character followed by a colon with "long"
sub(".:", "long", label(segs) )

\keyword{misc}

