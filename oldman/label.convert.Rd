\name{label.convert}
\alias{label.convert}
\title{
Translate labels in a label vector
}
\usage{
label.convert(labels, l1, l2)
}
\arguments{
\item{labels}{
A vector of labels. a
}
\item{l1}{
The label to be replaced.
}
\item{l2}{
The substitute label. 
}}
\value{
The \code{labels} vector with all instances of \code{l1} replaced by \code{l2}.
}
\examples{
> label.convert(c("A", "E", "I", "A"), "A", "V")
[1] "V" "E" "I" "V"


}
\keyword{mu}
% Converted by Sd2Rd version 0.3-3.
