\name{muclass}
\alias{muclass}
\title{
Find common elements in vectors 
}
\usage{
muclass(labels, class)
}
\description{}
\arguments{
\item{labels}{
A vector of labels. 
}
\item{class}{
A label or vector of labels. 
}}
\value{
A logical vector which is T for each element in \code{labels} which matches
\code{class} or an element of \code{class}.  
}
\seealso{
match
}
\examples{
muclass(c("a", "b", "c"), c("a", "c"))
[1] T F T
}
\keyword{misc}
% Converted by Sd2Rd version 0.3-3.
