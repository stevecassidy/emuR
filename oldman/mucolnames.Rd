\" -*-nroff-*-
\name{mucolnames}
\alias{mucolnames}
\title{
Find database column names. (Unix only)
}
\usage{
mucolnames(template="labed_template", dir=options()$templatedir)
}
\arguments{
\item{template}{
The name of the template file to look at. 
}
\item{dir}{
The directory in which to look for the template file.
}}
\value{
A vector of the column names defined by the template file. 
}
\details{
This function examines the template file to find the columns defined in
the database described by it.  These column names define the allowed
queries in the \code{phon} and \code{label} functions.
}
\seealso{
phon, label
}
\keyword{mu}
% Converted by Sd2Rd version 0.3-3.
