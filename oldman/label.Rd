\" -*-nroff-*-
\name{label}
\alias{label}
\title{
Label a segment list.
}
\usage{
label(segs, attribute="Phoneme", 
      extn=options()$labextn, dirc=options()$templatedir)
}
\arguments{
\item{segs}{
A MU+ segment list.
}
\item{attribute}{
A level name from the current MU+ database.
}
\item{extn}{
The extension on the label files in the database.
}
\item{dirc}{
The directory to look for the \code{labed_template} file.
}}
\value{
A vector of labels.
}
\details{
The label function can be used in two ways. Firstly it can retrieve the
labels associated with a segment list at the time the segment list was
made.  This is done by leaving out the \code{attribute} argument.  Secondly
it can retrieve the labels that dominate or are dominated by the
segments in the segment list. For example, to retrieve the labels of
the Words that dominate strong Syllable segments I could use the
following commands. 
> segs <- phon(utts, "Syllable='S'", find="Syllable")
> wordlabs <- label(segs, "Word")
If the \code{attribute} level is lower than the level of the segment list,
the labels of the segments that are dominated by each segment in the
list are concatenated together in the result. 
}
\seealso{
phon
}
\keyword{mu}
% Converted by Sd2Rd version 0.3-3.
