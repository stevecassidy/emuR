\name{emu.requery}
\alias{emu.requery}
\title{ Generate a segment list or label list derived from an existing
  Emu segment list }
\description{
  Given an Emu segment list, \code{emu.requery} can find other segments
  which are related to the segments in the original list. For example,
  the segment that follows or one dominated by the original segment. 
}
\usage{
emu.requery(segs, level, targetlevel=level, justlabels=F, sequence=0, longerok=F)
}
\arguments{
 \item{segs}{ An Emu segment list }
 \item{level}{ The level of the segments in \code{segs} (eg. Phonetic) }
 \item{targetlevel}{ The level of the segments/events to find }
 \item{justlabels}{ If TRUE, a label vector is returned instead of a
   segment list. }
 \item{sequence}{ An integer value (positive or negative) which denotes
   the relative position of the target segments.  }
 \item{longerok}{ If TRUE, the returned segment list or label list may
   be longer than the input in cases where one segment in the input
   dominates more than one segment at the target level. If FALSE, the
   result will always have the same length as the input and multiple
   labels will be concatenated.}
}
\details{
  \code{emu.requery} provides a way of locating segments related to
  those in an existing segment list either by sequence (follows or
  preceeds) or hierarchically (dominates, is dominated by).

  To find sequentially related segments or events, use the \code{sequence}
  argument to define the number of steps to take. For example
  \code{sequence=1} will find the immediately following segment,
  \code{sequence=-1} will find the preceeding segment and
  \code{sequence=3} will find the third following segment.

  To find hierarchically related segments, use the \code{targetlevel}
  argument to denote a different level to that of the original segment
  list. Segments at the target level related to the original segments
  will be retrieved.  For example, if the original segment list consists
  of Phonetic segments, \code{targetlevel="Word"} would find the Word
  level segment dominating each Phonetic segment.

  If both \code{targetlevel} and \code{sequence} are supplied together,
  the \code{targetlevel} argument is applied first and then the
  \code{sequence} argument is applied to find the following or
  preceeding segment at the target level.  For example,
  \code{targetlevel="Word" sequence=1} would find the Word following the
  word that dominates the original segment.

  In some cases, Emu can't locate an appropriate segment (for example,
  because there is no following segment). In this case the corresponding
  segment label in the result will be \code{"no-segment"}.

  If \code{justlabels} is TRUE, only the vector of labels for the
  matching segments is returned, otherwise a complete segment list is
  returned.

  If \code{longerok} is TRUE, the resulting segment list or label vector
  might be longer than the input in cases where more than one segment
  is dominated by (or dominates) the original segment. For example, if
  the original level is Word and the target level is Phoneme the segment
  \code{cat} might dominate /k/, /a/ and /t/ Phonemes.  If
  \code{longerok} is TRUE these will be included as seperate entries in
  the result, if it is FALSE the labels and times will be concatenated
  (eg. k-a-t).

  Note that \code{emu.requery} might fail in some cases since it relies
  on finding segments according to their times.  In some cases this
  might result in not finding a given segment due to rounding errors. 
 
}
\value{
  If \code{longerok} is FALSE: an Emu segment list, otherwise a vector
  of segment labels.
}
\author{ Steve Cassidy }
\seealso{ \code{\link{emu.query}} }

\examples{
library(emu)
segs <- emu.query("demo", "*", "Phonetic=vowel")

## find the word level segments 
wsegs <- emu.requery(segs, level="Phonetic", targetlevel="Text")

## now find the Phonetic segments corresponding to the words
## this will give the complete transcription, eg. (SAID -  s->E->d->H)
psegs <- emu.requery(wsegs, level="Text", targetlevel="Phonetic")

# if instead we want all of the Phonetic segments seperately:
psegs <- emu.requery(wsegs, level="Text", targetlevel="Phonetic", longerok=T)

# now find the Phonetic segment that follows the original vowels:
nsegs <- emu.requery(segs, level="Phonetic", sequence=1)

# and the one that preceeds them, but only get the labels this time
prelabs <- emu.requery(segs, level="Phonetic", sequence=-1, justlabels=T)



}
\keyword{ emu }