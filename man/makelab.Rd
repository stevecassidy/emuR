\name{makelab}
\alias{makelab}

\title{ Export labels}
\description{
Writes out label files in ESPS format so that they can be loaded into Emulabel.
}
\usage{makelab(vectimes, uttname, dir, extn="xlab", labels=NULL)
}

\arguments{
	\item{vectimes}{a vector of times}
	\item{uttname}{a character vector of the same length as vectimes giving the utterance name associated with each element of vectimes}
	\item{dir}{a character specifying the directory}
	\item{extn}{a character specifying the extension of the resulting files. Defaults to xlab}
	\item{labels}{if missing, each label written out as the label "x". Otherwise it can be a single element character vector such as "T" (each label then has the label "T") or else a vector of the same length as vectimes}
}

# labels:  if missing, each label written out

\details{
 Writes out label files so that they can be loaded into Emulabel.  You have to supply the name of the directory where you want the label files to be stored as the third argument to makelab().

\examples{
# s.vk <- emu.query("epg-demo", "*", "[Phoneme!=x -> Phoneme=k]")
# l.vk <- label(s.vk)
# e.vk <- emu.track(s.vk, "epg")
# e.dp <- dp(e.vk)
# maxzeit <- dmax(e.dp)
# makelab(maxzeit[,1], utt(s.vk), "c:/d/test", "T")
}
\keyword{datasets}
