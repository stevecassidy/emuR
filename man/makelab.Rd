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

\author{Jonathan Harrington}

\examples{
   #first two segments (for the whole example) of segmentlist vowlax 
   vowlax[1:2,]

   #format track of vowlax
   vowlax.fdat[1:2,]

   #Formant values of the midpoint of the segment
   vowlax.fdat.5 = dcut(vowlax.fdat,0.5,prop=T)

   #the time marks of the midpoint of the segment
   times = vowlax.fdat.5[1:2,1]
   times

   #utterance names to the segments
   utts = utt(vowlax[1:2,])
   utts 

   #the path to save the label files to "." is the RHOME Directory
   path = "."

   #write the label files to path
   makelab(times, utts, path, labels="T")

   #the first two segments are from the same utterance,
   #thus one label file was created in the R_HOME directory
}
\keyword{emu}
