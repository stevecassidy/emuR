\name{dplot}
\alias{dplot}
\title{ A function to plot one or more columns of EMU-trackdata as a function of time}
\description{ A general purpose routine for plotting EMU-trackdata on a single plot.
     Tracks can be aligned at an arbitrary position, length normalised
     or averaged. The plots can be colour-coded for different category types.
}
\usage{
dplot(dataset, labs = NULL, offset = 0, ref.time = NULL, average = F, main = "", xlab = "time (ms)", ylab = "", xlim = NULL, ylim = NULL, cex = 0.5, linetype = F, normalise = F, colour = T, legend = "topright", axes = T, n = 20)
}
\arguments{
  \item{dataset}{An EMU-trackdata object }
  \item{labs}{ A label vector with one element for each row in 'dataset' }
  \item{offset}{  A single numeric vector  between 0 and 1.
0 and 1 denote synchronize the trackdata at their temporal onsets
and offsets respectively; 0.5 denotes synchronization at the
temporal midpoint, etc. }
  \item{ref.time}{ A numeric vector. The length of the vector
must be same as the number of observations in dataset, i.e.
length(ref.time) must be equal to nrow(dataset$index). Trackdata
observations are lined up at t = 0 ms, where t is relative
to the times in this vector. }
  \item{average}{ If T, the data for each unique label in 'labs' is averaged }
  \item{main}{ A character vector for a main title for the plot. }
  \item{xlab}{ A character vector for the x-axis label  }
  \item{ylab}{ A character vector for the y-axis label. }
  \item{xlim}{ A vector of two numeric values  specifying the  x-axis range}
  \item{ylim}{ A vector of two numeric values  specifying the  y-axis range }
  \item{cex}{ The 'cex' parameter passed to 'par' to control text size on
          the plot }
  \item{linetype}{ A single element logical vector. Defaults to F. 
If T,  plot each label type in a different linetype}
  \item{normalise}{ If T, the data for each segment is linearly time normalised so
          that all observations have the same length. The number of points
used in the linear time normalisation is control by the argument \item{n} }
  \item{colour}{ A single element logical vector. Defaults to T 
to plot each label type in a different colour }
  \item{legend}{ Either a character vector to plot
the legend. Possible values are: "bottomright"', '"bottom"', '"bottomleft"',
     '"left"', '"topleft"', '"top"', '"topright"', '"right"' and
     '"center"'. This places the legend on the inside of the plot frame
     at the given location. Partial argument matching is used. Or 
a logical vector: legend = F suppresses legend plotting. legend = T
plots it at the default, legend = "topright"}
  \item{axes}{ A single element logical vector. Defaults to T 
to plot the axes }
  \item{n}{ A single element numeric vector. Only used if normalise=T.
The number of data points used to linearly time normalise each track }
}
%\details{
%  ~~ If necessary, more details than the description above ~~
%}
\value{
 NULL
}
\references{ }
\author{Jonathan Harrington}
%\note{ ~~further notes~~ 
%
% ~Make other sections like Warning with \section{Warning }{....} ~
%}
\seealso{ 
   \code{\link{dcut}}
   \code{\link{emu.track}}
}
\examples{

   # Plot of column 1 (which happens to be the 1st formant) of an EMU-trackdata object
   dplot(dip.fdat[,1])
	\dontshow{readline(prompt = "Press return to advance.")}

   # As above but only observations 1 to 5
   dplot(dip.fdat[1:5,1])
	\dontshow{readline(prompt = "Press return to advance.")}

   #  column 2 (which happens to be of the second formant) and colour-coded
   # for each label-type
   dplot(dip.fdat[,2], dip.l)
	\dontshow{readline(prompt = "Press return to advance.")}

   # put the legend bottom left
   dplot(dip.fdat[,2], dip.l, legend="bottomleft")
	\dontshow{readline(prompt = "Press return to advance.")}

   # as above with no legend and averaged per category
   dplot(dip.fdat[,2], dip.l, legend=F, average=T)
	\dontshow{readline(prompt = "Press return to advance.")}

   # both formants averaged
   dplot(dip.fdat[,1:2], dip.l, average=T)
	\dontshow{readline(prompt = "Press return to advance.")}

   # F2 only with linear-time normalisation
   dplot(dip.fdat[,2], dip.l, norm=T)
	\dontshow{readline(prompt = "Press return to advance.")}

   # linear time-normalisation, both formants and averaged
   dplot(dip.fdat[,1:2], dip.l, norm=T, average=T)
	\dontshow{readline(prompt = "Press return to advance.")}

   # synchronise at the temporal midpoint before averaging, F2 only
   dplot(dip.fdat[,2], dip.l, offset=0.5, average=T)
	\dontshow{readline(prompt = "Press return to advance.")}

   # synchronise 60 ms before the diphthong offset
   dplot(dip.fdat[,2], dip.l, ref.time=dip.fdat$ftime[,2]-60)
	\dontshow{readline(prompt = "Press return to advance.")}

   # as above averaged, no colour with linetype and averaged
   dplot(dip.fdat[,2], dip.l, ref.time=dip.fdat$ftime[,2]-60,
   average=T, colour=F, linetype=T)



}
\keyword{emu}

