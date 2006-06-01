\name{plot.trackdata}
\alias{plot.trackdata}

\title{ Produces time-series plots from trackdata }

\description{
The function produces a plot as a function of
time for a single segment or multiple plots
as a function of time for several segments.
}

\usage{
plot.trackdata (trackdata, timestart = NULL, xlim = NULL, ylim = NULL, labels = NULL, contig = T, ...) 
}

\arguments{
  \item{trackdata}{A trackdata object.}
  \item{timestart}{ A single valued numeric vector for setting
the time at which the trackdata should start. Defaults
to NULL which means that the start time is taken from start(trackdata), i.e. the time at which the trackdata object starts.}
  \item{xlim}{A numeric vector of two values for
specifying the time interval over which the trackdata is to be plotted. Defaults to NULL which means that the trackdata object is
plotted between between the start time of the first segment and the end time of the last segment.}
  \item{ylim}{Specify a yaxis range.}
  \item{labels}{ A character vector the same length as
the number of segments in the trackdata object. Each label
is plotted at side = 3 on the plotted at the temporal
midpoint of each segment in the trackdata object.
Defaults to NULL (plot no labels). Labels will only
be plotted if xlim=NULL.}
  \item{contig}{ A single valued logical vector T or F. If T,
then all the segments of the trackdata object are
assumed to be temporally contiguous, i.e. the
boundaries of the segments are abutting in time
and the start time of segment[j,] is 
the end time of segment[j-1,]. In this case,
all the segments of the trackdata object are
plotted on the same plot as a function of time.
An example of a contiguous trackdata object is coutts.sam.
contig = F is when a trackdata object is non-contiguous
e.g. all "i:" vowels in a database. An example
of a non-contiguous trackdata object is vowlax.fdat.
If contig=F then each segment of the trackdata object
is plotted separately.}
  \item{...}{ the same graphical parameters
can be supplied to this function as for plot e.g type="l", lty=2 etc.}
}

\details{
The function plots a single segment of trackdata as
a function of time. If the segment contains
multiple tracks, then these will be overlaid. If
there are several temporally non-contiguous
segments in the trackdata object,
each segment is plotted in a different panel
by specifying contig=F. This function 
is not suitable for overlaying
trackdata from more than one segments  on the same plot
as a function of time: for this use dplot().}

\author{ Jonathan Harrington}

\seealso{ \code{\link{plot}}, \code{\link{dplot}}}
\examples{

# a single segment of trackdata (F1) plotted as a function of time.
plot(vowlax.fdat[1,1])

# as above, but limits are set for the time axis.
plot(vowlax.fdat[1,1], xlim=c(880, 920))

# the same as above but the start-time of the x-axis is also set to 0 ms
plot(vowlax.fdat[1,1], xlim=c(880, 920), timestart=0)

# a single segment of trackdata (sampled speech data) plotted as a function of time.
plot(coutts.sam[1,], type="l")

# as above, but limits are set for the time axis.
plot(coutts.sam[1,], type="l", xlim=c(16200, 16300))

# the same as above but the start-time of the x-axis is also set to 0 ms
plot(coutts.sam, type="l", timestart=0, xlim=c(16200, 16300))

# get some labels
labs = label(coutts)

# superimpose these on the plot
plot(coutts.sam, type="l", timestart=0, labels=labs)


# plot each segment in its own window.
par(mfrow=c(2,3))
plot(coutts.sam, type="l", labels=labs, contig=F)


# plot each segment in its own window and set timestart to 0 ms, set the colour to red
par(mfrow=c(2,3))
plot(coutts.sam, type="l", labels=labs, timestart=0, contig=F, col="red")

# F1 and F2 of six vowels with labels

par(mfrow=c(2,3))
plot(vowlax.fdat[1:6,1:2], contig=F, labels=vowlax.l[1:6], ylab="F1 and F2", 
xlab="Time (ms)", type="b", ylim=c(300, 2400))

}

\keyword{ emu }
