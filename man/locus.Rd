\name{locus}
\alias{locus}
\title{ Calculates a locus equation}
\description{
  Calculates a locus equation.
}
\usage{locus(target, onset,  labels.vow=NULL, yxline=T, xlim = NULL, ylim = NULL, xlab="", ylab="", plotgraph=T)}

\arguments{
	\item{target}{vector of target freqs}
	\item{onset}{vector of onset freqs}
	\item{labels.vow}{optional vowel labels for plotting}
	\item{xlim, ylim}{optional range for x and y-axes}
	\item{xlab, ylab}{optional label for axes}
	\item{plot}{if T, produces a plot of target x onset with a superimposed regression line through the scatter with superimposed line target = onset}
}

# target: vector of target freqs
# onset: vector of onset freqs
# labels.vow: optional vowel labels for plotting
# xlim, ylim: optional range for x and y-axes
# xlab, ylab: optional label for axes
# plot: if T, produces a plot of  target x  onset
# with a superimposed regression line through
# the scatter with superimposed line target = onset
# returns: statistical results of the fitting
# the regression line and the locus frequency in $locus

\details{
  The first two arguments are the x- and y-axis data, the third optional argument is for superimposing a parallel set of labels.
}

\author{Jonathan Harrington}
\examples{
# F2-onset
f2onset = dcut(isol.fdat[,2], 0, prop=T)
# F2-"target"
f2targ = dcut(isol.fdat[,2], .5, prop=T)
# F2-offset
f2offset= dcut(isol.fdat[,2], 1, prop=T)
xlim = c(500, 2500); ylim = xlim; par(mfrow=c(1,2))

#The first two arguments are the x- and y-axis data (in this case the F2-target and the F2-onset respectively) and there is a third optional argument for superimposing a parallel set of labels. In this example, the vowel labels are superimposed on the scatter and the x- and y-ranges are set to be the same. The F2-target x F2-onset data is plotted in the left panel, the F2-target x F2-offset data in the right panel:

stats.on = locus(f2targ, f2onset, isol.l, xlim=xlim, ylim=ylim)
stats.off = locus(f2targ, f2offset, isol.l, xlim=xlim, ylim=ylim)
}
\keyword{datasets}
