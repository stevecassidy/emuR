\name{eplot}
\alias{eplot}
\title{ Plot ellipses for two-dimensional data}
\description{
  The function plots ellipses for different categories from two-dimensional data.
}
\usage{
eplot(data, labs, chars, formant = F, scaling = "linear", prob = 0.95, nsdev = NULL, dopoints = F, doellipse = T, centroid = F, main = "", xlab = "", ylab = "", axes = T, xlim, ylim, colour = T, linetype = F, boundary = F, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{ A two-columned matrix of data}
  \item{labs}{ An optional vector of labels, parallel to 'data' }
  \item{chars}{ An optional  vector of labels, parallel to 'data'. If this argument is
          specified these labels will be plotted rather than the labels
          in 'labs'. }
  \item{formant}{ If TRUE, then the data is negated and the axes are switched so that, for formant data,
          the plot is made with decreasing F2 on the x-axis and decreasing F1 on the y-axis.  }
  \item{scaling}{ Either "mel" or "bark" for mel or bark scaling of the data}
  \item{prob}{ A single numeric vector greater than zero and less than 1 representing the confidence interval
of the ellipse contours. Defaults to 0.95}
  \item{nsdev}{ Defines the length of the major and minor axes of the
          ellipses in terms of the standard deviation of the data and overrides the prob argument.}
  \item{dopoints}{If TRUE, character labels (from 'labs' or 'chars') are
          plotted for each data point}
  \item{doellipse}{ If T, ellipses are drawn on the plot. If F, no ellipses are
          drawn and, if 'dopoints' is also F, 'centroids' is set to T}
  \item{centroid}{ If T, a single character label is plotted at the center of
          each ellipse. }
  \item{main}{ A main title for the plot}
  \item{xlab}{ Title for the x axis. Defaults to F2 if formant=T}
  \item{ylab}{ Title for the y axis. Defaults to F1 if formant=T}
  \item{axes}{ If TRUE axes are drawn on the plot.  }
  \item{xlim}{ A vector of two numeric values  giving the range of the x-axis. }
  \item{ylim}{ A vector of two numeric values  giving the range of the y-axis. }
  \item{colour}{ If colour is TRUE, the ellipses and labels will be plotted in
          different colours }
  \item{linetype}{If linetype is TRUE, the ellipses will be plotted with
          different linetypes.  This is useful for plots that will be
          printed.}
  \item{boundary}{ If T, the plotting area is divided into nboundary^2 equally spaced points
(default = 50^2 = 2500) and all these points are classified as one of the 
label types in labs. The colour-coded classified label is superimposed on the plot.  }
  \item{\nboundary}{ used if boundary=T. The density of the points that are classified.
Defaults to 50 (resulting in 50^2 classified points) }
\item{\nboundary}{ used if boundary=T. A character vector either "bayes" or "mahal"
defining the metric for classifying the points. "bayes" is for Bayesian classification,
"mahal" is for classification according to Mahalanobis distances. ) }
}

\value{
  NULL
}

\author{ Jonathan Harrington jmh@ipds.uni-kiel.de, Steve Cassidy, Gordon Watson }

\seealso{ 
   \code{\link{dcut}}
}
\examples{

   attach(vowlax.df)
   data <- cbind(F1, F2)
   eplot(data, phonetic)
    \dontshow{readline(prompt = "Press return to advance.")}
  
   eplot(data, phonetic, form=T, main="F1 x F2 plane", centroid=T)
   \dontshow{readline(prompt = "Press return to advance.")}

   eplot(data, phonetic, form=T, main="F1 x F2 plane", dopoints=T)
   \dontshow{readline(prompt = "Press return to advance.")}

   eplot(data, phonetic, form=T, main="F1 x F2 plane in Bark", dopoints=T, scaling="bark")
   \dontshow{readline(prompt = "Press return to advance.")}

   eplot(data, phonetic, form=T, main="F1 x F2 plane in Bark b/w with linetype", colour=F, linetype=T, dopoints=T, scaling="bark")
   \dontshow{readline(prompt = "Press return to advance.")}

   eplot(data, phonetic, form=T, main="F1 x F2 plane", doellipse=F, dopoints=T)
   \dontshow{readline(prompt = "Press return to advance.")}

   eplot(data, phonetic, form=T, dopoints=T, prob=0.5, main="F1 x F2 plane, 50\% confidence intervals")
   \dontshow{readline(prompt = "Press return to advance.")}

   eplot(data, phonetic, form=T, dopoints=T, nsdev=2, main="F1 x F2 plane, 2 standard deviations")
   \dontshow{readline(prompt = "Press return to advance.")}

   temp <- phonetic \%in\% c("a", "O")
   eplot(data[temp,], phonetic[temp], form=T,  main="F1 x F2 [A] and [O] only", centroid=T)
   \dontshow{readline(prompt = "Press return to advance.")}

   temp <- phonetic=="O"
   eplot(data[temp,], phonetic[temp], word[temp], form=T, dopoints=T, main="[O] only showing word labels")
   \dontshow{readline(prompt = "Press return to advance.")}

   eplot(data, phonetic, form=T,  dopoints=T, boundary=T, main="F1 x F2 plane with Bayesian classification")
   \dontshow{readline(prompt = "Press return to advance.")}

   eplot(data, phonetic, form=T,  dopoints=T, boundary=T, nboundary=100, main="Bayesian classification with more points classified")
   \dontshow{readline(prompt = "Press return to advance.")}
   
   detach(vowlax.df)

}

\keyword{emu}