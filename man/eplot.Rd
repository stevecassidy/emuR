\name{eplot}
\alias{eplot}
\title{ Plot ellipses for two-dimensional data}
\description{
  The function plots ellipses for different categories from two-dimensional data.
}
\usage{
eplot (x, labs, chars, formant = FALSE, scaling = "linear", 
    prob = 0.95, nsdev = NULL, dopoints = FALSE, doellipse = TRUE, 
    centroid = FALSE, axes = TRUE, xlim, ylim, col = TRUE, lty = FALSE, 
    lwd = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ A two-columned matrix of data}
  \item{labs}{ An optional vector of labels, parallel to 'data' }
  \item{chars}{ An optional  vector of labels, parallel to 'data'. If this argument is
          specified these labels will be plotted rather than the labels
          in 'labs'. }
  \item{formant}{ If TRUE) then the data is negated and the axes are switched so that, for formant data,
          the plot is made with decreasing F2 on the x-axis and decreasing F1 on the y-axis.  }
  \item{scaling}{ Either "mel" or "bark" for mel or bark scaling of the data}
  \item{prob}{ A single numeric vector greater than zero and less than 1 representing the confidence interval
of the ellipse contours. Defaults to 0.95}
  \item{nsdev}{ Defines the length of the major and minor axes of the
          ellipses in terms of the standard deviation of the data and overrides the prob argument.}
  \item{dopoints}{If TRUE) character labels (from 'labs' or 'chars') are
          plotted for each data point}
  \item{doellipse}{ If TRUE, ellipses are drawn on the plot. If FALSE, no ellipses are
          drawn and, if 'dopoints' is also FALSE, 'centroids' is set to T}
   \item{centroid}{One label for each ellipse is drawn} 
   \item{axes}{ If TRUE axes are drawn on the plot.  }
  \item{xlim}{ A vector of two numeric values  giving the range of the x-axis. }
  \item{ylim}{ A vector of two numeric values  giving the range of the y-axis. }
  \item{col}{ If colour is TRUE) the ellipses and labels will be plotted in
          different colours }
  \item{lty}{If linetype is TRUE) the ellipses will be plotted with
          different linetypes.  This is useful for plots that will be
          printed.}
\item{lwd}{ A code passed to the lwd argument
in plotting functions.
'lwd' can be either
a single element numeric vector, or its length must
be equal to the number of unique types in labs.
For example, if lwd=3 and if labs = c("a", "b", "a", "c"),
then the output is c(3, 3, 3, 3). Alternatively,
if lwd = c(2,3,1), then the output is
c(2, 3, 2, 1) for the same example. The default is
NULL in which case all lines are drawn with lwd=1 }
\item{...}{graphical options \link{par}} 
}
\value{
  NULL
}

\author{ Jonathan Harrington jmh@ipds.uni-kiel.de, Steve Cassidy, Gordon Watson }

\seealso{ 
   \code{\link{dcut}}
}
\examples{

    data(vowlax)
   data <- cbind(vowlax.df$F1,vowlax.df$F2)
   phonetic = vowlax.l
   word = vowlax.word

   eplot(data, phonetic)
    
  
   eplot(data, phonetic, form=TRUE, main="F1 x F2 plane", centroid=TRUE)
   eplot(data, phonetic, form=TRUE, main="F1 x F2 plane", dopoints=TRUE)
   eplot(data, phonetic, form=TRUE, main="F1 x F2 plane in Bark", dopoints=TRUE, scaling="bark")
   eplot(data, phonetic, form=TRUE, main="F1 x F2 plane in Bark b/w with linetype", col=FALSE, lty=TRUE, dopoints=TRUE, scaling="bark") 
   eplot(data, phonetic, form=TRUE, main="F1 x F2 plane", doellipse=FALSE, dopoints=TRUE)
   eplot(data, phonetic, form=TRUE, dopoints=TRUE, prob=0.5, main="F1 x F2 plane, 50\% confidence intervals")
   eplot(data, phonetic, form=TRUE, dopoints=TRUE, nsdev=2, main="F1 x F2 plane, 2 standard deviations")
   

   temp <- phonetic \%in\% c("a", "O")
   eplot(data[temp,], phonetic[temp], form=TRUE,  main="F1 x F2 [A] and [O] only", centroid=TRUE)
   

   temp <- phonetic=="O"
   eplot(data[temp,], phonetic[temp], word[temp], form=TRUE, dopoints=TRUE, main="[O] only showing word labels")  
      
   


}

%\keyword{emu}
\keyword{dplot}