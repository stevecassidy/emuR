\name{eplot}
\alias{eplot}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~}
\description{
 ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
eplot(data, labs, chars, formant=F, scaling="linear", nsdev=2.447747, dopoints=F, classify=F, doellipse=T, centroid=F, main="", xlab="", ylab="", axes=T, xlim, ylim, colour=T, linetype=F, ...)
}
%- maybe also `usage' for other objects documented here.
\arguments{
 \item{data}{ ~~Describe \code{data} here~~ }
 \item{labs}{ ~~Describe \code{labs} here~~ }
 \item{chars}{ ~~Describe \code{chars} here~~ }
 \item{formant}{ ~~Describe \code{formant} here~~ }
 \item{scaling}{ ~~Describe \code{scaling} here~~ }
 \item{nsdev}{ ~~Describe \code{nsdev} here~~ }
 \item{dopoints}{ ~~Describe \code{dopoints} here~~ }
 \item{classify}{ ~~Describe \code{classify} here~~ }
 \item{doellipse}{ ~~Describe \code{doellipse} here~~ }
 \item{centroid}{ ~~Describe \code{centroid} here~~ }
 \item{main}{ ~~Describe \code{main} here~~ }
 \item{xlab}{ ~~Describe \code{xlab} here~~ }
 \item{ylab}{ ~~Describe \code{ylab} here~~ }
 \item{axes}{ ~~Describe \code{axes} here~~ }
 \item{xlim}{ ~~Describe \code{xlim} here~~ }
 \item{ylim}{ ~~Describe \code{ylim} here~~ }
 \item{colour}{ ~~Describe \code{colour} here~~ }
 \item{linetype}{ ~~Describe \code{linetype} here~~ }
 \item{\dots}{ ~~Describe \code{\dots} here~~ }
}
\details{
 ~~ If necessary, more details than the __description__  above ~~
}
\value{
  ~Describe the value returned
  If it is a LIST, use
  \item{comp1 }{Description of `comp1'}
  \item{comp2 }{Description of `comp2'}
  ...
}
\references{ ~put references to the literature/web site here ~ }
\author{ ~~who you are~~ }
\note{ ~~further notes~~ }

 ~Make other sections like WARNING with \section{WARNING }{....} ~

\seealso{ ~~objects to SEE ALSO as \code{\link{~~fun~~}}, ~~~ }

\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	     or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (data, labs, chars, formant = F, scaling = "linear", 
    nsdev = 2.447747, dopoints = F, classify = F, doellipse = T, 
    centroid = F, main = "", xlab = "", ylab = "", axes = T, 
    xlim, ylim, colour = T, linetype = F, ...) 
{
    if (missing(labs)) 
        labs <- rep(".", nrow(data))
    if (!doellipse & !dopoints) 
        centroid <- T
    if (nrow(data) != length(labs)) 
        stop("Data and labels don't match")
    if (ncol(data) != 2) 
        stop("Eplot needs 2 dimensional data")
    if (!missing(chars)) 
        if (length(labs) != length(chars)) 
            stop("Length of chars must match that of labs")
    if (scaling == "mel") 
        data <- mel(data)
    if (scaling == "bark") 
        data <- bark(data)
    if (formant) 
        data <- cbind(-data[, 2], -data[, 1])
    col.lty <- mu.colour(labs, colour, linetype)
    lty <- col.lty$linetype
    uniqlabels <- unique(labs)
    emat <- nums <- cen <- k <- l <- NULL
    for (j in uniqlabels) {
        temp <- labs == j
        mat <- data[temp, , drop = F]
        if (nrow(mat) > 2) {
            evals <- eigen(var(mat))
            m1 <- mean(mat[, 1])
            m2 <- mean(mat[, 2])
            e <- ellipse(m1, m2, sqrt(evals$values[1]) * nsdev, 
                sqrt(evals$values[2]) * nsdev, aperm(evals$vectors, 
                  c(2, 1)))
        }
        else {
            cat("Too few data points for label ", j, " will plot a point or a line\n")
            m1 <- mean(mat[, 1])
            m2 <- mean(mat[, 2])
            e <- mat
        }
        nums <- c(nums, nrow(e))
        emat <- rbind(emat, e)
        k <- c(k, col.lty$legend$col[match(j, col.lty$legend$lab)])
        l <- c(l, col.lty$legend$lty[match(j, col.lty$legend$lab)])
        if (centroid) 
            cen <- rbind(cen, cbind(m1, m2))
    }
    if (doellipse) {
        if (missing(xlim)) 
            xlim <- range(c(emat[, 1], data[, 1]))
        if (missing(ylim)) 
            ylim <- range(c(emat[, 2], data[, 2]))
    }
    else {
        if (missing(xlim)) 
            xlim <- range(data[, 1])
        if (missing(ylim)) 
            ylim <- range(data[, 2])
    }
    rightlim <- cumsum(nums)
    leftlim <- cumsum(nums) - (nums - 1)
    rowmarker <- cbind(leftlim, rightlim)
    old.par <- par()
    par(...)
    for (j in 1:nrow(rowmarker)) {
        lowerlim <- rowmarker[j, 1]
        upperlim <- rowmarker[j, 2]
        if (doellipse) {
            plot(emat[lowerlim:upperlim, ], type = "l", axes = F, 
                xlim = xlim, ylim = ylim, col = k[j], xlab = "", 
                ylab = "", lty = l[j])
        }
        else {
            plot(emat[lowerlim:upperlim, ], type = "n", axes = F, 
                xlim = xlim, ylim = ylim, col = k[j], xlab = "", 
                ylab = "", lty = l[j])
        }
        if (dopoints) {
            singlelab <- uniqlabels[j]
            temp <- labs == singlelab
            if (!missing(chars)) 
                text(data[temp, 1], data[temp, 2], chars[temp], 
                  col = k[j])
            else text(data[temp, 1], data[temp, 2], labs[temp], 
                col = k[j])
        }
        if (centroid) {
            singlelab <- uniqlabels[j]
            text(cen[j, 1], cen[j, 2], singlelab, col = k[j])
        }
        if (j < nrow(rowmarker)) 
            par(new = T)
    }
    par(col = 1)
    box()
    if (axes) {
        if (formant) {
            if (missing(xlab)) 
                xlab <- "F2"
            if (missing(ylab)) 
                ylab <- "F1"
            xaxp <- par("xaxp")
            yaxp <- par("yaxp")
            xat <- seq(xaxp[1], xaxp[2], length.out = xaxp[3] + 
                1)
            yat <- seq(yaxp[1], yaxp[2], length.out = yaxp[3] + 
                1)
            axis(1, at = xat, label = -xat)
            axis(2, at = yat, label = -yat, srt = 90)
        }
        else {
            axis(1)
            axis(2)
        }
    }
    title(main = main, xlab = xlab, ylab = ylab)
    if (classify) {
        tdat <- train(data, labs)
        mdat <- mahal(data, tdat)
        outlab <- sortmatrix(confusion(labs, mdat), unique(labs))
    }
    if (classify) 
        outlab
  }
}
\keyword{ ~keyword }%-- one or more ...
