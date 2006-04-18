"locus" <-
function (target, onset,  labels.vow=NULL, yxline=T, xlim = NULL, ylim = NULL, xlab="", ylab="", plotgraph=T) 
{
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
regr <- lm(onset ~ target)
mat <- rbind(cbind(1, -1), cbind(1, -regr$coef[2]))
vec <- c(0, regr$coef[1])
regr$locus <- ((solve(mat) %*% vec)[1])
if(plotgraph)
{
if(is.null(xlim))
xlim <- range(target)
if(is.null(ylim))
ylim <- range(onset)
if(is.null(labels.vow))
labels.vow <- rep("x", length(target))
plot(target, onset, xlim=xlim, ylim=ylim, xlab=xlab, ylab=ylab, type="n")
text(target, onset, labels.vow)
abline(regr)
if(yxline)
abline(0, 1, lty=2)
}

regr
}

