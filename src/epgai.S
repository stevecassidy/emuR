"epgai" <-
function(epgdata, weights = c(1, 9, 81, 729, 4921))
{
# function to calculate the anteriority index per palate
# as in Recasens & Pallares, 2001, 29, Jphon, p. 283, 
# epgdata: either a trackdata object or an array of type EPG
# or an 8 columned matrix or 8-element vector that's
# the output of dcut() applied to an EPG-trackdata object.

# weights: apply weights to rows 5, 4, 3, 2, 1.

# 
# returns: if p is a trackdata object, then
# the function returns trackdata of the
# same length as p with ant.index values.
# Otherwise, if p is an array of palates, 
# one value (the ant.index) per palate) is returned
#
if(!inherits(epgdata, "EPG")) p <- palate(epgdata)
else p <- epgdata
# in case there is only one palate
if(length(dim(p) )==2)
{
p <- array(p, c(8, 8, 1))
class(p) <- "EPG"
}
N <- dim(p)[3]
o <- epgsum(p, 1, rows=5:1)
w <- matrix(weights, nrow=N, ncol=5, byrow=T)
divisor <- matrix(c(rep(8, 4), 6), nrow=N, ncol=5, byrow=T)
num <- log(apply(w * o/divisor, 1, sum) + 1)
den <- log(sum(weights) + 1)
result <- cbind(num/den)
	if(is.trackdata(epgdata)) {
		epgdata$data <- result
		epgdata$trackname <- "anteriority"
	}
	else epgdata <- result
	epgdata
}

