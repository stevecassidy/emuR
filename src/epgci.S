"epgci" <-
function(epgdata, weights = c(1, 11, 120, 1320))
{
# function to calculate the centrality index per palate
# as in the CCa formula in Recasens & Pallares, 2001, 29, Jphon, p. 283, 
# p: either a list of epg track
# data returned by track () or a three-dimensionsal array of palates
# weights: apply weights to columns 1 and 8;
# columns 2 and 7, columns 3 and 6, columns 4 and 5.

# 
# returns: if p is a list, then
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
num <- log((weights[1] * epgsum(p, columns=c(1,8))/8 +
weights[2] * epgsum(p, columns=c(2,7))/8 +
weights[3] * epgsum(p, columns=c(3,6))/8 +
weights[4] * epgsum(p, columns=c(4,5))/8) + 1)
den <- log(sum(weights) + 1)
result <- cbind(num/den)

if(is.trackdata(epgdata)) {
epgdata$data <- result
epgdata$trackname <- "anteriority"
}
else epgdata <- result
epgdata
}

