"epgdi" <-
function(epgdata)
{
# function to calculate the Qp, or
# dorsopalatal  index per palate
# as in Recasens & Pallares, 2001, 29, Jphon, p. 283, 
# p: either a list of epg track
# data returned by track () or a three-dimensionsal array of palates

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
result <- cbind(epgsum(p, rows=6:8)/24)
if(is.trackdata(epgdata)) {
		epgdata$data <- result
		epgdata$trackname <- "dorsopalatal"
	}
	else epgdata <- result
	epgdata
}
