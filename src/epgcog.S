"epgcog" <-
function(epgdata, weights = seq(7.5, 0.5, by = -1), rows=1:8, columns=1:8, row1 = NULL)
{
# function to calculate the centre of gravity per palate
# p: either a list of epg track
# data returned by track () or a three-dimensionsal array of palates
# weights: apply weights to rows 1..8.
# (defaults to 7.5, 6.5...0.5)
# row1: an optional numeric argument
# to allow a separate weighting of
# the electrodes in row1. For example, if row1=4/3, 
# then all the electrodes in row1 are multiplied by 
# that value, before the COG is calculated.
# Defaults to NULL (no weighting).
# 
# returns: if p is a list, then
# the function returns trackdata of the
# same length as p with COG values.
# Otherwise, if p is an array of palates, 
# one value (the COG) per palate) is returned
#
# gives the same result (0.5 and 1.17) as the 
# posterior COG measure  in Fig. 10.5, 
# Gibbon & Nicolaidis, 1999, p. 239,
# in Hardcastle & Hewlett Eds, 'Coarticulation'. CUP
# r = array(0, c(8, 8, 2))
# r[6,c(1, 8),1] = 1
# r[7,c(1, 2, 7, 8), 1] = 1
# r[8, ,1] = 1
# r[4, c(1, 2, 8), 2] = 1
# r[5, c(1, 2, 7, 8), 2] = 1
# r[6, c(1, 2, 3, 7, 8), 2] = 1
# r[7, , 2] = 1
# r[8, , 2] = 1
# epgcog(r, rows=5:8, columns=3:6)




weights <- weights[rows]
if(!inherits(epgdata, "EPG")) p <- palate(epgdata)
else p <- epgdata
# in case there is only one palate
if(length(dim(p) )==2)
{
p <- array(p, c(8, 8, 1))
class(p) <- "EPG"
}
N <- dim(p)[3]
if(!is.null(row1))
p[1,  ,  ] <- p[1,  ,  ] * row1
rowsum <- epgsum(p, 1, columns=columns)
w <- matrix(weights, nrow=N, ncol=8, byrow=T)
prodsum <- rowsum * w
prodsum <- rbind(prodsum[,rows])
sumval <- apply(prodsum, 1, sum)
psum <- epgsum(p, rows=rows, columns=columns)
# prevent division by zero
result <- rep(0, length(psum))
temp <- psum==0
result[!temp] <- sumval[!temp]/psum[!temp]
result <- cbind(result)
if(is.trackdata(epgdata)) {
epgdata$data <- result
epgdata$trackname <- "centre of gravity"
}
else epgdata <- result
epgdata
}

