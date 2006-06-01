"epgsum" <-
function(epgdata, profile=c(1,3), inactive = F, rows=1:8, columns=1:8, trackname="EPG-sum")
{
# function  that sums by row or by column
# either the active or inactive electrodes of EPG-data.
# returns trackdata of the summed result.
# epgdata: epg data as returned by emu.track()
# allcontacts: if T, then all the contacts per palate are summed
# column: if T, then the summation is applied to
# columns, rather than to rows
# inactive: if T, then the summation is applied
# to the inactive (zero) electrodes, rather than to
# the active ones.
# 
k <- profile[1]
if(!inherits(epgdata, "EPG")) p<- palate(epgdata)
else p <- epgdata
# in case there is only one palate
if(length(dim(p) )==2)
p <- array(p, c(8, 8, 1))
if(length(rows) > 1 & length(columns) > 1)
p <- (p[rows, columns, ])
else 
p <- array(p[rows,columns,], c(length(rows), length(columns), dim(p)[3]))


# in case there is only one palate
if(length(dim(p) )==2)
p <- array(p, c(length(rows), length(columns), 1))
summation <- apply(p, c(k, 3), sum)
summation <- t(summation)
if(inactive) {
mat <- matrix(ncol(p), nrow = nrow(summation), ncol = ncol(summation)
)
summation <- mat - summation
}
if(length(profile)==2 & profile[2]==3)
summation <- apply(summation, 1, sum)
if(is.trackdata(epgdata))
result <- as.trackdata(summation, epgdata$index, epgdata$ftime, trackname)
else result <- summation
result
}

