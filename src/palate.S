"palate" <-
function(epgdata)
{
# epgdata: either a vector of length 8 or
# a matrix of ncol = 8 with 1 row per segment
# or trackdata. If it's trackdata, then the
# result returned is applied to epgdata$data
if(is.trackdata(epgdata))
epgdata <- epgdata$data
times <- dimnames(epgdata)[[1]]
	if(!is.matrix(epgdata)) epgdata <- rbind(epgdata)
	if(ncol(epgdata) != 8) {
		print("input must have 8 columns or be a vector of length 8")
		stop()
	}
	bingen <- function(n = 8)
	{
# n is the number of columns in the result
		mat <- NULL
		x <- 2^(0:(n - 1))
		vec <- rev(x)
		for(j in length(vec):1) {
			res <- rep(c(rep(0, x[j]), rep(1, x[j])), vec[j])
			mat <- cbind(mat, res)
		}
		mat
	}
	nsegs <- nrow(epgdata)
	epgdata <- c(t(epgdata))
	epgdata <- epgdata + 1
	p <- bingen()
	p <- p[epgdata,  ]
	amat <- array(t(p), c(8, 8, nsegs))
if(nsegs > 1)
	p <- aperm(amat[8:1, 8:1,  ], c(2, 1, 3))
# usual silly annoying hack in case there's only one palatogram
else
{
v <- amat[8:1, 8:1,  ]
v <- array(v, c(8, 8, 1))
p <- aperm(v, c(2, 1, 3))
}
 
charrow <- paste("R", 1:8, sep="")
charcol <- paste("C", 1:8, sep="")
dimnames(p) <- list(charrow, charcol, times)
class(p) <- c("array", "EPG")
p
}

