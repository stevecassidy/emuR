"epgplot" <-
function(epgdata, select=NULL, numbering = "times", gridlines = T, mfrow = NULL, col = 1, mar=c(.8, .1, .8, .1), xlim=NULL)
{
# epgdata: a list as returned by emu.track()
# or else an array of palates. 
# numbering can be T or F or else a numeric or character vector
# which is equal in length to the number of palates)
# xlim: can only be used if epgdata are contiguous!
oldpar <- par()
# reset plot defaults on exit
on.exit(par(mfrow=oldpar$mfrow))

# set margin size
par(mar=mar)

# function to draw EPG-gridlines
epggrid <- function()
{
xgrid <- NULL
	for(j in 0:8) {
		vec <- c(j, j, NA)
		xgrid <- c(xgrid, vec)
	}
	ygrid <- rep(c(0, 8, NA), 9)	
ygrid[c(2,26)] <- 7
lines(xgrid, ygrid)
ygrid[25] <- 1
ygrid[2] <- 8
lines(ygrid, xgrid)
}


	if(!inherits(epgdata, "EPG")) epgdata <- palate(epgdata)


if(!is.null(select))
{
times <- dimnames(epgdata)[[3]]
smat <- NULL
for(j in select){
cl <- closest(as.numeric(times), j)[1]
smat <- c(smat, cl)
}
epgdata <- epgdata[,,smat]
}





	
	# the number of palates
	N <- dim(epgdata)
	if(length(N) == 2)
{
		N <- 1
epgdata <- array(epgdata, c(8, 8, 1))
}
	else N <- N[3]
times <- dimnames(epgdata)[[3]]
if(!is.null(xlim))
{
temp <- as.numeric(times) > xlim[1] & as.numeric(times) < xlim[2]
epgdata <- epgdata[,,temp]
times <- times[temp]
N <- sum(temp)
}
	if(is.logical(numbering)) {
		if(numbering)
			main <- as.character(1:N)
		else main <- rep("", N)
	}
	else if(length(numbering)==1)
{
if(numbering == "times")
main <- times
}
else
main <- as.character(numbering)
	x <- rep(0:7, rep(8, 8))
	xpoly <- cbind(x, x + 1, x + 1, x)
	y <- rep(7:0, 8)
	ypoly <- cbind(y, y, y + 1, y + 1)	# work out a sensible mfrow default
	if(is.null(mfrow)) {
		foo <- ceiling(sqrt(N))
		bar <- ceiling(N/foo)
		mfrow <- c(foo, bar)
	}

	epgplot.sub <- function(pgram, xpoly, ypoly, col = 1, main = "")
	{
		which <- c(pgram) == 1
		if(any(which)) {
# do nothing if the palate is empty
			xpoly <- xpoly[which,  ]
			ypoly <- ypoly[which,  ]	
	# silly hack to coerce a vector into a matrix
			xpoly <- rbind(xpoly)
			ypoly <- rbind(ypoly)
			mat <- NULL
			for(j in 1:sum(which)) {
				mat$x <- c(mat$x, c(xpoly[j,  ], NA))
				mat$y <- c(mat$y, c(ypoly[j,  ], NA))
			}
# remove last NA
			mat$x <- mat$x[ - length(mat$x)]
			mat$y <- mat$y[ - length(mat$y)]
		}
		plot(0:8, 0:8, type = "n", axes = F, xlab = "", ylab = "", main
			 = main)
		if(any(which))
			polygon(mat$x, mat$y, col = col)
	}
# plot the palatograms with a hack in case there's only one
	par(mfrow = mfrow)
	if(N > 1) {
		for(j in 1:N) {
			epgplot.sub(epgdata[,  , j], xpoly, ypoly, col = col, 
				main = main[j])
			if(gridlines) 
epggrid()
		}
	}
	else {
		epgplot.sub(epgdata, xpoly, ypoly, col = col, main = main)
		if(gridlines) 
epggrid()
	}
par(mar=oldpar$mar)
}

