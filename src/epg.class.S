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
w <- matrix(weights, nrow=N, ncol=5, byrow=TRUE)
divisor <- matrix(c(rep(8, 4), 6), nrow=N, ncol=5, byrow=TRUE)
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



`epgci` <-
function (epgdata, weights = c(1, 17, 289, 4913)) 
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
    if (!inherits(epgdata, "EPG")) 
        p <- palate(epgdata)
    else p <- epgdata
    if (length(dim(p)) == 2) {
        p <- array(p, c(8, 8, 1))
        class(p) <- "EPG"
    }
    N <- dim(p)[3]
    num <- log((weights[1] * epgsum(p, columns = c(1, 8))/14 + 
        weights[2] * epgsum(p, columns = c(2, 7))/16 + weights[3] * 
        epgsum(p, columns = c(3, 6))/16 + weights[4] * epgsum(p, 
        columns = c(4, 5))/16) + 1)
    den <- log(sum(weights) + 1)
    result <- cbind(num/den)
    if (is.trackdata(epgdata)) {
        epgdata$data <- result
        epgdata$trackname <- "centrality"
    }
    else epgdata <- result
    epgdata
}







`epgcog` <-
function (epgdata, weights = seq(7.5, 0.5, by = -1), rows = 1:8, 
    columns = 1:8, row1 = NULL) 
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

    if (!inherits(epgdata, "EPG")) 
        p <- palate(epgdata)
    else p <- epgdata
    if (length(dim(p)) == 2) {
        p <- array(p, c(8, 8, 1))
        class(p) <- "EPG"
    }
    N <- dim(p)[3]
    times <- dimnames(p)[[3]]
    if (!is.null(row1)) 
        p[1, , ] <- p[1, , ] * row1
    rowsum <- epgsum(p, 1, columns = columns)
    w <- matrix(weights, nrow = N, ncol = 8, byrow = TRUE)
    prodsum <- rowsum * w
    prodsum <- rbind(prodsum[, rows])
    sumval <- apply(prodsum, 1, sum)
    psum <- epgsum(p, rows = rows, columns = columns)
    result <- rep(0, length(psum))
    temp <- psum == 0
    result[!temp] <- sumval[!temp]/psum[!temp]
    result <- cbind(result)
    rownames(result) <- times
    if (is.trackdata(epgdata)) {
        epgdata$data <- result
        epgdata$trackname <- "centre of gravity"
    }
    else epgdata <- result
    epgdata
}




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

"epggs" <-
function(epgdata, gscale=100, gridlines=TRUE, gridcol="gray", gridlty=1, axes=TRUE, xlab="", ylab="", ...)
{
# function to plot a 3D greyscale EPG imageb
# p is palate data, returned by palate() or EPG-trackdata
# plots greyscale image of contacts
# such that
# the darker the square, the greater the
# proportion of contacts. Thus a black square
# means that a contact was always on
# for all palatograms in p; a white
# square means that it was always off.

if(!inherits(epgdata, "EPG")) p <- palate(epgdata)
else p <- epgdata
# in case there is only one palate
if(length(dim(p) )==2)
{
p <- array(p, c(8, 8, 1))
class(p) <- "EPG"
}
n = dim(p)[3]
sump  = (apply(p, c(1,2), sum))/n 
image(1:8, 1:8, t(1-sump[8:1,]), col = gray(0:gscale/gscale), axes=FALSE, xlab=xlab, ylab=ylab, ...)
if(axes)
{
axis(side=1)
axis(side=2, at=c(1, 3, 5, 7), labels=as.character(c(8, 6, 4, 2)))
}
if(gridlines)
grid(8, 8, col = gridcol, lty=gridlty)
}

"epgplot" <-
function(epgdata, select=NULL, numbering = "times", gridlines = TRUE, mfrow = NULL, col = 1, mar=c(.8, .1, .8, .1), xlim=NULL)
{
# epgdata: a list as returned by emu.track()
# or else an array of palates. 
# numbering can be T or F or else a numeric or character vector
# which is equal in length to the number of palates)
# xlim: can only be used if epgdata are contiguous!

   oldpar = par(no.readonly=TRUE)
    on.exit(par(oldpar))
    par(mar = mar)
    epggrid <- function() {
        xgrid <- NULL
        for (j in 0:8) {
            vec <- c(j, j, NA)
            xgrid <- c(xgrid, vec)
        }
        ygrid <- rep(c(0, 8, NA), 9)
        ygrid[c(2, 26)] <- 7
        lines(xgrid, ygrid)
        ygrid[25] <- 1
        ygrid[2] <- 8
        lines(ygrid, xgrid)
    }
    if (!inherits(epgdata, "EPG")) 
        epgdata <- palate(epgdata)
    if (!is.null(select)) {
        times <- dimnames(epgdata)[[3]]
        smat <- NULL
        for (j in select) {
            cl <- closest(as.numeric(times), j)[1]
            smat <- c(smat, cl)
        }
        epgdata <- epgdata[, , smat]
    }
    N <- dim(epgdata)
    if (length(N) == 2) {
        N <- 1
        epgdata <- array(epgdata, c(8, 8, 1))
    }
    else N <- N[3]
    times <- dimnames(epgdata)[[3]]
    if (!is.null(xlim)) {
        temp <- as.numeric(times) > xlim[1] & as.numeric(times) < 
            xlim[2]
        epgdata <- epgdata[, , temp]
        times <- times[temp]
        N <- sum(temp)
    }
    if (is.logical(numbering)) {
        if (numbering) 
            main <- as.character(1:N)
        else main <- rep("", N)
    }
    else if (length(numbering) == 1) {
        if (numbering == "times") 
            main <- times
    }
    else main <- as.character(numbering)
    x <- rep(0:7, rep(8, 8))
    xpoly <- cbind(x, x + 1, x + 1, x)
    y <- rep(7:0, 8)
    ypoly <- cbind(y, y, y + 1, y + 1)
    if (is.null(mfrow)) {
        foo <- ceiling(sqrt(N))
        bar <- ceiling(N/foo)
        mfrow <- c(foo, bar)
    }
    epgplot.sub <- function(pgram, xpoly, ypoly, col = 1, main = "") {
        which <- c(pgram) == 1
        if (any(which)) {
            xpoly <- xpoly[which, ]
            ypoly <- ypoly[which, ]
            xpoly <- rbind(xpoly)
            ypoly <- rbind(ypoly)
            mat <- NULL
            for (j in 1:sum(which)) {
                mat$x <- c(mat$x, c(xpoly[j, ], NA))
                mat$y <- c(mat$y, c(ypoly[j, ], NA))
            }
            mat$x <- mat$x[-length(mat$x)]
            mat$y <- mat$y[-length(mat$y)]
        }
        plot(0:8, 0:8, type = "n", axes = FALSE, xlab = "", ylab = "", 
            main = main)
        if (any(which)) 
            polygon(mat$x, mat$y, col = col)
    }
    par(mfrow = mfrow)
    if (N > 1) {
        for (j in 1:N) {
            epgplot.sub(epgdata[, , j], xpoly, ypoly, col = col, 
                main = main[j])
            if (gridlines) 
                epggrid()
        }
    }
    else {
        epgplot.sub(epgdata, xpoly, ypoly, col = col, main = main)
        if (gridlines) 
            epggrid()
    }
    par(mar = oldpar$mar)
}

"epgsum" <-
function(epgdata, profile=c(1,3), inactive = FALSE, rows=1:8, columns=1:8, trackname="EPG-sum")
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

