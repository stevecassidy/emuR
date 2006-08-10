
"dct" <-
function (data, m=NULL, fit=F) 
{
# written by Catherine Watson, modified by Jonathan Harrington
    if (is.matrix(data)) 
        nz <- dimnames(data)[[1]]
else nz <- names(data)
    ldat <- length(data)
if(!is.null(m))
{
if ((m < 1) | (m > ldat-1))
stop("m must be between 1 and length(data)-1")
}
    transdat <- vector(length = ldat)
    
        transdat[1] <- (2/(ldat * sqrt(2))) * sum(data)
        for (n in 1:(ldat - 1)) {
            j <- 0:(ldat - 1)
            transdat[n + 1] <- (2/ldat) * sum(data * cos((pi * 
                n * (2 * j + 1))/(2 * ldat)))
        }
names(transdat) <- nz
if(!fit)
{
if(is.null(m))
return(transdat)
else
return(transdat[1:(m+1)])
}
    else {
data <- transdat
transdat <- vector(length = ldat)
if(is.null(m))
        m <- 1:(ldat - 1)
else
m <- 1:m
        for (n in 0:(ldat - 1)) {
            transdat[n + 1] <- (1/sqrt(2)) * data[1] * cos((pi * 
                0 * (2 * n + 1))/(2 * ldat)) + sum(data[m + 1] * 
                cos((pi * m * (2 * n + 1))/(2 * ldat)))
        }
    }
names(transdat) <- nz
    transdat
}

