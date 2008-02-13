cbind.trackdata <-
function (...) 
{
mat <- NULL
k <- 1
    for (j in list(...)) {
if(k==1)
{
inds <- mat$index <- j$index
mat$ftime <- j$ftime
}
else
{ 
if( nrow(j$index) != nrow(inds) )
stop("can't column bind trackdata from different segment lists")
lvec = (j$index[,1]==inds[,1]) & (j$index[,2]==inds[,2])
if(any(!lvec))
stop("can't column bind trackdata from different segment lists")
}
k = k+1
}



 for (j in list(...)) {
            mat$data <- cbind(mat$data, j$data)
        }

    if (version$major >= 5) {
        oldClass(mat) <- "trackdata"
    }
    else {
        class(mat) <- "trackdata"
    }
    mat
}

