"mu.colour" <-
function (labs, col, linetype, defaultcolour = 1) 
{
    len <- length(labs)
    ulen <- length(unique(labs))
    if (length(col) == 2 * ulen) {
        colours <- 1:len
        n <- 1
        while (n <= ulen) {
            lab <- col[2 * n - 1]
            colour <- col[2 * n]
            colours[labs == lab] <- as.numeric(colour)
            n <- n + 1
        }
        col <- colours
    }
    else if (is.numeric(col)) {
if(length(col)==1)
col <- rep(col, len)
else{
        if (length(col) != length(labs)) 
            stop("length of colour vector differs from that of labels")
}
    }
    else if (!is.logical(col)) {
        col <- 1:length(unique(col))
        if (length(col) != length(labs)) 
            stop("length of colour vector differs from that of labels")
    }
    else {
        if (col) {
            colours <- 1:ulen
            col <- labs
            ulabels <- unique(labs)
            for (i in 1:len) {
                col[i] <- colours[ulabels == labs[i]]
            }
        }
        else {
            col <- rep(defaultcolour, len)
        }
    }
    if (!is.logical(linetype)) {
        if (length(linetype) == 1) 
            linetype <- rep(linetype, len)
        else linetype <- 1:length(unique(linetype))
        if (length(linetype) != length(labs)) 
            stop("length of linetype vector differs from that of labels")
    }
    else {
        if (linetype) {
            lty <- 1:ulen
            linetype <- labs
            ulabels <- unique(labs)
            for (i in 1:len) {
                linetype[i] <- lty[ulabels == labs[i]]
            }
        }
        else {
            linetype <- rep(1, len)
        }
    }
    foo <- NULL
    foo$colour <- col
    foo$linetype <- linetype
    p1 <- paste(labs, col, linetype)
    p1.temp <- duplicated(p1)
    foo$legend$lab <- labs[!p1.temp]
    foo$legend$col <- foo$colour[!p1.temp]
    foo$legend$lty <- foo$linetype[!p1.temp]
    return(foo)
}
