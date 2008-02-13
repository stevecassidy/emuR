"trackapply" <-
function (trackdata, fun, ..., simplify = FALSE) 
{
    n = 1:nrow(trackdata$index)
    indices = rep(n, trackdata$index[, 2] - trackdata$index[, 
        1] + 1)
    result <- o <- by(I(trackdata$data), indices, fun, ...)
    if (simplify) {
        if (is.null(attributes(summary(o))$dim)) 
            result <- c(unlist(o))
        else {
            result <- NULL
            for (j in 1:length(o)) {
                result <- rbind(result, o[[j]])
            }
        }
    }
    result
}

