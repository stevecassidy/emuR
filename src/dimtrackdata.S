"dim.trackdata" <-
function(x)
{
# function returns the dimension attributes of
# a trackdata object as the number of segments x number of tracks
c(nrow(x$index), ncol(x$data))
}

