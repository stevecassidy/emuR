"-.trackdata" <-
function(x, y)
{
# function for doing arithmetic on trackdata objects
# see +.trackdata for examples

if(class(x)=="trackdata")
{
ftime <- x$ftime
inds <- x$index
nm <- x$name
}
else
{
ftime <- y$ftime
inds <- y$index
nm <- y$name
}

if(class(x)=="trackdata" & class(y)=="trackdata")
o <- x$data - y$data
else if (class(x)=="trackdata" & class(y)!="trackdata")
o <- x$data - y
else if (class(x)!="trackdata" & class(y)=="trackdata")
o <- x - y$data
as.trackdata(o, inds, ftime, nm)
}

"+.trackdata" <-
function(x, y)
{
# function for doing arithmetic on trackdata objects
# examples
# z = vowlax.fdat[,1]+vowlax.fdat[,2]
# same
# sum(z$data - (vowlax.fdat[,1]$data +vowlax.fdat[,2]$data))


# z = vowlax.fdat[,1]+vowlax.fdat[,2]+vowlax.fdat[,3]
# same
#sum(z$data - (vowlax.fdat[,1]$data +vowlax.fdat[,2]$data+vowlax.fdat[,3]$data))


# z = vowlax.fdat[,1]+20
# same
# sum(z$data -  (vowlax.fdat[,1]$data + 20))


# z = 20 + vowlax.fdat[,1] + 30 + vowlax.fdat[,2]
# same
# sum(z$data -  (20+vowlax.fdat[,1]$data + 30+vowlax.fdat[,2]$data))

if(class(x)=="trackdata")
{
ftime <- x$ftime
inds <- x$index
nm <- x$name
}
else
{
ftime <- y$ftime
inds <- y$index
nm <- y$name
}


if(class(x)=="trackdata" & class(y)=="trackdata")
o <- x$data + y$data
else if (class(x)=="trackdata" & class(y)!="trackdata")
o <- x$data + y
else if (class(x)!="trackdata" & class(y)=="trackdata")
o <- x + y$data
as.trackdata(o, inds, ftime, nm)
}

"/.trackdata" <-
function(x, y)
{

if(class(x)=="trackdata")
{
ftime <- x$ftime
inds <- x$index
nm <- x$name
}
else
{
ftime <- y$ftime
inds <- y$index
nm <- y$name
}

# function for doing arithmetic on trackdata objects
# see +.trackdata for examples
if(class(x)=="trackdata" & class(y)=="trackdata")
o <- x$data / y$data
else if (class(x)=="trackdata" & class(y)!="trackdata")
o <- x$data / y
else if (class(x)!="trackdata" & class(y)=="trackdata")
o <- x / y$data
as.trackdata(o, inds, ftime, nm)
}


"*.trackdata" <-
function(x, y)
{

if(class(x)=="trackdata")
{
ftime <- x$ftime
inds <- x$index
nm <- x$name
}
else
{
ftime <- y$ftime
inds <- y$index
nm <- y$name
}

# function for doing arithmetic on trackdata objects
# see +.trackdata for examples
if(class(x)=="trackdata" & class(y)=="trackdata")
o <- x$data * y$data
else if (class(x)=="trackdata" & class(y)!="trackdata")
o <- x$data * y
else if (class(x)!="trackdata" & class(y)=="trackdata")
o <- x * y$data
as.trackdata(o, inds, ftime, nm)
}

