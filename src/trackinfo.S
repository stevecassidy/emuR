"trackinfo" <-
function(template)
{
# returns the available track names for a database
# in column 1 and their extensions in column 2
template <- paste(template, "tpl", sep=".")
if(dirname(template)==".")
template <-  paste( dirname(emu.options("bindir")), template, sep="/")
p <- scan(template, what="list", quiet=TRUE)
which <- p=="track"
n <- 1:length(p)
valsa <- n[which]+1
valsb <- n[which]+2
result <- cbind(p[valsa], p[valsb])
dimnames(result) <- list(NULL, c("trackname", "extension"))
result
}

