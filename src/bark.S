# Convert Bark to Hz and vice-versa
# Formulae in 
# H. Traunmüller (1990) "Analytical expressions for the 
# tonotopic sensory scale" J. Acoust. Soc. Am. 88: 97-100. 
# f is a frequency in Hz unless inv=TRUE,
# in which case f is a frequency in Bark
# inv: if T, performs Bark to Hz conversion


"bark" <-
function (f, ...) {
   UseMethod("bark") 
}


"bark.default" <-
function (f, inv = FALSE, ...) 
{
    if (!inv) {
        result = ((26.81 * f)/(1960 + f)) - 0.53
    } else {
        result = (1960 * (f + 0.53))/(26.28 - f)
    }
    return(result)
}

