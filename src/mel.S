"mel" <-
function(a, ...)
{
UseMethod("mel")
}

"mel.default" <-
function (x) 
{
    1/log(2) * (log(1 + (x/1000))) * 1000
}



