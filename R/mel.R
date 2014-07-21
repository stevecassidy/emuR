"mel" <- function(a)
{
  UseMethod("mel")
}

"mel.default" <- function (a) 
{
  1/log(2) * (log(1 + (a/1000))) * 1000
}



