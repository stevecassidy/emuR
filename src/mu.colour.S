#############################################################################
#                                                                           #
#   copyright            : (C) 2000 SHLRC, Macquarie University             #
#   email                : Steve.Cassidy@mq.edu.au			    #
#   url			 : http://www.shlrc.mq.edu.au/emu		    #
#									    #
#   This program is free software; you can redistribute it and/or modify    #
#   it under the terms of the GNU General Public License as published by    #
#   the Free Software Foundation; either version 2 of the License, or       #
#   (at your option) any later version.                                     #
#									    #
#############################################################################

"mu.colour"<- function( labs, col, linetype, defaultcolour=1 )
{
  ## Generate a colour and line type vector for 
  ## one of the mu+ plots
  ## col and linetype can be either T, F or a vector 
  ## if they are F then the corresponding return element
  ## is a vector of 1's of length len
  ## if they are T then the corresponding return element
  ## is a vector of different numbers, length len
  ## if either is a vector then it is converted to a 
  ## numeric vector with a different integer per unique label 

  ## if col is a vector of length 2*length(unique(labs)) then
  ## it should be a set of pairs {label, number} and number
  ## will be assigned to label in the resulting list
  
  len _ length(labs)

  if( length(col)==2*length(unique(labs)) ) {
    colours _ 1:length(labs)
    n <- 1
    while( n <= length(unique(labs)) ) {
      lab <- col[2*n-1]
      colour <- col[2*n]
      colours[labs==lab] <- as.numeric(colour)
      n <- n+1
    }
    col <- colours
  } else if(is.numeric(col)) {
    if(length(col)!=length(labs)) 
      stop("length of colour vector differs from that of labels")
  } else if(!is.logical(col)){
    col <- codes(as.factor(col))
    if(length(col)!=length(labs)) 
      stop("length of colour vector differs from that of labels")
  } else {
    if(col)
      col <- codes(as.factor(labs))
    else col <- rep(defaultcolour, len)
  }

  if(!is.logical(linetype)){
    if( length(linetype) == 1)
      linetype <- rep(linetype, len)
    else
      linetype <- codes(as.factor(linetype))
    
    if(length(linetype)!=length(labs)) 
      stop("length of linetype vector differs from that of labels")
  }
  else {
    if(linetype)
      linetype <- codes(as.factor(labs))
    else 
      linetype <- rep(1, len)
  }
  foo _ NULL
  foo$colour _ col
  foo$linetype  _ linetype
  ## obtain unique elements for the purpose of creating a legend
  p1 _ paste(labs, col, linetype)
  p1.temp _ duplicated(p1)
  foo$legend$lab _ labs[!p1.temp]
  foo$legend$col _ foo$colour[!p1.temp]
  foo$legend$lty _ foo$linetype[!p1.temp]
  return(foo)
}


# Local Variables:
# mode:S
# S-temp-buffer-p:t
# End:
