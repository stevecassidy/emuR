##########################################################################
#									 #
#	  This file is part of the MU+ Speech Database System		 #
#			  (c) Copyright 1994,				 #
#	     Speech Hearing and Language Research Centre,		 #
#	       Macquarie University, Sydney, Australia.			 #
#									 #
#	Copying and redistribution of this software is subject to the	 #
#	conditions of the MU+ licence.  Any modifications of this code 	 #
#	remain the property of Macquarie University.			 #
#									 #
##########################################################################

## Methods that define operations on the class "segs"
## see also is.segs and isegs.to.segs

"print.segs"<- function(segs)
{
  cat( "Segment list\n" )
  if( !is.null(attr(segs, "level")) ) 
    cat("Segment Level:", attr(segs,"level"), "\n")
  if( !is.null(attr(segs, "query")) ) 
    cat( "From query:", attr(segs, "query" ), "\n" )
  if( !is.null(attr(segs, "template")) ) 
    cat( "Template:", attr(segs, "template" ), "\n" )
  print.matrix(segs)
  invisible()
}

"[.segs"<- function(segs,i,j)
{
  x _ NextMethod("[", drop=F)
  if(is.matrix(x) & ncol(x)==3){
    class(x) <- "segs"
    attr(x,"level") <- attr(segs,"level") 
    attr(x,"template") <- attr(segs,"template") 
    attr(x,"query") <- attr(segs,"query") 
  }
  x
}

"summary.segs" <- function(segs, ...)
{
  if( is.null(attributes(segs)$level))
    level <- "Unknown"
  else
    level <- attributes(segs)$level
  cat("Mu+ segment list, level:", level)
  cat(" with", nrow(segs), "segments\n\n")
  cat("Segment distribution:\n")
  print(table(label(segs)))
  invisible()
}

"segbind" <- function(segs1,segs2)
{
  # join together two segment lists
  if(!is.segs(segs1)) stop("First argument to segbind isn't a segment list")
  if(!is.segs(segs2)) stop("Second argument to segbind isn't a segment list")

  level <- attributes(segs1)$level

  if(attributes(segs2)$level != level)
    stop("Segment lists at different levels in segbind")

  as.segs(rbind(segs1,segs2), level=level)  
}



"utt.segs"<- function(segs) segs[, 3]

"mustart.segs"<- function(segs) as.single(segs[, 1])
"muend.segs"<- function(segs) as.single(segs[, 2])



"museg.write.segs"<-
function(SegList, File)
{
	mat <- cbind(dimnames(SegList)[[1]], SegList)
	write(t(mat), File, ncolumns = 4)
}


# Local Variables:
# mode:S
# S-temp-buffer-p:t
# End:
