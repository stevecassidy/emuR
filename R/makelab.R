"makelab" <- function(vectimes,  uttname, dir, extn="xlab", labels=NULL)
{
  # Function to write out ESPS label files
  # One label file is written per element in uttname
  # The resulting file is uttname.extn
  # and it is written to the directory given by dir.
  # vectimes:  a vector of times
  # uttname: a character vector of the same length as vectimes
  # giving the utterance name associated with each
  # element of vectimes
  # dir: a character specifying the directory
  # extn: a character specifying the extension of the
  # resulting files. Defaults to xlab
  # labels:  if missing, each label written out
  # has the label "x". Otherwise it can be a single
  # element character vector such as "T" (each label
  # then has the label "T") or else a vector of
  # the same length as vectimes. 
  # Example:
  
  # s.vk <- emu.query("epg-demo", "*", "[Phoneme!=x -> Phoneme=k]")
  # l.vk <- label(s.vk)
  # e.vk <- emu.track(s.vk, "epg")
  # e.dp <- dp(e.vk)
  # maxzeit <- dmax(e.dp)
  # labelfile(maxzeit[,1], utt(s.vk), "c:/d/test", "T")
  
  if(is.null(labels))
    labels <- rep("x", length(vectimes))
  if(length(labels)==1)
    labels <- rep(labels, length(vectimes))
  
  ufun <- function(vectimes, uttname, labels, extn, dir)
  {
    a1 <- paste("signal", uttname)
    a2 <- "nfields 1"
    a3 <- "#"
    omat <- cbind(vectimes/1000, rep(125, length(vectimes)), 
                  labels)
    psort <- sort.list(vectimes/1000)
    omat <- omat[psort,]
    
    
    dirloc <- paste(paste(dir, uttname, sep="/"), extn, sep=".")
    write(t(a1), dirloc)
    write(t(a2), dirloc, append=TRUE)
    write(t(a3), dirloc, append=TRUE)
    write(t(omat), dirloc, ncolumns=3, append=TRUE)
    
  }
  
  
  for(j in unique(uttname)){
    temp <- uttname==j
    ufun(vectimes[temp], j, labels[temp], extn, dir)
  }
  
}

