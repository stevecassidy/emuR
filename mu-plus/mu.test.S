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

mu.test <- function(){

  cat( "First, we'll find out if there's a graphics window...\n" )
  display <- getenv("DISPLAY")
  if( display != "" ){
    if(is.null(dev.list())) {
      cat("No, so I'll make one...\n")
      cat("Starting openlook\n")
      openlook()            # start up graphics window
    }
    else{
      cat("Yes, there is\n")
      par(mfrow=c(1,1))
    }
  }
  else
    cat( "OOPS!!\nCan't start graphics window so no graphics tests can be done.\n" )

  cat( "Now I'll try to make an utterance list from the default database...\n" )
  utts <- mkdb()
  if( length(utts) > 0 ) 
    cat( "that seemed to go ok\n")
  else{
    stop(paste("I didn't find any utterances,\n",
	       "There should be a bunch of .lab files in", 
	       options()$dbdefault, 
	       "\nCheck them against the file list provided\n"))
  }
  cat( "Now I'll make an utterance list of only the isolated words\n")
  cat( "(I'm assuming that all the right utterances are there now)...\n")
  utts <- mkdb(pattern="msd*")
  if( length(utts) > 0 ) 
    cat( "that worked as well\n")
  else{
    cat( "Ah well, you are missing some of the isolated word utterances.\n" )
    cat( "There should be  lots of msd*.lab files in ", 
	options()$dbdefault )
    stop()
  }
  
  cat("Now, we'll see what tracks are available for these utterances.\n")
  cat("(This is the first test of the C code)\n")
  old.extensions <- options()$extensions
  on.exit( options(extensions=old.extensions) )
  mu.extensions("SSFF")
  tr <- tracks(utts[1])
  if( length(tr)>0 ){
    print(tr)
    cat("Seems healthy enough.\n")
  }
  else {
    stop( "Oh dear, it didn't work. Here's where you should get in touch with\nSHLRC. Try running track() on it's own (see the documentation) and send a\n transcript of this run and your own back to us.")
  }
  
  cat("Time to generate a segment list,\n",
      "I'll only use the first four utterances...\n")
  segs <- phon(utts[1:4])
  if( nrow(segs) > 0 )
    cat("Looking good\n")
  else
    stop( "Baaaarp! failure!! Please contact SHRLC with a transcript of this\nsession. Also you might try running phon yourself (see the documentation) and \nsend us a transcript of that (if it doesn't work).\n")


  cat( "Now we extract some data for the first two formants...\n" )
  data <- track(segs, "fm[1:2]", cut=0.5)
  if( ncol(data) == 2 & nrow(data) == nrow(segs) )
    cat( "That worked ok\n" )
  else
    stop( "Something went wrong while using track(). Contact SHLRC with\na transcript of this session and perhaps try track yourself (see\nthe documentation) and send us a transcript of that (if it doesn't work).\n" )
    
  cat( "and we'll get the text labels for these segments...\n" )
  labs <- label(segs, "Text")
  if( length(labs) == nrow(segs) )
    cat( "label works fine\n" )
  else
    stop( "Something went wrong while using label(). Contact SHLRC with\na transcript of this session and perhaps try track yourself (see\nthe documentation) and send us a transcript of that (if it doesn't work).\n" )
  

  if( display == "" ){
    cat( "Well, without a plot window I can't try a lot more.\n" )
    cat( "Try running mu.test again under X-windows\n" )
    return(invisible())
  }

  cat( "OK. Now we'll try to plot that data...\n" )
  plot( data, type="n", xlab="First Formant Frequency (Hz)", 
       ylab="Second Formant Frequency (Hz)")
  text( data, labs )
  
  cat( "You should see a plot of the F1/F2 plane with each segment plotted\n" )
  cat( "with it's text label\n" )
  cat( "If you don't, something went wrong!\n" )
  cat( "I'll wait a little for you to admire the plot\n\n" )

  for(i in 1:5000) { frob <- i + 1 }

  cat( "Now for some more advanced plots using the mu+ plotting functions.\n")
  cat( "First, a segplot of the first formant for each segment...\n" )
  segplot( segs, "fm[1]" )
	
  cat( "Now, an uttplot of F0 for the first utterance\n")
  cat( "You will be able to play segments of the speech file using the mouse...\n\n" )
  ## work out how many segments in the first utterance...
  nn <- sum(utt(segs)==as.character(utt(segs[1,])))
  uttplot( segs[1:nn,], "fm[1:3]", talk=T )

  cat("Hope you enjoyed that!\n")

  cat("I'll now try to call up the muspec and cda programs under unix.\n")
  cat("First I'll extract the bark spectra at the midpoint for the\n" )
  cat("phonetic segments from the first 30 utterances\n")
  segs <- phon(utts[1:30])
  dat <- muspec( segs, dbspec=F, bark=T, offset=0.5, verbose=T)
  
  if( nrow(dat$bark) == nrow(segs) ){
    cat("That seemed to work. Here's a quick plot of 20 spectra for your\n")
    cat("edification. (If they look wiered maybe something went wrong)\n\n")
  }
  else
    stop("Ummm...it didn't seem to work. Please get in touch with SHLRC\nor perhaps try muspec yourself to check.\n")

  matplot(matrix(rep(1:20,10), ncol=10, nrow=20),t(dat$bark[1:10,]), type="l")

  cat("Now I'll try a canonical discriminant analysis of that data\n")
  cat("Using the cda program.\n")
  labs <- label(segs)
  dat.dis <- discrim( dat$bark, labs, dims=2, verbose=T)

  if( nrow(dat.dis$data) == nrow(dat$bark) )
    cat("Looks ok. Let's try a plot\n")
  else
    stop("It didn't seem to work. Please get in touch with SHLRC\nor perhaps try discrim yourself to check.\n")

  plot(dat.dis$data, type="n")
  text(dat.dis$data, labs)

  cat("That's all folks!!\n\n" )
}


# Local Variables:
# mode:S
# S-temp-buffer-p:t
# End:
