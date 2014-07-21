

##' Three-columned matrix
##' 
##' An EMU dataset
##' 
##' 
##' @keywords datasets
NULL





##' EPG-compressed trackdata from the segment list coutts
##' 
##' An EMU dataset
##' 
##' 
##' @keywords datasets
NULL





##' Vector of word label from the segment list coutts
##' 
##' An EMU dataset
##' 
##' 
##' @keywords datasets
NULL





##' Segment list of words, read speech, female speaker of Australian English
##' from database epgcoutts
##' 
##' An EMU dataset
##' 
##' 
##' @keywords datasets
NULL





##' rms Data to coutts segment list
##' 
##' An EMU dataset
##' 
##' 
##' @name coutts.rms
##' @docType data
##' @keywords datasets
##' @examples
##' 
##' data(coutts.rms)
##' 
NULL





##' Trackdata of acoustic waveforms from the segment list coutts
##' 
##' An EMU dataset
##' 
##' 
##' @keywords datasets
NULL





##' EPG-compressed trackdata from the segment list coutts2
##' 
##' An EMU dataset
##' 
##' 
##' @keywords datasets
NULL





##' Vector of word label from the segment list coutts2
##' 
##' An EMU dataset
##' 
##' 
##' @keywords datasets
NULL





##' Segment list, same as coutts but at a slower speech rate
##' 
##' An EMU dataset
##' 
##' 
##' @keywords datasets
NULL





##' Trackdata of acoustic waveforms from the segment list coutts2
##' 
##' An EMU dataset
##' 
##' 
##' @keywords datasets
NULL





##' Emu segment list
##' 
##' Segment list of the demo database that is part of the Emu system.  It is
##' the result of a database query, that searched all segments at level
##' Phonetic.
##' 
##' A segment list is created via emu.query() or by using the EMU Query Tool.
##' 
##' @format First Column labels Second start time of the segment Third end time
##' of the segment Fourth utterance name of the utterance the segment was found
##' @seealso \code{\link{demo.vowels}} \code{\link{segmentlist}}
##' @keywords datasets
NULL





##' Emu track data for a rms track for segment list demo.all
##' 
##' A track list of the demo database that is part of the Emu system.  It is
##' the result of get rms data for the segment list demo.all (data(demo.all)).
##' 
##' A track list is created via emu.track() or via get data within the EMU
##' Query Tool.
##' 
##' @format A object with \$index, \$ftime and \$data
##' 
##' index: a two columned matrix with the range of the \$data rows that belong
##' to the segment ftime: a two columned matrix with the times marks of the
##' segment data: a vector with the rms data
##' @seealso \code{\link{demo.vowels.fm}} \code{\link{segmentlist}}
##' \code{\link{trackdata}}
##' @keywords datasets
NULL





##' F0 track data for segment list demo.vowels
##' 
##' A track list of the demo database that is part of the Emu system.  It is
##' the result of get F0 data for the segment list demo.vowels (see
##' data(demo.vowels)).
##' 
##' A track list is created via emu.track() or via get data within the EMU
##' Query Tool.
##' 
##' @format An object with \$index, \$ftime and \$data
##' 
##' index: a two columned matrix with the range of the \$data rows that belong
##' to the segment ftime: a two columned matrix with the times marks of the
##' segment data: a one columned matrix with the F0 values
##' @seealso \code{\link{demo.all.rms}} \code{\link{segmentlist}}
##' \code{\link{trackdata}}
##' @keywords datasets
NULL





##' Formant track data for segment list demo.vowels
##' 
##' A track list of the demo database that is part of the Emu system.  It is
##' the result of get fm data for the segment list demo.vowels (see
##' data(demo.vowels)).
##' 
##' A track list is created via emu.track() or via get data within the EMU
##' Query Tool.
##' 
##' @format index: a two columned matrix with the range of the \$data rows that
##' belong to the segment ftime: a two columned matrix with the times marks of
##' the segment data: a three columned matrix with the formant values of the
##' first three formants for each segment
##' @seealso \code{\link{demo.all.rms}} \code{\link{segmentlist}}
##' \code{\link{trackdata}}
##' @keywords datasets
NULL





##' Emu segment List
##' 
##' Segment list of the demo database that is part of the Emu system.  It is
##' the result of a database query, that searched all vowel segments at level
##' Phonetic.
##' 
##' A segment list is created via emu.query() or by using the EMU Query Tool.
##' 
##' @format First Column labels Second start time of the segment Third end time
##' of the segment Fourth utterance name of the utterance the segment was found
##' @seealso \code{\link{demo.all}} \code{\link{segmentlist}}
##' @keywords datasets
NULL





##' Trackdata of formants from the segment list dip
##' 
##' An EMU dataset
##' 
##' 
##' @keywords datasets
NULL





##' Vector of phoneme labels from the segment list dip
##' 
##' An EMU dataset
##' 
##' 
##' @keywords datasets
NULL





##' Segment list of dipththongs, two speakers one male, one female , Standard
##' North German, read speech from database kielread
##' 
##' An EMU dataset
##' 
##' 
##' @keywords datasets
NULL





##' Vector of speaker labels from the segment list dip
##' 
##' An EMU dataset
##' 
##' 
##' @keywords datasets
NULL





##' duration
##' 
##' calculates durations
##' 
##' 
##' @name dur
##' @docType data
##' @keywords attribute
NULL





##' Spectral vector of a single E vowel produced by a male speaker of Standard
##' North German.
##' 
##' An EMU dataset
##' 
##' 
##' @keywords datasets
NULL





##' EMU/R - R Programming Interface to the EMU Speech Database System
##' 
##' The EMU Speech Database System for Creation, Analysis and Query of Speech
##' Databases including acoustic and articulaty data as well as the
##' annotations. It provides annotation, signal processing and scripting
##' facilities, an interface to Praat, WaveSurfer as well as to Articulate
##' Instruments. Query of annotations in EQL or SQL via data export provided.
##' Query of signals using R. Developer: Tina John, Lasse Bombien, Steve
##' Cassidy, Jonathan Harrington.
##' 
##' This package is part of the EMU System and provides an interface to the R
##' Programming Environment for the query and analysis of the speech data
##' stored in the EMU Speech Database System and data exports from this source.
##' 
##' \tabular{ll}{ Package: \tab emu\cr Type: \tab Package\cr Version: \tab
##' 4.3\cr Date: \tab 2012-03-10\cr License: \tab Copyright 2012 IPS LMU
##' Munich. All rights reserved.\cr } Workflow: \tabular{ll}{ Query database -
##' \code{\link{emu.query}} \cr Extract labels - \code{\link{label}} \cr Get
##' signals over segment interval - \code{\link{emu.track}} \cr Extract point
##' in signal - \code{\link{dcut}} \cr Plot data - \code{\link{plot}},
##' \code{\link{dplot}}, \code{\link{eplot}} \cr Further analysis and
##' statistical processing \cr }
##' 
##' @name emu-package
##' @aliases emu emu-package
##' @docType package
##' @author Tina John (help text)
##' 
##' Maintainer: <jmh@@phonetik.uni-muenchen.de>
##' @seealso \code{\link{emu.testsuite}}
##' @references Harrington, J. (2010). The Phonetic Analysis of Speech Corpora.
##' Blackwell.
##' @keywords package
##' @examples
##' 
##'     \dontrun{
##'     # emu.query
##'     # Available Databases:
##'     dbinfo()
##' 
##'     if(!any(dbinfo()=="demo")) {
##'         message("No database demo found in the local EMU System - function can not be run")
##'     } else {
##'         # emu.query
##'         # An EMU query    
##'         seg = emu.query("demo","*","Phonetic = @: | e: | ei | A | E | @u")
##'         seg
##'         
##'         #Extract Labels from segment list
##'         seg.lab = label(seg)
##'         seg.lab
##'        
##'         # emu.track
##'         # Extraction of the tracks for the segment list
##'         seg.sample = emu.track(seg,"samples")
##'     
##'         # Plot of signals
##'         plot(seg.sample,label=seg.lab, type="l", main="waveforms")
##'         
##'         # dplot, eplot
##'         # Extract track values at point in time
##'         seg.fm = emu.track(seg,"fm")
##'         seg.fm.5 = dcut(seg.fm, .5, prop =TRUE)
##'         
##'         # Plot the data as time signal and formant card
##'         dplot(seg.fm[,1:2], seg.lab, normalise=TRUE, main = "Formants over vowel duration")
##'         eplot(seg.fm.5[,1:2], seg.lab, dopoints=TRUE, doellipse=FALSE, main = "F1/F2 of vowel midpoint", form=TRUE, xlab = "F2 in Hz", ylab = "F1 in Hz")
##'         
##'         # emu.requery
##'         # An EMU query and ...
##'         segH = emu.query("demo","*","Phonetic = H")
##'         segH
##'         
##'         # ... and requery
##'         segHseql1 = emu.requery(segH,"Phonetic","Phonetic",sequence=-1)
##'         segHseql1
##'         segH.lab = label(segHseql1)
##'     
##'         # plot.spectral
##'         # Plot of spectral data from 50% of aspiration duration 
##'         segH.dft = emu.track(segH,"dft")
##'         segH.dft.5 = dcut(segH.dft, .5,prop=TRUE)
##'         plot(segH.dft.5,segH.lab, main = "Spectral data of aspiration")
##'     }
##' }
##' 
##'     \dontrun{emu.testsuite()}
##' 
NULL





##' the emu command send to EMU
##' 
##' this is the platform dependent command sent to EMU
##' 
##' 
##' @keywords internal
NULL





##' EMU directory
##' 
##' Gives the EMU installation path
##' 
##' 
##' @name emu.directory
##' @docType data
##' @keywords datasets
NULL





##' emu init
##' 
##' initialises emu
##' 
##' 
##' @keywords internal
NULL





##' emu inquotes
##' 
##' quotes emu
##' 
##' 
##' @keywords internal
NULL





##' emu options
##' 
##' the emu options
##' 
##' 
##' @keywords internal
NULL





##' emu platform
##' 
##' where are we running
##' 
##' 
##' @keywords internal
NULL





##' emu system
##' 
##' the emu system
##' 
##' 
##' @keywords internal
NULL





##' emu tempfile
##' 
##' Tempfile for read.trackdata
##' 
##' 
##' @keywords internal
NULL





##' Emu variables
##' 
##' \describe{ \item{list("emu.version")}{Current version of the emu R package}
##' \item{list("emu.date")}{Date of package creation}
##' \item{list("emu.year")}{Year of package creation, copyright.} }
##' 
##' 
##' @name emu.variables
##' @aliases emu.version emu.date emu.year
##' @docType data
##' @keywords datasets
NULL





##' emudata init
##' 
##' loads package emudata as far as installed or reports message
##' 
##' 
##' @keywords internal
NULL





##' EPG-compressed trackdata from the segment list engassim
##' 
##' An EMU dataset
##' 
##' 
##' @keywords datasets
NULL





##' Vector of phonetic labels from the segment list engassim: nK = nk,ng , sK =
##' sk,sg
##' 
##' An EMU dataset
##' 
##' 
##' @keywords datasets
NULL





##' Segment list of a a sequence of syllable final n or N preceding k or g ,
##' isolated words single speaker, Australian English female from database
##' epgassim.
##' 
##' An EMU dataset
##' 
##' 
##' @keywords datasets
NULL





##' Vector of word labels from the segment list engassim.
##' 
##' An EMU dataset
##' 
##' 
##' @keywords datasets
NULL





##' Electropalatographic contact indices
##' 
##' epgai(), epgci(), epgdi() return the anteriority index, the centrality
##' index, the dorsopalatal index respectively as a trackdata object or a
##' vector
##' 
##' These are exact implementations of the formulae for calculating the EPG
##' anteriority, EPG centrality, and EPG dorsopalatal indices as described in
##' Recasens & Pallares (2001).
##' 
##' @aliases epgai epgci epgdi
##' @param epgdata An eight-columned EPG-compressed trackdata object, or an
##' eight columned matrix of EPG-compressed trackdata, or a 3D palatographic
##' array that is the output of palate()
##' @param weights A vector of five values that are applied to EPG rows 1-5
##' respectively in epgai(). A vector of four values that are applied to
##' columns 1 and 8, to columns 2 and 7, columns 3 and 6, columns 4 and 5
##' respectively. Defaults to the values given in Recasens & Pallares (2001).
##' @return These functions return a trackdata object if they are applied to an
##' eight-columned EPG-compressed trackdata object, otherwise a one-columned
##' matrix.
##' @author Jonathan Harrington
##' @seealso \code{\link{epgcog}} \code{\link{epggs}} \code{\link{palate}}
##' @references GIBBON, F. AND NICOLAIDIS, K. (1999). Palatography.  In W.J.
##' Hardcastle & N. Hewlett (eds). Coarticulation.  (pp. 229-245). Cambridge
##' University Press: Cambridge.
##' 
##' RECASENS, D. & PALLARES, M. (2001) Coarticulation, assimilation and
##' blending in Catalan consonant clusters. Journal of Phonetics, 29, 273-301.
##' @keywords math
##' @examples
##' 
##' #  Anteriority index: trackdata
##' ai <- epgai(coutts.epg)
##' #  Dorsopalatal index, one-columned matrix
##' di <- epgdi(dcut(coutts.epg, 0.5, prop=TRUE))
##' # Next to examples: Centrality  index, one-columed matrix
##' ci <- epgci(palate(coutts.epg))
##' ci <- epgci(palate(dcut(coutts.epg, 0.5, prop=TRUE)))
##' 
##' 
NULL





##' Expand emusegs
##' 
##' see function
##' 
##' 
##' @aliases [.emusegs
##' @keywords internal
NULL





##' expand EPG
##' 
##' see function
##' 
##' 
##' @aliases [.EPG
##' @keywords internal
NULL





##' Expand spectral
##' 
##' see function
##' 
##' 
##' @aliases [.spectral
##' @keywords internal
NULL





##' Expand trackdata
##' 
##' see function
##' 
##' 
##' @aliases [.trackdata
##' @keywords internal
NULL





##' Spectral trackdata object from the segment list fric.
##' 
##' An EMU dataset
##' 
##' 
##' @keywords datasets
NULL





##' Vector of labels from the segment list fric
##' 
##' An EMU dataset
##' 
##' 
##' @keywords datasets
NULL





##' Segment list of word-medial s or z one male speaker of Standard North
##' German, read speech from database kielread.
##' 
##' An EMU dataset
##' 
##' 
##' @keywords datasets
NULL





##' Vector of word labels from the segment list fric.
##' 
##' An EMU dataset
##' 
##' 
##' @keywords datasets
NULL





##' get originalFreq
##' 
##' see function
##' 
##' 
##' @keywords internal
NULL





##' Trackdata of formants from the segment list isol
##' 
##' An EMU dataset
##' 
##' 
##' @keywords datasets
NULL





##' Vector of vowel phoneme labels from the segment list isol
##' 
##' An EMU dataset
##' 
##' 
##' @keywords datasets
NULL





##' Segment list of vowels in a d d context isolated word speech, one male
##' speaker of Australian English from database isolated.
##' 
##' An EMU dataset
##' 
##' 
##' @keywords datasets
NULL





##' EPG-compressed trackdata from the segment list polhom
##' 
##' An EMU dataset
##' 
##' 
##' @keywords datasets
NULL





##' Vector of phonetic labels from the segment list polhom
##' 
##' An EMU dataset
##' 
##' 
##' @keywords datasets
NULL





##' Segment list of four Polish homorganic fricatives from database epgpolish.
##' 
##' An EMU dataset
##' 
##' 
##' @keywords datasets
NULL





##' Segment list
##' 
##' A segment list is the result of emu.query() or read.emusegs().
##' 
##' 
##' @aliases segmentlist emusegs
##' @format multi-columned matrix one row per segment \itemize{ \itemfirst
##' columnlabel \itemsecond columnsegment onset time \itemthird columnsegment
##' offset time \itemfourth columnutterance name }
##' @seealso \code{\link{emu.query}}, \code{\link{demo.vowels}}
##' @keywords classes
##' @examples
##' 
##'    data(demo.vowels)
##'    
##'    #demo.vowels is a segment list
##'    demo.vowels
##' 
NULL





##' Start and end times for EMU segment lists and trackdata objects
##' 
##' Obtain start and end times for EMU segment lists and trackdata objects
##' 
##' The function returns the start and/or end times of either a segment list or
##' a trackdata object. The former refers to the boundary times of segments,
##' the latter the start and end times at which the tracks from segments occur.
##' start.emusegs and end.emusegs give exactly the same output as start and end
##' respectively.
##' 
##' @aliases start.emusegs end.emusegs start.trackdata end.trackdata
##' @param x a segment list or a trackdata object
##' @param ...  due to the generic only
##' @return A vector of times.
##' @author Jonathan Harrington
##' @seealso \code{\link{tracktimes}}
##' @keywords utilities
##' @examples
##' 
##' # start time of a segment list
##' start(polhom)
##' # duration of a segment list
##' end(polhom) - start(polhom)
##' # duration from start time of segment list
##' # and start time of parallel EPG trackdata
##' start(polhom) - start(polhom.epg)
##' 
##' 
NULL





##' Data frame of various parameters and labels from the segment list vowlax
##' 
##' An EMU dataset
##' 
##' 
##' @keywords datasets
NULL





##' Spectral matrix centred at the temporal midpoint of the vowels from the
##' segment list vowlax.
##' 
##' An EMU dataset
##' 
##' 
##' @keywords datasets
NULL





##' Matrix of formant data extracted at the temporal midpoint from the segment
##' list vowlax.
##' 
##' An EMU dataset
##' 
##' 
##' @keywords datasets
NULL





##' Trackdata of formants from the segment list vowlax
##' 
##' An EMU dataset
##' 
##' 
##' @keywords datasets
NULL





##' Vector of fundamental frequency extracted at the temporal midpoint from the
##' segment list vowlax.
##' 
##' An EMU dataset
##' 
##' 
##' @keywords datasets
NULL





##' Trackdata of fundamental frequency from the segment list vowlax
##' 
##' An EMU dataset
##' 
##' 
##' @keywords datasets
NULL





##' Vector of phoneme labels from the segment list vowlax
##' 
##' An EMU dataset
##' 
##' 
##' @keywords datasets
NULL





##' Vector of labels preceding the vowels from the segment list vowlax
##' 
##' An EMU dataset
##' 
##' 
##' @keywords datasets
NULL





##' Segment list of four lax vowels, read speech, one male and one female
##' speaker of Standard North German from database kielread.
##' 
##' An EMU dataset
##' 
##' 
##' @keywords datasets
NULL





##' Vector of labels following the vowels from the segment list vowlax
##' 
##' An EMU dataset
##' 
##' 
##' @keywords datasets
NULL





##' Vector of RMS energy values at the temporal midpoint extracted at the
##' temporal midpoint from the segment list vowlax
##' 
##' An EMU dataset
##' 
##' 
##' @keywords datasets
NULL





##' Trackdata of RMS energy from the segment list vowlax
##' 
##' An EMU dataset
##' 
##' 
##' @keywords datasets
NULL





##' Vector of speaker labels from the segment list vowlax.
##' 
##' An EMU dataset
##' 
##' 
##' @keywords datasets
NULL





##' Vector of word labels from the segment list vowlax.
##' 
##' An EMU dataset
##' 
##' 
##' @keywords datasets
NULL





##' Vector of word labels from segment list wordlax
##' 
##' For wordlax (see data(vowlax))
##' 
##' 
##' @keywords datasets
NULL



