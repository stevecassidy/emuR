\name{emu-package}
\alias{emu}
\alias{emu-package}
\docType{package}
\title{
EMU/R - R Programming Interface to the EMU Speech Database System
}
\description{
    The EMU Speech Database System for Creation, Analysis and Query of Speech Databases including acoustic and articulaty data as well as the annotations. It provides annotation, signal processing and scripting facilities, an interface to Praat, WaveSurfer as well as to Articulate Instruments. Query of annotations in EQL or SQL via data export provided. Query of signals using R. Developer: Tina John, Lasse Bombien, Steve Cassidy, Jonathan Harrington.
    
    This package is part of the EMU System and provides an interface to the R Programming Environment for the query and analysis of the speech data stored in the EMU Speech Database System and data exports from this source. 
}
\details{
\tabular{ll}{
Package: \tab emu\cr
Type: \tab Package\cr
Version: \tab 4.3\cr
Date: \tab 2012-03-10\cr
License: \tab Copyright 2012 IPS LMU Munich. All rights reserved.\cr
}
Workflow:
\tabular{ll}{
    Query database - \code{\link{emu.query}} \cr
    Extract labels - \code{\link{label}} \cr
    Get signals over segment interval - \code{\link{emu.track}} \cr
    Extract point in signal - \code{\link{dcut}} \cr
    Plot data - \code{\link{plot}}, \code{\link{dplot}}, \code{\link{eplot}} \cr
    Further analysis and statistical processing \cr
}
}
\author{
Tina John (help text)

Maintainer: <jmh@phonetik.uni-muenchen.de>
}
\references{
Harrington, J. (2010). The Phonetic Analysis of Speech Corpora. Blackwell.
}
\keyword{ package }
\seealso{
\code{\link{emu.testsuite}}
}
\examples{
   
    # emu.query
    # Available Databases:
    dbinfo()

    if(!any(dbinfo()=="demo")) {
        message("No database demo found in the local EMU System - function can not be run")
    } else {
        # emu.query
        # An EMU query    
        seg = emu.query("demo","*","Phonetic = @: | e: | ei | A | E | @u")
        seg
        
        #Extract Labels from segment list
        seg.lab = label(seg)
        seg.lab
       
        # emu.track
        # Extraction of the tracks for the segment list
        seg.sample = emu.track(seg,"samples")
    
        # Plot of signals
        plot(seg.sample,label=seg.lab, type="l", main="waveforms")
        
        # dplot, eplot
        # Extract track values at point in time
        seg.fm = emu.track(seg,"fm")
        seg.fm.5 = dcut(seg.fm, .5, prop =TRUE)
        
        # Plot the data as time signal and formant card
        dplot(seg.fm[,1:2], seg.lab, normalise=TRUE, main = "Formants over vowel duration")
        eplot(seg.fm.5[,1:2], seg.lab, dopoints=TRUE, doellipse=FALSE, main = "F1/F2 of vowel midpoint", form=TRUE, xlab = "F2 in Hz", ylab = "F1 in Hz")
        
        # emu.requery
        # An EMU query and ...
        segH = emu.query("demo","*","Phonetic = H")
        segH
        
        # ... and requery
        segHseql1 = emu.requery(segH,"Phonetic","Phonetic",sequence=-1)
        segHseql1
        segH.lab = label(segHseql1)
    
        # plot.spectral
        # Plot of spectral data from 50% of aspiration duration 
        segH.dft = emu.track(segH,"dft")
        segH.dft.5 = dcut(segH.dft, .5,prop=TRUE)
        plot(segH.dft.5,segH.lab, main = "Spectral data of aspiration")
    }


    \dontrun{emu.testsuite()}
}
