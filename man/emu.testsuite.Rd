\name{emu.testsuite}
\alias{emu.testsuite}
\alias{emu.testsuite}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Test suite for functions in the \code{\link{emu-package}}
}
\description{
Checks all functions in the emuR Tcl library in the EMU Speech Database System.
}
\usage{
emu.testsuite()
}
\author{
Tina John
}

\seealso{
\code{\link{emu-package}}
}
\examples{
\dontrun{emu.testsuite()

    # emu.query
    # Available Databases:
    dbinfo()

    # emu.query
    seg = emu.query("demo","*","Phonetic = @: | e: | ei | A | E | @u")
    
    # Extract Labels from segment list
    seg.lab = label(seg)
   
    # emu.track
    # Extraction of the tracks for the segment list
    seg.sample = emu.track(seg,"samples")
    
    #Plot of signals
    plot(seg.sample,label=seg.lab, type="l", main="waveforms")
    
    # dplot, eplot
    # Extract track values at point in time
    seg.fm = emu.track(seg,"fm")
    seg.fm.5 = dcut(seg.fm, .5, prop =T)
    
    #Plot the data as time signal and formant card
    dplot(seg.fm[,1:2], seg.lab, normalise=T, main = "Formants over vowel duration")
    eplot(seg.fm.5[,1:2], seg.lab, dopoints=T, doellipse=F, main = "F1/F2 of vowel midpoint", form=T, xlab = "F2 in Hz", ylab = "F1 in Hz")
    
    # emu.requery
    segH = emu.query("demo","*","Phonetic = H")
    segHseql1 = emu.requery(segH,"Phonetic","Phonetic",sequence=-1)
    segH.lab = label(segHseql1)

    # plot.spectral
    segH.dft = emu.track(segH,"dft")
    segH.dft.5 = dcut(segH.dft, .5,prop=T)
    plot(segH.dft.5,segH.lab, main = "Spectral data of aspiration")
}

}
\keyword{utilities}
