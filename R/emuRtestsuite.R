
emu.testsuite = function ( ) {
  # emu.query
  message("\n Available Databases:")
  cat("dbinfo() \n\n")
  print(dbinfo())
  
  if(!any(dbinfo()=="demo")) {
    message("No database demo found in the local EMU System - function can not be run")
    return(invisible())
  }
  par(mfrow=c(2,2))
  
  # emu.query
  message("\n An EMU query")
  cat("seg = emu.query(\"demo\",\"*\",\"Phonetic = @: | e: | ei | A | E | @u | \'4\'\")\n\n")
  
  seg = emu.query("demo","*","Phonetic = @: | e: | ei | A | E | @u | '4'")
  print(seg)
  
  message("\n Extract Labels from segment list")
  cat("seg.lab = label(seg) \n")
  seg.lab = label(seg)
  print(seg.lab)
  
  # emu.track
  message("\n Extraction of the tracks for the segment list")
  cat("seg.sample = emu.track(seg,\"samples\")\n\n")
  seg.sample = emu.track(seg,"samples")
  message("\n Plot of first signal in track objekt and segment list [@]")
  cat("plot(seg.sample[1],label=seg.lab[1], type=\"l\", main=\"waveforms\")\n\n")
  plot(seg.sample[1],label=seg.lab[1], type="l", main="waveforms")
  
  # dplot, eplot
  message("\n Extract track values at point in time")
  cat("seg.fm = emu.track(seg,\"fm\"); # get formants\n")
  seg.fm = emu.track(seg,"fm")
  cat("seg.fm.5 = dcut(seg.fm, .5, prop =TRUE); # cut data at 50% segment duration\n")
  seg.fm.5 = dcut(seg.fm, .5, prop =TRUE)
  message("\n Plot the data as time signal and formant card\n")
  cat("dplot(seg.fm[,1:2], seg.lab, normalise=TRUE, main = \"Formants over vowel duration\"); #time signal\n")
  dplot(seg.fm[,1:2], seg.lab, normalise=TRUE, main = "Formants over vowel duration")
  cat("eplot(seg.fm.5[,1:2], seg.lab, dopoints=TRUE, doellipse=FALSE, main = \"F1/F2 of vowel midpoint\", formant=TRUE, xlab = \"F2 in Hz\", ylab = \"F1 in Hz\"); # F1/F2 plane\n")    
  eplot(seg.fm.5[,1:2], seg.lab, dopoints=TRUE, doellipse=FALSE, main = "F1/F2 of vowel midpoint", formant=TRUE, xlab = "F2 in Hz", ylab = "F1 in Hz")
  
  # emu.requery
  message("\n An EMU query and ...")
  cat("segH = emu.query(\"demo\",\"*\",\"Phonetic = H\")\n\n")
  segH = emu.query("demo","*","Phonetic = H")
  print(segH)
  
  message("\n ... and requery")
  cat("segHseql1 = emu.requery(segH,\"Phonetic\",\"Phonetic\",sequence=-1)\n\n")
  segHseql1 = emu.requery(segH,"Phonetic","Phonetic",sequence=-1)
  print(segHseql1)
  segH.lab = label(segHseql1)
  
  message("\n Plot of spectral data from 1% of aspiration duration (burst) ")
  cat("segH.dft = emu.track(segH,\"dft\"); #spectral data\n")
  segH.dft = emu.track(segH, "dft")
  cat("segH.dft.01 = dcut(segH.dft, .01,prop=TRUE); #Extract mid point\n")
  segH.dft.01 = dcut(segH.dft, 0.01, prop = TRUE)
  cat("plot(segH.dft.01,segH.lab, main = \"Spectral data of aspiration\"); #Plot data\n")
  cat("... ... #Create label vektor with alv and bil to separate stops\n")
  alv = segH.lab 
  alv = segH.lab == "d"
  bil = segH.lab %in% c("p","b")
  alvbi = segH.lab 
  alvbi[segH.lab == "d"] = "alv"
  alvbi[segH.lab %in% c("p","b")] = "bil"
  
  cat("segH.dft.01.smooth = fapply(segH.dft.01,dct,5,fit=TRUE); #Smooth data with 5 dct coefficients\n")
  segH.dft.01.smooth = fapply(segH.dft.01,dct,5,fit=TRUE)
  
  cat("plot(segH.dft.01.smooth[alv | bil,], alvbi[alv | bil], fun=mean, power=TRUE, main = \"Spectral data of burst\"); #Plot data\n")
  plot(segH.dft.01.smooth[alv | bil,], alvbi[alv | bil], fun=mean, power=TRUE,main = "DCT smoothed spectral data of burst")
  
}
