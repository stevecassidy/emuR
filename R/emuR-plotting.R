
##' Create spectrogram image as raster
##' 
##' @param audioFilePath path to audio file to plot spectrogram of
##' @param begin begin time in seconds (passed into begin parameter of \code{wrassp::read.AsspDataObj})
##' @param end  end time in seconds (passed into end parameter of \code{wrassp::read.AsspDataObj})
##' @param windowSizeInSecs window size in seconds
##' @param alpha value of spectrogram
##' @param lowerFreq lower frequency limit of spectrogram
##' @param upperFreq upper frequency limit of spectrogram
##' @param window window type used in spectrogram calculation. Allowed values
##' are:
##' \itemize{
##'  \item "BARTLETT"
##'  \item "BARTLETTHANN"
##'  \item "BLACKMAN"
##'  \item "COSINE"
##'  \item "GAUSS" (the default)
##'  \item "HAMMING"
##'  \item "HANN"
##'  \item "LANCZOS"
##'  \item "RECTANGULAR"
##'  \item "TRIANGULAR"
##' }
##' @param dynRangeInDB dynamic range in DB of spectrogram 
##' @param audioChannel channel of audio file to draw spectrogram of (only
##' applicable when using multi-channel audio files)
##' @param preEmphasisFilterFactor used in time domain for amplifying high-freqs
##' @param invert invert the colors of the spectrogram
##' @return a image raster object
##' @importFrom  grDevices as.raster
##' @export
create_spectrogram_image_as_raster <- function(audioFilePath, 
                                               begin = 0,
                                               end = 0,
                                               windowSizeInSecs = 0.01,
                                               alpha = 0.16,
                                               lowerFreq = 0,
                                               upperFreq = 5000,
                                               window = "GAUSS",
                                               dynRangeInDB = 70,
                                               audioChannel = 1,
                                               preEmphasisFilterFactor = 0.97,
                                               invert = F){
  
  path2jsFile = file.path(system.file('inst', package='emuR'), 
                          "js", 
                          "spectro-drawing.class.js")
  ct = V8::v8()
  ct$reset()
  ct$source(path2jsFile)
  # create new instance
  ct$eval("let spectro_drawer = new SpectroDrawingClass();")
  
  ado = wrassp::read.AsspDataObj(audioFilePath, 
                                 begin = begin,
                                 end = end)
  
  ct$assign("srXwsis", attr(ado, "sampleRate") * windowSizeInSecs) 
  fftN = as.numeric(ct$eval("spectro_drawer.calcClosestPowerOf2Gt(srXwsis);"))
  # for better resolution set lower limit
  if(fftN < 512){
    fftN = 512
  }
  
  imgWidth = 1920
  imgHeight = 1080
  
  allowedWindows = c("BARTLETT", 
                     "BARTLETTHANN", 
                     "BLACKMAN", 
                     "COSINE", 
                     "GAUSS", 
                     "HAMMING", 
                     "HANN", 
                     "LANCZOS", 
                     "RECTANGULAR", 
                     "TRIANGULAR")
  
  win_idx = match(window, allowedWindows)
  
  if(is.na(win_idx)){
    stop("unsupported window type!")
  }
  
  args = list(
    windowSizeInSecs = windowSizeInSecs,
    fftN = fftN,
    alpha = alpha,
    lowerFreq = lowerFreq,
    upperFreq = upperFreq,
    samplesPerPxl = (nrow(ado$audio) + 1 - 0) / imgWidth , # (end sample index + 1 - start sample index) / imgWidth
    window = 5,
    imgWidth = imgWidth,
    imgHeight = imgHeight,
    dynRangeInDB = dynRangeInDB,
    pixelRatio = 1,
    sampleRate = attr(ado, "sampleRate"),
    transparency = 255,
    audioBuffer = c(rep(0, fftN/2 + 1), as.numeric(ado$audio[,audioChannel]), rep(0, fftN/2 + 1)), # zero pad for first and last spectral slice
    audioBufferChannels = ncol(ado$audio), # this doesn't seem to be used 
    drawHeatMapColors = F,
    preEmphasisFilterFactor = preEmphasisFilterFactor,
    heatMapColorAnchors = list(c(255, 0, 0), c(0, 255, 0), c(0, 0, 0)), # does this map to a matrix?,
    invert = invert
  );
  
  ct$assign("args", args)
  ct$eval("res = spectro_drawer.renderSpectrogram(args);")
  res = ct$get("res")
  
  res_mat = matrix(as.integer(res[seq(1, length(res), 4)])/255, 
                   nrow = imgHeight,
                   ncol = imgWidth, 
                   byrow = T)
  
  
  return(grDevices::as.raster(res_mat))
}

############
# test code
# op = par(mar = rep(0, 4),
#          xaxs = "i",
#          yaxs = "i")
# plot.new()
# plot.window(xlim = c(0, 1),
#             ylim = c(0, 1)
# )
# spect_raster = create_spectrogram_image_as_raster("~/Desktop/emuR_demoData/ae_emuDB/0000_ses/msajc003_bndl/msajc003.wav",
#                                                   begin = 0,
#                                                   end = 2,
#                                                   invert = F)
# rasterImage(spect_raster,
#             0,
#             0,
#             1,
#             1,
#             interpolate = F)  # interpolate
# par(op)
