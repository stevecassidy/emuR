play_segs <- function(emuDBhandle, seglist){
  
  # for(row_idx in 1:nrow(seglist)){
  #   
  #   cur_row = seglist[row_idx,]
  #   
  #   # get audio and write to temp folder
  #   audio = wrassp::read.AsspDataObj(file.path(emuDBhandle$basePath, 
  #                                              paste0(cur_row$session, session.suffix), 
  #                                              paste0(cur_row$bundle, bundle.dir.suffix),
  #                                              paste0(cur_row$bundle, ".wav")), 
  #                                    begin = as.numeric(cur_row$sample_start),
  #                                    end = as.numeric(cur_row$sample_end),
  #                                    samples = T)
  #   
  #   wrassp::write.AsspDataObj(audio, file.path(tempdir(), "cur_play_segs.wav"))
  #   
  #   # generate spectrogram
  #   
  #   # set parameters for dftSpectrum function
  #   params = formals(wrassp::dftSpectrum)
  #   params$fftLength = 512
  #   params$windowShift = 1 # in ms
  #   params$bandwidth = 100 # == 0.01 secs
  #   params$window = "GAUSS2_5"
  #   
  #   dft_vals = get_trackdata(emuDBhandle = emuDBhandle,
  #                            cur_row,
  #                            onTheFlyFunctionName = "dftSpectrum",
  #                            onTheFlyParams = params,
  #                            verbose = F,
  #                            resultType = "tibble")
  #   
  #   
  #   # select data columns
  #   dft_vals_tracks = dft_vals %>%
  #     select(matches("^T[0-9]+")) %>%
  #     as.matrix()
  #   
  #   # normalize trackdata to values between 0 and 1 (for each row)
  #   # apply also transposed matrix
  #   # '1 - ' to give peaks high values (== more black in raster plot)
  #   dft_vals_tracks_norm_transp = apply(dft_vals_tracks, 1, function(dft_row) {
  #     1 - ((dft_row - min(dft_row)) / (max(dft_row) - min(dft_row)))
  #   })
  #   
  #   # flip so low Hz values are at the bottom
  #   td_tracks_norm_transp_flip = apply(dft_vals_tracks_norm_transp, 2, rev)
  #   
  #   jpeg(file = file.path(tempdir(), "cur_play_segs.jpg"))
  #   plot(as.raster(td_tracks_norm_transp_flip, max=1))
  #   
  #   title(main = paste("seglist row entry", row_idx, sep = " "),
  #         sub = paste(cur_row, collapse = "; ")) # 1 is boundary 0 is non boundary
  #   dev.off()
  #   
  #   
  #   viewer <- getOption("viewer")
  #   # if (!is.null(viewer)){x
  #   file.copy("~/Developer/emuR/R/play_segs.html", file.path(tempdir(), "play_segs.html"), overwrite = T)
  #   # viewer(file.path(tempdir(), "play_segs.html"), height = 500)
  #   # }else{
  #   utils::browseURL(file.path(tempdir(), "play_segs.html"))
  #   # }
  #   
  #   input_key <- readline(prompt="Press any key to continue (press c to cancel): ")
  #   if(input_key == "c") break
  #   
  #   
  # }
  
}