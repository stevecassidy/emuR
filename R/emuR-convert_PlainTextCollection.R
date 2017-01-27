##' Converts a collection of audio files and plain text transcriptions into an emuDB
##' 
##' This function takes as input pairs of media files (i.e. wav files) and plain text
##' transcriptions files. It creates a new emuDB with one bundle per media file, and
##' turns the associated transcription into an item in that bundle. For this purpose,
##' media files and text files belonging to the same bundle must be named identically
##' (with the exception of their respective file extensions). The newly created
##' emuDB is stored in the target directory, and its handle is returned.
##' 
##' @param dbName name of the new emuDB
##' @param sourceDirTxt directory containing the plain text transcription files
##' @param sourceDirMedia directory containing the media files (may be identical to sourceDirTxt)
##' @param targetDir directory where the new emuDB will be stored
##' @param txtExtension file extension of transcription files
##' @param mediaFileExtension file extension of media files
##' @param transcriptionLevel level name of the transcription level
##' @param transcriptionLabel label name of the transcription items
##' @param cleanWhitespaces if true, any sequence of whitespaces in the transcription (including newlines and tabs)
##' is transformed into a single blank
##' @param verbose display progress bar
##' 
##' @export
##' @seealso convert_BPFCollection, convert_TextGridCollection

convert_PlainTextCollection <- function(dbName,
                                        sourceDirTxt,
                                        sourceDirMedia,
                                        targetDir,
                                        txtExtension = 'txt',
                                        mediaFileExtension = 'wav',
                                        transcriptionLevel = "Transcription",
                                        transcriptionLabel = "Transcription",
                                        cleanWhitespaces = TRUE,
                                        verbose = TRUE)
{
  file_pair_list = create_filePairList(sourceDirMedia, sourceDirTxt, mediaFileExtension, txtExtension)
  
  if(nrow(file_pair_list) == 0)
  {
    stop("Could not find any file pairs in ", sourceDir)
  }
  
  all_sessions = list()
  for(idx in 1:nrow(file_pair_list))
  {
    mediaFile = file_pair_list[idx, 1]
    txtFile = file_pair_list[idx, 2]
    session = get_bpfSession(mediaFile, sourceDirMedia)
    
    mediaDir = normalizePath(dirname(mediaFile), winslash = "/")
    txtDir = normalizePath(dirname(txtFile), winslash = "/")
    
    all_sessions[[session]] = c(mediaDir, txtDir)
  }
  
  create_emuDB(
    name = dbName,
    targetDir = targetDir,
    mediaFileExtension = mediaFileExtension,
    store = TRUE
  )
  handle = load_emuDB(file.path(targetDir, paste0(dbName, emuDB.suffix)))
  
  if (verbose)
  {
    progress = 0
    cat("INFO: Importing media and text files for", length(names(all_sessions)), "sessions ...\n")
    pb = utils::txtProgressBar(
      min = 0,
      max = length(names(all_sessions)),
      initial = progress,
      style = 3
    )
    utils::setTxtProgressBar(pb, progress)
  }
  
  for(session in names(all_sessions))
  {
    import_mediaFiles(handle, all_sessions[[session]][1], targetSessionName = session, verbose = FALSE)
    
    import_transcriptionFiles(
      handle,
      source = all_sessions[[session]][2],
      txtExt = txtExtension,
      transcriptionLevel = transcriptionLevel,
      transcriptionLabel = transcriptionLabel,
      targetSessionName = session,
      verbose = FALSE,
      cleanWhitespaces = cleanWhitespaces,
      rewriteAllAnnots = FALSE,
      changeDBConfig = FALSE
    )
    
    if (verbose)
    {
      progress = progress + 1
      utils::setTxtProgressBar(pb, progress)
    }
  }
  
  if(verbose)
  {
    cat("\n")
  }
  
  add_levelDefinition(handle, transcriptionLevel, "ITEM", verbose = FALSE, rewriteAllAnnot = FALSE)
  if (transcriptionLabel != transcriptionLevel) {
    add_attributeDefinition(handle, transcriptionLevel, transcriptionLabel, verbose = FALSE)
  }
  
  rewrite_allAnnots(handle, verbose = verbose)
  
  return(handle)
}



import_transcriptionFiles <- function(handle,
                                      source,
                                      txtExt = 'txt',
                                      transcriptionLevel = "Transcription",
                                      transcriptionLabel = "Transcription",
                                      cleanWhitespaces = TRUE,
                                      rewriteAllAnnots = TRUE,
                                      changeDBConfig = TRUE,
                                      targetSessionName = "0000",
                                      verbose = TRUE)
{
  source = suppressWarnings(normalizePath(source))
  
  if (!is.null(get_levelDefinition(handle, transcriptionLevel)))
  {
    stop("There is already a level ", transcriptionLevel)
  }
  
  if (!is.null(get_levelNameForAttributeName(handle, transcriptionLabel)))
  {
    stop("There is already a level with an attribute called ",
         transcriptionLabel)
  }
  
  txtList = list.files(source, pattern = paste0(".*[.]", txtExt, "$"))
  
  if (length(txtList) > 0)
  {
    if (verbose)
    {
      progress = 0
      cat("INFO: Importing ",
          length(txtList),
          " transcriptions into session ", targetSessionName, " ...\n")
      pb = utils::txtProgressBar(
        min = 0,
        max = length(txtList),
        initial = progress,
        style = 3
      )
      utils::setTxtProgressBar(pb, progress)
    }
    
    if (length(txtList) != nrow(list_bundles(handle, targetSessionName)))
    {
      stop()
    }
    
    for (txtfile in txtList)
    {
      path = file.path(source, txtfile)
      bundle = sub('[.][^.]*$', '', txtfile)
      session = targetSessionName
      
      samplerate = bas_get_samplerate(handle, session, bundle)
      
      transcription = paste(readLines(path), collapse = " ")
      transcription = stringr::str_trim(transcription)
      transcription = stringr::str_replace_all(transcription, "'", "''")
      
      if (cleanWhitespaces)
      {
        transcription = stringr::str_replace_all(transcription, "\\s+", " ")
      }
      
      item_id = bas_get_max_id(handle, session, bundle) + 1
      queryTxt = paste0(
        "INSERT INTO items VALUES('",
        handle$UUID,
        "','",
        session,
        "','",
        bundle,
        "',",
        item_id ,
        ",'",
        transcriptionLevel,
        "','ITEM',1,",
        samplerate,
        ",NULL, NULL, NULL)"
      )
      DBI::dbGetQuery(handle$connection, queryTxt)
      
      queryTxt = paste0(
        "INSERT INTO labels VALUES('",
        handle$UUID,
        "','",
        session,
        "','",
        bundle,
        "',",
        item_id,
        ",1,'",
        transcriptionLabel,
        "','",
        transcription,
        "')"
      )
      DBI::dbGetQuery(handle$connection, queryTxt)
      
      if (verbose)
      {
        progress = progress + 1
        utils::setTxtProgressBar(pb, progress)
      }
    }
  }
  
  if(changeDBConfig)
  {
    add_levelDefinition(handle, transcriptionLevel, "ITEM", verbose = FALSE, rewriteAllAnnots = FALSE)
    if (transcriptionLabel != transcriptionLevel) {
      add_attributeDefinition(handle, transcriptionLevel, transcriptionLabel, verbose = FALSE, rewriteAllAnnots = FALSE)
    }
  }
  
  if(rewriteAllAnnots)
  {
    rewrite_allAnnots(handle, verbose = verbose)
  }
}