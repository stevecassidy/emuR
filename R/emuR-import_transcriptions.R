##' 
##' 
##' @export
##' @seealso

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
  
  add_levelDefinition(handle, transcriptionLevel, "ITEM", verbose = verbose)
  if (transcriptionLabel != transcriptionLevel) {
    add_attributeDefinition(handle, transcriptionLevel, transcriptionLabel, verbose = verbose)
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
    
    bas_prepare(handle)
    
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
        "INSERT INTO ",
        BAS_ITEMS_TMP_TABLE,
        " VALUES('",
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
        "INSERT INTO ",
        BAS_LABELS_TMP_TABLE,
        " VALUES('",
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
  bas_clear(handle)
  
  if(changeDBConfig)
  {
    add_levelDefinition(handle, transcriptionLevel, "ITEM", verbose = verbose)
    if (transcriptionLabel != transcriptionLevel) {
      add_attributeDefinition(handle, transcriptionLevel, transcriptionLabel, verbose = verbose)
    }
  }
  
  if(rewriteAllAnnots)
  {
    rewrite_allAnnots(handle, verbose = verbose)
  }
}