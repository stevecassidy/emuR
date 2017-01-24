import_transcription_files <- function(handle,
                                       source,
                                       txtExt = 'txt',
                                       transcriptionLevel = "Transcription",
                                       transcriptionLabel = "Transcription",
                                       cleanWhitespaces = TRUE,
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
          " transcriptions...\n")
      pb = utils::txtProgressBar(
        min = 0,
        max = length(txtList),
        initial = progress,
        style = 3
      )
      utils::setTxtProgressBar(pb, progress)
    }
    
    if (length(txtList) != nrow(list_bundles(handle)))
    {
      stop()
    }
    
    bas_prepare(handle)
    
    for (txtfile in txtList)
    {
      path = file.path(source, txtfile)
      bundle = sub('[.][^.]*$', '', txtfile)
      
      queryTxt = paste0("SELECT session, name, sample_rate FROM bundle WHERE name=='", bundle, "'")
      res = DBI::dbGetQuery(handle$connection, queryTxt)
      if (nrow(res) == 0)
      {
        stop()
      }
      
      if(nrow(res) > 1)
      {
        stop()
      }
      
      session = res[1, "session"]
      bundle = res[1, "name"]
      samplerate = res[1, "sample_rate"]
      
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
  
  add_levelDefinition(handle, transcriptionLevel, "ITEM")
  if (transcriptionLabel != transcriptionLevel) {
    add_attributeDefinition(handle, transcriptionLevel, transcriptionLabel)
  }
}