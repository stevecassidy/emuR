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
##' @param sourceDir directory containing the plain text transcription files and media files
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

convert_txtCollection <- function(dbName,
                                  sourceDir,
                                  targetDir,
                                  txtExtension = 'txt',
                                  mediaFileExtension = 'wav',
                                  transcriptionLevel = "Transcription",
                                  transcriptionLabel = "Transcription",
                                  cleanWhitespaces = TRUE,
                                  verbose = TRUE)
{
  # ---------------------------------------------------------------------------
  # -------------------------- Get directories --------------------------------
  # ---------------------------------------------------------------------------
  
  sourceDir = suppressWarnings(normalizePath(sourceDir))
  targetDir = suppressWarnings(normalizePath(targetDir))
  basePath = file.path(targetDir, paste0(dbName, emuDB.suffix))
  
  if(!dir.exists(targetDir))
  {
    res = try(suppressWarnings(dir.create(targetDir)))
    if (class(res) == "try-error")
    {
      stop("Could not create target directory ", targetDir)
    }
  }
  
  if(dir.exists(basePath))
  {
    stop("emuDB ", basePath, " already exists")
  }
  
  
  # ---------------------------------------------------------------------------
  # -------------------------- Get file pair list ----------------------------
  # ---------------------------------------------------------------------------
  
  filePairList = create_filePairList(sourceDir,
                                     sourceDir,
                                     txtExtension,
                                     mediaFileExtension)
  
  # ---------------------------------------------------------------------------
  # ---------------------------- Initialize dbHandle --------------------------
  # ---------------------------------------------------------------------------

  
  dbHandle = emuDBhandle(dbName, basePath = basePath, uuid::UUIDgenerate(), ":memory:")
  # insert into emuDB table
  queryTxt = paste0("INSERT INTO emu_db (uuid, name) VALUES('", dbHandle$UUID, "', '", dbName,"')")
  DBI::dbGetQuery(dbHandle$connection, queryTxt)
  
  # ---------------------------------------------------------------------------
  # ------------------------ Initialize progress bar --------------------------
  # ---------------------------------------------------------------------------
  
  if (verbose)
  {
    progress = 0
    nbFilePairs = length(filePairList) / 2
    
    cat("INFO: Parsing plain text collection containing", nbFilePairs, "file pair(s)...\n")
    pb = utils::txtProgressBar(
      min = 0, max = nbFilePairs, initial = progress, style = 3
    )
    utils::setTxtProgressBar(pb, progress)
  }
  
  # ---------------------------------------------------------------------------
  # --------------------------- Loop over bundles -----------------------------
  # ---------------------------------------------------------------------------
  
  for (idx in 1:nrow(filePairList)[1])
  {
    # ---------------------------------------------------------------------------
    # ------------------ Get session and bundle names ---------------------------
    # ---------------------------------------------------------------------------
    
    session = get_bpfSession(filePath = filePairList[idx, 2],
                             sourceDir = sourceDir)
    
    txtPath = normalizePath(filePairList[idx, 1], winslash = .Platform$file.sep)
    wavPath = normalizePath(filePairList[idx, 2], winslash = .Platform$file.sep)
    bundle = tools::file_path_sans_ext(basename(wavPath))
    annotates = basename(wavPath)
    
    # Escaping single quotes in anything user-generated that will be fed into SQL
    session = stringr::str_replace_all(session, "'", "''")
    bundle = stringr::str_replace_all(bundle, "'", "''")
    annotates = stringr::str_replace_all(annotates, "'", "''")
    
    # -----------------------------------------------------------------------
    # -------------------------- Get sample rate ----------------------------
    # -----------------------------------------------------------------------
    
    asspObj = wrassp::read.AsspDataObj(filePairList[idx, 2])
    samplerate = attributes(asspObj)$sampleRate
    
    # -----------------------------------------------------------------------
    # --------------- Write session and bundle to temp DB -------------------
    # -----------------------------------------------------------------------
    queryTxt = paste0("SELECT name from session WHERE name='", session, "'")
    all_sessions = DBI::dbGetQuery(dbHandle$connection, queryTxt)
    
    if (!session %in% all_sessions)
    {
      queryTxt = paste0("INSERT INTO session VALUES('", dbHandle$UUID, "', '", session, "')")
      DBI::dbGetQuery(dbHandle$connection, queryTxt)
    }
    
    queryTxt = paste0(
      "INSERT INTO bundle VALUES('", dbHandle$UUID, "', '", session, "', '", bundle, "', '",
      annotates, "', ", samplerate, ", 'NULL')"
    )
    
    DBI::dbGetQuery(dbHandle$connection, queryTxt)
    
    lines = suppressWarnings(readLines(filePairList[idx, 1]))
    
    transcription = paste(lines, collapse = " ")
    transcription = stringr::str_trim(transcription)
    transcription = stringr::str_replace_all(transcription, "'", "''")
    
    if (cleanWhitespaces)
    {
      transcription = stringr::str_replace_all(transcription, "\\s+", " ")
    }
    
    queryTxt = paste0(
      "INSERT INTO items VALUES('",
      dbHandle$UUID,
      "','",
      session,
      "','",
      bundle,
      "',1,'",
      transcriptionLevel,
      "','ITEM',1,",
      samplerate,
      ",NULL, NULL, NULL)"
    )
    DBI::dbGetQuery(dbHandle$connection, queryTxt)
    
    queryTxt = paste0(
      "INSERT INTO labels VALUES('",
      dbHandle$UUID,
      "','",
      session,
      "','",
      bundle,
      "',1,1,'",
      transcriptionLabel,
      "','",
      transcription,
      "')"
    )
    DBI::dbGetQuery(dbHandle$connection, queryTxt)
    
    
    if (verbose)
    {
      utils::setTxtProgressBar(pb, idx)
    }
  }
  if (verbose)
  {
    cat("\n")
  }
  
  
  dbConfig = list(
    name = dbName,
    UUID = dbHandle$UUID,
    mediafileExtension = mediaFileExtension,
    ssffTrackDefinitions = list(),
    levelDefinitions = list(
      list(
        name = transcriptionLevel,
        type = "ITEM",
        attributeDefinitions = list(list(name = transcriptionLabel,
                                         type = "STRING"))
      )
    ),
    linkDefinitions = list(),
    EMUwebAppConfig = list(
      perspectives = list(
        list(
          name = 'default',
          signalCanvases = list(
            order = c("OSCI","SPEC"),
            assign = list(),
            contourLims = list()
          ),
          levelCanvases = list(order = list()),
          twoDimCanvases = list(order = list())
        )
      ),
      activeButtons = list(saveBundle = TRUE,
                           showHierarchy = TRUE)
    )
  )
  
  res = try(dir.create(basePath))
  if (class(res) == "try-error")
  {
    stop("Could not create emuDB base directory ", basePath)
  }
  
  store_DBconfig(dbHandle, dbConfig)
  
  make_bpfDbSkeleton(dbHandle)
  
  copy_bpfMediaFiles(
    basePath = basePath,
    sourceDir = sourceDir,
    mediaFiles = filePairList[,2],
    verbose = verbose
  )
  
  rewrite_allAnnots(dbHandle, verbose = verbose)
}