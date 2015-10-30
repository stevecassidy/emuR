require(RSQLite)

##' Convert a Bas Partitur File Collection (audio and BAS Partitur files) to an emuDB
##' 
##' Converts a Bas Partitur File Collection to an emuDB. Expects a collection of the following form:
##' One master directory <targetDir> containing any number of file pairs (= bundles). A file pair 
##' consists of an audio file with the extension <audioExt> and a BPF file with the extension <bpfExt>. 
##' Apart from extensions, the names of corresponding audio and BPF files must be identical. Each BPF 
##' file is converted into an emuDB annot file. An emuDB config file matching the data base is created 
##' after parsing.
##' 
##' @param sourceDir path to the directory containing the Bas Partitur File collection
##' @param targetDir directory where the new emuDB should be saved
##' @param dbName name given to the new emuDB
##' 
##' @param bpfExt extension of BPF files (default = "par")
##' @param audioExt extension of audio files (default = "wav")
##' 
##' @param extractLevels optional vector containing the names of levels that should be extracted. If NULL (the default) all levels found in the BPF collection are extracted.
##' @param refLevel optional name of level used as reference for symbolic links. If NULL (the default), a link-less data base is created.
##' @param unifyLevels optional vector containing names of levels to be unified with the reference level. This means that they are treated as labels of the reference level rather than independent items. At the moment, only purely symbolic (class 1) levels can be unified. Links between the reference level and levels in unifyLevels must be one-to-one.
##' 
##' @param newLevels optional vector containing names of levels in the BPF collection that are not part of the standard BPF levels. See http://www.bas.uni-muenchen.de/forschung/Bas/BasFormatseng.html#Partitur_tiersdef for details on standard BPF levels.
##' @param newLevelClasses optional vector containing the classes of levels in the newLevels vector as integers. Must have the same length and order as newLevels.
##' 
##' @param segmentToEventLevels optional vector containing names of segment levels with overlapping segments. The parser treats segments on these levels as events (SEGMENT xyz becomes EVENT xyz_start and EVENT xyz_end). If a level contains segmental overlap but is not in this vector, the parser will throw an error. If overlap resolution leads to event overlap (e.g. if one segment's end coincides with the next segment's start), an error is thrown either way. If in doubt whether a level contains segmental overlap, try running the converter with segmentToEventLevels = NULL and see whether an error occurs.
##'
##' @param verbose display infos, warnings and show progress bar
##' 
##' @import RSQLite
##' @export
##' @author Nina Pörner

convert_BPFCollection_to_emuDB <- function(sourceDir,
                                           targetDir,
                                           dbName,
                                           bpfExt = 'par',
                                           audioExt = 'wav',
                                           extractLevels = NULL,
                                           refLevel = NULL,
                                           newLevels = NULL,
                                           newLevelClasses = NULL,
                                           segmentToEventLevels = NULL,
                                           unifyLevels = NULL,
                                           verbose = TRUE)
{
  # ---------------------------------------------------------------------------
  # ------------------------ Standard BPF levels ------------------------------
  # ---------------------------------------------------------------------------
  
  # To add a new level type to the BPF format:
  
  # - add new level name (three character string) to STANDARD_LEVELS
  # - add its class (integer in range 1-5) to STANDARD_LEVEL_CLASSES
  # - the order of both vectors must match (i.e. if you add the name at position 10, add the class at position 10 as well)
  
  # If you do not wish to extend the format directly in the source code, use newLevels and newLevelClasses arguments.
  
  STANDARD_LEVELS = c(
    "KAN", "KAS", "PTR", "ORT", "TRL", "TR2", "SUP", "DAS", "PRS", "NOI", 
    "POS", "LMA", "TRS", "TRW", "PRO", "SYN", "FUN", "LEX", "TLN",
    "IPA", "GES", "USH", "USM", "OCC",
    "PRM", "LBG", "LBP",
    "SAP", "MAU", "WOR", "PHO", "MAS", "USP", "TRN",
    "PRB"
  )
  
  STANDARD_LEVEL_CLASSES = c(
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
    1, 1, 1, 1, 1, 1, 1, 1, 1,
    2, 2, 2, 2, 2,
    3, 3, 3,
    4, 4, 4, 4, 4, 4, 4,
    5
  )
  
  # ---------------------------------------------------------------------------
  # ---------------------- Default session name -------------------------------
  # ---------------------------------------------------------------------------
  
  SESSION = "0000"
  
  # ---------------------------------------------------------------------------
  # -------------------------- Get directories --------------------------------
  # ---------------------------------------------------------------------------
  
  sourceDir = suppressWarnings(normalizePath(sourceDir))
  targetDir = suppressWarnings(normalizePath(targetDir))
  
  # ---------------------------------------------------------------------------
  # --------------- First round of argument checks ----------------------------
  # ---------------------------------------------------------------------------
  
  bpf_argument_checks_without_level_classes(sourceDir = sourceDir,
                                            targetDir = targetDir,
                                            dbName = dbName,
                                            newLevels = newLevels,
                                            newLevelClasses = newLevelClasses,
                                            standardLevels = STANDARD_LEVELS,
                                            verbose = verbose,
                                            refLevel = refLevel,
                                            audioExt = audioExt,
                                            extractLevels = extractLevels)
  
  # ---------------------------------------------------------------------------
  # ---------------- Combine standard and new level classes -------------------
  # ---------------------------------------------------------------------------
  
  levelClasses = as.list(STANDARD_LEVEL_CLASSES)
  names(levelClasses) = STANDARD_LEVELS
  levelClasses[newLevels] = newLevelClasses
  
  # ---------------------------------------------------------------------------
  # ---------------------- Second round of argument checks --------------------
  # ---------------------------------------------------------------------------
  
  bpf_argument_checks_with_level_classes(unifyLevels = unifyLevels,
                                         refLevel = refLevel,
                                         extractLevels = extractLevels,
                                         levelClasses = levelClasses,
                                         segmentToEventLevels)
  
  # ---------------------------------------------------------------------------
  # -------------------------- Get file pair list ----------------------------
  # ---------------------------------------------------------------------------

  filePairList = create_filePairList(sourceDir, sourceDir, bpfExt, audioExt)
  
  # ---------------------------------------------------------------------------
  # ------------------------ Initialize temporary data base -------------------
  # ---------------------------------------------------------------------------
  
  dbUUID = UUIDgenerate()
  .initialize.DBI.database(get_emuDBcon())
  
  # ---------------------------------------------------------------------------
  # ------------------------ Initialize progress bar --------------------------
  # ---------------------------------------------------------------------------
  
  if(verbose)
  {
    progress = 0
    counter = 0
    nbFilePairs = length(filePairList) / 2
    
    cat("INFO: Parsing BPF collection containing", nbFilePairs, "file pair(s)...\n")
    pb = txtProgressBar(min = 0, max = nbFilePairs, initial = progress, style=3)
    setTxtProgressBar(pb, progress)
  }
  
  # ---------------------------------------------------------------------------
  # -------------------- Initialize container variables -----------------------
  # ---------------------------------------------------------------------------
  
  # "Tracker" containers for levels and links in the parsed BPFs. Used for config file and post-processing.
  levelTracker = list()
  linkTracker = list()
  
  # Tracker for warnings produced by the parser. Warnings displayed in batch after the whole collection has been parsed.
  warningsTracker = list(semicolonFound = list())
  
  # ---------------------------------------------------------------------------
  # ------------------------------------- Parsing -----------------------------
  # ---------------------------------------------------------------------------
  
  # -------------------------------------------------------------------------
  # ----------------------- Write session to temp DB ------------------------
  # -------------------------------------------------------------------------
    
  queryTxt = paste0("INSERT INTO session VALUES('", dbUUID, "', '", SESSION, "')")
  dbSendQuery(get_emuDBcon(), queryTxt)
    
  for(idx in 1:dim(filePairList)[1])
  {
    bundle = basename(file_path_sans_ext(normalizePath(filePairList[idx, 2], winslash = .Platform$file.sep)))
    bundleBasename = basename(filePairList[idx, 2])
    mediaPath = filePairList[idx, 2]
    bpfPath = filePairList[idx, 1]
    
    # Escaping single quotes in anything user-generated that will be put into the SQL db
    bundle = str_replace_all(bundle, "'", "''")
    bundleBasename = str_replace_all(bundleBasename, "'", "''")
    mediaPath = str_replace_all(mediaPath, "'", "''")
    
    # -----------------------------------------------------------------------
    # -------- Get sample rate for comparison with info in BPF header -------
    # -----------------------------------------------------------------------
      
    asspObj = read.AsspDataObj(filePairList[idx, 2])
    samplerate = attributes(asspObj)$sampleRate
      
    # -----------------------------------------------------------------------
    # ---------------- Write track and  bundle to temp DB -------------------
    # -----------------------------------------------------------------------
      
    queryTxt = paste0("INSERT INTO track VALUES('", dbUUID, "', '", SESSION, "', '", bundle, "', '",
                      mediaPath, "')")
    dbGetQuery(get_emuDBcon(), queryTxt)
    
    queryTxt = paste0("INSERT INTO bundle VALUES('", dbUUID, "', '", SESSION, "', '", bundle, "', '",
                      bundleBasename, "', ", samplerate, ",'", mediaPath, "', 'NULL')")
    dbSendQuery(get_emuDBcon(), queryTxt)
      
    # -----------------------------------------------------------------------
    # ------------------------------ Parse BPF ------------------------------
    # -----------------------------------------------------------------------
      
    # returnContainer is a container for BPF's info about levels (returnContainer$levelInfo) and
    # links (returnContainer$linkInfo) found in the BPF.
      
    returnContainer = parse_BPF(bpfPath = bpfPath,
                                dbName = dbName, 
                                bundle = bundle, 
                                dbUUID = dbUUID,
                                session = SESSION,
                                refLevel = refLevel, 
                                extractLevels = extractLevels,
                                samplerate = samplerate, 
                                segmentToEventLevels = segmentToEventLevels, 
                                levelClasses = levelClasses,
                                unifyLevels = unifyLevels)
    
    levelInfo = returnContainer$levelInfo
    linkInfo = returnContainer$linkInfo
    semicolonFound = returnContainer$semicolonFound
      
    # -----------------------------------------------------------------------
    # --------------------- Update tracker variables ------------------------
    # -----------------------------------------------------------------------
      
    if(semicolonFound)
    {
      warningsTracker$semicolonFound[[length(warningsTracker$semicolonFound) + 1L]] = bpfPath
    }

    if(length(levelInfo) > 0)
    {
      levelTracker = bpf_update_level_tracker(levelInfo = levelInfo,
                                              levelTracker = levelTracker)
    }
      
    if(length(linkInfo) > 0)
    {
      linkTracker = bpf_update_link_tracker(linkInfo = linkInfo,
                                            linkTracker = linkTracker)
    }
      
    # -----------------------------------------------------------------------
    # -------------------------- Update progress bar ------------------------
    # -----------------------------------------------------------------------
      
    if(verbose)
    {
      counter = counter + 1
      setTxtProgressBar(pb, counter)
    }
  }
  
  # ---------------------------------------------------------------------------
  # ----------------------------- Post-processing -----------------------------
  # ---------------------------------------------------------------------------
  
  if(verbose)
  {
    cat("\n")
    cat("INFO: Doing some post-processing...\n")
  }
  
  # ---------------------------------------------------------------------------
  # ---------------- Resolve link type and direction ambiguities --------------
  # ---------------------------------------------------------------------------


  if(length(linkTracker) > 0)
  {
    linkTracker = bpf_link_disambiguation(linkTracker = linkTracker,
                                          refLevel = refLevel)
  }

  # ---------------------------------------------------------------------------
  # ----- Link from Utterance level to refLevel and levels above refLevel -----
  # ---------------------------------------------------------------------------
  
  if(length(linkTracker) > 0)
  {
    linkTracker = bpf_link_utterance_level(linkTracker = linkTracker,
                                           refLevel = refLevel)
  }
  
  # ---------------------------------------------------------------------------
  # ---------------------- Create dbConfig Schema -----------------------------
  # ---------------------------------------------------------------------------
  
  if(verbose)
  {
    cat("INFO: Creating EMU database config schema...\n")
  }
  
  dbSchema = bpf_create_schema(levelTracker = levelTracker,
                               linkTracker = linkTracker,
                               dbName = dbName,
                               dbUUID = dbUUID,
                               audioExt = audioExt)
  
  # ---------------------------------------------------------------------------
  # -------------------- Create, store and purge temp DB ----------------------
  # ---------------------------------------------------------------------------
  
  db=create.database(name = dbName, 
                     basePath = normalizePath(targetDir), 
                     DBconfig = dbSchema)
  
  db[['DBconfig']][['EMUwebAppConfig']][['activeButtons']]=list(saveBundle=TRUE,
                                                                showHierarchy=TRUE)
  
  .store.emuDB.DBI(con = get_emuDBcon(), database = db)
  store(dbName, targetDir, showProgress = verbose)
  .purge.emuDB(dbUUID)
  
  # ---------------------------------------------------------------------------
  # -------------- Display any warnings collected during parsing --------------
  # ---------------------------------------------------------------------------
  
  if(verbose)
  {
    bpf_display_semicolon_warnings(warningsTracker)
  }
}











# -----------------------------------------------------------------------------
# --------------------------- HELPER FUNCTIONS --------------------------------
# -----------------------------------------------------------------------------

###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################

## First round of argument checks
## 
## @param sourceDir
## @param targetDir
## @param dbName
## @param newLevels
## @param newLevelClasses
## @param standardLevels
## @param verbose
## @param refLevel
## @param audioExt
## @param extractLevels
## @keywords emuR BPF Emu
## @return

bpf_argument_checks_without_level_classes <- function(sourceDir,
                                                      targetDir,
                                                      dbName,
                                                      newLevels,
                                                      newLevelClasses,
                                                      standardLevels,
                                                      verbose,
                                                      refLevel,
                                                      audioExt,
                                                      extractLevels)
{
  if(!file.exists(sourceDir))
  {
    stop("Source directory does not exist!")
  }
  
  if(!file.exists(targetDir))
  {
    stop("Target directory does not exist!")
  }
  
  if(file.exists(file.path(targetDir, dbName)))
  {
    stop('The directory ', file.path(targetDir, dbName), ' already exists. Can not generate a new emuDB here.')
  }
  
  if(length(newLevels) != length(newLevelClasses))
  {
    stop("Length of newLevels and newLevelClasses must be identical.")
  }
  
  if(!all(newLevelClasses %in% c(1,2,3,4,5)))
  {
    stop("Level classes must be integers between 1 and 5. See BPF specifications for details.")
  }
  
  if(any(newLevels %in% standardLevels))
  {
    stop("You cannot introduce a standard BPF level via the newLevels argument. ",
         "Standard BPF levels are: '", paste(standardLevels, collapse = "', '"), "'")
  }
  
  if(is.null(refLevel) && verbose)
  {
    ans = readline("WARNING: No reference level has been declared. EMU database will be built without any symbolic links. Do you wish to continue? (y/n)")
    if(!ans == "y")
    {
      stop("BPF converter interrupted.")
    }
  }
  
  if(!is.null(extractLevels))
  {
    if(!is.null(refLevel))
    {
      if(!refLevel %in% extractLevels)
      {
        stop("Reference level is not in extractLevels")
      }
    }
  }
}

###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################

## Second round of argument checks
## 
## @param unifyLevels
## @param refLevel
## @param extractLevels
## @param levelClasses
## @param segmentToEventLevels
## @keywords emuR BPF Emu
## @return 

bpf_argument_checks_with_level_classes <- function(unifyLevels,
                                                   refLevel,
                                                   extractLevels,
                                                   levelClasses,
                                                   segmentToEventLevels)
{
  for(level in c(unifyLevels, refLevel, extractLevels))
  {
    if(!level %in% names(levelClasses))
    {
      stop("Unknown level: ", level, ". If this is not a standard BPF level you need to declare this level via the newLevels argument, and assign it a class via the newLevelClasses argument")
    }
  }
  
  # Throw an exception if a link-less level is made reference level.
  if(!is.null(refLevel))
  {
    if(levelClasses[[refLevel]] %in% c(2, 3))
    {
      stop("Link-less level ", refLevel, " cannot be reference level.")
    }
  }
  
  if(!is.null(unifyLevels))
  {
    if(is.null(refLevel))
    {
      stop("If you want to unify levels with the reference level, you must declare a reference level.")
    }
    if(refLevel %in% unifyLevels)
    {
      stop("Reference level cannot be unified with itself.")
    }
    if(any(levelClasses[unifyLevels] != 1))
    {
      stop("Levels to be unified with the reference level must be of class 1 (time-less).")
    }
  }
  
  if(any(!levelClasses[segmentToEventLevels] %in% c(2,4)))
  {
    stop("Only segment levels (classes 2 and 4) can be listed in segmentToEventLevels.")
  }
}


###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################

## Update level tracker with information from parsing process
## 
## @param levelInfo
## @param levelTracker
## @keywords emuR BPF Emu
## @return levelTracker

bpf_update_level_tracker <- function(levelInfo,
                                     levelTracker)
{
  for(idx in 1:length(levelInfo))
  {
    found = FALSE
    if(length(levelTracker) > 0)
    {
      for(jdx in 1:length(levelTracker))
      {
        if(levelTracker[[jdx]][["key"]] == levelInfo[[idx]][["key"]] &&
            levelTracker[[jdx]][["type"]] == levelInfo[[idx]][["type"]])
        {
          for(label in levelInfo[[idx]][["labels"]])
          {
            if(!label %in% levelTracker[[jdx]][["labels"]])
            {
              levelTracker[[jdx]][["labels"]][[length(levelTracker[[jdx]][["labels"]]) + 1L]] = label
            }
          }
          found = TRUE
          break
        }
      }
    }
    
    if(!found)
    {
      levelTracker[[length(levelTracker) + 1L]] = levelInfo[[idx]]
    }
  }
  
  return(levelTracker)
}

###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################

## Update link tracker with information from parsing process
## 
## @param linkTracker
## @param linkInfo
## @keywords emuR BPF Emu
## @return linkTracker

bpf_update_link_tracker <- function(linkTracker,
                                    linkInfo)
{
  for(jdx in 1:length(linkInfo))
  {
    found = FALSE
    if(length(linkTracker) > 0)
    {
      for(kdx in 1:length(linkTracker))
      {
        if(linkTracker[[kdx]][["fromkey"]] == linkInfo[[jdx]][["fromkey"]] &&
           linkTracker[[kdx]][["tokey"]] == linkInfo[[jdx]][["tokey"]] &&
           linkTracker[[kdx]][["type"]] == linkInfo[[jdx]][["type"]])
        {
          found = TRUE
          linkTracker[[kdx]][["countRight"]] = linkTracker[[kdx]][["countRight"]] + linkInfo[[jdx]][["countRight"]]
          linkTracker[[kdx]][["countWrong"]] = linkTracker[[kdx]][["countWrong"]] + linkInfo[[jdx]][["countWrong"]]
          break
        }
      }
    }
    
    if(!found)
    {
      linkTracker[[length(linkTracker) + 1L]] = linkInfo[[jdx]]
    }
  }
  
  return(linkTracker)
}

###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################

## Disambiguate link directions and types in case individual BPFs did not agree on them
## 
## @param linkTracker
## @param refLevel
## @keywords emuR BPF Emu
## @return list(linkTracker)

bpf_link_disambiguation <- function(linkTracker, 
                                    refLevel)
{
  # ------------------------------- THE PROBLEM -------------------------------
  #
  # Individual BPFs might not agree on the direction of certain links.
  #
  # EXAMPLE: There is a BPF collection with two levels: "ORT" at the word level (reference level), 
  # and "TRN" at the turn level. In any BPF where there are at least two words per turn, 
  # the parser will recognize "TRN" as being hierarchically higher.
  # But if there is one BPF with only one word per turn (i.e. a local ONE_TO_ONE relationship),
  # the parser will by default put the reference level on top. Thus, we have conflicting link
  # directions in the SQL data base, and conflicting entries in the linkTracker:
  #
  # fromkey = "ORT", tokey = "TRN", type = ..., countRight = ..., countWrong = ...
  # fromkey = "TRN", fromkey = "ORT", type = ..., countRight = ..., countWrong = ...
  #
  # Individual BPFs might also not agree on the types of links.
  # EXAMPLE: links between words ("ORT") and syllables ("MAS") might be ONE_TO_MANY in the collection as a whole,
  # but in a bundle with only monosyllabic words the parser will have interpreted them as ONE_TO_ONE.
  # In this case, we would have two conflicting pieces of information in the link tracker:
  #
  # fromkey = "ORT", tokey = "MAS", type = "ONE_TO_ONE"
  # fromkey = "ORT", tokey = "MAS", type = "ONE_TO_MANY"

  # ---------------------------------------------------------------------------
  # --- Collect pairs of levels between which links have to be turned around --
  # ---------------------------------------------------------------------------
  
  turnAround = bpf_get_turn_around(linkTracker = linkTracker)
  
  # ---------------------------------------------------------------------------
  # --------------------- Set countRight & countWrong to NA -------------------
  # ---------------------------------------------------------------------------
  
  # (we don't need them anymore and they interfere with the unique() function)
  
  for(idx in 1:length(linkTracker))
  {
    linkTracker[[idx]][["countRight"]] = NA
    linkTracker[[idx]][["countWrong"]] = NA
  }
  
  # ---------------------------------------------------------------------------
  # ---- Turn the links from turnAround in the temp DB and the link tracker ---
  # ---------------------------------------------------------------------------
  
  if(length(turnAround) > 0)
  {
    bpf_turn_links(turnAround = turnAround)
    linkTracker = bpf_turn_link_tracker_entries(turnAround = turnAround,
                                                linkTracker = linkTracker)
  }
  
  # ---------------------------------------------------------------------------
  # ------------------------------ Merge link types ---------------------------
  # ---------------------------------------------------------------------------
  
  linkTracker = bpf_merge_link_types(linkTracker = linkTracker)

  # ---------------------------------------------------------------------------
  # -------------- Return link tracker to caller function ---------------------
  # ---------------------------------------------------------------------------
  
  return(linkTracker)
}

###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################

## Get a list of all links (as fromkey - tokey pairs) that need to be turned around
## 
## @param linkTracker
## @keywords emuR BPF Emu
## @return turnAround

bpf_get_turn_around <- function(linkTracker)
{
  turnAround = list()
  
  for(idx in 1:length(linkTracker))
  {
    # Boolean that's switched on when encountering a link tracker entry whose direction is the reverse of the current one's. 
    # If there isn't one, there is no direction issue, and therefore no turnAround entry.
    turnAroundNecessary = FALSE
    
    # countRight and countWrong for the current entry's direction.
    countRight = 0
    countWrong = 0
    
    for(jdx in 1:length(linkTracker))
    {
      if(linkTracker[[idx]][["fromkey"]] == linkTracker[[jdx]][["fromkey"]] &&
         linkTracker[[idx]][["tokey"]] == linkTracker[[jdx]][["tokey"]])
      {
        countRight = countRight + linkTracker[[jdx]][["countRight"]]
        countWrong = countWrong + linkTracker[[jdx]][["countWrong"]]
      }
      
      else if(linkTracker[[idx]][["fromkey"]] == linkTracker[[jdx]][["tokey"]] &&
              linkTracker[[idx]][["tokey"]] == linkTracker[[jdx]][["fromkey"]])
      {
        countRight = countRight + linkTracker[[jdx]][["countWrong"]]
        countWrong = countWrong + linkTracker[[jdx]][["countRight"]]
        turnAroundNecessary = TRUE
      }
    }
    
    if(turnAroundNecessary)
    {
      # -----------------------------------------------------------------------
      # ----------------- Evaluate countRight and countWrong ------------------
      # -----------------------------------------------------------------------
      
      # If countRight ends up greater, the direction of the current link tracker entry "wins".
      if(countRight > countWrong)
      {
        turnAround[[length(turnAround) + 1L]] = 
          list(fromkey = linkTracker[[idx]][["tokey"]], tokey = linkTracker[[idx]][["fromkey"]])
      }
      
      # If countWrong ends up greater, the reverse direction "wins".
      else if(countRight < countWrong)
      {
        turnAround[[length(turnAround) + 1L]] = 
          list(fromkey = linkTracker[[idx]][["fromkey"]], tokey = linkTracker[[idx]][["tokey"]])
      }
      
      # Special case: If countRight == countWrong, we could get mirror entries in turnAround.
      # Therefore, check whether the reverse of a given entry exists in turnAround. 
      # If it does, don't add the current entry.
      else if(countRight == countWrong)
      {
        found = FALSE
        for(link in turnAround)
        {
          if(link$fromkey == linkTracker[[idx]][["tokey"]] && link$tokey == linkTracker[[idx]][["fromkey"]])
          {
            found = TRUE
            break
          }
        }
        
        if(!found)
        {
          turnAround[[length(turnAround) + 1L]] = 
            list(fromkey = linkTracker[[idx]][["fromkey"]], tokey = linkTracker[[idx]][["tokey"]])
        }
      }
    }
  }
  
  # Remove duplicates from turnAround.
  turnAround = unique(turnAround)
  
  return(turnAround)
}

###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################

## Turn around eligible links in the temp DB
## 
## @param turnAround
## @keywords emuR BPF Emu
## @return

bpf_turn_links <- function(turnAround)
{
  for(link in turnAround)
  {
    queryTxt = paste0("UPDATE links SET fromID = toID, toID = fromID WHERE fromID IN",
                      "(SELECT itemID FROM items WHERE level = '", link[["fromkey"]], 
                      "' AND db_uuid = links.db_uuid AND session = links.session AND bundle = links.bundle) ",
                      "AND toID IN(SELECT itemID FROM items WHERE level = '", link[["tokey"]], "' ",
                      "AND db_uuid = links.db_uuid AND session = links.session AND bundle = links.bundle);")
    dbSendQuery(get_emuDBcon(), queryTxt)
  }
}

###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################

## Turn around eligible entries in the link tracker
## 
## @param turnAround
## @param linkTracker
## @keywords emuR BPF Emu
## @return linkTracker
  
bpf_turn_link_tracker_entries <- function(turnAround = turnAround,
                                          linkTracker = linkTracker)
{
  for(idx in 1:length(turnAround))
  {
    for(jdx in 1:length(linkTracker))
    {
      if(turnAround[[idx]][["fromkey"]] == linkTracker[[jdx]][["fromkey"]] &&
         turnAround[[idx]][["tokey"]] == linkTracker[[jdx]][["tokey"]])
      {
        linkTracker[[jdx]][["fromkey"]] = turnAround[[idx]][["tokey"]]
        linkTracker[[jdx]][["tokey"]] = turnAround[[idx]][["fromkey"]]
        
        # If an entry of type ONE_TO_MANY is turned around, it becomes MANY_TO_MANY (as MANY_TO_ONE is not an option in emuR)
        if(linkTracker[[jdx]][["type"]] == "ONE_TO_MANY")
        {
          linkTracker[[jdx]][["type"]] = "MANY_TO_MANY"
        }
      }
    }
  }
  return(linkTracker)
}

###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################

## Merge link types in linkTracker
## 
## @param linkTracker
## @keywords emuR BPF Emu
## @return linkTracker

bpf_merge_link_types <- function(linkTracker)
{
  for(idx in 1:length(linkTracker))
  {
    for(jdx in 1:length(linkTracker))
    {
      if(linkTracker[[idx]][["fromkey"]] == linkTracker[[jdx]][["fromkey"]] &&
         linkTracker[[idx]][["tokey"]] == linkTracker[[jdx]][["tokey"]])
      {
        if(linkTracker[[idx]][["type"]] %in% c("ONE_TO_ONE", "ONE_TO_MANY") &&
           linkTracker[[jdx]][["type"]] %in% c("ONE_TO_MANY", "MANY_TO_MANY"))
        {
          linkTracker[[idx]][["type"]] = linkTracker[[jdx]][["type"]]
        }
      }
    }
  }
  
  # Remove duplicates created in the merging process.
  linkTracker = unique(linkTracker)
  
  return(linkTracker)
}

###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################

## Create links from the utterance level to the next highest level(s)
## 
## @param linkTracker
## @param refLevel
## @keywords emuR BPF Emu
## @return list(linkTracker)

bpf_link_utterance_level <- function(linkTracker,
                                     refLevel)
{
  # ---------------------------------------------------------------------------
  # --- Get list of levels that should be linked to from the Utterance level --
  # ---------------------------------------------------------------------------
  
  # (contains refLevel and any levels that are hierarchically higher than refLevel)
  
  underUtterance = bpf_get_levels_under_utterance(linkTracker = linkTracker,
                                                  refLevel = refLevel)
  
  for(level in underUtterance)
  {
    # -------------------------------------------------------------------------
    # --------- Create links from Utterance to current level in temp DB -------
    # -------------------------------------------------------------------------
    
    nbItems = bpf_link_utterance_level_to_current_level(currentLevel = level)
    
    # -------------------------------------------------------------------------
    # ----------------- Determine link type (cardinality) ---------------------
    # -------------------------------------------------------------------------
    
    # Check whether there is one item of this specific level per bundle, or more than one.
    # This determines whether the links from 'Utterance' are ONE_TO_ONE or ONE_TO_MANY.
    
    queryTxt = paste0("SELECT DISTINCT db_uuid, session, bundle FROM items WHERE level = '", level, "'")
    distinctUuidSessionBundle = dbGetQuery(get_emuDBcon(), queryTxt)
    nbBundles = nrow(distinctUuidSessionBundle)
    
    if(nbBundles < nbItems)
    {
      linkType = "ONE_TO_MANY"
    }
    
    else
    {
      linkType = "ONE_TO_ONE"
    }
    
    # -------------------------------------------------------------------------
    # -------------------------- Update link tracker --------------------------
    # -------------------------------------------------------------------------
    
    linkTracker[[length(linkTracker) + 1L]] = list(fromkey = "Utterance", 
                                                     tokey = level, 
                                                     type = linkType)
  }
  
  # ---------------------------------------------------------------------------
  # -------------------------- Return link tracker ----------------------------
  # ---------------------------------------------------------------------------
  
  return(linkTracker)
}
 
###############################################################################
###############################################################################
###############################################################################
###############################################################################
############################################################################### 

## Get list of levels that should be linked to from the Utterance level (reference level plus any levels above reference level)
## 
## @param linkTracker
## @param refLevel
## @keywords emuR BPF Emu
## @return dbSchema

bpf_get_levels_under_utterance <- function(linkTracker,
                                           refLevel)
{
  underUtterance = list(refLevel)
  
  for(idx in 1:length(linkTracker))
  {
    if(linkTracker[[idx]][["tokey"]] == refLevel)
    {
      underUtterance[[length(underUtterance) + 1L]] = linkTracker[[idx]][["fromkey"]]
    }
  }
  
  return(underUtterance)
}

###############################################################################
###############################################################################
###############################################################################
###############################################################################
############################################################################### 

## Link utterance level with current level
## 
## @param currentLevel
## @keywords emuR BPF Emu
## @return nbItems 

bpf_link_utterance_level_to_current_level <- function(currentLevel)
{
  # Get UUID, session, bundle and itemID of all items of the relevant level
  queryTxt = paste0("SELECT db_uuid, session, bundle, itemID FROM items WHERE level = '", currentLevel, "'")
  uuidSessionBundleItemID = dbGetQuery(get_emuDBcon(), queryTxt)
  
  # Loop over all items on this level
  for(idx in 1:nrow(uuidSessionBundleItemID))
  {
    db_uuid = uuidSessionBundleItemID[idx,][["db_uuid"]]
    session = uuidSessionBundleItemID[idx,][["session"]]
    bundle = uuidSessionBundleItemID[idx,][["bundle"]]
    itemID = uuidSessionBundleItemID[idx,][["itemID"]]
    
    # Link all items to their corresponding Utterance item 
    # (same UUID, session & bundle, Utterance itemID is always 1).
    queryTxt = paste0("INSERT INTO links VALUES('", db_uuid, "', '", session, "', '", bundle, "', 1, ", itemID, ", NULL)")
    dbSendQuery(get_emuDBcon(), queryTxt)
  }
  
  nbItems = nrow(uuidSessionBundleItemID)
  return(nbItems)
}

###############################################################################
###############################################################################
###############################################################################
###############################################################################
############################################################################### 

## Create an emuDB schema from link and level tracker
## 
## @param levelTracker
## @param linkTracker
## @param dbName
## @param dbUUID
## @param audioExt
## @keywords emuR BPF Emu
## @return dbSchema

bpf_create_schema <- function(levelTracker, 
                              linkTracker,
                              dbName, 
                              dbUUID, 
                              audioExt)
{
  
  # ---------------------------------------------------------------------------
  # --- Get default level order and level definitions from level tracker ------
  # ---------------------------------------------------------------------------
  
  defaultLevelOrder = bpf_get_default_level_order(levelTracker = levelTracker)
  levelDefinitions = bpf_get_level_definitions(levelTracker = levelTracker)
  
  # ---------------------------------------------------------------------------
  # ------------------- Get link definitions from link tracker ----------------
  # ---------------------------------------------------------------------------
  
  linkDefinitions = bpf_get_link_definitions(linkTracker = linkTracker)
  
  # ---------------------------------------------------------------------------
  # ------------------------------ Create DB schema ---------------------------
  # ---------------------------------------------------------------------------
  
  # Create signalCanvas config.
  sc = create.EMUwebAppConfig.signalCanvas(order = c("OSCI","SPEC"), 
                                           assign = list(), 
                                           contourLims = list())
  
  # Create perspective.
  defPersp = create.EMUwebAppConfig.perspective(name = 'default', 
                                                signalCanvases = sc, 
                                                levelCanvases = list(order = defaultLevelOrder), 
                                                twoDimCanvases = list(order = list()))

  dbSchema = create.schema.databaseDefinition(name = dbName,
                                              UUID = dbUUID,
                                              mediafileBasePathPattern = '',
                                              mediafileExtension = audioExt,
                                              ssffTrackDefinitions = list(),
                                              levelDefinitions = levelDefinitions,
                                              linkDefinitions = linkDefinitions,
                                              EMUwebAppConfig = create.EMUwebAppConfig(perspectives=list(defPersp)),
                                              annotationDescriptors = list(),
                                              tracks = list(),
                                              flags=list())
  
  dbSchema = .update.transient.schema.values(dbSchema)
  
  return(dbSchema)
}

###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################

## Get default level order from level tracker
## 
## @param levelTracker
## @keywords emuR BPF Emu
## @return defaultLevelOrder

bpf_get_default_level_order <- function(levelTracker)
{
  defaultLevelOrder = list()
 
  if(length(levelTracker) > 0)
  {
    for(levelIdx in 1:length(levelTracker))
    {
      if(levelTracker[[levelIdx]][["type"]] %in% c("SEGMENT", "EVENT"))
      {
        defaultLevelOrder[[length(defaultLevelOrder)+1L]] = levelTracker[[levelIdx]][["key"]]
      }
    }
  }
  return(defaultLevelOrder)
}

###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################

## Get level definitions from level tracker
## 
## @param levelTracker
## @keywords emuR BPF Emu
## @return levelDefinitions

bpf_get_level_definitions <- function(levelTracker)
{
  levelDefinitions = list()
  
  if(length(levelTracker) > 0)
  {
    for(levelIdx in 1:length(levelTracker))
    {
      attrDefList = list()
      
      for(label in levelTracker[[levelIdx]][["labels"]])
      {
        attrDefList[[length(attrDefList) + 1L]] = create.schema.attributeDefinition(label)
      }
      
      levelDefinitions[[length(levelDefinitions) + 1L]] = list(name = levelTracker[[levelIdx]][["key"]],
                                                               type = levelTracker[[levelIdx]][["type"]],
                                                               attributeDefinitions = attrDefList)
    }
  }
  
  return(levelDefinitions)
}

###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################

## Get link defintions from link tracker
## 
## @param linkTracker
## @keywords emuR BPF Emu
## @return linkDefinitions

bpf_get_link_definitions <- function(linkTracker = linkTracker)
{
  linkDefinitions = list()
  if(length(linkTracker) > 0)
  {
    for(linkIdx in 1:length(linkTracker))
    {
      linkDefinitions[[length(linkDefinitions)+1L]] = list(type = linkTracker[[linkIdx]][["type"]],
                                                           superlevelName = linkTracker[[linkIdx]][["fromkey"]], 
                                                           sublevelName = linkTracker[[linkIdx]][["tokey"]])
    }
  }
  return(linkDefinitions)
}

###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################

## Display collected warnings about semicolon entries in batch
## 
## @param warningsTracker
## @keywords emuR BPF Emu
## @return

bpf_display_semicolon_warnings <- function(warningsTracker)   
{
  msg = paste0("WARNING: The following BPF files contain links pointing to the space between items (using ';'). ",
                "This feature has not been implemented yet, so the affected items were treated as link-less:\n")
    
  for(path in warningsTracker$semicolonFound)
  {
    msg = paste0(msg, path, "\n")
  }
    
  if(length(warningsTracker$semicolonFound) > 0)
  {
    warning(msg)
  }
}