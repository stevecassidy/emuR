

BAS_ITEMS_TMP_TABLE = "bas_items_tmp"
BAS_LINKS_TMP_TABLE = "bas_links_tmp"
BAS_LABELS_TMP_TABLE = "bas_labels_tmp"
BAS_WORKDIR = file.path(tempdir(), "emuR_bas_workdir")
BAS_ADDRESS = "https://clarin.phonetik.uni-muenchen.de/BASWebServices/services/"

bas_run_maus_from_transcription <- function(
  handle,
  transcriptionLabel,
  language,
  canoLabel = "KAN",
  orthoLabel = "ORT",
  mauLabel = "MAU",
  wordLevel = NULL,
  mauLevel = NULL,
  verbose = TRUE
  )
{
  drop_allTmpTablesDBI(handle)
  bas_create_tmp_tables(handle)
  dir.create(BAS_WORKDIR)
  
  transcriptionLevel = get_levelNameForAttributeName(handle, transcriptionLabel)

  if(is.null(wordLevel)) { wordLevel = orthoLabel }
  if(is.null(mauLevel)) { mauLevel = mauLabel }
  
  
  
  bas_run_maus_from_transcription_dbi(handle = handle,
                                      transcriptionLabel = transcriptionLabel,
                                      canoLabel = canoLabel,
                                      orthoLabel = orthoLabel,
                                      mauLabel = mauLabel,
                                      transcriptionLevel = transcriptionLevel,
                                      wordLevel = wordLevel,
                                      mauLevel = mauLevel,
                                      language = language,
                                      verbose = verbose)
  
  add_levelDefinition(handle, wordLevel, "ITEM")
  add_levelDefinition(handle, mauLevel, "SEGMENT")
  add_attributeDefinition(handle, wordLevel, canoLabel)
  add_attributeDefinition(handle, wordLevel, orthoLabel)
  add_attributeDefinition(handle, mauLevel, mauLabel)
  add_linkDefinition(handle, "ONE_TO_MANY", transcriptionLevel, wordLevel)
  add_linkDefinition(handle, "ONE_TO_MANY", wordLevel, mauLevel)
  
  unlink(BAS_WORKDIR, recursive = TRUE)
}


bas_create_tmp_tables <- function(handle)
{
  queryTxt = paste0("CREATE TEMP TABLE ", BAS_ITEMS_TMP_TABLE, " (",
                    "db_uuid VARCHAR(36),",
                    "session TEXT,",
                    "bundle TEXT,",
                    "seq_start_id INTEGER,",
                    "seq_end_id INTEGER,",
                    "p_seq_start_id INTEGER,",
                    "p_seq_end_id INTEGER,",
                    "p_seq_len INTEGER,",
                    "p_level TEXT,",
                    "p_seq_start_seq_idx INTEGER,",
                    "p_seq_end_seq_idx INTEGER",
                    ");")
  DBI::getQuery(handle$connection, queryTxt)
  
  queryTxt = paste0("CREATE TEMP TABLE ", BAS_LINKS_TMP_TABLE, " (",
                    "db_uuid VARCHAR(36),",
                    "session TEXT,",
                    "bundle TEXT,",
                    "from_id INTEGER,",
                    "to_id INTEGER,",
                    "label TEXT",
                    ");")
  DBI::getQuery(handle$connection, queryTxt)
  
  queryTxt = paste0("CREATE TEMP TABLE ", BAS_LABELS_TMP_TABLE, " (",
                    "db_uuid VARCHAR(36),",
                    "session TEXT,",
                    "bundle TEXT,",
                    "item_id INTEGER,",
                    "label_idx INTEGER,",
                    "name TEXT,",
                    "label TEXT",
                    ");")
  DBI::getQuery(handle$connection, queryTxt)
}

bas_run_maus_from_transcription_dbi <- function(
  handle,
  transcriptionLabel,
  canoLabel,
  orthoLabel,
  mauLabel,
  transcriptionLevel,
  wordLevel,
  mauLevel,
  language,
  verbose
)
{
  bas_run_g2p_from_transcription_dbi(
    handle = handle,
    transcriptionLabel = transcriptionLabel,
    canoLabel = canoLabel,
    orthoLabel = orthoLabel,
    wordLevel = wordLevel,
    language = language,
    verbose = verbose
  )
  
  if(TRUE) # NEED TRN check
  
  bas_run_maus_from_cano_dbi(
    handle = handle,
    canoLabel = canoLabel,
    mauLabel = mauLabel,
    mauLevel = mauLevel,
    language = language,
    verbose = verbose
  )
}

bas_run_g2p_from_transcription_dbi <- function(
  handle,
  transcriptionLabel,
  canoLabel,
  orthoLabel,
  transcriptionLevel,
  wordLevel,
  language,
  verbose
)
{
  bundles_list = list_bundles(handle)
  if(nrow(bundles_list) > 0)
  {
    for(bundle_idx in 1:nrow(bundles_list))
    {
      bundle = bundles_list[idx, "name"]
      session = bundles_list[idx, "session"]

      emuQueryTxt = paste0("[", transcriptionLabel, "=~.*]")
      labels_list = query(handle, emuQueryTxt, 
                          sessionPattern = session, bundlePattern = bundle, calcTimes = F)
      
      if(nrow(labels_list) > 0)
      {
        seq_idx = 1
        max_id = bas_get_max_id(handle, session, bundle)
        
        for(label_idx in 1:nrow(labels_list))
        {
          transcription_label = str_trim(labels_list[label_idx, "labels"])
          transcription_item_id = labels_list[label_idx, "start_item_id"]
          
          if(str_length(transcription_label) > 0)
          {
            textfile = file.path(BAS_WORKDIR, paste0(bundle, toString(word_seq_idx), ".txt"))
            g2pfile = file.path(BAS_WORKDIR, paste0(bundle, toString(word_seq_idx), ".g2p.par"))
            
            write(transcription_label, file = textfile)
            
            address = paste0(BAS_ADDRESS, "runG2P")
            res = RCurl::postForm(address, language = language, iform = "txt", nrm = "yes",
                                  oform = "bpf", i = RCurl::fileUpload(textFile), style = "HTTPPOST")
            
            g2pLines = bas_read_download(res, g2pfile)
            
            if(length(g2pLines) > 0)
            {
              for(line_idx in 1:length(g2pLines))
              {
                line = g2pLines[idx]
                if(stringr::str_detect(line, "^KAN:") || stringr::str_detect(line, "^ORT:"))
                {
                  splitline = stringr::str_split_fixed(line, "\\s+", n = 3)
                  item_id = max_id + 1 + as.integer(splitline[2])
                  label = str_replace_all(splitline[3], "'", "''")
                  
                  if(stringr::str_detect(line, "^ORT:"))
                  {
                    bas_add_item(handle=handle, session=session, bundle=bundle, 
                      seq_idx=seq_idx, item_id=item_id, level=wordLevel, type="ITEM")
                    
                    seq_idx = seq_idx + 1
                    
                    bas_add_label(handle=handle, session=session, bundle=bundle, 
                                  item_id=item_id, label_idx=1, label_name=orthoLabel,
                                  label=label)
                    
                    bas_add_link(handle=handle, session=session, bundle=bundle,
                                 from_id=transcription_item_id, to_id=item_id)
                  }
                  else
                  {
                    bas_add_label(handle=handle, session=session, bundle=bundle,
                                  item_id=item_id, label_idx=2, label_name=canoLabel,
                                  label=label)
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}

bas_add_item <- function(
  handle, 
  session, 
  bundle, 
  item_id, 
  level, 
  type, 
  seq_idx, 
  sample_start = NA,
  sample_end = NA,
  sample_point = NA
)
{
  queryTxt = paste0("SELECT samplerate FROM bundles WHERE db_uuid=='", handle$UUID, 
                    "' AND session=='", session, "' AND bundle=='", bundle)
  
  samplerate = DBI::dbGetQuery(dbHandle$connection, queryTxt)[1,1]
  
  queryTxt = paste0("INSERT INTO ", BAS_ITEMS_TMP_TABLE, " VALUES('", handle$UUID, 
                    "','", session, "','", bundle, 
                    "',", item_id, ",'", wordLevel, "','ITEM', ", 
                    seq_idx, ",", samplerate, ",", sample_start, ",", 
                    sample_end, ",", sample_point, ")")
  DBI::dbGetQuery(dbHandle$connection, queryTxt)
}

bas_add_label <- function(
  handle, 
  session, 
  bundle, 
  item_id,
  label_idx,
  label_name,
  label
)
{
  queryTxt = paste0("INSERT INTO ", BAS_LABELS_TMP_TABLE, " VALUES('", handle$UUID, 
                    "', '", session, "', '", bundle, 
                    "', ", item_id, ", ", label_idx,
                    ", '", label_name, "', '", label, "')")
  DBI::dbGetQuery(dbHandle$connection, queryTxt)
}

bas_add_link <- function(
  handle, 
  session, 
  bundle,
  from_id,
  to_id
)
{
  queryTxt = paste0("INSERT INTO ", BAS_LINKS_TMP_TABLE, " VALUES('", handle$UUID, 
                    "', '", session, "', '", bundle, 
                    "', ", from_id, ", ", to_id,
                    ", NA)")
  DBI::dbGetQuery(dbHandle$connection, queryTxt)
}

bas_get_max_id <- function(handle, session, bundle)
{
  queryTxt = paste0("SELECT max(item_id) FROM items WHERE db_uuid=='", handle$UUID, 
                    "' AND session=='", session, "' AND bundle=='", bundle)
  res = DBI::dbGetQuery(handle$connection, queryTxt)
  if(nrow(res) > 0)
  {
    return(res[1,1])
  }
  return(NULL)
}

bas_read_download <- function(result, target)
{
  
  if(stringr::str_detect(result, "<success>false</success>"))
  {
    stop("Unsuccessful webservice call: ", result)
  }
  
  downloadLink = stringr::str_match(result, "<downloadLink>(.*)</downloadLink>")[1,2]
  download.file(downloadLink, target, method="auto", quiet = T, 
                mode = "w", cacheOK = TRUE)
  
  lines = try(readLines(target))
  
  if(class(lines) == "try-error") 
  {
    stop("Cannot read from G2P output ", target)
  }
  
  if(length(lines) == 0)
  {
    stop("Zero line output from webservice: ", target)
  }
  
  return(lines)
}



