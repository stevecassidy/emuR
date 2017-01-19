BAS_ITEMS_TMP_TABLE = "bas_items_tmp"
BAS_LINKS_TMP_TABLE = "bas_links_tmp"
BAS_LABELS_TMP_TABLE = "bas_labels_tmp"
BAS_WORKDIR = file.path(tempdir(), "emuR_bas_workdir")
BAS_ADDRESS = "https://clarin.phonetik.uni-muenchen.de/BASWebServices/services/"

#####################################################################
############################# MAUS ##################################
#####################################################################

bas_run_maus_from_cano_dbi <- function(
  handle,
  canoLabel,
  canoLevel,
  mauLabel,
  mauLevel,
  language,
  trnLevel,
  verbose)
{
  bundles_list = list_bundles(handle)
  if(nrow(bundles_list) > 0)
  {
    for(bundle_idx in 1:nrow(bundles_list))
    {
      bundle = bundles_list[idx, "name"]
      session = bundles_list[idx, "session"]
      samplerate = bas_get_samplerate(handle, session, bundle)
      
      
      emuQueryTxt = paste0("[", canoLabel, "=~.*]")
      labels_list = query(handle, emuQueryTxt, 
                          sessionPattern = session, bundlePattern = bundle, calcTimes = F)
      
      if(nrow(labels_list) > 0)
      {
        seq_idx = 1
        max_id = bas_get_max_id(handle, session, bundle)
        
        kanfile = file.path(BAS_WORKDIR, paste0(bundle, ".kan.par"))
        maufile = file.path(BAS_WORKDIR, paste0(bundle, ".mau.par"))
        signalfile = bas_get_signal_path(handle, session, bundle)
        
        kancon <- file(kanfile) 
        open(kancon, "w")
        write(paste0("SAM: ", samplerate, "\nLBD:\n"), kancon)
        
        bas_id = 0
        item_id_to_bas_id = list()
        bas_id_to_item_id = list()
        for(label_idx in 1:nrow(labels_list))
        {
          cano_label = str_trim(labels_list[label_idx, "labels"])
          cano_item_id = labels_list[label_idx, "start_item_id"]
          
          if(str_length(cano_label) > 0)
          {
            write(paste0("KAN: ", bas_id, " ", cano_label, "\n"), kancon)
            item_id_to_bas_id[[cano_item_id]] = bas_id
            bas_id_to_item_id[[bas_id]] = cano_item_id
            bas_id = bas_id + 1
          }
        }
        
        usetrn = "no"
        if(!is.null(trnLevel))
        {
          usetrn = "yes"
          if(get_levelDefinition(handle, trnLevel)$type != "SEGMENT")
          {
            stop("TRN level", trnLevel, "must be a segment level")
          }
          queryTxt = paste0("SELECT item_id, sample_start, sample_dur FROM items", basic_cond(handle, session, bundle), 
                            "AND level=='", trnLevel, "'")
          turn_list = DBI::dbGetQuery(handle$connection, queryTxt)
          if(nrow(turn_list) > 0)
          {
            for(turn_idx in 1:nrow(turn_list))
            {
              turn_item_id = turn_list[turn_idx, "item_id"]
              turn_start = turn_list[turn_idx, "sample_start"]
              turn_duration = turn_list[turn_idx, "sample_dur"]
              links = get_links(handle, session, bundle, turn_item_id, level=canoLevel)
              write(paste("TRN:", turn_start, turn_duration, paste0(item_id_to_bas_id[links], collapse=","), "<NA>\n"), kancom)
            }
          }
        }
        
        close(kancon)
        
        address = paste0(BAS_ADDRESS, "runMAUS")
        res = RCurl::postForm(address, LANGUAGE=language, OUTFORM="par", USETRN=usetrn,
                              SIGNAL=RCurl::fileUpload(signalFile), style = "HTTPPOST",
                              BPF=RCurl::fileUpload(kanFile), style = "HTTPPOST")
        
        mauLines = bas_download(res, maufile)
        
        if(length(mauLines) > 0)
        {
          for(line_idx in 1:length(mauLines))
          {
            line = mauLines[line_idx]
            if(stringr::str_detect(line, "^MAU:"))
            {
              splitline = stringr::str_split_fixed(line, "\\s+", n = 5)
              start = as.integer(splitline[2])
              duration = as.integer(splitline[3])
              item_id = max_id + 1 + as.integer(splitline[4])
              label = str_replace_all(str_trim(splitline[5]), "'", "''")
              
              bas_add_item(handle=handle, session=session, bundle=bundle,
                           seq_idx=seq_idx, item_id=item_id, level=mauLevel, samplerate=samplerate, type="SEGMENT",
                           start=start, duration=duration)
              
              seq_idx = seq_idx + 1
              
              bas_add_label(handle=handle, session=session, bundle=bundle, 
                            item_id=item_id, label_idx=1, label_name=mauLabel,
                            label=label)
              
              bas_add_link(handle=handle, session=session, bundle=bundle,
                           from_id=bas_id_to_item_id[[bas_id]], to_id=item_id)
            }
          }
        }
      }
    }
  }
}

#####################################################################
############################# MINNI #################################
#####################################################################

bas_run_minni_dbi <- function(
  handle,
  language,
  minniLabel,
  minniLevel,
  verbose,
  topLevel)
{
  
  top_id = bas_get_top_id(topLevel)
  
  bundles_list = list_bundles(handle)
  if(nrow(bundles_list) > 0)
  {
    for(bundle_idx in 1:nrow(bundles_list))
    {
      bundle = bundles_list[idx, "name"]
      session = bundles_list[idx, "session"]
      samplerate = bas_get_samplerate(handle, session, bundle)
      signalfile = bas_get_signal_path(handle, session, bundle)
      
      max_id = bas_get_max_id(handle, session, bundle)
      
      minnifile = file.path(BAS_WORKDIR, paste0(bundle, ".minni.par"))
      
      address = paste0(BAS_ADDRESS, "runMINNI")
      res = RCurl::postForm(address, LANGUAGE=language, OUTFORM="par",
                            SIGNAL=RCurl::fileUpload(signalFile), style = "HTTPPOST")
      
      minniLines = bas_download(res, minnifile)
      if(length(minniLines) > 0)
      {
        seq_idx = 1
        for(line_idx in 1:length(minniLines))
        {
          line = minniLines[line_idx]
          if(stringr::str_detect(line, "^MAU:"))
          {
            splitline = stringr::str_split_fixed(line, "\\s+", n = 5)
            start = as.integer(splitline[2])
            duration = as.integer(splitline[3])
            item_id = max_id + 1 + as.integer(splitline[4])
            label = str_replace_all(str_trim(splitline[5]), "'", "''")
            
            
            bas_add_item(handle=handle, session=session, bundle=bundle,
                         seq_idx=seq_idx, item_id=item_id, level=minniLevel, samplerate=samplerate, type="SEGMENT",
                         start=start, duration=duration)
            
            seq_idx = seq_idx + 1
            
            bas_add_label(handle=handle, session=session, bundle=bundle, 
                          item_id=item_id, label_idx=1, label_name=minniLabel,
                          label=label)
            
            if(!is.null(top_id))
            {
              bas_add_link(handle=handle, session=session, bundle=bundle,
                           from_id=top_id, to_id=item_id)
            }
          }
        }
      }
    }
  }
}

#####################################################################
############################## G2P ##################################
#####################################################################

bas_run_g2p_for_tokenization_dbi <- function(
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
      samplerate = bas_get_samplerate(handle, session, bundle)
      
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
            textfile = file.path(BAS_WORKDIR, paste0(bundle, toString(transcription_item_id), ".txt"))
            g2pfile = file.path(BAS_WORKDIR, paste0(bundle, toString(transcription_item_id), ".g2p.par"))
            
            write(transcription_label, file = textfile)
            
            address = paste0(BAS_ADDRESS, "runG2P")
            res = RCurl::postForm(address, language = language, iform = "txt", nrm = "yes",
                                  oform = "bpf", i = RCurl::fileUpload(textFile), style = "HTTPPOST")
            
            g2pLines = bas_download(res, g2pfile)
            
            if(length(g2pLines) > 0)
            {
              for(line_idx in 1:length(g2pLines))
              {
                line = g2pLines[idx]
                if(stringr::str_detect(line, "^ORT:"))
                {
                  splitline = stringr::str_split_fixed(line, "\\s+", n = 3)
                  item_id = max_id + 1 + as.integer(splitline[2])
                  label = str_replace_all(splitline[3], "'", "''")
                  
                  bas_add_item(handle=handle, session=session, bundle=bundle, 
                               seq_idx=seq_idx, item_id=item_id, level=wordLevel, samplerate=samplerate, type="ITEM")
                  
                  seq_idx = seq_idx + 1
                  
                  bas_add_label(handle=handle, session=session, bundle=bundle, 
                                item_id=item_id, label_idx=1, label_name=orthoLabel, label=label)
                  
                  bas_add_link(handle=handle, session=session, bundle=bundle,
                               from_id=transcription_item_id, to_id=item_id)
                }
              }
            }
          }
        }
      }
    }
  }
}

bas_run_g2p_from_ortho_dbi <- function(
  handle,
  orthoLabel,
  orthoLevel,
  canoLabel,
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
      samplerate = bas_get_samplerate(handle, session, bundle)
      
      emuQueryTxt = paste0("[", orthoLabel, "=~.*]")
      labels_list = query(handle, emuQueryTxt, 
                          sessionPattern = session, bundlePattern = bundle, calcTimes = F)
      
      if(nrow(labels_list) > 0)
      {
        seq_idx = 1
        max_id = bas_get_max_id(handle, session, bundle)
        
        orthofile = file.path(BAS_WORKDIR, paste0(bundle, toString(transcription_item_id), ".par"))
        kanfile = file.path(BAS_WORKDIR, paste0(bundle, toString(transcription_item_id), ".g2p.par"))
        
        orthoCon = open(orthofile, "r")
        write(paste0("SAM: ", samplerate, "\nLBD:\n"), orthoCon)
        
        bas_id = 0
        
        bas_id_to_item_id = list()
        
        for(label_idx in 1:nrow(labels_list))
        {
          ortho_label = str_trim(labels_list[label_idx, "labels"])
          ortho_item_id = labels_list[label_idx, "start_item_id"]
          
          if(str_length(ortho_label) > 0)
          {
            write(paste("ORT:", bas_id, ortho_label), orthoCon)
            bas_id_to_item_id[[bas_id]] = ortho_item_id
            bas_id = bas_id + 1
          }
        }
        
        address = paste0(BAS_ADDRESS, "runG2P")
        res = RCurl::postForm(address, language = language, iform = "bpf",
                              oform = "bpf", i = RCurl::fileUpload(orthoFile), style = "HTTPPOST")
        
        g2pLines = bas_download(res, g2pfile)
        
        if(length(g2pLines) > 0)
        {
          for(line_idx in 1:length(g2pLines))
          {
            line = g2pLines[idx]
            if(stringr::str_detect(line, "^KAN:"))
            {
              splitline = stringr::str_split_fixed(line, "\\s+", n = 3)
              item_id = bas_id_to_item_id[[as.integer(splitline[2])]]
              label = str_replace_all(splitline[3], "'", "''")
              queryTxt = paste0("SELECT max(label_idx) FROM labels WHERE", basic_cond(handle, session, bundle),
                                "AND item_id==", item_id)
              label_idx = DBI::dbGetQuery(handle$connection, queryTxt)[1,1] + 1
              
              bas_add_label(handle=handle, session=session, bundle=bundle, 
                            item_id=item_id, label_idx=2, label_name=canoLabel, label=label)
            }
          }
        }
      }
    }
  }
}

#####################################################################
############################ CHUNKER ################################
#####################################################################

bas_run_chunker_from_cano_dbi <- function(handle,
                                          canoLabel,
                                          canoLevel,
                                          trnLabel,
                                          language,
                                          verbose,
                                          trnLevel,
                                          topLevel)
{
  top_id = bas_get_top_id(handle, session, bundle, topLevel)
  
}

#####################################################################
############################ PHO2SYL ################################
#####################################################################

bas_run_pho2syl_from_cano_dbi <- function(handle,
                                          canoLabel,
                                          canoLevel,
                                          language,
                                          verbose,
                                          kasLabel)
{
  
}

bas_run_pho2syl_from_mau_dbi <- function(handle,
                                         mauLabel,
                                         mauLevel,
                                         language,
                                         verbose,
                                         masLabel,
                                         masLevel)
{
  
}

#####################################################################
############################ HELPERS ################################
#####################################################################
bas_prepare <- function(handle)
{
  drop_allTmpTablesDBI(handle)
  bas_create_tmp_tables(handle)
  dir.create(BAS_WORKDIR)
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

bas_clear <- function(handle)
{
  bas_join_tmp_tables(handle)
  drop_allTmpTablesDBI(handle)
  unlink(BAS_WORKDIR, recursive = TRUE)
}

bas_join_tmp_tables <- function(handle)
{
  queryTxt = paste0("INSERT INTO items SELECT * FROM ", BAS_ITEMS_TMP_TABLE)
  DBI::getQuery(handle$connection, queryTxt)
  
  queryTxt = paste0("INSERT INTO links SELECT * FROM ", BAS_LINKS_TMP_TABLE)
  DBI::getQuery(handle$connection, queryTxt)
  
  queryTxt = paste0("INSERT INTO labels SELECT * FROM ", BAS_LABELS_TMP_TABLE)
  DBI::getQuery(handle$connection, queryTxt)
}

bas_evaluate_result <- function (result)
{
  return(result)
}

bas_download <- function(result, target)
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

bas_get_max_id <- function(handle, session, bundle)
{
  queryTxt = paste0("SELECT max(item_id) FROM items", basic_cond(handle, session, bundle))
  res = DBI::dbGetQuery(handle$connection, queryTxt)
  if(nrow(res) > 0)
  {
    return(res[1,1])
  }
  return(NULL)
}

bas_add_item <- function(
  handle, 
  session, 
  bundle, 
  item_id, 
  level, 
  type, 
  seq_idx,
  samplerate,
  sample_start = NA,
  sample_end = NA,
  sample_point = NA
)
{
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

bas_get_signal_path <- function(handle, session, bundle)
{
  queryTxt = paste0("SELECT annotates FROM bundles", basic_cond(handle, session, bundle, bundlename="name"))
  res = dbGetQuery(handle$connection, queryTxt)
  if(nrow(res) > 0)
  {
    return(file.path(handle$basePath, paste0(session, session.suffix), paste0(bundle, bundle.dir.suffix), res[1,1]))
  }
  return(NULL)
}

bas_get_samplerate <- function(handle, session, bundle)
{
  queryTxt = paste0("SELECT samplerate FROM bundles", basic_cond(handle, session, bundle, bundlename="name"))
  res = dbGetQuery(handle$connection, queryTxt)
  if(nrow(res) > 0)
  {
    return(res[1,1])
  }
  return(NULL)
}

bas_get_top_id <- function(handle, session, bundle, topLevel)
{
  if(is.null(topLevel)) { return(NULL) }
  queryTxt = paste0("SELECT item_id FROM items", basic_cond(handle, session, bundle), "AND level=='", topLevel)
  
  res = DBI::dbGetQuery(handle$connection, queryTxt)
  if(nrow(res) == 0) { return(NULL) }
  if(nrow(res) > 1) { stop() }
  return(res[[1,1]])
}
