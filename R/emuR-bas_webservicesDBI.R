# MISSING: Chunker, pho2syl (segment)


BAS_ITEMS_TMP_TABLE = "bas_items_tmp"
BAS_LINKS_TMP_TABLE = "bas_links_tmp"
BAS_LABELS_TMP_TABLE = "bas_labels_tmp"
BAS_WORKDIR = file.path(tempdir(), "emuR_bas_workdir")
BAS_ADDRESS = "https://clarin.phonetik.uni-muenchen.de/BASWebServices/services/"

#####################################################################
############################# MAUS ##################################
#####################################################################

bas_run_maus_from_cano_dbi <- function(handle,
                                       canoLabel,
                                       canoLevel,
                                       mauLabel,
                                       mauLevel,
                                       language,
                                       trnLevel,
                                       verbose)
{
  bundles_list = list_bundles(handle)
  if (nrow(bundles_list) > 0)
  {
    if (verbose)
    {
      cat("INFO: Running MAUS on emuDB containing",
          nrow(bundles_list),
          "bundles...\n")
      progress = 0
      pb = utils::txtProgressBar(
        min = 0,
        max = nrow(bundles_list),
        initial = 0,
        style = 3
      )
      utils::setTxtProgressBar(pb, progress)
    }
    
    for (bundle_idx in 1:nrow(bundles_list))
    {
      bundle = bundles_list[bundle_idx, "name"]
      session = bundles_list[bundle_idx, "session"]
      samplerate = bas_get_samplerate(handle, session, bundle)
      
      
      
      queryTxt = paste0(
        "SELECT label, item_id FROM ",
        BAS_LABELS_TMP_TABLE,
        basic_cond(handle, session, bundle),
        "AND name=='",
        canoLabel,
        "'"
      )
      labels_list = DBI::dbGetQuery(handle$connection, queryTxt)
      
      if (nrow(labels_list) > 0)
      {
        seq_idx = 1
        max_id = bas_get_max_id(handle, session, bundle)
        
        kanfile = file.path(BAS_WORKDIR, paste0(bundle, ".kan.par"))
        maufile = file.path(BAS_WORKDIR, paste0(bundle, ".mau.par"))
        signalfile = bas_get_signal_path(handle, session, bundle)
        
        kancon <- file(kanfile)
        open(kancon, "w")
        write(paste0("SAM: ", samplerate, "\nLBD:"), kancon)
        
        bas_id = 0
        item_id_to_bas_id = new.env(hash = TRUE)
        bas_id_to_item_id = new.env(hash = TRUE)
        
        for (label_idx in 1:nrow(labels_list))
        {
          cano_label = str_trim(labels_list[label_idx, "label"])
          cano_item_id = labels_list[label_idx, "item_id"]
          
          if (str_length(cano_label) > 0)
          {
            write(paste0("KAN: ", bas_id, " ", cano_label, "\n"),
                  kancon)
            item_id_to_bas_id[[toString(cano_item_id)]] = bas_id
            bas_id_to_item_id[[toString(bas_id)]] = cano_item_id
            bas_id = bas_id + 1
          }
        }
        usetrn = "false"
        if (!is.null(trnLevel))
        {
          usetrn = "true"
          if (get_levelDefinition(handle, trnLevel)$type != "SEGMENT")
          {
            stop("TRN level", trnLevel, "must be a segment level")
          }
          queryTxt = paste0(
            "SELECT item_id, sample_start, sample_dur FROM ",
            BAS_ITEMS_TMP_TABLE,
            basic_cond(handle, session, bundle),
            "AND level=='",
            trnLevel,
            "'"
          )
          turn_list = DBI::dbGetQuery(handle$connection, queryTxt)
          if (nrow(turn_list) > 0)
          {
            for (turn_idx in 1:nrow(turn_list))
            {
              turn_item_id = turn_list[turn_idx, "item_id"]
              turn_start = turn_list[turn_idx, "sample_start"]
              turn_duration = turn_list[turn_idx, "sample_dur"]
              links = get_links(handle, session, bundle, turn_item_id, level =
                                  canoLevel)
              linkstrings = c()
              for (link in links)
              {
                linkstrings = c(linkstrings, item_id_to_bas_id[[toString(link)]])
              }
              write(paste(
                "TRN:",
                turn_start,
                turn_duration,
                paste0(linkstrings, collapse = ","),
                "<NA>\n"
              ),
              kancon)
            }
          }
        }
        
        close(kancon)
        
        address = paste0(BAS_ADDRESS, "runMAUS")
        res = RCurl::postForm(
          address,
          LANGUAGE = language,
          OUTFORMAT = "par",
          USETRN = usetrn,
          SIGNAL = RCurl::fileUpload(signalfile),
          style = "HTTPPOST",
          BPF = RCurl::fileUpload(kanfile)
        )
        
        mauLines = bas_download(res, maufile)
        
        if (length(mauLines) > 0)
        {
          for (line_idx in 1:length(mauLines))
          {
            line = mauLines[line_idx]
            if (stringr::str_detect(line, "^MAU:"))
            {
              splitline = stringr::str_split_fixed(line, "\\s+", n = 5)
              start = as.integer(splitline[2])
              duration = as.integer(splitline[3])
              item_id = max_id + seq_idx
              label = str_replace_all(str_trim(splitline[5]), "'", "''")
              
              bas_id = splitline[[4]]
              if (as.integer(bas_id) >= 0)
              {
                bas_add_item(
                  handle = handle,
                  session = session,
                  bundle = bundle,
                  seq_idx = seq_idx,
                  item_id = item_id,
                  level =
                    mauLevel,
                  samplerate = samplerate,
                  type = "SEGMENT",
                  sample_start = start,
                  sample_dur = duration
                )
                
                seq_idx = seq_idx + 1
                
                bas_add_label(
                  handle = handle,
                  session = session,
                  bundle = bundle,
                  item_id = item_id,
                  label_idx = 1,
                  label_name =
                    mauLabel,
                  label = label
                )
                
                bas_add_link(
                  handle = handle,
                  session = session,
                  bundle = bundle,
                  from_id = bas_id_to_item_id[[splitline[4]]],
                  to_id =
                    item_id
                )
              }
            }
          }
        }
      }
      if (verbose)
      {
        utils::setTxtProgressBar(pb, bundle_idx)
      }
    }
  }
  if (verbose)
  {
    cat("\n")
  }
}

#####################################################################
############################# MINNI #################################
#####################################################################


bas_run_minni_dbi <- function(handle,
                              language,
                              minniLabel,
                              minniLevel,
                              verbose,
                              topLevel)
{
  bundles_list = list_bundles(handle)
  if (nrow(bundles_list) > 0)
  {
    if (verbose)
    {
      cat("INFO: Running MINNI on emuDB containing",
          nrow(bundles_list),
          "bundles...\n")
      progress = 0
      pb = utils::txtProgressBar(
        min = 0,
        max = nrow(bundles_list),
        initial = 0,
        style = 3
      )
      utils::setTxtProgressBar(pb, progress)
    }
    
    for (bundle_idx in 1:nrow(bundles_list))
    {
      bundle = bundles_list[bundle_idx, "name"]
      session = bundles_list[bundle_idx, "session"]
      samplerate = bas_get_samplerate(handle, session, bundle)
      signalfile = bas_get_signal_path(handle, session, bundle)
      
      top_id = NULL
      if (!is.null(topLevel))
      {
        top_id = bas_get_top_id(handle, session, bundle, topLevel)
      }
      
      
      max_id = bas_get_max_id(handle, session, bundle)
      
      minnifile = file.path(BAS_WORKDIR, paste0(bundle, ".minni.par"))
      
      address = paste0(BAS_ADDRESS, "runMINNI")
      res = RCurl::postForm(
        address,
        LANGUAGE = language,
        OUTFORMAT = "mau",
        SIGNAL = RCurl::fileUpload(signalfile),
        style = "HTTPPOST"
      )
      
      minniLines = bas_download(res, minnifile)
      if (length(minniLines) > 0)
      {
        seq_idx = 1
        for (line_idx in 1:length(minniLines))
        {
          line = minniLines[line_idx]
          if (stringr::str_detect(line, "^MAU:"))
          {
            splitline = stringr::str_split_fixed(line, "\\s+", n = 5)
            item_id = max_id + seq_idx
            label = str_replace_all(str_trim(splitline[5]), "'", "''")
            
            bas_id = splitline[[4]]
            if (as.integer(bas_id) >= 0)
            {
              bas_add_item(
                handle = handle,
                session = session,
                bundle = bundle,
                seq_idx = seq_idx,
                item_id = item_id,
                level = minniLevel,
                samplerate =
                  samplerate,
                type = "SEGMENT",
                sample_start = as.integer(splitline[2]),
                sample_dur =
                  as.integer(splitline[3])
              )
              
              seq_idx = seq_idx + 1
              
              bas_add_label(
                handle = handle,
                session = session,
                bundle = bundle,
                item_id = item_id,
                label_idx = 1,
                label_name =
                  minniLabel,
                label = label
              )
              
              if (!is.null(top_id))
              {
                bas_add_link(
                  handle = handle,
                  session = session,
                  bundle = bundle,
                  from_id = top_id,
                  to_id = item_id
                )
              }
            }
          }
        }
      }
      if (verbose)
      {
        utils::setTxtProgressBar(pb, bundle_idx)
      }
    }
  }
  if (verbose)
  {
    cat("\n")
  }
}

#####################################################################
############################## G2P ##################################
#####################################################################

bas_run_g2p_for_tokenization_dbi <- function(handle,
                                             transcriptionLabel,
                                             canoLabel,
                                             orthoLabel,
                                             transcriptionLevel,
                                             orthoLevel,
                                             language,
                                             verbose)
{
  bundles_list = list_bundles(handle)
  if (nrow(bundles_list) > 0)
  {
    if (verbose)
    {
      cat(
        "INFO: Running G2P tokenizer on emuDB containing",
        nrow(bundles_list),
        "bundles...\n"
      )
      progress = 0
      pb = utils::txtProgressBar(
        min = 0,
        max = nrow(bundles_list),
        initial = 0,
        style = 3
      )
      utils::setTxtProgressBar(pb, progress)
    }
    for (bundle_idx in 1:nrow(bundles_list))
    {
      bundle = bundles_list[bundle_idx, "name"]
      session = bundles_list[bundle_idx, "session"]
      samplerate = bas_get_samplerate(handle, session, bundle)
      
      queryTxt = paste0(
        "SELECT label, item_id FROM ",
        BAS_LABELS_TMP_TABLE,
        basic_cond(handle, session, bundle),
        "AND name=='",
        transcriptionLabel,
        "'"
      )
      labels_list = DBI::dbGetQuery(handle$connection, queryTxt)
      
      if (nrow(labels_list) > 0)
      {
        seq_idx = 1
        max_id = bas_get_max_id(handle, session, bundle)
        
        for (label_idx in 1:nrow(labels_list))
        {
          transcription_label = str_trim(labels_list[label_idx, "label"])
          transcription_item_id = labels_list[label_idx, "item_id"]
          
          if (str_length(transcription_label) > 0)
          {
            textfile = file.path(BAS_WORKDIR, paste0(bundle, toString(transcription_item_id), ".txt"))
            g2pfile = file.path(BAS_WORKDIR,
                                paste0(
                                  bundle,
                                  toString(transcription_item_id),
                                  ".g2p.par"
                                ))
            
            write(transcription_label, file = textfile)
            
            address = paste0(BAS_ADDRESS, "runG2P")
            res = RCurl::postForm(
              address,
              lng = language,
              iform = "txt",
              nrm = "yes",
              oform = "bpfs",
              i = RCurl::fileUpload(textfile),
              style = "HTTPPOST"
            )
            
            g2pLines = bas_download(res, g2pfile)
            
            if (length(g2pLines) > 0)
            {
              for (line_idx in 1:length(g2pLines))
              {
                line = g2pLines[line_idx]
                if (stringr::str_detect(line, "^ORT:"))
                {
                  splitline = stringr::str_split_fixed(line, "\\s+", n = 3)
                  item_id = max_id + seq_idx
                  label = str_replace_all(splitline[3], "'", "''")
                  
                  bas_add_item(
                    handle = handle,
                    session = session,
                    bundle = bundle,
                    seq_idx = seq_idx,
                    item_id = item_id,
                    level =
                      orthoLevel,
                    samplerate = samplerate,
                    type = "ITEM"
                  )
                  
                  seq_idx = seq_idx + 1
                  
                  bas_add_label(
                    handle = handle,
                    session = session,
                    bundle = bundle,
                    item_id = item_id,
                    label_idx = 1,
                    label_name =
                      orthoLabel,
                    label = label
                  )
                  
                  bas_add_link(
                    handle = handle,
                    session = session,
                    bundle = bundle,
                    from_id = transcription_item_id,
                    to_id =
                      item_id
                  )
                }
              }
            }
          }
        }
      }
      if (verbose)
      {
        utils::setTxtProgressBar(pb, bundle_idx)
      }
    }
  }
  if (verbose)
  {
    cat("\n")
  }
}


bas_run_g2p_from_ortho_dbi <- function(handle,
                                       orthoLabel,
                                       orthoLevel,
                                       canoLabel,
                                       language,
                                       verbose)
{
  bundles_list = list_bundles(handle)
  if (nrow(bundles_list) > 0)
  {
    for (bundle_idx in 1:nrow(bundles_list))
    {
      if (verbose)
      {
        cat("INFO: Running G2P on emuDB containing",
            nrow(bundles_list),
            "bundles...\n")
        progress = 0
        pb = utils::txtProgressBar(
          min = 0,
          max = nrow(bundles_list),
          initial = 0,
          style = 3
        )
        utils::setTxtProgressBar(pb, progress)
      }
      
      bundle = bundles_list[bundle_idx, "name"]
      session = bundles_list[bundle_idx, "session"]
      samplerate = bas_get_samplerate(handle, session, bundle)
      
      queryTxt = paste0(
        "SELECT label, item_id FROM ",
        BAS_LABELS_TMP_TABLE,
        basic_cond(handle, session, bundle),
        "AND name=='",
        orthoLabel,
        "'"
      )
      labels_list = DBI::dbGetQuery(handle$connection, queryTxt)
      
      if (nrow(labels_list) > 0)
      {
        seq_idx = 1
        max_id = bas_get_max_id(handle, session, bundle)
        
        orthofile = file.path(BAS_WORKDIR, paste0(bundle, ".orth.par"))
        kanfile = file.path(BAS_WORKDIR, paste0(bundle, ".kan.par"))
        
        orthoCon <- file(orthofile)
        open(orthoCon, "w")
        write(paste0("SAM: ", samplerate, "\nLBD:"), orthoCon)
        
        bas_id = 0
        
        bas_id_to_item_id = new.env(hash = TRUE)
        
        for (label_idx in 1:nrow(labels_list))
        {
          ortho_label = str_trim(labels_list[label_idx, "label"])
          ortho_item_id = labels_list[label_idx, "item_id"]
          
          if (str_length(ortho_label) > 0)
          {
            write(paste("ORT:", bas_id, ortho_label), orthoCon)
            bas_id_to_item_id[[toString(bas_id)]] = ortho_item_id
            bas_id = bas_id + 1
          }
        }
        
        close(orthoCon)
        
        address = paste0(BAS_ADDRESS, "runG2P")
        res = RCurl::postForm(
          address,
          lng = language,
          iform = "bpf",
          oform = "bpfs",
          i = RCurl::fileUpload(orthofile),
          style = "HTTPPOST"
        )
        
        g2pLines = bas_download(res, kanfile)
        
        if (length(g2pLines) > 0)
        {
          for (line_idx in 1:length(g2pLines))
          {
            line = g2pLines[line_idx]
            if (stringr::str_detect(line, "^KAN:"))
            {
              splitline = stringr::str_split_fixed(line, "\\s+", n = 3)
              item_id = bas_id_to_item_id[[splitline[2]]]
              label = str_replace_all(splitline[3], "'", "''")
              
              bas_add_label(
                handle = handle,
                session = session,
                bundle = bundle,
                item_id = item_id,
                label_idx = bas_get_max_label_idx(handle, session, bundle, item_id) + 1,
                label_name =
                  canoLabel,
                label = label
              )
            }
          }
        }
      }
      if (verbose)
      {
        utils::setTxtProgressBar(pb, bundle_idx)
      }
    }
  }
  if (verbose)
  {
    cat("\n")
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
  bundles_list = list_bundles(handle)
  if (nrow(bundles_list) > 0)
  {
    if (verbose)
    {
      cat(
        "INFO: Running Pho2Syl (canonical) on emuDB containing",
        nrow(bundles_list),
        "bundles...\n"
      )
      progress = 0
      pb = utils::txtProgressBar(
        min = 0,
        max = nrow(bundles_list),
        initial = 0,
        style = 3
      )
      utils::setTxtProgressBar(pb, progress)
    }
    
    for (bundle_idx in 1:nrow(bundles_list))
    {
      bundle = bundles_list[bundle_idx, "name"]
      session = bundles_list[bundle_idx, "session"]
      samplerate = bas_get_samplerate(handle, session, bundle)
      
      queryTxt = paste0(
        "SELECT label, item_id FROM ",
        BAS_LABELS_TMP_TABLE,
        basic_cond(handle, session, bundle),
        "AND name=='",
        canoLabel,
        "'"
      )
      labels_list = DBI::dbGetQuery(handle$connection, queryTxt)
      
      if (nrow(labels_list) > 0)
      {
        seq_idx = 1
        max_id = bas_get_max_id(handle, session, bundle)
        
        kanfile = file.path(BAS_WORKDIR, paste0(bundle, ".kan.par"))
        kasfile = file.path(BAS_WORKDIR, paste0(bundle, ".kas.par"))
        
        kancon <- file(kanfile)
        open(kancon, "w")
        write(paste0("SAM: ", samplerate, "\nLBD:"), kancon)
        
        bas_id = 0
        bas_id_to_item_id = new.env(hash = TRUE)
        
        for (label_idx in 1:nrow(labels_list))
        {
          cano_label = str_trim(labels_list[label_idx, "label"])
          cano_item_id = labels_list[label_idx, "item_id"]
          
          if (str_length(cano_label) > 0)
          {
            write(paste0("KAN: ", bas_id, " ", cano_label, "\n"),
                  kancon)
            bas_id_to_item_id[[toString(bas_id)]] = cano_item_id
            bas_id = bas_id + 1
          }
        }
        
        close(kancon)
        
        address = paste0(BAS_ADDRESS, "runPho2Syl")
        res = RCurl::postForm(
          address,
          lng = language,
          tier = "KAN",
          oform = "bpf",
          style = "HTTPPOST",
          i = RCurl::fileUpload(kanfile)
        )
        
        kasLines = bas_download(res, kasfile)
        
        if (length(kasLines) > 0)
        {
          for (line_idx in 1:length(kasLines))
          {
            line = kasLines[line_idx]
            if (stringr::str_detect(line, "^KAS:"))
            {
              splitline = stringr::str_split_fixed(line, "\\s+", n = 3)
              item_id = max_id + seq_idx
              label = str_replace_all(str_trim(splitline[3]), "'", "''")
              
              bas_id = splitline[[2]]
              if (as.integer(bas_id) >= 0)
              {
                item_id = bas_id_to_item_id[[bas_id]]
                
                bas_add_label(
                  handle = handle,
                  session = session,
                  bundle = bundle,
                  item_id = item_id,
                  label_idx = bas_get_max_label_idx(handle, session, bundle, item_id) + 1,
                  label_name =
                    kasLabel,
                  label = label
                )
              }
            }
          }
        }
      }
      if (verbose)
      {
        utils::setTxtProgressBar(pb, bundle_idx)
      }
    }
  }
  if (verbose)
  {
    cat("\n")
  }
}

bas_run_pho2syl_from_mau_dbi <- function(handle,
                                         mauLabel,
                                         mauLevel,
                                         language,
                                         verbose,
                                         masLabel,
                                         masLevel) # will need a KAN tier for this (as reference for which mau items belong together)
{}

#####################################################################
############################ HELPERS ################################
#####################################################################
bas_prepare <- function(handle)
{
  drop_allTmpTablesDBI(handle)
  bas_create_tmp_tables(handle)
  dir.create(BAS_WORKDIR, recursive = TRUE)
}

bas_create_tmp_tables <- function(handle)
{
  queryTxt = paste0(
    "CREATE TEMP TABLE ",
    BAS_ITEMS_TMP_TABLE,
    " (",
    "db_uuid VARCHAR(36),",
    "session TEXT,",
    "bundle TEXT,",
    "item_id INTEGER,",
    "level TEXT,",
    "type TEXT,",
    "seq_idx INTEGER,",
    "sample_rate INTEGER,",
    "sample_point INTEGER,",
    "sample_start INTEGER,",
    "sample_dur INTEGER",
    ");"
  )
  DBI::dbGetQuery(handle$connection, queryTxt)
  
  queryTxt = paste0(
    "CREATE TEMP TABLE ",
    BAS_LINKS_TMP_TABLE,
    " (",
    "db_uuid VARCHAR(36),",
    "session TEXT,",
    "bundle TEXT,",
    "from_id INTEGER,",
    "to_id INTEGER,",
    "label TEXT",
    ");"
  )
  DBI::dbGetQuery(handle$connection, queryTxt)
  
  queryTxt = paste0(
    "CREATE TEMP TABLE ",
    BAS_LABELS_TMP_TABLE,
    " (",
    "db_uuid VARCHAR(36),",
    "session TEXT,",
    "bundle TEXT,",
    "item_id INTEGER,",
    "label_idx INTEGER,",
    "name TEXT,",
    "label TEXT",
    ");"
  )
  DBI::dbGetQuery(handle$connection, queryTxt)
  
  queryTxt = paste0("INSERT INTO ", BAS_ITEMS_TMP_TABLE, " SELECT * FROM items")
  DBI::dbGetQuery(handle$connection, queryTxt)
  queryTxt = paste0("INSERT INTO ", BAS_LABELS_TMP_TABLE, " SELECT * FROM labels")
  DBI::dbGetQuery(handle$connection, queryTxt)
  queryTxt = paste0("INSERT INTO ", BAS_LINKS_TMP_TABLE, " SELECT * FROM links")
  DBI::dbGetQuery(handle$connection, queryTxt)
}

bas_clear <- function(handle)
{
  bas_join_tmp_tables(handle)
  drop_allTmpTablesDBI(handle)
  unlink(BAS_WORKDIR, recursive = TRUE)
}

bas_join_tmp_tables <- function(handle)
{
  queryTxt = paste0("INSERT OR IGNORE INTO items SELECT * FROM ", BAS_ITEMS_TMP_TABLE)
  DBI::dbGetQuery(handle$connection, queryTxt)
  
  queryTxt = paste0("INSERT OR IGNORE INTO links SELECT * FROM ", BAS_LINKS_TMP_TABLE)
  DBI::dbGetQuery(handle$connection, queryTxt)
  
  queryTxt = paste0("INSERT OR IGNORE INTO labels SELECT * FROM ",
                    BAS_LABELS_TMP_TABLE)
  DBI::dbGetQuery(handle$connection, queryTxt)
}

bas_evaluate_result <- function (result)
{
  return(result)
}

bas_download <- function(result, target)
{
  if (stringr::str_detect(result, "<success>false</success>"))
  {
    stop("Unsuccessful webservice call: ", result)
  }
  
  downloadLink = stringr::str_match(result, "<downloadLink>(.*)</downloadLink>")[1, 2]
  download.file(
    downloadLink,
    target,
    method = "auto",
    quiet = T,
    mode = "w",
    cacheOK = TRUE
  )
  
  lines = try(readLines(target))
  
  if (class(lines) == "try-error")
  {
    stop("Cannot read from G2P output ", target)
  }
  
  if (length(lines) == 0)
  {
    stop("Zero line output from webservice: ", target)
  }
  
  return(lines)
}

bas_get_max_label_idx <- function(handle, session, bundle, item_id)
{
  queryTxt = paste0(
    "SELECT max(label_idx) FROM ",
    BAS_LABELS_TMP_TABLE,
    basic_cond(handle, session, bundle),
    "AND item_id==",
    item_id
  )
  
  return(label_idx = DBI::dbGetQuery(handle$connection, queryTxt)[1, 1])
}

bas_get_max_id <- function(handle, session, bundle)
{
  queryTxt = paste0(
    "SELECT max(item_id) FROM ",
    BAS_ITEMS_TMP_TABLE,
    basic_cond(handle, session, bundle)
  )
  res = DBI::dbGetQuery(handle$connection, queryTxt)
  if (nrow(res) == 0 || is.na(res[1, 1]) || is.null(res[1, 1]))
  {
    return(0)
  }
  return(res[1, 1])
}

bas_add_item <- function(handle,
                         session,
                         bundle,
                         item_id,
                         level,
                         type = 'ITEM',
                         seq_idx,
                         samplerate,
                         sample_start = "NULL",
                         sample_dur = "NULL",
                         sample_point = "NULL")
{
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
    item_id,
    ",'",
    level,
    "','",
    type,
    "', ",
    seq_idx,
    ",",
    samplerate,
    ",",
    sample_point,
    ",",
    sample_start,
    ",",
    sample_dur,
    ")"
  )
  
  DBI::dbGetQuery(handle$connection, queryTxt)
}

bas_add_label <- function(handle,
                          session,
                          bundle,
                          item_id,
                          label_idx,
                          label_name,
                          label)
{
  queryTxt = paste0(
    "INSERT INTO ",
    BAS_LABELS_TMP_TABLE,
    " VALUES('",
    handle$UUID,
    "', '",
    session,
    "', '",
    bundle,
    "', ",
    item_id,
    ", ",
    label_idx,
    ", '",
    label_name,
    "', '",
    label,
    "')"
  )
  DBI::dbGetQuery(handle$connection, queryTxt)
}

bas_add_link <- function(handle,
                         session,
                         bundle,
                         from_id,
                         to_id)
{
  queryTxt = paste0(
    "INSERT INTO ",
    BAS_LINKS_TMP_TABLE,
    " VALUES('",
    handle$UUID,
    "', '",
    session,
    "', '",
    bundle,
    "', ",
    from_id,
    ", ",
    to_id,
    ", NULL)"
  )
  DBI::dbGetQuery(handle$connection, queryTxt)
}

bas_get_signal_path <- function(handle, session, bundle)
{
  queryTxt = paste0(
    "SELECT annotates FROM bundle",
    basic_cond(handle, session, bundle, bundlename =
                 "name")
  )
  res = dbGetQuery(handle$connection, queryTxt)
  if (nrow(res) > 0)
  {
    return(file.path(
      handle$basePath,
      paste0(session, session.suffix),
      paste0(bundle, bundle.dir.suffix),
      res[1, 1]
    ))
  }
  return(NULL)
}

bas_get_samplerate <- function(handle, session, bundle)
{
  queryTxt = paste0(
    "SELECT sample_rate FROM bundle",
    basic_cond(handle, session, bundle, bundlename =
                 "name")
  )
  res = dbGetQuery(handle$connection, queryTxt)
  if (nrow(res) > 0)
  {
    return(res[1, 1])
  }
  return(NULL)
}

bas_get_top_id <- function(handle, session, bundle, topLevel)
{
  queryTxt = paste0(
    "SELECT item_id FROM ",
    BAS_ITEMS_TMP_TABLE,
    basic_cond(handle, session, bundle),
    "AND level=='",
    topLevel,
    "'"
  )
  
  res = DBI::dbGetQuery(handle$connection, queryTxt)
  if (nrow(res) == 0) {
    return(NULL)
  }
  if (nrow(res) > 1) {
    stop("More than one possible node on level ",
         topLevel,
         " in bundle ",
         bundle)
  }
  return(res[[1, 1]])
}

bas_new_canvas <- function(handle, perspective, canvas)
{
  if (!is.null(perspective))
  {
    if (!(perspective %in% list_perspectives(handle)$name))
    {
      add_perspective(handle, perspective)
    }
    
    set_levelCanvasesOrder(handle,
                           perspectiveName = perspective,
                           c(get_levelCanvasesOrder(handle, perspective), canvas))
  }
}