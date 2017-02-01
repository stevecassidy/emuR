BAS_WORKDIR = file.path(tempdir(), "emuR_bas_workdir")
BAS_TMPDBDIR = file.path(tempdir(), "BASTMP_emuDB")

BAS_ADDRESS = "https://clarin.phonetik.uni-muenchen.de/BASWebServices/services/"

#####################################################################
############################# MAUS ##################################
#####################################################################

bas_run_maus_dbi <- function(handle,
                             canoLabel,
                             canoLevel,
                             mausLabel,
                             mausLevel,
                             languages,
                             chunkLabel,
                             verbose,
                             params,
                             resume,
                             oldBasePath)
{
  bundles_list = languages
  
  if (nrow(bundles_list) > 0)
  {
    bas_ping(verbose)
    
    if (verbose)
    {
      cat("INFO: Running MAUS on emuDB containing",
          nrow(bundles_list),
          "bundle(s)...\n")
      progress = 0
      pb = utils::txtProgressBar(
        min = 0,
        max = nrow(bundles_list),
        initial = 0,
        style = 3
      )
      utils::setTxtProgressBar(pb, progress)
    }
    
    queryTxt = paste0("[", canoLabel,"=~.*\\S.*]")
    cano_items = query(handle, queryTxt, calcTimes = F)
    
    if (!is.null(chunkLabel))
    {
      queryTxt = paste0("[", chunkLabel, "=~.*]")
      trn_items_tmp = query(handle, queryTxt, calcTimes = F)
      
      if (nrow(trn_items_tmp) > 0)
      {
        if (trn_items_tmp[1,"type"] != "SEGMENT")
        {
          stop("Chunk segmentation must be of type SEGMENT")
        }
      }
      
      queryTxt = paste0("[", chunkLabel, "=~.*]")
      trn_items = query(handle, queryTxt)
    }
    
    
    for (bundle_idx in 1:nrow(bundles_list))
    {
      bundle = bundles_list[bundle_idx, "bundle"]
      session = bundles_list[bundle_idx, "session"]
      language = bundles_list[bundle_idx, "language"]
      
      samplerate = bas_get_samplerate(handle, session, bundle)
      
      if (resume &&
          bas_label_exists_in_bundle(handle, session, bundle, mausLabel))
      {
        if (verbose)
        {
          cat(" Skipping bundle", bundle)
          utils::setTxtProgressBar(pb, bundle_idx)
        }
        next
      }
      
      cano_items_bundle = cano_items[cano_items$bundle == bundle &
                                       cano_items$session == session,]
      
      if (nrow(cano_items_bundle) > 0)
      {
        seq_idx = 1
        max_id = bas_get_max_id(handle, session, bundle)
        
        kanfile = file.path(BAS_WORKDIR, paste0(bundle, ".kan.par"))
        maufile = file.path(BAS_WORKDIR, paste0(bundle, ".mau.par"))
        signalfile = bas_get_signal_path(handle, session, bundle, oldBasePath)
        
        kancon <- file(kanfile)
        open(kancon, "w")
        write(paste0("SAM: ", samplerate, "\nLBD:"), kancon)
        
        bas_id = 0
        item_id_to_bas_id = new.env(hash = TRUE)
        bas_id_to_item_id = new.env(hash = TRUE)
        
        for (label_idx in 1:nrow(cano_items_bundle))
        {
          cano_label = stringr::str_trim(cano_items_bundle[label_idx, "labels"])
          cano_item_id = cano_items_bundle[label_idx, "start_item_id"]
          
          kanline = paste0("KAN: ", bas_id, " ", cano_label)
          write(kanline, kancon)
          
          item_id_to_bas_id[[toString(cano_item_id)]] = bas_id
          bas_id_to_item_id[[toString(bas_id)]] = cano_item_id
          bas_id = bas_id + 1
        }
        
        usetrn = "false"
        if (!is.null(chunkLabel))
        {
          usetrn = "true"
          trn_items_bundle = trn_items[trn_items$bundle == bundle &
                                         trn_items$session == session,]
          if (nrow(trn_items_bundle) > 0)
          {
            for (turn_idx in 1:nrow(trn_items_bundle))
            {
              turn_item_id = trn_items_bundle[turn_idx, "start_item_id"]
              turn_start = trn_items_bundle[turn_idx, "sample_start"]
              turn_end = trn_items_bundle[turn_idx, "sample_end"]
              
              linked_ids = requery_hier(
                handle, trn_items_bundle[turn_idx,], canoLabel, calcTimes = F, collapse = F
              )$start_item_id
              
              if (length(linked_ids) > 0)
              {
                bas_ids = sapply(linked_ids, function(x)
                  item_id_to_bas_id[[toString(x)]])
                
                
                trnline = paste(
                  "TRN:", turn_start, turn_end - turn_start,
                  paste0(bas_ids, collapse = ","),"_"
                )
                
                write(trnline, kancon)
              }
            }
          }
        }
        
        close(kancon)
        
        curlParams = list(
          LANGUAGE = language,
          OUTFORMAT = "par",
          USETRN = usetrn,
          SIGNAL = RCurl::fileUpload(signalfile),
          BPF = RCurl::fileUpload(kanfile)
        )
        
        for (key in names(params))
        {
          if (!(key %in% names(curlParams)))
          {
            curlParams[[key]] = params[[key]]
          }
        }
        
        res = RCurl::postForm(
          paste0(BAS_ADDRESS, "runMAUS"),
          .params = curlParams,
          style = "HTTPPOST",
          .opts = bas_get_options()
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
              label = stringr::str_replace_all(stringr::str_trim(splitline[5]), "'", "''")
              
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
                    mausLevel,
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
                    mausLabel,
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
                              languages,
                              minniLabel,
                              minniLevel,
                              verbose,
                              topLevel,
                              params,
                              resume,
                              oldBasePath)
{
  bundles_list = languages
  if (nrow(bundles_list) > 0)
  {
    bas_ping(verbose)
    
    if (verbose)
    {
      cat("INFO: Running MINNI on emuDB containing",
          nrow(bundles_list),
          "bundle(s)...\n")
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
      bundle = bundles_list[bundle_idx, "bundle"]
      session = bundles_list[bundle_idx, "session"]
      language = bundles_list[bundle_idx, "language"]
      
      samplerate = bas_get_samplerate(handle, session, bundle)
      signalfile = bas_get_signal_path(handle, session, bundle, oldBasePath)
      
      if (resume &&
          bas_label_exists_in_bundle(handle, session, bundle, minniLabel))
      {
        if (verbose)
        {
          cat(" Skipping bundle", bundle)
          utils::setTxtProgressBar(pb, bundle_idx)
        }
        next
      }
      
      top_id = NULL
      if (!is.null(topLevel))
      {
        top_id = bas_get_top_id(handle, session, bundle, topLevel)
      }
      
      
      max_id = bas_get_max_id(handle, session, bundle)
      
      minnifile = file.path(BAS_WORKDIR, paste0(bundle, ".minni.par"))
      
      
      curlParams = list(
        LANGUAGE = language,
        OUTFORMAT = "mau",
        SIGNAL = RCurl::fileUpload(signalfile)
      )
      
      for (key in names(params))
      {
        if (!(key %in% names(curlParams)))
        {
          curlParams[[key]] = params[[key]]
        }
      }
      
      
      res = RCurl::postForm(
        uri = paste0(BAS_ADDRESS, "runMINNI"),
        .params = curlParams,
        style = "HTTPPOST",
        .opts = bas_get_options()
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
            label = stringr::str_replace_all(stringr::str_trim(splitline[5]), "'", "''")
            
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
                                             languages,
                                             verbose,
                                             resume,
                                             params)
{
  bundles_list = languages
  if (nrow(bundles_list) > 0)
  {
    bas_ping(verbose)
    
    if (verbose)
    {
      cat(
        "INFO: Running G2P tokenizer on emuDB containing",
        nrow(bundles_list),
        "bundle(s)...\n"
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
    
    queryTxt = paste0("[", transcriptionLabel,"=~.*\\S.*]")
    transcription_items = query(handle, queryTxt, calcTimes = F)
    
    for (bundle_idx in 1:nrow(bundles_list))
    {
      bundle = bundles_list[bundle_idx, "bundle"]
      session = bundles_list[bundle_idx, "session"]
      language = bundles_list[bundle_idx, "language"]
      
      samplerate = bas_get_samplerate(handle, session, bundle)
      
      if (resume &&
          bas_label_exists_in_bundle(handle, session, bundle, orthoLabel))
      {
        if (verbose)
        {
          cat(" Skipping bundle", bundle)
          utils::setTxtProgressBar(pb, bundle_idx)
        }
        next
      }
      
      transcription_items_bundle = transcription_items[transcription_items$bundle == bundle &
                                                         transcription_items$session == session,]
      
      if (nrow(transcription_items_bundle) > 0)
      {
        seq_idx = 1
        max_id = bas_get_max_id(handle, session, bundle)
        
        for (label_idx in 1:nrow(transcription_items_bundle))
        {
          transcription_label = stringr::str_trim(transcription_items_bundle [label_idx, "labels"])
          transcription_item_id = transcription_items_bundle [label_idx, "start_item_id"]
          
          textfile = file.path(BAS_WORKDIR, paste0(bundle, ".", toString(transcription_item_id), ".txt"))
          g2pfile = file.path(BAS_WORKDIR,
                              paste0(bundle,
                                     ".",
                                     toString(transcription_item_id),
                                     ".g2p.par"))
          
          write(transcription_label, file = textfile)
          
          curlParams = list(
            lng = language,
            iform = "txt",
            oform = "bpfs",
            i = RCurl::fileUpload(textfile)
          )
          
          for (key in names(params))
          {
            if (!(key %in% names(curlParams)))
            {
              curlParams[[key]] = params[[key]]
            }
          }
          
          address = paste0(BAS_ADDRESS, "runG2P")
          res = RCurl::postForm(
            uri = address,
            .params = curlParams,
            style = "HTTPPOST",
            .opts = bas_get_options()
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
                label = stringr::str_replace_all(splitline[3], "'", "''")
                
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


bas_run_g2p_for_pronunciation_dbi <- function(handle,
                                              orthoLabel,
                                              orthoLevel,
                                              canoLabel,
                                              languages,
                                              verbose,
                                              resume,
                                              params)
{
  bundles_list = languages
  if (nrow(bundles_list) > 0)
  {
    bas_ping(verbose)
    
    if (verbose)
    {
      cat("INFO: Running G2P on emuDB containing",
          nrow(bundles_list),
          "bundle(s)...\n")
      progress = 0
      pb = utils::txtProgressBar(
        min = 0,
        max = nrow(bundles_list),
        initial = 0,
        style = 3
      )
      utils::setTxtProgressBar(pb, progress)
    }
    
    queryTxt = paste0("[", orthoLabel,"=~.*\\S.*]")
    ortho_items = query(handle, queryTxt, calcTimes = F)
    
    for (bundle_idx in 1:nrow(bundles_list))
    {
      bundle = bundles_list[bundle_idx, "bundle"]
      session = bundles_list[bundle_idx, "session"]
      language = bundles_list[bundle_idx, "language"]
      
      samplerate = bas_get_samplerate(handle, session, bundle)
      
      if (resume &&
          bas_label_exists_in_bundle(handle, session, bundle, canoLabel))
      {
        if (verbose)
        {
          cat(" Skipping bundle", bundle)
          utils::setTxtProgressBar(pb, bundle_idx)
        }
        next
      }
      
      ortho_items_bundle = ortho_items[ortho_items$bundle == bundle &
                                         ortho_items$session == session,]
      
      if (nrow(ortho_items_bundle) > 0)
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
        
        for (label_idx in 1:nrow(ortho_items_bundle))
        {
          ortho_label = stringr::str_trim(ortho_items_bundle[label_idx, "labels"])
          ortho_item_id = ortho_items_bundle[label_idx, "start_item_id"]
          
          write(paste("ORT:", bas_id, ortho_label), orthoCon)
          bas_id_to_item_id[[toString(bas_id)]] = ortho_item_id
          bas_id = bas_id + 1
        }
        
        close(orthoCon)
        
        curlParams = list(
          lng = language,
          iform = "bpf",
          oform = "bpfs",
          i = RCurl::fileUpload(orthofile)
        )
        
        for (key in names(params))
        {
          if (!(key %in% names(curlParams)))
          {
            curlParams[[key]] = params[[key]]
          }
        }
        
        res = RCurl::postForm(
          uri = paste0(BAS_ADDRESS, "runG2P"),
          .params = curlParams,
          style = "HTTPPOST",
          .opts = bas_get_options()
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
              label = stringr::str_replace_all(stringr::str_trim(splitline[3]), "'", "''")
              
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

bas_run_chunker_dbi <- function(handle,
                                canoLabel,
                                canoLevel,
                                chunkLabel,
                                languages,
                                verbose,
                                chunkLevel,
                                topLevel,
                                orthoLabel,
                                params,
                                resume,
                                oldBasePath)
{
  bundles_list = languages
  if (nrow(bundles_list) > 0)
  {
    bas_ping(verbose)
    if (verbose)
    {
      cat(
        "INFO: Running Chunker on emuDB containing",
        nrow(bundles_list),
        "bundle(s)...\n"
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
    
    queryTxt = paste0("[", canoLabel,"=~.*\\S.*]")
    cano_items = query(handle, queryTxt, calcTimes = F)
    
    for (bundle_idx in 1:nrow(bundles_list))
    {
      bundle = bundles_list[bundle_idx, "bundle"]
      session = bundles_list[bundle_idx, "session"]
      language = bundles_list[bundle_idx, "language"]
      
      samplerate = bas_get_samplerate(handle, session, bundle)
      
      if (resume &&
          bas_label_exists_in_bundle(handle, session, bundle, chunkLabel))
      {
        if (verbose)
        {
          cat(" Skipping bundle", bundle)
          utils::setTxtProgressBar(pb, bundle_idx)
        }
        next
      }
      
      cano_items_bundle = cano_items[cano_items$bundle == bundle &
                                       cano_items$session == session,]
      
      top_id = bas_get_top_id(handle, session, bundle, topLevel)
      
      if (nrow(cano_items_bundle) > 0)
      {
        seq_idx = 1
        max_id = bas_get_max_id(handle, session, bundle)
        
        kanfile = file.path(BAS_WORKDIR, paste0(bundle, ".kan.par"))
        trnfile = file.path(BAS_WORKDIR, paste0(bundle, ".trn.par"))
        signalfile = bas_get_signal_path(handle, session, bundle, oldBasePath)
        
        kancon <- file(kanfile)
        open(kancon, "w")
        write(paste0("SAM: ", samplerate, "\nLBD:"), kancon)
        
        bas_id = 0
        item_id_to_bas_id = new.env(hash = TRUE)
        bas_id_to_item_id = new.env(hash = TRUE)
        
        for (label_idx in 1:nrow(cano_items_bundle))
        {
          cano_label = stringr::str_trim(cano_items_bundle[label_idx, "labels"])
          cano_item_id = cano_items_bundle[label_idx, "start_item_id"]
          
          if (!is.null(orthoLabel))
          {
            ortho_labels = requery_hier(
              handle, seglist = cano_items_bundle[label_idx,], level = orthoLabel, calcTimes = F
            )
            
            if (length(ortho_labels) > 0)
            {
              write(paste0("ORT: ", bas_id, " ", ortho_labels[1, "labels"]), kancon)
            }
          }
            
          write(paste0("KAN: ", bas_id, " ", cano_label),
                kancon)
          item_id_to_bas_id[[toString(cano_item_id)]] = bas_id
          bas_id_to_item_id[[toString(bas_id)]] = cano_item_id
          bas_id = bas_id + 1
        }
        
        close(kancon)
        
        curlParams = list(
          language = language,
          audio = RCurl::fileUpload(signalfile),
          bpf = RCurl::fileUpload(kanfile)
        )
        
        for (key in names(params))
        {
          if (!(key %in% names(curlParams)))
          {
            curlParams[[key]] = params[[key]]
          }
        }
        
        res = RCurl::postForm(
          paste0(BAS_ADDRESS, "runChunker"),
          .params = curlParams,
          style = "HTTPPOST",
          .opts = bas_get_options()
        )
        
        trnLines = bas_download(res, trnfile)
        
        if (length(trnLines) > 0)
        {
          for (line_idx in 1:length(trnLines))
          {
            line = trnLines[line_idx]
            if (stringr::str_detect(line, "^TRN:"))
            {
              splitline = stringr::str_split_fixed(line, "\\s+", n = 5)
              start = as.integer(splitline[2])
              duration = as.integer(splitline[3])
              item_id = max_id + seq_idx
              label = stringr::str_replace_all(stringr::str_trim(splitline[5]), "'", "''")
              
              bas_ids = splitline[[4]]
              bas_ids_split = stringr::str_split(bas_ids, ",")[[1]]
              
              if (as.integer(bas_ids_split[1]) >= 0)
              {
                bas_add_item(
                  handle = handle,
                  session = session,
                  bundle = bundle,
                  seq_idx = seq_idx,
                  item_id = item_id,
                  level =
                    chunkLevel,
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
                    chunkLabel,
                  label = label
                )
                
                for (split_idx in 1:length(bas_ids_split))
                {
                  bas_add_link(
                    handle = handle,
                    session = session,
                    bundle = bundle,
                    to_id = bas_id_to_item_id[[bas_ids_split[split_idx]]],
                    from_id =
                      item_id
                  )
                }
                
                if (!is.null(top_id))
                {
                  bas_add_link(
                    handle = handle,
                    session = session,
                    bundle = bundle,
                    to_id = item_id,
                    from_id = top_id
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

#####################################################################
############################ PHO2SYL ################################
#####################################################################

bas_run_pho2syl_canonical_dbi <- function(handle,
                                          canoLabel,
                                          canoLevel,
                                          languages,
                                          verbose,
                                          canoSylLabel,
                                          resume,
                                          params)
{
  bundles_list = languages
  if (nrow(bundles_list) > 0)
  {
    bas_ping(verbose)
    if (verbose)
    {
      cat(
        "INFO: Running Pho2Syl (canonical) on emuDB containing",
        nrow(bundles_list),
        "bundle(s)...\n"
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
    
    queryTxt = paste0("[", canoLabel,"=~.*\\S.*]")
    cano_items = query(handle, queryTxt, calcTimes = F)
    
    for (bundle_idx in 1:nrow(bundles_list))
    {
      bundle = bundles_list[bundle_idx, "bundle"]
      session = bundles_list[bundle_idx, "session"]
      language = bundles_list[bundle_idx, "language"]
      
      samplerate = bas_get_samplerate(handle, session, bundle)
      
      if (resume &&
          bas_label_exists_in_bundle(handle, session, bundle, canoSylLabel))
      {
        if (verbose)
        {
          cat(" Skipping bundle", bundle)
          utils::setTxtProgressBar(pb, bundle_idx)
        }
        next
      }
      
      cano_items_bundle = cano_items[cano_items$bundle == bundle &
                                       cano_items$session == session,]
      
      
      if (nrow(cano_items_bundle) > 0)
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
        
        for (label_idx in 1:nrow(cano_items_bundle))
        {
          cano_label = stringr::str_trim(cano_items_bundle[label_idx, "labels"])
          cano_item_id = cano_items_bundle[label_idx, "start_item_id"]
          
          kanline = paste0("KAN: ", bas_id, " ", cano_label)
          write(kanline, kancon)
          
          bas_id_to_item_id[[toString(bas_id)]] = cano_item_id
          bas_id = bas_id + 1
        }
        
        close(kancon)
        
        curlParams = list(
          lng = language,
          i = RCurl::fileUpload(kanfile),
          tier = "KAN",
          oform = "bpf"
        )
        
        for (key in names(params))
        {
          if (!(key %in% names(curlParams)))
          {
            curlParams[[key]] = params[[key]]
          }
        }
        
        res = RCurl::postForm(
          paste0(BAS_ADDRESS, "runPho2Syl"),
          .params = curlParams,
          style = "HTTPPOST",
          .opts = bas_get_options()
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
              label = stringr::str_replace_all(stringr::str_trim(splitline[3]), "'", "''")
              
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
                    canoSylLabel,
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

bas_run_pho2syl_segmental_dbi <- function(handle,
                                          mausLabel,
                                          mausLevel,
                                          languages,
                                          verbose,
                                          sylLabel,
                                          sylLevel,
                                          wordLabel,
                                          resume,
                                          params)
{
  bundles_list = languages
  if (nrow(bundles_list) > 0)
  {
    bas_ping(verbose)
    if (verbose)
    {
      cat(
        "INFO: Running Pho2Syl (segmental) on emuDB containing",
        nrow(bundles_list),
        "bundle(s)...\n"
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
    
    
    queryTxt = paste0("[", wordLabel,"=~.*]")
    word_items = query(handle, queryTxt, calcTimes = T, timeRefSegmentLevel = mausLevel)
    
    queryTxt = paste0("[", mausLabel,"=~.*]")
    maus_items = query(handle, queryTxt, calcTimes = T, timeRefSegmentLevel = mausLevel)
    
    for (bundle_idx in 1:nrow(bundles_list))
    {
      bundle = bundles_list[bundle_idx, "bundle"]
      session = bundles_list[bundle_idx, "session"]
      language = bundles_list[bundle_idx, "language"]
      
      samplerate = bas_get_samplerate(handle, session, bundle)
      
      if (resume &&
          bas_label_exists_in_bundle(handle, session, bundle, sylLabel))
      {
        if (verbose)
        {
          cat(" Skipping bundle", bundle)
          utils::setTxtProgressBar(pb, bundle_idx)
        }
        next
      }
      
      word_items_bundle = word_items[word_items$bundle == bundle &
                                       word_items$session == session,]
      
      maus_items_bundle = maus_items[maus_items$bundle == bundle &
                                       maus_items$session == session,]
      
      if (nrow(word_items_bundle) > 0 && nrow(maus_items_bundle) > 0)
      {
        seq_idx = 1
        max_id = bas_get_max_id(handle, session, bundle)
        
        maufile = file.path(BAS_WORKDIR, paste0(bundle, ".mau.par"))
        masfile = file.path(BAS_WORKDIR, paste0(bundle, ".mas.par"))
        
        maucon <- file(maufile)
        open(maucon, "w")
        write(paste0("SAM: ", samplerate, "\nLBD:"), maucon)
        
        bas_id = 0
        bas_id_to_word_item_id = new.env(hash = TRUE)
        
        written_anything = FALSE
        
        
        mau_start = - 1
        mau_idx = 1
        
        for (word_idx in 1:nrow(word_items_bundle))
        {
          word_item_id = word_items_bundle[word_idx, "start_item_id"]
          word_end = word_items_bundle[word_idx, "sample_end"]
          word_start = word_items_bundle[word_idx, "sample_start"]
          written_mau = FALSE
          
          while(maus_items_bundle[mau_idx, "sample_end"] <= word_end && mau_idx <= nrow(maus_items_bundle))
          {
            mau_label = stringr::str_trim(maus_items_bundle[mau_idx, "labels"])
            mau_start = maus_items_bundle[mau_idx, "sample_start"]
            mau_end = maus_items_bundle[mau_idx, "sample_end"]

            if (stringr::str_length(mau_label) > 0 && mau_start >= word_start)
            {
              write(
                paste0(
                  "MAU: ",
                  mau_start,
                  " ",
                  mau_end - mau_start,
                  " ",
                  bas_id,
                  " ",
                  mau_label
                ),
                maucon
              )
                
              written_mau = TRUE
              written_anything = TRUE
            }
            
            mau_idx = mau_idx + 1
          }
          if (written_mau)
          {
            bas_id_to_word_item_id[[toString(bas_id)]] = word_item_id
            bas_id = bas_id + 1
          }
        }
        
        close(maucon)
        
        if (written_anything)
        {
          curlParams = list(
            lng = language,
            tier = "MAU",
            oform = "bpf",
            i = RCurl:::fileUpload(maufile)
          )
          
          for (key in names(params))
          {
            if (!(key %in% names(curlParams)))
            {
              curlParams[[key]] = params[[key]]
            }
          }
          
          res = RCurl::postForm(
            paste0(BAS_ADDRESS, "runPho2Syl"),
            .params = curlParams,
            style = "HTTPPOST",
            .opts = bas_get_options()
          )
          
          masLines = bas_download(res, masfile)
          
          if (length(masLines) > 0)
          {
            for (line_idx in 1:length(masLines))
            {
              line = masLines[line_idx]
              if (stringr::str_detect(line, "^MAS:"))
              {
                splitline = stringr::str_split_fixed(line, "\\s+", n = 5)
                item_id = max_id + seq_idx
                start = as.integer(splitline[[2]])
                dur = as.integer(splitline[[3]])
                label = stringr::str_replace_all(stringr::str_trim(splitline[[5]]), "'", "''")
                
                bas_ids = splitline[[4]]
                bas_ids_split = stringr::str_split(bas_ids, ",")[[1]]
                
                if (as.integer(bas_ids_split[1]) >= 0)
                {
                  bas_add_item(
                    handle = handle,
                    session = session,
                    bundle = bundle,
                    item_id = item_id,
                    level = sylLevel,
                    type = "SEGMENT",
                    seq_idx = seq_idx,
                    sample_start = start,
                    sample_dur = dur,
                    samplerate = samplerate
                  )
                  
                  seq_idx = seq_idx + 1
                  
                  bas_add_label(
                    handle = handle,
                    session = session,
                    bundle = bundle,
                    item_id = item_id,
                    label_idx = 1,
                    label_name =
                      sylLabel,
                    label = label
                  )
                  
                  for (split_idx in 1:length(bas_ids_split))
                  {
                    word_item_id = bas_id_to_word_item_id[[bas_ids_split[split_idx]]]
                    
                    
                    bas_add_link(
                      handle = handle,
                      session = session,
                      bundle = bundle,
                      from_id = word_item_id,
                      to_id = item_id
                    )
                  }
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

#####################################################################
############################ HELPERS ################################
#####################################################################
bas_prepare <- function(handle, resume, verbose)
{
  if(dir.exists(BAS_WORKDIR))
  {
    unlink(BAS_WORKDIR, recursive = TRUE)
  }
  
  dir.create(BAS_WORKDIR, recursive = TRUE)
  
  dbConfig = load_DBconfig(handle)
  oldBasePath = handle$basePath
  
  handle$basePath <- file.path(BAS_TMPDBDIR, paste0(handle$dbName, emuDB.suffix))
  tmpCache = file.path(handle$basePath, paste0(handle$dbName, database.cache.suffix))
  
  if(!(resume && file.exists(tmpCache)))
  {
    if(verbose)
    {
      cat("INFO: Preparing temporary database. This may take a while...\n")
    }
    if(dir.exists(BAS_TMPDBDIR))
    {
      unlink(BAS_TMPDBDIR, recursive = TRUE)
    }
    
    dir.create(handle$basePath, recursive = TRUE)
    
    oldCache = file.path(oldBasePath, paste0(handle$dbName, database.cache.suffix))
    if(!file.copy(oldCache, tmpCache, overwrite = T))
    {
      stop("Could not create temporary DB cache")
    }
  }
  
  store_DBconfig(handle, dbConfig)
  
  handle$connection <- DBI::dbConnect(RSQLite::SQLite(), tmpCache)
  return(handle)
}

bas_clear <- function(handle, oldBasePath)
{
  
  oldCache = file.path(oldBasePath, paste0(handle$dbName, database.cache.suffix))
  
  if(!file.copy(file.path(handle$basePath, paste0(handle$dbName, database.cache.suffix)), oldCache, overwrite = TRUE))
  {
    stop("Could not copy temporary DB cache into original DB")
  }
  
  dbConfig = load_DBconfig(handle)
  
  handle$basePath <- oldBasePath
  store_DBconfig(handle, dbConfig)
  
  #unlink(BAS_WORKDIR, recursive = TRUE)
  unlink(BAS_TMPDBDIR, recursive = TRUE)
  
  handle$connection <- DBI::dbConnect(RSQLite::SQLite(), oldCache)
  
  return(handle)
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
    "SELECT max(label_idx) FROM labels",
    basic_cond(handle, session, bundle),
    "AND item_id==",
    item_id
  )
  
  return(DBI::dbGetQuery(handle$connection, queryTxt)[1, 1])
}

bas_get_max_id <- function(handle, session, bundle)
{
  queryTxt = paste0("SELECT max(item_id) FROM items", basic_cond(handle, session, bundle))
  
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
    "INSERT INTO items VALUES('",
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
    "INSERT INTO labels VALUES('",
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
    "INSERT INTO links VALUES('",
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

bas_get_signal_path <- function(handle, session, bundle, basePath)
{
  queryTxt = paste0(
    "SELECT annotates FROM bundle",
    basic_cond(handle, session, bundle, bundlename =
                 "name")
  )
  res = DBI::dbGetQuery(handle$connection, queryTxt)
  if (nrow(res) > 0)
  {
    return(file.path(
      basePath,
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
  res = DBI::dbGetQuery(handle$connection, queryTxt)
  if (nrow(res) > 0)
  {
    return(res[1, 1])
  }
  return(NULL)
}

bas_get_top_id <- function(handle, session, bundle, topLevel)
{
  queryTxt = paste0(
    "SELECT item_id FROM items",
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

bas_label_item_join <- function(labels_table, items_table)
{
  return(
    paste0(
      " ",
      labels_table,
      " JOIN ",
      items_table,
      " ON ",
      labels_table,
      ".db_uuid==",
      items_table,
      ".db_uuid AND ",
      labels_table,
      ".session==",
      items_table,
      ".session AND ",
      labels_table,
      ".bundle==",
      items_table,
      ".bundle AND ",
      labels_table,
      ".item_id==",
      items_table,
      ".item_id "
    )
  )
}



bas_label_exists_in_bundle <-
  function(handle, session, bundle, labelName)
  {
    queryTxt = paste0(
      "SELECT count(*) FROM labels",
      basic_cond(handle, session, bundle),
      "AND name=='",
      labelName,
      "'"
    )
    return(DBI::dbGetQuery(handle$connection, queryTxt)[1, 1] > 0)
  }

bas_get_options <- function()
{
  return(RCurl::curlOptions(connecttimeout = 10, timeout = 2400))
}

bas_ping <- function(verbose)
{
  if(verbose)
  {
    cat("Sending ping to webservices provider.\n")
  }
  
  res = RCurl::getURL(
    url = paste0(BAS_ADDRESS, "help"),
    .opts = RCurl::curlOptions(connecttimeout = 10, timeout = 30)
  )
}


bas_long_enough_for_chunker <- function(handle, basePath)
{
  bundles = list_bundles(handle)
  if (nrow(bundles) > 0)
  {
    for (bundle_idx in nrow(bundles))
    {
      annotates = bas_get_signal_path(handle, session = bundles[bundle_idx, "session"], bundles[bundle_idx, "name"], basePath = basePath)
      obj = wrassp::read.AsspDataObj(annotates)
      if ((attr(obj, "endRecord") / attr(obj, "sampleRate")) > 60)
      {
        return (TRUE)
      }
    }
  }
  return (FALSE)
}

bas_segment_to_item_level <- function(handle, segmentLevel)
{
  bas_segment_to_item_level_dbi(handle, segmentLevel)
  
  dbConfig = load_DBconfig(handle)
  
  for (i in 1:length(dbConfig$levelDefinitions)) {
    if(dbConfig$levelDefinitions[[i]]$name == segmentLevel) {
      dbConfig$levelDefinitions[[i]]$type = 'ITEM'
    }
  }
  
  store_DBconfig(handle, dbConfig)
  
  perspectives = list_perspectives(handle)
  if(nrow(perspectives) > 0)
  {
    for (perspective_idx in 1:nrow(perspectives))
    {
      perspective = perspectives[perspective_idx, "name"]
      oldOrder = get_levelCanvasesOrder(handle, perspective)
      set_levelCanvasesOrder(handle, perspective, oldOrder[oldOrder != segmentLevel])
    }
  }
}


bas_segment_to_item_level_dbi <-
  function(handle, segmentLevel, items_table = "items")
  {
    queryTxt = paste0(
      "UPDATE ", items_table, " SET type='ITEM', sample_point = NULL, sample_start = NULL, sample_dur = NULL WHERE level=='", segmentLevel,
      "' AND db_uuid=='", handle$UUID, "'"
    )
    
    DBI::dbGetQuery(handle$connection, queryTxt)
  }

bas_check_this_is_a_new_label <- function(handle, label)
{
  if (!is.null(get_levelNameForAttributeName(handle, label))) {
    stop("There is already a level with label", label)
  }
  
  if (!is.null(get_levelDefinition(handle, label))) {
    stop("Level ", label, " already exists!")
  }
}


bas_evaluate_language_option <- function(handle, language)
{
  bundles = list_bundles(handle)
  names(bundles)[names(bundles) == "name"] <- "bundle"
  
  if (is.data.frame(language))
  {
    if (nrow(language) != nrow(bundles))
    {
      stop(
        "You have provided a dataframe as language option. This dataframe must contain the same number of bundles as your emuDB. ",
        "Currently, your emuDB contains ", nrow(bundles), " bundles, and the language dataframe ", nrow(language), " bundles."
      )
    }
    if (unique(language$bundle) != unique(bundles$bundle))
    {
      stop("Your language dataframe must contain the same bundles as your emuDB.")
    }
    return(language)
  }
  else
  {
    if (!is.character(language))
    {
      stop("Language option must either be a string or a dataframe.")
    }
    languages = bundles
    languages$language = language
    return(languages)
  }
}