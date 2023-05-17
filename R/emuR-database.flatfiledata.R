read_and_join_long_flatData <- function(emuDBhandle, 
                                        file, 
                                        x, 
                                        bundleName, 
                                        sessionName,
                                        readFunction){
  if(file.exists(file)){
    key_value_data = suppressMessages(readFunction(file = file,
                                                   col_types = readr::cols()))
    
    if(all(names(key_value_data) == c("key", "value"))){
      key_value_data_pivoted = tidyr::pivot_wider(key_value_data, names_from = "key", values_from = "value")
      if(missing(bundleName) && missing(sessionName)){
        res = dplyr::cross_join(x, key_value_data_pivoted)
      } else if(missing(bundleName) && !missing(sessionName)) {
        # join by session
        key_value_data_pivoted$session = sessionName
        res = dplyr::left_join(x, key_value_data_pivoted, by = "session")
      } else if(!missing(bundleName) && !missing(sessionName)){
        key_value_data_pivoted$session = sessionName
        key_value_data_pivoted$bundle = bundleName
        res = dplyr::left_join(x, key_value_data_pivoted, by = c("session", "bundle"))
      }
    } else {
      stop(file, " doesn't only contain the columns 'key' and 'value'. Only these two columns are permitted!")
    }
    return(res)
  }else{
    return(x)
  }
}

read_and_join_wide_flatData <- function(emuDBhandle, 
                                        file, 
                                        x, 
                                        bundleName, 
                                        sessionName,
                                        readFunction){
  if(file.exists(file)){
    long_flatData = suppressMessages(readFunction(file, 
                                                  col_types = readr::cols()))
    
    if(missing(bundleName) && missing(sessionName)){
      # emuDB level
      if(all(c("session", "bundle") %in% names(long_flatData))){
        res = dplyr::left_join(x, long_flatData, by = c("bundle", "session"))
      } else {
        stop(paste0("session and/or bundle columns not found in ", file))
      }
    } else if(missing(bundleName) && !missing(sessionName)){
      # session level 
      if("bundle" %in% names(long_flatData)){
        long_flatData$session = sessionName
        res = dplyr::left_join(x, long_flatData, by = c("session", "bundle"))
      } else {
        stop(paste0("bundle column not found in ", file))
      }
    } else if(!missing(bundleName) && !missing(sessionName)){
      # bundle level
      long_flatData$session = sessionName
      long_flatData$bundle = bundleName
      res = dplyr::left_join(x, long_flatData, by = c("session", "bundle"))
    } else{
      stop(file, " doesn't only contain the columns 'session' and 'bundle'. Only these two columns are permitted!")
    }
    return(res)
  } else {
    return(x)
  }
}

## Join flat file data (UTF-8 .csv/.tsv files) to x 
## 
## Join flat file data that is present within 
## the directories of an emuDB to a tibble/data.frame object
## usually either produced by \link{query} or \link{get_trackdata}. As
## it uses the "session" and "bundle" columns to perform the joins these
## have to be present in x. 
## 
## This function recognizes 2 types flat files files:
## \itemize{
## \item *_keyValue files
## }
## 
## @param emuDBhandle emuDB handle object (see \link{load_emuDB})
## @param sessionPattern A regular expression pattern matching session names to be searched from the database
## @param bundlePattern A regular expression pattern matching bundle names to be searched from the database
## @param fileExtension file extension of flat data files (default: '.csv')
## @param readFunction function used to read in flat data files (default: \link{readr::read_csv2})
## 
## @return tibble with the columns 
## \itemize{
## \item session
## \item bundle
## \item sample_rate_annot_json
## \item sample_rate_media_file
## }
## \code{session}, \code{b}
## @export
join_flatFileData <- function(emuDBhandle, 
                              x,
                              sessionPattern = '.*', 
                              bundlePattern = '.*',
                              fileExtension = '.csv',
                              readFunction = readr::read_csv2){
  
  # gen. strat. move from bundles to session to emuDB level
  all_bundles = list_bundles(emuDBhandle)
  # filter to sessions & bundles only in x
  all_bundles = all_bundles[all_bundles$session %in% x$session & all_bundles$name %in% x$bundle,]
  # filter by sessionPattern & bundlePattern
  all_bundles = all_bundles[
    grepl(pattern = sessionPattern, x = all_bundles, perl = TRUE)
    & grepl(pattern = bundlePattern, x = all_bundles, perl = TRUE)
  ]
  
  ##############################
  # handle emuDB level
  # get long flat data file on emuDB level
  path2flatDataFile = file.path(emuDBhandle$basePath, 
                                paste0(emuDBhandle$dbName, "_long", fileExtension))
  x = read_and_join_long_flatData(emuDBhandle, 
                                  file = path2flatDataFile, 
                                  x = x,
                                  readFunction = readFunction)
  # get wide flat data file on emuDB level
  path2tsv = file.path(emuDBhandle$basePath, paste0(emuDBhandle$dbName, "_wide", fileExtension))
  x = read_and_join_wide_flatData(emuDBhandle, 
                                  file = path2tsv, 
                                  x = x,
                                  readFunction = readFunction)
  
  ##############################
  # handle session level
  for(session_name in unique(all_bundles$session)){
    # get long flat data file on session level
    path2flatDataFile = file.path(emuDBhandle$basePath, 
                                  paste0(session_name, session.suffix), 
                                  paste0(session_name, "_long", fileExtension))
    
    x = read_and_join_long_flatData(emuDBhandle, 
                                    file = path2flatDataFile, 
                                    x = x, 
                                    sessionName = session_name,
                                    readFunction = readFunction)
    
    # get wide flat data file on session level
    path2flatDataFile = file.path(emuDBhandle$basePath, 
                                  paste0(session_name, session.suffix), 
                                  paste0(session_name, "_wide", fileExtension))
    
    x = read_and_join_wide_flatData(emuDBhandle, 
                                    file = path2flatDataFile, 
                                    x = x, 
                                    sessionName = session_name,
                                    readFunction = readFunction)
    
  }
  
  ##############################
  # handle bundle level
  for(bndl_row_idx in 1:nrow(all_bundles)){
    
    cur_bndl = all_bundles[bndl_row_idx,]
    # get long flat data file on session level
    path2flatDataFile = file.path(emuDBhandle$basePath, 
                                  paste0(cur_bndl$session, session.suffix), 
                                  paste0(cur_bndl$name, bundle.dir.suffix),
                                  paste0(cur_bndl$name, "_long", fileExtension))
    
    x = read_and_join_long_flatData(emuDBhandle, 
                                    file = path2flatDataFile, 
                                    x = x, 
                                    sessionName = cur_bndl$session,
                                    bundleName = cur_bndl$name,
                                    readFunction = readFunction)
    
    # get wide flat data file on session level
    path2flatDataFile = file.path(emuDBhandle$basePath, 
                                  paste0(session_name, session.suffix), 
                                  paste0(session_name, "_wide", fileExtension))
    
    x = read_and_join_wide_flatData(emuDBhandle, 
                                    file = path2flatDataFile, 
                                    x = x, 
                                    sessionName = session_name,
                                    readFunction = readFunction)
    
  }
  
  
  return(x)
  
}

#######################
# FOR DEVELOPMENT
# library('testthat')
# test_file('tests/testthat/test_aaa_initData.R')
# test_file('tests/testthat/test_emuR-database.flatfiledata.R')

