read_and_join_key_value_tsv <- function(emuDBhandle, 
                                        file, 
                                        x, 
                                        bundleName, 
                                        sessionName,
                                        delim = "\t"){
  if(file.exists(file)){
    browser()
    key_value_tsv = readr::read_delim(file, 
                                      col_types = readr::cols(),
                                      delim)
    if(all(names(key_value_tsv) == c("key", "value"))){
      key_value_tsv_pivoted = tidyr::pivot_wider(key_value_tsv, names_from = "key", values_from = "value")
      if(missing(bundleName) && missing(sessionName)){
        res = dplyr::full_join(x, key_value_tsv_pivoted, by = character())
      } else if(missing(bundleName) && !missing(sessionName)) {
        # join by session
        key_value_tsv_pivoted$session = sessionName
        res = dplyr::left_join(x, key_value_tsv_pivoted, by = "session")
      } else if(!missing(bundleName) && !missing(sessionName)){
        stop("handeling bundle key value pairs not implemented yet!")
      }
    } else {
      stop(path2tsv, " doesn't only contain the columns 'key' and 'value'. Only these two columns are permitted!")
    }
    return(res)
  }else{
    return(x)
  }
}

read_and_join_long_tsv <- function(emuDBhandle, file, x){
  if(file.exists(file)){
    long_tsv = readr::read_tsv(file, col_types = readr::cols())
    if(all(c("session", "bundle") %in% names(long_tsv))){
      res = dplyr::left_join(x, long_tsv, by = c("bundle", "session"))
    } else {
      stop(file, " doesn't only contain the columns 'session' and 'value'. Only these two columns are permitted!")
    }
    return(res)
  } else {
    return(x)
  }
}

##' Join flat file data (UTF-8 .tsv/.csv files) to x 
##' 
##' Function to join flat file data that is present within 
##' the directories of an emuDB to a tibble/data.frame object
##' usually either produced by \link{query} or \link{get_trackdata}. As
##' it uses the "session" and "bundle" columns to perform the joins these
##' have to be present in x. 
##' 
##' This function recognizes 2 types flat files files:
##' \itemize{
##' \item *_keyValue files
##' }
##' 
##' @param emuDBhandle emuDB handle object (see \link{load_emuDB})
##' @param sessionPattern A regular expression pattern matching session names to be searched from the database
##' @param bundlePattern A regular expression pattern matching bundle names to be searched from the database
##' 
##' @return tibble with the columns 
##' \itemize{
##' \item session
##' \item bundle
##' \item sample_rate_annot_json
##' \item sample_rate_media_file
##' }
##' \code{session}, \code{b}
##' @export
join_flatFileData <- function(emuDBhandle, 
                              x,
                              sessionPattern = '.*', 
                              bundlePattern = '.*',
                              fileExtension = '.tsv'){
  
  # gen. strat. move from bundles to session to emuDB level
  all_bundles = list_bundles(emuDBhandle)
  # filter to sessions & bundles only in x
  all_bundles = all_bundles[all_bundles$session %in% x$session & all_bundles$name %in% x$bundle,]
  # filter by sessionPattern & bundlePattern
  all_bundles = all_bundles[
    grepl(pattern = sessionPattern, x = all_bundles, perl = T) 
    & grepl(pattern = bundlePattern, x = all_bundles, perl = T)
  ]
  
  ##############################
  # handle emuDB level
  
  # get keyValue tsv file on emuDB level
  path2tsv = file.path(emuDBhandle$basePath, paste0(emuDBhandle$dbName, "_keyValue", fileExtension))
  x = read_and_join_key_value_tsv(emuDBhandle, file = path2tsv, x = x)
  
  # get long tsv file on emuDB level
  path2tsv = file.path(emuDBhandle$basePath, paste0(emuDBhandle$dbName, "_long", fileExtension))
  x = read_and_join_long_tsv(emuDBhandle, file = path2tsv, x)
  
  ##############################
  # handle session level
  for(session_name in unique(all_bundles$session)){
    # get keyValue tsv file on session level
    path2tsv = file.path(emuDBhandle$basePath, paste0(session_name, session.suffix), paste0(session_name, "_keyValue", fileExtension))  
    x = read_and_join_key_value_tsv(emuDBhandle, file = path2tsv, x = x, sessionName = session_name)
  }
  
  return(x)
  
}

#######################
# FOR DEVELOPMENT
# library('testthat')
# test_file('tests/testthat/test_aaa_initData.R')
# test_file('tests/testthat/test_emuR-database.flatfiledata.R')

