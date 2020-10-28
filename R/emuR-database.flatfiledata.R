##' List sample rates of media and annotation (_annot.json) files
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
join_tsvs <- function(emuDBhandle, 
                      x,
                      sessionPattern = '.*', 
                      bundlePattern = '.*'){
  
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
  # handle session level
  
  for(sessio_name in unique(all_bundles$session)){
    # get keyValue tsv file on session level
    path2tsv = file.path(emuDBhandle$basePath, sessio_name, paste0(sessio_name, "_keyValue.", "tsv"))  
  }
  
  ##############################
  # handle emuDB level
  
  # get keyValue tsv file on emuDB level
  #browser()
  path2tsv = file.path(emuDBhandle$basePath, paste0(emuDBhandle$dbName, "_keyValue.", "tsv"))
  if(file.exists(path2tsv)){
    key_value_tsv = readr::read_tsv(path2tsv, col_types = readr::cols())
    if(all(names(key_value_tsv) == c("key", "value"))){
      res = dplyr::full_join(x, key_value_tsv, by = character())
    } else {
      stop(path2tsv, " doesn't only contain the columns 'key' and 'value'. Only these two columns are permitted!")
    }
    
  }

  # get long tsv file on emuDB level
  path2tsv = file.path(emuDBhandle$basePath, paste0(emuDBhandle$dbName, "_long.", "tsv"))
  if(file.exists(path2tsv)){
    long_tsv = readr::read_tsv(path2tsv, col_types = readr::cols())
    if(all(c("session", "bundle") %in% names(long_tsv))){
      res = dplyr::left_join(res, long_tsv, by = c("bundle", "session"))
    } else {
      stop(path2tsv, " doesn't only contain the columns 'key' and 'value'. Only these two columns are permitted!")
    }
  }
  
  
  return(res)
  
}

#######################
# FOR DEVELOPMENT
# library('testthat')
# test_file('tests/testthat/test_aaa_initData.R')
# test_file('tests/testthat/test_emuR-database.flatfiledata.R')

