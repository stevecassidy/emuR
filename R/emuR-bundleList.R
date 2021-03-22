##' read bundleList
##' @description read bundleList JSON file in emuDB
##' @details Read bundleList JSON file in emuDB that is stored in
##' the databases root dir sub-dir \code{bundleLists/}
##' @param emuDBhandle emuDB handle object (see \link{load_emuDB})
##' @param name name of bundleList (excluding the _bundleList.json suffix)
##' @return tibble with the columns \code{session}, \code{name}, 
##' \code{comment}, \code{finishedEditing}
##' @export
read_bundleList <- function(emuDBhandle, 
                            name){
  
  bl_dir_path = file.path(emuDBhandle$basePath, "bundleLists")
  # check if folder exists
  if(!dir.exists(bl_dir_path)){
    stop("no bundleList dir found in emuDB in dir: ", bl_dir_path)
  }
  
  bl_path = file.path(bl_dir_path, 
                      paste0(name, "_bundleList.json"))
  
  return(tibble::as_tibble(jsonlite::read_json(bl_path, simplifyVector = T)))
  
}

##' write bundleList
##' @description write bundleList JSON file to emuDB
##' @details Write bundleList JSON file to emuDB sub-dir \code{bundleLists/}
##' @param emuDBhandle emuDB handle object (see \link{load_emuDB})
##' @param name name of bundleList (excluding the _bundleList.json suffix)
##' @param bundleList tibble/data.frame with the columns \code{session}, \code{name}, 
##' \code{comment} (optional), \code{finishedEditing} (optional). Use \link{list_bundles}
##' @param seglist segment list returned by \link{query} function. If set the 
##' \code{bundleList} parameter will be ignored and a bundleList will be created by
##' collapsing the segments as timeAnchors into the \code{_bundleList.json}
##' @param updateDBconfig if set to TRUE (the default) DBconfig will be updated 
##' with the fields       
##' \itemize{
##'  \item \code{"bundleComments": true}
##'  \item \code{"bundleFinishedEditing": true}
##' }
##' @export
##' @importFrom rlang .data
write_bundleList <- function(emuDBhandle, 
                             name, 
                             bundleList, 
                             seglist, 
                             updateDBconfig = T){
  
  if(missing(name)){
    stop("name parameter must be given")
  } 
  # set path vars
  bl_dir_path = file.path(emuDBhandle$basePath, "bundleLists")
  
  if(!dir.exists(bl_dir_path)){
    print(paste0("INFO: No bundleList dir found in emuDB (path: ", bl_dir_path, ")! Creating directory..."))
    dir.create(bl_dir_path)
  }
  
  bl_path = file.path(bl_dir_path, paste0(name, "_bundleList.json"))
  
  # update DBconfig to display 
  if(is.null(DBconfig$EMUwebAppConfig$restrictions$bundleComments) || is.null(DBconfig$EMUwebAppConfig$restrictions$bundleFinishedEditing)){
    # TODO ask user to set?
    DBconfig$EMUwebAppConfig$restrictions$bundleComments = TRUE
    DBconfig$EMUwebAppConfig$restrictions$bundleFinishedEditing = TRUE
    store_DBconfig(emuDBhandle, DBconfig)
  }
  
  
  if(missing(seglist)){
    
    bundleList %>% 
      dplyr::select(.data$session, 
                    .data$name, 
                    dplyr::contains("comment"), 
                    dplyr::contains("finishedEditing")) -> bundleList
    
    # add if not available
    if(!"comment" %in% names(bundleList)){
      bundleList$comment = ""
    }
    if(!"finishedEditing" %in% names(bundleList)){
      bundleList$finishedEditing = FALSE
    }
    
    jsonlite::write_json(bundleList, bl_path, pretty = T)
  } else {
    if(!missing(bundleList)){
      warning("'bundleList' parameter is ignored as 'seglist' parameter is set")
    }
    
    dataWithTimeAnchors = list()
    distinctBundles = seglist %>% 
      dplyr::select(.data$session, .data$bundle) %>% 
      dplyr::distinct()
    
    for(i in 1:nrow(distinctBundles)){
      sesBool = distinctBundles[i,]$session == seglist$session 
      bndlBool = distinctBundles[i,]$bundle == seglist$bundle
      start_sample_vals = round(((seglist[sesBool & bndlBool,]$start / 1000) + 0.5 / seglist[sesBool & bndlBool,]$sample_rate) 
                                * seglist[sesBool & bndlBool,]$sample_rate)
      # end_sample_vals calculated with + 1 as EMU-webApp seems to always mark the right boundary left of the selected sample
      end_sample_vals = round(((seglist[sesBool & bndlBool,]$end / 1000) + 0.5/seglist[sesBool & bndlBool,]$sample_rate) * 
                                seglist[sesBool & bndlBool,]$sample_rate)
      # append to dataWithTimeAnchors
      dataWithTimeAnchors[[i]] = list(session = distinctBundles[i,]$session, 
                                      name = distinctBundles[i,]$bundle,
                                      comment = "",
                                      finishedEditing = FALSE,
                                      timeAnchors = data.frame(sample_start = start_sample_vals,
                                                               sample_end = end_sample_vals))
      
    }
    
    jsonBundleList = jsonlite::toJSON(dataWithTimeAnchors,
                                      auto_unbox = TRUE,
                                      force = TRUE,
                                      pretty = TRUE)

    writeLines(jsonBundleList, 
               bl_path, 
               useBytes = TRUE)
  }
}
