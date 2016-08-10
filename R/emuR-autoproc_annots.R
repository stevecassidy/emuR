##' Replace item labels
##' 
##' Replace the labels of all annotation items, or more specifially 
##' of attribute definitions belonging to annotation items, in an emuDB that 
##' match the provided \code{origLabels} character vector which the 
##' corresponding labels provided by the \code{newLabels} character vector. 
##' The indicies of the label vectors provided are used to match the labels 
##' (i.e. \code{origLabels[i]} will be replaced by \code{newLabels[i]}).
##' 
##' 
##' @param emuDBhandle emuDB handle object (see \link{load_emuDB})
##' @param attributeDefinitionName name of a attributeDefinition of a emuDB where the labels are to be 
##' replaced
##' @param origLabels character vector containing labels that are to be replaced
##' @param newLabels character vector containing labels that are to replaced the labels of \code{origLabels}. 
##' This vector has to be of equal length to the \code{origLabels} vector.
##' @param verbose Show progress bars and further information
##' @export
##' @seealso \code{\link{load_emuDB}}
##' @keywords emuDB
##' @examples
##' \dontrun{
##' 
##' ##################################
##' # prerequisite: loaded ae emuDB 
##' # (see ?load_emuDB for more information)
##' 
##' # replace all "I" and "p" labels with "I_replaced" and "p_replaced"
##' replace_itemLabels(ae, attributeDefinitionName = "Phonetic", 
##'                        origLabels = c("I", "p"), 
##'                        newLabels = c("I_replaced", "p_replaced"))
##' 
##' }
##' 
replace_itemLabels <- function(emuDBhandle, attributeDefinitionName, origLabels, newLabels, verbose = TRUE) {
  
  #############################
  # check input parameters
  
  allAttrNames = get_allAttributeNames(emuDBhandle)
  if(!attributeDefinitionName %in% allAttrNames){
    stop(paste0("No attributeDefinitionName: ", attributeDefinitionName, " found in emuDB! The available attributeNames are: ", paste0(get_allAttributeNames(emuDBhandle), collapse = "; ")))
  }
  
  if(class(origLabels) != "character" | class(newLabels) != "character" | length(origLabels) != length(newLabels)){
    stop("origLabels and newLabels have to be a character vector of the same length!")  
  }
  
  #
  #############################
  
  cat("\n  INFO: creating temporary index...\n")
  # create temp index
  DBI::dbGetQuery(emuDBhandle$connection, paste0("CREATE INDEX IF NOT EXISTS label_replace_tmp_idx ON labels(db_uuid, name, label)"))
  
  # progressbar 
  if(verbose){
    cat("\n  INFO: replacing ", length(origLabels), " attribute labels\n")
    pb <- utils::txtProgressBar(min = 0, max = length(origLabels), style = 3)
  }
  
  # transaction start
  DBI::dbBegin(emuDBhandle$connection)
  
  for(i in 1:length(origLabels)){
    DBI::dbGetQuery(emuDBhandle$connection, paste0("UPDATE labels SET label = '", newLabels[i], "' ",
                                                   "WHERE db_uuid='", emuDBhandle$UUID, "' AND name = '", attributeDefinitionName, "' AND label = '", origLabels[i], "'"))
    if(verbose){
      utils::setTxtProgressBar(pb, i)
    }
  }
  
  # transaction end
  DBI::dbCommit(emuDBhandle$connection)
  
  # remove temp index
  DBI::dbGetQuery(emuDBhandle$connection, paste0("DROP INDEX IF EXISTS label_replace_tmp_idx"))
  
  # close progress bar if open
  if(exists('pb')){
    close(pb)
    cat("\n")
  }
  
  
  rewrite_allAnnots(emuDBhandle, verbose = verbose)
  
}

##' Duplicate level
##' 
##' Duplicate level of emuDB including all of its items and its various 
##' attributeDefinitions. If the \code{duplicateLinks} variable is set 
##' to \code{TRUE} all the links to and from the original items are also 
##' duplicated.
##' 
##' @param emuDBhandle emuDB handle object (see \link{load_emuDB})
##' @param levelName name of level in emuDB that is to be duplicated
##' @param duplicateLevelName name given to newly duplicated level
##' @param duplicateLinks if set to \code{TRUE} (the default) all the
##' links to and from the original items are duplicated to point to the 
##' new items of the new duplicate level.
##' @param verbose Show progress bars and further information
##' @export
##' @seealso \code{\link{load_emuDB}}
##' @keywords emuDB
##' @examples
##' \dontrun{
##' 
##' ##################################
##' # prerequisite: loaded ae emuDB 
##' # (see ?load_emuDB for more information)
##' 
##' # duplicate Phonetic level
##' duplicate_level(ae, levelName = "Phonetic",
##'                     duplicateLevelName = "Phonetic2")
##' 
##' }
##' 
duplicate_level <- function(emuDBhandle, levelName, duplicateLevelName, 
                            duplicateLinks = TRUE, verbose = TRUE) {
  
  ldefs = list_levelDefinitions(emuDBhandle)
  
  if(!levelName %in% ldefs$name){
    stop(paste0(levelName, " is not a valid level name! Available levels are: ", paste0(ldefs$name, collapse = "; ")))
  }
  
  if(duplicateLevelName %in% ldefs$name){
    stop(paste0(duplicateLevelName, " already exists in the emuDB: ", emuDBhandle$dbName))
  }
  
  
  ldef = ldefs[ldefs$name == levelName,]
  
  #########################
  # duplicate item entries
  
  # create temp tables
  DBI::dbGetQuery(emuDBhandle$connection, "CREATE TEMP TABLE IF NOT EXISTS bndl_max_item_id_tmp (
                  db_uuid VARCHAR(36),
                  session TEXT,
                  bundle TEXT,
                  bndl_max_item_id INTEGER,
                  PRIMARY KEY (db_uuid, session, bundle)
  )")
  # create bndl_max_item_id_tmp table
  DBI::dbGetQuery(emuDBhandle$connection, paste0("INSERT INTO bndl_max_item_id_tmp ",
                                                 "SELECT db_uuid, session, bundle, max(item_id) AS bndl_max_item_id FROM items WHERE db_uuid = '", emuDBhandle$UUID, "' ",
                                                 "GROUP BY db_uuid, session, bundle"))
  # duplicate level items table elements
  DBI::dbGetQuery(emuDBhandle$connection, paste0("INSERT INTO items ",
                                                 "SELECT items.db_uuid, items.session, items.bundle, (item_id + bndl_max_item_id) AS item_id, '", duplicateLevelName, "' AS level, type, seq_idx, sample_rate, sample_point, sample_start, sample_dur ",
                                                 "FROM items, bndl_max_item_id_tmp ", 
                                                 "WHERE items.db_uuid = bndl_max_item_id_tmp.db_uuid AND items.session = bndl_max_item_id_tmp.session ",
                                                 "AND items.bundle = bndl_max_item_id_tmp.bundle AND items.level = '", levelName, "'"))
  
  ##########################
  # duplicate labels entries
  DBI::dbGetQuery(emuDBhandle$connection, paste0("INSERT INTO labels ",
                                                 "SELECT l.db_uuid, l.session, l.bundle, (l.item_id + mid.bndl_max_item_id) AS item_id, l.label_idx, ",
                                                 "CASE WHEN l.name = '", levelName, "' THEN '", duplicateLevelName, "' ELSE l.name END AS name, l.label ",
                                                 "FROM items AS it, labels AS l, bndl_max_item_id_tmp AS mid ",
                                                 "WHERE it.db_uuid = l.db_uuid AND it.session = l.session AND it.bundle = l.bundle AND it.item_id = l.item_id ",
                                                 "AND it.db_uuid = mid.db_uuid AND it.session = mid.session AND it.bundle = mid.bundle ",
                                                 "AND it.level = '", levelName, "'"))
  
  
  if(duplicateLinks){
    ##########################
    # duplicate links entries
    
    # where duplicate items are parents
    DBI::dbGetQuery(emuDBhandle$connection, paste0("INSERT INTO links ",
                                                   "SELECT li.db_uuid, li.session, li.bundle, (li.from_id + mid.bndl_max_item_id) AS from_id, li.to_id, li.label ",
                                                   "FROM items AS it, links AS li, bndl_max_item_id_tmp AS mid ",
                                                   "WHERE it.db_uuid = li.db_uuid AND it.session = li.session AND it.bundle = li.bundle AND it.item_id = li.from_id ",
                                                   "AND it.db_uuid = mid.db_uuid AND it.session = mid.session AND it.bundle = mid.bundle ",
                                                   "AND it.level = '", levelName, "'"))
    
    # where duplicate items are children
    DBI::dbGetQuery(emuDBhandle$connection, paste0("INSERT INTO links ",
                                                   "SELECT li.db_uuid, li.session, li.bundle, li.from_id, (li.to_id + mid.bndl_max_item_id) AS to_id, li.label ",
                                                   "FROM items AS it, links AS li, bndl_max_item_id_tmp AS mid ",
                                                   "WHERE it.db_uuid = li.db_uuid AND it.session = li.session AND it.bundle = li.bundle AND it.item_id = li.to_id ",
                                                   "AND it.db_uuid = mid.db_uuid AND it.session = mid.session AND it.bundle = mid.bundle ",
                                                   "AND it.level = '", levelName, "'"))
    
  }
  
  # drop temp tables
  DBI::dbGetQuery(emuDBhandle$connection, paste0("DROP TABLE IF EXISTS ", "bndl_max_item_id_tmp"))
  
  ########################
  # add levelDefs 
  add_levelDefinition(emuDBhandle, duplicateLevelName, type = ldef$type, verbose = verbose) # this also calls rewrite_allAnnots()
  
  ########################
  # add linkDefinitions
  linkDefs = list_linkDefinitions(emuDBhandle)
  # super 
  superLds = linkDefs[linkDefs$superlevelName == levelName,]
  if(nrow(superLds) > 0){
    for(i in 1:nrow(superLds)){
      add_linkDefinition(emuDBhandle, type = superLds[i,]$type, superlevelName = duplicateLevelName, sublevelName = superLds[i,]$sublevelName)
    }
  }

  # sub 
  subLds = linkDefs[linkDefs$sublevelName == levelName,]
  if(nrow(subLds) > 0){
    for(i in 1:nrow(subLds)){
      add_linkDefinition(emuDBhandle, type = subLds[i,]$type, superlevelName = subLds[i,]$superlevelName, sublevelName = duplicateLevelName)
    }
  }

  ########################
  # add attributeDefintions
  attrDefs = list_attributeDefinitions(emuDBhandle, levelName)
  if(nrow(attrDefs) > 1){
    stop("copying levels with multiple attrDefs not implemented yet!")
  }
}

# FOR DEVELOPMENT
# library('testthat')
# test_file('tests/testthat/test_emuR-autoproc_annots.R')
