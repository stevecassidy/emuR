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

# FOR DEVELOPMENT
# library('testthat')
# test_file('tests/testthat/test_emuR-autoproc_annots.R')
