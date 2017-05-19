change_labels = function (emuDBhandle,
                          labels,
                          verbose = TRUE) {
  
  ##
  ## Resolve attribute definition names to level names
  ##
  labelLevels = unlist(
    lapply(
      labels$attributeDefinition,
      emuR:::get_levelNameForAttributeName,
      emuDBhandle = emuDBhandle
    )
  )
  

  ##
  ## First thing, make sure all the items whose labels are gonna be changed do exist
  ##
  statement = DBI::dbSendStatement(
    emuDBhandle$connection,
    "SELECT count(*) FROM items
    WHERE
    session = ? AND
    bundle = ? AND
    level = ? AND
    seq_idx = ?"
  )
  DBI::dbBind(
    statement,
    list(
      labels$session,
      labels$bundle,
      labelLevels,
      labels$sequence_index
    )
  )
  
  existenceMatrix = DBI::dbFetch(statement)
  DBI::dbClearResult(statement)
  
  if (!all(existenceMatrix)) {
    total = nrow(labels)
    notFound = length(existenceMatrix[existenceMatrix == 0, ])
    
    print (paste(
      "Error:",
      notFound,
      "of the",
      total,
      "specified items do/does not exist"
    ))
    
    if (verbose) {
      print(labels[existenceMatrix == 0, ])
    } else {
      print ("Set verbose to TRUE to see them listed.")
    }
    
    return ()
  }
  
  statement = DBI::dbSendStatement(
    emuDBhandle$connection,
    "SELECT item_id FROM items
    WHERE
    session = ? AND
    bundle = ? AND
    level = ? AND
    seq_idx = ?"
  )
  
  DBI::dbBind(
    statement,
    list(
      labels$session,
      labels$bundle,
      labelLevels,
      labels$sequence_index
    )
  )
  
  item_id_list = DBI::dbFetch(statement)
  DBI::dbClearResult(statement)
  
  statement = DBI::dbSendStatement(
    emuDBhandle$connection,
    "UPDATE labels SET label = ?
    WHERE
    db_uuid = ? AND
    session = ? AND
    bundle = ? AND
    item_id = ? AND
    name = ?"
  )
  
  DBI::dbBind(
    statement,
    list(
      labels$label,
      rep(emuDBhandle$UUID, nrow(labels)),
      labels$session,
      labels$bundle,
      item_id_list$item_id,
      labels$attributeDefinition
    )
  )
  
  rowsAffected = DBI::dbGetRowsAffected(statement)
  DBI::dbClearResult(statement)
  
  print(paste("Updated", rowsAffected, "labels."))
  
  emuR:::rewrite_allAnnots(emuDBhandle, verbose)
  
  return ()
}