## @export
change_labels = function (emuDBhandle,
                          labels,
                          verbose = TRUE) {
  ##
  ## Resolve attribute definition names to level names
  ##
  levelNames = unlist(
    lapply(
      labels$attributeDefinition,
      get_levelNameForAttributeName,
      emuDBhandle = emuDBhandle
    )
  )
  
  ##
  ## Find the index of each attribute definition on its respective level
  ##
  labelIndex = rep (0, length(labels$attributeDefinition))
  
  for (i in seq_len(length(labelIndex))) {
    levelDefinition = get_levelDefinition(emuDBhandle, levelNames[i])
    
    for (j in seq_len(length(levelDefinition$attributeDefinitions))) {
      if (levelDefinition$attributeDefinitions[[j]]$name == labels$attributeDefinition[i]) {
        labelIndex[i] = j
      }
    }
  }

  

  ##
  ## First thing, make sure all the items whose labels are gonna be changed do exist
  ##
  statement = DBI::dbSendStatement(
    emuDBhandle$connection,
    "SELECT count(*) FROM items
    WHERE
    db_uuid = ? AND
    session = ? AND
    bundle = ? AND
    level = ? AND
    seq_idx = ?"
  )
  DBI::dbBind(
    statement,
    list(
      rep(emuDBhandle$UUID, nrow(labels)),
      labels$session,
      labels$bundle,
      levelNames,
      labels$sequenceIndex
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
    
    return (invisible(NULL))
  }
  
  statement = DBI::dbSendStatement(
    emuDBhandle$connection,
    "SELECT item_id FROM items
    WHERE
    db_uuid = ? AND
    session = ? AND
    bundle = ? AND
    level = ? AND
    seq_idx = ?"
  )
  
  DBI::dbBind(
    statement,
    list(
      rep(emuDBhandle$UUID, nrow(labels)),
      labels$session,
      labels$bundle,
      levelNames,
      labels$sequenceIndex
    )
  )
  
  item_id_list = DBI::dbFetch(statement)
  DBI::dbClearResult(statement)
  
  statement = DBI::dbSendStatement(
    emuDBhandle$connection,
    "INSERT OR REPLACE INTO labels
    (db_uuid, session, bundle, item_id, label_idx, name, label)
    VALUES (?, ?, ?, ?, ?, ?, ?)"
  )
  
  DBI::dbBind(
    statement,
    list(
      rep(emuDBhandle$UUID, nrow(labels)),
      labels$session,
      labels$bundle,
      item_id_list$item_id,
      labelIndex,
      labels$attributeDefinition,
      labels$label
    )
  )
  
  rowsAffected = DBI::dbGetRowsAffected(statement)
  DBI::dbClearResult(statement)
  
  print(paste("Updated", rowsAffected, "labels."))
  
  rewrite_allAnnots(emuDBhandle, verbose)
  
  invisible(NULL)
}
