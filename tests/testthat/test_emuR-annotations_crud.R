##' testthat tests for CRUD annotation operations
##'
context("testing CRUD annotation operations functions")

dbName = "ae"

path2orig = file.path(tempdir(), "emuR_demoData", paste0(dbName, emuDB.suffix))
path2testData = file.path(tempdir(), "emuR_testthat")
path2db = file.path(path2testData, paste0(dbName, emuDB.suffix))

# extract internalVars from environment .emuR_pkgEnv
internalVars = get("internalVars", envir = .emuR_pkgEnv)

test_that("errors are thrown on bad inputs", {

  # delete, copy and load
  unlink(path2db, recursive = T)
  file.copy(path2orig, path2testData, recursive = T)

  ae = load_emuDB(path2db, inMemoryCache = internalVars$testingVars$inMemoryCache, verbose = F)

  # missing cols
  expect_error(create_itemsInLevel(ae, itemsToCreate = data.frame(session = "", stringsAsFactors = F)))
  # bad sequenceIndex type
  expect_error(create_itemsInLevel(ae, itemsToCreate = data.frame(session = "",
                                                                  bundle = "",
                                                                  level = "",
                                                                  sequenceIndex = "",
                                                                  attribute = "",
                                                                  labels = "",
                                                                  stringsAsFactors = F)))

  # bad session / bundle
  expect_error(create_itemsInLevel(ae,
                                   itemsToCreate = data.frame(session = "0000",
                                                              bundle = "badBndlName",
                                                              level = "",
                                                              sequenceIndex = 1.5,
                                                              attribute = "",
                                                              labels = "",
                                                              stringsAsFactors = F)))

  # existing sequence index
  expect_error(create_itemsInLevel(ae,
                                   itemsToCreate = data.frame(session = "0000",
                                                              bundle = "msajc003",
                                                              level = "Utterance",
                                                              sequenceIndex = 1,
                                                              attribute = "Utterance",
                                                              labels = "newLabel",
                                                              stringsAsFactors = F),
                                   verbose = F))

  # clean up
  DBI::dbDisconnect(ae$connection)
  ae = NULL

})


test_that("create_itemsInLevel in ITEM levels works as expected", {

  # delete, copy and load
  unlink(path2db, recursive = T)
  file.copy(path2orig, path2testData, recursive = T)

  ae = load_emuDB(path2db, inMemoryCache = internalVars$testingVars$inMemoryCache, verbose = F)

  # insert new root node after existing item
  create_itemsInLevel(ae, itemsToCreate = data.frame(session = "0000",
                                                     bundle = "msajc003",
                                                     level = "Utterance",
                                                     start_item_seq_idx = 1.5,
                                                     attribute = "Utterance",
                                                     labels = "newLabel_post",
                                                     stringsAsFactors = F),
                      verbose = F)


  sl = query(ae, "Utterance == newLabel_post", calcTimes = F)

  expect_equal(nrow(sl), 1)
  expect_equal(sl$start_item_seq_idx, 2)

  # insert new root node be4 existing item
  create_itemsInLevel(ae, itemsToCreate = data.frame(session = "0000",
                                                     bundle = "msajc003",
                                                     level = "Utterance",
                                                     start_item_seq_idx = 0.5,
                                                     attribute = "Utterance",
                                                     labels = "newLabel_pre",
                                                     stringsAsFactors = F),
                      verbose = F)


  sl = query(ae, "Utterance == newLabel_pre", calcTimes = F)

  expect_equal(nrow(sl), 1)
  expect_equal(sl$start_item_seq_idx, 1)

  # clean up
  DBI::dbDisconnect(ae$connection)
  ae = NULL

})

test_that("create_itemsInLevel in EVENT levels works as expected", {
  
  # delete, copy and load
  unlink(path2db, recursive = T)
  file.copy(path2orig, path2testData, recursive = T)
  ae = load_emuDB(path2db, inMemoryCache = internalVars$testingVars$inMemoryCache, verbose = F)
  
  sl = query(ae,
             "Tone =~ .*",
             resultType = "tibble")
  
  sl$labels = "new_labels"
  
  # insert new root node after existing item (should cause error as the times exist)
  expect_error(create_itemsInLevel(ae, itemsToCreate = sl, verbose = F))
  
  sl$start = sl$start + 10 
  
  create_itemsInLevel(ae, itemsToCreate = sl, verbose = F)
  
  
  sl_new = query(ae,
                 "Tone =~ .*",
                 resultType = "tibble")
  
  # twice as many
  expect_equal(2*nrow(sl), nrow(sl_new))
  
  # clean up
  DBI::dbDisconnect(ae$connection)
  ae = NULL
  
})


test_that("update_itemsInLevel updates labels correctly", {

  # delete, copy and load
  unlink(path2db, recursive = T)
  file.copy(path2orig, path2testData, recursive = T)

  ae = load_emuDB(path2db, inMemoryCache = internalVars$testingVars$inMemoryCache, verbose = F)

  sl = query(ae, "[Phonetic == I ^ Syllable == S]", resultType = "tibble")

  sl$labels = paste0(sl$labels, "_in_strong_syl")

  update_itemsInLevel(ae, itemsToUpdate = sl, verbose = F)

  sl_new = query(ae, "Phonetic == I_in_strong_syl", resultType = "tibble")

  expect_equal(nrow(sl), nrow(sl_new))

  # clean up
  DBI::dbDisconnect(ae$connection)
  ae = NULL

})

