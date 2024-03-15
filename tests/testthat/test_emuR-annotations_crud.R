##' testthat tests for CRUD annotation operations
##'
context("testing CRUD annotation operations functions")

dbName = "ae"

path2orig = file.path(tempdir(), 
                      "emuR_demoData", 
                      paste0(dbName, emuDB.suffix))
path2testData = file.path(tempdir(), "emuR_testthat")
path2db = file.path(path2testData, 
                    paste0(dbName, emuDB.suffix))

# extract internalVars from environment .emuR_pkgEnv
internalVars = get("internalVars", 
                   envir = .emuR_pkgEnv)

test_that("errors are thrown on bad inputs", {
  
  # delete, copy and load
  unlink(path2db, recursive = TRUE)
  file.copy(path2orig, path2testData, recursive = TRUE)
  
  ae = load_emuDB(path2db, 
                  inMemoryCache = internalVars$testingVars$inMemoryCache, 
                  verbose = FALSE)
  
  # missing cols
  expect_error(create_itemsInLevel(ae, 
                                   itemsToCreate = data.frame(session = "", stringsAsFactors = FALSE)))
  # bad sequenceIndex type
  expect_error(create_itemsInLevel(ae, itemsToCreate = data.frame(session = "",
                                                                  bundle = "",
                                                                  level = "",
                                                                  sequenceIndex = "",
                                                                  attribute = "",
                                                                  labels = "",
                                                                  stringsAsFactors = FALSE)))
  
  # bad session / bundle
  expect_error(create_itemsInLevel(ae,
                                   itemsToCreate = data.frame(session = "0000",
                                                              bundle = "badBndlName",
                                                              level = "",
                                                              sequenceIndex = 1.5,
                                                              attribute = "",
                                                              labels = "",
                                                              stringsAsFactors = FALSE)))
  
  # existing sequence index
  expect_error(create_itemsInLevel(ae,
                                   itemsToCreate = data.frame(session = "0000",
                                                              bundle = "msajc003",
                                                              level = "Utterance",
                                                              sequenceIndex = 1,
                                                              attribute = "Utterance",
                                                              labels = "newLabel",
                                                              stringsAsFactors = FALSE),
                                   verbose = FALSE))
  
  # clean up
  DBI::dbDisconnect(ae$connection)
  ae = NULL
  
})


test_that("create_itemsInLevel in ITEM levels works as expected", {
  
  # delete, copy and load
  unlink(path2db, recursive = TRUE)
  file.copy(path2orig, 
            path2testData, 
            recursive = TRUE)
  
  ae = load_emuDB(path2db, 
                  inMemoryCache = internalVars$testingVars$inMemoryCache, 
                  verbose = FALSE)
  
  # insert new root node after existing item
  create_itemsInLevel(ae, 
                      itemsToCreate = data.frame(session = "0000",
                                                 bundle = "msajc003",
                                                 level = "Utterance",
                                                 start_item_seq_idx = 1.5,
                                                 attribute = "Utterance",
                                                 labels = "newLabel_post",
                                                 stringsAsFactors = FALSE),
                      verbose = FALSE)
  
  
  sl = query(ae, 
             "Utterance == newLabel_post", 
             calcTimes = FALSE)
  
  expect_equal(nrow(sl), 1)
  expect_equal(sl$start_item_seq_idx, 2)
  
  # insert new root node be4 existing item
  create_itemsInLevel(ae, 
                      itemsToCreate = data.frame(session = "0000",
                                                 bundle = "msajc003",
                                                 level = "Utterance",
                                                 start_item_seq_idx = 0.5,
                                                 attribute = "Utterance",
                                                 labels = "newLabel_pre",
                                                 stringsAsFactors = FALSE),
                      verbose = FALSE)
  
  
  sl = query(ae, 
             "Utterance == newLabel_pre", 
             calcTimes = FALSE)
  
  expect_equal(nrow(sl), 1)
  expect_equal(sl$start_item_seq_idx, 1)
  
  # clean up
  DBI::dbDisconnect(ae$connection)
  ae = NULL
  
})

test_that("create_itemsInLevel in EVENT levels works as expected", {
  
  # delete, copy and load
  unlink(path2db, recursive = TRUE)
  file.copy(path2orig, 
            path2testData, 
            recursive = TRUE)
  ae = load_emuDB(path2db, 
                  inMemoryCache = internalVars$testingVars$inMemoryCache, 
                  verbose = FALSE)
  
  sl = query(ae,
             "Tone =~ .*",
             resultType = "tibble")
  
  sl$labels = "new_labels"
  
  # insert new root node after existing item (should cause error as the times exist)
  expect_error(create_itemsInLevel(ae, 
                                   itemsToCreate = sl, 
                                   verbose = FALSE))
  
  sl$start = sl$start + 10 
  
  create_itemsInLevel(ae, 
                      itemsToCreate = sl, 
                      verbose = FALSE)
  
  
  sl_new = query(ae,
                 "Tone =~ .*",
                 resultType = "tibble")
  
  # twice as many
  expect_equal(2*nrow(sl), nrow(sl_new))
  
  # clean up
  DBI::dbDisconnect(ae$connection)
  ae = NULL
  
})

test_that("create_itemsInLevel in SEGMENT levels works as expected", {
  
  # delete, copy and load
  unlink(path2db, recursive = TRUE)
  file.copy(path2orig, 
            path2testData, 
            recursive = TRUE)
  ae = load_emuDB(path2db, 
                  inMemoryCache = internalVars$testingVars$inMemoryCache, 
                  verbose = FALSE)
  
  sl = query(ae,
             "Phonetic =~ .*",
             resultType = "tibble")
  
  sl$labels = "new_labels"
  # sl$sample_end = -1
  
  expect_error(create_itemsInLevel(ae, 
                                   itemsToCreate = sl, 
                                   verbose = FALSE))
  
  # add new level
  add_levelDefinition(ae, 
                      name = "new_level", 
                      type = "SEGMENT", 
                      verbose = FALSE)
  
  sl$level = "new_level"
  sl$attribute = "new_level"
  
  create_itemsInLevel(ae, 
                      itemsToCreate = sl, 
                      verbose = FALSE,
                      calculateEndTimeForSegments = FALSE)
  
  
  sl_new = query(ae,
                 "new_level =~ .*",
                 resultType = "tibble")
  
  # same nr of segs
  expect_equal(nrow(sl), nrow(sl_new))
  # same length of segs (not all coz last segment as long as )
  expect_equal(sl[1:6,]$sample_start, sl_new[1:6,]$sample_start)
  expect_equal(sl[1:6,]$sample_end, sl_new[1:6,]$sample_end)
  
  # set_levelCanvasesOrder(ae, "default", c("Phonetic", "Tone", "new_level"))
  # serve(ae)
  
  # clean up
  DBI::dbDisconnect(ae$connection)
  ae = NULL
  
})

test_that("update_itemsInLevel updates labels correctly", {
  
  # delete, copy and load
  unlink(path2db, recursive = TRUE)
  file.copy(path2orig, 
            path2testData, 
            recursive = TRUE)
  
  ae = load_emuDB(path2db, 
                  inMemoryCache = internalVars$testingVars$inMemoryCache, 
                  verbose = FALSE)
  
  sl = query(ae, 
             "[Phonetic == I ^ Syllable == S]", 
             resultType = "tibble")
  
  sl$labels = paste0(sl$labels, "_in_strong_syl")
  
  update_itemsInLevel(ae, 
                      itemsToUpdate = sl, 
                      verbose = FALSE)
  
  sl_new = query(ae, 
                 "Phonetic == I_in_strong_syl", 
                 resultType = "tibble")
  
  expect_equal(nrow(sl), nrow(sl_new))
  
  # clean up
  DBI::dbDisconnect(ae$connection)
  ae = NULL
  
})
