##' testthat tests for flatfiledata
##'
context("testing database flatfiledata functions")

dbName = "ae"

path2orig = file.path(tempdir(), 
                      "emuR_demoData", 
                      paste0(dbName, emuDB.suffix))
path2testData = file.path(tempdir(), "emuR_testthat")
path2db = file.path(path2testData, 
                    paste0(dbName, emuDB.suffix))

# delete, copy and load
unlink(path2db, recursive = T)
file.copy(path2orig, 
          path2testData, 
          recursive = T)
db = load_emuDB(path2db, 
                inMemoryCache = internalVars$testingVars$inMemoryCache, 
                verbose = F)


test_that("join_tsvs works on emuDB level", {
  
  # key value emuDB data
  flat_data = tibble::tibble(key = c("location of creation", "institution"), value = c("Muenchen", "IPS"))
  
  readr::write_tsv(x = flat_data, file = file.path(db$basePath, paste0(db$dbName, "_keyValue.", "tsv")))
  
  sl = query(db, "Phonetic == S")
  
  sl_joined = join_tsvs(db, sl)
  
  expect_true(all(c("key", "value") %in% names(sl_joined)))
  
  expect_equal(sl_joined$key, c("location of creation", 
                                "institution",
                                "location of creation",
                                "institution",
                                "location of creation",
                                "institution",
                                "location of creation",
                                "institution",
                                "location of creation",
                                "institution"))
  
  expect_equal(sl_joined$value, c("Muenchen",
                                  "IPS",
                                  "Muenchen",
                                  "IPS",
                                  "Muenchen",
                                  "IPS",
                                  "Muenchen",
                                  "IPS",
                                  "Muenchen",
                                  "IPS"))
  
  # long emuDB data
  # specify session and bundle
  long_data = tibble::tibble(session = c("0000", "0000"),
                             bundle = c("msajc003", "msajc012"),
                             eyecolor = c("blue", "brown"))
  
  readr::write_tsv(x = long_data, file = file.path(db$basePath, paste0(db$dbName, "_long.", "tsv")))
  
  sl_joined = join_tsvs(db, sl)
  # 4 NAs
  expect_equal(length(which(is.na(sl_joined$eyecolor))), 4)
  
  
})