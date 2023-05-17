##' testthat tests for get_trackdata
##'
context("testing bundleList functions")

dbName = "ae"

path2orig = file.path(tempdir(), 
                      "emuR_demoData", 
                      paste0(dbName, emuDB.suffix))
path2testData = file.path(tempdir(), 
                          "emuR_testthat")
path2db = file.path(path2testData, 
                    paste0(dbName, emuDB.suffix))

# extract internalVars from environment .emuR_pkgEnv
internalVars = get("internalVars", envir = .emuR_pkgEnv)

test_that("write_bundleList with list of bundles works", {
  # delete, copy and load
  unlink(path2db, recursive = TRUE)
  file.copy(path2orig, 
            path2testData, 
            recursive = TRUE)
  ae = load_emuDB(path2db, 
                  inMemoryCache = internalVars$testingVars$inMemoryCache, 
                  verbose = FALSE)
  
  bund <- list_bundles(ae)
  write_bundleList(ae,
                   "RW",
                   bundleList = bund, 
                   updateDBconfig = TRUE)
  expect_true(dir.exists(file.path(path2db, "bundleLists")))
  expect_true(file.exists(file.path(path2db, 
                                    "bundleLists", 
                                    "RW_bundleList.json")))
  
  bl_tmp = jsonlite::read_json(file.path(path2db, 
                                         "bundleLists", 
                                         "RW_bundleList.json"), 
                               simplifyVector = TRUE)
  
  expect_true(all(dim(bl_tmp) == c(7,4)))
  
})
