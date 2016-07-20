##' testthat tests for autobuild
##'
context("testing export_TextGridCollection")

dbName = "ae"
path2orig = file.path(tempdir(), "emuR_demoData", paste0(dbName, emuDB.suffix))
path2testData = file.path(tempdir(), "emuR_testthat")
path2db = file.path(path2testData, paste0(dbName, emuDB.suffix))

# extract internalVars from environment .emuR_pkgEnv
internalVars = get("internalVars", envir = .emuR_pkgEnv)

test_that("export_TextGridCollection works correctly", {

  # delete, copy and load
  unlink(path2db, recursive = T)
  file.copy(path2orig, path2testData, recursive = T)
  ae = load_emuDB(path2db, inMemoryCache = internalVars$testingVars$inMemoryCache, verbose = F)
  
  test_that("exporting every level works", {
    # preclean just in case
    unlink(file.path(path2testData, "tgCol"), recursive = T)
    
    export_TextGridCollection(ae, targetDir = file.path(path2testData, "tgCol"))
    
    
    # clean up
    # unlink(file.path(path2testData, "tgCol"), recursive = T)
  })
  
  })