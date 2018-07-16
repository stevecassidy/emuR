##' testthat tests for CRUD annotation operations
##'
context("testing CRUD annotation operations functions")

dbName = "ae"

path2orig = file.path(tempdir(), "emuR_demoData", paste0(dbName, emuDB.suffix))

# extract internalVars from environment .emuR_pkgEnv
internalVars = get("internalVars", envir = .emuR_pkgEnv)

test_that("errors are thrown", {
  
  ae = load_emuDB(path2orig, inMemoryCache = internalVars$testingVars$inMemoryCache, verbose = F)
  
  # sl = query(ae, "Utterance =~ .*", 
             # resultType = "tibble")

  
  # create_itemsInLevel(ae, )
  
})