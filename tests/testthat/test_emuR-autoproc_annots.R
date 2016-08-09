##' testthat tests for autoproc_annots
##'
context("testing autoproc_annots")

dbName = "ae"
path2orig = file.path(tempdir(), "emuR_demoData", paste0(dbName, emuDB.suffix))
path2testData = file.path(tempdir(), "emuR_testthat")
path2db = file.path(path2testData, paste0(dbName, emuDB.suffix))

# extract internalVars from environment .emuR_pkgEnv
internalVars = get("internalVars", envir = .emuR_pkgEnv)

test_that("replace_itemLabels works correctly", {
  
  # delete, copy and load
  unlink(path2db, recursive = T)
  file.copy(path2orig, path2testData, recursive = T)
  ae = load_emuDB(path2db, inMemoryCache = internalVars$testingVars$inMemoryCache, verbose = F)

  test_that("replace_itemLabels throws correct errors", {
    
    expect_error(replace_itemLabels(ae, attributeDefinitionName = "badName", origLabels = "a", newLabels = "a"), regexp = "No attributeDefinitionName: badName", ignore.case = T)
    expect_error(replace_itemLabels(ae, attributeDefinitionName = "Phonetic", origLabels = "a", newLabels = c("a","b")), regexp = "origLabels and newLabels have to be", ignore.case = T)
    expect_error(replace_itemLabels(ae, attributeDefinitionName = "Phonetic", origLabels = 1, newLabels = "a"), regexp = "origLabels and newLabels have to be", ignore.case = T) 
    expect_error(replace_itemLabels(ae, attributeDefinitionName = "Phonetic", origLabels = "a", newLabels = 1), regexp = "origLabels and newLabels have to be", ignore.case = T)
  })

  
  test_that("replace_itemLabels replaces correct labels", {
    
    replace_itemLabels(ae, attributeDefinitionName = "Phonetic", origLabels = "n", newLabels = "n_rep", verbose = F)
    
    sl = query(ae, "Phonetic == n")
    expect_equal(nrow(sl), 0)
    sl = query(ae, "Phonetic == n_rep")
    expect_equal(origAmount, 12)
    
    replace_itemLabels(ae, attributeDefinitionName = "Phonetic", origLabels = c("I", "p"), newLabels = c("I_rep", "p_rep"), verbose = F)
    
    sl = query(ae, "Phonetic == I_rep")
    expect_equal(nrow(sl), 14)
    sl = query(ae, "Phonetic == p_rep")
    expect_equal(nrow(sl), 2)
  })
  
})
