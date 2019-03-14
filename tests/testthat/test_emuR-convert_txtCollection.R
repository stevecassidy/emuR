
# ---------------------------------------------------------------------------
context("testing convert_txtCollection")
# ---------------------------------------------------------------------------

sourceDirMain = file.path(tempdir(), "emuR_demoData")
testDir = file.path(tempdir(), "emuR_testthat")
dbName = "txt_converter_test"

# Cleaning up (just in case)
unlink(file.path(testDir, dbName), recursive = T)

# ---------------------------------------------------------------------------
# Testing with original BPFs
# ---------------------------------------------------------------------------

sourceDir = file.path(sourceDirMain, "txt_collection")
newDbFolderName = paste0(dbName, emuDB.suffix)
newDbPath = file.path(testDir, newDbFolderName) 
configPath = file.path(newDbPath, paste0(dbName, '_DBconfig.json')) 
            
# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------
test_that("testing txt collection conversion",
          {
            convert_txtCollection(sourceDir = sourceDir, 
                                  targetDir = testDir, 
                                  dbName = dbName, 
                                  verbose = F)
            
            # Format of data base.
            expect_true(newDbFolderName %in% list.dirs(testDir, 
                                                       full.names = F, 
                                                       recursive = F))
            expect_equal(length(list.files(newDbPath, 
                                           recursive = F)), 2)
            expect_equal(length(list.files(file.path(newDbPath, 
                                                     "0000_ses"), 
                                           recursive = F)), 7)
            expect_equal(length(list.files(file.path(newDbPath, 
                                                     "0000_ses", 
                                                     "msajc003_bndl"), 
                                           recursive = F)), 2)
            
            # Correctness of config file.
            dbConfigLines = readLines(configPath, warn = F)
            dbConfig = jsonlite::fromJSON(paste(dbConfigLines, 
                                                collapse = ''), simplifyVector = F)
            
            # General & webAppConfig
            expect_equal(dbConfig$name, dbName)
            expect_equal(length(dbConfig$ssffTrackDefinitions), 0)
            expect_true(dbConfig$EMUwebAppConfig$activeButtons$saveBundle)
            expect_true(dbConfig$EMUwebAppConfig$activeButtons$showHierarchy)
            
            # Check that there are no level canvas orders (bundle is item!)
            expect_equal(length(dbConfig$EMUwebAppConfig$perspectives[[1]]$levelCanvases$order), 0)
                                    
            # Check that there is one level definition (bundle)
            expect_equal(length(dbConfig$levelDefinitions), 1)
                                    
            # Check that level names and types are correct
            expect_equal(sapply(dbConfig$levelDefinitions, function(x) x$name), "bundle")
            expect_equal(sapply(dbConfig$levelDefinitions, function(x) x$type), "ITEM")
                                    
            # Check that each level has the appropriate amount of attribute definitions
            expect_equal(sapply(dbConfig$levelDefinitions, function(x) length(x$attributeDefinitions)), 2)
            expect_equal(sapply(dbConfig$levelDefinitions, function(x) x$attributeDefinitions[[1]]$name), "bundle")
            expect_equal(sapply(dbConfig$levelDefinitions, function(x) x$attributeDefinitions[[2]]$name), "transcription")
            
            # No link definitions
            expect_equal(length(dbConfig$linkDefinitions), 0)
            
            # Correctness of one annot file (msajc003_annot)
            annotPath = file.path(newDbPath, 
                                  "0000_ses", 
                                  "msajc003_bndl", 
                                  "msajc003_annot.json")
            dbAnnotLines = readLines(annotPath, warn = F)
            dbAnnot = jsonlite::fromJSON(paste(dbAnnotLines, 
                                               collapse = ''), 
                                         simplifyVector = F)
                                    
            # Check that all levels have the appropriate number of items
            expect_equal(length(dbAnnot$levels[[1]]$items), 1)
                                    
            # Check individual items
            expect_equal(dbAnnot$levels[[1]]$items[[1]]$id, 1)
            expect_equal(dbAnnot$levels[[1]]$items[[1]]$labels[[2]]$value, "amongst her friends she was considered beautiful")
                                    
            # Check that there are no links
            expect_equal(length(dbAnnot$links), 0)
            }
          )

# Cleaning up.
unlink(newDbPath, recursive = T)

# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------
 
