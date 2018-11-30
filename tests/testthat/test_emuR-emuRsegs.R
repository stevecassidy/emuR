##' testthat tests for emuRsegs/tibble
##'
context("testing emuRsegs functions")

dbName = "ae"

path2orig = file.path(tempdir(), "emuR_demoData", paste0(dbName, emuDB.suffix))
path2testData = file.path(tempdir(), "emuR_testthat")
path2db = file.path(path2testData, paste0(dbName, emuDB.suffix))

# extract internalVars from environment .emuR_pkgEnv
internalVars = get("internalVars", envir = .emuR_pkgEnv)

test_that("emuRtrackdata functions work", {
  # delete, copy and load
  unlink(path2db, recursive = T)
  file.copy(path2orig, path2testData, recursive = T)
  ae = load_emuDB(path2db, inMemoryCache = internalVars$testingVars$inMemoryCache, verbose = F)
  
  ################
  test_that("export_seglistToTxtCollection works", {
    sl_his = query(ae, "Text == his")
    export_seglistToTxtCollection(ae, 
                                  seglist = sl_his, 
                                  targetDir = tempdir())
    
    fps_wav = list.files(file.path(tempdir(), 
                                   "ae_txt_col_from_seglist"), 
                         pattern = "wav$", 
                         full.names = T)
    fps_txt = list.files(file.path(tempdir(), 
                                   "ae_txt_col_from_seglist"), 
                         pattern = "txt$",
                         full.names = T)
    file_content = readr::read_file(fps_txt[1])
    
    expect_equal(length(fps_wav), 2)
    expect_equal(length(fps_txt), 2)
    expect_equal(file_content, "his")
    
    unlink(file.path(tempdir(), 
                     "ae_txt_col_from_seglist"), 
           recursive = T)
    
  })
  
  
  # clean up
  DBI::dbDisconnect(ae$connection)
  ae = NULL
  unlink(path2db, recursive = T)
  
})