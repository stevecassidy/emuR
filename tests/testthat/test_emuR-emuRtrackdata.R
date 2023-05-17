##' testthat tests for emuRtrackdata
##'
context("testing emuRtrackdata functions")

dbName = "ae"

path2orig = file.path(tempdir(), 
                      "emuR_demoData", 
                      paste0(dbName, emuDB.suffix))
path2testData = file.path(tempdir(), "emuR_testthat")
path2db = file.path(path2testData, 
                    paste0(dbName, emuDB.suffix))

# extract internalVars from environment .emuR_pkgEnv
internalVars = get("internalVars", envir = .emuR_pkgEnv)

test_that("emuRtrackdata functions work", {
  # delete, copy and load
  unlink(path2db, recursive = TRUE)
  file.copy(path2orig, 
            path2testData, 
            recursive = TRUE)
  ae = load_emuDB(path2db, 
                  inMemoryCache = internalVars$testingVars$inMemoryCache, 
                  verbose = FALSE)
  
  
  ##############################
  test_that("correct classes are returned", {
    
    sl = query(ae, "Phonetic == @ | i:", 
               resultType = "emuRsegs")
    td = get_trackdata(ae, 
                       seglist = sl, 
                       ssffTrackName = 'fm',
                       resultType = "trackdata",
                       verbose = FALSE)
    
    newTd = create_emuRtrackdata(sl, td)
    
    expect_true(inherits(newTd, "emuRtrackdata"))
    
  })
  
  ##############################
  # test_that("cut works correctly", {
  #   
  #   sl = query(dbName, "Phonetic=@|i:")
  #   td = get_trackdata(dbName, 
  #                      seglist = sl, 
  #                      ssffTrackName = 'fm')
  #   
  #   newTd = create_emuRtrackdata(sl, td)
  #   
  #   propRes = cut_td(newTd, 0.5, prop=TRUE)
  #   print(propRes)
  #   })
  
  
  test_that("normalize_length returns normalized segments on tibble", {

    sl = query(ae, "Phonetic = I", 
               resultType = "tibble")
    td = get_trackdata(ae,
                       seglist = sl,
                       ssffTrackName = 'fm',
                       resultType = "tibble")
  
    N = 21
    td_norm = normalize_length(td, N = N)
    
    expect_true(nrow(td_norm[td_norm$sl_rowIdx == 1,]) == N)
    })

  test_that("normalize_length returns normalized segments on emuRtrackdata", {
    
    sl = query(ae, "Phonetic = I")
    td = get_trackdata(ae,
                       seglist = sl,
                       ssffTrackName = 'fm',
                       resultType = "emuRtrackdata")
    
    N = 21
    td_norm = normalize_length(td, N = N)
    
    expect_true(nrow(td_norm[td_norm$sl_rowIdx == 1,]) == N)
  })
  
  
  # clean up
  DBI::dbDisconnect(ae$connection)
  ae = NULL
  unlink(path2db, recursive = TRUE)
  
})


