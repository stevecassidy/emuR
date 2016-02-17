##' testthat tests for emuRtrackdata
##'
##' @author Raphael Winkelmann
context("testing emuRtrackdata functions")

dbName = "ae"

path2orig = file.path(tempdir(), "emuR_demoData", paste0(dbName, emuDB.suffix))
ae = load_emuDB(path2orig, inMemoryCache = testingVars$inMemoryCache, verbose = F)


##############################
test_that("correct classes are returned", {
  
  sl = query(dbName, "Phonetic=@|i:")
  td = get_trackdata(ae, 
                     seglist = sl, 
                     ssffTrackName = 'fm')
  
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
#   propRes = cut_td(newTd, 0.5, prop=T)
#   print(propRes)
#   })