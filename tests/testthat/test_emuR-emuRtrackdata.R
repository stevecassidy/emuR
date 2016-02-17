##' testthat tests for emuRtrackdata
##'
##' @author Raphael Winkelmann
context("testing emuRtrackdata functions")

dbName = "ae"

path2orig = file.path(tempdir(), "emuR_demoData", paste0(dbName, emuDB.suffix))
path2testData = file.path(tempdir(), "emuR_testthat")
path2db = file.path(path2testData, paste0(dbName, emuDB.suffix))

# delete, copy and load
unlink(path2db, recursive = T)
file.copy(path2orig, path2testData, recursive = T)
ae = load_emuDB(path2db, inMemoryCache = testingVars$inMemoryCache, verbose = F)

print(list_sessions(ae))

##############################
test_that("correct classes are returned", {
  
  sl = query(ae, "Phonetic=@|i:")
  td = get_trackdata(ae, 
                     seglist = sl, 
                     ssffTrackName = 'fm', verbose = F)
  
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