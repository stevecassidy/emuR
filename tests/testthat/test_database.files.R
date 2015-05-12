##' testthat tests for convert.TextGridCollection.to.emuDB
##'
##' @author Raphael Winkelmann
context("testing add.files.to.emuDB functions")

path2extdata = system.file("extdata", package = "emuR")

if(!is.emuDB.loaded("ae")){
  load_emuDB(paste(path2extdata, '/emu/DBs/ae/', sep = ''), verbose = F) # SIC / hardcoded!!!!!!!!!!
}

tmpDbName = 'ae_copy'


#######################################
test_that("file operations work", {
  # pre clean (just in case)
  unlink(file.path(tempdir(),tmpDbName), recursive = TRUE)
  
  # copy ae and rename
  file.copy(file.path(path2extdata, '/emu/DBs/ae/'), tempdir(), recursive = T)
  file.rename(file.path(tempdir(), 'ae'), file.path(tempdir(), 'ae_copy'))
  
  # make copy of ae to mess with (caution correct DBconfig not stored)
  fp = file.path(tempdir(), tmpDbName)
  duplicate.loaded.emuDB("ae", tmpDbName, fp)
  
  test_that("import_mediaFiles works", {
    wavPath = system.file('extdata', package='wrassp')
    import_mediaFiles(tmpDbName, dir = wavPath, targetSessionName = 'newSes', verbose = F)
    # TODO check files
  })
  
  
  # clean up
  if(is.emuDB.loaded(tmpDbName)){
    UUID = get_emuDB_UUID(dbName = tmpDbName)
    purge_emuDB(dbName = tmpDbName, dbUUID = UUID, interactive = F)
  }

})


##############################
# test_that("files are copied and added correctly", {
#   # load database 
#   if(!is.emuDB.loaded("ae")){
#     load_emuDB(path2newDB, verbose = F)
#   }
#   add.files.to.emuDB(emuDB = ae, path2rootDir = path2legacy_ae, fileExt = 'fms')
#   fmsFilePaths = list.files(path2newDB, pattern = '*.fms', recursive = T)
#   expect_equal(length(fmsFilePaths), 7)
#   expect_equal(fmsFilePaths[1], file.path('0000_ses', 'msajc003_bndl', 'msajc003.fms'))
#   
# })



