# ##' testthat tests for validation of bundles
# ##'
# ##' @author Raphael Winkelmann
context("testing validate.XXX.bundle functions")

path2tg = system.file("extdata/legacy_emu/DBs/ae/labels/msajc003.TextGrid", package = "emuR")

newDbName = "ae_copy"

# clean up
if(is.emuDB.loaded(tmpDbName)){
  UUID = get.emuDB.UUID(dbName = newDbName)
  .purge.emuDB(UUID)
}

# gereate schema from TextGrid
schema = create.DBconfig.from.TextGrid(path2tg, newDbName)
# set transient values
schema=.update.transient.schema.values(schema)
# create db object
db=create.database(name = schema[['name']],basePath = normalizePath(tempdir()),DBconfig = schema)

.initialize.DBI.database()
dbsDf=dbGetQuery(emuDBs.con,paste0("SELECT * FROM emuDB WHERE uuid='",schema[['UUID']],"'"))
if(nrow(dbsDf)>0){
  stop("EmuDB '",dbsDf[1,'name'],"', UUID: '",dbsDf[1,'uuid'],"' already loaded!")
}

.store.emuDB.DBI(db)


parse.textgrid(path2tg, 20000, dbName=newDbName, bundle="msajc003", session="0000")
dbUUID = get.emuDB.UUID(dbName = tmpDbName)


#################################
test_that("unaltered bundle (sqlTableRep) validates successfully", {
  res = validateBundle.emuDB.DBI(newDbName, session = "0000", bundle = "msajc003")
  expect_equal(res$type, 'SUCCESS')
  expect_equal(res$message, '')
})


