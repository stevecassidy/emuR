# ##' testthat tests for validation of bundles
# ##'
# ##' @author Raphael Winkelmann
context("testing validate.XXX.bundle functions")

path2demoData = file.path(tempdir(),"emuR_demoData")

path2tg = file.path(path2demoData, "TextGrid_collection/msajc003.TextGrid")

newDbName = "ae_copy"

# clean up
if(is.emuDB.loaded(newDbName)){
  UUID = get_emuDB_UUID(dbName = newDbName)
  .purge.emuDB(UUID)
}

# gereate schema from TextGrid
schema = create.DBconfig.from.TextGrid(path2tg, newDbName)
# set transient values
schema=.update.transient.schema.values(schema)
# create db object
db=create.database(name = schema[['name']],basePath = normalizePath(tempdir()),DBconfig = schema)


dbsDf=dbGetQuery(get_emuDBcon(),paste0("SELECT * FROM emuDB WHERE uuid='",schema[['UUID']],"'"))
if(nrow(dbsDf)>0){
  stop("EmuDB '",dbsDf[1,'name'],"', UUID: '",dbsDf[1,'uuid'],"' already loaded!")
}

.store.emuDB.DBI(get_emuDBcon(), db)


parse.textgrid(path2tg, 20000, dbName=newDbName, bundle="msajc003", session="0000")
dbUUID = get_emuDB_UUID(dbName = newDbName)


#################################
test_that("unaltered bundle (sqlTableRep) validates successfully", {
  res = validateBundle.emuDB.DBI(newDbName, session = "0000", bundle = "msajc003")
  expect_equal(res$type, 'SUCCESS')
  expect_equal(res$message, '')
})


