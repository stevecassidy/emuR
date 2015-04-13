##' testthat tests for parse.textgrid
##'
##' @author Raphael Winkelmann
context("testing parse.textgrid function")

path2tg = system.file("extdata/legacy_emu/DBs/ae/labels/msajc003.TextGrid", package = "emuR")

sR = 20000 # sample rate for audio files of ae

newDbName = "ae_copy"

# clean up
if(is.emuDB.loaded(newDbName)){
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


parse.textgrid(path2tg, sR, dbName=newDbName, bundle="msajc003", session="0000")
dbUUID = get.emuDB.UUID(dbName = newDbName)

##############################
test_that("correct SEGMENT values are parsed and calculated in SQLite items table", {  
  
  # get Phonetic table
  phoneticTbl <- dbGetQuery(emuDBs.con, paste0("SELECT * FROM items WHERE db_uuid='", dbUUID, "' AND level = 'Phonetic'"))
  
  # get labels table
  labelsTbl = dbGetQuery(emuDBs.con, paste0("SELECT * FROM labels WHERE db_uuid='", dbUUID,"'"))
  
  expect_that(phoneticTbl[1,]$type, equals('SEGMENT'))
  
  # first segment of Phonetic
  # item[0] = {id: XYZ, labels: [{name: ‘lab', value: ‘V'}], sampleStart: 3749, sampleDur: 1389}
  expect_that(phoneticTbl[1,]$sampleStart, equals(0))
  
  # second segment
  expect_that(phoneticTbl[2,]$sampleStart, equals(3749))
  expect_that(phoneticTbl[2,]$sampleDur, equals(1389))
  qRes = dbGetQuery(emuDBs.con, paste0("SELECT * FROM labels WHERE db_uuid ='", phoneticTbl[2,]$db_uuid, "' ",
                                       "AND session='", phoneticTbl[2,]$session, "' ",
                                       "AND bundle='", phoneticTbl[2,]$bundle, "' ",
                                       "AND itemID='", phoneticTbl[2,]$itemID, "' "))
  
  expect_that(qRes$label, equals('V'))
  
  # 18th segment
  # item[16] = {id: XYZ, labels: [{name: ‘lab', value: ‘@'}], sampleStart: 30124, sampleDur: 844}
  expect_that(phoneticTbl[18,]$sampleStart, equals(30124))
  expect_that(phoneticTbl[18,]$sampleDur, equals(844))
  qRes = dbGetQuery(emuDBs.con, paste0("SELECT * FROM labels WHERE db_uuid ='", phoneticTbl[18,]$db_uuid, "' ",
                                       "AND session='", phoneticTbl[18,]$session, "' ",
                                       "AND bundle='", phoneticTbl[18,]$bundle, "' ",
                                       "AND itemID='", phoneticTbl[18,]$itemID, "' "))
  expect_that(qRes$label, equals('@'))
  
  # 35th segment
  # item[33] = {id: XYZ, labels: [{name: ‘lab', value: ‘l'}], sampleStart: 50126, sampleDur: 1962}
  expect_that(phoneticTbl[35,]$sampleStart, equals(50126))
  expect_that(phoneticTbl[35,]$sampleDur, equals(1962))
  #   expect_that(labelsTbl[labelsTbl$itemID == phoneticTbl[35,]$id,]$label, equals('l'))
  qRes = dbGetQuery(emuDBs.con, paste0("SELECT * FROM labels WHERE db_uuid ='", phoneticTbl[35,]$db_uuid, "' ",
                                       "AND session='", phoneticTbl[35,]$session, "' ",
                                       "AND bundle='", phoneticTbl[35,]$bundle, "' ",
                                       "AND itemID='", phoneticTbl[35,]$itemID, "' "))
  expect_that(qRes$label, equals('l'))
  
})

##############################
test_that("correct EVENT values are parsed and calculated in SQLite items table", {
  
  # get Tone table
  toneTbl <- dbGetQuery(emuDBs.con, paste0("SELECT * FROM items WHERE db_uuid='", dbUUID, "' AND level = 'Tone'"))
  
  # get labels table
  labelsTbl = dbGetQuery(emuDBs.con, paste0("SELECT * FROM labels WHERE db_uuid='", dbUUID,"'"))
  
  
  # first event
  # item[0] = {id: XYZ, labels: [{name: ’tone', value: ‘H*'}], samplePoint: 8381}
  expect_that(toneTbl[1,]$samplePoint, equals(8381))
  qRes = dbGetQuery(emuDBs.con, paste0("SELECT * FROM labels WHERE db_uuid ='", toneTbl[1,]$db_uuid, "' ",
                                       "AND session='", toneTbl[1,]$session, "' ",
                                       "AND bundle='", toneTbl[1,]$bundle, "' ",
                                       "AND itemID='", toneTbl[1,]$itemID, "' "))
  
  expect_that(qRes$label, equals('H*'))
  
  # 4th event
  # item[3] = {id: XYZ, labels: [{name: ’tone', value: ‘H*'}], samplePoint: 38255}
  expect_that(toneTbl[4,]$samplePoint, equals(38255))
  qRes = dbGetQuery(emuDBs.con, paste0("SELECT * FROM labels WHERE db_uuid ='", toneTbl[4,]$db_uuid, "' ",
                                       "AND session='", toneTbl[4,]$session, "' ",
                                       "AND bundle='", toneTbl[4,]$bundle, "' ",
                                       "AND itemID='", toneTbl[4,]$itemID, "' "))
  expect_that(qRes$label, equals('H*'))
  
  # 7th event
  # item[6] = {id: XYZ, labels: [{name: ’tone', value: ‘L%'}], samplePoint: 51552}
  expect_that(toneTbl[7,]$samplePoint, equals(51552))
  qRes = dbGetQuery(emuDBs.con, paste0("SELECT * FROM labels WHERE db_uuid ='", toneTbl[7,]$db_uuid, "' ",
                                       "AND session='", toneTbl[7,]$session, "' ",
                                       "AND bundle='", toneTbl[7,]$bundle, "' ",
                                       "AND itemID='", toneTbl[7,]$itemID, "' "))
  expect_that(qRes$label, equals('L%'))
    
})  

##############################
test_that("SEGMENTs & EVENTs have correct itemIDs in SQLite tables", {
  
  # get Phonetic table
  phoneticTbl <- dbGetQuery(emuDBs.con, paste0("SELECT * FROM items WHERE db_uuid='", dbUUID, "' AND level = 'Phonetic'"))
  
  # get Tone table
  toneTbl <- dbGetQuery(emuDBs.con, paste0("SELECT * FROM items WHERE db_uuid='", dbUUID, "' AND level = 'Tone'"))
  
  # get labels table
  labelsTbl = dbGetQuery(emuDBs.con, paste0("SELECT * FROM labels WHERE db_uuid='", dbUUID,"'"))
  
  # increment IDs for EVENTs
  expect_equal(toneTbl[2,]$itemID, toneTbl[1,]$itemID + 1)
  expect_equal(toneTbl[3,]$itemID, toneTbl[2,]$itemID + 1)
  
  # increment ids for SEGMENTs
  expect_equal(phoneticTbl[2,]$itemID, phoneticTbl[1,]$itemID + 1)
  expect_equal(phoneticTbl[3,]$itemID, phoneticTbl[2,]$itemID + 1)

})


##############################
test_that("SQLite label table has correct values", {
  # get Phonetic table
  phoneticTbl <- dbGetQuery(emuDBs.con, paste0("SELECT * FROM items WHERE db_uuid='", dbUUID, "' AND level = 'Phonetic'"))
  
  # get Tone table
  toneTbl <- dbGetQuery(emuDBs.con, paste0("SELECT * FROM items WHERE db_uuid='", dbUUID, "' AND level = 'Tone'"))
  
  
  # check phoneticsTable are ok
  expect_equal(phoneticTbl[1,]$session, '0000')
  expect_equal(phoneticTbl[1,]$bundle, 'msajc003')
  expect_equal(phoneticTbl[1,]$itemID, 86)
  expect_equal(sum(phoneticTbl[1,]$labelIdx), 0)
  expect_equal(phoneticTbl[1,]$level, 'Phonetic')
  
  # check toneTbl are ok
  expect_equal(toneTbl[1,]$session, '0000')
  expect_equal(toneTbl[1,]$bundle, 'msajc003')
  expect_equal(toneTbl[1,]$itemID, 122)
  expect_equal(sum(toneTbl[1,]$labelIdx), 0)
  expect_equal(toneTbl[1,]$level, 'Tone')

  # check labelTbl
  labelsTbl = dbGetQuery(emuDBs.con, paste0("SELECT * FROM labels WHERE db_uuid='", dbUUID,"' AND name='Phonetic'"))
  expect_equal(paste0(labelsTbl$label, collapse = ''), 'VmVNstH@:frEnzSi:w@zkH@nsId@dbju:dH@f@l')
  labelsTbl = dbGetQuery(emuDBs.con, paste0("SELECT * FROM labels WHERE db_uuid='", dbUUID,"' AND name='Tone'"))
  expect_equal(paste0(labelsTbl$label, collapse = ''), 'H*H*L-H*H*L-L%')

})

