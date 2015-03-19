##' testthat tests for parse.textgrid
##'
##' @author Raphael Winkelmann
context("testing parse.textgrid function")

path2tg = system.file("extdata/legacy_emu/DBs/ae/labels/msajc003.TextGrid", package = "emuR")

sR = 20000 # sample rate for audio files of ae

itemsTableName = "emuR_emuDB_items_tmp"

labelsTableName ="emuR_emuDB_labels_tmp"

linksTableName = "emuR_emuDB_links_tmp"

# Create an ephemeral in-memory RSQLite database
con <- dbConnect(RSQLite::SQLite(), ":memory:")

initialize_database_tables(con, itemsTableName, labelsTableName, linksTableName)

parse.textgrid(path2tg, sR, db='ae', bundle="msajc003", session="0000", conn = con, itemsTableName=itemsTableName, labelsTableName=labelsTableName)

##############################
test_that("correct SEGMENT values are parsed and calculated in SQLite items table", {
  # get Phonetic table
  res <- dbSendQuery(con, paste0("SELECT * FROM ", itemsTableName," WHERE level = 'Phonetic'"))
  phoneticTbl = dbFetch(res)
  dbClearResult(res)
  
  # get labels table
  labelsTbl = dbReadTable(con, labelsTableName)
  
  expect_that(phoneticTbl[1,]$type, equals('SEGMENT'))
  
  # first segment of Phonetic
  # item[0] = {id: XYZ, labels: [{name: ‘lab', value: ‘V'}], sampleStart: 3749, sampleDur: 1389}
  expect_that(phoneticTbl[1,]$sampleStart, equals(0))

  # second segment
  expect_that(phoneticTbl[2,]$sampleStart, equals(3749))
  expect_that(phoneticTbl[2,]$sampleDur, equals(1389))
  expect_that(labelsTbl[labelsTbl$itemID == phoneticTbl[2,]$id,]$label, equals('V'))
  
  # 18th segment
  # item[16] = {id: XYZ, labels: [{name: ‘lab', value: ‘@'}], sampleStart: 30124, sampleDur: 844}
  expect_that(phoneticTbl[18,]$sampleStart, equals(30124))
  expect_that(phoneticTbl[18,]$sampleDur, equals(844))
  expect_that(labelsTbl[labelsTbl$itemID == phoneticTbl[18,]$id,]$label, equals('@'))
  
  # 35th segment
  # item[33] = {id: XYZ, labels: [{name: ‘lab', value: ‘l'}], sampleStart: 50126, sampleDur: 1962}
  expect_that(phoneticTbl[35,]$sampleStart, equals(50126))
  expect_that(phoneticTbl[35,]$sampleDur, equals(1962))
  expect_that(labelsTbl[labelsTbl$itemID == phoneticTbl[35,]$id,]$label, equals('l'))
})

##############################
test_that("correct EVENT values are parsed and calculated in SQLite items table", {

  # get Tone table
  res <- dbSendQuery(con, paste0("SELECT * FROM ", itemsTableName," WHERE level = 'Tone'"))
  toneTbl = dbFetch(res)
  dbClearResult(res)

  # get labels table
  labelsTbl = dbReadTable(con, labelsTableName)
  
  
  # first event
  # item[0] = {id: XYZ, labels: [{name: ’tone', value: ‘H*'}], samplePoint: 8381}
  expect_that(toneTbl[1,]$samplePoint, equals(8381))
  expect_that(labelsTbl[labelsTbl$itemID == toneTbl[1,]$id,]$label, equals('H*'))

  # 4th event
  # item[3] = {id: XYZ, labels: [{name: ’tone', value: ‘H*'}], samplePoint: 38255}
  expect_that(toneTbl[4,]$samplePoint, equals(38255))
  expect_that(labelsTbl[labelsTbl$itemID == toneTbl[4,]$id,]$label, equals('H*'))

  # 7th event
  # item[6] = {id: XYZ, labels: [{name: ’tone', value: ‘L%'}], samplePoint: 51552}
  expect_that(toneTbl[7,]$samplePoint, equals(51552))
  expect_that(labelsTbl[labelsTbl$itemID == toneTbl[7,]$id,]$label, equals('L%'))

})  

##############################
test_that("SEGMENTs & EVENTs have correct itemIDs in SQLite tables", {
  # get Phonetic table
  res <- dbSendQuery(con, paste0("SELECT * FROM ", itemsTableName," WHERE level = 'Phonetic'"))
  phoneticTbl = dbFetch(res)
  dbClearResult(res)
  
  # get Tone table
  res <- dbSendQuery(con, paste0("SELECT * FROM ", itemsTableName," WHERE level = 'Tone'"))
  toneTbl = dbFetch(res)
  dbClearResult(res)
  
  # get labels table
  labelsTbl = dbReadTable(con, labelsTableName)
  
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
  res <- dbSendQuery(con, paste0("SELECT * FROM ", labelsTableName, " WHERE name = 'Phonetic'"))
  phoneticsTable = dbFetch(res)
  dbClearResult(res)

  # get Tone table
  res <- dbSendQuery(con, paste0("SELECT * FROM ", labelsTableName, " WHERE name = 'Tone'"))
  toneTbl = dbFetch(res)
  dbClearResult(res)
  
  # get labels table
  labelsTbl = dbReadTable(con, labelsTableName)
  
  
  # check phoneticsTable are ok
  expect_equal(phoneticsTable[1,]$itemID, 'ae_0000_msajc003_86')
  expect_equal(phoneticsTable[1,]$session, '0000')
  expect_equal(phoneticsTable[1,]$bundle, 'msajc003')
  expect_equal(sum(phoneticsTable[1,]$labelIdx), 0)
  expect_equal(phoneticsTable[1,]$name, 'Phonetic')
  expect_equal(phoneticsTable[1,]$label, '')
  expect_equal(paste0(phoneticsTable$label, collapse = ''), 'VmVNstH@:frEnzSi:w@zkH@nsId@dbju:dH@f@l')
  
  # check toneTbl are ok
  expect_equal(toneTbl[1,]$itemID, 'ae_0000_msajc003_122')
  expect_equal(toneTbl[1,]$session, '0000')
  expect_equal(toneTbl[1,]$bundle, 'msajc003')
  expect_equal(sum(toneTbl[1,]$labelIdx), 0)
  expect_equal(toneTbl[1,]$name, 'Tone')
  expect_equal(toneTbl[1,]$label, 'H*')
  expect_equal(paste0(toneTbl$label, collapse = ''), 'H*H*L-H*H*L-L%')

})

##############################
test_that("Parsing TextGrids not of File type ooTextFile causes error", {
  tg = 'File type = "ooTextFile short"
  "TextGrid"
  '
  fileConn<-file(file.path(tempdir(), "shortTg.TextGrid"))
  writeLines(tg, fileConn)
  close(fileConn)
  
  expect_error(parse.textgrid(file.path(tempdir(), "shortTg.TextGrid"), sR, db='ae', bundle="msajc003", session="0000", conn = con, itemsTableName=itemsTableName, labelsTableName=labelsTableName))
  
})

# Disconnect from the database
dbDisconnect(con)
