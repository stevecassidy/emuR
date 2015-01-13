# ##' testthat tests for validation of bundles
# ##'
# ##' @author Raphael Winkelmann
context("testing validate.XXX.bundle functions")

path2tg = system.file("extdata/legacy_emu/DBs/ae/labels/msajc003.TextGrid", package = "emuR")

dbd = create.DBconfig.from.TextGrid(path2tg, 'ae')

sR = 20000 # sample rate for audio files of ae

itemsTableName = "emuR_emuDB_items_tmp"

labelsTableName ="emuR_emuDB_labels_tmp"

linksTableName = "emuR_emuDB_links_tmp"

# Create an ephemeral in-memory RSQLite database
con <- dbConnect(RSQLite::SQLite(), ":memory:")

initialize_database_tables(con, itemsTableName, labelsTableName, linksTableName)

parse.textgrid(path2tg, sR, db='ae', bundle="msajc003", session="0000", conn = con, itemsTableName=itemsTableName, labelsTableName=labelsTableName)


#################################
test_that("unaltered bundle (sqlTableRep) validates successfully", {
  res = validate.sqlTableRep.bundle(dbd, "msajc003", con, itemsTableName, labelsTableName, linksTableName)
  expect_equal(res$type, 'SUCCESS')
  expect_equal(res$message, '')
})


# Disconnect from the database
dbDisconnect(con)

# 
# sampleRate = 20000
# 
# path2aeLabels = system.file("extdata/legacy_emu/DBs/ae/labels/", package = "emuR")
# 
# path2tg = paste(path2aeLabels, 'msajc003.TextGrid', sep='/')
# 
# # gereate schema from TextGrid
# dbd = create.DBconfig.from.TextGrid(path2tg, 'testhatDBconfig')
# 
# # parse TextGrid
# levels = parse.textgrid(path2tg, sampleRate)
# 
# bndlName = basename(file_path_sans_ext(path2tg))
# 
# # create bundle
# bundle = create.bundle(name = bndlName,
#                        annotates = paste0('0000_ses/', bndlName, '_bndl/', bndlName, '.wav'),
#                        sampleRate = sampleRate,
#                        levels = levels,
#                        signalpaths = list('UNKNOWN'),
#                        mediaFilePath = 'UNKNOWN',
#                        links = list())
# 
# 
# 
# 
# 
# ##############################
# test_that("unaltered bundle validates successfully", {
#   res = validate.listFrom.bundle(dbd, bundle)
#   expect_equal(res$type, 'SUCCESS')
#   expect_equal(res$message, '')
# })
# 
# ##############################
# test_that("missing level causes error", {
#   bundle$levels[[9]] = NULL
#   res = validate.listFrom.bundle(dbd, bundle)
#   expect_equal(res$type, 'ERROR')
#   expect_match(res$message, 'Following levelDefinition where not found')
# })
# 
# ##############################
# test_that("wrong level name causes error", {
#   bundle$levels[[9]]$name = 'honeticsssssss'
#   res = validate.listFrom.bundle(dbd, bundle)
#   expect_equal(res$type, 'ERROR')
#   expect_match(res$message, 'Following levels where found that do not match any levelDefinition')
# })
# 
# ##############################
# test_that("wrong level type causes error", {
#   bundle$levels[[9]]$type = 'EVENT'
#   res = validate.listFrom.bundle(dbd, bundle)
#   expect_equal(res$type, 'ERROR')
#   expect_match(res$message, 'Following level types differ from those defined')
# })
# 
# ##############################
# test_that("negative SEGMENT sampleDur causes error", {
#   bundle$levels[[6]]$items[[5]]$sampleDur = -10
#   res = validate.listFrom.bundle(dbd, bundle)
#   expect_equal(res$type, 'ERROR')
#   expect_match(res$message, 'Found sampleDur value < 0 in item')
# })
# 
# ##############################
# test_that("overlaping SEGMENTs cause error", {
#   bundle$levels[[6]]$items[[6]]$sampleStart = 29263
#   res = validate.listFrom.bundle(dbd, bundle)
#   expect_equal(res$type, 'ERROR')
#   expect_match(res$message, 'Found sampleStart <= sampleStart')
# })
# 
# ##############################
# test_that("overlaping EVENTs cause error", {
#   bundle$levels[[10]]$items[[5]]$samplePoint = 38255
#   res = validate.listFrom.bundle(dbd, bundle)
#   expect_equal(res$type, 'ERROR')
#   expect_match(res$message, 'Found samplePoint <= samplePoint ')
# })
# 
# 
