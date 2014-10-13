##' testthat tests for validate.listFrom.bundle
##'
##' @author Raphael Winkelmann
context("testing validate.XXX.bundle functions")

sampleRate = 20000

path2aeLabels = system.file("extdata/legacy_emu/DBs/ae/labels/", package = "emuR")

path2tg = paste(path2aeLabels, 'msajc003.TextGrid', sep='/')

# gereate schema from TextGrid
dbd = create.DBconfig.from.TextGrid(path2tg, 'testhatDBconfig')

# parse TextGrid
levels = parse.textgrid(path2tg, sampleRate)

bndlName = basename(file_path_sans_ext(path2tg))

# create bundle
bundle = create.bundle(name = bndlName,
                       annotates = paste0('0000_ses/', bndlName, '_bndl/', bndlName, '.wav'),
                       sampleRate = sampleRate,
                       levels = levels,
                       signalpaths = list('UNKNOWN'),
                       mediaFilePath = 'UNKNOWN',
                       links = list())





##############################
test_that("unaltered bundle validates successfully", {
  res = validate.listFrom.bundle(dbd, bundle)
  expect_equal(res$type, 'SUCCESS')
  expect_equal(res$message, '')
})

##############################
test_that("missing level causes error", {
  bundle$levels[[9]] = NULL
  res = validate.listFrom.bundle(dbd, bundle)
  expect_equal(res$type, 'ERROR')
  expect_match(res$message, 'Following levelDefinition where not found')
})

##############################
test_that("wrong level name causes error", {
  bundle$levels[[9]]$name = 'honeticsssssss'
  res = validate.listFrom.bundle(dbd, bundle)
  expect_equal(res$type, 'ERROR')
  expect_match(res$message, 'Following levels where found that do not match any levelDefinition')
})

##############################
test_that("wrong level type causes error", {
  bundle$levels[[9]]$type = 'EVENT'
  res = validate.listFrom.bundle(dbd, bundle)
  expect_equal(res$type, 'ERROR')
  expect_match(res$message, 'Following level types differ from those defined')
})

##############################
test_that("negative SEGMENT sampleDur causes error", {
  bundle$levels[[6]]$items[[5]]$sampleDur = -10
  res = validate.listFrom.bundle(dbd, bundle)
  expect_equal(res$type, 'ERROR')
  expect_match(res$message, 'Found sampleDur value < 0 in item')
})

##############################
test_that("overlaping SEGMENTs cause error", {
  bundle$levels[[6]]$items[[6]]$sampleStart = 29263
  res = validate.listFrom.bundle(dbd, bundle)
  expect_equal(res$type, 'ERROR')
  expect_match(res$message, 'Found sampleStart <= sampleStart')
})

##############################
test_that("overlaping EVENTs cause error", {
  bundle$levels[[10]]$items[[5]]$samplePoint = 38255
  res = validate.listFrom.bundle(dbd, bundle)
  expect_equal(res$type, 'ERROR')
  expect_match(res$message, 'Found samplePoint <= samplePoint ')
})


