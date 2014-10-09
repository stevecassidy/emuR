##' testthat tests for parse.textgrid
##'
##' @author Raphael Winkelmann
context("testing parse.textgrid function")

path2tg = system.file("extdata/legacy_emu/DBs/ae/labels/msajc003.TextGrid", package = "emuR")

sR = 20000 # sample rate for audio files of ae

tgObj = parse.textgrid(path2tg, sR)

##############################
test_that("correct SEGMENT values are parsed and calculated", {
  # first segment
  expect_that(tgObj$Phonetic$items[[1]]$sampleStart, equals(0))
  
  # second segment
  expect_that(tgObj$Phonetic$items[[2]]$sampleStart, equals(3749))
  expect_that(tgObj$Phonetic$items[[2]]$sampleDur, equals(1389))
  expect_that(tgObj$Phonetic$items[[2]]$labels[[1]]$name, equals('Phonetic'))
  expect_that(tgObj$Phonetic$items[[2]]$labels[[1]]$value, equals('V'))
  
  # 18th segment
  expect_that(tgObj$Phonetic$items[[18]]$sampleStart, equals(30124))
  expect_that(tgObj$Phonetic$items[[18]]$sampleDur, equals(844))
  expect_that(tgObj$Phonetic$items[[18]]$labels[[1]]$name, equals('Phonetic'))
  expect_that(tgObj$Phonetic$items[[18]]$labels[[1]]$value, equals('@'))
  
  # 35th segment
  expect_that(tgObj$Phonetic$items[[35]]$sampleStart, equals(50126))
  expect_that(tgObj$Phonetic$items[[35]]$sampleDur, equals(1962))
  expect_that(tgObj$Phonetic$items[[35]]$labels[[1]]$name, equals('Phonetic'))
  expect_that(tgObj$Phonetic$items[[35]]$labels[[1]]$value, equals('l'))
  
  
})

##############################
test_that("correct EVENT values are parsed and calculated", {

  # second segment
  expect_that(tgObj$Tone$items[[1]]$samplePoint, equals(8381))
  expect_that(tgObj$Tone$items[[1]]$labels[[1]]$name, equals('Tone'))
  expect_that(tgObj$Tone$items[[1]]$labels[[1]]$value, equals('H*'))

  # 4th segment
  expect_that(tgObj$Tone$items[[4]]$samplePoint, equals(38255))
  expect_that(tgObj$Tone$items[[4]]$labels[[1]]$name, equals('Tone'))
  expect_that(tgObj$Tone$items[[4]]$labels[[1]]$value, equals('H*'))

  # 7th segment
  expect_that(tgObj$Tone$items[[7]]$samplePoint, equals(51552))
  expect_that(tgObj$Tone$items[[7]]$labels[[1]]$name, equals('Tone'))
  expect_that(tgObj$Tone$items[[7]]$labels[[1]]$value, equals('L%'))
})  