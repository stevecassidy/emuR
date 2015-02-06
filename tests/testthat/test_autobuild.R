##' testthat tests for autobuild
##'
##' @author Raphael Winkelmann
context("testing autobuild functions")

path2ae = system.file("extdata/emu/DBs/ae/", package = "emuR")

ae = load.emuDB(path2ae, verbose = F)

##############################
test_that("bad calls to autobuild.linkFromTimes", {
  
  expect_error(autobuild.linkFromTimes(ae, 'Phoneti', 'Tone'))
  expect_error(autobuild.linkFromTimes(ae, 'Phonetic', 'Ton'))
  expect_error(autobuild.linkFromTimes(ae, 'Phonetic', 'Tone'))
  
})


##############################
test_that("correct links are present after autobuild.linkFromTimes with EVENTS", {
  # add linkDef.
  tmpLevDef = create.schema.linkDefinition(type='ONE_TO_MANY', superlevelName='Phonetic', sublevelName='Tone')
  ae$DBconfig$linkDefinitions[[length(ae$DBconfig$linkDefinitions) + 1]] = tmpLevDef 
  
  res = autobuild.linkFromTimes(ae, 'Phonetic', 'Tone', FALSE)
  
  expect_equal(dim(res$links)[1], 839)
  expect_equal(res$links[786,]$session, '0000')
  expect_equal(res$links[786,]$bundle, 'msajc003')
  expect_equal(res$links[786,]$fromID, 149)
  expect_equal(res$links[786,]$toID, 181)

  expect_equal(res$links[787,]$session, '0000')
  expect_equal(res$links[787,]$bundle, 'msajc003')
  expect_equal(res$links[787,]$fromID, 156)
  expect_equal(res$links[787,]$toID, 182)
})

#############################
test_that("no duplicates are present after autobuild.linkFromTimes with EVENTs", {
  # add linkDef.
  tmpLevDef = create.schema.linkDefinition(type='ONE_TO_MANY', superlevelName='Phonetic', sublevelName='Tone')
  ae$DBconfig$linkDefinitions[[length(ae$DBconfig$linkDefinitions) + 1]] = tmpLevDef
  
  # addlink that will also be automatically linked
  ae$links[786,] = c('0000', 'msajc003', 149, 181, NA)
  res = autobuild.linkFromTimes(ae, 'Phonetic', 'Tone', FALSE)
  
  # extract only one link to be present
  expect_equal(sum(res$links$fromID == 149 & res$links$toID == 181), 1)
  
  # if re-run nothing should change (duplicate links)
  res2 = autobuild.linkFromTimes(ae, 'Phonetic', 'Tone', FALSE)
  expect_equal(dim(res$links)[1], dim(res2$links)[1])

})


##############################
test_that("correct links are present after autobuild.linkFromTimes with EVENTS", {
  # add linkDef.
  tmpLevDef = create.schema.linkDefinition(type='ONE_TO_MANY', superlevelName='Phonetic', sublevelName='Tone')
  ae$DBconfig$linkDefinitions[[length(ae$DBconfig$linkDefinitions) + 1]] = tmpLevDef 
  
  res = autobuild.linkFromTimes(ae, 'Phonetic', 'Tone', FALSE)
  
  expect_equal(dim(res$links)[1], 839)
  expect_equal(res$links[786,]$session, '0000')
  expect_equal(res$links[786,]$bundle, 'msajc003')
  expect_equal(res$links[786,]$fromID, 149)
  expect_equal(res$links[786,]$toID, 181)
  
  expect_equal(res$links[787,]$session, '0000')
  expect_equal(res$links[787,]$bundle, 'msajc003')
  expect_equal(res$links[787,]$fromID, 156)
  expect_equal(res$links[787,]$toID, 182)
})

