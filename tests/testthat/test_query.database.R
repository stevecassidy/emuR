require(testthat)
require(wrassp)
require(emuR)

context("emuR queries")

.test_emu_ae_db=NULL

## "private" function does not work with R CMD check !
test_load_ae_database<-function(){
  if(is.null(.test_emu_ae_db)){
    legacyDbEmuAeTpl <- system.file("extdata/legacy_emu/DBs/ae","ae.tpl", package="emuR")
    aeTmpDir=tempfile('test_emu_ae')
    convert.legacyEmuDB.to.emuDB(emuTplPath=legacyDbEmuAeTpl,targetDir=aeTmpDir,verbose=FALSE)
    .test_emu_ae_db<<-load.emuDB(file.path(aeTmpDir,'ae'),verbose=FALSE)
    #return(.test_emu_ae_db)
  } 
 return(.test_emu_ae_db)
}
test_that("Load example database ae",{

  ae=test_load_ae_database()
  expect_that(ae[['name']],is_equivalent_to('ae'))
  
})

test_that("Query labels",{
#   r1=query.with.eql(andosl,"Text = chill")
#   expect_that(nrow(r1[['items']]),equals(2))
#   
#   # sequence as seglist
#   sl1=query.seglist(andosl,"[Text=chill -> Text=wind]")
#   expect_that(nrow(sl1),equals(2))
#   expect_that(sl1[1,'labels'][['labels']],is_identical_to(I('chill->wind')))
#   expect_that(sl1[2,'labels'][['labels']],is_identical_to(I('chill->wind')))
#   expect_that(sl1[1,'utts'][['utts']],is_identical_to(I('msadb012')))
#   expect_that(sl1[2,'utts'][['utts']],is_identical_to(I('msajc012')))
#   
  
  ae=test_load_ae_database()
  #legacyDbEmuAeTpl <- system.file("extdata/legacy_emu_db/ae","ae.tpl", package="emuR")
  #expect_that(legacyDbEmuAeTpl,is_equivalent_to('/homes/klausj/DEVELOPMENT/NewEMU/emuR/inst/extdata/legacy_emu_db/ae/ae.tpl'))
  #aeTmpDir=tempfile('test_emu_ae')
  #convert.database.from.legacy.emu(emuTplPath=legacyDbEmuAeTpl,targetDir=aeTmpDir)
  #ae<<-load.database(file.path(aeTmpDir,'ae'))
  expect_that(ae[['name']],is_equivalent_to('ae'))
#   r1=query.with.eql(andosl,"Text = more")
#   expect_that(nrow(r1[['items']]),equals(8))
#   
  # sequence as seglist
  sl1=query(ae,"[Text=more -> Text=customers]",resultType='emusegs')
  expect_that(class(sl1),is_identical_to(c('emusegs','data.frame')))
  expect_that(nrow(sl1),equals(1))
  expect_that('[.data.frame'(sl1,1,'labels'),is_identical_to(I('more->customers')))
  #expect_that(labels.emusegs,is_identical_to(I('more->customers')))
  expect_that('[.data.frame'(sl1,1,'utts'),is_identical_to(I('0000:msajc057')))
})

test_that("Query label groups",{
  
  ae=test_load_ae_database()
  sl1=query(ae,"Phoneme=nasal",resultType='emusegs')
  # TODO check some items
  expect_that(nrow(sl1),equals(23))
  sl2=query(ae,"Phonetic=nasal",resultType='emusegs')
  # TODO check some items
  expect_that(nrow(sl2),equals(19))
})

# 
test_that("Query sequence",{
# #   r1=query.with.eql(andosl,"[[[Phoneme='tS' ^ Phonetic='t'] -> Phoneme=a:] -> Phoneme=n]")
# #   r1Its=r1[['items']]
# #   expect_that(nrow(r1Its),equals(1))
# #   expect_that(r1Its[1,'seqStartId'],is_identical_to('andosl_msajc020_97'))  
# #   expect_that(r1Its[1,'seqEndId'],is_identical_to('andosl_msajc020_106'))
  ae=test_load_ae_database()

  r1=query(ae,"[[[Phoneme='tS' ^ Phonetic='t'] -> Phoneme=I] -> Phoneme=l]",resultType=NULL)
  r1Its=r1[['items']]
  expect_that(nrow(r1Its),equals(1))
  expect_that(r1Its[1,'seqStartId'],is_identical_to('ae_0000_msajc012_121'))  
  expect_that(r1Its[1,'seqEndId'],is_identical_to('ae_0000_msajc012_123'))
  
  sl1=query(ae,"[[[Phoneme='tS' ^ Phonetic='t'] -> Phoneme=I] -> Phoneme=l]",resultType='emusegs')
  expect_that(nrow(sl1),equals(1))
  expect_that('[.data.frame'(sl1,1,'labels'),is_identical_to(I('tS->I->l')))
  expect_that('[.data.frame'(sl1,1,'utts'),is_identical_to(I('0000:msajc012')))
 })
# 
test_that("Query combined sequence dominance",{
# # r1=query.with.eql(andosl,"[[Syllable=W -> Syllable=W] ^ [Phoneme='n' -> Phoneme='S']]")
# # r1Its=r1[['items']]
# # expect_that(nrow(r1Its),equals(6))
# # expect_that(r1Its[1,'seqStartId'],is_identical_to('andosl_msadb020_59'))
# # expect_that(r1Its[1,'seqEndId'],is_identical_to('andosl_msadb020_68'))
# # expect_that(r1Its[2,'seqStartId'],is_identical_to('andosl_msadb058_88'))
# # expect_that(r1Its[2,'seqEndId'],is_identical_to('andosl_msadb058_97'))
# # expect_that(r1Its[3,'seqStartId'],is_identical_to('andosl_msadb063_54'))
# # expect_that(r1Its[3,'seqEndId'],is_identical_to('andosl_msadb063_64'))
# # expect_that(r1Its[4,'seqStartId'],is_identical_to('andosl_msajc020_65'))
# # expect_that(r1Its[4,'seqEndId'],is_identical_to('andosl_msajc020_76'))
# # expect_that(r1Its[5,'seqStartId'],is_identical_to('andosl_msajc058_98'))
# # expect_that(r1Its[5,'seqEndId'],is_identical_to('andosl_msajc058_109'))
# # expect_that(r1Its[6,'seqStartId'],is_identical_to('andosl_msajc063_61'))
# # expect_that(r1Its[6,'seqEndId'],is_identical_to('andosl_msajc063_73'))

ae=test_load_ae_database()
r1=query(ae,"[[Syllable=W->Syllable=W] ^ [Phoneme=@->Phoneme=s]]",resultType=NULL)
r1Its=r1[['items']]
expect_that(nrow(r1Its),equals(2))
expect_that(r1Its[1,'seqStartId'],is_identical_to('ae_0000_msajc015_131'))
expect_that(r1Its[1,'seqEndId'],is_identical_to('ae_0000_msajc015_132'))
expect_that(r1Its[2,'seqStartId'],is_identical_to('ae_0000_msajc015_141'))
expect_that(r1Its[2,'seqEndId'],is_identical_to('ae_0000_msajc015_142'))
# 
})
# 
test_that("Query using Start function",{
  ae=test_load_ae_database()
  r1=query(ae,"Phoneme = w & Start(Word, Phoneme)=1",resultType=NULL)
  
  r1Its=r1[['items']]
  expect_that(nrow(r1Its),equals(4))
  expect_that(r1Its[1,'seqStartId'],is_identical_to('ae_0000_msajc003_128'))
  expect_that(r1Its[2,'seqStartId'],is_identical_to('ae_0000_msajc012_124'))
  expect_that(r1Its[3,'seqStartId'],is_identical_to('ae_0000_msajc015_164'))
  expect_that(r1Its[4,'seqStartId'],is_identical_to('ae_0000_msajc015_177'))
  
  r2=query(ae,"Phoneme = p & Start(Word, Phoneme)=0",resultType=NULL)
  
  r2Its=r2[['items']]
  expect_that(nrow(r2Its),equals(3))
  expect_that(r2Its[1,'seqStartId'],is_identical_to('ae_0000_msajc015_147'))
  expect_that(r2Its[2,'seqStartId'],is_identical_to('ae_0000_msajc022_122'))
  expect_that(r2Its[3,'seqStartId'],is_identical_to('ae_0000_msajc057_136'))
#  
})

test_that("Query using End function",{
  ae=test_load_ae_database()
  r1=query(ae,"Phoneme = n & End(Word, Phoneme)=1",resultType=NULL)
  
  r1Its=r1[['items']]
  expect_that(nrow(r1Its),equals(2))
  expect_that(r1Its[1,'seqStartId'],is_identical_to('ae_0000_msajc023_103'))
  expect_that(r1Its[2,'seqStartId'],is_identical_to('ae_0000_msajc057_158'))

})

test_that("Query using Num function",{
  ae=test_load_ae_database()
  # query words with exactly four phonemes
  r1=query(ae,"Num(Word, Phoneme)=4",resultType=NULL)
  
  r1Its=r1[['items']]
  expect_that(nrow(r1Its),equals(6))
  #expect_that(r1Its[1,'seqStartId'],is_identical_to('ae_0000_msajc023_103'))
  #expect_that(r1Its[2,'seqStartId'],is_identical_to('ae_0000_msajc057_158'))
  
})

test_that("Query using and operator",{
  ae=test_load_ae_database()
  sl1=query(ae,'Text=them & Accent=W',resultType='emusegs')
  
  expect_that(nrow(sl1),equals(1))
  expect_that('[.data.frame'(sl1,1,'labels'),is_identical_to(I('them')))
  #expect_that(labels.emusegs,is_identical_to(I('more->customers')))
  expect_that('[.data.frame'(sl1,1,'utts'),is_identical_to(I('0000:msajc012')))
  
})

test_that("Check Phonetic tier seglist",{
  ae=test_load_ae_database()
  # load legacy emu seglist
  legacyEmuAePhoneticSeglist <- system.file("extdata/legacy_emu/seglist","legacy_emu_ae_phonetic_seglist.RData", package="emuR")
  load(file=legacyEmuAePhoneticSeglist)
  tsl=.legacy_emu_ae_phonetic_seglist
  # get original query string
  #tslQuery=emusegs.query(tsl)
  tslQuery=attr(tsl,'query')
  # reprduce the original query
  sl=query(ae,tslQuery,resultType='emusegs')
  sr=ae[['sessions']][[1]][['bundles']][[1]][['sampleRate']]
  halfSampleTime=1/sr/2
  # we have to accept numeric deviations caused by double precision calculations
  # therefore add the machine epsilon to the tolerance of a half sample 
  tolSec=halfSampleTime+.Machine[['double.eps']]
  tolMs=tolSec*1000
  # compare legacy meu generated and new seglist
  eq=equal.emusegs(sl,tsl,tolerance =tolMs,uttsPrefix2='0000:')
  expect_true(eq)
 
})

