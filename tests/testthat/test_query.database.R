require(testthat)
require(wrassp)
require(emuR)

context("testing queries")

.aeSampleRate=20000

.test_emu_ae_db=NULL
.test_emu_ae_db_uuid='3f627b7b-4fb5-4b4a-8c79-b5f49df4df25'
.test_emu_ae_db_dir=NULL

# purge ae if loaded to make tests work
if(is.emuDB.loaded("ae")){
  UUID = get_emuDB_UUID(dbName = "ae")
  .purge.emuDB(UUID)
}


test_that("Purge example database ae",{
  if(is.emuDB.loaded(dbUUID=.test_emu_ae_db_uuid)){
    purge_emuDB(dbName='ae',dbUUID=.test_emu_ae_db_uuid,interactive=FALSE)
  }
})
test_that("Convert example database ae",{
  legacyDbEmuAeTpl <- system.file("extdata/legacy_emu/DBs/ae","ae.tpl", package="emuR")
  .test_emu_ae_db_dir<<-tempfile('test_emu_ae')
  convert_legacyEmuDB_to_emuDB(emuTplPath=legacyDbEmuAeTpl,targetDir=.test_emu_ae_db_dir,dbUUID=.test_emu_ae_db_uuid,verbose=FALSE)
})

test_that("Load example database ae",{  
  load_emuDB(file.path(.test_emu_ae_db_dir,'ae'),verbose=FALSE)
  #load_emuDB("/scratch/klausj/WORK/EmuDbs/ae")
  
  db=get.database(uuid=.test_emu_ae_db_uuid)
  expect_that(db[['name']],is_equivalent_to('ae'))
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
  
  #ae=test_load_ae_database()
  #legacyDbEmuAeTpl <- system.file("extdata/legacy_emu_db/ae","ae.tpl", package="emuR")
  #expect_that(legacyDbEmuAeTpl,is_equivalent_to('/homes/klausj/DEVELOPMENT/NewEMU/emuR/inst/extdata/legacy_emu_db/ae/ae.tpl'))
  #aeTmpDir=tempfile('test_emu_ae')
  #convert.database.from.legacy.emu(emuTplPath=legacyDbEmuAeTpl,targetDir=aeTmpDir)
  #ae<<-load.database(file.path(aeTmpDir,'ae'))
  #expect_that(ae[['name']],is_equivalent_to('ae'))
#   r1=query.with.eql(andosl,"Text = more")
#   expect_that(nrow(r1[['items']]),equals(8))
#   
  # sequence as seglist
  sl1=query('ae',"[Text=more -> Text=customers]",resultType='emusegs')
  expect_that(class(sl1),is_identical_to(c('emusegs','data.frame')))
  expect_that(nrow(sl1),equals(1))
  expect_that('[.data.frame'(sl1,1,'labels'),is_identical_to(I('more->customers')))
  #expect_that(labels.emusegs,is_identical_to(I('more->customers')))
  expect_that('[.data.frame'(sl1,1,'utts'),is_identical_to(I('0000:msajc057')))
})

test_that("Query label groups",{
  
  
  sl1=query('ae',"Phoneme=nasal",resultType='emusegs')
  # TODO check some items
  expect_that(nrow(sl1),equals(23))
  sl2=query('ae',"Phonetic=nasal",resultType='emusegs')
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

  r1=query('ae',"[[[Phoneme='tS' ^ Phonetic='t'] -> Phoneme=I] -> Phoneme=l]",resultType=NULL)
  r1Its=r1[['items']]
  expect_that(nrow(r1Its),equals(1))
  #expect_that(r1Its[1,'seqStartId'],is_identical_to('ae_0000_msajc012_121'))
  expect_that(r1Its[1,'db_uuid'],is_identical_to(.test_emu_ae_db_uuid))  
  expect_that(r1Its[1,'session'],is_identical_to('0000'))
  expect_that(r1Its[1,'bundle'],is_identical_to('msajc012'))  
  expect_that(r1Its[1,'seqStartId'], equals(121))  
  expect_that(r1Its[1,'seqEndId'], equals(123))
  
  sl1=query('ae',"[[[Phoneme='tS' ^ Phonetic='t'] -> Phoneme=I] -> Phoneme=l]",resultType='emusegs')
  expect_that(nrow(sl1),equals(1))
  expect_that('[.data.frame'(sl1,1,'labels'),is_identical_to(I('tS->I->l')))
  expect_that('[.data.frame'(sl1,1,'utts'),is_identical_to(I('0000:msajc012')))
 })

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

r1=query('ae',"[[Syllable=W->Syllable=W] ^ [Phoneme=@->Phoneme=s]]",resultType=NULL)
r1Its=r1[['items']]
expect_that(nrow(r1Its),equals(2))
expect_that(r1Its[1,'db_uuid'],is_identical_to(.test_emu_ae_db_uuid))  
expect_that(r1Its[1,'session'],is_identical_to('0000'))
expect_that(r1Its[1,'bundle'],is_identical_to('msajc015'))  
expect_that(r1Its[1,'seqStartId'], equals(131))
expect_that(r1Its[1,'seqEndId'], equals(132))

expect_that(r1Its[2,'db_uuid'],is_identical_to(.test_emu_ae_db_uuid))  
expect_that(r1Its[2,'session'],is_identical_to('0000'))
expect_that(r1Its[2,'bundle'],is_identical_to('msajc015'))  
expect_that(r1Its[2,'seqStartId'], equals(141))
expect_that(r1Its[2,'seqEndId'], equals(142))
# 
})
# 

test_that("Query dominance over more than one level",{
  
  r1=query('ae',"[ Syllable=S ^ Phonetic=p ]",resultType=NULL)
  r1Its=r1[['items']]
  expect_that(nrow(r1Its),equals(2))
  
})

test_that("Distinct result set for dominance query",{
  
  r1=query('ae',"[ Syllable=S ^ Phonetic=s]")
  r1Its=r1[['items']]
  expect_that(nrow(r1Its),equals(9))

})

test_that("Query using Start function",{
  r1=query('ae',"Phoneme = w & Start(Word, Phoneme)=1",resultType=NULL)
  
  r1Its=r1[['items']]
  expect_that(nrow(r1Its),equals(4))
  expect_that(r1Its[1,'db_uuid'],is_identical_to(.test_emu_ae_db_uuid))  
  expect_that(r1Its[1,'session'],is_identical_to('0000'))
  expect_that(r1Its[1,'bundle'],is_identical_to('msajc003'))  
  expect_that(r1Its[1,'seqStartId'],equals(128))
  expect_that(r1Its[2,'db_uuid'],is_identical_to(.test_emu_ae_db_uuid))  
  expect_that(r1Its[2,'session'],is_identical_to('0000'))
  expect_that(r1Its[2,'bundle'],is_identical_to('msajc012'))  
  expect_that(r1Its[2,'seqStartId'],equals(124))
  expect_that(r1Its[3,'db_uuid'],is_identical_to(.test_emu_ae_db_uuid))  
  expect_that(r1Its[3,'session'],is_identical_to('0000'))
  expect_that(r1Its[3,'bundle'],is_identical_to('msajc015'))  
  expect_that(r1Its[3,'seqStartId'],equals(164))
  expect_that(r1Its[4,'db_uuid'],is_identical_to(.test_emu_ae_db_uuid))  
  expect_that(r1Its[4,'session'],is_identical_to('0000'))
  expect_that(r1Its[4,'bundle'],is_identical_to('msajc015'))  
  expect_that(r1Its[4,'seqStartId'],equals(177))
  
  r2=query('ae',"Phoneme = p & Start(Word, Phoneme)=0",resultType=NULL)
  
  r2Its=r2[['items']]
  expect_that(nrow(r2Its),equals(3))
  expect_that(r2Its[1,'db_uuid'],is_identical_to(.test_emu_ae_db_uuid))  
  expect_that(r2Its[1,'session'],is_identical_to('0000'))
  expect_that(r2Its[1,'bundle'],is_identical_to('msajc015'))  
  expect_that(r2Its[1,'seqStartId'],equals(147))
  expect_that(r2Its[2,'db_uuid'],is_identical_to(.test_emu_ae_db_uuid))  
  expect_that(r2Its[2,'session'],is_identical_to('0000'))
  expect_that(r2Its[2,'bundle'],is_identical_to('msajc022'))  
  expect_that(r2Its[2,'seqStartId'],equals(122))
  expect_that(r2Its[3,'db_uuid'],is_identical_to(.test_emu_ae_db_uuid))  
  expect_that(r2Its[3,'session'],is_identical_to('0000'))
  expect_that(r2Its[3,'bundle'],is_identical_to('msajc057'))  
  expect_that(r2Its[3,'seqStartId'],equals(136))
  
  # and some bundle pattern tests
  r3=query('ae',"Phoneme = p & Start(Word, Phoneme)=0",bundlePattern='msajc0??',resultType=NULL)
  
  r3Its=r3[['items']]
  expect_that(nrow(r3Its),equals(3))
  expect_that(r3Its[1,'db_uuid'],is_identical_to(.test_emu_ae_db_uuid))  
  expect_that(r3Its[1,'session'],is_identical_to('0000'))
  expect_that(r3Its[1,'bundle'],is_identical_to('msajc015'))  
  expect_that(r3Its[1,'seqStartId'],equals(147))
  expect_that(r3Its[2,'db_uuid'],is_identical_to(.test_emu_ae_db_uuid))  
  expect_that(r3Its[2,'session'],is_identical_to('0000'))
  expect_that(r3Its[2,'bundle'],is_identical_to('msajc022'))
  expect_that(r3Its[2,'seqStartId'],equals(122))
  expect_that(r3Its[3,'db_uuid'],is_identical_to(.test_emu_ae_db_uuid))  
  expect_that(r3Its[3,'session'],is_identical_to('0000'))
  expect_that(r3Its[3,'bundle'],is_identical_to('msajc057'))  
  expect_that(r3Its[3,'seqStartId'],equals(136))
  
  r4=query('ae',"Phoneme = p & Start(Word, Phoneme)=0",bundlePattern='msajc02?',resultType=NULL)
  
  r4Its=r4[['items']]
  expect_that(nrow(r4Its),equals(1))
  expect_that(r4Its[1,'seqStartId'],equals(122))
  
  r5=query('ae',"Phoneme = p & Start(Word, Phoneme)=0",bundlePattern='*7',resultType=NULL)
  
  r5Its=r5[['items']]
  expect_that(nrow(r5Its),equals(1))
  expect_that(r5Its[1,'seqStartId'],equals(136))
  
 
})

test_that("Query using End function",{
  r1=query('ae',"Phoneme = n & End(Word, Phoneme)=1",resultType=NULL)
  
  r1Its=r1[['items']]
  expect_that(nrow(r1Its),equals(2))
  expect_that(r1Its[1,'seqStartId'],equals(103))
  expect_that(r1Its[2,'seqStartId'],equals(158))

})

test_that("Query using Num function",{
  
  # query words with exactly four phonemes
  r1=query('ae',"Num(Word, Phoneme)=4",resultType=NULL)
  
  r1Its=r1[['items']]
  expect_that(nrow(r1Its),equals(6))
  
})

test_that("Query using and operator",{
  sl1=query('ae','Text=them & Accent=W',resultType='emusegs')
  
  expect_that(nrow(sl1),equals(1))
  expect_that('[.data.frame'(sl1,1,'labels'),is_identical_to(I('them')))
  #expect_that(labels.emusegs,is_identical_to(I('more->customers')))
  expect_that('[.data.frame'(sl1,1,'utts'),is_identical_to(I('0000:msajc012')))
  
})

test_that("Check Phonetic tier seglist",{
  # load legacy emu seglist
  legacyEmuAePhoneticSeglist <- system.file("extdata/legacy_emu/seglist","legacy_emu_ae_phonetic_seglist.RData", package="emuR")
  load(file=legacyEmuAePhoneticSeglist)
  tsl=.legacy_emu_ae_phonetic_seglist
  # get original query string
  #tslQuery=emusegs.query(tsl)
  tslQuery=attr(tsl,'query')
  # reprduce the original query
  sl=query('ae',tslQuery,resultType='emusegs')
  #sr=ae[['sessions']][[1]][['bundles']][[1]][['sampleRate']]
  sr=.aeSampleRate
  halfSampleTime=1/sr/2
  # we have to accept numeric deviations caused by double precision calculations
  # therefore add the machine epsilon to the tolerance of a half sample 
  tolSec=halfSampleTime+.Machine[['double.eps']]
  tolMs=tolSec*1000
  # compare legacy emu generated and new seglist
  eq=equal.emusegs(sl,tsl,tolerance =tolMs,uttsPrefix2='0000:')
  expect_true(eq)
 
})

