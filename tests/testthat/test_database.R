require(testthat)
require(compare)
require(wrassp)
require(emuR)

context("testing database functions")

.aeSampleRate=20000

.test_emu_ae_db=NULL
.test_emu_ae_db_uuid='3f627b7b-4fb5-4b4a-8c79-b5f49df4df25'
.test_emu_ae_db_dir=NULL


test_that("function get.legacy.file.path()",{
  primaryTrackFilePath=get.legacy.file.path("/path/to/db",'BLOCK*/SES*',c('BLOCK30','SES3042','0001abc'),'wav')
  expect_equal(primaryTrackFilePath,"/path/to/db/BLOCK30/SES3042/0001abc.wav")
  
  signalTrackFilePath=get.legacy.file.path("/path/to/db",'F0',c('BLOCK30','SES3042','0001abc'),'f0')
  expect_equal(signalTrackFilePath,"/path/to/db/F0/0001abc.f0")
})

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

test_that("Data types are correct",{
  
  items=dbReadTable(get_emuDBcon(),'items')
  
  expect_that(class(items[['seqIdx']]),is_equivalent_to('integer'))
  expect_that(class(items[['itemID']]),is_equivalent_to('integer'))
  expect_that(class(items[['sampleRate']]),is_equivalent_to('numeric'))
  expect_that(class(items[['samplePoint']]),is_equivalent_to('integer'))
  expect_that(class(items[['sampleStart']]),is_equivalent_to('integer'))
  expect_that(class(items[['sampleDur']]),is_equivalent_to('integer'))
  
  labels=dbReadTable(get_emuDBcon(),'labels')
  expect_that(class(labels[['labelIdx']]),is_equivalent_to('integer'))
  
  links=dbReadTable(get_emuDBcon(),'links')
  expect_that(class(links[['fromID']]),is_equivalent_to('integer'))
  expect_that(class(links[['toID']]),is_equivalent_to('integer'))
})

test_that("Test ae samples",{
  
  aeB1=get.bundle(sessionName='0000',bundleName='msajc003',dbUUID=.test_emu_ae_db_uuid)
  expect_equivalent(aeB1[['sampleRate']],.aeSampleRate)
  
  halfSample=0.5/.aeSampleRate
  msajc015_lab_values=c(0.300000,0.350276,0.425417,0.496601,0.558601,0.639601,0.663601,0.706601,0.806601,1.006101,1.085101,1.097601,1.129101,1.160101,1.213101,1.368101,1.413095,1.449550,1.464601,1.500731,1.578583,1.623228,1.653718,1.717601,1.797463,1.828601,1.903635,2.070101,2.104101,2.154601,2.200911,2.226601,2.271132,2.408601,2.502214,2.576618,2.606558,2.693704,2.749004,2.780766,2.798504,2.876593,2.958101,3.026668,3.046168,3.067703,3.123168,3.238668,3.297668,3.456899) 
  msajc015_tone_events=c(0.531305,1.486760,1.609948,2.445220,2.910929,3.110782,3.262078)
  lvCnt=length(msajc015_lab_values)
  teCnt=length(msajc015_tone_events)
  #msajc015_phonetic=ae[['items']][ae[['items']][['bundle']]=="msajc015" & ae[['items']][['level']]=='Phonetic',]
  msajc015_phonetic=dbGetQuery(get_emuDBcon(),paste0("SELECT * FROM items WHERE db_uuid='",.test_emu_ae_db_uuid,"' AND session='0000' AND bundle='msajc015' AND level='Phonetic'"))
  rc=nrow(msajc015_phonetic)
  expect_equivalent(rc+1,lvCnt)
  # order by sequence index
  msajc015_phonetic_ordered=msajc015_phonetic[order(msajc015_phonetic[['seqIdx']]),]
  rc=nrow(msajc015_phonetic_ordered)
  expect_equivalent(rc+1,lvCnt)
  
  #msajc015_tone=ae[['items']][ae[['items']][['bundle']]=="msajc015" & ae[['items']][['level']]=='Tone',]
  msajc015_tone=dbGetQuery(get_emuDBcon(),paste0("SELECT * FROM items WHERE db_uuid='",.test_emu_ae_db_uuid,"' AND session='0000' AND bundle='msajc015' AND level='Tone'"))
  msajc015_tone_ordered=msajc015_tone[order(msajc015_tone[['seqIdx']]),]
  lvSq=1:rc
  
  # check sequence
  for(i in lvSq){
   
    poSampleStart=msajc015_phonetic_ordered[i,'sampleStart']
    poSampleDur=msajc015_phonetic_ordered[i,'sampleDur']
    if(i<rc){
      poNextSampleStart=msajc015_phonetic_ordered[i+1,'sampleStart']
      # TODO
      expect_equivalent(poNextSampleStart,poSampleStart+poSampleDur+1)
      #expect_equivalent(poNextSampleStart,poSampleStart+poSampleDur+1)
    }
  }
  # check segment boundaries
  for(i in lvSq){
    lv=msajc015_lab_values[i]
    poSampleStart=msajc015_phonetic_ordered[i,'sampleStart']
    poSampleDur=msajc015_phonetic_ordered[i,'sampleDur']
    poStart=(poSampleStart+0.5)/.aeSampleRate
    absFail=abs(poStart-lv)
    # accept deviation of at least half a sample
    expect_less_than(absFail,halfSample)
  }
  # and the last value
  lv=msajc015_lab_values[lvCnt]
  poSampleEnd=msajc015_phonetic_ordered[rc,'sampleStart']+msajc015_phonetic_ordered[rc,'sampleDur']+1
  poEnd=(poSampleEnd+0.5)/.aeSampleRate
  absFail=abs(poEnd-lv)
  # accept deviation of at least half a sample
  expect_less_than(absFail,halfSample)
  
  # check tone events
  teS=1:teCnt
  for(i in teS){
    teTime=msajc015_tone_events[i]
    teLSample=msajc015_tone_ordered[i,'samplePoint']
    teLTime=teLSample/.aeSampleRate
    absFail=abs(teLTime-teTime)
    expect_less_than(absFail,halfSample)
  }
  
})

test_that("Test ae modify",{
  orgItems=dbGetQuery(get_emuDBcon(),paste0("SELECT * FROM items WHERE db_uuid='",.test_emu_ae_db_uuid,"'"))
  orgLabels=dbGetQuery(get_emuDBcon(),paste0("SELECT * FROM labels WHERE db_uuid='",.test_emu_ae_db_uuid,"'"))
  orgLinks=dbGetQuery(get_emuDBcon(),paste0("SELECT * FROM links WHERE db_uuid='",.test_emu_ae_db_uuid,"'"))
  orgLinksExt=dbGetQuery(get_emuDBcon(),paste0("SELECT * FROM linksExt WHERE db_uuid='",.test_emu_ae_db_uuid,"'"))
  
  expect_equivalent(nrow(orgItems),736)
  expect_equivalent(nrow(orgLinks),785)
  expect_equivalent(nrow(orgLinksExt),3950)
  b015=get.bundle(sessionName='0000',bundleName = 'msajc015',dbUUID = .test_emu_ae_db_uuid)
  # select arbitrary item
  b015m=b015
  phoneticLvlIt10=b015m[['levels']][['Phonetic']][['items']][[10]]
  lblOrg=phoneticLvlIt10[['labels']][[1]][['value']]
  b015m[['levels']][['Phonetic']][['items']][[10]][['labels']][[1]][['value']]='test!!'
  store.bundle.annotation(dbUUID=.test_emu_ae_db_uuid,bundle=b015m)
  
  modItems=dbGetQuery(get_emuDBcon(),paste0("SELECT * FROM items WHERE db_uuid='",.test_emu_ae_db_uuid,"'"))
  modLabels=dbGetQuery(get_emuDBcon(),paste0("SELECT * FROM labels WHERE db_uuid='",.test_emu_ae_db_uuid,"'"))
  modLinks=dbGetQuery(get_emuDBcon(),paste0("SELECT * FROM links WHERE db_uuid='",.test_emu_ae_db_uuid,"'"))
  modLinksExt=dbGetQuery(get_emuDBcon(),paste0("SELECT * FROM linksExt WHERE db_uuid='",.test_emu_ae_db_uuid,"'"))
  
  expect_equivalent(nrow(modItems),736)
  expect_equivalent(nrow(modLinks),785)
  expect_equivalent(nrow(modLinksExt),3950)
  
  # change only affects labels
  # items should be equal
  cm1=compare(orgItems,modItems,allowAll=TRUE)
  expect_true(cm1$result)
  # labels not
  cmLbls1=compare(orgLabels,modLabels,allowAll=TRUE)
  expect_false(cmLbls1$result)
  # links are not changed, should be equal to original
  cml1=compare(orgLinks,modLinks,allowAll=TRUE)
  expect_true(cml1$result)
  cmle1=compare(orgLinksExt,modLinksExt,allowAll=TRUE)
  expect_true(cmle1$result)
  
  b015m[['levels']][['Phonetic']][['items']][[10]][['sampleDur']]=99
  store.bundle.annotation(dbUUID=.test_emu_ae_db_uuid,bundle=b015m)
  
  mod2Items=dbGetQuery(get_emuDBcon(),paste0("SELECT * FROM items WHERE db_uuid='",.test_emu_ae_db_uuid,"'"))
  mod2Labels=dbGetQuery(get_emuDBcon(),paste0("SELECT * FROM labels WHERE db_uuid='",.test_emu_ae_db_uuid,"'"))
  mod2Links=dbGetQuery(get_emuDBcon(),paste0("SELECT * FROM links WHERE db_uuid='",.test_emu_ae_db_uuid,"'"))
  mod2LinksExt=dbGetQuery(get_emuDBcon(),paste0("SELECT * FROM linksExt WHERE db_uuid='",.test_emu_ae_db_uuid,"'"))
  
  expect_equivalent(nrow(mod2Items),736)
  expect_equivalent(nrow(mod2Links),785)
  expect_equivalent(nrow(mod2LinksExt),3950)
  #   
  #   # should all be equal to original 
  cm2=compare(orgItems,mod2Items,allowAll=TRUE)
  expect_false(cm2$result)
  cmLbls2=compare(orgLabels,mod2Labels,allowAll=TRUE)
  expect_false(cmLbls2$result)
  cml2=compare(orgLinks,mod2Links,allowAll=TRUE)
  expect_true(cml2$result)
  cmle2=compare(orgLinksExt,mod2LinksExt,allowAll=TRUE)
  expect_true(cmle2$result)
  
  # remove link
  b015Lks=b015m[['links']]
  b015LksM=list()
  for(b015Lk in b015Lks){
    if(!(b015Lk[['fromID']]==177 & b015Lk[['toID']]==224)){
      b015LksM[[length(b015LksM)+1]]=b015Lk
    }
  }
  b015m2=b015m
  b015m2[['links']]=b015LksM
  store.bundle.annotation(dbUUID=.test_emu_ae_db_uuid,bundle=b015m2)
  mod3Items=dbGetQuery(get_emuDBcon(),paste0("SELECT * FROM items WHERE db_uuid='",.test_emu_ae_db_uuid,"'"))
  mod3Labels=dbGetQuery(get_emuDBcon(),paste0("SELECT * FROM labels WHERE db_uuid='",.test_emu_ae_db_uuid,"'"))
  mod3Links=dbGetQuery(get_emuDBcon(),paste0("SELECT * FROM links WHERE db_uuid='",.test_emu_ae_db_uuid,"'"))
  mod3LinksExt=dbGetQuery(get_emuDBcon(),paste0("SELECT * FROM linksExt WHERE db_uuid='",.test_emu_ae_db_uuid,"'"))
  
  expect_equivalent(nrow(mod3Items),736)
  expect_equivalent(nrow(mod3Links),784)
  #expect_equivalent(nrow(mod2LinksExt),3950)
  
  cm3=compare(mod3Items,mod2Items,allowAll=TRUE)
  expect_true(cm3$result)
  cmLbls3=compare(mod2Labels,mod3Labels,allowAll=TRUE)
  expect_true(cmLbls3$result)
  cml3=compare(mod3Links,mod2Links,allowAll=TRUE)
  expect_false(cml3$result)
  cmle3=compare(mod3LinksExt,mod2LinksExt,allowAll=TRUE)
  expect_false(cmle3$result)
  
  # insert the link again
  b015m3=get.bundle(dbUUID = .test_emu_ae_db_uuid,sessionName = '0000',bundleName = 'msajc015')
  b015m3Lks=b015m3[['links']]
  b015m3Lks[[length(b015m3Lks)+1]]=list(fromID=177,toID=224)
  b015m3[['links']]=b015m3Lks
  
  store.bundle.annotation(dbUUID=.test_emu_ae_db_uuid,bundle=b015m3)
  mod4Links=dbGetQuery(get_emuDBcon(),paste0("SELECT * FROM links WHERE db_uuid='",.test_emu_ae_db_uuid,"'"))
  cml3=compare(orgLinks,mod4Links,allowAll=TRUE)
  expect_true(cml3$result)
  
  #   
  #   # TODO move segment boundaries, change links,etc...
  #   
  #   
  
#   # store original bundle
   store.bundle.annotation(dbUUID=.test_emu_ae_db_uuid,bundle=b015)
#   
   modOrgItems=dbGetQuery(get_emuDBcon(),paste0("SELECT * FROM items WHERE db_uuid='",.test_emu_ae_db_uuid,"'"))
  modOrgLabels=dbGetQuery(get_emuDBcon(),paste0("SELECT * FROM labels WHERE db_uuid='",.test_emu_ae_db_uuid,"'"))
   modOrgLinks=dbGetQuery(get_emuDBcon(),paste0("SELECT * FROM links WHERE db_uuid='",.test_emu_ae_db_uuid,"'"))
   modOrgLinksExt=dbGetQuery(get_emuDBcon(),paste0("SELECT * FROM linksExt WHERE db_uuid='",.test_emu_ae_db_uuid,"'"))

    expect_equivalent(nrow(modOrgItems),736)
  expect_equivalent(nrow(modOrgLinks),785)
  expect_equivalent(nrow(modOrgLinksExt),3950)
#   
#   # should all be equal to original 
   cm2=compare(orgItems,modOrgItems,allowAll=TRUE)
   expect_true(cm2$result)
   cmLbls2=compare(orgLabels,modOrgLabels,allowAll=TRUE)
    expect_true(cmLbls2$result)
   cml2=compare(orgLinks,modOrgLinks,allowAll=TRUE)
   expect_true(cml2$result)
   cmle2=compare(orgLinksExt,modOrgLinksExt,allowAll=TRUE)
   expect_true(cmle2$result)




  
})

# clean up
purge_emuDB(dbName='ae',dbUUID=.test_emu_ae_db_uuid,interactive=FALSE)