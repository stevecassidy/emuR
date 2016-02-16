require(testthat)
require(compare)
require(wrassp)
require(emuR)

context("testing database functions")

aeSampleRate=20000

# .test_emu_ae_db=NULL
# .test_emu_ae_db_uuid='3f627b7b-4fb5-4b4a-8c79-b5f49df4df25'
# .test_emu_ae_db_dir=NULL

# path2demoData = file.path(tempdir(),"emuR_demoData")
# path2testhatFolder = file.path(tempdir(),"emuR_testthat")

dbName = "ae"
path2demoData = file.path(tempdir(),"emuR_demoData")
path2orig = file.path(tempdir(), "emuR_demoData", paste0(dbName, emuDB.suffix))
path2testData = file.path(tempdir(), "emuR_testthat")
path2db = file.path(path2testData, paste0(dbName, emuDB.suffix))



test_that("database functions work", {
  
  # delete, copy and load
  unlink(path2db, recursive = T)
  unlink(file.path(path2testData, "fromLegacy"), recursive = T)
  file.copy(path2orig, path2testData, recursive = T)
  ae = load_emuDB(path2db, inMemoryCache = testingVars$inMemoryCache, verbose = F)
  
  # convert and load legacy database
  convert_legacyEmuDB(emuTplPath=file.path(path2demoData, "legacy_ae", "ae.tpl"),targetDir=file.path(path2testData, "fromLegacy"),dbUUID=ae$UUID,verbose=FALSE)
  aeFromLegacy = load_emuDB(file.path(path2testData, "fromLegacy", paste0(dbName, emuDB.suffix)), verbose = F)
  
  test_that("function get_legacyFilePath()",{
    primaryTrackFilePath=get_legacyFilePath("/path/to/db",'BLOCK*/SES*',c('BLOCK30','SES3042','0001abc'),'wav')
    expect_equal(primaryTrackFilePath,"/path/to/db/BLOCK30/SES3042/0001abc.wav")
    
    signalTrackFilePath=get_legacyFilePath("/path/to/db",'F0',c('BLOCK30','SES3042','0001abc'),'f0')
    expect_equal(signalTrackFilePath,"/path/to/db/F0/0001abc.f0")
  })
  
  
  test_that("Converted emuDB is equal to original",{
    expect_equal(ae$dbName, aeFromLegacy$dbName)
    expect_equal(ae$UUI, aeFromLegacy$UUID)
    
    origItems = dbReadTable(ae$connection, "items")
    convItems = dbReadTable(aeFromLegacy$connection, "items")
    expect_equal(origItems, convItems)
    
    origLabels = dbReadTable(ae$connection, "labels")
    convLabels = dbReadTable(aeFromLegacy$connection, "labels")
    expect_equal(origLabels, convLabels)
    
    origLinksExt = dbReadTable(ae$connection, "linksExt")
    convLinksExt = dbReadTable(aeFromLegacy$connection, "linksExt")
    expect_equal(origLinksExt, convLinksExt)
  })
  
  test_that("properties of ae are correct",{
    bp=file.path(path2testData, 'ae_emuDB')
    nbp=normalizePath(path2db)
    # DBconfig=get.database(uuid=.test_emu_ae_db_uuid)
    expect_that(ae$dbName,is_equivalent_to('ae'))
    expect_that(ae$basePath,is_equivalent_to(nbp))
    sesss=list_sessionsDBI(ae)
    expect_that(nrow(sesss),is_equivalent_to(1))
    bndls=list_bundlesDBI(ae)
    expect_that(nrow(bndls),is_equivalent_to(7))
    itCntQ=paste0("SELECT count(*) FROM items WHERE db_uuid='",ae$UUID,"'")
    itCntDf=dbGetQuery(ae$connection,itCntQ)
    itemCnt=itCntDf[[1]]
    liCntQ=paste0("SELECT count(*) FROM links WHERE db_uuid='",ae$UUID,"'")
    liCntDf=dbGetQuery(ae$connection,liCntQ)
    linkCnt=liCntDf[[1]]
    expect_that(itemCnt,is_equivalent_to(736))
    expect_that(linkCnt,is_equivalent_to(785))
  })
  
  test_that("Create emuDB from scratch works",{
    create_emuDB('create_emuDB_test1',path2testData)
    t1BasePath=file.path(path2testData,'create_emuDB_test1_emuDB')
    t1=load_emuDB(t1BasePath)
    expect_that(t1$dbName,is_equivalent_to('create_emuDB_test1'))
    unlink(t1BasePath,recursive = T)
  })
  
  test_that("Data types are correct",{
    items=dbReadTable(ae$connection, 'items')
    
    expect_that(class(items[['seqIdx']]),is_equivalent_to('integer'))
    expect_that(class(items[['itemID']]),is_equivalent_to('integer'))
    expect_that(class(items[['sampleRate']]),is_equivalent_to('numeric'))
    expect_that(class(items[['samplePoint']]),is_equivalent_to('integer'))
    expect_that(class(items[['sampleStart']]),is_equivalent_to('integer'))
    expect_that(class(items[['sampleDur']]),is_equivalent_to('integer'))
    
    labels=dbReadTable(ae$connection,'labels')
    expect_that(class(labels[['labelIdx']]),is_equivalent_to('integer'))
    
    links=dbReadTable(ae$connection,'links')
    expect_that(class(links[['fromID']]),is_equivalent_to('integer'))
    expect_that(class(links[['toID']]),is_equivalent_to('integer'))
  })
  
  test_that("Test ae samples",{
    
    # aeB1=get.bundle(sessionName='0000',bundleName='msajc003',dbUUID=.test_emu_ae_db_uuid)
    
    bundleAnnotDFs = load_bundleAnnotDFsDBI(ae, "0000", "msajc003")
    aeB1char = bundleAnnotDFsToAnnotJSONchar(ae, bundleAnnotDFs)
    aeB1 = jsonlite::fromJSON(aeB1char, simplifyVector = F)
    
    expect_equivalent(aeB1[['sampleRate']],aeSampleRate)
    
    halfSample=0.5/aeSampleRate
    msajc015_lab_values=c(0.300000,0.350276,0.425417,0.496601,0.558601,0.639601,0.663601,0.706601,0.806601,1.006101,1.085101,1.097601,1.129101,1.160101,1.213101,1.368101,1.413095,1.449550,1.464601,1.500731,1.578583,1.623228,1.653718,1.717601,1.797463,1.828601,1.903635,2.070101,2.104101,2.154601,2.200911,2.226601,2.271132,2.408601,2.502214,2.576618,2.606558,2.693704,2.749004,2.780766,2.798504,2.876593,2.958101,3.026668,3.046168,3.067703,3.123168,3.238668,3.297668,3.456899) 
    msajc015_tone_events=c(0.531305,1.486760,1.609948,2.445220,2.910929,3.110782,3.262078)
    lvCnt=length(msajc015_lab_values)
    teCnt=length(msajc015_tone_events)
    #msajc015_phonetic=ae[['items']][ae[['items']][['bundle']]=="msajc015" & ae[['items']][['level']]=='Phonetic',]
    msajc015_phonetic=dbGetQuery(ae$connection, paste0("SELECT * FROM items WHERE db_uuid='",ae$UUID,"' AND session='0000' AND bundle='msajc015' AND level='Phonetic'"))
    rc=nrow(msajc015_phonetic)
    expect_equivalent(rc+1,lvCnt)
    # order by sequence index
    msajc015_phonetic_ordered=msajc015_phonetic[order(msajc015_phonetic[['seqIdx']]),]
    rc=nrow(msajc015_phonetic_ordered)
    expect_equivalent(rc+1,lvCnt)
    
    #msajc015_tone=ae[['items']][ae[['items']][['bundle']]=="msajc015" & ae[['items']][['level']]=='Tone',]
    msajc015_tone=dbGetQuery(ae$connection, paste0("SELECT * FROM items WHERE db_uuid='", ae$UUID, "' AND session='0000' AND bundle='msajc015' AND level='Tone'"))
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
      poStart=(poSampleStart+0.5)/aeSampleRate
      absFail=abs(poStart-lv)
      # accept deviation of at least half a sample
      expect_less_than(absFail,halfSample)
    }
    # and the last value
    lv=msajc015_lab_values[lvCnt]
    poSampleEnd=msajc015_phonetic_ordered[rc,'sampleStart']+msajc015_phonetic_ordered[rc,'sampleDur']+1
    poEnd=(poSampleEnd+0.5)/aeSampleRate
    absFail=abs(poEnd-lv)
    # accept deviation of at least half a sample
    expect_less_than(absFail,halfSample)
    
    # check tone events
    teS=1:teCnt
    for(i in teS){
      teTime=msajc015_tone_events[i]
      teLSample=msajc015_tone_ordered[i,'samplePoint']
      teLTime=teLSample/aeSampleRate
      absFail=abs(teLTime-teTime)
      expect_less_than(absFail,halfSample)
    }
    
  })
  
  test_that("Test ae modify",{
    orgItems=dbGetQuery(ae$connection, paste0("SELECT * FROM items WHERE db_uuid='",ae$UUID,"'"))
    orgLabels=dbGetQuery(ae$connection,paste0("SELECT * FROM labels WHERE db_uuid='",ae$UUID,"'"))
    orgLinks=dbGetQuery(ae$connection,paste0("SELECT * FROM links WHERE db_uuid='",ae$UUID,"'"))
    orgLinksExt=dbGetQuery(ae$connection,paste0("SELECT * FROM linksExt WHERE db_uuid='",ae$UUID,"'"))
    
    expect_equivalent(nrow(orgItems),736)
    expect_equivalent(nrow(orgLinks),785)
    expect_equivalent(nrow(orgLinksExt),3950)
    # b015=get.bundle(sessionName='0000',bundleName = 'msajc015',dbUUID = .test_emu_ae_db_uuid)
    bundleAnnotDFs = load_bundleAnnotDFsDBI(ae, "0000", "msajc015")
    b015char = bundleAnnotDFsToAnnotJSONchar(ae, bundleAnnotDFs)
    b015 = jsonlite::fromJSON(b015char, simplifyVector = F, na = 'null')
    
    # select arbitrary item
    b015m=b015
    phoneticLvlIt10=b015m[['levels']][[7]][['items']][[10]]
    lblOrg=phoneticLvlIt10[['labels']][[1]][['value']]
    b015m[['levels']][[7]][['items']][[10]][['labels']][[1]][['value']]='test!!'
    
    # convert to bundleAnnotDFs
    b015mChar = jsonlite::toJSON(b015m, auto_unbox = T, pretty = T)
    bundleAnnotDFs = annotJSONcharToBundleAnnotDFs(b015mChar[1])
    remove_bundleAnnotDBI(ae, bundleName = "msajc015", "0000")
    store_bundleAnnotDFsDBI(ae, bundleAnnotDFs, "0000", "msajc015")
    # build redundat links and calc positions
    build_allRedundantLinks(ae, "0000", "msajc015")
    calculate_postionsOfLinks(ae)
    
    # store.bundle.annotation(dbUUID=.test_emu_ae_db_uuid,bundle=b015m)
    
    modItems=dbGetQuery(ae$connection, paste0("SELECT * FROM items WHERE db_uuid='",ae$UUID,"'"))
    modLabels=dbGetQuery(ae$connection, paste0("SELECT * FROM labels WHERE db_uuid='",ae$UUID,"'"))
    modLinks=dbGetQuery(ae$connection, paste0("SELECT * FROM links WHERE db_uuid='",ae$UUID,"'"))
    modLinksExt=dbGetQuery(ae$connection, paste0("SELECT * FROM linksExt WHERE db_uuid='",ae$UUID,"'"))
    
    expect_equivalent(nrow(modItems),736)
    expect_equivalent(nrow(modLinks),785)
    expect_equivalent(nrow(modLinksExt),3950)
    
    # change only affects labels
    # items should be equal
    expect_equal(orgItems,modItems)
    # labels not
    cmLbls1=compare(orgLabels,modLabels,allowAll=TRUE)
    expect_false(cmLbls1$result)
    # links are not changed, should be equal to original
    expect_equal(orgLinks,modLinks)
    expect_equal(orgLinksExt,modLinksExt)
    
    b015m[['levels']][[7]][['items']][[10]][['sampleDur']] = 99
    # store.bundle.annotation(dbUUID=.test_emu_ae_db_uuid,bundle=b015m)
    # convert to bundleAnnotDFs
    b015mChar = jsonlite::toJSON(b015m, auto_unbox = T, pretty = T)
    bundleAnnotDFs = annotJSONcharToBundleAnnotDFs(b015mChar[1])
    remove_bundleAnnotDBI(ae, bundleName = "msajc015", "0000")
    store_bundleAnnotDFsDBI(ae, bundleAnnotDFs, "0000", "msajc015")
    # build redundat links and calc positions
    build_allRedundantLinks(ae, "0000", "msajc015")
    calculate_postionsOfLinks(ae)
    
    mod2Items=dbGetQuery(ae$connection,paste0("SELECT * FROM items WHERE db_uuid='",ae$UUID,"'"))
    mod2Labels=dbGetQuery(ae$connection,paste0("SELECT * FROM labels WHERE db_uuid='",ae$UUID,"'"))
    mod2Links=dbGetQuery(ae$connection,paste0("SELECT * FROM links WHERE db_uuid='",ae$UUID,"'"))
    mod2LinksExt=dbGetQuery(ae$connection,paste0("SELECT * FROM linksExt WHERE db_uuid='",ae$UUID,"'"))
    
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
    # store.bundle.annotation(dbUUID=.test_emu_ae_db_uuid,bundle=b015m2)
    # convert to bundleAnnotDFs and store
    b015m2Char = jsonlite::toJSON(b015m2, auto_unbox = T, pretty = T)
    bundleAnnotDFs = annotJSONcharToBundleAnnotDFs(b015m2Char[1])
    remove_bundleAnnotDBI(ae, bundleName = "msajc015", "0000")
    store_bundleAnnotDFsDBI(ae, bundleAnnotDFs, "0000", "msajc015")
    # build redundat links and calc positions
    build_allRedundantLinks(ae, "0000", "msajc015")
    calculate_postionsOfLinks(ae)
    
    
    mod3Items=dbGetQuery(ae$connection,paste0("SELECT * FROM items WHERE db_uuid='",ae$UUID,"'"))
    mod3Labels=dbGetQuery(ae$connection,paste0("SELECT * FROM labels WHERE db_uuid='",ae$UUID,"'"))
    mod3Links=dbGetQuery(ae$connection,paste0("SELECT * FROM links WHERE db_uuid='",ae$UUID,"'"))
    mod3LinksExt=dbGetQuery(ae$connection,paste0("SELECT * FROM linksExt WHERE db_uuid='",ae$UUID,"'"))
    
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
    # b015m3=get.bundle(dbUUID = .test_emu_ae_db_uuid,sessionName = '0000',bundleName = 'msajc015')
    bundleAnnotDFs = load_bundleAnnotDFsDBI(ae, "0000", "msajc015")
    b015m3char = bundleAnnotDFsToAnnotJSONchar(ae, bundleAnnotDFs)
    b015m3 = jsonlite::fromJSON(b015m3char, simplifyVector = F, na = 'null')
    
    b015m3Lks=b015m3[['links']]
    b015m3Lks[[length(b015m3Lks)+1]]=list(fromID=177,toID=224)
    b015m3[['links']]=b015m3Lks
    
    # store.bundle.annotation(dbUUID=.test_emu_ae_db_uuid,bundle=b015m3)
    # convert to bundleAnnotDFs and store
    b015m3Char = jsonlite::toJSON(b015m3, auto_unbox = T, pretty = T)
    bundleAnnotDFs = annotJSONcharToBundleAnnotDFs(b015m3Char[1])
    remove_bundleAnnotDBI(ae, bundleName = "msajc015", "0000")
    store_bundleAnnotDFsDBI(ae, bundleAnnotDFs, "0000", "msajc015")
    # build redundat links and calc positions
    build_allRedundantLinks(ae, "0000", "msajc015")
    calculate_postionsOfLinks(ae)
    
    
    mod4Links=dbGetQuery(ae$connection, paste0("SELECT * FROM links WHERE db_uuid='",ae$UUID,"'"))
    cml3=compare(orgLinks,mod4Links,allowAll=TRUE)
    expect_true(cml3$result)
    
    #   
    #   # TODO move segment boundaries, change links,etc...
    #   
    #   
    
    #   # store original bundle
    
    # store.bundle.annotation(dbUUID=.test_emu_ae_db_uuid,bundle=b015)
    # convert to bundleAnnotDFs and store
    b015Char = jsonlite::toJSON(b015, auto_unbox = T, pretty = T)
    bundleAnnotDFs = annotJSONcharToBundleAnnotDFs(b015Char[1])
    remove_bundleAnnotDBI(ae, bundleName = "msajc015", "0000")
    store_bundleAnnotDFsDBI(ae, bundleAnnotDFs, "0000", "msajc015")
    # build redundat links and calc positions
    build_allRedundantLinks(ae, "0000", "msajc015")
    calculate_postionsOfLinks(ae)
    #   
    modOrgItems=dbGetQuery(ae$connection,paste0("SELECT * FROM items WHERE db_uuid='",ae$UUID,"'"))
    modOrgLabels=dbGetQuery(ae$connection,paste0("SELECT * FROM labels WHERE db_uuid='",ae$UUID,"'"))
    modOrgLinks=dbGetQuery(ae$connection,paste0("SELECT * FROM links WHERE db_uuid='",ae$UUID,"'"))
    modOrgLinksExt=dbGetQuery(ae$connection,paste0("SELECT * FROM linksExt WHERE db_uuid='",ae$UUID,"'"))
    
    expect_equivalent(nrow(modOrgItems),736)
    expect_equivalent(nrow(modOrgLinks),785)
    expect_equivalent(nrow(modOrgLinksExt),3950)
    #   
    #   # should all be equal to original 
    cm2=compare(orgItems,modOrgItems,allowAll=TRUE)
    expect_true(cm2$result)
    cmLbls2=compare(arrange(origLabels, bundle, itemID), arrange(modOrgLabels, bundle, itemID),allowAll=TRUE)
    expect_true(cmLbls2$result)
    cml2=compare(orgLinks,modOrgLinks,allowAll=TRUE)
    expect_true(cml2$result)
    cmle2=compare(orgLinksExt,modOrgLinksExt,allowAll=TRUE)
    expect_true(cmle2$result)
    
    b015ModInsrt=b015
    # insert segment
    its=b015ModInsrt[['levels']][[7]][['items']]
    
    #b$levels[['Phonetic']][['items']][[9]]
    #$id
    #[1] 193
    #
    #$sampleStart
    #[1] 16132
    #  
    #$sampleDur
    #[1] 3989
    #
    #$labels
    #$labels[[1]]
    #$labels[[1]]$name
    #[1] "Phonetic"
    #
    #$labels[[1]]$value
    #[1] "ai"
    
    # split this item:
    # shrink item to length 3500
    b015ModInsrt[['levels']][[7]][['items']][[9]]$sampleDur=3500
    b015ModInsrt[['levels']][[7]][['items']][[9]]$labels[[1]]$value='a'
    
    # shift items to the right to free index 10
    itCnt=length(b015ModInsrt[['levels']][[7]][['items']])
    shiftSeq=itCnt:10
    
    for(itIdx in shiftSeq){
      b015ModInsrt[['levels']][[7]][['items']][[itIdx+1]]=b015ModInsrt[['levels']][[7]][['items']][[itIdx]]
    }
    
    # insert item at index 10
    itLbl=list(name='Phonetic',value='i')
    itLbls=list(itLbl)
    insertIt=list(id=999,sampleStart=19633,sampleDur=488,labels=itLbls)
    b015ModInsrt[['levels']][[7]][['items']][[10]]=insertIt
    
    # store.bundle.annotation(dbUUID=.test_emu_ae_db_uuid,bundle=b015ModInsrt)
    # convert to bundleAnnotDFs and store
    b015ModInsrtChar = jsonlite::toJSON(b015ModInsrt, auto_unbox = T, pretty = T)
    bundleAnnotDFs = annotJSONcharToBundleAnnotDFs(b015ModInsrtChar[1])
    remove_bundleAnnotDBI(ae, bundleName = "msajc015", "0000")
    store_bundleAnnotDFsDBI(ae, bundleAnnotDFs, "0000", "msajc015")
    # build redundat links and calc positions
    build_allRedundantLinks(ae, "0000", "msajc015")
    calculate_postionsOfLinks(ae)
    
    # read again
    # b015Read=get.bundle(sessionName='0000',bundleName = 'msajc015',dbUUID = .test_emu_ae_db_uuid)
    bundleAnnotDFs = load_bundleAnnotDFsDBI(ae, "0000", "msajc015")
    b015ReadChar = bundleAnnotDFsToAnnotJSONchar(ae, bundleAnnotDFs)
    b015Read = jsonlite::fromJSON(b015ReadChar, simplifyVector = F, na = 'null')
    rItCnt=length(b015Read[['levels']][[7]][['items']])
    # check insert sequence
    for(itIdx in 1:9){
      expect_equal(b015[['levels']][[7]][['items']][[itIdx]][['id']],b015Read[['levels']][[7]][['items']][[itIdx]][['id']])
    }
    expect_equivalent(b015Read[['levels']][[7]][['items']][[10]]$id,999)
    
    for(itIdx in 11:rItCnt){
      expect_equal(b015[['levels']][[7]][['items']][[itIdx-1]][['id']],b015Read[['levels']][[7]][['items']][[itIdx]][['id']])
    }
    
    
  })
  # cleanup
  unlink(file.path(path2testData, "fromLegacy"), recursive = T)
  unlink(unlink(path2db, recursive = T), recursive = T)
})

test_that("store works correctly",{
  
  # delete, copy and load
  unlink(path2db, recursive = T)
  unlink(file.path(path2testData, "fromStore"), recursive = T)
  file.copy(path2orig, path2testData, recursive = T)
  ae = load_emuDB(path2db, inMemoryCache = testingVars$inMemoryCache, verbose = F)
  
  newFolderPath = file.path(path2testData, "fromStore")
  unlink(newFolderPath, recursive = T)
  store(ae, targetDir = newFolderPath, verbose = F)
  aeStored = load_emuDB(file.path(newFolderPath, "ae_emuDB"), verbose = F)
  
  aeItems=dbGetQuery(ae$connection,paste0("SELECT * FROM items WHERE db_uuid='",ae$UUID,"'"))
  aeLabels=dbGetQuery(ae$connection,paste0("SELECT * FROM labels WHERE db_uuid='",ae$UUID,"'"))
  aeLinks=dbGetQuery(ae$connection,paste0("SELECT * FROM links WHERE db_uuid='",ae$UUID,"'"))
  aeLinksExt=dbGetQuery(ae$connection,paste0("SELECT * FROM linksExt WHERE db_uuid='",ae$UUID,"'"))
  
  aeStoredItems=dbGetQuery(aeStored$connection,paste0("SELECT * FROM items WHERE db_uuid='",aeStored$UUID,"'"))
  aeStoredLabels=dbGetQuery(aeStored$connection,paste0("SELECT * FROM labels WHERE db_uuid='",aeStored$UUID,"'"))
  aeStoredLinks=dbGetQuery(aeStored$connection,paste0("SELECT * FROM links WHERE db_uuid='",aeStored$UUID,"'"))
  aeStoredLinksExt=dbGetQuery(aeStored$connection,paste0("SELECT * FROM linksExt WHERE db_uuid='",aeStored$UUID,"'"))
  
  # check that all tabels are the same
  expect_equal(aeItems, aeStoredItems)
  expect_equal(aeLabels, aeStoredLabels)
  expect_equal(aeLinks, aeStoredLinks)
  expect_equal(aeLinksExt, aeStoredLinksExt)
  
  # cleanup
  unlink(file.path(path2testData, "fromStore"), recursive = T)
})


