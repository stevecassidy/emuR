require(testthat)
require(compare)
require(emuR)

context("testing requeries")

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

test_that("Requery sequential",{
  
  # Phoneme sequences n->t
  sl1=query('ae',"[Phoneme=n -> Phoneme=t]")
  # requery two elemnts before and one after sequence
  rsl1=requery_seq(sl1,offset=-2,length=5)
  rsl2=requery_seq(sl1,offset=-3,length=5,offsetRef = 'END')
  
  # equivalent requery results should be equal
  crsl=compare(rsl1,rsl2,allowAll=TRUE)
  expect_true(crsl$result)
  
  expect_that(class(rsl1),is_identical_to(c('emuRsegs','emusegs','data.frame')))
  expect_that(nrow(sl1),equals(2))
  expect_that(nrow(rsl1),equals(2))
  expect_that('[.data.frame'(rsl1,1,'labels'),is_equivalent_to('l->@->n->t->l'))
  expect_that('[.data.frame'(rsl1,1,'startItemID'),equals(144))
  expect_that('[.data.frame'(rsl1,1,'endItemID'),equals(149))
  
  expect_that('[.data.frame'(rsl1,2,'labels'),is_equivalent_to('s->@->n->t->ei'))
  expect_that('[.data.frame'(rsl1,2,'startItemID'),equals(101))
  expect_that('[.data.frame'(rsl1,2,'endItemID'),equals(106))
 
})

test_that("Requery hierarchical",{
  
  # Text beginning with 'a'
  sl1=query('ae',"Text=~'a[mn].*'")
  # requery to level Phoneme
  rsl1=requery_hier(sl1,level='Phoneme')
  expect_that(class(rsl1),is_identical_to(c('emuRsegs','emusegs','data.frame')))
  expect_that(nrow(sl1),equals(3))
  expect_that(nrow(rsl1),equals(3))
  expect_that('[.data.frame'(rsl1,1,'labels'),is_equivalent_to('V->m->V->N->s->t'))
  expect_that('[.data.frame'(rsl1,1,'startItemID'),equals(114))
  expect_that('[.data.frame'(rsl1,1,'endItemID'),equals(119))
  
  expect_that('[.data.frame'(rsl1,2,'labels'),is_equivalent_to('E->n->i:'))
  expect_that('[.data.frame'(rsl1,2,'startItemID'),equals(135))
  expect_that('[.data.frame'(rsl1,2,'endItemID'),equals(137))
  
  expect_that('[.data.frame'(rsl1,3,'labels'),is_equivalent_to('@->n'))
  expect_that('[.data.frame'(rsl1,3,'startItemID'),equals(102))
  expect_that('[.data.frame'(rsl1,3,'endItemID'),equals(103))
  
})

