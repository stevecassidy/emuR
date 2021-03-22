context("testing queries")

.aeSampleRate = 20000

.test_emu_ae_db = NULL
.test_emu_ae_db_uuid = '3f627b7b-4fb5-4b4a-8c79-b5f49df4df25'
.test_emu_ae_db_dir = NULL

path2demoData = file.path(tempdir(),
                          "emuR_demoData")
path2testhatFolder = file.path(tempdir(),
                               "emuR_testthat")

# extract internalVars from environment .emuR_pkgEnv
internalVars = get("internalVars", envir = .emuR_pkgEnv)

test_that("Convert example database ae", {
  legacyDbEmuAeTpl <- file.path(path2demoData, "legacy_ae", "ae.tpl")
  .test_emu_ae_db_dir <<- file.path(path2testhatFolder, 'test_emu_ae')
  unlink(.test_emu_ae_db_dir, recursive = T)
  convert_legacyEmuDB(emuTplPath = legacyDbEmuAeTpl,
                      targetDir = .test_emu_ae_db_dir, 
                      dbUUID = .test_emu_ae_db_uuid, 
                      verbose = FALSE)
})

test_that("Load example database ae", {  
  ae = load_emuDB(file.path(.test_emu_ae_db_dir,'ae_emuDB'), 
                  inMemoryCache = internalVars$testingVars$inMemoryCache, 
                  verbose = FALSE)
  
  dbConfig = load_DBconfig(ae)
  expect_that(dbConfig[['name']],
              is_equivalent_to('ae'))
  
  test_that("sessionPattern and bundlePattern work ",{
    # sequence as seglist
    sl1 = query(ae, "Phonetic == n", bundlePattern = "msajc003")
    
    expect_equal(unique(sl1$bundle), "msajc003")
    
    sl2 = query(ae, "Phonetic == n")
    sl3 = query(ae, "Phonetic == n", sessionPattern = "0000")
    
    expect_true(all(sl2$bundle == sl3$bundle))
    
  })
  
  test_that("Query labels",{
    # sequence as seglist
    sl1 = query(ae,"[Text == more -> Text == customers]",
                resultType = 'emusegs')
    expect_that(class(sl1),
                is_identical_to(c('emusegs', 'data.frame')))
    expect_that(nrow(sl1),
                equals(1))
    expect_that('[.data.frame'(sl1, 1, 'labels'), 
                is_identical_to(I('more->customers')))
    expect_that('[.data.frame'(sl1, 1, 'utts'),
                is_identical_to(I('0000:msajc057')))
  })
  
  
  test_that("Query level label groups",{
    
    sl1 = query(ae, 
                "Phoneme == nasal",
                resultType = 'emusegs')
    # TODO check some items
    expect_that(nrow(sl1), equals(23))
    sl2 = query(ae,
                "Phonetic == nasal",
                resultType = 'emusegs')
    # TODO check some items
    expect_that(nrow(sl2), equals(19))
  })
  
  test_that("Query database label groups",{
    
    add_labelGroup(ae, 
                   'testGroup1',
                   c('p', 'r'))
    sl1 = query(ae,
                "Phoneme == testGroup1")
    expect_that(nrow(sl1),
                equals(11))
    
  })
  
  # 
  test_that("Query sequence",{
    
    r1 = query(ae, 
               "[[[Phoneme == 'tS' ^ Phonetic == 't'] -> Phoneme == I] -> Phoneme == l]",
               resultType = NULL)
    expect_that(nrow(r1),
                equals(1))
    expect_that(r1[1, 'db_uuid'], is_identical_to(.test_emu_ae_db_uuid))  
    expect_that(r1[1,'session'], is_identical_to('0000'))
    expect_that(r1[1,'bundle'], is_identical_to('msajc012'))  
    expect_that(r1[1,'start_item_id'], equals(121))  
    expect_that(r1[1,'end_item_id'], equals(123))
    
    sl1 = query(ae,
                "[[[Phoneme == 'tS' ^ Phonetic == 't'] -> Phoneme == I] -> Phoneme == l]",
                resultType = 'emusegs')
    expect_that(nrow(sl1), equals(1))
    expect_that('[.data.frame'(sl1, 1, 'labels'),
                is_identical_to(I('tS->I->l')))
    expect_that('[.data.frame'(sl1, 1, 'utts'),
                is_identical_to(I('0000:msajc012')))
    
    sl2 = query(ae, 
                "[Phoneme == n -> Phoneme == t]")
    expect_that(sl2$end_item_seq_idx[1], 
                equals(sl2$start_item_seq_idx[1] + 1))
    
  })
  
  test_that("Query combined sequence dominance",{
    
    r1 = query(ae,
               "[[Syllable == W -> Syllable == W] ^ [Phoneme == @ -> Phoneme == s]]",
               resultType = NULL)
    expect_that(nrow(r1),
                equals(2))
    expect_that(r1[1, 'db_uuid'],
                is_identical_to(.test_emu_ae_db_uuid))  
    expect_that(r1[1, 'session'], is_identical_to('0000'))
    expect_that(r1[1, 'bundle'], is_identical_to('msajc015'))  
    expect_that(r1[1, 'start_item_id'], equals(131))
    expect_that(r1[1, 'end_item_id'], equals(132))
    
    expect_that(r1[2, 'db_uuid'],
                is_identical_to(.test_emu_ae_db_uuid))  
    expect_that(r1[2, 'session'],
                is_identical_to('0000'))
    expect_that(r1[2, 'bundle'],
                is_identical_to('msajc015'))  
    expect_that(r1[2, 'start_item_id'], 
                equals(141))
    expect_that(r1[2, 'end_item_id'], 
                equals(142))
    # 
  })
  # 
  
  test_that("Query dominance over more than one level",{
    
    r1 = query(ae,"[ Syllable == S ^ Phonetic == p ]",
               resultType = NULL)
    expect_that(nrow(r1),
                equals(2))
    
  })
  
  test_that("Distinct result set for dominance query",{
    
    r1 = query(ae,"[ Syllable == S ^ Phonetic == s]")
    expect_that(nrow(r1),equals(9))
    
  })
  
  test_that("Query using Start function",{
    r1 = query(ae,
               "Phoneme = w & Start(Word, Phoneme) = 1",
               resultType = NULL)
    
    expect_that(nrow(r1),
                equals(4))
    expect_that(r1[1, 'db_uuid'],
                is_identical_to(.test_emu_ae_db_uuid))  
    expect_that(r1[1, 'session'],
                is_identical_to('0000'))
    expect_that(r1[1, 'bundle'],
                is_identical_to('msajc003'))  
    expect_that(r1[1, 'start_item_id'],
                equals(128))
    expect_that(r1[2, 'db_uuid'],
                is_identical_to(.test_emu_ae_db_uuid))  
    expect_that(r1[2, 'session'],
                is_identical_to('0000'))
    expect_that(r1[2, 'bundle'],
                is_identical_to('msajc012'))  
    expect_that(r1[2, 'start_item_id'],
                equals(124))
    expect_that(r1[3, 'db_uuid'],
                is_identical_to(.test_emu_ae_db_uuid))  
    expect_that(r1[3, 'session'],
                is_identical_to('0000'))
    expect_that(r1[3, 'bundle'],
                is_identical_to('msajc015'))  
    expect_that(r1[3, 'start_item_id'],
                equals(164))
    expect_that(r1[4, 'db_uuid'],
                is_identical_to(.test_emu_ae_db_uuid))  
    expect_that(r1[4, 'session'],
                is_identical_to('0000'))
    expect_that(r1[4, 'bundle'],
                is_identical_to('msajc015'))  
    expect_that(r1[4, 'start_item_id'],
                equals(177))
    
    r2 = query(ae, "Phoneme == p & Start(Word, Phoneme) == 0",
               resultType = NULL)
    
    expect_that(nrow(r2),
                equals(3))
    expect_that(r2[1, 'db_uuid'],
                is_identical_to(.test_emu_ae_db_uuid))  
    expect_that(r2[1, 'session'],
                is_identical_to('0000'))
    expect_that(r2[1, 'bundle'],
                is_identical_to('msajc015'))  
    expect_that(r2[1, 'start_item_id'],
                equals(147))
    expect_that(r2[2, 'db_uuid'],
                is_identical_to(.test_emu_ae_db_uuid))  
    expect_that(r2[2, 'session'],
                is_identical_to('0000'))
    expect_that(r2[2, 'bundle'],
                is_identical_to('msajc022'))  
    expect_that(r2[2, 'start_item_id'],
                equals(122))
    expect_that(r2[3, 'db_uuid'],
                is_identical_to(.test_emu_ae_db_uuid))  
    expect_that(r2[3, 'session'],
                is_identical_to('0000'))
    expect_that(r2[3, 'bundle'],
                is_identical_to('msajc057'))  
    expect_that(r2[3, 'start_item_id'],
                equals(136))
    
    # and some bundle pattern tests
    r3 = query(ae,
               "Phoneme == p & Start(Word, Phoneme) == 0",
               bundlePattern = 'msajc0..',
               resultType = NULL)
    
    expect_that(nrow(r3),
                equals(3))
    expect_that(r3[1, 'db_uuid'],
                is_identical_to(.test_emu_ae_db_uuid))  
    expect_that(r3[1, 'session'],
                is_identical_to('0000'))
    expect_that(r3[1, 'bundle'],
                is_identical_to('msajc015'))  
    expect_that(r3[1, 'start_item_id'],
                equals(147))
    expect_that(r3[2, 'db_uuid'],
                is_identical_to(.test_emu_ae_db_uuid))  
    expect_that(r3[2, 'session'],
                is_identical_to('0000'))
    expect_that(r3[2, 'bundle'],
                is_identical_to('msajc022'))
    expect_that(r3[2, 'start_item_id'],
                equals(122))
    expect_that(r3[3, 'db_uuid'],
                is_identical_to(.test_emu_ae_db_uuid))  
    expect_that(r3[3, 'session'],
                is_identical_to('0000'))
    expect_that(r3[3, 'bundle'],
                is_identical_to('msajc057'))  
    expect_that(r3[3, 'start_item_id'],
                equals(136))
    
    r4 = query(ae, 
               "Phoneme == p & Start(Word, Phoneme) = 0",
               bundlePattern = 'msajc02.',
               resultType = NULL)
    
    expect_that(nrow(r4), equals(1))
    expect_that(r4[1, 'start_item_id'], equals(122))
    
    r5 = query(ae,
               "Phoneme == p & Start(Word, Phoneme) == 0",
               bundlePattern = '.*7',
               resultType = NULL)
    
    expect_that(nrow(r5), equals(1))
    expect_that(r5[1, 'start_item_id'], equals(136))
    
    
  })
  
  test_that("Query using End function",{
    r1 = query(ae,
               "Phoneme == n & End(Word, Phoneme) == 1")
    
    expect_that(nrow(r1), equals(2))
    expect_that(r1[1,]$start_item_id, equals(103))
    expect_that(r1[2,]$start_item_id, equals(158))
    
  })
  
  test_that("Query using Num function",{
    
    # query words with exactly four phonemes
    r = query(ae,"Num(Word, Phoneme) = 4")
    expect_that(nrow(r), equals(6))
    
    # Test for GitHub Issue #41 
    # Num() function returns no values if level of first parameter is sublevel of second parameter.
    # not this now produces an error as the legacy behaviour 
    # mentioned in #41 is not consistent with anything 
    # and makes no sense!
    # Hence, decided to brake with backward compat. here...
    expect_error(query(ae, 
              "Num(Phonetic, Phoneme) == 1"), regexp = "Second level/attribute name")
    
  })
  
  test_that("Query using and operator",{
    sl1 = query(ae,
                'Text = them & Accent = W',
                resultType = 'emusegs')
    
    expect_that(nrow(sl1), equals(1))
    expect_that('[.data.frame'(sl1, 1, 'labels'),
                is_identical_to(I('them')))
    expect_that('[.data.frame'(sl1, 1, 'utts'),
                is_identical_to(I('0000:msajc012')))
    
  })
  
  test_that("Projection operator #",{
    
    r1 = query(ae, "[Syllable == S ^ #Phonetic == s]")
    expect_that(nrow(r1), equals(10))
    
  })
  
  test_that("Projection operator # for emusegs result type ",{
    
    r1 = query(ae,
               "[Syllable == S ^ #Phonetic == s]",
               resultType = 'emusegs')
    
    expect_that(nrow(r1), equals(10))
    
  })
  
  
  test_that("Check Phonetic tier seglist",{
    # load legacy emu seglist
    legacyEmuAePhoneticSeglist <- system.file("extdata",
                                              "legacy_emu_ae_phonetic_seglist.RData", 
                                              package = "emuR")
    load(file = legacyEmuAePhoneticSeglist)
    tsl = .legacy_emu_ae_phonetic_seglist
    # get original query string
    tslQuery = attr(tsl, 'query')
    # reprduce the original query
    sl = query(ae, tslQuery, resultType = 'emusegs')
    sr = .aeSampleRate
    doubleSampleTime = 2 / sr
    # we have to accept numeric deviations caused by double precision calculations
    # therefore add the machine epsilon to the tolerance of two sample 
    tolSec = doubleSampleTime + .Machine[['double.eps']]
    tolMs = tolSec * 1000
    # compare legacy emu generated and new seglist
    eq = equal.emusegs(sl,
                       tsl,
                       tolerance = tolMs,
                       uttsPrefix2 = '0000:')
    expect_true(eq)
    
  })
  
  
  # 
  test_that("bad calls cause errors",{
    expect_error(query(ae, "[#Text == more -> #Text == customers]"), 
                 regexp = "Multiple hash tags")
  })
  
  test_that("additional queries (simple and complex) work for more thorough query testing",{
    skip_on_cran()
    # SQ
    qs = "Phonetic == m"
    sl = query(ae, qs, resultType = "emuRsegs")
    expect_equal(nrow(sl), 7)
    expect_equal(attributes(sl)$query, qs)
    sl = query(ae, "[Phonetic == m]")
    expect_equal(nrow(sl), 7)
    expect_equal(attr(sl, "query"), NULL)
    sl = query(ae, "[Phonetic == m | n]")
    expect_equal(nrow(sl), 19)
    sl = query(ae, "[Phonetic != m | n]")
    expect_equal(nrow(sl), 234)
    sl = query(ae, "[Syllable =~ .*]")
    expect_equal(nrow(sl), 83)
    sl = suppressWarnings(query(ae, "[Text =~ am.*]"))
    expect_equal(nrow(sl), 1)
    sl = suppressWarnings(query(ae, "[Text !~ am.*]"))
    expect_equal(nrow(sl), 53)
    
    # SEQQ
    sl = query(ae, "[#Text == to -> Text =~ .*]")
    expect_equal(nrow(sl), 3)
    sl = query(ae, "[Text = to -> #Text =~ .*]")
    expect_equal(nrow(sl), 3)
    qs = "[Phonetic == m -> Phonetic == I]"
    sl = query(ae, qs, resultType = "emuRsegs")
    expect_equal(nrow(sl), 0)
    expect_equal(attributes(sl)$query, qs)
    sl = query(ae, "[#Phonetic == m -> Phonetic == I]")
    expect_equal(nrow(sl), 0)
    sl = query(ae, "[Phonetic == m -> #Phonetic == I]")
    expect_equal(nrow(sl), 0)
    sl = query(ae, "[Phonetic == m -> #Phonetic == o]")
    expect_equal(nrow(sl), 0)
    sl = query(ae, "[[Phonetic == m -> Phonetic == I ] -> Phonetic == n]")
    expect_equal(nrow(sl), 0)
    sl = query(ae, "[Text == more -> [Text == customers -> Text == than]]")
    expect_equal(sl$labels, "more->customers->than")
    sl = query(ae, "[Text =~ .* -> [Text == customers -> Text =~ .*]]")
    expect_equal(sl$labels, "more->customers->than")
    sl = query(ae, "[#Text == more -> [Text == customers -> Text == than]]")
    expect_equal(sl$labels, "more")
    sl = query(ae, "[Text == more -> [#Text == customers -> Text == than]]")
    expect_equal(sl$labels, "customers")
    sl = query(ae, "[Text == more -> [Text == customers -> #Text == than]]")
    expect_equal(sl$labels, "than")
    expect_error(query(ae, "[Syllable == S & Pitch_Accent == L+H*]"), 
                 regexp = "Unknown level attribute name")
    
    #CONJQ
    sl = query(ae, "[Text =~ .* & Word == F]")
    expect_equal(nrow(sl), 20)
    sl = query(ae, "[Text =~ .* & #Word == F]")
    expect_equal(nrow(sl), 20)
    sl = query(ae, "[Text =~ .* & Word == C & Accent == S]")
    expect_equal(nrow(sl), 25)
    
    # DOMQ
    sl = query(ae, "[Phoneme == p ^ Syllable == S]")
    expect_equal(nrow(sl), 3)
    sl = query(ae, "[Syllable =~ .* ^ Phoneme == p]")
    expect_equal(nrow(sl), 3)
    sl = query(ae, "[Phoneme == p ^ #Syllable =~ .*]")
    expect_equal(nrow(sl), 3)
    sl = query(ae, "[#Phoneme == p ^ Syllable =~ .*]")
    expect_equal(nrow(sl), 3)
    sl = query(ae, "[Syllable =~ .* ^ Phoneme != p | t | k]")
    expect_equal(nrow(sl), 83)
    sl = query(ae, "[#Syllable =~ .* ^ Phoneme != p | t | k]")
    expect_equal(nrow(sl), 83)
    sl = query(ae, "[Syllable =~ .* ^ #Phoneme != p | t | k]")
    expect_equal(nrow(sl), 195)
    
    # multiple DOMQs
    sl = query(ae, "[[Phoneme == p ^ Syllable =~ .*] ^ Word =~.*]")
    expect_equal(sl$labels, c("p", "p", "p"))
    sl = query(ae, "[[#Phoneme == p ^ Syllable =~ .*] ^ Word =~.*]")
    expect_equal(sl$labels, c("p", "p", "p"))
    sl = query(ae, "[[Phoneme == p ^ #Syllable =~ .*] ^ Word =~.*]")
    expect_equal(sl$labels, c("S", "S", "S"))
    sl = query(ae, "[[Phoneme == p ^ Syllable =~ .*] ^ #Word =~.*]")
    expect_equal(sl$labels, c("C", "C", "C"))
    sl = suppressWarnings(query(ae, "[[Phoneme == p ^ Syllable =~.*] ^ Text =~ emphasized | tempting]"))
    expect_equal(sl$labels, c("p", "p"))
    sl = suppressWarnings(query(ae, "[[Phoneme == p ^ Syllable =~.*] ^ #Text =~ emphasized | tempting]"))
    expect_equal(sl$labels, c("emphasized", "tempting"))
    
    # Position
    # Simple usage of Start(), End() and Medial()
    nWord = nrow(query(ae, "Word =~.*"))
    sl = query(ae, "[Start(Word, Syllable) == 1]")
    expect_equal(nrow(sl), nWord)
    sl = query(ae, "[Start(Word, Phoneme) == 1]")
    expect_equal(nrow(sl), nWord)
    sl = query(ae, "[Start(Word, Syllable) == 0]")  
    expect_equal(nrow(sl), 28)
    sl = query(ae, "[End(Word, Syllable) == 1]")
    expect_equal(nrow(sl), nWord)
    sl = query(ae, "[Medial(Word, Syllable) == 1]")
    expect_equal(nrow(sl), 9)
    sl = query(ae, "[Medial(Word, Syllable) == 0]")
    expect_equal(nrow(sl), 73)
    # Position and Boolean &
    sl = query(ae, "[Phoneme == m & Start(Word, Phoneme) == 1]") # word initial m's
    expect_equal(nrow(sl), 2)
    sl = query(ae, "[Phoneme == m & End(Word, Phoneme) == 1]") # word final m's
    expect_equal(nrow(sl), 1)
    sl = query(ae, "[Syllable == S & End(Word, Syllable) == 0]") # non word final strong syllables
    expect_equal(nrow(sl), 16)
    # Position and Boolean ^
    sl = query(ae, "[Phoneme =~ .* ^ End(Word, Syllable) == 0]") # non word final Phonemes
    expect_equal(nrow(sl), 61)
    
    # Count
    sl = query(ae, "[Num(Word, Syllable) == 4]")
    expect_equal(nrow(sl), 1)
    sl = query(ae, "[Num(Syllable, Phoneme) > 6]")
    expect_equal(nrow(sl), 1)
    
    # Count and Boolean &
    sl = query(ae, "[Text =~ .* & Num(Word, Phoneme) > 4 ]")
    expect_equal(nrow(sl), 18)
    sl = query(ae, "[Syllable == S & Num(Syllable, Phoneme) == 5]")
    expect_equal(nrow(sl), 4)
    
    # Count and ^
    sl = query(ae, "[Phoneme == m ^ Num(Word, Syllable) == 3]")
    expect_equal(nrow(sl), 2)
    sl = query(ae, "[Syllable = W ^ Num(Word, Syllable) < 3]")
    expect_equal(nrow(sl), 28)
    sl = query(ae, "[Text =~ .* ^ Num(Syllable, Phoneme) == 4]")
    expect_equal(nrow(sl), 7)
    
    # Combinations
    # ^ and -> (Domination and Sequence)
    sl = query(ae, "[[Phoneme == m -> Phoneme =~ .*] ^ Syllable == S]")
    expect_equal(nrow(sl), 4)
    sl = query(ae, "[Phoneme == s -> [Phoneme =~ .* ^ Syllable == W]]")
    expect_equal(nrow(sl), 5)
    sl = query(ae, "[[Syllable == S ^ Phoneme == p] -> Syllable == W]")
    expect_equal(nrow(sl), 3)
    expect_true(all(grepl("->", sl$labels)))
    sl = query(ae, "[[Syllable == S ^ #Phoneme == p] -> Syllable == W]") # SIC is this supposed to work?
    expect_equal(nrow(sl), 3)
    
    # ^ and -> and & (Domination and Sequence and Boolean &)
    sl = query(ae, "[Text =~ .* ^ Phoneme == @ & Start(Text, Phoneme) == 1]")
    expect_equal(nrow(sl), 3)
    sl = query(ae, "[[Phoneme == m & Start(Word, Phoneme) == 1 -> Phoneme == o:] ^ Syllable == S]")
    expect_equal(nrow(sl), 1)
    sl = query(ae, "[[Phoneme == m & Start(Word, Phoneme) == 1 -> Phoneme == o:] ^ Syllable == S]")
    expect_equal(nrow(sl), 1)
    sl = query(ae, "[[[Phoneme == m & Start(Word, Phoneme) == 1 -> Phoneme == o:] ^ Syllable == S] ^ #Text != x]")
    expect_equal(sl$labels, "more")
    sl = query(ae, "[[Text =~ .* & Num(Text, Syllable) == 3 ^ [Phoneme == @ ^ Start(Word, Syllable) == 1]] -> Text == his]")
    
    # A few more Q & A’s (because practice makes perfect)
    sl = query(ae, "[Phoneme == m | n & Medial(Word, Phoneme) == 1]")
    expect_equal(nrow(sl), 12)
    sl = query(ae, "[[Phonetic == H -> Phonetic =~ .*] -> Phonetic == I | U ]")
    expect_equal(sl$labels, "H->h->I")
    sl = query(ae, "[Syllable =~ .* & Medial(Word, Syllable) == 0]")
    expect_equal(nrow(sl), 73)
    sl = query(ae, "[Text =~ .* & Num(Text, Syllable) == 2]")
    expect_equal(nrow(sl), 11)
    sl = query(ae, "[Text == the -> #Text =~ .* & Accent == S]")
    expect_equal(sl$labels, "chill")
    sl = query(ae, "[Syllable = S ^ Num(Word, Phoneme) == 5]")
    expect_equal(nrow(sl), 4)
    sl = query(ae, "[Syllable == W ^ Phoneme == @]")
    expect_equal(nrow(sl), 29)
    qs = "[Text =~ .* ^ #Tone == L* | L+H*]"
    sl = query(ae, qs)
    expect_equal(nrow(sl), 2)
    expect_equal(attr(sl, "query"), NULL) # used to be qs but not with tibble
    sl = query(ae, 
               "[Tone =~.* ^ [End(Word, Syllable) == 1 ^ Num(Word, Syllable) == 2]]")
    expect_equal(nrow(sl), 1)
    sl = query(ae, 
               "[[[Phoneme =~ .* ^ Phonetic == H] ^ Start(Word, Syllable) == 1] ^ Accent == S]")
    expect_equal(nrow(sl), 10)
    sl  = query (ae ,
                 "[[[Phonetic = n -> Phonetic = z] -> Phonetic = S ] ^ [Text = friends -> Text = she]]")
    expect_equal(sl$labels, "n->z->S")
    sl = query(ae, "[Utterance =~ .* ^ Phonetic == @]", 
               verbose = F)
    expect_equal(nrow(sl), 7)
    expect_equal(sl$labels[1], "")
    
    sl = query(ae, "[Text == she ^ [Phonetic == S -> Phonetic == i:]]")
    expect_equal(sl$labels, "she")

    sl = query(ae, "[[Phonetic == S -> Phonetic == i:] ^ Text == she]")
    expect_equal(sl$labels, "S->i:")

    # a few more for -> + ^ queries
    # overlap start -> empty as not dominated
    sl = query(ae, "[[Phonetic == z -> Phonetic == S] ^ Text == she]")
    expect_equal(nrow(sl), 0)

    sl = query(ae, "[Text == she ^ [Phonetic == z -> Phonetic == S]]")
    expect_equal(nrow(sl), 0)
    
    # overlap end -> empty as not dominated    
    sl = query(ae, "[[Phonetic == i: -> Phonetic == w] ^ Text == she]")
    expect_equal(nrow(sl), 0)

    sl = query(ae, "[Text == she ^ [Phonetic == i: -> Phonetic == w]]")
    expect_equal(nrow(sl), 0)
    
    sl = query(ae, "[Tone =~ .* ^ [[Text == amongst -> Text == her] -> Text == friends]]")
    expect_equal(nrow(sl), 2)
  })
  
  # 
  test_that("timeRefSegmentLevel works correctly",{
    skip_on_cran()
    sl = query(ae, 
               "[Syllable == W]")
    sl = query(ae, 
               "[Syllable == W]", 
               timeRefSegmentLevel = "Phonetic")
    duplicate_level(ae, 
                    "Phonetic", 
                    "Phonetic2", 
                    verbose = F)
    expect_error(query(ae, "[Syllable == W]"), 
                 regexp = "The level is linked")
    expect_error(query(ae, 
                       "[Syllable == W]", 
                       timeRefSegmentLevel = "badLevelName"), 
                 regexp = "Cannot resolve time information for result level")
    query(ae, 
          "[Syllable == W]", 
          timeRefSegmentLevel = "Phonetic2")
    
    # clean up
    remove_linkDefinition(ae, 
                          superlevelName = "Phoneme",
                          sublevelName = "Phonetic2",
                          force = T,
                          verbose = F)
    
        
    remove_levelDefinition(ae, 
                           name = "Phonetic2",
                           force = T, 
                           verbose = F)
  })
  
  # 
  test_that("calcTimes works correctly",{
    skip_on_cran()
    sl = query(ae, 
               "[Syllable == W]", 
               calcTimes = F)
  })
  
  test_that("correct times are calculated for Intonational",{
    skip_on_cran()
    sl = query(ae, "Intonational == L%", timeRefSegmentLevel = "Phonetic", verbose = F)
    all(round(sl$start, 3) == round(c(256.925, 571.925, 379.525, 425.375, 299.975, 513.925, 475.775), 3))
    all(round(sl$end, 3) == round(c(2604.425, 2753.975, 2692.325, 3456.825, 2469.525, 2554.175, 2794.925), 3))
  })
  
  test_that("correct times are calculated for sequence dom. queries",{
    skip_on_cran()
    sl = query(ae, 
               "[[Phonetic == N -> Phonetic == s] -> Phonetic == t]", 
               verbose = F)
    expect_equal(sl$sample_start, 8534)
    expect_equal(sl$sample_end, 11933)
    # move up one level
    sl = query(ae, 
               "[[Phoneme == N -> Phoneme == s] -> Phoneme == t]",
               verbose = F)
    expect_equal(sl$sample_start, 8534)
    expect_equal(sl$sample_end, 13483)
    # even further up the hierarchy
    sl = query(ae,
               "[Text == more -> Text == customers]",
               verbose = F)
    expect_equal(sl$sample_start, 31574)
    expect_equal(sl$sample_end, 47355)
    
  })
  
  # clean up (also disconnect)
  DBI::dbDisconnect(ae$connection)
  ae = NULL
  unlink(.test_emu_ae_db_dir, recursive = T)
})


