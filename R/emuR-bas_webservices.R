#####################################################################
############################# MAUS ##################################
#####################################################################

<<<<<<< HEAD
bas_run_maus_from_transcription <- function(handle,
                                            transcriptionLabel,
                                            language,
                                            mauLabel = "MAU",
                                            orthoLabel = "ORT",
                                            canoLabel = "KAN",
                                            orthoLevel = NULL,
                                            mauLevel = NULL,
                                            perspective = "default",
                                            verbose = TRUE)
{
  transcriptionLevel = get_levelNameForAttributeName(handle, transcriptionLabel)
  if (is.null(transcriptionLevel)) {
    stop("Could not find a level for label", transcriptionLabel)
  }
  
  if (is.null(orthoLevel)) {
    orthoLevel = orthoLabel
  }
  if (is.null(mauLevel)) {
    mauLevel = mauLabel
  }
  
  if (!is.null(get_levelDefinition(handle, orthoLevel))) {
    stop("Level", orthoLevel, "already exists!")
  }
  if (!is.null(get_levelDefinition(handle, mauLevel))) {
    stop("Level", mauLevel, "already exists!")
  }
=======
bas_run_maus_from_transcription <- function(
  handle,
  transcriptionLabel,
  language,
  mauLabel = "MAU",
  wordLevel = NULL,
  mauLevel = NULL,
  verbose = TRUE
)
{
  transcriptionLevel = get_levelNameForAttributeName(handle, transcriptionLabel)
  if(is.null(transcriptionLevel)) { stop("Could not find a level for label", transcriptionLabel) }
  
  if(is.null(wordLevel)) { wordLevel = orthoLabel }
  if(is.null(mauLevel)) { mauLevel = mauLabel }
  
  if(!is.null(get_levelDefinition(handle, wordLevel))) { stop("Level", wordLevel, "already exists!") }
  if(!is.null(get_levelDefinition(handle, mauLevel))) { stop("Level", mauLevel, "already exists!") }
>>>>>>> 55b1e30267d7870fefb54f360147c67933309bb9
  
  trnLevel = NULL
  if(get_levelDefinition(handle, transcriptionLevel)$type == "SEGMENT")
  {
    trnLevel = transcriptionLevel
  }
  else
  {
<<<<<<< HEAD
    queryTxt = paste0(
      "SELECT count(*) FROM items WHERE", basic_cond(handle, session, bundle), "AND level==", transcriptionLevel
    )
    nbItems = DBI::dbGetQuery(handle$connection, queryTxt)[1,1]
    if (nbItems > 1)
    {
      stop(
        "The transcription level", transcriptionLevel, "is an ITEM level but contains more than one item."
      )
=======
    queryTxt = paste0("SELECT count(*) FROM items WHERE", basic_cond(handle, session, bundle), "AND level==", transcriptionLevel)
    nbItems = DBI::dbGetQuery(handle$connection, queryTxt)[1,1]
    if(nbItems > 1)
    {
      stop("The transcription level", transcriptionLevel, "is an ITEM level but contains more than one item.")
>>>>>>> 55b1e30267d7870fefb54f360147c67933309bb9
    }
  }
  
  
<<<<<<< HEAD
  bas_prepare(handle)
  
  bas_run_g2p_for_tokenization_dbi(
    handle = handle,
    transcriptionLabel = transcriptionLabel,
    orthoLabel = orthoLabel,
    orthoLevel = orthoLevel,
    language = language,
    verbose = verbose
  )
  
  bas_run_g2p_from_ortho_dbi(
    handle = handle,
    orthoLevel = orthoLevel,
    orthoLabel = orthoLabel,
    canoLabel = canoLabel,
    language = language,
    verbose = verbose,
    embedded = TRUE
  )
  
  bas_run_maus_from_cano_dbi(
    handle = handle,
    canoLevel = orthoLevel,
    canoLabel = canoLabel,
    language = language,
    verbose = verbose,
    trnLevel = trnLevel,
    mauLabel = mauLabel,
    mauLevel = mauLevel,
    embedded = TRUE
  )
  
  bas_clear(handle)
  
  add_levelDefinition(handle, orthoLevel, "ITEM", verbose = verbose)
  add_levelDefinition(handle, mauLevel, "SEGMENT", verbose = verbose)
  if (orthoLevel != canoLabel) {
    add_attributeDefinition(handle, orthoLevel, canoLabel, verbose = verbose)
  }
  if (orthoLevel != orthoLabel) {
    add_attributeDefinition(handle, orthoLevel, orthoLabel, verbose = verbose)
  }
  if (mauLevel != mauLabel) {
    add_attributeDefinition(handle, mauLevel, mauLabel, verbose = verbose)
  }
  add_linkDefinition(handle, "ONE_TO_MANY", transcriptionLevel, orthoLevel)
  add_linkDefinition(handle, "ONE_TO_MANY", orthoLevel, mauLevel)
  
  bas_new_canvas(handle, perspective, mauLevel)
  rewrite_allAnnots(handle, verbose = verbose)
}

bas_run_maus_from_orth <- function(handle,
                                   orthoLabel,
                                   language,
                                   canoLabel = "KAN",
                                   mauLabel = "MAU",
                                   mauLevel = NULL,
                                   trnLevel = NULL,
                                   verbose = TRUE,
                                   perspective = "default")
{
  if (is.null(mauLevel)) {
    mauLevel = mauLabel
  }
  if (!is.null(get_levelDefinition(handle, mauLevel))) {
    stop("Level", mauLevel, "already exists!")
  }
  
  orthLevel = get_levelNameForAttributeName(handle, orthLabel)
  if (is.null(orthLevel)) {
    stop("Could not find a level for label", orthLabel)
  }
  
  if (is.null(trnLevel) &&
      get_levelDefinition(handle, orthLevel)$type == "SEGMENT") {
    trnLevel = orthLevel
  }
  
  bas_prepare(handle)
  
  bas_run_g2p_from_ortho_dbi(
    handle = handle,
    orthLabel = orthLabel,
    orthLevel = orthLevel,
    language = language,
    canoLabel = canoLabel,
    verbose = verbose
  )
  
  bas_run_maus_from_cano_dbi(
    handle = handle,
    canoLabel = canoLabel,
    canoLevel = orthLevel,
    language = language,
    mauLabel = mauLabel,
    trnLevel = trnLevel,
    verbose = verbose,
    mauLevel = mauLevel
  )
  
  bas_clear(handle)
  
  add_levelDefinition(handle, mauLevel, "SEGMENT", verbose = verbose)
  if (mauLevel != mauLabel) {
    add_attributeDefinition(handle, mauLevel, mauLabel, verbose = verbose)
  }
  add_linkDefinition(handle, "ONE_TO_MANY", orthLevel, mauLevel)
  bas_new_canvas(handle, perspective, mauLevel)
  rewrite_allAnnots(handle, verbose = verbose)
}

bas_run_maus_from_cano <- function(handle,
                                   canoLabel,
                                   canoLevel = NULL,
                                   language,
                                   mauLabel = "MAU",
                                   trnLevel = NULL,
                                   mauLevel = NULL,
                                   verbose = TRUE,
                                   perspective = "default")
{
  if (is.null(mauLevel)) {
    mauLevel = mauLabel
  }
  if (!is.null(get_levelDefinition(handle, mauLevel))) {
    stop("Level", mauLevel, "already exists!")
  }
  
  canoLevel = get_levelNameForAttributeName(handle, canoLabel)
  if (is.null(canoLevel)) {
    stop("Could not find a level for label", canoLabel)
  }
  
  if (is.null(trnLevel) &&
      get_levelDefinition(handle, canoLevel)$type == "SEGMENT") {
    trnLevel = canoLevel
  }
  
  bas_prepare(handle)
  
  bas_run_maus_from_cano_dbi(
    handle = handle,
    canoLabel = canoLabel,
    canoLevel = canoLevel,
    mauLabel = mauLabel,
    mauLevel = mauLevel,
    language = language,
    verbose = verbose,
    trnLevel = trnLevel
  )
  
  
  bas_clear(handle)
  
  add_levelDefinition(handle, mauLevel, "SEGMENT", verbose = verbose)
  if (mauLevel != mauLabel) {
    add_attributeDefinition(handle, mauLevel, mauLabel, verbose = verbose)
  }
  add_linkDefinition(handle, "ONE_TO_MANY", canoLevel, mauLevel)
  
  bas_new_canvas(handle, perspective, mauLevel)
  rewrite_allAnnots(handle, verbose = verbose)
}


#####################################################################
############################## G2P ##################################
#####################################################################

bas_run_g2p_from_ortho <- function(handle,
                                   orthoLabel,
                                   orthoLevel = NULL,
                                   language,
                                   canoLabel = "KAN",
                                   verbose = TRUE)
{
  orthoLevel = get_levelNameForAttributeName(handle, orthoLabel)
  if (is.null(orthoLevel)) {
    stop("Could not find a level for label ", orthoLabel)
  }
  
  canoLevel = get_levelNameForAttributeName(handle, canoLabel)
  if (!is.null(canoLevel)) {
    stop("There is aleady a level with the label ", canoLabel)
  }
  
  bas_prepare(handle)
  
  
  bas_run_g2p_from_ortho_dbi(
    handle = handle,
    orthoLabel = orthoLabel,
    orthoLevel = orthoLevel,
    language = language,
    canoLabel = canoLabel,
    verbose = verbose
  )
  
  bas_clear(handle)
  
  if (orthoLevel != canoLabel) {
    add_attributeDefinition(handle, orthoLevel, canoLabel, verbose = verbose)
  }
  rewrite_allAnnots(handle, verbose = verbose)
}


bas_run_g2p_from_transcription <- function(handle,
                                           transcriptionLabel,
                                           language,
                                           canoMode = "label",
                                           canoLabel = "KAN",
                                           orthoLabel = "ORT",
                                           orthoLevel = NULL,
                                           verbose = TRUE)
{
  transcriptionLevel = get_levelNameForAttributeName(handle, transcriptionLabel)
  if (is.null(transcriptionLevel)) {
    stop("Could not find a level for label", transcriptionLabel)
  }
  
  if (is.null(orthoLevel)) {
    orthoLevel = orthoLabel
  }
  if (!is.null(get_levelDefinition(handle, orthoLevel))) {
    stop("Level", orthoLevel, "already exists!")
  }
  
  bas_prepare(handle)
  
  bas_run_g2p_for_tokenization_dbi(
    handle = handle,
    transcriptionLabel = transcriptionLabel,
    orthoLabel = orthoLabel,
    orthoLevel = orthoLevel,
    language = language,
    verbose = verbose
  )
  
  bas_run_g2p_from_ortho_dbi(
    handle = handle,
    orthoLevel = orthoLevel,
    orthoLabel = orthoLabel,
    canoLabel = canoLabel,
    language = language,
    verbose = verbose
  )
  
  
  bas_clear(handle)
  
  add_levelDefinition(handle, orthoLevel, "ITEM", verbose = verbose)
  if (orthoLevel != orthoLabel) {
    add_attributeDefinition(handle, orthoLevel, orthoLabel, verbose = verbose)
  }
  if (orthoLevel != canoLabel) {
    add_attributeDefinition(handle, orthoLevel, canoLabel, verbose = verbose)
  }
  add_linkDefinition(handle, "ONE_TO_MANY", transcriptionLevel, orthoLevel)
  rewrite_allAnnots(handle, verbose = verbose)
}

#####################################################################
########################### CHUNKER #################################
#####################################################################

bas_run_chunker_from_cano <- function(handle,
                                      canoLabel,
                                      language,
                                      trnLabel = "TRN",
                                      trnLevel = NULL,
                                      verbose = TRUE,
                                      perspective = "default",
                                      topLevel = NULL)
{
  if (is.null(trnLevel)) {
    trnLevel = trnLabel
  }
  if (!is.null(get_levelDefinition(handle, trnLevel))) {
    stop("Level", trnLevel, "already exists!")
  }
  
  canoLevel = get_levelNameForAttributeName(handle, canoLabel)
  if (is.null(canoLevel)) {
    stop("Could not find a level for label", canoLabel)
  }
  
  if ((!is.null(topLevel)) &&
      is.null(get_levelDefinition(handle, topLevel)))
  {
    stop("Top level", topLevel, "does not exist")
  }
  
  bas_prepare(handle)
  
  bas_run_chunker_from_cano_dbi(
    handle = handle,
    canoLabel = canoLabel,
    canoLevel = canoLevel,
    trnLabel = trnLabel,
    language = language,
    verbose = verbose,
    trnLevel = trnLevel,
    topLevel = topLevel
  )
  
  
  bas_clear(handle)
  
  add_levelDefinition(handle, trnLevel, "SEGMENT", verbose = verbose)
  if (trnLevel != trnLabel) {
    add_attributeDefinition(handle, trnLevel, trnLabel, verbose = verbose)
  }
  add_linkDefinition(handle, "ONE_TO_MANY", trnLevel, canoLevel)
  bas_new_canvas(handle, perspective, trnLevel)
  rewrite_allAnnots(handle, verbose = verbose)
}




#####################################################################
############################ MINNI ##################################
#####################################################################

bas_run_minni <- function(handle,
                          language,
                          minniLabel = "MINNI",
                          minniLevel = NULL,
                          verbose = TRUE,
                          topLevel = NULL,
                          perspective = "default")
{
  if (is.null(minniLevel)) {
    minniLevel = minniLabel
  }
  if (!is.null(get_levelDefinition(handle, minniLevel))) {
    stop("Level", minniLevel, "already exists!")
  }
  
  if ((!is.null(topLevel)) &&
      is.null(get_levelDefinition(handle, topLevel)))
  {
    stop("Top level", topLevel, "does not exist")
  }
  
  bas_prepare(handle)
  
  bas_run_minni_dbi(
    handle = handle,
    language = language,
    minniLabel = minniLabel,
    minniLevel = minniLevel,
    verbose = verbose,
    topLevel = topLevel
  )
  bas_clear(handle)
  
  add_levelDefinition(handle, minniLevel, "SEGMENT", verbose = verbose)
  if (minniLevel != minniLabel) {
    add_attributeDefinition(handle, minniLevel, minniLabel, verbose = verbose)
  }
  if(!is.null(topLevel))
  {
    add_linkDefinition(handle, "ONE_TO_MANY", topLevel, minniLevel)
  }
  bas_new_canvas(handle, perspective, minniLevel)
  rewrite_allAnnots(handle, verbose = verbose)
}


#####################################################################
########################### PHO2SYL #################################
#####################################################################

bas_run_pho2syl_from_cano <- function(handle,
                                      canoLabel,
                                      language,
                                      verbose,
                                      kasLabel = "KAS",
                                      mode = "label",
                                      kasLevel = NULL)
{
  canoLevel = get_levelNameForAttributeName(handle, canoLabel)
  if (is.null(canoLevel)) {
    stop("Could not find a level for label", canoLabel)
  }
  
  kasLevel = get_levelNameForAttributeName(handle, kasLabel)
  if (!is.null(kasLevel)) {
    stop("There is aleady a level with the label ", kasLabel)
  }
  
  bas_prepare(handle)
  
  bas_run_pho2syl_from_cano_dbi(
    handle = handle,
    canoLabel = canoLabel,
    canoLevel = canoLevel,
    language = language,
    verbose = verbose,
    kasLabel = kasLabel
  )
  
  bas_clear(handle)
  
  if (canoLevel != kasLabel) {
    add_attributeDefinition(handle, canoLevel, kasLabel, verbose = verbose)
  }
  rewrite_allAnnots(handle)
}

bas_run_pho2syl_from_mau <- function(handle,
                                     mauLabel,
                                     language,
                                     verbose,
                                     masLabel = "MAS",
                                     perspective = "default",
                                     masLevel = NULL)
{
  if (is.null(masLevel)) {
    masLevel = masLabel
  }
  if (!is.null(get_levelDefinition(handle, masLevel))) {
    stop("Level", masLevel, "already exists!")
  }
  
  mauLevel = get_levelNameForAttributeName(handle, mauLabel)
  if (is.null(mauLevel)) {
    stop("Could not find a level for label", mauLabel)
  }
  if (get_levelDefinition(handle, mauLevel)$type != "SEGMENT")
  {
    stop(mauLevel, "must be a segment tier in order to run pho2syl from segment")
  }
  
  bas_prepare(handle)
  
  bas_run_pho2syl_from_mau_dbi(
    handle = handle,
    mauLabel = mauLabel,
    mauLevel = mauLevel,
    language = language,
    verbose = verbose,
    masLabel = masLabel,
    masLevel = masLevel
  )
  
  bas_clear(handle)
  
  add_levelDefinition(handle, masLevel, "SEGMENT", verbose = verbose)
  if (masLevel != masLabel) {
    add_attributeDefinition(handle, masLevel, masLabel, verbose = verbose)
  }
  
  add_linkDefinition(handle, "MANY_TO_MANY", canoLevel, masLevel)
  autobuild_linkFromTimes(handle, masLevel, mauLevel, convertSuperlevel = TRUE, verbose = verbose)
  bas_new_canvas(handle, perspective, masLevel)
  rewrite_allAnnots(handle, verbose = verbose)
}
=======
  bas_prepare()
  
  bas_run_g2p_for_tokenization_dbi(handle = handle,
                                   transcriptionLabel = transcriptionLabel,
                                   orthoLabel = orthoLabel,
                                   orthoLevel = wordLevel,
                                   language = language,
                                   verbose = verbose)
  
  bas_run_g2p_from_orth_dbi(handle = handle,
                            orthoLevel=wordLevel,
                            orthoLabel=orthoLabel,
                            canoLabel=canoLabel,
                            language=language,
                            verbose=verbose)
  
  bas_run_maus_from_cano_dbi(handle=handle,
                             canoLevel=wordLevel,
                             canoLabel=canoLabel,
                             language=language,
                             verbose=verbose,
                             trnLevel=trnLevel,
                             mauLabel=mauLabel,
                             mauLevel=mauLevel)
  
  bas_clear()
  
  add_levelDefinition(handle, wordLevel, "ITEM")
  add_levelDefinition(handle, mauLevel, "SEGMENT")
  if(wordLevel != canoLabel) { add_attributeDefinition(handle, wordLevel, canoLabel) }
  if(wordLevel != orthoLabel) { add_attributeDefinition(handle, wordLevel, orthoLabel) }
  if(mauLevel != mauLabel) { add_attributeDefinition(handle, mauLevel, mauLabel) }
  add_linkDefinition(handle, "ONE_TO_MANY", transcriptionLevel, wordLevel)
  add_linkDefinition(handle, "ONE_TO_MANY", wordLevel, mauLevel)
}

bas_run_maus_from_orth <- function(
  handle,
  orthLabel,
  language,
  canoLabel = "KAN",
  mauLabel = "MAU",
  mauLevel = NULL,
  trnLevel = NULL,
  verbose = TRUE
)
{
  if(is.null(mauLevel)) { mauLevel = mauLabel }
  if(!is.null(get_levelDefinition(handle, mauLevel))) { stop("Level", mauLevel, "already exists!") }
  
  orthLevel = get_levelNameForAttributeName(handle, orthLabel)
  if(is.null(orthLevel)) { stop("Could not find a level for label", orthLabel) }
  
  if(is.null(trnLevel) && get_levelDefinition(handle, orthLevel)$type == "SEGMENT") { trnLevel = orthLevel }
  
  bas_prepare()
  
  bas_run_g2p_from_orth_dbi(handle=handle,
                            orthLabel=orthLabel,
                            orthLevel=orthLevel,
                            language=language,
                            canoLabel=canoLabel,
                            verbose=verbose)
  
  bas_run_maus_from_cano_dbi(handle=handle,
                             canoLabel=canoLabel,
                             canoLevel=orthLevel,
                             language=language,
                             mauLabel=mauLabel,
                             trnLevel=trnLevel,
                             verbose=verbose,
                             mauLevel=mauLevel)
  
  bas_clear()
  
  add_levelDefinition(handle, mauLevel, "SEGMENT")
  if(mauLevel != mauLabel) { add_attributeDefinition(handle, mauLevel, mauLabel) }
  add_linkDefinition(handle, "ONE_TO_MANY", orthLevel, mauLevel)
}

bas_run_maus_from_cano <- function(
  handle,
  canoLabel,
  language,
  mauLabel = "MAU",
  canoMode = "label",
  wordLevel = NULL,
  trnLevel = NULL,
  mauLevel = NULL,
  verbose = TRUE
)
{
  if(is.null(mauLevel)) { mauLevel = mauLabel }
  if(!is.null(get_levelDefinition(handle, mauLevel))) { stop("Level", mauLevel, "already exists!") }

  if(is.null(trnLevel) && get_levelDefinition(handle, canoLevel)$type == "SEGMENT") { trnLevel = canoLevel }
  bas_prepare()
  
  canoLevel = get_levelNameForAttributeName(handle, canoLabel)
  if(is.null(canoLevel)) { stop("Could not find a level for label", canoLabel) }
    
  bas_run_maus_from_cano_dbi(handle = handle,
                             canoLabel = canoLabel,
                             canoLevel = canoLevel,
                             mauLabel = mauLabel,
                             mauLevel = mauLevel,
                             language = language,
                             verbose = verbose,
                             trnLevel = trnLevel)
  
  
  bas_clear()
  
  add_levelDefinition(handle, mauLevel, "SEGMENT")
  if(mauLevel != mauLabel) { add_attributeDefinition(handle, mauLevel, mauLabel) }
  add_linkDefinition(handle, "ONE_TO_MANY", wordLevel, mauLevel)
}


#####################################################################
############################## G2P ##################################
#####################################################################

bas_run_g2p_from_ortho <- function(
  handle,
  orthoLabel,
  language,
  canoLabel = "KAN",
  verbose = TRUE,
  canoLevel = NULL
)
{
  orthoLevel = get_levelNameForAttributeName(handle, orthoLabel)
  if(is.null(orthoLevel)) { stop("Could not find a level for label", orthoLabel) }
  
  bas_prepare()
  
  
  bas_run_g2p_from_ortho_dbi(handle=handle,
                             orthoLabel=orthoLabel,
                             orthoLevel=orthoLevel,
                             language=language,
                             canoLabel=canoLabel,
                             verbose=verbose)
  
  bas_clear()
  
  if(orthoLevel != canoLabel) { add_attributeDefinition(handle, orthLevel, canoLabel) }
}


bas_run_g2p_from_transcription <- function(
  handle,
  transcriptionLabel,
  language,
  canoMode = "label",
  canoLabel = "KAN",
  orthoLabel = "ORT",
  wordLevel = NULL,
  verbose = TRUE
)
{
  transcriptionLevel = get_levelNameForAttributeName(handle, transcriptionLabel)
  if(is.null(transcriptionLevel)) { stop("Could not find a level for label", transcriptionLabel) }
  
  if(is.null(wordLevel)) { wordLevel = orthoLabel }
  if(!is.null(get_levelDefinition(handle, wordLevel))) { stop("Level", wordLevel, "already exists!") }
  
  bas_prepare()
  
  bas_run_g2p_for_tokenization_dbi(handle = handle,
                                   transcriptionLabel = transcriptionLabel,
                                   orthoLabel = orthoLabel,
                                   orthoLevel = wordLevel,
                                   language = language,
                                   verbose = verbose)
  
  bas_run_g2p_from_ortho_dbi(handle = handle,
                            orthoLevel=wordLevel,
                            orthoLabel=orthoLabel,
                            canoLabel=canoLabel,
                            language=language,
                            verbose=verbose)
  
  
  bas_clear()
  
  add_levelDefinition(handle, wordLevel, "ITEM")
  if(wordLevel != orthoLabel) { add_attributeDefinition(handle, wordLevel, orthoLabel)}
  if(wordLevel != canoLabel) { add_attributeDefinition(handle, wordLevel, canoLabel)}
  add_linkDefinition(handle, "ONE_TO_MANY", transcriptionLevel, wordLevel)
}

#####################################################################
########################### CHUNKER #################################
#####################################################################

bas_run_chunker_from_cano <- function(
  handle,
  canoLabel,
  language,
  trnLabel = "TRN",
  trnLevel = NULL,
  verbose = TRUE,
  topLevel = NULL
)
{
  if(is.null(trnLevel)) { trnLevel = trnLabel }
  if(!is.null(get_levelDefinition(handle, trnLevel))) { stop("Level", trnLevel, "already exists!") }
  
  canoLevel = get_levelNameForAttributeName(handle, canoLabel)
  if(is.null(canoLevel)) { stop("Could not find a level for label", canoLabel) }
  
  if((!is.null(topLevel)) && is.null(get_levelDefinition(handle, topLevel)))
  {
    stop("Top level", topLevel, "does not exist")
  }
  
  bas_prepare()
  
  bas_run_chunker_from_cano_dbi(handle = handle,
                                canoLabel = canoLabel,
                                canoLevel = canoLevel,
                                trnLabel = trnLabel,
                                language = language,
                                verbose = verbose,
                                trnLevel = trnLevel,
                                topLevel = topLevel)
  
  
  bas_clear()
  
  add_levelDefinition(handle, trnLevel, "SEGMENT")
  if(trnLevel != trnLabel) { add_attributeDefinition(handle, trnLevel, trnLabel) }
  add_linkDefinition(handle, "ONE_TO_MANY", trnLevel, canoLevel)
}



#####################################################################
############################ MINNI ##################################
#####################################################################

bas_run_minni <- function(
  handle,
  language,
  minniLabel = "MINNI",
  minniLevel = NULL,
  verbose = TRUE,
  topLevel = NULL
)
{
  if(is.null(minniLevel)) { minniLevel = minniLabel }
  if(!is.null(get_levelDefinition(handle, minniLevel))) { stop("Level", minniLevel, "already exists!") }
  
  if((!is.null(topLevel)) && is.null(get_levelDefinition(handle, topLevel)))
  {
    stop("Top level", topLevel, "does not exist")
  }
  
  bas_prepare()
  
  bas_run_minni_dbi(handle=handle,
                    language=language,
                    minniLabel=minniLabel,
                    minniLevel=minniLevel,
                    verbose=verbose,
                    topLevel = topLevel)
  bas_clear()
  
  add_levelDefinition(handle, minniLevel, "SEGMENT")
  if(minniLevel != minniLabel) { add_attributeDefinition(handle, minniLevel, minniLabel) }
}

#####################################################################
########################### PHO2SYL #################################
#####################################################################

bas_run_pho2syl_from_cano <- function(handle,
                          canoLabel,
                          language,
                          verbose,
                          kasLabel = "KAS",
                          mode = "label",
                          kasLevel = NULL)
{
  canoLevel = get_levelNameForAttributeName(handle, canoLabel)
  if(is.null(canoLevel)) { stop("Could not find a level for label", canoLabel) }
  
  bas_prepare()
  
  bas_run_pho2syl_from_cano_dbi(handle=handle,
                                canoLabel=canoLabel,
                                canoLevel=canoLevel,
                                language=language,
                                verbose=verbose,
                                kasLabel=kasLabel)
  
  bas_clear()
  
  if(canoLevel != kasLabel) { add_attributeDefinition(handle, canoLevel, kasLabel) }
}

bas_run_pho2syl_from_mau<- function(handle,
                         mauLabel,
                         language,
                         verbose,
                         masLabel = "MAS",
                         masLevel = NULL)
{
  if(is.null(masLevel)) { masLevel = masLabel }
  if(!is.null(get_levelDefinition(handle, masLevel))) { stop("Level", masLevel, "already exists!") }
  
  mauLevel = get_levelNameForAttributeName(handle, mauLabel)
  if(is.null(mauLevel)) { stop("Could not find a level for label", mauLabel) }
  if(get_levelDefinition(handle, mauLevel)$type != "SEGMENT")
  {
    stop(mauLevel, "must be a segment tier in order to run pho2syl from segment")
  }
  
  bas_prepare()
  
  bas_run_pho2syl_from_mau_dbi(handle=handle,
                               mauLabel=mauLabel,
                               mauLevel=mauLevel,
                               language=language,
                               verbose=verbose,
                               masLabel=masLabel,
                               masLevel=masLevel)
  
  bas_clear()
  
  add_levelDefinition(handle, masLevel, "SEGMENT")
  if(masLevel != masLabel) { add_attributeDefinition(handle, masLevel, masLabel) }
  
  add_linkDefinition(handle, "MANY_TO_MANY", canoLevel, masLevel)
  autobuild_linkFromTimes(handle, masLevel, mauLevel, convertSuperlevel = TRUE)
}

>>>>>>> 55b1e30267d7870fefb54f360147c67933309bb9

