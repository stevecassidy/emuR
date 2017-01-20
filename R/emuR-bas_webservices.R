#####################################################################
############################# MAUS ##################################
#####################################################################

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
  
  if (!is.null(get_levelNameForAttributeName(handle, orthoLabel)))
  {
    stop("There is already a level with label", orthoLabel)
  }
  
  if (!is.null(get_levelNameForAttributeName(handle, canoLabel)))
  {
    stop("There is already a level with label", canoLabel)
  }
  
  if (!is.null(get_levelNameForAttributeName(handle, mauLabel)))
  {
    stop("There is already a level with label", mauLabel)
  }
  
  if (is.null(orthoLevel)) {
    orthoLevel = orthoLabel
  }
  if (is.null(mauLevel)) {
    mauLevel = mauLabel
  }
  
  if (!is.null(get_levelDefinition(handle, orthoLevel))) {
    stop("Level ", orthoLevel, " already exists!")
  }
  if (!is.null(get_levelDefinition(handle, mauLevel))) {
    stop("Level ", mauLevel, " already exists!")
  }
  
  trnLevel = NULL
  if (get_levelDefinition(handle, transcriptionLevel)$type == "SEGMENT")
  {
    trnLevel = transcriptionLevel
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
  
  bas_run_maus_from_cano_dbi(
    handle = handle,
    canoLevel = orthoLevel,
    canoLabel = canoLabel,
    language = language,
    verbose = verbose,
    trnLevel = trnLevel,
    mauLabel = mauLabel,
    mauLevel = mauLevel
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
  if (!is.null(get_levelNameForAttributeName(handle, mauLabel)))
  {
    stop("There is already a level with label ", mauLabel)
  }
  
  if (!is.null(get_levelNameForAttributeName(handle, canoLabel)))
  {
    stop("There is already a level with label ", canoLabel)
  }
  
  if (is.null(mauLevel)) {
    mauLevel = mauLabel
  }
  
  if (!is.null(get_levelDefinition(handle, mauLevel))) {
    stop("Level ", mauLevel, " already exists!")
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
  stop()
  
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
  
  stop()
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
  
  stop()
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
  
  stop()
  
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
  
  stop()
  
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
  if (!is.null(topLevel))
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
                                      verbose = TRUE,
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
                                     verbose = TRUE,
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
    stop(mauLevel,
         "must be a segment tier in order to run pho2syl from segment")
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
  
  stop()
  
  bas_clear(handle)
  
  add_levelDefinition(handle, masLevel, "SEGMENT", verbose = verbose)
  if (masLevel != masLabel) {
    add_attributeDefinition(handle, masLevel, masLabel, verbose = verbose)
  }
  
  add_linkDefinition(handle, "MANY_TO_MANY", canoLevel, masLevel)
  autobuild_linkFromTimes(handle,
                          masLevel,
                          mauLevel,
                          convertSuperlevel = TRUE,
                          verbose = verbose)
  bas_new_canvas(handle, perspective, masLevel)
  rewrite_allAnnots(handle, verbose = verbose)
}