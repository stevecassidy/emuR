#####################################################################
############################# MAUS ##################################
#####################################################################

bas_run_maus_from_transcription <- function(handle,
                                            transcriptionLabel,
                                            language,
                                            
                                            mausLabel = "MAU",
                                            orthoLabel = "ORT",
                                            canoLabel = "KAN",
                                            
                                            orthoLevel = NULL,
                                            mausLevel = NULL,
                                            
                                            normalize = NULL,
                                            mausMode = NULL,
                                            
                                            perspective = "default",
                                            verbose = TRUE,
                                            resume = FALSE)
{
  transcriptionLevel = get_levelNameForAttributeName(handle, transcriptionLabel)
  if (is.null(transcriptionLevel)) {
    stop("Could not find a level for label ", transcriptionLabel)
  }
  
  if (!is.null(get_levelNameForAttributeName(handle, orthoLabel)))
  {
    stop("There is already a level with label ", orthoLabel)
  }
  
  if (!is.null(get_levelNameForAttributeName(handle, canoLabel)))
  {
    stop("There is already a level with label ", canoLabel)
  }
  
  if (!is.null(get_levelNameForAttributeName(handle, mausLabel)))
  {
    stop("There is already a level with label ", mausLabel)
  }
  
  if (is.null(orthoLevel)) {
    orthoLevel = orthoLabel
  }
  if (is.null(mausLevel)) {
    mausLevel = mausLabel
  }
  
  if (!is.null(get_levelDefinition(handle, orthoLevel))) {
    stop("Level ", orthoLevel, " already exists!")
  }
  if (!is.null(get_levelDefinition(handle, mausLevel))) {
    stop("Level ", mausLevel, " already exists!")
  }
  
  chunkLevel = NULL
  if (get_levelDefinition(handle, transcriptionLevel)$type == "SEGMENT")
  {
    chunkLevel = transcriptionLevel
  }
  
  if (!resume)
  {
    bas_prepare(handle)
  }
  
  bas_run_g2p_for_tokenization_dbi(
    handle = handle,
    transcriptionLabel = transcriptionLabel,
    orthoLabel = orthoLabel,
    orthoLevel = orthoLevel,
    language = language,
    verbose = verbose,
    normalize = normalize,
    resume = resume
  )
  
  bas_run_g2p_from_ortho_dbi(
    handle = handle,
    orthoLevel = orthoLevel,
    orthoLabel = orthoLabel,
    canoLabel = canoLabel,
    language = language,
    verbose = verbose,
    resume = resume
  )
  
  bas_run_maus_from_cano_dbi(
    handle = handle,
    canoLevel = orthoLevel,
    canoLabel = canoLabel,
    language = language,
    chunkLevel = chunkLevel,
    mausLabel = mausLabel,
    mausLevel = mausLevel,
    verbose = verbose,
    resume = resume
  )
  
  bas_clear(handle)
  
  
  add_levelDefinition(handle, orthoLevel, "ITEM", verbose = verbose)
  add_levelDefinition(handle, mausLevel, "SEGMENT", verbose = verbose)
  
  if (orthoLevel != canoLabel) {
    add_attributeDefinition(handle, orthoLevel, canoLabel, verbose = verbose)
  }
  if (orthoLevel != orthoLabel) {
    add_attributeDefinition(handle, orthoLevel, orthoLabel, verbose = verbose)
  }
  if (mausLevel != mausLabel) {
    add_attributeDefinition(handle, mausLevel, mausLabel, verbose = verbose)
  }
  add_linkDefinition(handle, "ONE_TO_MANY", transcriptionLevel, orthoLevel)
  add_linkDefinition(handle, "ONE_TO_MANY", orthoLevel, mausLevel)
  
  bas_new_canvas(handle, perspective, mausLevel)
  rewrite_allAnnots(handle, verbose = verbose)
}

bas_run_maus_from_orth <- function(handle,
                                   orthoLabel,
                                   language,
                                   
                                   canoLabel = "KAN",
                                   mausLabel = "MAU",
                                   
                                   mausLevel = NULL,
                                   chunkLevel = NULL,
                                   
                                   mausMode = NULL,
                                   
                                   verbose = TRUE,
                                   perspective = "default",
                                   resume = FALSE)
{
  if (!is.null(get_levelNameForAttributeName(handle, mausLabel)))
  {
    stop("There is already a level with label ", mausLabel)
  }
  
  if (!is.null(get_levelNameForAttributeName(handle, canoLabel)))
  {
    stop("There is already a level with label ", canoLabel)
  }
  
  if (is.null(mausLevel)) {
    mausLevel = mausLabel
  }
  
  if (!is.null(get_levelDefinition(handle, mausLevel))) {
    stop("Level ", mausLevel, " already exists!")
  }
  
  orthLevel = get_levelNameForAttributeName(handle, orthLabel)
  if (is.null(orthLevel)) {
    stop("Could not find a level for label", orthLabel)
  }
  
  if (is.null(chunkLevel) &&
      get_levelDefinition(handle, orthLevel)$type == "SEGMENT") {
    chunkLevel = orthLevel
  }
  
  if (!resume)
  {
    bas_prepare(handle)
  }
  
  bas_run_g2p_from_ortho_dbi(
    handle = handle,
    orthLabel = orthLabel,
    orthLevel = orthLevel,
    language = language,
    canoLabel = canoLabel,
    verbose = verbose,
    resume = resume
  )
  
  bas_run_maus_from_cano_dbi(
    handle = handle,
    canoLabel = canoLabel,
    canoLevel = orthLevel,
    language = language,
    mausLabel = mausLabel,
    chunkLevel = chunkLevel,
    verbose = verbose,
    verbose = verbose,
    resume = resume
  )
  
  bas_clear(handle)
  
  add_levelDefinition(handle, mausLevel, "SEGMENT", verbose = verbose)
  if (mausLevel != mausLabel) {
    add_attributeDefinition(handle, mausLevel, mausLabel, verbose = verbose)
  }
  add_linkDefinition(handle, "ONE_TO_MANY", orthLevel, mausLevel)
  bas_new_canvas(handle, perspective, mausLevel)
  rewrite_allAnnots(handle, verbose = verbose)
}

bas_run_maus_from_cano <- function(handle,
                                   canoLabel,
                                   language,
                                   
                                   mausLabel = "MAU",
                                   
                                   chunkLevel = NULL,
                                   mausLevel = NULL,
                                   
                                   mausMode = NULL,
                                   
                                   verbose = TRUE,
                                   perspective = "default",
                                   resume = FALSE)
{
  if (is.null(mausLevel)) {
    mausLevel = mausLabel
  }
  if (!is.null(get_levelDefinition(handle, mausLevel))) {
    stop("Level", mausLevel, "already exists!")
  }
  
  canoLevel = get_levelNameForAttributeName(handle, canoLabel)
  if (is.null(canoLevel)) {
    stop("Could not find a level for label", canoLabel)
  }
  
  if (is.null(chunkLevel) &&
      get_levelDefinition(handle, canoLevel)$type == "SEGMENT") {
    chunkLevel = canoLevel
  }
  
  if(!is.null(chunkLevel))
  {
    if(is.null(get_levelDefinition(handle, chunkLevel)))
    {
      stop("Could not find level ", chunkLevel)
    }
    if(get_levelDefinition(handle, chunkLevel)$type != "SEGMENT")
    {
      stop("TRN level ", chunkLevel, " must be a segment level")
    }
  }
  
  if (!resume)
  {
    bas_prepare(handle)
  }
  
  bas_run_maus_from_cano_dbi(
    handle = handle,
    canoLabel = canoLabel,
    canoLevel = canoLevel,
    mausLabel = mausLabel,
    mausLevel = mausLevel,
    language = language,
    chunkLevel = chunkLevel,
    verbose = verbose,
    resume = resume
  )
  
  bas_clear(handle)
  
  add_levelDefinition(handle, mausLevel, "SEGMENT", verbose = verbose)
  if (mausLevel != mausLabel) {
    add_attributeDefinition(handle, mausLevel, mausLabel, verbose = verbose)
  }
  add_linkDefinition(handle, "ONE_TO_MANY", canoLevel, mausLevel)
  
  bas_new_canvas(handle, perspective, mausLevel)
  rewrite_allAnnots(handle, verbose = verbose)
}


#####################################################################
############################## G2P ##################################
#####################################################################

bas_run_g2p_for_tokenization <- function(handle,
                                         transcriptionLabel,
                                         language,
                                         
                                         orthoLabel = "ORT",
                                         
                                         normalize = NULL,
                                         
                                         orthoLevel = NULL,
                                         verbose = TRUE,
                                         resume = FALSE)
{
  transcriptionLevel = get_levelNameForAttributeName(handle, transcriptionLabel)
  if (is.null(transcriptionLevel)) {
    stop("Could not find a level for label ", transcriptionLabel)
  }
  
  if (!is.null(get_levelNameForAttributeName(handle, orthoLabel)))
  {
    stop("There is already a level with label", orthoLabel)
  }
  
  if (is.null(orthoLevel)) {
    orthoLevel = orthoLabel
  }
  
  if (!is.null(get_levelDefinition(handle, orthoLevel))) {
    stop("Level ", orthoLevel, " already exists!")
  }
  
  if (!resume)
  {
    bas_prepare(handle)
  }
  
  bas_run_g2p_for_tokenization_dbi(
    handle = handle,
    transcriptionLabel = transcriptionLabel,
    orthoLabel = orthoLabel,
    orthoLevel = orthoLevel,
    language = language,
    normalize = normalize,
    verbose = verbose,
    resume = resume
  )
  
  bas_clear(handle)
  
  
  add_levelDefinition(handle, orthoLevel, "ITEM", verbose = verbose)
  
  if (orthoLevel != orthoLabel) {
    add_attributeDefinition(handle, orthoLevel, orthoLabel, verbose = verbose)
  }
  
  add_linkDefinition(handle, "ONE_TO_MANY", transcriptionLevel, orthoLevel)
  rewrite_allAnnots(handle, verbose = verbose)
}


bas_run_g2p_from_ortho <- function(handle,
                                   orthoLabel,
                                   language,
                                   
                                   canoLabel = "KAN",
                                   orthoLevel = NULL,
                                   
                                   verbose = TRUE,
                                   resume = FALSE)
{
  orthoLevel = get_levelNameForAttributeName(handle, orthoLabel)
  if (is.null(orthoLevel)) {
    stop("Could not find a level for label ", orthoLabel)
  }
  
  canoLevel = get_levelNameForAttributeName(handle, canoLabel)
  if (!is.null(canoLevel)) {
    stop("There is aleady a level with the label ", canoLabel)
  }
  
  if (!resume)
  {
    bas_prepare(handle)
  }
  
  
  bas_run_g2p_from_ortho_dbi(
    handle = handle,
    orthoLabel = orthoLabel,
    orthoLevel = orthoLevel,
    language = language,
    canoLabel = canoLabel,
    verbose = verbose,
    resume = resume
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
                                           
                                           canoLabel = "KAN",
                                           orthoLabel = "ORT",
                                           
                                           normalize = NULL,
                                           
                                           orthoLevel = NULL,
                                           verbose = TRUE,
                                           resume = FALSE)
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
  
  if (!resume)
  {
    bas_prepare(handle)
  }
  
  bas_run_g2p_for_tokenization_dbi(
    handle = handle,
    transcriptionLabel = transcriptionLabel,
    orthoLabel = orthoLabel,
    orthoLevel = orthoLevel,
    language = language,
    verbose = verbose,
    normalize = normalize,
    resume = resume
  )
  
  bas_run_g2p_from_ortho_dbi(
    handle = handle,
    orthoLevel = orthoLevel,
    orthoLabel = orthoLabel,
    canoLabel = canoLabel,
    language = language,
    verbose = verbose,
    resume = resume
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
                                      
                                      chunkLabel = "TRN",
                                      rootLevel = NULL,
                                      
                                      chunkLevel = NULL,
                                      orthoLabel = NULL,
                                      
                                      minAnchorLength = NULL,
                                      minChunkDuration = NULL,
                                      minSilenceDuration = NULL,
                                      
                                      verbose = TRUE,
                                      perspective = "default",
                                      resume = FALSE)
{
  if (is.null(chunkLevel)) {
    chunkLevel = chunkLabel
  }
  if (!is.null(get_levelDefinition(handle, chunkLevel))) {
    stop("Level ", chunkLevel, " already exists!")
  }
  
  canoLevel = get_levelNameForAttributeName(handle, canoLabel)
  if (is.null(canoLevel)) {
    stop("Could not find a level for label ", canoLabel)
  }
  
  if ((!is.null(rootLevel)) &&
      is.null(get_levelDefinition(handle, rootLevel)))
  {
    stop("Root level ", rootLevel, " does not exist")
  }
  
  if (!resume)
  {
    bas_prepare(handle)
  }
  
  bas_run_chunker_from_cano_dbi(
    handle = handle,
    canoLabel = canoLabel,
    canoLevel = canoLevel,
    chunkLabel = chunkLabel,
    language = language,
    verbose = verbose,
    chunkLevel = chunkLevel,
    topLevel = rootLevel,
    orthoLabel = orthoLabel,
    resume = resume
  )
  
  
  bas_clear(handle)
  
  add_levelDefinition(handle, chunkLevel, "SEGMENT", verbose = verbose)
  if (chunkLevel != chunkLabel) {
    add_attributeDefinition(handle, chunkLevel, chunkLabel, verbose = verbose)
  }
  add_linkDefinition(handle, "ONE_TO_MANY", chunkLevel, canoLevel)
  if (!is.null(rootLevel))
  {
    add_linkDefinition(handle, "ONE_TO_MANY", rootLevel, chunkLevel)
  }
  bas_new_canvas(handle, perspective, chunkLevel)
  rewrite_allAnnots(handle, verbose = verbose)
}




#####################################################################
############################ MINNI ##################################
#####################################################################

bas_run_minni <- function(handle,
                          language,
                          
                          minniLabel = "MINNI",
                          rootLevel = NULL,
                          
                          minniLevel = NULL,
                          
                          verbose = TRUE,
                          perspective = "default",
                          resume = FALSE)
{
  if (is.null(minniLevel)) {
    minniLevel = minniLabel
  }
  if (!is.null(get_levelDefinition(handle, minniLevel))) {
    stop("Level ", minniLevel, " already exists!")
  }
  
  if ((!is.null(rootLevel)) &&
      is.null(get_levelDefinition(handle, rootLevel)))
  {
    stop("Root level ", rootLevel, " does not exist")
  }
  
  if (!resume)
  {
    bas_prepare(handle)
  }
  
  bas_run_minni_dbi(
    handle = handle,
    language = language,
    minniLabel = minniLabel,
    minniLevel = minniLevel,
    verbose = verbose,
    topLevel = rootLevel,
    resume = resume
  )
  
  bas_clear(handle)
  
  add_levelDefinition(handle, minniLevel, "SEGMENT", verbose = verbose)
  if (minniLevel != minniLabel) {
    add_attributeDefinition(handle, minniLevel, minniLabel, verbose = verbose)
  }
  if (!is.null(rootLevel))
  {
    add_linkDefinition(handle, "ONE_TO_MANY", rootLevel, minniLevel)
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
                                      canoSylLabel = "KAS",
                                      canoSylLevel = NULL,
                                      verbose = TRUE,
                                      resume = FALSE)
{
  canoLevel = get_levelNameForAttributeName(handle, canoLabel)
  if (is.null(canoLevel)) {
    stop("Could not find a level for label ", canoLabel)
  }
  
  canoSylLevel = get_levelNameForAttributeName(handle, canoSylLabel)
  if (!is.null(canoSylLevel)) {
    stop("There is aleady a level with the label ", canoSylLabel)
  }
  
  if (!resume)
  {
    bas_prepare(handle)
  }
  
  bas_run_pho2syl_from_cano_dbi(
    handle = handle,
    canoLabel = canoLabel,
    canoLevel = canoLevel,
    language = language,
    verbose = verbose,
    canoSylLabel = canoSylLabel,
    resume = resume
  )
  
  bas_clear(handle)
  
  if (canoLevel != canoSylLabel) {
    add_attributeDefinition(handle, canoLevel, canoSylLabel, verbose = verbose)
  }
  rewrite_allAnnots(handle)
}

bas_run_pho2syl_from_mau <- function(handle,
                                     mausLabel,
                                     canoLabel,
                                     language,
                                     
                                     sylLabel = "MAS",
                                     sylLevel = NULL,
                                     perspective = "default",
                                     resume = FALSE,
                                     verbose = TRUE)
{
  if (!is.null(get_levelNameForAttributeName(handle, sylLabel)))
  {
    stop("There is already a level with label ", sylLabel)
  }
  
  if (is.null(sylLevel)) {
    sylLevel = sylLabel
  }
  
  if (!is.null(get_levelDefinition(handle, sylLevel))) {
    stop("Level ", sylLevel, " already exists!")
  }
  
  mausLevel = get_levelNameForAttributeName(handle, mausLabel)
  if (is.null(mausLevel)) {
    stop("Could not find a level for label ", mausLabel)
  }
  if (get_levelDefinition(handle, mausLevel)$type != "SEGMENT")
  {
    stop(mausLevel,
         " must be a segment tier in order to run pho2syl from segment")
  }
  
  canoLevel = get_levelNameForAttributeName(handle, canoLabel)
  if (is.null(canoLevel)) {
    stop("Could not find a level for label ", canoLabel)
  }
  
  if (!resume)
  {
    bas_prepare(handle)
  }
  
  bas_run_pho2syl_from_mau_dbi(
    handle = handle,
    mausLabel = mausLabel,
    mausLevel = mausLevel,
    language = language,
    verbose = verbose,
    sylLabel = sylLabel,
    sylLevel = sylLevel,
    canoLevel = canoLevel,
    resume = resume
  )
  
  bas_clear(handle)
  
  add_levelDefinition(handle, sylLevel, "SEGMENT", verbose = verbose)
  if (sylLevel != sylLabel) {
    add_attributeDefinition(handle, sylLevel, sylLabel, verbose = verbose)
  }
  
  add_linkDefinition(handle, "MANY_TO_MANY", canoLevel, sylLevel)
  add_linkDefinition(handle, "ONE_TO_MANY", sylLevel, mausLevel)
  autobuild_linkFromTimes(handle,
                          sylLevel,
                          mausLevel,
                          convertSuperlevel = FALSE,
                          verbose = verbose)
  bas_new_canvas(handle, perspective, sylLevel)
  rewrite_allAnnots(handle, verbose = verbose)
}