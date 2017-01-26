##' Runs several BAS webservices with default options, starting from an orthographic transcription
##'
##' This function calls the BAS webservices G2P, MAUS, Pho2Syl, MINNI and (if necessary) Chunker.
##' Starting from an orthographic transcription, it derives a tokenized orthographical word tier
##' using the G2P tool. It also derives canonical pronunciations (in SAMPA) for the words.
##' If the audio file is longer than 60 seconds, the function then calls the Chunker webservice
##' to presegment the recording. Subsequently, the webservice MAUS is called to derive a phonetic
##' segmentation. A second, rough segmentation is created by running the phoneme decoder MINNI.
##' Finally, syllabification is performed by calling Pho2Syl. All necessary level, attribute and link
##' definitions are created in the process. \strong{This function requires an internet connection.}
##'
##' Note that this function will run all BAS webservices in default mode. If you wish to change
##' options, you must use the individual runBASwebservices functions. This will also allow you to carry
##' out manual corrections in between the individual steps, or to use different languages for different
##' webservices.
##'
##' @family BAS webservice functions
##'
##' @export
##' @param handle emuDB handle
##' @param transcriptionLabel name of the label (not level!) containing an orthographic transcription.
##' @param language language to be used. Up-to-date lists of the languages accepted by all webservices can be found here:
##' \url{https://clarin.phonetik.uni-muenchen.de/BASWebServices/services/help}
##'
##' @param orthoLabel label name for orthographic words
##' @param canoLabel label name for canonical pronunciations of words
##' @param chunkLabel label name for the chunk segmentation
##' Please note that the chunk segmentation will only be generated if your emuDB contains
##' audio files beyond the one minute mark.
##' @param mausLabel label name for the MAUS segmentation
##' @param minniLabel label name for the MINNI segmentation
##' @param sylLabel label name for syllable segmentation
##' @param canoSylLabel label name for syllabified canonical pronunciations of words
##'
##' @param verbose Display progress bars and other information
##' @param resume If a previous call to this function has failed (and you think you have fixed the issue
##' that caused the error), you can set resume=TRUE to recover any progress made up to that point. This
##' will only work if you have not run any other emuR functions (such as the query function or other
##' webservice functions) in the meantime, as they are likely to delete your temporary data.

runBASwebservice_all <- function(handle,
                                 transcriptionLabel,
                                 language,
                                 
                                 mausLabel = "MAU",
                                 orthoLabel = "ORT",
                                 canoLabel = "KAN",
                                 sylLabel = "MAS",
                                 canoSylLabel = "KAS",
                                 minniLabel = "MINNI",
                                 chunkLabel = "TRN",
                                 
                                 resume = FALSE,
                                 verbose = TRUE)
{
  transcriptionLevel = get_levelNameForAttributeName(handle, transcriptionLabel)
  
  if (is.null(transcriptionLevel)) {
    stop("Could not find a level for label ", transcriptionLabel)
  }
  
  for (label in c(orthoLabel,
                  canoLabel,
                  mausLabel,
                  sylLabel,
                  canoSylLabel,
                  minniLabel,
                  chunkLabel))
  {
    if (!is.null(get_levelNameForAttributeName(handle, label)))
    {
      stop("There is already a level with label ", label)
    }
  }
  
  
  
  orthoLevel = orthoLabel
  mausLevel = mausLabel
  sylLevel = sylLabel
  minniLevel = minniLabel
  
  chunkLevel = NULL
  running_chunker = FALSE
  
  # if our transcription is a segment level, we assume it is a manual chunk segmentation
  if (get_levelDefinition(handle, transcriptionLevel)$type == "SEGMENT") {
    chunkLevel = transcriptionLevel
  }
  
  # else, we check if we will need to perform automatic chunk segmentation
  else if (bas_long_enough_for_chunker(handle)) {
    running_chunker = TRUE
    chunkLevel = chunkLabel
  }
  
  if (!resume) {
    bas_prepare(handle)
  }
  
  bas_run_g2p_for_tokenization_dbi(
    handle = handle,
    transcriptionLabel = transcriptionLabel,
    orthoLabel = orthoLabel,
    orthoLevel = orthoLevel,
    language = language,
    verbose = verbose,
    resume = resume,
    params = list()
  )
  
  bas_run_g2p_for_pronunciation_dbi(
    handle = handle,
    orthoLevel = orthoLevel,
    orthoLabel = orthoLabel,
    canoLabel = canoLabel,
    language = language,
    verbose = verbose,
    resume = resume,
    params = list()
  )
  
  # if we previously decided to run automatic chunk segmentation
  if (running_chunker)
  {
    bas_run_chunker_dbi(
      handle = handle,
      canoLabel = canoLabel,
      canoLevel = canoLevel,
      chunkLabel = chunkLabel,
      chunkLevel = chunkLevel,
      orthoLabel = orthoLabel,
      topLevel = transcriptionLevel,
      params = list(),
      resume = resume,
      verbose = verbose,
      language = language
    )
  }
  
  bas_run_maus_dbi(
    handle = handle,
    canoLevel = orthoLevel,
    canoLabel = canoLabel,
    language = language,
    chunkLevel = chunkLevel,
    mausLabel = mausLabel,
    mausLevel = mausLevel,
    verbose = verbose,
    resume = resume,
    params = list()
  )
  
  bas_run_minni_dbi(
    handle = handle,
    language = language,
    minniLabel = minniLabel,
    minniLevel = minniLevel,
    topLevel = transcriptionLevel,
    verbose = verbose,
    resume = resume,
    params = list()
  )
  
  bas_run_pho2syl_canonical_dbi(
    handle = handle,
    canoLabel = canoLabel,
    canoSylLabel = canoSylLabel,
    canoLevel = orthoLevel,
    language = language,
    verbose = verbose,
    params = list(),
    resume = resume
  )
  
  bas_run_pho2syl_segmental_dbi(
    handle = handle,
    mausLabel = mausLabel,
    mausLevel = mausLevel,
    language = language,
    sylLabel = sylLabel,
    sylLevel = sylLevel,
    wordLevel = orthoLevel,
    resume = resume,
    params = list(),
    verbose = verbose
  )
  
  bas_clear(handle)
  
  add_levelDefinition(handle, orthoLevel, "ITEM", verbose = verbose)
  
  if (running_chunker)
  {
    add_levelDefinition(handle, chunkLevel, "SEGMENT", verbose = verbose)
    bas_segment_to_item_level_dbi(handle, chunkLevel)
  }
  
  add_levelDefinition(handle, sylLevel, "SEGMENT", verbose = verbose)
  
  add_levelDefinition(handle, mausLevel, "SEGMENT", verbose = verbose)
  bas_new_canvas(handle, "default", mausLevel)
  
  add_levelDefinition(handle, minniLevel, "SEGMENT", verbose = verbose)
  bas_new_canvas(handle, "default", minniLevel)
  
  add_attributeDefinition(handle, orthoLevel, canoLabel, verbose = verbose)
  add_attributeDefinition(handle, orthoLevel, canoSylLabel, verbose = verbose)
  
  # if we ran the chunker, our path is transcription -> chunk -> word
  if (running_chunker)
  {
    add_linkDefinition(handle, "ONE_TO_MANY", transcriptionLevel, chunkLevel)
    add_linkDefinition(handle, "ONE_TO_MANY", chunkLevel, orthoLevel)
  }
  # else, it is transcription -> word
  else
  {
    add_linkDefinition(handle, "ONE_TO_MANY", transcriptionLevel, orthoLevel)
  }
  
  add_linkDefinition(handle, "MANY_TO_MANY", orthoLevel, sylLevel)
  add_linkDefinition(handle, "ONE_TO_MANY", orthoLevel, mausLevel)
  
  add_linkDefinition(handle, "ONE_TO_MANY", transcriptionLevel, minniLevel)
  
  add_linkDefinition(handle, "ONE_TO_MANY", sylLevel, mausLevel)
  
  autobuild_linkFromTimes(
    handle,
    sylLevel,
    mausLevel,
    rewriteAllAnnots = FALSE,
    convertSuperlevel = TRUE
  )
  
  rewrite_allAnnots(handle, verbose = verbose)
}

##' Runs MAUS webservice to create a phonetic segmentation
##'
##' This function calls the BAS webservice MAUS to generate a phonemic segmentation.
##' It requires a word-tokenized tier with a SAMPA pronunciation, which can be generated
##' by the function \link{runBASwebservice_g2pForPronunciation}. All necessary level, attribute and link definitions
##' are created in the process. \strong{This function requires an internet connection.}
##'
##' @family BAS webservice functions
##'
##' @export
##'
##' @param canoLabel name of the label (not level!) containing the SAMPA word pronunciations.
##' If this label resides on a segment level, the segment time information is used as a presegmentation.
##' If it is an item level, no assumption is made about the temporal position of segments.
##' @param mausLevel name of the level for the MAUS segmentation. Defaults to the value of mausLabel.
##' @param chunkLevel if you have a chunk segmentation level, you can provide it to improve the speed and accuracy
##' of MAUS. The chunk segmentation level must be a segment level, and it must link to the level of orthoLabel.
##' @param chunkLevelToItem if TRUE, and if a chunk level is provided, the chunk level is converted into an ITEM level after segmentation
##' @param params named list of parameters to be passed on to the webservice. It is your own reponsibility to
##' ensure that these parameters are compatible with the webservice API
##' (see \url{https://clarin.phonetik.uni-muenchen.de/BASWebServices/services/help}).
##' Some options accepted by the API (e.g. output format) cannot be set when calling a webservice from within emuR,
##' and will be overridden.
##' @param perspective the webApp perspective that the new level will be added to.
##' If NULL, the new level is not added to any perspectives.
##'
##' @inheritParams runBASwebservice_all

runBASwebservice_maus <- function(handle,
                                  canoLabel,
                                  language,
                                  
                                  mausLabel = "MAU",
                                  
                                  chunkLevel = NULL,
                                  chunkLevelToItem = TRUE,
                                  mausLevel = NULL,
                                  
                                  params = NULL,
                                  
                                  perspective = "default",
                                  resume = FALSE,
                                  verbose = TRUE)
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
  
  if (!is.null(chunkLevel))
  {
    if (is.null(get_levelDefinition(handle, chunkLevel)))
    {
      stop("Could not find level ", chunkLevel)
    }
    if (get_levelDefinition(handle, chunkLevel)$type != "SEGMENT")
    {
      stop("Chunk level ", chunkLevel, " must be a segment level")
    }
  }
  
  if (!resume)
  {
    bas_prepare(handle)
  }
  
  bas_run_maus_dbi(
    handle = handle,
    canoLabel = canoLabel,
    canoLevel = canoLevel,
    mausLabel = mausLabel,
    mausLevel = mausLevel,
    language = language,
    chunkLevel = chunkLevel,
    verbose = verbose,
    resume = resume,
    params = params
  )
  
  bas_clear(handle)
  
  if(chunkLevelToItem && !is.null(chunkLevel))
  {
    bas_segment_to_item_level(handle, chunkLevel)
  }
  
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


##' Tokenizes an orthographic transcription.
##'
##' @family BAS webservice functions
##'
##' @export
##' @param orthoLevel name of the word level that will be created. Defaults to the value of orthoLabel.
##' @inheritParams runBASwebservice_maus
##' @inheritParams runBASwebservice_all

runBASwebservice_g2pForTokenization <- function(handle,
                                                transcriptionLabel,
                                                language,
                                                
                                                orthoLabel = "ORT",
                                                
                                                params = list(),
                                                
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
    resume = resume,
    params = params
  )
  
  bas_clear(handle)
  
  
  add_levelDefinition(handle, orthoLevel, "ITEM", verbose = verbose)
  
  if (orthoLevel != orthoLabel) {
    add_attributeDefinition(handle, orthoLevel, orthoLabel, verbose = verbose)
  }
  
  add_linkDefinition(handle, "ONE_TO_MANY", transcriptionLevel, orthoLevel)
  rewrite_allAnnots(handle, verbose = verbose)
}

##' Creates SAMPA labels for a tier of tokenized orthographical words.
##'
##' @family BAS webservice functions
##' @export
##'
##' @param orthoLabel name of a label (not level!) containing orthographic words.
##' 
##' @inheritParams runBASwebservice_all
##' @inheritParams runBASwebservice_maus


runBASwebservice_g2pForPronunciation <- function(handle,
                                                 orthoLabel,
                                                 language,
                                                 
                                                 canoLabel = "KAN",
                                                 
                                                 params = list(),
                                                 
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
  
  
  bas_run_g2p_for_pronunciation_dbi(
    handle = handle,
    orthoLabel = orthoLabel,
    orthoLevel = orthoLevel,
    language = language,
    canoLabel = canoLabel,
    verbose = verbose,
    resume = resume,
    params = params
  )
  
  bas_clear(handle)
  
  if (orthoLevel != canoLabel) {
    add_attributeDefinition(handle, orthoLevel, canoLabel, verbose = verbose)
  }
  rewrite_allAnnots(handle, verbose = verbose)
}



#####################################################################
########################### CHUNKER #################################
#####################################################################

##' Creates a chunk segmentation using the webservice Chunker.
##' @family BAS webservice functions
##'
##' @export
##' @param canoLabel name of the label (not level!) containing a canonical pronunciation of the words.
##' @param rootLevel if provided, the new level will be linked to the root level
##' @param orthoLabel if provided, chunk labels will contain orthographic instead of SAMPA strings.
##' Must be paired with the canonical pronunciation labels in canoLabel.
##' @param chunkLabel label name for the chunk segmentation
##' @param chunkLevel name of the level for the chunk segmentation. Defaults to the value of chunkLabel
##'
##' @inheritParams runBASwebservice_all
##' @inheritParams runBASwebservice_maus

runBASwebservice_chunker <- function(handle,
                                     canoLabel,
                                     language,
                                     
                                     chunkLabel = "TRN",
                                     rootLevel = NULL,
                                     
                                     chunkLevel = NULL,
                                     orthoLabel = NULL,
                                     
                                     params = list(),
                                     
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
  
  bas_run_chunker_dbi(
    handle = handle,
    canoLabel = canoLabel,
    canoLevel = canoLevel,
    chunkLabel = chunkLabel,
    language = language,
    verbose = verbose,
    chunkLevel = chunkLevel,
    topLevel = rootLevel,
    orthoLabel = orthoLabel,
    resume = resume,
    params = params
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

##' Creates a rough phonetic segmentation by running the phoneme decoder webservice MINNI.
##' @family BAS webservice functions
##' @export
##' 
##' @inheritParams runBASwebservice_all
##' @inheritParams runBASwebservice_maus
##' @inheritParams runBASwebservice_chunker



runBASwebservice_minni <- function(handle,
                                   language,
                                   
                                   minniLabel = "MINNI",
                                   rootLevel = NULL,
                                   
                                   minniLevel = NULL,
                                   
                                   params = list(),
                                   
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
    resume = resume,
    params = params
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

##' Adds syllabified word labels to a word level that already contains a canonical pronunciation label.
##'
##' @family BAS webservice functions
##' @export
##' @param canoLabel name of the label (not level!) containing a canonical pronunciation of the words.
##'
##' @inheritParams runBASwebservice_all
##' @inheritParams runBASwebservice_maus

runBASwebservice_pho2sylCanonical <- function(handle,
                                              canoLabel,
                                              language,
                                              canoSylLabel = "KAS",
                                              
                                              params = list(),
                                              
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
  
  bas_run_pho2syl_canonical_dbi(
    handle = handle,
    canoLabel = canoLabel,
    canoLevel = canoLevel,
    language = language,
    verbose = verbose,
    canoSylLabel = canoSylLabel,
    resume = resume,
    params = params
  )
  
  bas_clear(handle)
  
  if (canoLevel != canoSylLabel) {
    add_attributeDefinition(handle, canoLevel, canoSylLabel, verbose = verbose)
  }
  rewrite_allAnnots(handle)
}


##' Creates a syllable segmentation on the basis of a phonetic segmentation.
##' @family BAS webservice functions
##' @export
##' @param segmentLabel name of the label (not level!) containing a phonetic segmentation.
##' @param wordLevel name of word level. Must be a parent lavel of the segmentation level.
##' @param sylLevel name of the new syllabification level. Defaults to the value of sylLabel.
##'
##' @inheritParams runBASwebservice_all
##' @inheritParams runBASwebservice_maus

runBASwebservice_pho2sylSegmental <- function(handle,
                                              segmentLabel,
                                              wordLevel,
                                              language,
                                              
                                              sylLabel = "MAS",
                                              sylLevel = NULL,
                                              
                                              params = list(),
                                              
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
  
  segmentLevel = get_levelNameForAttributeName(handle, segmentLabel)
  if (is.null(segmentLevel)) {
    stop("Could not find a level for label ", segmentLevel)
  }
  if (get_levelDefinition(handle, mausLevel)$type != "SEGMENT")
  {
    stop(segmentLabel,
         " must be a segment tier in order to run pho2syl from segment")
  }
  
  if (is.null(get_levelDefinition(handle, wordLevel))) {
    stop("Could not find a level for label ", wordLevel)
  }
  
  if (!resume)
  {
    bas_prepare(handle)
  }
  
  bas_run_pho2syl_segmental_dbi(
    handle = handle,
    mausLabel = segmentLabel,
    mausLevel = segmentLevel,
    language = language,
    verbose = verbose,
    sylLabel = sylLabel,
    sylLevel = sylLevel,
    wordLevel = wordLevel,
    resume = resume,
    params = params
  )
  
  bas_clear(handle)
  
  add_levelDefinition(handle, sylLevel, "SEGMENT", verbose = verbose)
  if (sylLevel != sylLabel) {
    add_attributeDefinition(handle, sylLevel, sylLabel, verbose = verbose)
  }
  
  add_linkDefinition(handle, "MANY_TO_MANY", wordLevel, sylLevel)
  add_linkDefinition(handle, "ONE_TO_MANY", sylLevel, mausLevel)
  autobuild_linkFromTimes(handle,
                          sylLevel,
                          mausLevel,
                          convertSuperlevel = TRUE,
                          verbose = verbose)
  rewrite_allAnnots(handle, verbose = verbose)
}