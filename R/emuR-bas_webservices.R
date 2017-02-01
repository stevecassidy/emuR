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
##' @param language language(s) to be used. If you pass a single string (e.g. "deu-DE"), this language will be used for all bundles.
##' Alternatively, you can select the language for every bundle individually. To do so, you must pass a data frame with the columns
##' session, bundle, language. This data frame must contain one row for every bundle in your emuDB. 
##' Up-to-date lists of the languages accepted by all webservices can be found here:
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
                                 
                                 orthoLabel = "ORT",
                                 canoLabel = "KAN",
                                 mausLabel = "MAU",
                                 minniLabel = "MINNI",
                                 sylLabel = "MAS",
                                 canoSylLabel = "KAS",
                                 chunkLabel = "TRN",
                                 
                                 resume = FALSE,
                                 verbose = TRUE)
{
  transcriptionLevel = get_levelNameForAttributeName(handle, transcriptionLabel)
  
  oldBasePath = handle$basePath
  
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
    bas_check_this_is_a_new_label(handle, label)
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
    chunkLabel = transcriptionLabel
  }
  
  # else, we check if we will need to perform automatic chunk segmentation
  else if (bas_long_enough_for_chunker(handle, oldBasePath)) {
    running_chunker = TRUE
    chunkLevel = chunkLabel
  }
  
  else
  {
    chunkLabel = NULL
  }
  
  languages = bas_evaluate_language_option(handle = handle, language = language)
  
  
  handle = bas_prepare(handle, resume, verbose)

  add_levelDefinition(handle, orthoLevel, "ITEM", verbose = FALSE, rewriteAllAnnots = FALSE)
  
  if(!running_chunker)
  {
    add_linkDefinition(handle, "ONE_TO_MANY", transcriptionLevel, orthoLevel)
  }
  
  bas_run_g2p_for_tokenization_dbi(
    handle = handle,
    transcriptionLabel = transcriptionLabel,
    orthoLabel = orthoLabel,
    orthoLevel = orthoLevel,
    languages = languages,
    verbose = verbose,
    resume = resume,
    params = list()
  )

  bas_run_g2p_for_pronunciation_dbi(
    handle = handle,
    orthoLevel = orthoLevel,
    orthoLabel = orthoLabel,
    canoLabel = canoLabel,
    languages = languages,
    verbose = verbose,
    resume = resume,
    params = list(embed="maus")
  )
  
  internal_add_attributeDefinition(handle, orthoLevel, canoLabel, verbose = FALSE, rewriteAllAnnots = FALSE, insertLabels = FALSE)
  
  # if we previously decided to run automatic chunk segmentation
  if (running_chunker)
  {
    add_levelDefinition(handle, chunkLevel, "SEGMENT", verbose = FALSE, rewriteAllAnnots = FALSE)
    add_linkDefinition(handle, "ONE_TO_MANY", transcriptionLevel, chunkLevel)
    add_linkDefinition(handle, "ONE_TO_MANY", chunkLevel, orthoLevel)
    
    bas_run_chunker_dbi(
      handle = handle,
      canoLabel = canoLabel,
      canoLevel = canoLevel,
      chunkLabel = chunkLabel,
      chunkLevel = chunkLevel,
      orthoLabel = orthoLabel,
      topLevel = transcriptionLevel,
      params = list(force="rescue"),
      resume = resume,
      verbose = verbose,
      languages = languages,
      oldBasePath = oldBasePath
    )
  }

  
  add_levelDefinition(handle, mausLevel, "SEGMENT", verbose = FALSE, rewriteAllAnnots = FALSE)
  bas_new_canvas(handle, "default", mausLevel)
  add_linkDefinition(handle, "ONE_TO_MANY", orthoLevel, mausLevel)
  
  bas_run_maus_dbi(
    handle = handle,
    canoLevel = orthoLevel,
    canoLabel = canoLabel,
    languages = languages,
    chunkLabel = chunkLabel,
    mausLabel = mausLabel,
    mausLevel = mausLevel,
    verbose = verbose,
    resume = resume,
    params = list(),
    oldBasePath = oldBasePath
  )
  
  add_levelDefinition(handle, minniLevel, "SEGMENT", verbose = FALSE, rewriteAllAnnots = FALSE)
  bas_new_canvas(handle, "default", minniLevel)
  add_linkDefinition(handle, "ONE_TO_MANY", transcriptionLevel, minniLevel)
  
  bas_run_minni_dbi(
    handle = handle,
    languages = languages,
    minniLabel = minniLabel,
    minniLevel = minniLevel,
    topLevel = transcriptionLevel,
    verbose = verbose,
    resume = resume,
    params = list(),
    oldBasePath = oldBasePath
  )
  
  bas_run_pho2syl_canonical_dbi(
    handle = handle,
    canoLabel = canoLabel,
    canoSylLabel = canoSylLabel,
    canoLevel = orthoLevel,
    languages = languages,
    verbose = verbose,
    params = list(),
    resume = resume
  )
  
  internal_add_attributeDefinition(handle, orthoLevel, canoSylLabel, verbose = FALSE, rewriteAllAnnots = FALSE, insertLabels = FALSE)
  
  
  add_levelDefinition(handle, sylLevel, "SEGMENT", verbose = FALSE, rewriteAllAnnots = FALSE)
  add_linkDefinition(handle, "MANY_TO_MANY", orthoLevel, sylLevel)
  
  bas_run_pho2syl_segmental_dbi(
    handle = handle,
    mausLabel = mausLabel,
    mausLevel = mausLevel,
    languages = languages,
    sylLabel = sylLabel,
    sylLevel = sylLevel,
    wordLabel = orthoLabel,
    resume = resume,
    params = list(),
    verbose = verbose
  )
  
  add_linkDefinition(handle, "ONE_TO_MANY", sylLevel, mausLevel)
  
  autobuild_linkFromTimes(
    handle,
    sylLevel,
    mausLevel,
    rewriteAllAnnots = FALSE,
    convertSuperlevel = TRUE
  )
  
  remove_levelDefinition(handle, paste0(sylLevel, formals(autobuild_linkFromTimes)$backupLevelAppendStr), 
                         force = T, 
                         verbose = F)
  
  if(running_chunker)
  {
    bas_segment_to_item_level(handle, chunkLevel)
  }
  
  handle = bas_clear(handle, oldBasePath)
  
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
##' @param chunkLabel if you have a chunk segmentation level, you can provide one of its labels to improve the speed and accuracy
##' of MAUS. The chunk segmentation level must be a segment level, and it must link to the level of orthoLabel.
##' @param turnChunkLevelIntoItemLevel if TRUE, and if a chunk level is provided, the chunk level is converted into an ITEM level after segmentation
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
                                  
                                  chunkLabel = NULL,
                                  turnChunkLevelIntoItemLevel = TRUE,
                                  
                                  params = NULL,
                                  
                                  perspective = "default",
                                  resume = FALSE,
                                  verbose = TRUE)
{
  mausLevel = mausLabel
  
  bas_check_this_is_a_new_label(handle, mausLabel)
  
  canoLevel = get_levelNameForAttributeName(handle, canoLabel)
  if (is.null(canoLevel)) {
    stop("Could not find a level for label", canoLabel)
  }
  
  if (is.null(chunkLabel) &&
      get_levelDefinition(handle, canoLevel)$type == "SEGMENT") {
    chunkLabel = canoLabel
  }
  
  if (!is.null(chunkLabel))
  {
    chunkLevel = get_levelNameForAttributeName(handle, chunkLabel)
    if (is.null(chunkLevel))
    {
      stop("Could not find level for label ", chunkLabel)
    }
    
    if (get_levelDefinition(handle, chunkLevel)$type != "SEGMENT")
    {
      stop("Chunk level ", chunkLevel, " must be a segment level")
    }
  }
  
  languages = bas_evaluate_language_option(handle = handle, language = language)
  
  oldBasePath = handle$basePath
  handle = bas_prepare(handle, resume, verbose)
  
  add_levelDefinition(handle, mausLevel, "SEGMENT", verbose = FALSE, rewriteAllAnnots = FALSE)
  bas_new_canvas(handle, perspective, mausLevel)
  add_linkDefinition(handle, "ONE_TO_MANY", canoLevel, mausLevel)
  
  bas_run_maus_dbi(
    handle = handle,
    canoLabel = canoLabel,
    canoLevel = canoLevel,
    mausLabel = mausLabel,
    mausLevel = mausLevel,
    languages = languages,
    chunkLabel = chunkLabel,
    verbose = verbose,
    resume = resume,
    params = params,
    oldBasePath = oldBasePath
  )
  
  if(turnChunkLevelIntoItemLevel && !is.null(chunkLabel)) {
    bas_segment_to_item_level(handle, chunkLevel)
  }
  
  
  
  handle = bas_clear(handle, oldBasePath)
  
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
                                                
                                                resume = FALSE,
                                                verbose = TRUE)
{
  orthoLevel = orthoLabel
  
  bas_check_this_is_a_new_label(handle, orthoLabel)
  
  transcriptionLevel = get_levelNameForAttributeName(handle, transcriptionLabel)
  if (is.null(transcriptionLevel)) {
    stop("Could not find a level for label ", transcriptionLabel)
  }
  
  languages = bas_evaluate_language_option(handle = handle, language = language)
  
  oldBasePath = handle$basePath
  handle = bas_prepare(handle, resume, verbose)
  
  add_levelDefinition(handle, orthoLevel, "ITEM", verbose = FALSE, rewriteAllAnnots = FALSE)
  add_linkDefinition(handle, "ONE_TO_MANY", transcriptionLevel, orthoLevel)
  
  bas_run_g2p_for_tokenization_dbi(
    handle = handle,
    transcriptionLabel = transcriptionLabel,
    orthoLabel = orthoLabel,
    orthoLevel = orthoLevel,
    languages = languages,
    verbose = verbose,
    resume = resume,
    params = params
  )

  
  handle = bas_clear(handle, oldBasePath)
  
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

                                                 resume = FALSE,
                                                 verbose = TRUE)
{
  orthoLevel = get_levelNameForAttributeName(handle, orthoLabel)
  if (is.null(orthoLevel)) {
    stop("Could not find a level for label ", orthoLabel)
  }
  
  bas_check_this_is_a_new_label(handle, canoLabel)
  
  languages = bas_evaluate_language_option(handle = handle, language = language)
  
  oldBasePath = handle$basePath
  handle = bas_prepare(handle, resume, verbose)
  
  bas_run_g2p_for_pronunciation_dbi(
    handle = handle,
    orthoLabel = orthoLabel,
    orthoLevel = orthoLevel,
    languages = languages,
    canoLabel = canoLabel,
    verbose = verbose,
    resume = resume,
    params = params
  )
  
  internal_add_attributeDefinition(handle, orthoLevel, canoLabel, verbose = FALSE, rewriteAllAnnots = FALSE, insertLabels = FALSE)
  

  handle = bas_clear(handle, oldBasePath)
  
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
##'
##' @inheritParams runBASwebservice_all
##' @inheritParams runBASwebservice_maus

runBASwebservice_chunker <- function(handle,
                                     canoLabel,
                                     language,
                                     
                                     chunkLabel = "TRN",
                                     rootLevel = NULL,
                                     orthoLabel = NULL,
                                     
                                     params = list(),
                                     
                                     perspective = "default",
                                     resume = FALSE,
                                     verbose = TRUE)
{
  chunkLevel = chunkLabel
  bas_check_this_is_a_new_label(handle, chunkLabel)
  
  canoLevel = get_levelNameForAttributeName(handle, canoLabel)
  if (is.null(canoLevel)) {
    stop("Could not find a level for label ", canoLabel)
  }
  
  if ((!is.null(rootLevel)) &&
      is.null(get_levelDefinition(handle, rootLevel)))
  {
    stop("Root level ", rootLevel, " does not exist")
  }
  
  languages = bas_evaluate_language_option(handle = handle, language = language)
  
  oldBasePath = handle$basePath
  handle = bas_prepare(handle, resume, verbose)
  
  add_levelDefinition(handle, chunkLevel, "SEGMENT", verbose = FALSE, rewriteAllAnnots = FALSE)
  bas_new_canvas(handle, perspective, chunkLevel)
  add_linkDefinition(handle, "ONE_TO_MANY", chunkLevel, canoLevel)
  
  bas_run_chunker_dbi(
    handle = handle,
    canoLabel = canoLabel,
    canoLevel = canoLevel,
    chunkLabel = chunkLabel,
    languages = languages,
    verbose = verbose,
    chunkLevel = chunkLevel,
    topLevel = rootLevel,
    orthoLabel = orthoLabel,
    resume = resume,
    params = params,
    oldBasePath = oldBasePath
  )
  
  
  
  if (!is.null(rootLevel)) {
    add_linkDefinition(handle, "ONE_TO_MANY", rootLevel, chunkLevel)
  }
  
  
  handle = bas_clear(handle, oldBasePath)
  
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
                                   
                                   params = list(),
                                   
                                   perspective = "default",
                                   resume = FALSE,
                                   verbose = TRUE)
{
  minniLevel = minniLabel
  bas_check_this_is_a_new_label(handle, minniLabel)
  
  if ((!is.null(rootLevel)) &&
      is.null(get_levelDefinition(handle, rootLevel)))
  {
    stop("Root level ", rootLevel, " does not exist")
  }
  
  languages = bas_evaluate_language_option(handle = handle, language = language)
  
  oldBasePath = handle$basePath
  handle = bas_prepare(handle, resume, verbose)
  
  add_levelDefinition(handle, minniLevel, "SEGMENT", verbose = FALSE, rewriteAllAnnots = FALSE)
  if (!is.null(rootLevel)) {
    add_linkDefinition(handle, "ONE_TO_MANY", rootLevel, minniLevel)
  }
  bas_new_canvas(handle, perspective, minniLevel)
  
  bas_run_minni_dbi(
    handle = handle,
    languages = languages,
    minniLabel = minniLabel,
    minniLevel = minniLevel,
    verbose = verbose,
    topLevel = rootLevel,
    resume = resume,
    params = params,
    oldBasePath = oldBasePath
  )

  
  
  
  handle = bas_clear(handle, oldBasePath)
  
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
                                              
                                              resume = FALSE,
                                              verbose = TRUE)
{
  canoLevel = get_levelNameForAttributeName(handle, canoLabel)
  if (is.null(canoLevel)) {
    stop("Could not find a level for label ", canoLabel)
  }
  
  bas_check_this_is_a_new_label(handle, canoSylLabel)
  
  languages = bas_evaluate_language_option(handle = handle, language = language)

  oldBasePath = handle$basePath
  handle = bas_prepare(handle, resume, verbose)
  
  bas_run_pho2syl_canonical_dbi(
    handle = handle,
    canoLabel = canoLabel,
    canoLevel = canoLevel,
    languages = languages,
    verbose = verbose,
    canoSylLabel = canoSylLabel,
    resume = resume,
    params = params
  )
  
  internal_add_attributeDefinition(handle, canoLevel, canoSylLabel, verbose = FALSE, rewriteAllAnnots = FALSE, insertLabels = FALSE)
  
  handle = bas_clear(handle, oldBasePath)
  
  rewrite_allAnnots(handle, verbose = verbose)
}


##' Creates a syllable segmentation on the basis of a phonetic segmentation.
##' @family BAS webservice functions
##' @export
##' @param segmentLabel name of the label (not level!) containing a phonetic segmentation.
##' @param wordLabel name of a word label. This label's level must be a parent of the segmentation level.
##'
##' @inheritParams runBASwebservice_all
##' @inheritParams runBASwebservice_maus

runBASwebservice_pho2sylSegmental <- function(handle,
                                              segmentLabel,
                                              wordLabel,
                                              language,
                                              
                                              sylLabel = "MAS",
                                              
                                              params = list(),
                                              
                                              perspective = "default",
                                              resume = FALSE,
                                              verbose = TRUE)
{
  sylLevel = sylLabel
  
  bas_check_this_is_a_new_label(handle, sylLabel)
  
  segmentLevel = get_levelNameForAttributeName(handle, segmentLabel)
  if (is.null(segmentLevel)) {
    stop("Could not find a level for label ", segmentLevel)
  }
  
  if (get_levelDefinition(handle, segmentLevel)$type != "SEGMENT") {
    stop(segmentLevel,
         " must be a segment tier in order to run pho2syl from segment")
  }
  
  wordLevel = get_levelNameForAttributeName(handle, wordLabel)
  if (is.null(wordLevel)) {
    stop("Could not find a level for label ", wordLabel)
  }
  
  languages = bas_evaluate_language_option(handle = handle, language = language)
  
  oldBasePath = handle$basePath
  handle = bas_prepare(handle, resume, verbose)
  
  bas_run_pho2syl_segmental_dbi(
    handle = handle,
    mausLabel = segmentLabel,
    mausLevel = segmentLevel,
    languages = languages,
    verbose = verbose,
    sylLabel = sylLabel,
    sylLevel = sylLevel,
    wordLabel = wordLabel,
    resume = resume,
    params = params
  )
  
  add_levelDefinition(handle, sylLevel, "SEGMENT", verbose = FALSE, rewriteAllAnnots = FALSE)
  
  add_linkDefinition(handle, "MANY_TO_MANY", wordLevel, sylLevel)
  add_linkDefinition(handle, "ONE_TO_MANY", sylLevel, segmentLevel)
  autobuild_linkFromTimes(handle,
                          sylLevel,
                          segmentLevel,
                          convertSuperlevel = TRUE,
                          verbose = verbose)
  
  remove_levelDefinition(handle, paste0(sylLevel, formals(autobuild_linkFromTimes)$backupLevelAppendStr), 
                         force = T, 
                         verbose = F)
  
  handle = bas_clear(handle, oldBasePath)
  
  rewrite_allAnnots(handle, verbose = verbose)
}