##' Runs several BAS webservices with default options, starting from an orthographic transcription
##'
##' This function calls the BAS webservices G2P, MAUS, Pho2Syl, MINNI and (if necessary) Chunker.
##' Starting from an orthographic transcription, it derives a tokenized orthographical word tier
##' using the G2P tool. It also derives canonical pronunciations (in SAMPA) for the words.
##' If at least one audio file is longer than 60 seconds, the function then calls the Chunker webservice
##' to presegment the recordings. Subsequently, the webservice MAUS is called to derive a phonetic
##' segmentation. A second, rough segmentation is created by running the phoneme decoder MINNI.
##' Finally, syllabification is performed by calling Pho2Syl. \strong{This function requires an internet connection.}
##'
##' All necessary level, attribute and link definitions are created in the process.
##' Note that this function will run all BAS webservices with default parameters, with three exceptions: 
##' \describe{
##' \item{"Chunker"}{force=rescue}
##' \item{"G2P"}{embed=maus}
##' \item{"Pho2Syl"}{wsync=no}
##' }
##' If you wish to change parameters, you must use the individual runBASwebservices functions. This will also allow 
##' you to carry out manual corrections in between the steps, or to use different languages for different webservices.
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
  add_linkDefinition(handle, "ONE_TO_MANY", transcriptionLevel, orthoLevel)
  
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
      canoLevel = orthoLevel,
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
    
    remove_linkDefinition(handle, superlevelName = transcriptionLevel, sublevelName = orthoLevel, force = T, verbose = F)
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
  add_linkDefinition(handle, "ONE_TO_MANY", orthoLevel, sylLevel)
  
  bas_run_pho2syl_segmental_dbi(
    handle = handle,
    mausLabel = mausLabel,
    mausLevel = mausLevel,
    languages = languages,
    sylLabel = sylLabel,
    sylLevel = sylLevel,
    superLabel = orthoLabel,
    resume = resume,
    params = list(wsync="yes"),
    verbose = verbose,
    allowmultilink = F
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
  
  remove_linkDefinition(handle, orthoLevel, mausLevel, force = T, verbose = F)
  
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
##' by the function \link{runBASwebservice_g2pForPronunciation}. 
##' \strong{This function requires an internet connection.}
##' 
##' All necessary level, link and attribute definitions are created in the process. 
##'
##' @family BAS webservice functions
##'
##' @export
##'
##' @param canoLabel name of the label (not level!) containing the SAMPA word pronunciations.
##' If this label resides on a segment level, the segment time information is used as a presegmentation.
##' If it is an item level, no assumption is made about the temporal position of segments.
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
##' This function calls the webservice G2P to break up a transcription into tokens, or words.
##' In addition to tokenization, G2P performs normalization of numbers and other special words.
##' All necessary level, link and attribute definitions are created in the process.
##' A call to this function is usually followed by a call to \link{runBASwebservice_g2pForPronunciation}.
##' \strong{This function requires an internet connection.}
##' 
##' @family BAS webservice functions
##'
##' @export
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

##' Creates canonical pronunciation labels for a tier of tokenized orthographical words.
##' 
##' This function calls the G2P webservice to add canonical pronunciation labels in SAMPA (default)
##' or IPA to a tier of tokenized orthographical words. It is usually called after tokenization
##' with \link{runBASwebservice_g2pForTokenization}. Its output can be used as input to
##' \link{runBASwebservice_maus} or \link{runBASwebservice_chunker}. 
##' \strong{This function requires an internet connection.}
##' 
##' By default, G2P is called in MAUS embed mode. This is important if you intend to use MAUS
##' afterwards. To disable MAUS embed mode, call this function with params=list(embed="no").
##' To derive IPA symbols, add outsym="ipa" to the parameter list.
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
                                                 
                                                 params = list(embed="maus"),

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
##' 
##' When audio input files are longer than approximately 10 minutes, alignment-based segmentation
##' tools such as MAUS will take a long time to run. In these cases, the Chunker pre-segments
##' the input into more digestable "chunks". As input, it requires a word tier with canonical
##' pronunciation labels (which can be derived by \link{runBASwebservice_g2pForPronunciation}).
##' The resulting chunk level can be passed as input to \link{runBASwebservice_maus}.
##' \strong{This function requires an internet connection.}
##' 
##' Please note that the chunker output is \strong{not} a semantically meaningful sentence 
##' or turn segmentation, meaning that it cannot be used for analyses of sentence durations and the like.
##' By default, the chunker is called in force rescue mode. This means that the chunker is first run
##' in its normal mode, and switches to forced chunking mode only when it fails to find chunks that 
##' are short enough for processing by MAUS. To disable the force mode completely, call this function with
##' params=list(force="false"). To skip the normal chunking mode and go directly into forced chunking
##' mode, use params=list(force="true").
##' 
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
                                     
                                     params = list(force="rescue"),
                                     
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
##' 
##' The MINNI phoneme decoder performs phoneme-based decoding on the signal without input from
##' the transcription. Therefore, labelling quality is usually worse than that obtained from
##' MAUS (\link{runBASwebservice_maus}). Contrary to MAUS however, there is no need for a pre-
##' existing transcription.
##' 
##' All necessary level, link and attribute definitions are created in the process.
##' 
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
##' This function calls the webservice Pho2Syl to add a syllabified canonical pronunciation label
##' to a word level that already contains an unsyllabified canonical pronunciation label (as can be
##' derived using \link{runBASwebservice_g2pForPronunciation}). \strong{This function requires an internet
##' connection.}
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
##' 
##' This function calls the BAS webservice Pho2Syl to create a syllable segmentation on the basis
##' of a phonetic segmentation (created by, for example, \link{runBASwebservice_maus}.
##' 
##' 
##' 
##' All necessary level, link and parameter definitions are created in the process.
##' By default, Pho2Syl is called in word synchronized mode. To override this, call this function
##' with the parameter params=list(wsync="no").
##' 
##' @family BAS webservice functions
##' @export
##' @param segmentLabel name of the label (not level!) containing a phonetic segmentation.
##' @param superLabel name of a label on the segments' parent level (typically words).
##' If set to NULL, the syllable level cannot be linked up.
##'
##' @inheritParams runBASwebservice_all
##' @inheritParams runBASwebservice_maus

runBASwebservice_pho2sylSegmental <- function(handle,
                                              segmentLabel,
                                              language,
                                              
                                              superLabel = NULL,
                                              
                                              sylLabel = "MAS",
                                              
                                              params = list(wsync="yes"),
                                              
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
  
  if(!is.null(superLabel))
  {
    superLevel = get_levelNameForAttributeName(handle, superLabel)
    if (is.null(superLevel)) {
      stop("Could not find a level for label ", superLabel)
    }
  }
  
  multilink = F
  if("wsync" %in% names(params) && params$wsync == "no")
  {
    multilink = T
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
    superLabel = superLabel,
    resume = resume,
    params = params,
    allowmultilink = multilink
  )
  
  add_levelDefinition(handle, sylLevel, "SEGMENT", verbose = FALSE, rewriteAllAnnots = FALSE)
  
  if(!is.null(superLabel))
  {
    if(multilink)
    {
      add_linkDefinition(handle, "MANY_TO_MANY", superLevel, sylLevel)
    }
    else
    {
      add_linkDefinition(handle, "ONE_TO_MANY", superLevel, sylLevel)
    }
  }
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