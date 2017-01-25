#####################################################################
############################## ALL ##################################
#####################################################################

##' Runs a number of BAS webservices in default mode, starting from an orthographic transcription.
##'
##' This function calls the BAS webservices G2P, MAUS, Pho2Syl, MINNI and (if necessary) Chunker.
##' Starting from an orthographic transcription, it derives a tokenized orthographical word tier
##' using the G2P tool. It also derives canonical pronunciations (in SAMPA) for the words.
##' If the audio file is longer than 60 seconds, the function then calls the Chunker webservice
##' to presegment the recording. Subsequently, the webservice MAUS is called to derive a phonetic
##' segmentation. A second, rough segmentation is created by running the phoneme decoder MINNI.
##' Finally, syllabification is performed by calling Pho2Syl. All necessary level, attribute and link
##' definitions are created in the process. This function requires an internet connection.
##'
##' Note that this function will run all BAS webservices in default mode. If you wish to change individual
##' options, you may use the individual runBASwebservices_* functions. This will also allow you to carry
##' out manual corrections in between the individual steps.
##'
##' @export
##' @param handle emuDB handle
##' @param transcriptionLabel name of the label (not level!) containing an orthographic transcription.
##' @param language language to be used. Must be accepted by all webservices. If you want to use different
##' languages for different services, you will need to run the webservices separately. In this case,
##' it is your responsibility to ensure that SAMPA inventories are compatible between webservices.
##' Up-to-date lists of the languages accepted by all webservices can be found here:
##' https://clarin.phonetik.uni-muenchen.de/BASWebServices/services/help
##'
##' @param mausLabel label name for the MAUS segmentation. Default: MAU
##' @param orthoLabel label name for orthographic words. Default: ORT
##' @param canoLabel label name for canonical pronunciations of words. Default: KAN
##' @param sylLabel label name for syllable segmentation. Default: MAS
##' @param canoSylLabel label name for syllabified canonical pronunciations of words. Default: KAS
##' @param minniLabel label name for the MINNI segmentation. Default: MINNI
##' @param chunkLabel label name for the chunk segmentation. Default: TRN.
##' Please note that the chunk segmentation will only be generated if your emuDB contains
##' audio files beyond the one minute mark.
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
                                 
                                 verbose = TRUE,
                                 resume = FALSE)
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
  
  if (get_levelDefinition(handle, transcriptionLevel)$type == "SEGMENT") {
    chunkLevel = transcriptionLevel
  }
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
    g2pParams = list()
  )
  
  bas_run_g2p_from_ortho_dbi(
    handle = handle,
    orthoLevel = orthoLevel,
    orthoLabel = orthoLabel,
    canoLabel = canoLabel,
    language = language,
    verbose = verbose,
    resume = resume,
    g2pParams = list()
  )
  
  if (running_chunker)
  {
    bas_run_chunker_from_cano_dbi(
      handle = handle,
      canoLabel = canoLabel,
      canoLevel = canoLevel,
      chunkLabel = chunkLabel,
      chunkLevel = chunkLevel,
      topLevel = transcriptionLevel,
      chunkerParams = list(),
      resume = resume,
      verbose = verbose,
      language = language
    )
  }
  
  bas_run_maus_from_cano_dbi(
    handle = handle,
    canoLevel = orthoLevel,
    canoLabel = canoLabel,
    language = language,
    chunkLevel = chunkLevel,
    mausLabel = mausLabel,
    mausLevel = mausLevel,
    verbose = verbose,
    resume = resume,
    mausParams = list()
  )
  
  bas_run_minni_dbi(
    handle = handle,
    language = language,
    minniLabel = minniLabel,
    minniLevel = minniLevel,
    topLevel = transcriptionLevel,
    verbose = verbose,
    resume = resume,
    minniParams = list()
  )
  
  bas_run_pho2syl_from_cano_dbi(
    handle = handle,
    canoLabel = canoLabel,
    canoSylLabel = canoSylLabel,
    canoLevel = orthoLevel,
    language = language,
    verbose = verbose,
    pho2sylParams = list(),
    resume = resume
  )
  
  bas_run_pho2syl_from_mau_dbi(
    handle = handle,
    mausLabel = mausLabel,
    mausLevel = mausLevel,
    language = language,
    sylLabel = sylLabel,
    sylLevel = sylLevel,
    canoLevel = orthoLevel,
    resume = resume,
    pho2sylParams = list(),
    verbose = verbose
  )
  
  bas_clear(handle)
  
  add_levelDefinition(handle, orthoLevel, "ITEM", verbose = verbose)
  
  if (running_chunker)
  {
    add_levelDefinition(handle, chunkLevel, "SEGMENT", verbose = verbose)
    bas_new_canvas(handle, "default", chunkLevel)
  }
  
  add_levelDefinition(handle, sylLevel, "SEGMENT", verbose = verbose)
  
  add_levelDefinition(handle, mausLevel, "SEGMENT", verbose = verbose)
  bas_new_canvas(handle, "default", mausLevel)
  
  add_levelDefinition(handle, minniLevel, "SEGMENT", verbose = verbose)
  bas_new_canvas(handle, "default", minniLevel)
  
  add_attributeDefinition(handle, orthoLevel, canoLabel, verbose = verbose)
  add_attributeDefinition(handle, orthoLevel, canoSylLabel, verbose = verbose)
  
  if (running_chunker)
  {
    add_linkDefinition(handle, "ONE_TO_MANY", transcriptionLevel, chunkLevel)
    add_linkDefinition(handle, "ONE_TO_MANY", chunkLevel, orthoLevel)
  }
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

#####################################################################
############################# MAUS ##################################
#####################################################################
# 
# ##' Creates a phonetic segmentation on the basis of an orthographic transcription using the webservices G2P and MAUS.
# ##'
# ##' This function calls the BAS webservices G2P to tokenize a pre-existing orthographic transcription and
# ##' to derive a canonical pronunciation from it. On the basis of this canonical pronunciation, it then calls
# ##' the BAS webservice MAUS to generate a phonemic segmentation. All necessary level, attribute and link definitions
# ##' are created in the process. This function requires an internet connection.
# ##'
# ##' @export
# ##' @param handle emuDB handle
# ##' @param transcriptionLabel name of the label (not level!) containing an orthographic transcription.
# ##' If this label resides on a segment level, the segment time information is used as a pre-segmentation.
# ##' If it is an item level, no assumption is made about the temporal positions of segments.
# ##' @param language language to be used. Must be accepted by both the G2P and the MAUS webservices.
# ##' If you want to use different language options for G2P and MAUS, please run the services separately
# ##' by first calling bas_run_g2p_from_transcription, and then bas_run_maus_from_cano. If you do this, you
# ##' must ensure that the label inventories of both languages are compatible.
# ##' @param mausLabel label that will be given to the MAUS segmentation. Default: MAU
# ##' @param orthoLabel label that will be given to the orthographic words. Default: ORT
# ##' @param canoLabel label that will be given to the canonical form of words. Default: KAN
# ##' @param orthoLevel name of the word level that will be created. Defaults to the value of orthoLabel.
# ##' @param mausLevel name of the level for the MAUS segmentation. Defaults to the value of mausLabel.
# ##' @param g2pParams list of parameters to be passed on to G2P. It is your responsibility to ensure that
# ##' these parameters are compatible with the webservice API. If they are not, you will likely get an error.
# ##' Some parameters (such as the output format) cannot be set when calling G2P from emuR, and will be internally overridden.
# ##' Example: g2pParameters = list(nrm="yes")
# ##' @param mausParams list of parameters to be passed on to MAUS It is your responsibility to ensure that
# ##' these parameters are compatible with the webservice API. If they are not, you will likely get an error.
# ##' Example: mausParameters = list(MODUS="align", RELAXMINDUR="true")
# ##' Some parameters (such as the output format) cannot be set when calling MAUS from emuR, and will be internally overridden.
# ##' @param perspective webApp perspective in which the MAUS segment tier will be displayed. Default: default.
# ##' Set to NULL if you do not want the tier in any perspective.
# ##' @param verbose Display progress bars and other information
# ##' @param resume If a previous call to run_maus_from_transcription has failed (and you think you have fixed the issue),
# ##' you can set resume to TRUE for the next call to start where the last call failed. This will only work if you have not
# ##' run any other emuR functions (such as the query function) in the meantime, as they are likely to delete your temporary data.
# 
# runBASwebservice_mausFromTranscription <- function(handle,
#                                                    transcriptionLabel,
#                                                    language,
#                                                    
#                                                    mausLabel = "MAU",
#                                                    orthoLabel = "ORT",
#                                                    canoLabel = "KAN",
#                                                    
#                                                    orthoLevel = NULL,
#                                                    mausLevel = NULL,
#                                                    
#                                                    g2pParams = list(),
#                                                    mausParams = list(),
#                                                    
#                                                    perspective = "default",
#                                                    verbose = TRUE,
#                                                    resume = FALSE)
# {
#   transcriptionLevel = get_levelNameForAttributeName(handle, transcriptionLabel)
#   if (is.null(transcriptionLevel)) {
#     stop("Could not find a level for label ", transcriptionLabel)
#   }
#   
#   if (!is.null(get_levelNameForAttributeName(handle, orthoLabel)))
#   {
#     stop("There is already a level with label ", orthoLabel)
#   }
#   
#   if (!is.null(get_levelNameForAttributeName(handle, canoLabel)))
#   {
#     stop("There is already a level with label ", canoLabel)
#   }
#   
#   if (!is.null(get_levelNameForAttributeName(handle, mausLabel)))
#   {
#     stop("There is already a level with label ", mausLabel)
#   }
#   
#   if (is.null(orthoLevel)) {
#     orthoLevel = orthoLabel
#   }
#   if (is.null(mausLevel)) {
#     mausLevel = mausLabel
#   }
#   
#   if (!is.null(get_levelDefinition(handle, orthoLevel))) {
#     stop("Level ", orthoLevel, " already exists!")
#   }
#   if (!is.null(get_levelDefinition(handle, mausLevel))) {
#     stop("Level ", mausLevel, " already exists!")
#   }
#   
#   chunkLevel = NULL
#   if (get_levelDefinition(handle, transcriptionLevel)$type == "SEGMENT")
#   {
#     chunkLevel = transcriptionLevel
#   }
#   
#   if (!resume)
#   {
#     bas_prepare(handle)
#   }
#   
#   bas_run_g2p_for_tokenization_dbi(
#     handle = handle,
#     transcriptionLabel = transcriptionLabel,
#     orthoLabel = orthoLabel,
#     orthoLevel = orthoLevel,
#     language = language,
#     verbose = verbose,
#     resume = resume,
#     g2pParams = g2pParams
#   )
#   
#   bas_run_g2p_from_ortho_dbi(
#     handle = handle,
#     orthoLevel = orthoLevel,
#     orthoLabel = orthoLabel,
#     canoLabel = canoLabel,
#     language = language,
#     verbose = verbose,
#     resume = resume,
#     g2pParams = g2pParams
#   )
#   
#   bas_run_maus_from_cano_dbi(
#     handle = handle,
#     canoLevel = orthoLevel,
#     canoLabel = canoLabel,
#     language = language,
#     chunkLevel = chunkLevel,
#     mausLabel = mausLabel,
#     mausLevel = mausLevel,
#     verbose = verbose,
#     resume = resume,
#     mausParams = mausParams
#   )
#   
#   bas_clear(handle)
#   
#   
#   add_levelDefinition(handle, orthoLevel, "ITEM", verbose = verbose)
#   add_levelDefinition(handle, mausLevel, "SEGMENT", verbose = verbose)
#   
#   if (orthoLevel != canoLabel) {
#     add_attributeDefinition(handle, orthoLevel, canoLabel, verbose = verbose)
#   }
#   if (orthoLevel != orthoLabel) {
#     add_attributeDefinition(handle, orthoLevel, orthoLabel, verbose = verbose)
#   }
#   if (mausLevel != mausLabel) {
#     add_attributeDefinition(handle, mausLevel, mausLabel, verbose = verbose)
#   }
#   add_linkDefinition(handle, "ONE_TO_MANY", transcriptionLevel, orthoLevel)
#   add_linkDefinition(handle, "ONE_TO_MANY", orthoLevel, mausLevel)
#   
#   bas_new_canvas(handle, perspective, mausLevel)
#   rewrite_allAnnots(handle, verbose = verbose)
# }
# 
# ##' Creates a phonetic segmentation on the basis of an tokenized orthographic word tier using the segmentation webservices G2P and MAUS.
# ##'
# ##' This function requires a pre-existing tier with tokenized orthographic words. It calls the BAS
# ##' webservice G2P to derive a canonical pronunciation from it. On the basis of this canonical pronunciation, it then calls
# ##' the BAS webservice MAUS to generate a phonemic segmentation. All necessary level, attribute and link definitions
# ##' are created in the process. This function requires an internet connection.
# ##'
# ##' @export
# ##' @param handle emuDB handle
# ##' @param orthoLabel name of the label (not level!) containing orthographic words.
# ##' If this label resides on a segment level, the segment time information is used as a presegmentation.
# ##' If it is an item level, no assumption is made about the temporal position of segments.
# ##' @param language language to be used. Must be accepted by both the G2P and the MAUS webservices.
# ##' If you want to use different language options for G2P and MAUS, please run the services separately
# ##' by first calling bas_run_g2p_from_ortho, and then bas_run_maus_from_cano. If you do this, you
# ##' must ensure that the label inventories of both languages are compatible.
# ##' @param mausLabel label that will be given to the MAUS segmentation. Default: MAU
# ##' @param canoLabel label that will be given to the canonical form of words. Default: KAN
# ##' @param mausLevel name of the level for the MAUS segmentation. Defaults to the value of mausLabel.
# ##' @param chunkLevel if you have a chunk segmentation level, you can provide it to improve the speed and accuracy
# ##' of MAUS. The chunk segmentation level must be a segment level, and it must link to the level where orthoLabel resides.
# ##' @param g2pParams list of parameters to be passed on to G2P. It is your responsibility to ensure that
# ##' these parameters are compatible with the webservice API. If they are not, you will likely get an error.
# ##' Some parameters (such as the output format) cannot be set when calling G2P from emuR, and will be internally overridden.
# ##' Example: g2pParameters = list(nrm="yes")
# ##' @param mausParams list of parameters to be passed on to MAUS It is your responsibility to ensure that
# ##' these parameters are compatible with the webservice API. If they are not, you will likely get an error.
# ##' Example: mausParameters = list(MODUS="align", RELAXMINDUR="true")
# ##' Some parameters (such as the output format) cannot be set when calling MAUS from emuR, and will be internally overridden.
# ##' @param perspective webApp perspective in which the MAUS segment tier will be displayed. Default: default.
# ##' Set to NULL if you do not want the tier in any perspective.
# ##' @param verbose Display progress bars and other information
# ##' @param resume If a previous call to run_maus_from_transcription has failed (and you think you have fixed the issue),
# ##' you can set resume to TRUE for the next call to start where the last call failed. This will only work if you have not
# ##' run any other emuR functions (such as the query function) in the meantime, as they are likely to delete your temporary data.
# 
# runBASwebservice_mausFromOrtho <- function(handle,
#                                            orthoLabel,
#                                            language,
#                                            
#                                            canoLabel = "KAN",
#                                            mausLabel = "MAU",
#                                            
#                                            mausLevel = NULL,
#                                            chunkLevel = NULL,
#                                            
#                                            mausParams = list(),
#                                            g2pParams = list(),
#                                            
#                                            verbose = TRUE,
#                                            perspective = "default",
#                                            resume = FALSE)
# {
#   if (!is.null(get_levelNameForAttributeName(handle, mausLabel)))
#   {
#     stop("There is already a level with label ", mausLabel)
#   }
#   
#   if (!is.null(get_levelNameForAttributeName(handle, canoLabel)))
#   {
#     stop("There is already a level with label ", canoLabel)
#   }
#   
#   if (is.null(mausLevel)) {
#     mausLevel = mausLabel
#   }
#   
#   if (!is.null(get_levelDefinition(handle, mausLevel))) {
#     stop("Level ", mausLevel, " already exists!")
#   }
#   
#   orthLevel = get_levelNameForAttributeName(handle, orthLabel)
#   if (is.null(orthLevel)) {
#     stop("Could not find a level for label", orthLabel)
#   }
#   
#   if (is.null(chunkLevel) &&
#       get_levelDefinition(handle, orthLevel)$type == "SEGMENT") {
#     chunkLevel = orthLevel
#   }
#   
#   if (!resume)
#   {
#     bas_prepare(handle)
#   }
#   
#   bas_run_g2p_from_ortho_dbi(
#     handle = handle,
#     orthLabel = orthLabel,
#     orthLevel = orthLevel,
#     language = language,
#     canoLabel = canoLabel,
#     verbose = verbose,
#     resume = resume,
#     g2pParams = g2pParams
#   )
#   
#   bas_run_maus_from_cano_dbi(
#     handle = handle,
#     canoLabel = canoLabel,
#     canoLevel = orthLevel,
#     language = language,
#     mausLabel = mausLabel,
#     chunkLevel = chunkLevel,
#     verbose = verbose,
#     resume = resume,
#     mausParams = mausParams
#   )
#   
#   bas_clear(handle)
#   
#   add_levelDefinition(handle, mausLevel, "SEGMENT", verbose = verbose)
#   if (mausLevel != mausLabel) {
#     add_attributeDefinition(handle, mausLevel, mausLabel, verbose = verbose)
#   }
#   add_linkDefinition(handle, "ONE_TO_MANY", orthLevel, mausLevel)
#   bas_new_canvas(handle, perspective, mausLevel)
#   rewrite_allAnnots(handle, verbose = verbose)
# }

##' Creates a phonetic segmentation on the basis of a tokenized word tier with canonical pronunciation in SAMPA, using the segmentation webservice MAUS.
##'
##' This function requires a pre-existing tier with words transcribed in SAMPA. It calls the
##' BAS webservice MAUS to generate a phonemic segmentation. All necessary level, attribute and link definitions
##' are created in the process. This function requires an internet connection.
##'
##' @export
##' @param handle emuDB handle
##' @param canoLabel name of the label (not level!) containing canonical pronunciations of words.
##' If this label resides on a segment level, the segment time information is used as a presegmentation.
##' If it is an item level, no assumption is made about the temporal position of segments.
##' @param language language to be used.
##' @param mausLabel label that will be given to the MAUS segmentation. Default: MAU
##' @param mausLevel name of the level for the MAUS segmentation. Defaults to the value of mausLabel.
##' @param chunkLevel if you have a chunk segmentation level, you can provide it to improve the speed and accuracy
##' of MAUS. The chunk segmentation level must be a segment level, and it must link to the level where orthoLabel resides.
##' @param mausParams list of parameters to be passed on to MAUS It is your responsibility to ensure that
##' these parameters are compatible with the webservice API. If they are not, you will likely get an error.
##' Example: mausParameters = list(MODUS="align", RELAXMINDUR="true")
##' Some parameters (such as the output format) cannot be set when calling MAUS from emuR, and will be internally overridden.
##' @param perspective webApp perspective in which the MAUS segment tier will be displayed. Default: default.
##' Set to NULL if you do not want the tier in any perspective.
##' @param verbose Display progress bars and other information
##' @param resume If a previous call to run_maus_from_transcription has failed (and you think you have fixed the issue),
##' you can set resume to TRUE for the next call to start where the last call failed. This will only work if you have not
##' run any other emuR functions (such as the query function) in the meantime, as they are likely to delete your temporary data.

runBASwebservice_mausFromCano <- function(handle,
                                          canoLabel,
                                          language,
                                          
                                          mausLabel = "MAU",
                                          
                                          chunkLevel = NULL,
                                          mausLevel = NULL,
                                          
                                          mausParams = NULL,
                                          
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
  
  if (!is.null(chunkLevel))
  {
    if (is.null(get_levelDefinition(handle, chunkLevel)))
    {
      stop("Could not find level ", chunkLevel)
    }
    if (get_levelDefinition(handle, chunkLevel)$type != "SEGMENT")
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
    resume = resume,
    mausParams = mausParams
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


##' Tokenizes an orthographic transcription.
##'
##' @export
##' @param handle emuDB handle
##' @param transcriptionLabel name of the label (not level!) containing an orthographic transcription.
##' @param language language to be used.
##' @param orthoLabel label that will be given to the orthographic words. Default: ORT
##' @param orthoLevel name of the word level that will be created. Defaults to the value of orthoLabel.
##' @param g2pParams list of parameters to be passed on to G2P. It is your responsibility to ensure that
##' these parameters are compatible with the webservice API. If they are not, you will likely get an error.
##' Some parameters (such as the output format) cannot be set when calling G2P from emuR, and will be internally overridden.
##' Example: g2pParameters = list(nrm="yes")
##' @param verbose Display progress bars and other information
##' @param resume If a previous call to run_maus_from_transcription has failed (and you think you have fixed the issue),
##' you can set resume to TRUE for the next call to start where the last call failed. This will only work if you have not
##' run any other emuR functions (such as the query function) in the meantime, as they are likely to delete your temporary data.

runBASwebservice_g2pForTokenization <- function(handle,
                                                transcriptionLabel,
                                                language,
                                                
                                                orthoLabel = "ORT",
                                                
                                                g2pParams = list(),
                                                
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
    g2pParams = g2pParams
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
##' @export
##' @param handle emuDB handle
##' @param orthoLabel name of the label (not level!) containing orthographic words.
##' If this label resides on a segment level, the segment time information is used as a presegmentation.
##' If it is an item level, no assumption is made about the temporal position of segments.
##' @param language language to be used.
##' @param canoLabel label that will be given to the canonical form of words. Default: KAN
##' @param g2pParams list of parameters to be passed on to G2P. It is your responsibility to ensure that
##' these parameters are compatible with the webservice API. If they are not, you will likely get an error.
##' Some parameters (such as the output format) cannot be set when calling G2P from emuR, and will be internally overridden.
##' Example: g2pParameters = list(nrm="yes")
##' Some parameters (such as the output format) cannot be set when calling MAUS from emuR, and will be internally overridden.
##' @param verbose Display progress bars and other information
##' @param resume If a previous call to run_maus_from_transcription has failed (and you think you have fixed the issue),
##' you can set resume to TRUE for the next call to start where the last call failed. This will only work if you have not
##' run any other emuR functions (such as the query function) in the meantime, as they are likely to delete your temporary data.

runBASwebservice_g2pFromOrtho <- function(handle,
                                          orthoLabel,
                                          language,
                                          
                                          canoLabel = "KAN",
                                          
                                          g2pParams = list(),
                                          
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
    resume = resume,
    g2pParams = g2pParams
  )
  
  bas_clear(handle)
  
  if (orthoLevel != canoLabel) {
    add_attributeDefinition(handle, orthoLevel, canoLabel, verbose = verbose)
  }
  rewrite_allAnnots(handle, verbose = verbose)
}

# ##' Creates a word tier with canonical pronunciation in SAMPA from an orthographic transcription.
# ##'
# ##' @export
# ##' @param handle emuDB handle
# ##' @param transcriptionLabel name of the label (not level!) containing an orthographic transcription.
# ##' If this label resides on a segment level, the segment time information is used as a chunk segmentation (presegmentation).
# ##' If it is an item level, no assumption is made about the temporal position of segments.
# ##' @param language language to be used.
# ##' @param orthoLabel label that will be given to the orthographic words. Default: ORT
# ##' @param canoLabel label that will be given to the canonical form of words. Default: KAN
# ##' @param orthoLevel name of the word level that will be created. Defaults to the value of orthoLabel.
# ##' @param g2pParams list of parameters to be passed on to G2P. It is your responsibility to ensure that
# ##' these parameters are compatible with the webservice API. If they are not, you will likely get an error.
# ##' Some parameters (such as the output format) cannot be set when calling G2P from emuR, and will be internally overridden.
# ##' Example: g2pParameters = list(nrm="yes")
# ##' @param verbose Display progress bars and other information
# ##' @param resume If a previous call to run_maus_from_transcription has failed (and you think you have fixed the issue),
# ##' you can set resume to TRUE for the next call to start where the last call failed. This will only work if you have not
# ##' run any other emuR functions (such as the query function) in the meantime, as they are likely to delete your temporary data.
# 
# runBASwebservice_g2pFromTranscription <- function(handle,
#                                                   transcriptionLabel,
#                                                   language,
#                                                   
#                                                   canoLabel = "KAN",
#                                                   orthoLabel = "ORT",
#                                                   
#                                                   g2pParams = list(),
#                                                   
#                                                   orthoLevel = NULL,
#                                                   verbose = TRUE,
#                                                   resume = FALSE)
# {
#   transcriptionLevel = get_levelNameForAttributeName(handle, transcriptionLabel)
#   if (is.null(transcriptionLevel)) {
#     stop("Could not find a level for label", transcriptionLabel)
#   }
#   
#   if (is.null(orthoLevel)) {
#     orthoLevel = orthoLabel
#   }
#   if (!is.null(get_levelDefinition(handle, orthoLevel))) {
#     stop("Level", orthoLevel, "already exists!")
#   }
#   
#   if (!resume)
#   {
#     bas_prepare(handle)
#   }
#   
#   bas_run_g2p_for_tokenization_dbi(
#     handle = handle,
#     transcriptionLabel = transcriptionLabel,
#     orthoLabel = orthoLabel,
#     orthoLevel = orthoLevel,
#     language = language,
#     verbose = verbose,
#     normalize = normalize,
#     resume = resume,
#     g2pParams = g2pParams
#   )
#   
#   bas_run_g2p_from_ortho_dbi(
#     handle = handle,
#     orthoLevel = orthoLevel,
#     orthoLabel = orthoLabel,
#     canoLabel = canoLabel,
#     language = language,
#     verbose = verbose,
#     resume = resume,
#     g2pParams = g2pParams
#   )
#   
#   bas_clear(handle)
#   
#   add_levelDefinition(handle, orthoLevel, "ITEM", verbose = verbose)
#   if (orthoLevel != orthoLabel) {
#     add_attributeDefinition(handle, orthoLevel, orthoLabel, verbose = verbose)
#   }
#   if (orthoLevel != canoLabel) {
#     add_attributeDefinition(handle, orthoLevel, canoLabel, verbose = verbose)
#   }
#   add_linkDefinition(handle, "ONE_TO_MANY", transcriptionLevel, orthoLevel)
#   rewrite_allAnnots(handle, verbose = verbose)
# }

#####################################################################
########################### CHUNKER #################################
#####################################################################

##' Creates a chunk segmentation using the webservice Chunker.
##'
##' @export
##' @param handle emuDB handle
##' @param canoLabel name of the label (not level!) containing a canonical pronunciation of the words.
##' @param language language to be used.
##' @param chunkLabel label that will be given to the chunk segmentation. Default: TRN
##' @param rootLevel level that will link to the newly created chunk level. Should be the root bundle level.
##' @param orthoLevel name of the chunk level that will be created. Defaults to the value of chunkLabel.
##' @param orthoLabel if there is an orthographic label paired with the canonical pronunciation label, you can provide it here
##' to get orthographic chunk labels. If NULL, you will end up with chunk labels in SAMPA.
##' @param chunkerParams list of parameters to be passed on to the chunker It is your responsibility to ensure that
##' these parameters are compatible with the webservice API. If they are not, you will likely get an error.
##' Example: g2pParameters = list(minChunkDuration = 10)
##' @param perspective webApp perspective in which the chunk segment tier will be displayed. Default: default.
##' Set to NULL if you do not want the tier in any perspective.
##' @param verbose Display progress bars and other information
##' @param resume If a previous call to this function has failed (and you think you have fixed the issue),
##' you can set resume to TRUE for the next call to start where the last call failed. This will only work if you have not
##' run any other emuR functions (such as the query function) in the meantime, as they are likely to delete your temporary data.

runBASwebservice_cunkerFromCano <- function(handle,
                                            canoLabel,
                                            language,
                                            
                                            chunkLabel = "TRN",
                                            rootLevel = NULL,
                                            
                                            chunkLevel = NULL,
                                            orthoLabel = NULL,
                                            
                                            chunkerParams = list(),
                                            
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
    resume = resume,
    chunkerParams = chunkerParams
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
##'
##' @export
##' @param handle emuDB handle
##' @param language language to be used.
##' @param minniLabel label that will be given to the MINNI segmentation. Default: MINNI
##' @param rootLevel level that will link to the newly created chunk level. Should be the root bundle level.
##' @param minniParams list of parameters to be passed on to MINNI It is your responsibility to ensure that
##' these parameters are compatible with the webservice API. If they are not, you will likely get an error.
##' Some parameters (such as the output format) cannot be set when calling MINNI from emuR, and will be internally overridden.
##' Example: minniParams = list()
##' @param perspective webApp perspective in which the MINNI segment tier will be displayed. Default: default.
##' Set to NULL if you do not want the tier in any perspective.
##' @param verbose Display progress bars and other information
##' @param resume If a previous call to this function has failed (and you think you have fixed the issue),
##' you can set resume to TRUE for the next call to retrieve any progress made up to that point. This will only work if you have not
##' run any other emuR functions (such as the query function) in the meantime, as they are likely to delete your temporary data.

runBASwebservice_minni <- function(handle,
                                   language,
                                   
                                   minniLabel = "MINNI",
                                   rootLevel = NULL,
                                   
                                   minniLevel = NULL,
                                   
                                   minniParams = list(),
                                   
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
    minniParams = minniParams
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
##' @export
##' @param handle emuDB handle
##' @param canoLabel name of the label (not level!) containing a canonical pronunciation of the words.
##' @param language language to be used.
##' @param canoSylLabel label that will be given to the new syllabification label. Default: KAS
##' @param pho2sylParams list of parameters to be passed on to pho2syl It is your responsibility to ensure that
##' these parameters are compatible with the webservice API. If they are not, you will likely get an error.
##' Some parameters (such as the output format) cannot be set when calling pho2syl from emuR, and will be internally overridden.
##' @param verbose Display progress bars and other information
##' @param resume If a previous call to this function has failed (and you think you have fixed the issue),
##' you can set resume to TRUE for the next call to retrieve any progress made up to that point. This will only work if you have not
##' run any other emuR functions (such as the query function) in the meantime, as they are likely to delete your temporary data.

runBASwebservice_pho2sylFromCano <- function(handle,
                                             canoLabel,
                                             language,
                                             canoSylLabel = "KAS",
                                             
                                             pho2sylParams = list(),
                                             
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
    resume = resume,
    pho2sylParams = pho2sylParams
  )
  
  bas_clear(handle)
  
  if (canoLevel != canoSylLabel) {
    add_attributeDefinition(handle, canoLevel, canoSylLabel, verbose = verbose)
  }
  rewrite_allAnnots(handle)
}


##' Creates a syllable segmentation on the basis of a phonetic segmentation.
##'
##' @export
##' @param handle emuDB handle
##' @param canoLabel name of the label (not level!) containing a phonetic segmentation.
##' @param wordLevel name of word level. Must be a parent lavel of the segmentation level.
##' @param language language to be used.
##' @param sylLabel label that will be given to the new syllabification labels. Default: MAS
##' @param sylLevel name of the new syllabification level. Defaults to the value of sylLabel.
##' @param pho2sylParams list of parameters to be passed on to pho2syl It is your responsibility to ensure that
##' these parameters are compatible with the webservice API. If they are not, you will likely get an error.
##' Some parameters (such as the output format) cannot be set when calling pho2syl from emuR, and will be internally overridden.
##' @param verbose Display progress bars and other information
##' @param perspective webApp perspective in which the syllable tier will be displayed. Default: default.
##' Set to NULL if you do not want the tier in any perspective.
##' @param resume If a previous call to this function has failed (and you think you have fixed the issue),
##' you can set resume to TRUE for the next call to retrieve any progress made up to that point. This will only work if you have not
##' run any other emuR functions (such as the query function) in the meantime, as they are likely to delete your temporary data.

runBASwebservice_pho2sylFromSegment <- function(handle,
                                                mausLabel,
                                                wordLevel,
                                                language,
                                                
                                                sylLabel = "MAS",
                                                sylLevel = NULL,
                                                
                                                pho2sylParams = list(),
                                                
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
  
  if (is.null(get_levelDefinition(handle, wordLevel))) {
    stop("Could not find a level for label ", wordLevel)
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
    canoLevel = wordLevel,
    resume = resume,
    pho2sylParams = pho2sylParams
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