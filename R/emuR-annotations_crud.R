##' Create new items programmatically
##' 
##' @description Create annotation items programmatically on a single level.
##' You have to pass in a data frame, called `itemsToCreate`, describing
##' the new items. The required columns depend on the type of the level (ITEM,
##' EVENT, or SEGMENT).
##' 
##' This function belongs to emuR’s CRUD family of functions, which let the user
##' manipulate items programmatically:
##' 
##' * Create items ([create_itemsInLevel])
##' * Read items ([query])
##' * Update items ([update_itemsInLevel])
##' * Delete items ([delete_itemsInLevel]))
##' 
##' @details
##' This function creates new annotation items on an existing level, in existing
##' bundles.
##' 
##' Regardless of the type of level you are creating items on, your input data
##' frame `itemsToCreate` must describe your new items by specifying the columns
##' `session`, `bundle`, `level`, `attribute` and `labels`. `level` must have the
##' same value for all rows, as we can only create items on one level at a time.
##' 
##' `attribute` must also have the same value for all rows, and it must be an
##' existing attribute that belongs to the `level`.
##' 
##' A major use case for this function is to obtain a segment list using [query],
##' modify the segment list and feed it to this function. That is why the column
##' `labels` has a plural name: segment lists also have a column `labels` and
##' not `label`. The same is true for the sequence index columns introduced below.
##' 
##' Creating new items works differently depending on the level type. The three
##' types are explained in the following sections.
##' 
##' ## Levels of type ITEM
##' 
##' In addition to the columns that are always required, ITEM-typed levels require
##' a column with a sequence index to be present in the `itemsToCreate` data
##' frame. Its name must be `start_item_seq_idx`. This name was chosen instead
##' of `sequence_index` because it is present as a column name in segment lists
##' obtained with [query]. That makes it easer to use a segment list as input to
##' [create_itemsInLevel()].
##' 
##' Along the time axis, there can be multiple annotation items on every level.
##' Their order within the level is given by their sequence index. All *existing*
##' items have a natural-valued sequence index and there are no gaps in the
##' sequences (i.e. if a level contains N annotation items, they are indexed 1..N).
##' 
##' Any newly created item must be given a sequence index. The sequence index may
##' be real-valued (it will automatically be replaced with a natural value). To
##' prepend the new item to the existing ones, pass a value lower than one. To
##' append it to the existing items, you can either pass `NA` or any value that
##' you know is greater than N (the number of existing items in that level). It
##' does not need to be exactly N+1. To place the new item between two existing
##' ones, use any real value between the sequence indexes of the existing neighbors.
##' 
##' If you are appending multiple items at the same time, every sequence index
##' (including `NA`) can only be used once per session/bundle/level combination
##' (because session/bundle/level/sequence index are the unique identifier of an
##' item).
##' 
##' After creating the items, all sequence indexes (which may now be real-valued,
##' natural-valued or NA) are sorted in ascending order and then replaced with
##' the values 1..N, where N is the number of items on that level. While sorting,
##' `NA` values are placed at the end.
##' 
##' ## Levels of type EVENT
##' 
##' In addition to the columns that are always required, EVENT-typed levels require
##' a column with the time of the event to be present in the `itemsToCreate` data
##' frame. Its name must be `start`. This name was chosen because it is present
##' as a column name in segment lists obtained with [query]. That makes it easer
##' to use a segment list as input to [create_itemsInLevel()]. The `end` column
##' in segment lists is 0 for EVENT-typed levels.
##' 
##' The `start` column must be given in milliseconds.
##' 
##' You cannot create an EVENT item at a point on the time axis where another
##' item already exists on the same level. If you specify such an event, the
##' entire function will fail.
##' 
##' ## Levels of type SEGMENT
##' 
##' You can only create SEGMENT-typed items in bundles where the respective level
##' is empty.
##' 
##' In addition to the columns that are always required, SEGMENT-typed levels
##' require the column `start` to be present in the `itemsToCreate` data frame,
##' representing the start time of the segment. It must be given in milliseconds.
##' 
##' Segments also need to have an end, and there are two strategies to determine
##' the end. Either, you explicitly provide an `end` column in the `itemsToCreate`
##' data frame. It must be given in milliseconds. If you do that, you have to
##' specify the `calculateEndTimeForSegments` parameter as `FALSE`.
##' 
##' Alternatively, you can leave `calculateEndTimeForSegments` at `TRUE` (which
##' is the default) and provide your `itemsToCreate` data frame without an `end`
##' column. In that case, the end time will be aligned to the next neighbor’s
##' start time. The end time of the last segment will be aligned with the end of
##' the annotated media file.
##' 
##'
##' @param emuDBhandle emuDB handle as returned by [load_emuDB]
##' @param itemsToCreate A data frame with the columns:
##' * `session` (character)
##' * `bundle` (character)
##' * `level` (character)
##' * `attribute` (character)
##' * `labels` (character)
##' * `start_item_seq_idx` (numeric; only when `level` refers to a ITEM-typed
##'    level)
##' * `start` (numeric, milliseconds; only when `level` refers to an EVENT-typed
##'    or SEGMENT-typed level)
##' * `end` (numeric, milliseconds; only when `level` refers to a SEGMENT-typed
##'    level and `calculateEndTimeForSegments` is `FALSE`)
##' @param calculateEndTimeForSegments *Only applicable if the level type is SEGMENT.*
##' If set to `TRUE`, then each segment’s end time is automatically aligned
##' with the start time of the following segment. In that case, user-provided
##' end times are ignored. The last segment’s end time is the end time of the
##' annotated media file. If set to `FALSE`, then the user has to provide
##' an end time for each segment.
##' @param allowGapsAndOverlaps *Only applicable if the level type is SEGMENT
##' and `calculateEndTimeForSegments` is `FALSE`.*
##' If set to `FALSE`, this function fails when `itemsToCreate` contains
##' gaps or overlaps between segments. The offending segments are returned invisibly.
##' You can inspect them by assigning the return value to a variable. The return
##' value will include a new column \code{gap_samples} that indicates the size
##' of the gap (positive values) or overlap (negative values) with the previous
##' segment, respectively. It is measured in audio samples, not in milliseconds.
##' Setting this to \code{TRUE} allows the function to complete even with gaps
##' and/or overlaps, but this is is **not recommended as it can cause bugs in
##' the EMU-webApp**.
##' @param rewriteAllAnnots should changes be written to file system (_annot.json
##' files) (intended for expert use only)
##' @param verbose if set to \code{TRUE}, more status messages are printed
##' 
##' @export
##' @importFrom rlang .data
##' @md
  create_itemsInLevel = function(emuDBhandle,
                               itemsToCreate,
                               calculateEndTimeForSegments = TRUE,
                               allowGapsAndOverlaps = FALSE,
                               rewriteAllAnnots = TRUE,
                               verbose = TRUE) {
  # check that only one level is provided
  levelName = unique(itemsToCreate$level)
  if(length(levelName) > 1 || length(levelName) < 1) {
    stop("'itemsToCreate' contains multiple levels or none at all! The created ITEMs have to be on the same level!")
  }
  
  # check that only one attribute is provided (currenlty only single attributes allowed)
  attributeName = unique(itemsToCreate$attribute)
  if(length(levelName) > 1){
    stop("'itemsToCreate' contains multiple attributes! The created ITEMs have to be on the same attribute!")
  }
  # @FIXME make sure the provided attribute belongs to the provided level
  
  ## check the level exists and has a known type (other types can only exist when the Emu system is changed fundamentally)
  levelDefinition = get_levelDefinition(emuDBhandle, levelName)
  if(is.null(levelDefinition)) stop("level '", levelName, "' doesn't exist!")
  if (!(levelDefinition$type %in% c("ITEM", "EVENT", "SEGMENT"))) {
    stop(paste0("The level:", levelName, " provided is not of type ITEM, EVENT or SEGMENT"))
  }
  
  ## check that all required columns are available
  required_colnames = c("session", "bundle", "level", "attribute", "labels")
  if(levelDefinition$type == "ITEM"){
    required_colnames = c(required_colnames, "start_item_seq_idx")
  }else if(levelDefinition$type == "EVENT"){
    required_colnames = c(required_colnames, "start")
  }else if(levelDefinition$type == "SEGMENT"){
    if (calculateEndTimeForSegments) {
      required_colnames = c(required_colnames, "start")
      if ("end" %in% names(itemsToCreate)) {
        stop("itemsToCreate contains an 'end' column, but calculateEndTimeForSegments is TRUE; please decide how end times should be determined.")
      }
    } else {
      required_colnames = c(required_colnames, "start", "end")
    }
  }
  
  if(!all(required_colnames %in% names(itemsToCreate))){
    stop(paste0("Not all required columns are available in itemsToCreate data.frame! ",
                "The required columns are: ", paste(required_colnames, collapse = "; ")))
  }
  
  ## check types of columns
  if(!is.character(itemsToCreate$session) | 
     !is.character(itemsToCreate$bundle) | 
     !is.character(itemsToCreate$level) |
     !is.character(itemsToCreate$attribute) |
     !is.character(itemsToCreate$labels)
  ){
    stop(paste0("Not all columns match the required type!"))
  }
  if(levelDefinition$type == "ITEM"){
    if(!is.numeric(itemsToCreate$start_item_seq_idx)) stop(paste0("Not all columns match the required type!"))
  }else if(levelDefinition$type == "EVENT"){
    if(!is.numeric(itemsToCreate$start)) stop(paste0("Not all columns match the required type!"))
  }else if(levelDefinition$type == "SEGMENT"){
    if(!is.numeric(itemsToCreate$start)) stop(paste0("Not all columns match the required type!"))
    if(!calculateEndTimeForSegments) {
      if(!is.numeric(itemsToCreate$end)) stop(paste0("Not all columns match the required type!"))
    }
  }
  # rename labels column to label to match labels SQL table column name
  colnames(itemsToCreate)[colnames(itemsToCreate)=="labels"] <- "label"
  
  ## check that every session/bundle combination exists
  bundleList = list_bundles(emuDBhandle)
  
  invalidItems = dplyr::anti_join(x = itemsToCreate,
                                  y = bundleList,
                                  by = c("session",
                                         "bundle" = "name"))
  
  if (nrow(invalidItems) != 0) {
    stop("Some of the session/bundle combinations provided are invalid: ", invalidItems)
  }
  
  # extract all items
  items_all = DBI::dbReadTable(emuDBhandle$connection, "items")
  
  itemsToUpdate = NULL
  
  ## for ITEM levels
  if(levelDefinition$type == "ITEM"){
    
    ## Make sure all sequence indexes are unique within their respective level/attribute pair.
    itemsToCreate %>%
      dplyr::group_by(.data$session, .data$bundle, .data$level, .data$attribute) %>%
      dplyr::do(ensureSequenceIndexesAreUnique(.data))
    
    ## check for conflicting seq index
    in_both = dplyr::inner_join(itemsToCreate, 
                                items_all, 
                                by = c("session", 
                                       "bundle", 
                                       "level", 
                                       "start_item_seq_idx" = "seq_idx"))
    if(nrow(in_both) > 0){
      stop("Found existing items with same 'session', 'bundle', 'level', 'start_item_seq_idx'!")
    }
  }
  ## for EVENT
  if(levelDefinition$type == "EVENT"){
    
    # get sample_rate vector
    bundles_df = DBI::dbReadTable(emuDBhandle$connection, "bundle")
    sample_rate = dplyr::left_join(itemsToCreate, 
                                   bundles_df, 
                                   by = c("session", "bundle" = "name")) %>% 
      dplyr::select(dplyr::starts_with("sample_rate"))
    
    sample_rate = dplyr::as_tibble(sample_rate) # incase it isn't already
    sample_rate = dplyr::pull(sample_rate[,ncol(sample_rate)]) # relevant when multiple sample_rate.x sample_rate.y cols are present
    
    
    # calc. sample_point
    itemsToCreate$sample_point = round((itemsToCreate$start / 1000) * sample_rate)
    
    # check that times don't exist
    in_both = dplyr::inner_join(itemsToCreate, 
                                items_all, 
                                by = c("session", 
                                       "bundle", 
                                       "level", 
                                       "sample_point"))
    
    if(nrow(in_both) > 0){
      stop("Found existing items with same 'session', 'bundle', 'level', 'sample_point'!")
    }
    
    # calculate correct start_item_seq_idx
    
    # find existing items on same levels that are in itemsToCreate
    items_exist_in_levels = dplyr::left_join(items_all,
                                             itemsToCreate,
                                             by = c("session", 
                                                    "bundle", 
                                                    "level"),
                                             relationship = "many-to-many")
    
    items_exist_in_levels = items_exist_in_levels[!is.na(items_exist_in_levels$db_uuid.y),]
    
    # create data.frame object that contains them both
    # that can be used to sort by sample_point to calcualte start_item_seq_idx
    items_to_sort = rbind(
      dplyr::tibble(session = items_exist_in_levels$session, 
                    bundle = items_exist_in_levels$bundle,
                    level = items_exist_in_levels$level,
                    attribute = attributeName,
                    label = "XXX",
                    item_id = items_exist_in_levels$item_id,
                    sample_point = items_exist_in_levels$sample_point.x,
                    item_from = "items_exist_in_levels"
      ),
      dplyr::tibble(session = itemsToCreate$session, 
                    bundle = itemsToCreate$bundle,
                    level = itemsToCreate$level,
                    attribute = itemsToCreate$attribute,
                    label = itemsToCreate$label,
                    item_id = -1,
                    sample_point = itemsToCreate$sample_point,
                    item_from = "itemsToCreate"
      )
    )
    
    items_sorted = items_to_sort %>% 
      dplyr::group_by(.data$session, .data$bundle, .data$level) %>% 
      dplyr::arrange(.data$sample_point, .by_group = TRUE) %>%
      dplyr::mutate(start_item_seq_idx = dplyr::row_number())
    
    itemsToCreate = items_sorted %>% 
      dplyr::filter(.data$item_from == "itemsToCreate") %>% 
      dplyr::ungroup()
    
    itemsToUpdate = items_sorted %>% 
      dplyr::filter(.data$item_from == "items_exist_in_levels") %>% 
      dplyr::ungroup()
    
    
    
  }
  ## for SEGMENT
  if(levelDefinition$type == "SEGMENT"){
    
    # get sample_rate vector
    bundles_df = DBI::dbReadTable(emuDBhandle$connection, "bundle")
    sample_rate = dplyr::left_join(itemsToCreate, 
                                   bundles_df, 
                                   by = c("session", "bundle" = "name")) %>% 
      dplyr::select(dplyr::starts_with("sample_rate"))
    
    sample_rate = dplyr::as_tibble(sample_rate) # in case it isn't already
    sample_rate = dplyr::pull(sample_rate[,ncol(sample_rate)]) # relevant when multiple sample_rate.x sample_rate.y cols are present
    
    
    # calculate sample_start based on user input
    itemsToCreate$sample_start = round((itemsToCreate$start / 1000 + 0.5 / sample_rate) * sample_rate)
    
    if (calculateEndTimeForSegments == TRUE) {
      # Ignore user values and calculate each segment’s sample end based on the
      # following segment’s sample start. For the last segment, use the annotated
      # media file’s duration as sample_end.

      itemsToCreate %>%
        dplyr::group_by(.data$session, .data$bundle, .data$level) %>%
        dplyr::arrange(.data$sample_start, .by_group = TRUE) %>%
        dplyr::mutate(sample_end = dplyr::lead(.data$sample_start) - 1) -> itemsToCreate
      
      # test if no duplicate sample_start values exists
      itemsToCreate %>%
        dplyr::select("session", "bundle", "level", "sample_start") %>%
        dplyr::distinct() -> distinct_sample_start_rows
      
      if(nrow(distinct_sample_start_rows) != nrow(itemsToCreate)){
        stop("Found duplicate sample_start values on same level")
      }
      
      # fix end times of last segments (set to length of wavs)
      # -> currently NA due to dplyr::lead not having a next value 4 them
      last_items = itemsToCreate[is.na(itemsToCreate$sample_end),]
      
      wav_paths = file.path(emuDBhandle$basePath,
                            paste0(last_items$session, session.suffix),
                            paste0(last_items$bundle, bundle.dir.suffix),
                            paste0(last_items$bundle, ".wav"))
      
      itemsToCreate[is.na(itemsToCreate$sample_end),]$sample_end = sapply(wav_paths, FUN = function(wav_path){attr(wrassp::read.AsspDataObj(wav_path), "endRecord")})
    } else {
      # Use user-provided itemsToCreate$end to determine each segment’s sample_end
      itemsToCreate$sample_end = round((itemsToCreate$end / 1000 - 0.5 / sample_rate) * sample_rate)

      
      # Make sure no negative-duration items and no zero-length items and no
      # items shorter than one sample exist.
      #
      # The straightforward way would be to filter for:
      #    end <= start
      # But rounding and float precision make it more intricate.
      segments_with_duration_below_one_sample = itemsToCreate %>% 
        dplyr::filter(  (end - start)/1000 < 1/sample_rate  # This rule focuses on user input (measured in milliseconds).
                        |  sample_end < sample_start)       # This rule focuses on the derived values that will actually be used (measured in samples).
      #
      # Generally, the second rule should follow from the first (but not the first from the second).
      #
      # But I recently learned that R’s round() does round-half-to-even and not
      # round-half-up, and I have been trying to grok the consequences w.r.t.
      # to the +/- 0.5 terms in emuR’s conversion rules between seconds and samples.
      # 
      # And I can no longer wrap my head around whether there are consequences
      # for the relationship between these two filter rules. So I’ll just leave
      # them both, since that won’t hurt.
                        
      # Note: sample_end LESS THAN sample_start is not allowed, but
      # sample_end EQUALS sample_start would be allowed. On the other hand,
      # end EQUALS start (measured in seconds or milliseconds) would *not* be
      # allowed. That’s due to a subtlety of the _annot.json format, which
      # specifies sampleStart and sampleDur(ation), but not sampleEnd.
      #
      # Per definition:
      #    sample_start == sample_end   <=>  sampleDur == 0
      #
      # Also per definition: sampleDur==0 means that the segment only spans its
      # starting sample. Converted to seconds, this is 1/sampleRate and NOT 0.
      # Therefore, sampleDur==0 is not actually zero-length, and is therefore
      # allowed. And so is sample_start == sample_end. But not start==end.
      # 
      # Reference: EMU-SDMS Manual, Chapter File Formats, description of sampleDur.
      
      if (nrow(segments_with_duration_below_one_sample) != 0) {
        warning(paste("itemsToCreate contains",
                      nrow(segments_with_duration_below_one_sample),
                      "segments with duration below one sample or even negative. This is never allowed.",
                      "Inspect this function’s return value to see them. Exiting."))
        return(invisible(segments_with_duration_below_one_sample))
      }
      
      if (!allowGapsAndOverlaps) {
        segments_with_gaps_or_overlap = itemsToCreate %>%
          dplyr::group_by(.data$session, .data$bundle) %>%
          dplyr::arrange(.data$sample_start, .by_group = TRUE) %>%
          dplyr::mutate(gap_samples = .data$sample_start - dplyr::lag(.data$sample_end) - 1) %>%
          dplyr::filter(!is.na(.data$gap_samples) & .data$gap_samples != 0)
        
        if (nrow(segments_with_gaps_or_overlap) != 0) {
          warning(paste("itemsToCreate contains",
                        nrow(segments_with_gaps_or_overlap),
                        "segments with gaps between them or an overlap with their predecessor.",
                        "Inspect this function’s return value to see them, and check",
                        "the documentation of the parameter 'allowGapsAndOverlaps'",
                        "for details. Exiting."))
          return(invisible(segments_with_gaps_or_overlap))
        }
      }
    }
    
    # check if there are already any segments in bundles
    
    sl = query(emuDBhandle, 
               query = paste0(unique(itemsToCreate$level), " =~ .* "), 
               sessionPattern = paste0(unique(itemsToCreate$session), collapse = "|"),
               bundlePattern = paste0(unique(itemsToCreate$bundle), collapse = "|"))
    
    if(nrow(sl) != 0){
      stop("SEGMENT items already exist on the specified bundles & level. This is not permitted!")
    }
    
    itemsToCreate %>%
      dplyr::group_by(.data$session, .data$bundle, .data$level) %>%
      dplyr::arrange(.data$sample_start, .by_group = TRUE) %>%
      dplyr::mutate(start_item_seq_idx = 1:n()) -> itemsToCreate
  }
  
  ##
  ## Get the label index for each attribute (the label index marks the order of attributes within their level)
  ##
  itemsToCreate$labelIndex = get_labelIndex(emuDBhandle = emuDBhandle,
                                            levelName = itemsToCreate$level,
                                            attributeName = itemsToCreate$attribute)
  
  
  # remove old tmp tables if they exist
  remove_annotCrudTmpTables(emuDBhandle)
  
  ##
  ## Copy items data into temporary table
  ##
  create_annotCrudTmpTables(emuDBhandle)
  
  # update start_item_seq_idx in items_annot_crud_tmp table with itemsToUpdate
  if(!is.null(itemsToUpdate)){
    if(nrow(itemsToUpdate) >= 1){
      statement = DBI::dbSendStatement(
        emuDBhandle$connection,
        paste0("UPDATE items_annot_crud_tmp ",
               "SET seq_idx = ? ",
               "WHERE db_uuid = ? ",
               " AND session = ? ",
               " AND bundle = ? ",
               " AND item_id = ?"))
      
      DBI::dbBind(
        statement,
        list(
          itemsToUpdate$start_item_seq_idx,
          rep(emuDBhandle$UUID,length(itemsToUpdate$start_item_seq_idx)),
          itemsToUpdate$session,
          itemsToUpdate$bundle,
          itemsToUpdate$item_id
        )
      )
      
      DBI::dbClearResult(statement)
      
    }
  }
  
  ##
  ## Split the item list into individual items (identified by the session/bundle/level/sequenceIndex tuple),
  ## and proceed separately for each of them
  ##
  itemsToCreate %>%
    dplyr::group_by(.data$session, 
                    .data$bundle, 
                    .data$level, 
                    .data$start_item_seq_idx) %>%
    dplyr::do(insertItemIntoDatabase(emuDBhandle, 
                                     .data, 
                                     levelDefinition$type))
  
  
  ##
  ## Rewrite sequence indexes
  ##
  rewrite_allSequenceIndexes(emuDBhandle)
  ## @todo fail if the user is trying to add sequence indexes that are already
  ## there - or accept it silently, producing undefined order?
  
  ##
  ## Move data from temporary items table back to normal table
  ##
  moveback_annotCrudTmpTables(emuDBhandle)
  
  if (rewriteAllAnnots) {
    rewrite_annots(emuDBhandle, 
                   verbose = verbose)
  }
  
  invisible(NULL)
}

## A function called read_itemsInLevel will not exist - query() is the function that does the job.
# read_itemsInLevel = function (...)


##' Update items programmatically
##' @export
##' 
##' @param emuDBhandle emuDB handle as returned by \code{\link{load_emuDB}}
##' @param itemsToUpdate A data frame with the columns
##' \itemize{
##' \item \code{session},
##' \item \code{bundle},
##' \item \code{level},
##' \item \code{start_item_seq_idx} (\code{start_item_seq_idx} is used instead of e.g.
##' \code{seq_idx} so that the result of a \code{\link{query}} call can be used directly. 
##' \code{\link{query}} can return a sequence of items defined by \code{start_item_seq_idx} 
##' and \code{end_item_seq_idx} which have the same value if single items are returned), 
##' \item \code{attribute}, and
##' \item \code{labels}.
##' }
##' *None* of the columns should be factors.
##' \code{sequenceIndex} must be numeric (natural-valued), all other columns
##' must be of type character.
##' @param rewriteAllAnnots should changes be written to file system (_annot.json
##'                         files) (intended for expert use only)
##' @param verbose if set to \code{TRUE}, more status messages are printed
##' 
update_itemsInLevel = function (emuDBhandle,
                                itemsToUpdate,
                                rewriteAllAnnots = TRUE,
                                verbose = TRUE) {
  
  
  ##
  ## Find the index of each attribute definition on its respective level
  ##
  itemsToUpdate$label_index = get_labelIndex(emuDBhandle = emuDBhandle,
                                             levelName = itemsToUpdate$level,
                                             attributeName = itemsToUpdate$attribute)
  
  
  ##
  ## First thing, make sure all the items whose labels are gonna be changed do exist
  ##
  statement = DBI::dbSendStatement(
    emuDBhandle$connection,
    paste0("SELECT count(*) ",
           "FROM items ",
           "WHERE db_uuid = ? ",
           " AND session = ? ",
           " AND bundle = ? ",
           " AND level = ? ",
           " AND seq_idx = ?"))
  DBI::dbBind(
    statement,
    list(
      rep(emuDBhandle$UUID, nrow(itemsToUpdate)),
      itemsToUpdate$session,
      itemsToUpdate$bundle,
      itemsToUpdate$level,
      itemsToUpdate$start_item_seq_idx
    )
  )
  
  existenceMatrix = DBI::dbFetch(statement)
  DBI::dbClearResult(statement)
  
  if (!all(existenceMatrix)) {
    total = nrow(itemsToUpdate)
    notFound = length(existenceMatrix[existenceMatrix == 0, ])
    
    warning (paste(
      "Error:",
      notFound,
      "of the",
      total,
      "specified items do/does not exist"
    ))
    
    if (verbose) {
      warning(itemsToUpdate[existenceMatrix == 0, ])
    } else {
      warning ("Set verbose to TRUE to see them listed.")
    }
    
    return (invisible(NULL))
  }
  
  #  get item_ids of matching entries
  statement = DBI::dbSendStatement(
    emuDBhandle$connection,
    paste0("SELECT item_id ",
           "FROM items ",
           "WHERE db_uuid = ? ",
           " AND session = ? ",
           " AND bundle = ? ",
           " AND level = ? ",
           " AND seq_idx = ?"))
  
  DBI::dbBind(
    statement,
    list(
      rep(emuDBhandle$UUID, nrow(itemsToUpdate)),
      itemsToUpdate$session,
      itemsToUpdate$bundle,
      itemsToUpdate$level,
      itemsToUpdate$start_item_seq_idx
    )
  )
  
  item_id_list = DBI::dbFetch(statement)
  DBI::dbClearResult(statement)
  
  statement = DBI::dbSendStatement(
    emuDBhandle$connection,
    paste0("INSERT OR REPLACE INTO labels (",
           " db_uuid, ",
           " session, ",
           " bundle, ",
           " item_id, ",
           " label_idx, ",
           " name, ",
           " label ",
           ") VALUES (?, ?, ?, ?, ?, ?, ?)"))
  # rename labels column to label to match labels SQL table column name
  colnames(itemsToUpdate)[colnames(itemsToUpdate) == "labels"] <- "label"
  
  DBI::dbBind(
    statement,
    list(
      rep(emuDBhandle$UUID, nrow(itemsToUpdate)),
      itemsToUpdate$session,
      itemsToUpdate$bundle,
      item_id_list$item_id,
      itemsToUpdate$label_index,
      itemsToUpdate$attribute,
      itemsToUpdate$label
    )
  )
  rowsAffected = DBI::dbGetRowsAffected(statement)
  DBI::dbClearResult(statement)
  
  if (rewriteAllAnnots) {
    rewrite_annots(emuDBhandle, 
                   bundles = unique(data.frame(session = itemsToUpdate$session,
                                               name = itemsToUpdate$bundle)), 
                   verbose = verbose)
  }
  
  invisible(NULL)
}



##' Delete items programmatically
##' 
##' @description Delete annotation items programmatically. You have to pass in a
##' data frame, called `itemsToDelete`, describing these items. The required
##' columns are described below.
##' 
##' This function belongs to emuR’s CRUD family of functions, which let the user
##' manipulate items programmatically:
##' 
##' * Create items ([create_itemsInLevel])
##' * Read items ([query])
##' * Update items ([update_itemsInLevel])
##' * Delete items ([delete_itemsInLevel]))
##' 
##' @details
##' This function deletes annotation items from existing levels. Your input data
##' frame `itemsToDelete` must describe the items by specifying the columns
##' `session`, `bundle`, and `start_item_id`.
##' 
##' A major use case for this function is to obtain a segment list using [query],
##' possibly modify the segment list and feed it to this function. That is why
##' the column `start_item_id` is not called `item_id`: segment lists include
##' the former column name, not the latter.
##' 
##' @param emuDBhandle emuDB handle as returned by [load_emuDB]
##' @param itemsToDelete A data frame with the columns:
##' * `session` (character)
##' * `bundle` (character)
##' * `start_item_id` (numeric)
##' @param rewriteAllAnnots should changes be written to file system (_annot.json
##' files) (intended for expert use only)
##' @param verbose if set to `TRUE`, more status messages are printed
##' 
##' @export
##' @md
delete_itemsInLevel = function (emuDBhandle,
                                itemsToDelete,
                                rewriteAllAnnots = TRUE,
                                verbose = TRUE) {
  
  input_key <- readline(prompt = paste(
    "Currently no checks are performed so use at own risk!",
    "This could, for example, create gaps in the annotation.",
    "Do you wish to continue anyway (y/N)? ",
    sep = "\n")
  )
  if(input_key != "y") return()
  
  
  # check that all required columns are present
  required_colnames = c("session", "bundle", "start_item_id")
  
  if(!all(required_colnames %in% names(itemsToDelete))){
    stop(paste0("Not all required columns are available in itemsToDelete data.frame! ",
                "The required columns are: ", paste(required_colnames, collapse = "; ")))
  }
  
  
  # check types of columns
  if(!is.character(itemsToDelete$session) | 
     !is.character(itemsToDelete$bundle) | 
     !is.numeric(itemsToDelete$start_item_id)
  ){
    stop(paste0("Not all columns match the required type!"))
  }
  
  
  # check that no sequences are present (i.e. rows spanning more than one item).
  # sequences can come from querying thing like [Phonetic == a -> Phonetic == b].
  if ("end_item_id" %in% names(itemsToDelete)) {
    items_that_are_really_sequences = itemsToDelete %>% 
      dplyr::filter(.data$start_item_id != .data$end_item_id)
    
    if (nrow(items_that_are_really_sequences) != 0) {
      warning(paste("itemsToDelete contains",
                    nrow(items_that_are_really_sequences),
                    "rows that span more than one item (start_item_id !=",
                    "end_item_id). This is not allowed. Inspect this function’s",
                    "return value to see them. Exiting."))
      return(invisible(items_that_are_really_sequences))
    }
  }
  
  
  # remove old tmp tables if they exist
  remove_annotCrudTmpTables(emuDBhandle)
  
  ##
  ## Copy items data into temporary table
  ##
  create_annotCrudTmpTables(emuDBhandle)
  
  
  #########################
  # items
  
  statement = DBI::dbSendStatement(
    emuDBhandle$connection,
    paste0("DELETE FROM items_annot_crud_tmp ",
           "WHERE db_uuid = ? ",
           " AND session = ? ",
           " AND bundle = ? ",
           " and item_id = ?"))
  
  DBI::dbBind(
    statement,
    list(
      rep(emuDBhandle$UUID, nrow(itemsToDelete)),
      itemsToDelete$session,
      itemsToDelete$bundle,
      itemsToDelete$start_item_id
    )
  )
  
  
  numberOfDeletedItems = DBI::dbGetRowsAffected(statement)
  DBI::dbClearResult(statement)
  
  #########################
  # labels
  
  statement = DBI::dbSendStatement(
    emuDBhandle$connection,
    paste0("DELETE FROM labels_annot_crud_tmp ",
           "WHERE db_uuid = ? ",
           " AND session = ? ",
           " AND bundle = ? ",
           " and item_id = ?"))
  
  DBI::dbBind(
    statement,
    list(
      rep(emuDBhandle$UUID, nrow(itemsToDelete)),
      itemsToDelete$session,
      itemsToDelete$bundle,
      itemsToDelete$start_item_id
    )
  )
  
  
  numberOfDeletedLabels = DBI::dbGetRowsAffected(statement)
  DBI::dbClearResult(statement)
  
  #########################
  # links from
  
  statement = DBI::dbSendStatement(
    emuDBhandle$connection,
    paste0("DELETE FROM links_annot_crud_tmp ",
           "WHERE db_uuid = ? ",
           " AND session = ? ",
           " AND bundle = ? ",
           " and from_id = ?"))
  
  DBI::dbBind(
    statement,
    list(
      rep(emuDBhandle$UUID, nrow(itemsToDelete)),
      itemsToDelete$session,
      itemsToDelete$bundle,
      itemsToDelete$start_item_id
    )
  )
  

  numberOfDeletedLinks = DBI::dbGetRowsAffected(statement)
  DBI::dbClearResult(statement)
  
  #########################
  # links to
  
  statement = DBI::dbSendStatement(
    emuDBhandle$connection,
    paste0("DELETE FROM links_annot_crud_tmp ",
           "WHERE db_uuid = ? ",
           " AND session = ? ",
           " AND bundle = ? ",
           " and to_id = ?"))
  
  DBI::dbBind(
    statement,
    list(
      rep(emuDBhandle$UUID, nrow(itemsToDelete)),
      itemsToDelete$session,
      itemsToDelete$bundle,
      itemsToDelete$start_item_id
    )
  )
  
  
  numberOfDeletedLinks = numberOfDeletedLinks + DBI::dbGetRowsAffected(statement)
  DBI::dbClearResult(statement)
  

  ## Rewrite sequence indexes
  ##
  rewrite_allSequenceIndexes(emuDBhandle)
  
  ##
  ## Move data from temporary items table back to normal table
  ##
  moveback_annotCrudTmpTables(emuDBhandle)
  
  
  print(paste0("Deleted ",
               numberOfDeletedItems,
               " item(s), ",
               numberOfDeletedLabels,
               " label(s) and ",
               numberOfDeletedLinks,
               " link(s)."))
  
  if (rewriteAllAnnots) {
    rewrite_annots(emuDBhandle, 
                   verbose = verbose)
  }
  
  invisible(NULL)
}

##' create links between items
##'
##' @param emuDBhandle emuDB handle as returned by \code{\link{load_emuDB}}
##' @param links data.frame like object containing linking information. The required columns
##' are: 
##' \itemize{
##' \item \code{session}: 
##' \item \code{bundle}
##' \item \code{from_id}
##' \item \code{to_id}
##' }
##' @param rewriteAllAnnots should changes be written to file system (_annot.json
##'                         files) (intended for expert use only)
##' @param verbose if set to \code{TRUE}, more status messages are printed
##' @export
create_links = function(emuDBhandle,
                        links,
                        rewriteAllAnnots = TRUE,
                        verbose = TRUE) {
  
  input_key <- readline(prompt = "Currently no checks are performed so use at own risk! Do you wish to continue anyway (y/N)? ")
  if(input_key != "y") return()
  
  # todo check if items are all present in database
  # todo check that all links are valid
  # todo check that no links cross each other
  
  
  statement = DBI::dbSendStatement(
    emuDBhandle$connection,
    paste0("INSERT INTO links (",
           " db_uuid, ",
           " session, ",
           " bundle, ",
           " from_id, ",
           " to_id, ",
           " label",
           ") VALUES (?, ?, ?, ?, ?, NULL)"))
  
  DBI::dbBind(
    statement,
    list(
      rep(emuDBhandle$UUID, nrow(links)),
      links$session,
      links$bundle,
      links$from_id,
      links$to_id
    )
  ) 
  
  DBI::dbClearResult(statement)
  
  if (rewriteAllAnnots) {
    rewrite_annots(emuDBhandle, 
                   verbose = verbose)
  }
  
  invisible(NULL)
}


#######################
# FOR DEVELOPMENT
# library('testthat')
# test_file('tests/testthat/test_aaa_initData.R')
# test_file('tests/testthat/test_emuR-annotations_crud.R')
# test_file('tests/testthat/test_zzz_cleanUp.R')
