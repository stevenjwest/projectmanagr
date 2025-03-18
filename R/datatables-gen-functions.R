
#' Create new Plaintext Data Table
#'
#' Creates a new Plaintext DataTable vector. The DataTable will contain an
#' initial ID column containing the `IDs` vector, and an optional set of extra
#' data columns as specified in the `data_cols` vector.
#'
#' If the table exceeds `dt_length` characters (default 100), then the table is
#' split into multiple tables, with IDs as first col, and subsequent data_cols
#' given in subsequent tables.
#'
#' @param IDs Character vector of sample IDs
#'
#' @param data_cols Character vector of data table column titles
#'
#' @param datatable_name String of data table name - default "samples".
#'
#' @param default_data_vals List of default data values to add to data cols &
#' IDs.
#'
#' @param dt_length Int of data table max length in characters - default 100.
#'
#' @param expand Whether to expand any values in vectors set to `default_data_vals`,
#' default is FALSE, meaning custom values can be set in `default_data_vals` for
#' each ID. If TRUE, teh vector is expanded by the length of IDs, meaning ALL
#' values in each vector are set to EACH ID, added as a multi-obs set of rows
#' for each ID.
#'
#' @return List of datatables generated.
#'
#' @export
datatable_create <- function( IDs="", data_cols="", datatable_name = "samples",
                              default_data_vals=list(), dt_length = 100, expand=FALSE ) {

  cat( "\nprojectmanagr::datatable_create():\n" )


  #### check & modify args ####

  IDs <- define_default_ids(IDs)
  check_data_cols_unique(data_cols)


  #### define Data Col Values ####

  # return named list - data_cols, data_col_wds, default_data_vals
  dv_list <- define_data_vals(default_data_vals, data_cols, IDs, REPs = "",
                              rep_exists = FALSE, expand=expand)


  #### build datatable ####
  data_tables <- build_datatable("ID", IDs, dv_list$data_cols, dv_list$default_data_vals,
                                 "CREATE", datatable_name, dt_length)

  data_tables # return

}


define_default_ids <- function(IDs) {
  if( IDs[1] == "" ) {
    IDs <- "DATA_VAL"
  }
  IDs
} #### ________________________________ ####

#' Generate ADD_DATA Samples Datatable
#'
#' Add Data to `ids_vector` from `datatable_name` in `contents` with
#' `data_cols` (column names) and `default_data_vals` (data for each column).
#'
#' Adds new Sample data to an EXISTING Data Table.  The Sample DataTable will
#' contain an initial ID column containing all the EXISTING IDs (IDs that have
#' been resampled exported disposed will automatically be EXCLUDED), and an optional
#' set of extra data columns as specified in the data_cols vector.
#'
#' The function by default assumes all EXISTING SAMPLE (& REPS) are going to
#' have data added: if more than one rep exists for any of the samples, this table
#' will add the rep column and fill it with all the EXISTING rep numbers in the
#' format indicated by `summarise_reps` & `all_reps`.
#'
#' If the table exceeds dt_length characters (default 100), then the table is
#' split into multiple tables, with IDs as first col, and subsequent data_cols
#' given in subsequent tables.
#'
#' @param contents character vector containing current document with
#' existing data tables.
#'
#' @param data_cols Character vector of data table column titles to add.  CANNOT
#' BE BLANK!
#'
#' @param datatable_name String of data table name: This data table MUST ALREADY
#' EXIST/BE DECLARED BEFORE rmd_line in rmd_path.
#'
#' @param ids_vector A vector containing the IDs to add: Can be "ALL", a set of GROUP
#' IDs (which must already be declared in datatable name under a 'group' data
#' column), or (a subset of) IDs from datatable. Default blank string "" indicates
#' to add data to ALL EXISTING IDs.
#'
#' @param default_data_vals List of default data values to add to data cols &
#' IDs.
#'
#' @param dt_length Int of data table max length in characters - default 100.
#'
#' @param summarise_reps Boolean to indicate whether reps should be summarised in
#' the datatable.  If FALSE each ID/rep is on a separate line in the new datatable,
#' otherwise if TRUE, all reps are summarised using r vector index syntax on
#' one line in the new datatable.  i.e. each ID is listed ONCE and the reps are
#' indicated as: 1:3,5,6:10,12,14:25 etc.  Default to FALSE.
#'
#' @param all_reps Boolean to indicate whether all reps should be summarised in
#' the datatable.  If TRUE then ALL reps for each ID are summarised with keyword
#' ALL - which will apply data cols to all the reps for each sample, without the
#' potentially complex summary strings representing a disparate collection of reps.
#' This comes at the expense of not being clear from reading this datatable what
#' reps actually exist.
#'
#' @return List of datatables generated.
#'
#' @export
datatable_add_data_samples <- function(contents, data_cols, datatable_name,
                                       ids_vector="", default_data_vals=list(),
                                       dt_length = 100, summarise_reps = FALSE,
                                       all_reps = FALSE) {


  cat( "\nprojectmanagr::datatable_add_data_samples():\n" )

  # parse all lines in contents to extract all datatables:
  datatables <- datatable_read_vector(contents)

  # confirm datatable_name exists
  check_dt_name_id(datatables, datatable_name)

  # define datatable - the datatable named datatable_name
  datatable <- datatables[[ datatable_name ]]


  #### define IDs ####

  # return named list - IDs, REPs, summarise_reps, rep_exists, group
  ids_list <- define_ids(ids_vector, datatable, summarise_reps, all_reps)


  #### define Data Cols ####

  # return named list - IDs, REPs, summarise_reps, rep_exists
  dc_list <- define_data_cols(datatable, data_cols, ids_list$IDs, ids_list$REPs,
                              ids_list$rep_exists, ids_list$summarise_reps, all_reps)


  #### define Data Col Values ####

  # return named list - data_cols, data_col_wds, default_data_vals
  dv_list <- define_data_vals(default_data_vals, data_cols, dc_list$IDs,
                              dc_list$REPs, dc_list$rep_exists, ids_list$groups)


  #### build datatable ####

  data_tables <- build_datatable("ID", dc_list$IDs, dv_list$data_cols,
                                 dv_list$default_data_vals, "ADD_DATA",
                                 datatable_name, dt_length)

  # return
  data_tables

} #### ________________________________ ####


#' Generate ADD_DATA Variables Datatable
#'
#' Add Data to all `group_names` from `datatable_name` in `contents` with
#' `var_names` (column names) and `default_data_vals` (data for each column).
#'
#' for procedure data
#'
#' First column contains a list of variables, subsequent column titles are
#' typically group IDs, and data values are in cells.
#'
#' @param contents character vector containing current document with
#' existing data tables.
#'
#' @param var_names Names of variables to be added to first column.
#'
#' @param datatable_name The EXISTING datatable from which the IDs must be drawn.
#'
#' @param group_names String vector containing GROUP NAMES from a group set,
#' which will constitute the remaining column headers.  Can be Group Names, or
#' special group ALL.
#'
#' @param default_data_vals List of default data values to add to data cols.
#'
#' @param dt_length Int of data table max length in characters - default 100.
#'
#' @return List of datatables generated.
#'
#' @export
datatable_add_data_variables <- function(contents, var_names, datatable_name, group_names,
                                         default_data_vals = list(), dt_length = 100  ) {


  cat( "\nprojectmanagr::datatable_add_data_variables():\n" )

  # CHECK ERRORS

  # CHECK var_names are all unique
  if( anyDuplicated(var_names) != 0 ) {
    stop( paste0("  duplicate var_names: ", var_names[duplicated(var_names)]) )
  }

  # define ids_vector - blank (to add all IDs & REPs)
  ids_vector <- var_names

  # define data cols : dispose column ONLY
  data_cols <- group_names

  # parse all lines in contents to extract all datatables:
  datatables <- datatable_read_vector(contents)

  # confirm datatable_name exists
  check_dt_name_id(datatables, datatable_name)

  # define datatable - the datatable named datatable_name
  datatable <- datatables[[ datatable_name ]]


  #### define groups ####

  # return named list - IDs, REPs, data_cols, summarise_reps, reps_exist, all_reps
  g_list <- define_groups(datatable, ids_vector, data_cols)


  #### define Data Cols ####

  # return named list - IDs, REPs, summarise_reps, rep_exists
  dc_list <- define_data_cols(datatable, data_cols, g_list$IDs, g_list$REPs,
                              g_list$rep_exists, g_list$summarise_reps, g_list$all_reps)


  #### define Data Col Values ####

  # return named list - data_cols, data_col_wds, default_data_vals
  dv_list <- define_data_vals(default_data_vals, dc_list$IDs, # use dc_list$IDs as data_cols for correct length default data vals
                              dc_list$IDs, dc_list$REPs, dc_list$rep_exists)

  dv_list$default_data_vals <- correct_data_vals(dv_list$default_data_vals, data_cols)


  #### build datatable ####

  data_tables <- build_datatable("variables", dc_list$IDs, data_cols,
                                 dv_list$default_data_vals, "ADD_DATA",
                                 datatable_name, dt_length)

  # return
  data_tables

}


#' ensure data vals match data_cols length for add variables datatable
correct_data_vals <- function(default_data_vals, data_cols) {
  # and correct the list vector lengths to data_cols length
  ddl <- lapply(default_data_vals, '[', 1:length(data_cols))
  ddv <- unlist(lapply(ddl, '[', 1))
  ddl <- list() # now fill blank list with vector
  for(i in 1:length(data_cols)) {
    ddl[[i]] <- ddv
  }
  ddl
}


#' Validate & define group names - from datatable, var_names & group_names.
#'
#' @return group_names vector
define_groups <- function(dt, var_names, group_names) {

  gdt <- get_group_cols(dt)

  # Set IDs (values to go in first col) to var_names
  IDs <- var_names

  # converts group_names to vector same length as dt[["ID"]]
  group_names_comp_ids <- rep(sort(group_names), ((length(dt[["ID"]]) / length(sort(group_names)))+1) )[1:length(dt[["ID"]])]

  # CHECK the group_names exist in the datatable
  # either they are sample IDs, in which case they are from the $ID col
  # or it is the special group `ALL` - so just check this
  # or the group names are the names from a group declared in samples datatable
  if( length(group_names)==1 && group_names[1] == "ALL" ) {

    cat( "  group_names is 'ALL'\n" )

  } else if( length(group_names) == length(dt$ID) && all( group_names_comp_ids ==  sort(dt$ID) ) ) {

    # SHOULD NOT ALLOW THIS
    # gets complicated to handle if IDs have reps
    # so can only add ALL or groups to this kind of datatable..
    stop( paste0("  group names  CANNOT be IDs for var-first layout - use sample-first layout: datatables_add_data_samples"))

    #cat( "  group_names are IDs\n" )

  } else {

    # check all group cols
    group_names_match_group_cols <- c()
    for( i in 1:length(gdt) ) {
      group_names_match_group_cols[i] <- all( unique(gdt[[i]]) %in% group_names )
    }

    if( any(group_names_match_group_cols) == TRUE ) {
      # group_names represent a group
      cat( "  group_names is a group: ", names(gdt)[group_names_match_group_cols], "\n" )
    }
    else {
      # group_names is INVALID
      stop( paste0("  group_names is invalid: Must be ALL or names from EXISTING GROUP: ", group_names))
    }

  }

  # remove any NA from group_names
  group_names <- group_names[!is.na(group_names)]

  g_list <- list(IDs, "", group_names, FALSE, FALSE, FALSE)
  names(g_list) <- c("IDs", "REPs", "data_cols", "summarise_reps", "rep_exists", "all_reps")

  # return
  g_list
} #### ________________________________ ####


#' Generate ADD_DATA Timetable Datatable
#'
#' Add Data to all `group_names` from `datatable_name` in `contents` with
#' `col_name` (prefix for `_con` & `_dt` column names) and `step_names` (`_con`
#' & `_dt` data for each column).
#'
#' for staggered timings of procedures
#'
#' First column contains a list of PLANNED TIMINGS, subsequent column titles are
#' typically group IDs, with a final `change_dt` column for actual datetimes,
#'  and data values are protocol steps, placed in cells under group headers.
#'
#' @param contents character vector containing current document with
#' existing data tables.
#'
#' @param step_names Character vector of names of procedure steps to add to
#' timetable. Must NOT contain any spaces - use '-' or '_'.
#'
#' @param datatable_name The EXISTING datatable from which the IDs must be drawn.
#'
#' @param group_names Vector of sample IDs or GROUP NAMES from a group set, which
#' will constitute the remaining column headers.  These will typically be group
#' names. Must EXIST in contents and be declared from datatable_name IDs!
#'
#' @param col_name The column name where DATETIMES and CONDITIONS will be placed
#' in the read datatable. eg. if `col_name` is 'delip', the recorded DATETIMES
#' are written to column `delip_dt` and the recorded `step_names` are written
#' to column `delip_con`.
#'
#' @param dt_length Int of data table max length in characters - default 100.
#'
#' @return List of datatables generated.
#'
#' @export
datatable_add_data_timetable <- function(contents, step_names, datatable_name,
                                         group_names, col_name, dt_length=100 ) {

  cat( "\nprojectmanagr::datatable_add_data_timetable():\n" )

  # for timetable will add DEFAULT first and last column:
  # first col: timetable
  # will fill with RELATIVE TIMINGS of the protocol
  # last col: <col_name>_dt
  # will fill with ACTUAL DATETIME of execution of the step in the protocol

  # STEP NAMES will be added as data_cols when read and added to samples df!
  data_cols <- step_names
  # CHECK data_cols are all unique
  # REMOVING as can have duplicated step names!
  #if( anyDuplicated(data_cols) != 0 ) {
  #  stop( paste0("  duplicate column headers: ", data_cols[duplicated(data_cols)]) )
  #}
  # check step_names have no spaces
  if( any( grepl(" ", step_names) ) ) {
    stop( paste0("  step_names contains a space: ",
                 paste(step_names[grepl(" ", step_names)], collapse=' ') ) )
  }

  ### NAMED CONSTANTS ###
  obs_default_val_3 <- "VAL" # fill all data_cols with this value
  obs_default_val_5 <- "VALUE" # fill all data_cols with this value
  obs_default_val_8 <- "DATA_VAL" # fill all data_cols with this value
  obs_default_val_dt <- "INSERT__DATETIME" # this occupies 16 chars
  # same length as datetime String: 2021/02/26:18:35

  # parse all lines in contents to extract all datatables:
  datatables <- datatable_read_vector(contents)

  # identify datatable_name - check exists first
  check_dt_name_id(datatables, datatable_name)

  # save to local var:
  dt <- datatables[[ datatable_name ]]


  #### define group values as data cols ####

  gdt <- dplyr::select(dt, dplyr::starts_with("group")) # get all group cols in one dt

  # Set IDs (values to go in first col) to values 0:00 0:10, 0;20 .. up to number of step_names
  IDs <- c("0:00", paste0( "0:", seq(10, ((length(step_names)-1)*10), 10) ) )

  # CHECK the group_names exist in the datatable
  # either they are sample IDs, in which case they are from the $ID col
  # or it is the special group `ALL` - so just check this
  # or the group names are the names from a group declared in samples datatable
  if( length(group_names)==1 && group_names[1] == "ALL" ) {

    # no need to use timetable with ALL - so STOP and suggest var-first datatable
    stop("  Cannot add timetable to ALL - use variable-first or sample-first layout: ")

  } else if( length(group_names) == length(dt$ID) && all(group_names %in% dt$ID) ) {

    # SHOULD NOT ALLOW THIS
    # gets complicated to handle if IDs have reps
    # so can only add ALL or groups to this kind of datatable..
    stop( paste0("  group names CANNOT be IDs for timetable layout - create new group col and use this for timetable layout."))

  } else {

    # check all group cols
    group_names_match_group_cols <- c()
    for( i in 1:length(gdt) ) {
      group_names_match_group_cols[i] <- all( unique(gdt[[i]]) %in% group_names )
    }

    if( any(group_names_match_group_cols) == TRUE ) {
      # group_names represent a group
      cat( "  group_names is a group: ", names(gdt)[group_names_match_group_cols], "\n" )
    } else {
      # group_names is INVALID
      stop( paste0("  group_names is invalid: Must be ALL, <all-sample-IDs> or names from EXISTING GROUP: ", group_names))
    }

  }


  #### define Data Col Values ####

  # CHECK none of data_cols already exists in dt
  dc <- c(data_cols, names(dt) )
  # CHECK data_cols are all unique
  # REMOVING as data_cols (which are step_names) can be repeated!
  #if( anyDuplicated(dc) != 0 ) {
  #  stop( paste0("  Column headers already exist in datatable: ", dc[duplicated(dc)]) )
  #}

  # determine default widths of data cols
  data_col_wds <- c()
  for(i in 1:length(group_names) ) {
    data_col_wds[i] <- pmax(
      nchar(group_names[i])+2,
      max(nchar(step_names))+2,
      5) #pmax ensures min col width is 5!
  }

  # use data_col_wds to calc correct length of default_data_vals:
  default_data_vals <- list()
  for( i in 1:length(group_names) ) {
    # add default data vals to the data col for each ID
    # this is just the step_names for each col!
    default_data_vals[[i]] <- step_names
  }

  # now add the last col - col_name
  group_names <- c(group_names, paste0(col_name, "_dt"))
  default_data_vals[[ length(group_names) ]] <- rep(obs_default_val_dt, length(IDs) )


  #### build datatable ####

  data_tables <- build_datatable("timetable", IDs, group_names, default_data_vals,
                                 "ADD_DATA", datatable_name, dt_length)

  # return
  data_tables

} #### ________________________________ ####


#' Generate Dedicated GROUP Datatable
#'
#' Groups all samples IDs from `datatable_name` in `contents` with
#' `group_names` (group column name) and `groups` (string codes for groups).
#'
#' Adds Groups to EXISTING Data Table IDs in the character vector `contents`.
#' The Groups DataTable will contain an initial ID column containing all existing
#' sample IDs, and a series of extra data columns named using the `group_names`
#' vector.
#'
#' The function assumes all EXISTING SAMPLE (& REPS) will have groups added: if
#' more than one rep exists for any of the samples, this table will add the rep
#' column and fill it with all the EXISTING rep numbers in the format indicated
#' by `summarise_reps` & `all_reps`.
#'
#' If the table exceeds dt_length characters
#' (default 100), then the table is split into multiple tables, with IDs as
#' first col, and subsequent group_names given in subsequent tables.
#'
#' @param contents character vector containing current document with
#' existing data tables.
#'
#' @param group_names Character vector of data table column titles to add.
#' CANNOT BE BLANK!  MUST start all group names with `group`.
#'
#' @param datatable_name String of data table name: This data table MUST ALREADY
#' EXIST/BE DECLARED BEFORE rmd_line in rmd_path.
#'
#' @param groups A LIST OF VECTORS containing the group labels for each group
#' defined in group_names. Must be the same length as `group_names`.
#'
#' @param dt_length Int of data table max length in characters - default 100.
#'
#' @param summarise_reps Boolean to indicate whether reps should be summarised in
#' the datatable.  If FALSE each ID/rep is on a separate line in the new datatable,
#' otherwise if TRUE, all reps are summarised using r vector index syntax on
#' one line in the new datatable.  i.e. each ID is listed ONCE and the reps are
#' indicated as: 1:3,5,6:10,12,14:25 etc.  Default to FALSE.
#'
#' @param all_reps Boolean to indicate whether all reps should be summarised in
#' the datatable.  If TRUE then ALL reps for each ID are summarised with keyword
#' ALL - which will apply data cols to all the reps for each sample, without the
#' potentially complex summary strings representing a disparate collection of reps.
#' This comes at the expense of not being clear from reading this datatable what
#' reps actually exist.
#'
#' @return List of datatables generated.
#'
#' @export
datatable_add_group <- function(contents, group_names, datatable_name,
                                groups, dt_length=100, summarise_reps=FALSE,
                                all_reps=FALSE) {

  cat( "\nprojectmanagr::datatable_add_groups():\n" )

  # DEAL WITH ERRORS

  # check group_names - each entry starts with `group` - if not FAIL:
  if( any( startsWith(group_names, "group") == FALSE ) == TRUE ) {
    # stop this function and inform user to add prefix `group`
    stop( paste0("  group names must start with prefix `group`: ",
                 group_names[startsWith(group_names, "group") == FALSE]) )
  }

  if( length(groups) != length(group_names) ) {
    stop( paste0("  groups list and group_names must be same length: \n  groups length: ",
                 length(groups), " \n  group_names length: ", length(group_names) ) )
  }


  ### NAMED CONSTANTS ###

  # define ids_vector - blank (to add all IDs & REPs)
  ids_vector <- ""

  # define data cols : group_names
  data_cols <- group_names

  # parse all lines in contents to extract all datatables:
  datatables <- datatable_read_vector(contents)

  # confirm datatable_name exists
  check_dt_name_id(datatables, datatable_name)

  # define datatable - the datatable named datatable_name
  datatable <- datatables[[ datatable_name ]]

  # define default data cols as groups list
  default_data_vals <- groups


  #### define IDs ####

  # return named list - IDs, REPs, summarise_reps, rep_exists
  ids_list <- define_ids(ids_vector, datatable, summarise_reps, all_reps)


  #### define Data Cols ####

  # return named list - IDs, REPs, summarise_reps, rep_exists
  dc_list <- define_data_cols(datatable, data_cols, ids_list$IDs, ids_list$REPs,
                              ids_list$rep_exists, ids_list$summarise_reps, all_reps)


  #### define Data Col Values Groups ####

  # return named list - data_cols, data_col_wds, default_data_vals
  dv_list <- define_data_vals_groups(datatable, default_data_vals, data_cols,
                                     dc_list$IDs, dc_list$REPs, dc_list$rep_exists)


  #### build datatable ####

  data_tables <- build_datatable("ID", dc_list$IDs, dv_list$data_cols,
                                 dv_list$default_data_vals, "GROUP",
                                 datatable_name, dt_length)

  # return
  data_tables

} #### ________________________________ ####


#' Generate RESAMPLE Datatable
#'
#' Resamples all samples IDs from `datatable_name` in `contents` with
#' `resample_vector` (string codes for sub-samples) and `rep_vector` (integers
#' to indicate number of reps of each sub-sample).
#'
#' Creates a resample datatable consisting of: sample ID column containing all
#' EXISTING sample IDs, sample rep column (if applicable) containing all EXISTING
#' sample reps, and then the resample and reps columns, which represent the
#' resampling.
#'
#' The function assumes all EXISTING SAMPLE (& REPS) will be resampled: if
#' more than one rep exists for any of the samples, this table will add the rep
#' column and fill it with all the EXISTING rep numbers in the format indicated
#' by `summarise_reps` & `all_reps`.
#'
#' All IDs (& REPs) are added to the output resample datatable:
#'
#' * The User is free to DELETE sample IDs and reps from the resample table, and
#'   the deleted samples will no longer be resampled when read into tibbles.
#'
#' This functionality means the user can resample an INITIAL SUBSET of samples
#' in an experiment (perhaps as a pilot - for example resampling a block of CNS at
#' different thicknesses 100um 200um 400um 500um etc), and then later resample
#' the remaining samples in the experiment (eg. now the pilot shows 200um is the
#' best thickness, all other samples are resampled to 200um thick sections).
#'
#' These two resampling events can be documented in two separate resample tables,
#' and these will be collated into a single resample/rep columns in the final
#' compiled datatable tibble.
#'
#' @param contents character vector containing current document with
#' existing data tables.
#'
#' @param datatable_name String of data table name: This data table MUST ALREADY
#' EXIST/BE DECLARED BEFORE rmd_line in rmd_path.  The resampling will be
#' default include all sample IDs from this datatable.  The user may then
#' delete and sample IDs as is appropriate.
#'
#' @param resample_vector A character vector that contains all the resample
#' codes to be added to each sample/rep in the resample table.
#'
#' @param rep_vector Integer vector of length 1 or `resample_vector`, that
#' specifies how many reps each `resample_vector` code will generate.
#'
#' @param dt_length Int of data table max length in characters - default 100.
#'
#' @param summarise_reps Boolean to indicate whether reps should be summarised in
#' the datatable.  If FALSE each ID/rep is on a separate line in the new datatable,
#' otherwise if TRUE, all reps are summarised using r vector index syntax on
#' one line in the new datatable.  i.e. each ID is listed ONCE and the reps are
#' indicated as: 1:3,5,6:10,12,14:25 etc.  Default to TRUE.
#'
#' @param all_reps Boolean to indicate whether all reps should be summarised in
#' the datatable.  If TRUE then ALL reps for each ID are summarised with keyword
#' ALL - which will apply data cols to all the reps for each sample, without the
#' potentially complex summary strings representing a disparate collection of reps.
#' This comes at the expense of not being clear from reading this datatable what
#' reps actually exist.
#'
#' @return List of datatables generated.
#'
#' @export
datatable_resample <- function(contents, datatable_name, resample_vector, rep_vector=c(1),
                               dt_length=100, summarise_reps=TRUE, all_reps=FALSE) {

  cat( "\nprojectmanagr::datatable_resample():\n" )

  resample_error_check(resample_vector, rep_vector)

  #### NAMED CONSTANTS ####

  # define ids_vector - blank (to add all IDs & REPs)
  ids_vector <- ""

  # define data cols : resample and reps
  data_cols <- c("resample", "reps")

  # parse all lines in contents to extract all datatables:
  datatables <- datatable_read_vector(contents)

  # confirm datatable_name exists
  check_dt_name_id(datatables, datatable_name)

  # define datatable - the datatable named datatable_name
  datatable <- datatables[[ datatable_name ]]

  # define default data cols as resample_vector + rep_vector
  default_data_vals <- list(c(resample_vector), c(as.character(rep_vector) ))


  #### define IDs ####

  # return named list - IDs, REPs, summarise_reps, rep_exists
  ids_list <- define_ids(ids_vector, datatable, summarise_reps, all_reps)


  #### define Data Cols ####

  # return named list - IDs, REPs, summarise_reps, rep_exists
  dc_list <- define_data_cols(datatable, data_cols, ids_list$IDs, ids_list$REPs,
                              ids_list$rep_exists, ids_list$summarise_reps, all_reps)


  #### define Data Col Values ####

  # return named list - data_cols, data_col_wds, default_data_vals
  dv_list <- define_data_vals(default_data_vals, data_cols, dc_list$IDs,
                              dc_list$REPs, dc_list$rep_exists)


  #### build datatable ####

  data_tables <- build_datatable("ID", dc_list$IDs, dv_list$data_cols,
                                 dv_list$default_data_vals, "RESAMPLE",
                                 datatable_name, dt_length)

  # return
  data_tables

}


resample_error_check <- function(resample_vector, rep_vector) {

  if( any( grepl("_", resample_vector) ) ) {
    stop( paste0("  resample_vector cannot contain any UNDERSCORES - use `-`: ", resample_vector[grepl("_", resample_vector)] ) )
  }

  # check rep_vector is length 1 or length equal to resample_vector
  if( !(length(rep_vector) == 1) && !(length(rep_vector) == length(resample_vector)) ) {
    stop( paste0("rep_vector must be length 1 or same length as resample_vector:  \n  rep_vector length: ",
                 length(rep_vector), "  \n  resample_vector length: ", length(resample_vector) ) )
  }

  # modify rep_vector to be length of resample_vector if not so (ie. if length 1)
  if( !(length(rep_vector) == length(resample_vector)) ) {
    rep_vector <- rep(rep_vector, length(resample_vector) )
  }
} #### ________________________________ ####


#' Generate DISPOSE Datatable
#'
#' Disposes all samples IDs from `datatable_name` in `contents` at `cdt` (current
#' datetime).
#'
#' The generated DISPOSE DataTable will contain an initial ID column containing
#' all EXISTING IDs (IDs that have been resampled or exported will automatically
#' be EXCLUDED), and a `dispose` column that contains the `cdt` (current
#' datetime).
#'
#' The function assumes all EXISTING SAMPLE (& REPS) will be disposed: if
#' more than one rep exists for any of the samples, this table will add the rep
#' column and fill it with all the EXISTING rep numbers in the format indicated
#' by `summarise_reps` & `all_reps`.
#'
#' If the table exceeds `dt_length` characters (default 100), then the table is
#' split into multiple tables, with IDs as first col, and subsequent data_cols
#' given in subsequent tables.
#'
#' @param contents character vector containing current document with
#' existing data tables.
#'
#' @param datatable_name String of data table name: This data table MUST ALREADY
#' EXIST IN `contents`.  The resampling will be default include all sample IDs
#' from this datatable.  The user may then delete sample IDs as appropriate.
#'
#' @param dt_length Int of data table max length in characters - default 100.
#'
#' @param summarise_reps Boolean to indicate whether reps should be summarised in
#' the datatable.  If FALSE each ID/rep is on a separate line in the new datatable,
#' otherwise if TRUE, all reps are summarised using r vector index syntax on
#' one line in the new datatable.  i.e. each ID is listed ONCE and the reps are
#' indicated as: 1:3,5,6:10,12,14:25 etc.  Default to FALSE.
#'
#' @param all_reps Boolean to indicate whether all reps should be summarised in
#' the datatable.  If TRUE then ALL reps for each ID are summarised with keyword
#' ALL - which will apply data cols to all the reps for each sample, without the
#' potentially complex summary strings representing a disparate collection of reps.
#' This comes at the expense of not being clear from reading this datatable what
#' reps actually exist.
#'
#' @param cdt Current datetime to insert into dispose col.  Can modify this to
#' a custom datetime as desired.
#'
#' @return List of datatables generated.
#'
#' @export
datatable_dispose <- function(contents, datatable_name, dt_length = 100,
                              summarise_reps = TRUE, all_reps = FALSE,
                              cdt=get_datetime()) {

  cat( "\nprojectmanagr::datatable_dispose():\n" )

  ### NAMED CONSTANTS ###

  # define ids_vector - blank (to add all IDs & REPs)
  ids_vector <- ""

  # define data cols : dispose column ONLY
  data_cols <- c("dispose")

  # parse all lines in contents to extract all datatables:
  datatables <- datatable_read_vector(contents)

  # confirm datatable_name exists
  check_dt_name_id(datatables, datatable_name)

  # define datatable - the datatable named datatable_name
  datatable <- datatables[[ datatable_name ]]

  # define default data cols as cdt
  default_data_vals <- list(cdt)


  #### define IDs ####

  # return named list - IDs, REPs, summarise_reps, rep_exists
  ids_list <- define_ids(ids_vector, datatable, summarise_reps, all_reps)


  #### define Data Cols ####

  # return named list - IDs, REPs, summarise_reps, rep_exists
  dc_list <- define_data_cols(datatable, data_cols, ids_list$IDs, ids_list$REPs,
                              ids_list$rep_exists, ids_list$summarise_reps, all_reps)


  #### define Data Col Values ####

  # return named list - data_cols, data_col_wds, default_data_vals
  dv_list <- define_data_vals(default_data_vals, data_cols, dc_list$IDs,
                                  dc_list$REPs, dc_list$rep_exists)


  #### build datatable ####

  data_tables <- build_datatable("ID", dc_list$IDs, dv_list$data_cols,
                                 dv_list$default_data_vals, "DISPOSE",
                                 datatable_name, dt_length)

  # return
  data_tables

} #### ________________________________ ####


#' Generate EXPORT Datatable
#'
#' Exports all samples IDs from `datatable_name` in `contents` to
#' `destination_rel_path_link`.
#'
#' The generated EXPORT DataTable will contain an initial ID column containing
#' all EXISTING IDs (IDs that have been resampled or exported will automatically
#' be EXCLUDED), and an `export` column that contains the `destination_rel_path`.
#'
#' Explicit IDs or ID-REPs (if reps exist in selected datatable) can be set via
#' the `ids_vector` and `reps_vector` arguments.  If blank, all eisting IDs (&
#' REPs) will be exported.  This table will add the rep column in the format
#' indicated by `summarise_reps`.
#'
#' If the table exceeds `dt_length` characters (default 100), then the table is
#' split into multiple tables, with IDs as first col, and subsequent data_cols
#' given in subsequent tables.
#'
#' @param contents character vector containing current document with
#' existing data tables.
#'
#' @param datatable_name String of data table name: This data table MUST ALREADY
#' EXIST/BE DECLARED BEFORE rmd_line in rmd_path.
#'
#' @param destination_rel_path_link Markdown link containing relative path to
#' the Destination Note where these samples are exported to.  This is used to
#' fill the EXPORT datatable, allowing the tracing of exported sample data. Link
#' typically has structure of `[PREFIX](relative_path)` (name is prefix, relative
#' path declared in path portion of the link).
#'
#' @param destination_rel_path_link Markdown link containing the relative path
#' to the Destination Note where these samples are EXPORTED to.
#'
#' @param ids_vector A vector containing the IDs to add: Must be a set of EXISTING
#' IDs from the selected `datatable_name` - cannot be ALL or a set of Group IDs.
#' Default blank string "" indicates to add data to ALL EXISTING IDs.  Existing
#' IDs are all IDs that have not been resampled, exported, or disposed.
#'
#' @param reps_vector A vector containing the REPs to add: This is combined with
#' the `ids_vector` to specify ID-REP pairs.  Each ID-REP pair must exist (not
#' be resampled, exported, or disposed) from the selected `datatable_name`.
#' Default blank string "" indicates to add data to ALL EXISTING ID-REPs.
#'
#' @param dt_length Int of data table max length in characters - default 100.
#'
#' @param summarise_reps Boolean to indicate whether reps should be summarised in
#' the generated datatable.  If FALSE each ID/rep is on a separate line in the
#' new datatable, otherwise if TRUE, all reps are summarised using r vector index
#' syntax on one line in the new datatable.  i.e. each ID is listed ONCE and the
#' reps are indicated as: 1:3,5,6:10,12,14:25 etc.  Default to FALSE.
#'
#' @return List of datatables generated.
#'
#' @export
datatable_export <- function(contents, datatable_name, destination_rel_path_link,
                             ids_vector="", reps_vector="", dt_length=100,
                             summarise_reps=FALSE) {

  cat( "\nprojectmanagr::datatable_export():\n" )

  ### NAMED CONSTANTS ###

  # define all_reps as FALSE - want to explicitly define reps when present!
  all_reps <- FALSE

  # define data cols : export column ONLY
  data_cols <- c("export")

  # parse all lines in contents to extract all datatables:
  datatables <- datatable_read_vector(contents)

  check_dt_name_id(datatables, datatable_name)

  # define datatable - the datatable named datatable_name
  datatable <- datatables[[ datatable_name ]]

  # define the default_data_vals list as destination_rel_path_link
  default_data_vals <- list(c(destination_rel_path_link))


  #### define IDs REPs ####

  # return named list - IDs, REPs, summarise_reps, rep_exists
  ids_list <- define_ids_reps(ids_vector, reps_vector, datatable, summarise_reps)


  #### define Data Cols ####

  # return named list - IDs, REPs, summarise_reps, rep_exists
  dc_list <- define_data_cols(datatable, data_cols, ids_list$IDs, ids_list$REPs,
                              ids_list$rep_exists, ids_list$summarise_reps, all_reps)


  #### define Data Col Values ####

  # return named list - data_cols, data_col_wds, default_data_vals
  dv_list <- define_data_vals(default_data_vals, data_cols, dc_list$IDs,
                                  dc_list$REPs, dc_list$rep_exists)


  #### build datatable ####

  ID_col <- "ID"
  IDs <- dc_list$IDs
  data_cols <- dv_list$data_cols
  data <- dv_list$default_data_vals
  dt_function <- "EXPORT"
  datatable_name <- datatable_name
  MAX_DATATABLE_LENGTH <- dt_length
  DATATABLE_SPACER_CHAR = "="


  data_tables <- build_datatable("ID", dc_list$IDs, dv_list$data_cols,
                                 dv_list$default_data_vals, "EXPORT",
                                 datatable_name, dt_length)

  # return
  data_tables

} #### ________________________________ ####


#' Generate IMPORT Datatable
#'
#' Imports all samples IDs from `datatable_name` in `source_contents` from
#' `source_rel_path`.
#'
#' The generated IMPORT DataTable will contain:
#'
#' * an initial ID column containing all EXISTING IDs from the `source_contents`
#' datatable `datatable_name`  (IDs that have been resampled or exported will
#' automatically be EXCLUDED)
#'
#' * and an `import` column that contains the `source_rel_path` combined with
#' the ID & REP values in the source contents/datatable, for each ID/REP.  These
#' are declared using special separate syntax defined in `get_import_sep()` - by
#' default this is `::`.  This is necessary in case there are clashes in sample
#' IDs/REPs when IMPORTING data from different notes.
#'
#' A subset of existing IDs or IDs/REPs (if reps exist in selected datatable)
#' can be set via the `ids_vector` and `reps_vector` arguments.  If blank, ALL
#' existing IDs (& REPs) will be imported.  This table will add the rep column
#' in the format indicated by `summarise_reps`.
#'
#' If the table exceeds `dt_length` characters (default 100), then the table is
#' split into multiple tables, with IDs as first col, and subsequent data_cols
#' given in subsequent tables.
#'
#' @param source_contents character vector containing source document with
#' existing data tables for samples to be imported from.
#'
#' @param destination_contents character vector containing destination document,
#' which may contains existing CREATE or IMPORT datatables, that must be checked
#' for possible sample ID/REP clashes.
#'
#' @param datatable_name String of data table name: This data table MUST ALREADY
#' EXIST/BE DECLARED BEFORE rmd_line in rmd_path.
#'
#' @param source_rel_path_link Markdown link containing relative path to the Source
#' Note where these samples are impored from.  This is used to fill the IMPORT
#' datatable, allowing the tracing of imported sample data. Link typically has
#' structure of `[PREFIX](relative_path)` (name is prefix, relative path declared
#' in path sportion of the link).
#'
#' @param ids_vector A vector containing the IDs to add: Must be a set of EXISTING
#' IDs from the selected `datatable_name` - cannot be ALL or a set of Group IDs.
#' Default blank string "" indicates to add data to ALL EXISTING IDs.  Existing
#' IDs are all IDs that have not been resampled, exported, or disposed.
#'
#' @param reps_vector A vector containing the REPs to add: This is combined with
#' the `ids_vector` to specify ID-REP pairs.  Each ID-REP pair must exist (not
#' be resampled, exported, or disposed) from the selected `datatable_name`.
#' Default blank string "" indicates to add data to ALL EXISTING ID-REPs.
#'
#' @param dt_length Int of data table max length in characters - default 100.
#'
#' @param summarise_reps Boolean to indicate whether reps should be summarised in
#' the datatable.  If FALSE each ID/rep is on a separate line in the new datatable,
#' otherwise if TRUE, all reps are summarised using r vector index syntax on
#' one line in the new datatable.  i.e. each ID is listed ONCE and the reps are
#' indicated as: 1:3,5,6:10,12,14:25 etc.  Default to FALSE.
#'
#' @return List of datatables generated.
#'
#' @export
datatable_import <- function(source_contents, destination_contents, datatable_name,
                             source_rel_path_link, ids_vector="", reps_vector="",
                             dt_length=100, summarise_reps=FALSE) {

  cat( "\nprojectmanagr::datatable_import():\n" )

  ### NAMED CONSTANTS ###

  # define all_reps as FALSE - want to explicitly define reps when present!
  all_reps <- FALSE

  # define data cols : export column ONLY
  data_cols <- c("import")

  # parse all lines in source_contents to extract all datatables for import:
  datatables <- datatable_read_vector(source_contents)

  check_dt_name_id(datatables, datatable_name)

  # define datatable - the datatable named datatable_name
  datatable <- datatables[[ datatable_name ]]

  # extract any datatable that exist in destination_contents
   # to check IDs/REPs are unique!
  dest_datatables <- datatable_read_vector(destination_contents, notify=FALSE)

  #### define IDs REPs ####

  # return named list - IDs, REPs, summarise_reps, rep_exists
  ids_list <- define_ids_reps(ids_vector, reps_vector, datatable, summarise_reps)



  #### define Data Cols ####

  # return named list - IDs, REPs, summarise_reps, rep_exists
  dc_list <- define_data_cols(datatable, data_cols, ids_list$IDs, ids_list$REPs,
                              ids_list$rep_exists, ids_list$summarise_reps, all_reps)

  # define the default_data_vals list as source_rel_path_link concat with IDs & REPs
   # all separated by `get_import_sep()` string
  default_data_vals <- define_import_vals(dc_list, source_rel_path_link)
   # this performs data val expansion - so must NOT expand data in define_data_vals() below!


  #### modify IDs to be unique in destination ####

  # ID column must be unique for ALL datatables - if ANY ID already exists in datatable
  # of SAME NAME (a possibility as often will be importing same sample types!) need to alter
  # the IMPORT IDs using a string from the source note..?
  dc_list$IDs <- modify_matching_import_ids(dest_datatables, datatable_name,
                                            dc_list, source_rel_path_link)

  #### define Data Col Values ####

  # return named list - data_cols, data_col_wds, default_data_vals
  dv_list <- define_data_vals(default_data_vals, data_cols, dc_list$IDs,
                                  dc_list$REPs, dc_list$rep_exists, expand=FALSE) # do not expand!


  #### build datatable ####

  data_tables <- build_datatable("ID", dc_list$IDs, dv_list$data_cols,
                                 dv_list$default_data_vals, "IMPORT",
                                 datatable_name, dt_length)

  # return
  data_tables

}

#' Get Import Separator
#'
#' return double colon string - used to separate source relative path, ID,
#' and REP(s) in an IMPORT datatable.
#'
get_import_sep <- function() {
  "::"
}

#' Define Import Valus
#'
#' Returns list of `default_data_vals` for `import` col in IMPORT datatable,
#' consisting of source_rel_path_link separated from IDs with get_import_sep(), and
#' possibly REPs if they exist.
#'
define_import_vals <- function(dc_list, source_rel_path_link) {

  if( length(dc_list$REPs) == 1 && dc_list$REPs[1] == "" ) { # if no reps just concat source path && IDs
    list(
        paste0(rep(source_rel_path_link, length(dc_list$IDs)),
               get_import_sep(), dc_list$IDs ))
  } else { # REPS exist - concat source path && IDs && REPs
    list(
        paste0(rep(source_rel_path_link, length(dc_list$IDs)),
               get_import_sep(), dc_list$IDs,
               get_import_sep(), dc_list$REPs ))
  }

}


modify_matching_import_ids <- function(dest_datatables, datatable_name, dc_list,
                                       source_rel_path_link, ext_sep=".") {

  source_rel_name <- fs::path_file(get_path_from_link(source_rel_path_link))

  if( any(names(dest_datatables) == datatable_name) ) {
    # destination has a datatable of same name

    if( any( dest_datatables[[datatable_name]]$ID %in% dc_list$IDs ) ) {
      # some IDs are matching - so modify the IMPORT IDs!
      name_alnum <- stringr::str_replace_all(
        basename(source_rel_name), "[^[:alnum:]]", "") # convert filename to all alpha-numerics

      dc_list$IDs <- concat_ids_ext(dest_datatables, datatable_name, dc_list,
                                    name_alnum, ext_sep)
      return(dc_list$IDs)

    } else {
      return(dc_list$IDs)
    }
  } else {
    return(dc_list$IDs)
  }
}

concat_ids_ext <- function(dest_datatables, datatable_name, dc_list,
                           name_alnum, ext_sep, start=1, inc=2) {

  IDm <- dc_list$IDs[( dc_list$IDs %in% dest_datatables[[datatable_name]]$ID )] # extract matching IDs

  end <- start+inc
  #cat("start: ", start, '\n')
  #cat("inc: ", inc, '\n')
  #cat("end: ", end, '\n')

  name_ext <- substr(name_alnum, start, end) # extract a small string fragment from this
  #cat('name_ext: ', name_ext, '\n')

  # join the IDs that need modifying & name extension - separated by the ext_sep
  IDm <- paste0(IDm, ext_sep, rep(name_ext, length(IDm)))
  #cat('IDm: ', IDm, '\n\n')

  if( nchar(name_alnum) < end ) {
    # reached end of this increment - restart start and increase inc, and call again
    concat_ids_ext(dest_datatables, datatable_name, dc_list,
                   name_alnum, ext_sep, start=1, inc=(inc+1))
  } else if( any( dest_datatables[[datatable_name]]$ID %in% IDm ) ) { # check the new strings are now unique! if not
    # search again for a new string - shifted 1 along from starts
    concat_ids_ext(dest_datatables, datatable_name, dc_list,
                   name_alnum, ext_sep, start=(start+1), inc=inc)
  } else { # if unique
    # modify the IDs value and return it!
    dc_list$IDs[( dc_list$IDs %in% dest_datatables[[datatable_name]]$ID )] <- IDm
    return(dc_list$IDs)
  }
} #### ________________________________ ####


#' Define IDs and REPs - based on current datatable contents
#'
#' @return named list containing IDs, REPs, summarise_reps, rep_exists
define_ids <- function(ids_vector, datatable, summarise_reps, all_reps) {

  IDs <- ""
  REPs <- ""

  groups <- FALSE

  # identify if reps exist - if contains col named `rep`
  rep_exists <- any( names( datatable) == "rep" )

  if( length(ids_vector)== 1 && ids_vector == "" ) { # if this is BLANK, define IDs as all IDs that EXIST:

    if( summarise_reps == FALSE || rep_exists == FALSE) { # FIRST split whether rep col exists && summarise_reps is TRUE:

      # get ALL EXISTING IDs - all that have NOT been exported, disposed, resampled
      if( rep_exists == TRUE ) { # also get EXISTING REPs if rep_exists
        ID_rep <- check_divisions_ids_reps( datatable )
        IDs <- ID_rep$IDs
        REPs <- ID_rep$REPs
      } else {
        IDs <- check_divisions_ids( datatable )
      }

    } else if( summarise_reps == TRUE && rep_exists == TRUE ) {
      # rep_exists is TRUE - so need to process IDs and REPs accordingly

      # get ALL EXISTING IDs & REPs - all that have NOT been exported, disposed, resampled
      ID_rep <- check_divisions_ids_reps( datatable )
      IDs <- ID_rep$IDs
      REPs <- ID_rep$REPs

      if(length(IDs) == 0) {
        stop( paste0("  no IDs exist in this datatable - all resampled, disposed, exported: ", datatable_name) )
      }

      # NOW process IDs and REPs so there is ONE PER ID - summarise REPs!
      ID_rep <- summarise_id_rep(IDs, REPs, all_reps)
      IDs <- ID_rep$IDs
      REPs <- ID_rep$REPs

    }

  } else {

    # ids_vector contains either ALL, <group-names>, <ID-names>
    # CHECK they are VALID

    gdt <- dplyr::select(datatable, dplyr::starts_with("group"))
    iddt <- datatable$ID

    if( length(ids_vector)==1 && ids_vector[1] == "ALL" ) {
      # all good - set IDs to ALL
      IDs <- ids_vector[1]

      if(rep_exists == TRUE) {
        summarise_reps <- TRUE
        REPs <- "ALL" # use special summary syntax - ALL - for the rep col!
      }

    } else {

      # check each ids_vector - first through all IDs, then through all groups

      if( any(iddt %in% ids_vector) ) { # CHECK any IDs from DT are in ids_vector

        # check all IDs in ids_vector are in DT
        if( all(ids_vector %in% iddt) == FALSE ) {
          stop( paste0("  Non-existant ID in ids_vector: ", ids_vector[!(ids_vector %in% iddt)] ) )
        }
        # if this passes, set IDs to ids_vector
        IDs <- ids_vector # THIS MAY BE A SUBSET OF IDs!!

        if(rep_exists == TRUE) { # for each ID in IDs, get the VALID reps:
          # first get all IDs/REPs
          ID_rep <- check_divisions_ids_reps( datatable )
          # now filter through ID_rep$IDs/REPs saving only ones which match IDs
          IDs <- ID_rep$ID[ID_rep$ID %in% IDs]
          REPs <- ID_rep$REP[ID_rep$ID %in% IDs]

          # deal with summarising reps ONLY if rep_exists
          if(summarise_reps == TRUE) {
            # summarise the REPs variable (and IDs variable!)
            ID_rep <- summarise_id_rep(IDs, REPs, all_reps)
            IDs <- ID_rep$IDs
            REPs <- ID_rep$REPs
          }

        } else { # there are no reps, just check_divisions for IDs and filter for VALID IDs
          IDs <- IDs[IDs %in% check_divisions_ids(datatable)]
        }

      } else if( any(sort( unlist(gdt)[ !is.na(unlist(gdt)) ] ) == ids_vector[1]) ) { # CHECK GROUPs
        # this first checks if the first val in ids_vector is in gdt AT ALL

        # check ALL ids_vectors match gdt vals!

        # get index of the group dt first
        gdtindex <- 0
        for(q in 1:length(gdt) ) {
          # remove any NAs first
          gdt2 <- gdt[[q]][!is.na(gdt[[q]])]
          if( any(gdt2 == ids_vector[1]) ) { # only compare to FIRST ids_vector index
            # comparing vectors of differnet lengths does so with recycling, which may miss the value!
            gdtindex <- q
          }
        }
        # now check each ids_vector exists in this gdt vector
        gdt2 <- gdt[[gdtindex]][!is.na(gdt[[gdtindex]])]
        pass <- TRUE
        for(i in 1:length(ids_vector) ) {
          if(any(gdt2 == ids_vector[i]) == FALSE) {
            pass <- FALSE
          }
        }
        if(pass == FALSE) {
          stop( paste0("  Non-existant GROUP value in ids_vector: ", ids_vector ) )
        }
        # if this passes, set IDs to ids_vector
        IDs <- ids_vector
        groups <- TRUE # set groups to TRUE
      } else {
        # no matches for ID or GROUPs - FAIL
        stop( paste0("  Could not identify ids_vector as IDs or group names: ", ids_vector ) )
      }
    }

  } #### END DEFINE IDs

  if(length(IDs) == 0) {
    stop( paste0("  no IDs exist in this datatable - all resampled, disposed, exported: ", datatable_name) )
  }

  ids_list <- list(IDs, REPs, summarise_reps, rep_exists, groups)
  names(ids_list) <- c("IDs", "REPs", "summarise_reps", "rep_exists", "groups")

  # return
  ids_list

}


#' Define IDs and REPs - based on current datatable contents
#'
#' This function fails to parse ALL or <GROUP_NAMES>, and only allows explicit
#' subsets of IDs & REPs to be defined in the `ids_vector` & `reps_vector`.
#'
#' @return named list containing IDs, REPs, summarise_reps, rep_exists
define_ids_reps <- function(ids_vector, reps_vector, datatable, summarise_reps) {

  if( length(reps_vector) == 1 && reps_vector == "") {
    define_ids_explicit(ids_vector, reps_vector, datatable, summarise_reps)
  } else { # check ids_vector & reps_vector are valid
    define_ids_reps_explicit(ids_vector, reps_vector, datatable, summarise_reps)
  }
}


#' Define IDs and REPs explicitly - based on current datatable contents
#'
#' This function fails to parse ALL or <GROUP_NAMES>, and only allows explicit
#' subsets of IDs & REPs to be defined in the `ids_vector` & `reps_vector`.
#'
#' @return named list containing IDs, REPs, summarise_reps, rep_exists
define_ids_explicit <- function(ids_vector, reps_vector, datatable, summarise_reps) {

  IDs <- ""
  REPs <- ""

  # identify if reps exist - if contains col named `rep`
  rep_exists <- any( names( datatable) == "rep" )

  if( length(ids_vector)== 1 && ids_vector == "" ) { # if this is BLANK, define IDs as all IDs that EXIST:

    if( summarise_reps == FALSE || rep_exists == FALSE) { # FIRST split whether rep col exists && summarise_reps is TRUE:

      # get ALL EXISTING IDs - all that have NOT been exported, disposed, resampled
      if( rep_exists == TRUE ) { # also get EXISTING REPs if rep_exists
        ID_rep <- check_divisions_ids_reps( datatable )
        IDs <- ID_rep$IDs
        REPs <- ID_rep$REPs
      } else {
        IDs <- check_divisions_ids( datatable )
      }

    } else if( summarise_reps == TRUE && rep_exists == TRUE ) {
      # rep_exists is TRUE - so need to process IDs and REPs accordingly

      # get ALL EXISTING IDs & REPs - all that have NOT been exported, disposed, resampled
      ID_rep <- check_divisions_ids_reps( datatable )
      IDs <- ID_rep$IDs
      REPs <- ID_rep$REPs

      if(length(IDs) == 0) { # need to check here to prevent cryptic error in summarise_id_rep() with no IDs
        stop( paste0("  no IDs exist in this datatable - all resampled, disposed, exported: ", datatable_name) )
      }

      # NOW process IDs and REPs so there is ONE PER ID - summarise REPs!
      ID_rep <- summarise_id_rep(IDs, REPs, FALSE) # all_reps is FALSE
      IDs <- ID_rep$IDs
      REPs <- ID_rep$REPs

    }

  } else {

    # ids_vector contains <ID-names> : CHECK they are VALID
    iddt <- datatable$ID

    if( any(iddt %in% ids_vector) ) { # CHECK any IDs from DT are in ids_vector

      # check all IDs in ids_vector are in DT
      if( all(ids_vector %in% iddt) == FALSE ) {
        stop( paste0("  Non-existant ID in ids_vector: ", ids_vector[!(ids_vector %in% iddt)] ) )
      }

      # if this passes, set IDs to ids_vector
      IDs <- ids_vector # THIS MAY BE A SUBSET OF IDs!!

      if( rep_exists == TRUE) { # for each ID in IDs, get the VALID reps:
        # first get all IDs/REPs
        ID_rep <- check_divisions_ids_reps( datatable )
        # now filter through ID_rep$IDs/REPs saving only ones which match IDs
        IDs <- ID_rep$ID[ID_rep$ID %in% IDs]
        REPs <- ID_rep$REP[ID_rep$ID %in% IDs]

        # deal with summarising reps ONLY if rep_exists
        if(summarise_reps == TRUE) {
          # summarise the REPs variable (and IDs variable!)
          ID_rep <- summarise_id_rep(IDs, REPs, FALSE) # all_reps is FALSE
          IDs <- ID_rep$IDs
          REPs <- ID_rep$REPs
        }

      } else { # there are no reps, just check_divisions for IDs and stop if any or IDs is not VALID

        ID <- check_divisions_ids(datatable)
        if( any( !(IDs %in% ID) ) ) {
          stop( paste0("  ID has been disposed/exported/resampled: ",
                       paste(IDs[!(IDs %in% ID)], collapse=' ') ) )
        }
      }

    } else {
      # no matches for ID or GROUPs - FAIL
      stop( paste0("  Could not identify ids_vector as IDs: ", ids_vector ) )
    }

  } #### END DEFINE IDs

  if(length(IDs) == 0) {
    stop( paste0("  no IDs exist in this datatable - all resampled, disposed, exported: ", datatable_name) )
  }

  ids_list <- list(IDs, REPs, summarise_reps, rep_exists)
  names(ids_list) <- c("IDs", "REPs", "summarise_reps", "rep_exists")

  # return
  ids_list

}


#' Define IDs and REPs explicitly - based on current datatable contents
#'
#' This function fails to parse ALL or <GROUP_NAMES>, and only allows explicit
#' subsets of IDs & REPs to be defined in the `ids_vector` & `reps_vector`.
#'
#' This function is only called when reps_vector has been set - so only need to
#' confirm validity of ids_vector and reps_vector as valid IDs and REPs.
#'
#' @return named list containing IDs, REPs, summarise_reps, rep_exists
define_ids_reps_explicit <- function(ids_vector, reps_vector, datatable, summarise_reps,
                                     sep=".-.") {

  IDs <- ""
  REPs <- ""

  # identify if reps exist - if contains col named `rep`
  rep_exists <- any( names( datatable) == "rep" )

  if( rep_exists == FALSE ) { # there is an error - STOP
    stop( paste0("  reps_vector is set but no reps exist in datatable: ", datatable_name,
                 "\n    reps_vector: ", paste(reps_vector, collapse=' ')) )
  }

  # check combinations of all valid IDs & REPs against combinations of ids_vector & reps_vector
  # get all valid IDs & REPs
  ID_rep <- check_divisions_ids_reps( datatable )
  # create concat of ID and REPs
  IDsREPs <- paste0(ID_rep$IDs, sep, ID_rep$REPs)

  # create concat of ids_vector and reps_vector
  IDsREPsVector <- paste0(ids_vector, sep, reps_vector)

  if( any(IDsREPs %in% IDsREPsVector) ) { # IDsREPsVector contains at least SOME valid IDs & REPS combos

    # check all IDs & REPS combos are in DT
    if( all(IDsREPsVector %in% IDsREPs) == FALSE ) {
      stop( paste0("  Non-existant ID-REP combo in ids_vector & reps_vector: ",
                   paste(IDsREPsVector[!(IDsREPsVector %in% IDsREPs)], collapse=' ') ) )
    }
    # if this passes set IDs and REPs
    IDs <- ids_vector
    REPs <- reps_vector

    if( summarise_reps == TRUE ) {
      # summarise the REPs variable (and IDs variable!)
      ID_rep <- summarise_id_rep(IDs, REPs, FALSE) # all_reps is FALSE
      IDs <- ID_rep$IDs
      REPs <- ID_rep$REPs
      }

  } else {
    # no matches for IDs-REPs - FAIL
    stop( paste0("  Could not identify any ID-REP combo in ids_vector & reps_vector as IDs: ", paste(IDsREPsVector, collapse=' ') ) )
  }


  if(length(IDs) == 0) {
    stop( paste0("  no IDs exist in this datatable - all resampled, disposed, exported: ", datatable_name) )
  }

  ids_list <- list(IDs, REPs, summarise_reps, rep_exists)
  names(ids_list) <- c("IDs", "REPs", "summarise_reps", "rep_exists")

  # return
  ids_list

}




#' Define IDs and REPs - trimmed based on current datatable data_cols contents
#'
#' @return named list containing MODIFIEDs IDs, REPs, summarise_reps, rep_exists
define_data_cols <- function(datatable, data_cols, IDs, REPs, rep_exists,
                             summarise_reps, all_reps) {

  check_data_cols_unique(data_cols)

  # CHECK if any data_cols already exists in datatable
  dc <- c(data_cols, names(datatable) )
  # CHECK data_cols are all unique
  if( anyDuplicated(dc) != 0 ) {

    # if dcs exist in datatable, check the values in these columns for all IDs - only keep IDs that contain NA in all the data cols
    ids_l <- list()
    l_i <- 1
    for(dc in data_cols) {

      if( any( names(datatable) == dc ) ) {
        # identify the indices that are NA in dc in datatable
        # and use this to extract a vector of IDs from ID col into ids_l list
        ids_l[[l_i]] <- datatable[['ID']][which( !is.na(datatable[[dc]]) )]
      }
      l_i <- l_i + 1 # increment index
    }
    ids_exclude <- unique(unlist(ids_l)) # get unique vector of all IDs to exclude
    IDs <-  IDs[!(IDs %in% ids_exclude)] # remove IDs to exclude from IDs
    if( length(IDs) == 0 ) {
      stop( paste0("  Column headers already exist in all samples in datatable: ", dc[duplicated(dc)]) )
    }

    if( rep_exists == TRUE ) {
      # also modify the REPs array
      # re run through this code from DEFINE IDs
      ID_rep <- check_divisions_ids_reps( datatable )
      # now filter through ID_rep$IDs/REPs saving only ones which match IDs
      IDs2 <- c()
      REPs2 <- c()
      for(o in 1:length( ID_rep$IDs ) ) {
        if( any(ID_rep$IDs[o] == IDs) ) {
          IDs2 <- c(IDs2, ID_rep$IDs[o])
          REPs2 <- c(REPs2, ID_rep$REPs[o])
        }
      }
      # and set IDs and REPs to these new filtered values
      IDs <- IDs2
      REPs <- REPs2 # now these contain the subset of IDs and their REPs

      # deal with summarising reps ONLY if rep_exists
      if(summarise_reps == TRUE) {
        # summarise the REPs variable (and IDs variable!)
        ID_rep <- summarise_id_rep(IDs, REPs, all_reps)
        IDs <- ID_rep$IDs
        REPs <- ID_rep$REPs
      }
    } # end rep_exists
  } # end CHECK data_cols unique

  dc_list <- list(IDs, REPs, summarise_reps, rep_exists)

  names(dc_list) <- c("IDs", "REPs", "summarise_reps", "rep_exists")

  # return
  dc_list

}


check_data_cols_unique <- function(data_cols) {
  # CHECK data_cols are all unique
  if( anyDuplicated(data_cols) != 0 ) {
    stop( paste0("  data_cols contains duplicate column headers: ", data_cols[duplicated(data_cols)]) )
  }
}


#' Define data_cols + wds + vals - based on data_cols, IDs, & REPs
#'
#' @param groups Boolean to indicate id IDs are group IDs.  Stops rep col being added
#' if IDs are group IDs.
#'
#' @param expand Whether to expand default_data_vals by length of IDs.  If default_data_vals
#' have been set, they are expanded along the length of IDs by default (expand is TRUE). If
#' expand is set to FALSE, the default_data_vals are not expanded - this allows a list
#' of vectors that match length of IDs vector to be added, allowing custom values for
#' each ID.
#'
#' @return named list containing MODIFIED data_cols, data_col_wds, default_data_vals
define_data_vals <- function(default_data_vals, data_cols, IDs, REPs, rep_exists, groups=FALSE, expand=TRUE) {

  ### NAMED CONSTANTS ###
  obs_default_val_3 <- "VAL" # fill all data_cols 6 char or lower with this value
  obs_default_val_5 <- "VALUE" # fill all data_cols 7 - 9 char with this value
  obs_default_val_8 <- "DATA_VAL" # fill all data_cols 10 to 12 char with this value
  obs_default_val_11 <- "DATA__VALUE" # fill all data_cols 13+ char with this value
  obs_default_val_dt <- "INSERT__DATETIME" # this occupies 16 chars
  # same length as datetime String: 2021/02/26:18:35

  # use data_col_wds to calc correct length of default_data_vals:
  if( length(default_data_vals) == 0 ) { # default_data_vals is a BLANK LIST - so fill with default_data_vals

    # THEN check col rep exists - if so must copy the REPS to the new datatable
    # col rep is what is defined when a resampling generates many SECTIONS or REPS
    # its NOT the same as reps!
    # THIS ENSURES REPS ARE SUPPORTED IN ADD DATA TO SAMPLES!
    if( rep_exists == TRUE && groups == FALSE ) { # only add reps if the IDs are NOT groups
      # add rep col to data_cols to add!
      data_cols <- c("rep", data_cols)
    }

    # determine default widths of data cols
    data_col_wds <- c()
    for(i in 1:length(data_cols) ) {
      # if _dt col - must be 18 long to fit datetime: 2021/02/26:17:42
      if( data_cols[i] == "rep" ) {

        #if( summarise_reps == TRUE && rep_exists == TRUE ) { # summarising reps to r vector format
        # format stored in string - REPs
        # calc width based on REPs nchar!
        data_col_wds[i] <- pmax(nchar(data_cols[i])+4,
                                max(nchar(REPs)),
                                5)

        #} else {
        # need to calc width based on the widths of the CURRENT REP col in datatables
        #data_col_wds[i] <- pmax(nchar(data_cols[i])+4,
        #                       max(nchar(datatable[[ "rep" ]])),
        #                      5)
        #}
      } else if( endsWith(data_cols[i], "_dt") ) {
        data_col_wds[i] <- pmax(nchar(data_cols[i])+4, 18)
      } else {
        data_col_wds[i] <- pmax(nchar(data_cols[i])+4, 5) #pmax ensures min col width is 5!
      }
    }

    # COMPUTE DEFAULT DATA VALS
    for( i in 1:length(data_cols) ) {
      # add default data vals to the data col for each ID
      # first compute the most appropriate default data val to add - based on width of column
      if( data_cols[i] == "rep" ) {

        #if( summarise_reps == TRUE && rep_exists == TRUE ) { # summarising reps to r vector format
        # format stored in string - REPs
        default_data_vals[[i]] <- REPs

        #} else { # just use the rep col from datatable:
        # default_data_vals[[i]] <- datatable[[ "rep" ]]
        #}

      } else if( endsWith(data_cols[i], "_dt") ) {
        default_data_vals[[i]] <- rep(obs_default_val_dt, length(IDs) )
      } else if( data_col_wds[i] > 12 ) {
        default_data_vals[[i]] <- rep(obs_default_val_11, length(IDs) )
      } else if( data_col_wds[i] > 9 ) {
        default_data_vals[[i]] <- rep(obs_default_val_8, length(IDs) )
      } else if( data_col_wds[i] > 6 ) {
        default_data_vals[[i]] <- rep(obs_default_val_5, length(IDs) )
      } else {
        default_data_vals[[i]] <- rep(obs_default_val_3, length(IDs) )
      }
    }
  } else {

    # using default_data_vals as passed to this function
    # confirm the number of default_data_vals at LIST level is the same as data_cols
    if( length(default_data_vals) != length(data_cols)) {
      stop( paste0("  number of default data vals does not match data_cols length: ", default_data_vals, " ", data_cols) )
    }

    # THEN check col rep exists - if so must copy the REPS to the new datatable
    # col rep is what is defined when a resampling generates many SECTIONS or REPS
    # its NOT the same as reps!
    # THIS ENSURES REPS ARE SUPPORTED IN ADD DATA TO SAMPLES!
    if( rep_exists == TRUE ) {
      # add rep col to data_cols to add!
      data_cols <- c("rep", data_cols)
      # and add REPs to default_data_vals for rep
      ddv <- list(REPs)
      for(i in 1:length(default_data_vals) ) {
        ddv[[(i+1)]] <- default_data_vals[[i]]
      }
      default_data_vals <- ddv
    }

    # determine widths of data cols
    data_col_wds <- c()
    for(i in 1:length(data_cols) ) {
      data_col_wds[i] <- max(nchar(c(data_cols[[i]], default_data_vals[[i]]) ))+2
    }

    # now EXPAND default data values
    ddv <- list()
    for( i in 1:length(data_cols) ) {
      # add default data vals to the data col for each ID
      # first compute the most appropriate default data val to add - based on width of column
      if( data_cols[i] == "rep" ) {

        #if( summarise_reps == TRUE && rep_exists == TRUE ) { # summarising reps to r vector format
        # format stored in string - REPs
        ddv[[i]] <- REPs

        #} else { # just use the rep col from datatable:
        # default_data_vals[[i]] <- datatable[[ "rep" ]]
        #}

      } else {
        if( expand == TRUE ) {
          ddv[[i]] <- rep(default_data_vals[[i]], length(IDs) )
        } else { # do not expand by length of IDs
          ddv[[i]] <- default_data_vals[[i]]
        }
      }
    }
    default_data_vals <- ddv # default_data_vals should have correct number of reps using default vals passed to this function

    # finally to handle multi-obs data - split at any SPACE in default_data_vals
    # build_data : supports adding multi-obs data
    for( i in 1:length(data_cols) ) {
      default_data_vals[[i]] <- unlist(strsplit(default_data_vals[[i]], ' '))
    }

  }

  dv_list <- list(data_cols, data_col_wds, default_data_vals)
  names(dv_list) <- c("data_cols", "data_col_wds", "default_data_vals")

  # return
  dv_list

}


#' Define data_cols + wds + vals - based on data_cols, IDs, & REPs
#'
#' Staggers the values in groups along the length of IDs & REPs.
#'
#' @return named list containing MODIFIED data_cols, data_col_wds, default_data_vals
define_data_vals_groups <- function(datatable, groups, data_cols, IDs, REPs, rep_exists) {

  ### NAMED CONSTANTS ###
  obs_default_val_8 <- "DATA_VAL" # fill all data_cols 10 to 12 char with this value
  obs_default_val_11 <- "DATA__VALUE" # fill all data_cols 13+ char with this value
  obs_default_val_dt <- "INSERT__DATETIME" # this occupies 16 chars
  # same length as datetime String: 2021/02/26:18:35

  # THEN check col rep exists - if so must copy the REPS to the new datatable
  # col rep is what is defined when a resampling generates many SECTIONS or REPS
  # its NOT the same as reps!
  # THIS ENSURES REPS ARE SUPPORTED IN ADD GROUPS TO SAMPLES!
  if( any( names(datatable) == "rep" ) == TRUE ) {
    # add rep col to data_cols to add!
    data_cols <- c("rep", data_cols)
  }

  # determine default widths of data cols
  data_col_wds <- c()
  for(i in 1:length(data_cols) ) {
    # if _dt col - must be 18 long to fit datetime: 2021/02/26:17:42
    if( data_cols[i] == "rep" ) {

      #if( summarise_reps == TRUE && rep_exists == TRUE ) { # summarising reps to r vector format
      # format stored in string - REPs
      # calc width based on REPs nchar!
      data_col_wds[i] <- pmax(nchar(data_cols[i])+4,
                              max(nchar(REPs)),
                              5)

      #} else {
      # need to calc width based on the widths of the CURRENT REP col in datatables
      #data_col_wds[i] <- pmax(nchar(data_cols[i])+4,
      #                       max(nchar(datatables[[ datatable_name ]][[ "rep" ]])),
      #                      5)
      #}
    } else if( endsWith(data_cols[i], "_dt") ) {
      data_col_wds[i] <- pmax(nchar(data_cols[i])+4, 18)
    } else {
      data_col_wds[i] <- pmax(nchar(data_cols[i])+4, 5) #pmax ensures min col width is 5!
    }
  }


  # set the groups value to an appropriate list of vectors with default values
  if( groups[1] == "" ) {
    # use data_col_wds to calc correct length of default_data_vals:
    default_data_vals <- list()
    for( i in 1:length(data_cols) ) {
      # add default data vals to the data col for each ID
      # first compute the most appropriate default data val to add - based on width of column
      if( data_cols[i] == "rep" ) {
        default_data_vals[[i]] <- REPs

      } else if( data_col_wds[i] > 12 ) {
        default_data_vals[[i]] <- rep(obs_default_val_11, length(IDs) )
      } else {
        default_data_vals[[i]] <- rep(obs_default_val_8, length(IDs) )
      }
    }
    groups <- default_data_vals

  } else {
    # must ensure that each VECTOR in each List element of groups is of length(IDs)!
    for( i in 1:length(data_cols) ) {

      if( data_cols[i] == "rep" ) { # add reps to groups list at START!
        groups2 <- list()
        groups2[[1]] <- REPs
        for(q in 1:length(groups) ) {
          groups2[[(q+1)]] <- groups[[q]]
        }
        groups <- groups2

      } else { # need to duplicate out the groups vals to fill IDs length (also REPs length if it exists!)
        vec <- groups[[i]]
        groups[[i]] <- rep(vec, length(IDs))[1:length(IDs)]
      }
    }
  }

  dv_list <- list(data_cols, data_col_wds, groups)
  names(dv_list) <- c("data_cols", "data_col_wds", "default_data_vals")

  # return
  dv_list

} #### ________________________________ ####


#' Build Datatable
#'
#' Creates a new plaintext DataTable vector. The DataTable will contain an
#' initial ID column containing the `ID_col` vector, and an optional set of
#' extra data columns titled with the `data_cols` vector.  If the table exceeds
#' `dt_length` characters (default 100), then the table is split into multiple
#' tables, with IDs as first col, and subsequent data_cols given in subsequent
#' tables.
#'
#' This method supports adding Multi-Observations:  Multiple data points
#' for a given ID.  These are entered under the data_col and split across
#' lines next to an ID.  The length of each vector in `data` must be a multiple
#' of length of IDs.  If more than 1, the data is split between IDs in
#' data-first order.
#'
#' @param ID_col Character vector of ID col name.
#'
#' @param IDs Character vector of sample IDs
#'
#' @param data_cols Character vector of data table column titles
#'
#' @param data A list of vectors containing all the data to insert into the
#' datatable.  Length of list must match length of data_cols vector.
#' This method now supports adding multi-obs data.  Length of each
#' vector in data must be a MULTIPLE of IDs length.  If 2 or more multiples,
#' the data is added in a multi-obs set of rows for each ID: Added in data-first
#' order - so ID[1] gets data[1], data[2]; ID[2] gets data[3], data[4] etc.
#'
#' @param dt_function A string to set the FUNCTION of the datatable to be built.
#'
#' @param datatable_name String of data table name.
#'
#' @param MAX_DATATABLE_LENGTH Int of data table max length in characters.
#' Typically 100.
#'
#' @param DATATABLE_SPACER_CHAR Character used to underline column headers.
#' Default '='.
#'
#' @export
build_datatable <- function( ID_col, IDs, data_cols, data, dt_function,
                             datatable_name, MAX_DATATABLE_LENGTH,
                             DATATABLE_SPACER_CHAR="=" ) {


  #### datatable FUNCTION ####
  # dt_function_next defines the function of any FURTHER datatables
  # set to ADD_DATA for CREATE and ADD_DATA
  if(dt_function == "CREATE" || dt_function == "ADD_DATA") {
    dt_function_next <- "ADD_DATA"
  } else if( dt_function == "GROUP" ) {
    dt_function_next <- "GROUP" # create further group tables
  } else if( dt_function == "EXPORT" ) {
    dt_function_next <- "EXPORT" # create further export tables
  } else if( dt_function == "IMPORT" ) {
    dt_function_next <- "IMPORT" # create further import tables
  }

  # CHECKS

  # Check data_cols and data list lengths are equal
  if( length(data_cols) != length(data) ) {
    stop( paste0("  data and data_cols not same length: ", length(data), " ", length(data_cols) ) )
  }


  #### datatable col widths ####

  # FIRST determine widths of data cols
  # will calculate how many table are need FIRST
  # then can calc how many rows each of these tables needs to fit the data in
  # this is necessary to handle potential MULTI-OBSERVATIONS in data list

  # calc width of ID col
  ID_col_wd <- max(nchar( c(ID_col, IDs) )) + 2

  # calc length of row with IDs in it:
  col_spacers_len <- nchar(paste0("    ",
                                  strrep(DATATABLE_SPACER_CHAR, ID_col_wd),
                                  "  ") )
  # set initially to length of datatable row with IDs in it - increment as cols are added

  data_col_wds <- c()
  for(i in 1:length(data_cols) ) {
    # if _dt col - must be 18 long to fit datetime: 2021/02/26:17:42
    if( endsWith(data_cols[i], "_dt") ) {
      data_col_wds[i] <- pmax(
        nchar(data_cols[i])+4,
        max(nchar(unlist(strsplit(as.character(data[[i]]), ' ')))[ !is.na( nchar(unlist(strsplit(as.character(data[[i]]), ' '))) ) ])+4,
        18)
    } else {
      data_col_wds[i] <- pmax(
        nchar(data_cols[i])+4,
        max(nchar(unlist(strsplit(as.character(data[[i]]), ' ')))[ !is.na( nchar(unlist(strsplit(as.character(data[[i]]), ' '))) )])+4,
        5) # get max of data_col title, data contents, or 5 :  pmax ensures min col width is 5!
    }
  }

  # deal with case that any data_col_wds exceeds MAX_DATATABLE_LENGTH
  MAX_DATA_COL_LENGTH <- (MAX_DATATABLE_LENGTH - (4 + ID_col_wd + 2) ) # max minus the length of ID col
  for( i in 1:length(data_col_wds) ) {
    if( data_col_wds[i] > MAX_DATA_COL_LENGTH ) {
      # if this exceeds MAX data col length
      # then need to correct the data to split across multiple rows per ID
      # calculate the integer needed to divide the data & data_col_wd by to get it into range
      corr_fct <- as.integer(data_col_wds[i]/MAX_DATA_COL_LENGTH) + 1

      # first use corr_fct to correct the data_col_wds[i] value
      dcwdsO <- data_col_wds[i] # keep original for data splitting below
      data_col_wds[i] <- as.integer(data_col_wds[i]/corr_fct)


      # then use corr_fct to correct the data in data[[i]]
       # have to split ALL ENTRIES as length of data must be mutliple of ID_length
      da2 <- c()
      for( j in 1:length(data[[i]]) ) {
        da <- data[[i]][j]
        for( k in 1:corr_fct ) { # split into corr_fct number of pieces
          if(k==1) { # split string from start
            da2 <- c(da2, substr(da, 1, data_col_wds[i]))
          } else if(k<length(corr_fct)) { # split middle portions by k index
            da2 <- c(da2, substr(da, ((data_col_wds[i] * (k-1))+1), (data_col_wds[i] * k) ))
          } else { # split end portion up to the FULL LENGTH of the data string
            da2 <- c(da2, substr(da, ((data_col_wds[i] * (k-1))+1), nchar(da)) )
          }
        }
      }
      # replace data[[i]] with da2
      data[[i]] <- da2
    }
  }

  # Now - compute which cols will fit into tables
  # keep summing until row lengths exceed MAX_DATATABLE_LENGTH
  # THEN split the summing into a second list item
  row_length_list <- list(col_spacers_len)
  row_list_index <- 1
  initial_row_length <- col_spacers_len
  current_row_length <- col_spacers_len

  for( i in 1:length(data_col_wds) ) {

    current_row_length <- current_row_length + data_col_wds[i] + 2 # calc new row length - +2 for SPACER!

    if( current_row_length > MAX_DATATABLE_LENGTH ) {

      # if the row length is greater than MAX, SETUP the next list entry
      row_list_index <- row_list_index + 1
      row_length_list[[ row_list_index ]] <- c(initial_row_length)
      current_row_length <- col_spacers_len # reset current row length
      current_row_length <- current_row_length + data_col_wds[i] # calc new row length with resetted val
      # and add this new row_length to the list:
      row_length_list[[ row_list_index ]] <- c(row_length_list[[ row_list_index ]], current_row_length)

    } else {
      # just add the current_row_length to vector in row_length_list
      row_length_list[[ row_list_index ]] <- c(row_length_list[[ row_list_index ]], current_row_length)
    }
  }

  # now know how many datatables are needed - length(row_length_list)
  # AND can calculate how many ROWS are needed for each ID
  # as can determine in which table each data_col will be in


  #### datatable rows ####

  # calc number of LINES for each ID in EACH TABLE
  # data contains a list of vectors to add
  # if each vector are of lengths MULTIPLES of the IDs, then will be adding MUTLI-COLS to the table
  # if so, need to compute this NOW and factor this into the spacing of rows in table!
  data_length <- lapply(data, length)
  ID_length <- length(IDs)
  max_multiple_list <- list() # store each max_multiple for each table in a separate list element
  max_multiple <- 0 # start at 0 so max_multiple_list is always set to at least 1!
  multiple_list <- list() # also store each multiple value in a list of vectors
  multiple <- 1
  num_prev_data_cols <- 0

  for(a in 1:length(row_length_list) ) { # row_length_list list length - number of TABLES
    for(b in 2:length(row_length_list[[a]]) ) { # (2:length) num data cols in this table - as FIRST INDEX is always the length of IDS

      # data_length index calc'd from row_length_list as:
      # num_prev_data_cols : number of data cols parsed in previous for a loops
      # added to ad end of each for a loop with number of cols traversed
      # (b-1) as row_length_list ALWAYS starts with the ID length in each list element
      # BUT data_length does not contain ID length!

      # first CHECK that data_lengths are EXACT MULTIPLES of ID_length:
      if( (data_length[[ (num_prev_data_cols) + (b-1) ]] %% ID_length) != 0 ) {
        # if modulus is not 0, data_length is NOT an exact multiple of ID_length
        stop( paste0("  Number of data points added is not an exact multiple of IDs: ",
                     "\n  data length ", length(data[[a]]), " IDs length: ", length(IDs) ) )
      }

      # NOW calculate the MAX multiple - what is the max integer difference between data and IDs lengths?
      multiple <- data_length[[ (num_prev_data_cols) + (b-1) ]] / ID_length

      # add multiple to list:
      if(b == 2) { # when b is 2, multiple_list at index a is NULL - so just add multiple to it
        multiple_list[[a]] <- multiple
      }else { # otherwise CONCAT current list item with multiple:
        multiple_list[[a]] <- c(multiple_list[[a]], multiple)
      }

      # add multiple to max list IF exceeds previous max:
      if(multiple > max_multiple) {
        max_multiple_list[[a]] <- multiple # set max_multiple to current table index
        max_multiple <- multiple # and set max_multiple to keep track of the max in THIS ITERATION
      }

    }
    # at end of a:
    # keep track of index of data_cols:
    num_prev_data_cols <- num_prev_data_cols + (length(row_length_list[[a]]) -1)
    # and reset max_multiple:
    max_multiple <- 0
  }

  # cached data for each datatable in max_multiple_list and multiple_list
  # max_multiple_list returns the single LARGEST number of multiples for IDs data vals in a given table
  # multiple_list give a vector as each list element, with each number in the vector equal to the multiples for IDs in that data col


  #### datatable formation ####
  # form initial SKELETON of each table to insert
  # HEADER, IDs col (with correct spacing from max_multiple_list), FOOTER

  # create a list to hold the generated data_tables in
  data_table_list <- list()

  for( a in 1:length(max_multiple_list) ) { # length max_multiple_list give number of TABLES to create

    # add HEADER - header info plus datatable column heads
    if( a == 1 ) {
      data_table_list[[a]] <- c(
        datatable_get_delimiter(),
        "",
        "",
        paste0("    ",datatable_name,"  :  ", dt_function),
        "",
        "",
        paste0("    ",
               strrep(" ", ceiling( (ID_col_wd - nchar(ID_col))/2) ),
               ID_col,
               strrep(" ", floor( (ID_col_wd - nchar(ID_col))/2) ),
               "  " ), # Col Title : ID - with correct spacing
        paste0("    ",
               strrep(DATATABLE_SPACER_CHAR, ID_col_wd),
               "  " ), # Spacers under ID - '=' by default
        ""
      )
    } else { # fill with dt_function_next
      data_table_list[[a]] <- c(
        datatable_get_delimiter(),
        "",
        "",
        paste0("    ",datatable_name,"  :  ", dt_function_next),
        "",
        "",
        paste0("    ",
               strrep(" ", ceiling( (ID_col_wd - nchar(ID_col))/2) ),
               ID_col,
               strrep(" ", floor( (ID_col_wd - nchar(ID_col))/2) ),
               "  " ), # Col Title : ID - with correct spacing
        paste0("    ",
               strrep(DATATABLE_SPACER_CHAR, ID_col_wd),
               "  " ), # Spacers under ID - '=' by default
        ""
      )
    }

    cat( "\n  add IDs" )
    ids_spacer <- paste(replicate(col_spacers_len, " " ), collapse="" )
    data_table_ids <- replicate( (length(IDs)*(1+max_multiple_list[[a]][1])), ids_spacer ) # start with POPULATED vector
    # fill with SPACER equal to length of IDs
    # x(1+max_multiple) as enter the ID THEN number of blank lines in max_multiple to separate IDs in the table
    # gives correct spacing of IDs for adding multi observations in the data list
    for(i in 1:length(IDs) ) {

      # index must use max_multiple_list[[a]][1] to calc position of IDs
      data_table_ids[(i*(1+max_multiple_list[[a]][1]))-(max_multiple_list[[a]][1])] <- paste0(
        "    ",
        strrep(" ", ceiling( (ID_col_wd - nchar(IDs[i]))/2) ),
        IDs[i],
        strrep(" ", floor( (ID_col_wd - nchar(IDs[i]))/2) ),
        "  " ) # an ID - with correct spacing

    }


    # concat data_table and add FOOTER - whitespace plus end delimiter
    data_table_list[[a]] <- c(
      data_table_list[[a]],
      data_table_ids,
      "",
      datatable_get_delimiter(),
      ""    )

  }


  # Now add each data_col, according to the pre-defined number of cols calculated above in row_length_list

  # only add cols if there are any to add!
  if( data_cols[1] != "" ) {

    num_prev_data_cols <- 0 # keep track of data_cols index by counting data_cols traversed in for b loop
    row_length <- col_spacers_len   # initialise as INITIAL char count in each ID row

    for(a in 1:length(row_length_list) ) { # row_length_list list length - number of TABLES [a]
      for(b in 2:length(row_length_list[[a]]) ) { # (2:length) num data cols in this table  [b] - as FIRST INDEX is always the length of IDS

        i <- (num_prev_data_cols) + (b-1) # save current ID line index to i
        dt_index <- a # index of datatable is [a]!

        cat( "\n    add col: ", data_cols[i] )

        # first calc the number of chars to add to datatable
        row_length <- row_length + (data_col_wds[i]+2) #+2 for 2 spaces at end

        cat( "\n  data col::", data_cols[i], " index: ", i )
        cat( "\n    data_col_wds:", data_col_wds[i] )
        cat( "\n    col_spacers_len:", col_spacers_len )

        # add the data col and the default data vals for each ID

        # col title is index 7
        data_table_list[[dt_index]][7] <- paste0(

          data_table_list[[dt_index]][7], # keep what is already in this String!

          strrep(" ", ceiling( (data_col_wds[i] - nchar(data_cols[i]))/2) ),

          data_cols[i],

          strrep(" ", floor( (data_col_wds[i] - nchar(data_cols[i]))/2) ),

          "  " ) # Col Title : - with correct spacing


        # === SPACERS are index 8
        data_table_list[[dt_index]][8] <- paste0(

          data_table_list[[dt_index]][8], # keep what is already in this String!

          strrep(DATATABLE_SPACER_CHAR, data_col_wds[i]),

          "  ") # Spacers under data col - '=' by default


        # get number of chars to add to each row for IDs
        nchar_row <- nchar( paste0(

          strrep(DATATABLE_SPACER_CHAR, data_col_wds[i]),

          "  ") )

        # add blank lines to all the IDs and space rows
        for( j in 1:(length(IDs)*(max_multiple_list[[a]][1]+1) ) ) {
          # IDs start at index 10 : j+9
          data_table_list[[dt_index]][ j + 9 ] <- paste0(data_table_list[[dt_index]][ j + 9 ],
                                                         paste(replicate(nchar_row, " " ), collapse="" ) )
        }

        # add data to rows:
        l <- 1
        for( j in 1:length(IDs) ) {

          for( k in 1:multiple_list[[a]][(b-1)] ) { # multiple_list at [[a]][(b-1)] is the NUMBER OF MULTIPLES for this data_col!

            # adjust what is in the data_table at correct index:
            data_table_list[[dt_index]][ 9 + (j*(1+max_multiple_list[[a]][1])) - (max_multiple_list[[a]][1]) + (k-1) ] <- paste0(

              # keep what is already PREVIOUSLY in this String!
              # substr removes the extra blank lines added above
              substr(data_table_list[[dt_index]][ 9 + (j*(1+max_multiple_list[[a]][1])) - (max_multiple_list[[a]][1]) + (k-1) ],
                     1,
                     (nchar(data_table_list[[dt_index]][ 9 + (j*(1+max_multiple_list[[a]][1])) - (max_multiple_list[[a]][1]) ])-nchar_row) ),

              strrep(" ", ceiling( (data_col_wds[i] - nchar(as.character(data[[i]][l]) ))/2) ),

              as.character(data[[i]][l]),

              strrep(" ", floor( (data_col_wds[i] - nchar( as.character(data[[i]][l]) ))/2) ),

              "  " ) # a data entry at ID [9+(j*2)-1] - this gives ID line index : add correct spacing to line entry
            # count iterations in j and k with l
            #print( paste0("  l: ", l, "  data[[i]][l]: ", data[[i]][l]) )
            l <- l + 1
          }

        }

      }
      # at end of a:
      # keep track of index of data_cols:
      num_prev_data_cols <- num_prev_data_cols + (length(row_length_list[[a]]) -1)
      # Reset for new table: as INITIAL char count in each ID row
      row_length <- col_spacers_len
    }
  }

  # data_table_list has one or more data_tables with correct spacing, and width does not exceed MAX_DATATABLE_LENGTH

  # generate single vector containing all data tables:
  data_tables <- unlist(data_table_list)

  cat("\n\n")

  # return
  data_tables

}

#' Return the datatable delimiter
#'
#' String that indicates teh start and end of a datatable.  Using centralised
#' function to define this structure - for generation & for lookup.
datatable_get_delimiter <- function() {
  "+==============================================================================="
}



#' Return a datatable Rmd character vector from a tibble
#'
#' Assumes the tibble has initial col ID with all IDs in it. Spaces the
#' columns based on the col header and contents widths.
#'
#'
#' NOT CLEAR THIS FUNCTION HANDLES VARIABLE MULTI-ROWS CORRECTLY!
#'
#' Use `build_datatable_from_dataframe()`
#'
#' @param tb A tibble to parse into projectmanagr datatable format.
#'
#' @param datatable_name The name to set in the datatable.
#'
#' @param dt_function CREATE ADD_DATA RESAMPLE IMPORT EXPORT
#'
#' @export
build_datatable_from_tibble <- function(tb, datatable_name, dt_function="CREATE",
                                        dt_length=100, DATATABLE_SPACER_CHAR="=" ) {

  cat( "\nprojectmanagr::build_datatable_from_tibble():\n" )

  ### NAMED CONSTANTS ###
  MAX_DATATABLE_LENGTH <- dt_length

  # check tb has initial col called ID
  if( names(tb[1]) != "ID" ) {
    stop( paste0("  first col of tibble is not ID: ", names(tb[1]) ) )
  }

  # convert each col to char
  tb <- dplyr::mutate(tb, across(everything(), as.character))

  if( length(tb) == 1 ) {
    # add data same length as IDs, but no data cols
    data_tables <- build_datatable( "ID", tb[[1]], data_cols="", data=list(rep("VAL", length(tb[[1]])) ),
                     dt_function, datatable_name, MAX_DATATABLE_LENGTH,
                     DATATABLE_SPACER_CHAR )
  } else { # there is at least one extra data col - pass all into build_datatable

    # set data_cols
    data_cols <-  names(tb) # get all after ID
    data_cols <- data_cols[2:length(data_cols)]

    # extract data
    data <- list()
    for(dc in 1:length(data_cols)) {
      data[[dc]] <- unlist(strsplit(tb[[(dc+1)]], split=' '))
    }

    data_tables <- build_datatable( "ID", tb[[1]], data_cols, data, dt_function,
                     datatable_name, MAX_DATATABLE_LENGTH,
                     DATATABLE_SPACER_CHAR )

  }

  data_tables # return list of char vectors

}



#' Convert reps integer vector to R integer string
#'
#'
summarise_id_rep <- function(IDs, REPs, all_reps=FALSE) {

  # get unique IDs
  IDs_unique <- unique(IDs)

  if( all_reps == TRUE ) {
    # simply convert reps to "ALL" strings for each ID
    REPs_unique <- rep("ALL", length(IDs_unique))

  } else {

    REPs_unique <- c() # use to store the new reps string

    for(id in 1:length(IDs_unique) ) { # loop through each ID
      ID <- IDs_unique[id]
      # get and sort all reps for given ID
      REPv <- sort(as.integer(REPs[IDs %in% ID]))
      # form R integer string to summarise these reps
      REPs_unique <- c(REPs_unique, summarise_rep_v_str(REPv))
    }

  }

  # return as named list:
  lst <- list(IDs_unique, REPs_unique)
  names(lst) <- c("IDs", "REPs")
  lst

}


#' convert a sorted vector of integers to R integer string:
#'
#' eg. 1,2,3,5,6,7,9,10,11,12,14 -> 1:3,5:7,9:12,14
summarise_rep_v_str <- function(REPv) {

  # first assign the first integer to the string
  REPstr <- as.character(REPv[1])
  if(length(REPv) == 1) {
    return(REPstr)

  } else if(length(REPv) == 2) {
    # parse the end now
    if( (REPv[1]+1) == REPv[2] ) {
      REPstr <- paste0(REPstr, ":", REPv[2])
    } else {
      REPstr <- paste0(REPstr, ",", REPv[2])
    }

  } else {
    # loop through REPv
    last <- 0
    for(rv in 1:(length(REPv)-1) ) {
      # get current and next REPs
      cR <- REPv[rv]
      nR <- REPv[(rv+1)]
      if( (cR+1) == nR ) {
        if(rv == (length(REPv)-1) ) {
          REPstr <- paste0(REPstr, ":", nR)
        }
      } else {
        if(last != cR) {
          REPstr <- paste0(REPstr, ":", cR, ",", nR)
        } else {
          REPstr <- paste0(REPstr, ",", nR)
        }
        last <- nR
      }
    }
  }

  REPstr

}


#' Expand Rep Strings
#'
#' Expanding REPs in summarised form to a sequecne of integers.
#'
#' Example: "1:3,5,7:9" summary string of REPS should yield the following
#' integer sequence:
#' `1 2 3 5 7 8 9`
#'
expand_rep <- function(rep_str) {

  rep_s <- strsplit(rep_str, ",") # split at commas

  rep_int_list <- list() # blank list to store output

  for(r in 1:length(rep_s)) {
    rep_strs <- rep_s[[r]]
    if(rep_strs=="ALL") {# allow magic word ALL to parse unchanged
      rep_int <- "ALL"
    } else {
      rep_int <- integer()
      for(rs in 1:length(rep_strs) ){
        rstr <- rep_strs[rs]
        if( grepl(":", rstr, fixed=TRUE) ) { # handle: sequences
          first <- substr(rstr, 1, regexpr(":", rstr, fixed=TRUE)-1)
          last <- substr(rstr, regexpr(":", rstr, fixed=TRUE)+1, nchar(rstr))
          rep_int <- c(rep_int, seq(first,last))
        } else { # its just a single integer
          rep_int <- c(rep_int, as.integer(rstr))
        }
      }
    }
    rep_int_list[[r]] <- rep_int
  }
  rep_int_list # return
}



#' Check Divisions of sample IDs only in a datatable
#'
#' check and remove all samples IDs that no longer exist in datatable.  Sample
#' IDs are excluded if they have been resampled, disposed or exported in the
#' current datatable.
#'
#' This returns a character vector of sample IDs for use in generating a new
#' datatable consisting of only EXISTING IDs.
#'
#' @param df A dataframe derived from Rmd which contains sample IDs from which
#' a new datatable will be created.  This is screened to remove all IDs that
#' have been previously resampled, disposed, or exported.
#'
#' @return IDs vector.
#'
check_divisions_ids <- function( df ) {

  # define IDs
  IDs <- df$ID

  # check for each column - resample, disposed, exported, split
  # index all non NA values and REMOVE THESE FROM IDs
  keep_bool_vector <- rep(TRUE, length(IDs))

  # for each ID that is not NA in each col, set keep_bool_vector to FALSE

  if( any(names( df ) == "resample") ) {
    keep_bool_vector <- (is.na(df$resample) & keep_bool_vector)
  }

  if( any(names( df ) == "dispose") ) {
    keep_bool_vector <- (is.na(df$dispose) & keep_bool_vector)
  }

  if( any(names( df ) == "export") ) {
    keep_bool_vector <- (is.na(df$export) & keep_bool_vector)
  }

  # DEPRECATED
  #if( any(names( df ) == "split") ) {
  #  keep_bool_vector <- (is.na(df$split) & keep_bool_vector)
  #}

  # now keep_bool_vector is only TRUE where all above cols do not exist/are NA
  # i.e the sample IDs that should be kept!

  # So - filter the IDs with this boolean vector
  IDs <- IDs[keep_bool_vector]

  # return IDs
  IDs

}



#' Check Divisions of sample IDs & reps in a datatable
#'
#' check and remove all samples IDs & reps that no longer exist in datatable.
#' Sample IDs/reps are excluded if they have been resampled, disposed or
#' exported in the current datatable.
#'
#' This returns a list of two character vectors - IDs and REPs - for use in
#' generating a new datatable consisting of only EXISTING IDs/REPs.
#'
#' @param df A dataframe derived from Rmd which contains sample IDs/reps from
#' which a new datatable will be created.  This is screened to remove all
#' IDs/REPs that have been previously resampled, disposed, or exported.
#'
#' @return A list containing the IDs vector and REPs vector - all IDs/REPs that
#' still EXIST in the df.
#'
check_divisions_ids_reps <- function( df ) {

  # define IDs and REPs
  IDs <- df$ID
  REPs <- df$rep

  # check for each column - resample, disposed, exported, split
  # index all non NA values and REMOVE THESE FROM IDs and REPs
  keep_bool_vector <- rep(TRUE, length(IDs))

  # for each ID that is not NA in each col, set keep_bool_vector to FALSE
  if( any(names( df ) == "resample") ) {
    keep_bool_vector <- (is.na(df$resample) & keep_bool_vector)
  }

  if( any(names( df ) == "dispose") ) {
    keep_bool_vector <- (is.na(df$dispose) & keep_bool_vector)
  }

  if( any(names( df ) == "export") ) {
    keep_bool_vector <- (is.na(df$export) & keep_bool_vector)
  }

  # DEPRECATED
  #if( any(names( df ) == "split") ) {
  #  keep_bool_vector <- (is.na(df$split) & keep_bool_vector)
  #}

  # now keep_bool_vector is only TRUE where all above cols do not exist/are NA
  # i.e the sample IDs/REPs that should be kept!

  # So - filter the IDs and REPs with this boolean vector
  IDs <- IDs[keep_bool_vector]
  REPs <- REPs[keep_bool_vector]

  # return as named list:
  lst <- list(IDs, REPs)
  names(lst) <- c("IDs", "REPs")
  lst

}


# check datatable_name exists in datatables, and ID col exists in this datatable.
check_dt_name_id <- function(datatables, datatable_name) {

  # Check datatable_name EXISTS in rmd_contents!
  if( any(names(datatables) == datatable_name) == FALSE ) {
    # no datatable of name datatable_name to add data to - STOP
    stop( paste0("  No datatable of this name exists in contents: ", datatable_name ) )
  }

  # ?? THEN check col ID exists ??:
  if( any( names( datatables[[ datatable_name ]] ) == "ID" ) == FALSE ) {
    stop( paste0("  Column ID missing from datatable: ", datatable_name ) )
  }
} #### ________________________________ ####





