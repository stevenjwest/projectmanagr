
#' Create new Sample DataTable from TEMPLATE and insert into Rmd
#'
#' Creates a Sample DataTable in specified Rmd file from the template that
#' should be present between specified start & end line.
#'
#' 1. The template Sample DataTable will contain an initial ID column containing
#' the default `<<IDS>>` vector - which will be replaced with IDs from selected
#' existing datatable.  Can use this syntax to create a new Datatable using IDS
#' defined in existing table, with different PREFIX or SUFFIX strings.
#'
#' 2. All other columns will have their initial data value replicated along the
#' length of the datatable - length of IDs.
#'
#' @param rmd_path path to Rmd file
#'
#' @param rmd_line Selected line in Rmd file within TEMPLATE datatable.
#'
#' @param datatable_name Name of an EXISTING datatable to source IDs from: these
#' will replace the `<<IDS>>` String in the TEMPLATE datatable with all IDs.
#'
#' @param template_datatable_name Name the template dtaatable will take when created.
#'
#' @param all_ids Boolean to indicate whether all IDs from `source_daatable_name`
#' should be returned: ignoring any that are DISPOSED EXPORTED or RESAMPLED.
#' FALSE by default - only returning IDs that are NOT DISPOSED EXPORTED or RESAMPLED.
#'
#' @param dt_length Int of data table max length in characters - default 100.
#'
#' Missing param `expand` not required - never want to expand the values in the
#' template datatable, as purpose of template is to specify all values from one
#' line indicated by `<<IDS>>` variable!
#' Variable `expand` : Whether to expand any values in vectors set to `default_data_vals`,
#' default is FALSE, meaning custom values can be set in `default_data_vals` for
#' each ID. If TRUE, teh vector is expanded by the length of IDs, meaning ALL
#' values in each vector are set to EACH ID, added as a multi-obs set of rows
#' for each ID.
#'
#'
#' @export
datatable_create_template_rmd <- function(rmd_path, rmd_line, datatable_name,
                                          template_datatable_name, IDs=NULL,
                                          all_ids=FALSE, dt_length=100) {

  cat( "\nprojectmanagr::datatable_create_template_rmd():\n" )

  # confirm rmd_path : in org && Project Doc or Note (subdir of programme) && absolute + normalised
  rmd_path <- confirm_rmd_path(rmd_path)

  cat( "  Reading Rmd...\n" )
  rmd_contents <- read_file(rmd_path)


  #### extract template data ####

  if( is.null(datatable_name) ) {
    template_indices <- check_template_indices(rmd_contents, rmd_line)
    ic_list <- define_template_data_IDs(rmd_contents, rmd_line,
                                    template_indices, IDs)
    # returns named list : data_cols, IDs, default_data_vals

  } else {
    template_indices <- check_template_indices(rmd_contents, rmd_line)
    ic_list <- define_template_data(rmd_contents, rmd_line,
                                    template_indices, datatable_name,
                                    all_ids)
    # returns named list : data_cols, IDs, default_data_vals
  }


  #### datatable create ####

  data_tables <- datatable_create(
    IDs = ic_list$IDs,
    data_cols = ic_list$data_cols,
    datatable_name = template_datatable_name,
    default_data_vals = ic_list$default_data_vals,
    dt_length = dt_length,
    expand = FALSE
  )

  # insert to replace template table
  rmd_contents <- insert_dt_to_contents(data_tables, rmd_contents,
                                        template_indices[1], template_indices[2])

  write_file(rmd_contents, rmd_path)

}


#' Generate DISPOSE Data Table and insert into Rmd
#'
#' This generates a new datatable that disposes samples - indicates they no
#' longer exist.
#'
#' Datatable comprises the dataframe NAME, sample IDs, reps (if applicable), and
#' the special column titled `dispose`.  In this column the datetime of disposal
#' (i.e. time of creation of dispose datatable) is written.
#'
#' @param rmd_path path to Rmd file.
#'
#' @param rmd_line line in Rmd file to insert table.
#'
#' @param datatable_name String of data table name: This data table MUST ALREADY
#' EXIST IN `contents`.  The disposing will by default include all sample IDs
#' from this datatable.  The user may then delete sample IDs as appropriate.
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
#' param `cdt` is not passed - as it should be in the TEMPLATE datatable under the
#' `dispose` data column. s Datetime to insert into dispose column. Defaults to the
#' current datetime string retrieved from `get_datetime()`
#'
#' @export
datatable_dispose_template_rmd <- function(rmd_path, rmd_line,
                                           datatable_name="samples", dt_length=100,
                                           summarise_reps=FALSE, all_reps=FALSE) {

  cat( "\nprojectmanagr::datatable_dispose_template_rmd():\n" )

  # confirm rmd_path : in org && Project Doc or Note (subdir of programme) && absolute + normalised
  rmd_path <- confirm_rmd_path(rmd_path)

  cat( "  Reading Rmd...\n" )
  rmd_contents <- read_file(rmd_path)


  #### extract template data ####

  template_indices <- check_template_indices(rmd_contents, rmd_line)
  ic_list <- define_template_data(rmd_contents, rmd_line,
                                  template_indices, datatable_name,
                                  all_ids=FALSE, expand=FALSE) # only fill with EXISTING IDs & do not expand data values
  # returns named list : data_cols, IDs, default_data_vals

  # confirm template datatable is in correct format
  if( length(ic_list$data_cols) != 1 || ic_list$data_cols[1] != "dispose" ) {
    stop(paste0("  DISPOSE template datatable does not contain correct columns: ", datatable_name))
  }

  ddv <- split_multi_obs_data(ic_list$default_data_vals)
  ddv <- lapply(ddv, unlist) # remove inner list
  if( length(ddv[[1]]) != 1 ) {
    stop(paste0("  DISPOSE template datatable does not support multi values in dispose col: ",
                paste(ddv[[1]], collapse =' '), "\n    datatable: ", datatable_name))
  }

  #### datatable dispose ####

  data_tables <- datatable_dispose(
    contents = rmd_contents[1:rmd_line],
    datatable_name = datatable_name,
    dt_length = dt_length,
    summarise_reps = summarise_reps,
    all_reps = all_reps,
    cdt=ic_list$default_data_vals[[1]][1]  )

  # insert to replace template table
  rmd_contents <- insert_dt_to_contents(data_tables, rmd_contents,
                                        template_indices[1], template_indices[2])

  write_file(rmd_contents, rmd_path)

}


#' Generate RESAMPLE Data Table and insert into Rmd
#'
#' This generates a new datatable that resamples samples - indicates they no
#' longer exist.
#'
#' Datatable comprises the dataframe NAME, sample IDs, reps (if applicable), and
#' the special column titled `dispose`.  In this column the datetime of disposal
#' (i.e. time of creation of dispose datatable) is written.
#'
#' @param rmd_path path to Rmd file.
#'
#' @param rmd_line line in Rmd file to insert table.
#'
#' @param datatable_name String of data table name: This data table MUST ALREADY
#' EXIST IN `contents`.  The resampling will by default include all sample IDs
#' from this datatable.  The user may then delete sample IDs as appropriate.
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
#'
#' param `resample_vector` is not passed A character vector that contains all the resample
#' codes to be added to each sample/rep in the resample table.
#'
#' param `rep_vector` is not passed: Integer vector of length 1 or `resample_vector`, that
#' specifies how many reps each `resample_vector` code will generate.
#'
#'  Values for these parameters should be in the TEMPLATE datatable under the
#' `resample` & `rep` data columns
#'
#' @export
datatable_resample_template_rmd <- function(rmd_path, rmd_line,
                                           datatable_name="samples", dt_length=100,
                                           summarise_reps=FALSE, all_reps=FALSE) {

  cat( "\nprojectmanagr::datatable_resample_template_rmd():\n" )

  # confirm rmd_path : in org && Project Doc or Note (subdir of programme) && absolute + normalised
  rmd_path <- confirm_rmd_path(rmd_path)

  cat( "  Reading Rmd...\n" )
  rmd_contents <- read_file(rmd_path)


  #### extract template data ####

  template_indices <- check_template_indices(rmd_contents, rmd_line)
  ic_list <- define_template_data(rmd_contents, rmd_line,
                                  template_indices, datatable_name,
                                  all_ids=FALSE, expand=FALSE) # only fill with EXISTING IDs & do not expand data values
  # returns named list : data_cols, IDs, default_data_vals

  # confirm template datatable is in correct format
  if( length(ic_list$data_cols) != 2 || ic_list$data_cols[1] != "resample" || ic_list$data_cols[1] != "resample" ) {
    stop(paste0("  RESAMPLE template datatable does not contain correct columns: ", datatable_name))
  }

  ddv <- split_multi_obs_data(ic_list$default_data_vals)
  ddv <- lapply(ddv, unlist) # remove inner list
  if( length(ddv[[1]]) != length(ddv[[2]]) ) {
    stop(paste0("  RESAMPLE template datatable columns must contain same number of data lines: ",
                paste("\n    resample: ", paste(ic_list$default_data_vals[[1]], collapse=' ')),
                paste("\n    reps: ", paste(ic_list$default_data_vals[[2]], collapse=' ')),
                "\n    datatable: ", datatable_name))
  }

  #### datatable_resample ####

  data_tables <- datatable_resample(
    contents = rmd_contents[1:rmd_line],
    datatable_name = datatable_name,
    resample_vector = ic_list$default_data_vals[[1]],
    rep_vector = ic_list$default_data_vals[[2]],
    dt_length = dt_length,
    summarise_reps = summarise_reps,
    all_reps = all_reps )

  # insert to replace template table
  rmd_contents <- insert_dt_to_contents(data_tables, rmd_contents,
                                        template_indices[1], template_indices[2])

  write_file(rmd_contents, rmd_path)

}


#' Generate RESAMPLE Data Table and insert into Rmd
#'
#' This generates a new datatable that resamples samples - indicates they no
#' longer exist.
#'
#' Datatable comprises the dataframe NAME, sample IDs, reps (if applicable), and
#' the special column titled `dispose`.  In this column the datetime of disposal
#' (i.e. time of creation of dispose datatable) is written.
#'
#' @param rmd_path path to Rmd file.
#'
#' @param rmd_line line in Rmd file to insert table.
#'
#' @param datatable_name String of data table name: This data table MUST ALREADY
#' EXIST IN `contents`.  The resampling will by default include all sample IDs
#' from this datatable.  The user may then delete sample IDs as appropriate.
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
#'
#' param `group_names` Character vector of data table column titles to add.
#' CANNOT BE BLANK!  MUST start all group names with `group`.
#'
#' param `groups` A LIST OF VECTORS containing the group labels for each group
#' defined in group_names. Must be the same length as `group_names`.
#'
#'  Values for these parameters should be in the TEMPLATE datatable as the
#' data COL NAMES &  data col VALUES
#'
#' @export
datatable_add_group_template_rmd <- function(rmd_path, rmd_line,
                                            datatable_name="samples", dt_length=100,
                                            summarise_reps=FALSE, all_reps=FALSE) {

  cat( "\nprojectmanagr::datatable_add_group_template_rmd():\n" )

  # confirm rmd_path : in org && Project Doc or Note (subdir of programme) && absolute + normalised
  rmd_path <- confirm_rmd_path(rmd_path)

  cat( "  Reading Rmd...\n" )
  rmd_contents <- read_file(rmd_path)


  #### extract template data ####

  template_indices <- check_template_indices(rmd_contents, rmd_line)
  ic_list <- define_template_data(rmd_contents, rmd_line,
                                  template_indices, datatable_name,
                                  all_ids=FALSE, expand=FALSE) # only fill with EXISTING IDs & do not expand data values
  # returns named list : data_cols, IDs, default_data_vals

  ddv <- split_multi_obs_data(ic_list$default_data_vals)
  ddv <- lapply(ddv, unlist) # remove inner list

  # confirm template datatable is in correct format
  if( any(startsWith(ic_list$data_cols, "group") == FALSE) ) {
    stop(paste0("  ADD GROUP template datatable can only add data to group column names - prefixed with string group : ",
                ic_list$data_cols[startsWith(ic_list$data_cols, "group") == FALSE]))
  }


  #### datatable_add_group ####

  data_tables <- datatable_add_group(
    contents = rmd_contents[1:rmd_line],
    group_names = ic_list$data_cols,
    datatable_name = datatable_name,
    groups = ddv,
    dt_length = dt_length,
    summarise_reps = summarise_reps,
    all_reps = all_reps )

  # insert to replace template table
  rmd_contents <- insert_dt_to_contents(data_tables, rmd_contents,
                                        template_indices[1], template_indices[2])

  write_file(rmd_contents, rmd_path)

}


#' Add Data to Sample DataTable from TEMPLATE and insert into Rmd
#'
#' Creates a Sample DataTable in specified Rmd file from the template that
#' should be present between specified start & end line.
#'
#' The template Sample DataTable will contain an initial ID column containing
#' a default IDs vector (which will be replaced with IDs from selected existing
#' datatable), and any other columns will have their initial data
#' value replicated along the length of the datatable.
#'
#' @param rmd_path path to Rmd file
#'
#' @param rmd_line line in Rmd file inside the TEMPLATE datatable
#'
#' @param IDs Character vector of sample IDs
#'
#' @param datatable_name String of data table name: IDs are sourced from this
#' datatable, and is used as the name of the newly created datatable.
#'
#' @param dt_length Int of data table max length in characters - default 100.
#'
#' @param summarise_reps Whether to summarise reps of samples in the new datatable.
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
#' @export
datatable_add_data_samples_template_rmd <- function( rmd_path, rmd_line,
                                                     datatable_name="samples",
                                                     dt_length = 100, summarise_reps = FALSE,
                                                     all_reps = FALSE ) {

  cat( "\nprojectmanagr::datatable_add_data_samples_template_rmd():\n" )

  # confirm rmd_path : in org && Project Doc or Note (subdir of programme) && absolute + normalised
  rmd_path <- confirm_rmd_path(rmd_path)

  cat( "  Reading Rmd...\n" )
  rmd_contents <- read_file(rmd_path)


  #### extract template data ####

  template_indices <- check_template_indices(rmd_contents, rmd_line)
  ic_list <- define_template_data(rmd_contents, rmd_line,
                                  template_indices, datatable_name,
                                  all_ids=FALSE, expand=FALSE) # only fill with EXISTING IDs & do not expand data values
  # returns named list : data_cols, IDs, default_data_vals


  #### datatable add data samples ####

  data_tables <- datatable_add_data_samples(
    contents = rmd_contents[1:rmd_line],
    data_cols = ic_list$data_cols,
    datatable_name = datatable_name,
    ids_vector = ic_list$IDs,
    default_data_vals = ic_list$default_data_vals,
    dt_length = dt_length,
    summarise_reps = summarise_reps,
    all_reps = all_reps
  )

  # insert to replace template table
  rmd_contents <- insert_dt_to_contents(data_tables, rmd_contents,
                                        template_indices[1], template_indices[2])

  write_file(rmd_contents, rmd_path)

}



#' define template data with manual IDs
#'
#' @param expand Whether to expand any values in vectors set to `default_data_vals`,
#' default is FALSE, meaning custom values can be set in `default_data_vals` for
#' each ID. If TRUE, teh vector is expanded by the length of IDs, meaning ALL
#' values in each vector are set to EACH ID, added as a multi-obs set of rows
#' for each ID.
#'
#' @returns named list : data_cols, IDs, default_data_vals
define_template_data_IDs <- function(rmd_contents, rmd_line, template_indices,
                                     IDs, all_ids=FALSE, expand=TRUE) {

  template_dt_vector <- rmd_contents[template_indices[1]:template_indices[2]]
  check_template_datatable_validity(template_dt_vector, rmd_line)
  template_data <- datatable_extract(template_dt_vector)

  ic_list <- define_template_ids_cols(template_data, IDs, rmd_line, all_ids, expand, datatable_name)

  ic_list # return

}

#' define template data
#'
#' @param all_ids Boolean to indicate whether all IDs from `source_daatable_name`
#' should be returned: ignoring any that are DISPOSED EXPORTED or RESAMPLED. When
#' creating a new CREATE datatable from existing set of IDs, may want to include
#' all IDs independent of whether DISPOSED/EXPORTED/RESAMPLED.  Setting `all_ids`
#' to TRUE ensures all ID Strings are extracted to be written to the new datatable.
#'
#' @param expand Whether to expand any values in vectors set to `default_data_vals`,
#' default is FALSE, meaning custom values can be set in `default_data_vals` for
#' each ID. If TRUE, teh vector is expanded by the length of IDs, meaning ALL
#' values in each vector are set to EACH ID, added as a multi-obs set of rows
#' for each ID.
#'
#' @returns named list : data_cols, IDs, default_data_vals
define_template_data <- function(rmd_contents, rmd_line, template_indices,
                                 datatable_name, all_ids, expand=TRUE) {

  template_dt_vector <- rmd_contents[template_indices[1]:template_indices[2]]
  check_template_datatable_validity(template_dt_vector, rmd_line)
  template_data <- datatable_extract(template_dt_vector)

  source_table <- extract_source_table(rmd_contents, datatable_name, rmd_line)

  ic_list <- extract_template_ids_cols(template_data, source_table, rmd_line, all_ids, expand, datatable_name)

  ic_list # return

}


# #' Extract Template Data from Vector
# #'
# #' Checks rmd_line selection is inside a TEMPLATE datatable in rmd_contents.
# extract_template_data <- function(rmd_contents, rmd_line, template_indices) {
#
#   template_dt_vector <- rmd_contents[template_indices[1]:template_indices[2]]
#
#   check_template_datatable_validity(template_dt_vector, rmd_line)
#
#   datatable_extract(template_dt_vector)
#
#}


#' Check Template Indices
#'
#'
check_template_indices <- function(rmd_contents, rmd_line) {

  # identify the lines before selection that begin with : datatable_get_delimiter()
  indices1 <- which( startsWith(rmd_contents[1:rmd_line], datatable_get_delimiter()) )
  # lines after
  indices2 <- which( startsWith(rmd_contents[(rmd_line+1):length(rmd_contents)], datatable_get_delimiter()) )

  #if( length(indices1) < 2 ) { # no existing datatables to extract IDs from! STOP
  #  stop(paste0("  No datatables exist in RMD to extract IDs from - selection: ", rmd_line))
  #}
  if( length(indices2) == 0 ) { # no further datatable delimiter to define end of TEMPLATE
    stop(paste0("  No datatable delimiter to define TEMPLATE datatable: ", rmd_line))
  }

  c(indices1[length(indices1)], ((rmd_line)+indices2[1]))
}


check_template_datatable_validity <- function(template_dt_vector, rmd_line) {

  table_name <- extract_table_name(template_dt_vector)
  table_function <- extract_table_function(template_dt_vector)

  if( table_name != "TEMPLATE" ) { # stop
    stop(paste0("  Selected datatable is not a TEMPLATE - selection: ", rmd_line))
  }

  check_datatable_validity(template_dt_vector, rmd_line)

}


extract_source_table <- function(rmd_contents, datatable_name, rmd_line) {

  dts <- datatable_read_vector(rmd_contents)

  if( all( !(names(dts) == datatable_name) ) ) { # no table with name
    stop(paste0("  No source datatable with name: ", datatable_name, " - selection: ", rmd_line))
  }
  dts[[datatable_name]] # return
}


#' Exract IDS and Cols
#'
#' @param all_ids Boolean to indicate whether all IDs from `source_daatable_name`
#' should be returned: ignoring any that are DISPOSED EXPORTED or RESAMPLED.
#' @param dt_length Int of data table max length in characters - default 100.
#'
#' @param expand Whether to expand any values in vectors set to `default_data_vals`,
#' default is FALSE, meaning custom values can be set in `default_data_vals` for
#' each ID. If TRUE, teh vector is expanded by the length of IDs, meaning ALL
#' values in each vector are set to EACH ID, added as a multi-obs set of rows
#' for each ID.
#'
extract_template_ids_cols <- function(template_data, source_table, rmd_line, all_ids, expand, datatable_name) {

  if(all_ids == TRUE) {
    ids <- source_table$ID
  } else {
    ids <- check_divisions_ids(source_table)
  }
  if( length(ids) == 0 ) { # all ids already defined - so FAIL with sensible error message
    stop( paste0("  All IDs already defined in existing datatables of this name: ", datatable_name,
                 "\n    selection: ", rmd_line) )
  }


  # ids from template - should include special syntax <<IDS>>
  template_ids <- template_data[[1]][2:length(template_data[[1]])]

  if( any( !(grepl("<<IDS>>", template_ids)) ) ) { # invalid syntax
    stop(paste0("  Invalid ID syntax in TEMPLATE datatable: ",
                paste(template_ids[!(grepl("<<IDS>>", template_ids))], collapse=' '),
                "\n    selection line: ", rmd_line))
  }

  # expand template_ids with ids
  IDs <- unlist(lapply(ids, function(x) sub("<<IDS>>", x, template_ids, fixed=TRUE)))

  # extract the data_col names and values from template_data
  data_cols <- unlist(lapply(template_data[2:length(template_data)], function(l) l[[1]]))
  dcv <- lapply(template_data[2:length(template_data)], function(l) l[2:length(l)])

  if( expand == TRUE ) {
    default_data_vals <- lapply(dcv, function(x) rep(x, length(ids)))
  } else {
    default_data_vals <- dcv
  }

  ic_list <- list(data_cols, IDs, default_data_vals)
  names(ic_list) <- c("data_cols", "IDs", "default_data_vals")
  ic_list # return

}


#' Exract IDS and Cols
#'
#' @param all_ids Boolean to indicate whether all IDs from `source_daatable_name`
#' should be returned: ignoring any that are DISPOSED EXPORTED or RESAMPLED.
#' @param dt_length Int of data table max length in characters - default 100.
#'
#' @param expand Whether to expand any values in vectors set to `default_data_vals`,
#' default is FALSE, meaning custom values can be set in `default_data_vals` for
#' each ID. If TRUE, teh vector is expanded by the length of IDs, meaning ALL
#' values in each vector are set to EACH ID, added as a multi-obs set of rows
#' for each ID.
#'
define_template_ids_cols <- function(template_data, IDs, rmd_line, all_ids, expand, datatable_name) {

  ids <- IDs

  # ids from template - should include special syntax <<IDS>>
  template_ids <- template_data[[1]][2:length(template_data[[1]])]

  if( any( !(grepl("<<IDS>>", template_ids)) ) ) { # invalid syntax
    stop(paste0("  Invalid ID syntax in TEMPLATE datatable: ",
                paste(template_ids[!(grepl("<<IDS>>", template_ids))], collapse=' '),
                "\n    selection line: ", rmd_line))
  }

  # expand template_ids with ids
  IDs <- unlist(lapply(ids, function(x) sub("<<IDS>>", x, template_ids, fixed=TRUE)))

  # extract the data_col names and values from template_data
  data_cols <- unlist(lapply(template_data[2:length(template_data)], function(l) l[[1]]))
  dcv <- lapply(template_data[2:length(template_data)], function(l) l[2:length(l)])

  if( expand == TRUE ) {
    default_data_vals <- lapply(dcv, function(x) rep(x, length(ids)))
  } else {
    default_data_vals <- dcv
  }

  ic_list <- list(data_cols, IDs, default_data_vals)
  names(ic_list) <- c("data_cols", "IDs", "default_data_vals")
  ic_list # return

}




