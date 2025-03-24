
#' Generate new CREATE Data Table and insert into Rmd
#'
#' Creates a new Sample DataTable in specified Rmd file at specified line.
#' The Sample DataTable will contain an initial ID column containing the
#' IDs vector, and an optional set of extra data columns as specified in the
#' data_cols vector.  If the table exceeds dt_length characters (default 100),
#' then the table is split into multiple tables, with IDs as first col, and
#' subsequent data_cols given in subsequent tables.
#'
#' @param rmd_path path to Rmd file.
#'
#' @param rmd_line line in Rmd file to insert table.
#'
#' @param IDs Character vector of sample IDs.
#'
#' @param data_cols Character vector of data table column titles.
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
#' @export
datatable_create_rmd <- function( rmd_path, rmd_line, datatable_name="samples",
                                  data_cols="", IDs="", default_data_vals=list(),
                                  dt_length = 100, expand=FALSE ) {

  cat( "\nprojectmanagr::datatable_create_rmd():\n" )

  # confirm rmd_path : in org && Project Doc or Note (subdir of programme) && absolute + normalised
  rmd_path <- confirm_rmd_path(rmd_path)

  cat( "  Reading Rmd...\n" )
  rmd_contents <- read_file(rmd_path)

  #### datatable create ####
  data_tables <- datatable_create(
    IDs = IDs,
    data_cols = data_cols,
    datatable_name = datatable_name,
    default_data_vals = default_data_vals,
    dt_length = dt_length,
    expand = expand )

  rmd_contents <- insert_dt_to_contents(data_tables, rmd_contents, rmd_line, rmd_line)
   # start & end are same line

  write_file(rmd_contents, rmd_path)

}




#' Insert a datatable from a dataframe
#'
#' Will insert the passed dataframe, using the variable's NAME as the datatable
#' NAME.  Will CREATE the datatable first, then subsequent cols that dont fit in
#' the first datatable will be in subsequent ADD_DATA datatables.
#'
#' Inserts the datatable where the CURSOR IS in the Active Rmd.
#'
#' @param df a dataframe/tibble with data to insert into Rmd
#'
#' @param dt_function String to declare type of table: MUST be CREATE ADD_DATA
#' RESAMPLE GROUP EXPORT or IMPORT. Default is CREATE.
#'
#' @param dt_length Int that dictates the maximum length of any one inserted datatable.
#'
#' @export
datatable_insert_from_tibble <- function(rmd_path, rmd_line, tb, dt_name,
                                         dt_function = "CREATE", dt_length = 100) {

  cat( "\nprojectmanagr::datatable_insert_from_tibble():\n" )

  # confirm rmd_path : in org && Project Doc or Note (subdir of programme) && absolute + normalised
  rmd_path <- confirm_rmd_path(rmd_path)

  cat( "  Reading Rmd...\n" )
  rmd_contents <- read_file(rmd_path)

  data_tables <- build_datatable_from_tibble(tb, dt_name, dt_function, dt_length)

  # write these to the file:
  cat( "\n  write data table(s) to Rmd at line: ", rmd_line )
  rmd_contents <- insert_dt_to_contents(data_tables, rmd_contents, rmd_line, rmd_line)

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
#' EXIST IN `contents`.  The resampling will be default include all sample IDs
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
#' @param cdt Current Datetime to insert into dispose column. Defaults to the
#' current datetime string retrieved from `get_datetime()`
#'
#' @export
datatable_dispose_rmd <- function( rmd_path, rmd_line, datatable_name,
                                   dt_length=100, summarise_reps=TRUE, all_reps=FALSE,
                                   cdt=get_datetime() ) {

  cat( "\nprojectmanagr::datatable_dispose_rmd():\n" )

  # confirm rmd_path : in org && Project Doc or Note (subdir of programme) && absolute + normalised
  rmd_path <- confirm_rmd_path(rmd_path)

  cat( "  Reading Rmd...\n" )
  rmd_contents <- read_file(rmd_path)

  #### datatable_dispose ####

  data_tables <- datatable_dispose(
    contents = rmd_contents[1:rmd_line],
    datatable_name = datatable_name,
    dt_length = dt_length,
    summarise_reps = summarise_reps,
    all_reps = all_reps,
    cdt=cdt  )

  rmd_contents <- insert_dt_to_contents(data_tables, rmd_contents, rmd_line, rmd_line)
  # start & end are same line

  write_file(rmd_contents, rmd_path)

}





#' Generate RESAMPLE Data Table and insert into Rmd
#'
#' Creates a resample datatable consisting of: sample ID column containing all
#' EXISTING sample IDs, sample reps column (if applicable) containing all EXISTING
#' sample reps, and then the resample and rep columns, which represent the
#' resampling.
#'
#' The function assumes all EXISTING samples and reps from selected datatable are
#' resampled: ALL are added to the output resample datatable.
#'
#' The User is free to DELETE sample IDs and reps from the resample table, and
#' the removed samples will no longer be resampled when read into tibbles.
#'
#' This functionality means the user can resample an INITIAL SUBSET of samples
#' in an experiment (perhaps as a pilot - for example resampling a block of CNS at
#' different thicknesses 100um 200um 400um 500um etc), and then later to resample
#' the remaining samples in the experiment (eg. now the pilot shows 200um is the
#' best thickness, all other samples are resampled to 200um thick sections).
#'
#' These two resampling events can be documented in two separate resample tables,
#' and these will be collated into a single resample/rep columns in the final
#' compiled datatable tibble.
#'
#' @param rmd_path path to Rmd file
#'
#' @param rmd_line line in Rmd file to insert table
#'
#' @param datatable_name String of data table name - MUST exist in rmd_path!
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
#' @export
datatable_resample_rmd <- function( rmd_path, rmd_line, datatable_name,
                                    resample_vector, rep_vector=c(1), dt_length=100,
                                    summarise_reps=TRUE, all_reps=FALSE ) {

  cat( "\nprojectmanagr::datatable_resample_rmd():\n" )

  # confirm rmd_path : in org && Project Doc or Note (subdir of programme) && absolute + normalised
  rmd_path <- confirm_rmd_path(rmd_path)

  cat( "  Reading Rmd...\n" )
  rmd_contents <- read_file(rmd_path)

  #### datatable_resample ####

  data_tables <- datatable_resample(
    contents = rmd_contents[1:rmd_line],
    datatable_name = datatable_name,
    resample_vector = resample_vector,
    rep_vector = rep_vector,
    dt_length = dt_length,
    summarise_reps = summarise_reps,
    all_reps = all_reps )

  # write these to the file:
  cat( "\n  write data table(s) to Rmd at line: ", rmd_line )
  rmd_contents <- insert_dt_to_contents(data_tables, rmd_contents, rmd_line, rmd_line)
  # start & end are same line

  write_file(rmd_contents, rmd_path)

}





#' Generate GROUP Data Table and insert into Rmd
#'
#' Adds Groups to an EXISTING Data Table in specified Rmd file at specified line.
#' The Groups DataTable will contain an initial ID column containing all
#' sample IDs, and a series of extra data columns named using the
#' group_names vector.
#'
#' If the table exceeds dt_length characters
#' (default 100), then the table is split into multiple tables, with IDs as
#' first col, and subsequent group_names given in subsequent tables.
#'
#' @param rmd_path path to Rmd file
#'
#' @param rmd_startline start line in Rmd file to insert table
#'
#' @param rmd_endline end line in Rmd file to insert table - same as
#' `rmd_startline` if a single line is selected, otherwise the selected lines
#' (between start & end lines) will trim all lines from first COMMENT TEXT
#' line - beginning with `>>>>`.  Used to parse default group names.
#'
#' @param datatable_name String of data table name: This data table MUST ALREADY
#' EXIST/BE DECLARED BEFORE rmd_line in rmd_path.
#'
#' @param group_names Character vector of data table column titles to add.
#' CANNOT BE BLANK!
#'
#' @param groups A list of vectors containing the group labels for each group
#' defined in group_names.
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
#' @export
datatable_add_group_rmd <- function(rmd_path, rmd_startline, rmd_endline,
                                    datatable_name, group_names, groups, dt_length=100,
                                    summarise_reps = FALSE, all_reps=FALSE) {

  cat( "\nprojectmanagr::datatable_add_group_rmd():\n" )

  # confirm rmd_path : in org && Project Doc or Note (subdir of programme) && absolute + normalised
  rmd_path <- confirm_rmd_path(rmd_path)

  cat( "  Reading Rmd...\n" )
  rmd_contents <- read_file(rmd_path)

  #### datatable add group ####

  data_tables <- datatable_add_group(
    contents = rmd_contents[1:rmd_startline],
    group_names = group_names,
    datatable_name = datatable_name,
    groups = groups,
    dt_length = dt_length,
    summarise_reps = summarise_reps,
    all_reps = all_reps
  )

  # deal with case of selecting group declarations from comments
  if( rmd_startline < rmd_endline ) {
    # have made a selection
    cont_sel <- rmd_contents[rmd_startline:rmd_endline]
    # adjust the startline to be first line with COMMENTS
    if( any(grepl(">>>>", cont_sel, fixed=TRUE)) ) {
      rmd_startline <- ( grep(">>>>", cont_sel, fixed=TRUE)[1] + rmd_startline - 1)
    } else{
      rmd_startline <- (length(cont_sel) + rmd_startline - 1) # or essentially rmd_endline
    }
  }

  #rmd_contents <- insert_dt_to_contents(data_tables, rmd_contents, rmd_startline, rmd_endline)
  rmd_contents <- insert_dt_to_contents(data_tables, rmd_contents, rmd_startline, rmd_endline)

  write_file(rmd_contents, rmd_path)

}


#' Generate Samples ADD_DATA Data Table and insert into Rmd
#'
#' Creates an `ADD_DATA` Sample DataTable in `rmd_path` file at `rmd_line`
#'
#' The `ADD_DATA` format requires that a datatable titled `datatable_name` has
#' already been created in the `rmd_path` contents becore `rmd_line`.
#'
#' The Sample DataTable will contain an initial ID column containing the
#' IDs vector, and an optional set of extra data columns as specified in the
#' data_cols vector.  If the table exceeds dt_length characters (default 100),
#' then the table is split into multiple tables, with IDs as first col, and
#' subsequent data_cols given in subsequent tables.
#'
#' @param rmd_path path to Rmd file.
#'
#' @param rmd_line line in Rmd file to insert table.
#'
#' @param datatable_name String of data table name - MUST exist in rmd_path!
#'
#' @param data_cols Character vector of data table column titles.
#'
#' @param ids_vector A vector containing the IDs to add: Can be "ALL", a set of GROUP
#' IDs (which must already be declared in datatable name under a 'group-' data
#' column), or (a subset of) IDs from datatable.
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
#' @export
datatable_add_data_samples_rmd <- function(rmd_path, rmd_line, datatable_name,
                                           data_cols, ids_vector="", default_data_vals=list(),
                                           dt_length=100, summarise_reps=FALSE,
                                           all_reps=FALSE) {

  cat( "\nprojectmanagr::datatable_add_data_samples_rmd():\n" )

  # confirm rmd_path : in org && Project Doc or Note (subdir of programme) && absolute + normalised
  rmd_path <- confirm_rmd_path(rmd_path)

  cat( "  Reading Rmd...\n" )
  rmd_contents <- read_file(rmd_path)

  #### datatable_add_data_samples ####

  data_tables <- datatable_add_data_samples(
    contents = rmd_contents[1:rmd_line],
    data_cols = data_cols,
    datatable_name = datatable_name,
    ids_vector = ids_vector,
    default_data_vals = default_data_vals,
    dt_length = dt_length,
    summarise_reps = summarise_reps,
    all_reps = all_reps )

  rmd_contents <- insert_dt_to_contents(data_tables, rmd_contents, rmd_line, rmd_line)
  # start & end are same line

  write_file(rmd_contents, rmd_path)

}



#' Generate Variables ADD_DATA Data Table and insert into Rmd
#'
#' Creates a new VARIABLES DataTable in specified Rmd file at specified line.
#'
#' The Variables DataTable will contain an initial `variables` column containing
#' the `var_names`, and a set of extra data columns as specified in the
#' `group_names` vector. These must be the group names from an existing group
#' column from the existing datatable titled `datatable_name`.
#' If the table exceeds dt_length characters (default 100), then the table is
#' split into multiple tables, with IDs as first col, and subsequent `data_cols`
#' given in subsequent tables.
#'
#' @param rmd_path path to Rmd file.
#'
#' @param rmd_line line in Rmd file to insert table.
#'
#' @param datatable_name String of data table name - MUST exist in rmd_path!
#'
#' @param var_names Names of variables to be added to first column.
#'
#' @param group_names Vector of sample IDs or GROUP NAMES from a group set, which will
#' constitute the remaining column headers.  These will typically be group
#' names.
#'
#' @param default_data_vals List of default data values to add to data cols &
#' IDs.
#'
#' @param dt_length Int of data table max length in characters - default 100.
#'
#' @export
datatable_add_data_variables_rmd <- function( rmd_path, rmd_line, datatable_name,
                                              var_names, group_names, default_data_vals=list(),
                                              dt_length=100 ) {

    cat( "\nprojectmanagr::datatable_add_data_variables_rmd():\n" )

  # confirm rmd_path : in org && Project Doc or Note (subdir of programme) && absolute + normalised
  rmd_path <- confirm_rmd_path(rmd_path)

  cat( "  Reading Rmd...\n" )
  rmd_contents <- read_file(rmd_path)

  #### datatable_add_data_variables ####

  data_tables <- datatable_add_data_variables(
    contents = rmd_contents[1:rmd_line],
    var_names = var_names,
    datatable_name = datatable_name,
    group_names = group_names,
    default_data_vals = default_data_vals,
    dt_length = dt_length )

  rmd_contents <- insert_dt_to_contents(data_tables, rmd_contents, rmd_line, rmd_line)
  # start & end are same line

  write_file(rmd_contents, rmd_path)

}





#' Generate Timetable ADD_DATA Data Table and insert into Rmd
#'
#' Creates a new TIMETABLE DataTable in specified Rmd file at specified line.
#'
#' The Timetable DataTable will contain an initial ID column containing the
#' IDs vector, and an optional set of extra data columns as specified in the
#' data_cols vector.  If the table exceeds dt_length characters (default 100),
#' then the table is split into multiple tables, with IDs as first col, and
#' subsequent data_cols given in subsequent tables.
#'
#' @param rmd_path path to Rmd file.
#'
#' @param rmd_line line in Rmd file to insert table.
#'
#' @param datatable_name String of data table name - MUST exist in rmd_path!
#'
#' @param step_names Character vector of names of procedure steps to add to
#' timetable. Must NOT contain any spaces - use '-' or '_'.
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
#' @export
datatable_add_data_timetable_rmd <- function( rmd_path, rmd_line, datatable_name,
                                              step_names, group_names, col_name,
                                              dt_length=100 ) {

  cat( "\nprojectmanagr::datatable_add_data_timetable_rmd():\n" )

  # confirm rmd_path : in org && Project Doc or Note (subdir of programme) && absolute + normalised
  rmd_path <- confirm_rmd_path(rmd_path)

  cat( "  Reading Rmd...\n" )
  rmd_contents <- read_file(rmd_path)

  #### datatable_add_data_timetable ####

  data_tables <- datatable_add_data_timetable(
    contents = rmd_contents[1:rmd_line],
    step_names = step_names,
    datatable_name = datatable_name,
    group_names = group_names,
    col_name = col_name,
    dt_length = dt_length )

  rmd_contents <- insert_dt_to_contents(data_tables, rmd_contents, rmd_line, rmd_line)
  # start & end are same line

  write_file(rmd_contents, rmd_path)

}


#' Generate IMPORT & Export Data Tables and insert into Rmds
#'
#' This function generates an IMPORT datatable based on contents of `source_rmd_path`
#' up to `source_rmd_line`, within datatable contents with `datatable_name`. The IMPORT
#' datatable is inserted into `destination_rmd_path` at `destination_rmd_line`.
#'
#' This function then generates an EXPORT datatable based on contents of `source_rmd_path`
#' up to `source_rmd_line`, within datatable contents with `datatable_name`. The EXPORT
#' datatable is inserted into `source_rmd_line` at `source_rmd_line`.
#'
#' Both `source_rmd_path` and `destination_rmd_path` are confirmed to be
#' Project Notes or Docs with the `settings` yml list.
#'
#' @param source_rmd_path path to Source Rmd file: the file that contains the
#' datatables with `datatable_name` that is read to extract the IDs (& REPs).
#'
#' @param source_rmd_line line in Rmd file to read datatables up to, and where
#' the EXPORT datatable will be inserted.
#'
#' @param destination_rmd_path path to Destination Rmd file: the file where the
#' new IMPORT datatable is written to.
#'
#' @param destination_rmd_line line in Rmd file to insert the new IMPORT table.
#'
#' @param settings projectmanagr settings yml data - to define prefix in links
#' and the export header in template.
#'
#' @param datatable_name String of data table name - MUST exist in rmd_path!
#'
#' @param destination_rel_path The relative path to the Destination Note where
#' these samples are EXPORT/IMPORTED to.
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
#' @export
datatable_import_export_rmd <- function(source_rmd_path, source_rmd_line, destination_rmd_path,
                                        destination_rmd_line, settings, datatable_name,
                                        ids_vector="", reps_vector="", dt_length=100,
                                        summarise_reps=FALSE, exportTemplate="Datatables-Export-Template.Rmd") {

  #### datatable export rmd ####
  # first to check the exports are all valid
  dER <- datatable_export_rmd(source_rmd_path, source_rmd_line, destination_rmd_path,
                       settings, datatable_name, ids_vector, reps_vector,
                       dt_length, summarise_reps, exportTemplate)

  #### datatable import rmd ####
  # after exports are written to source file
  drl <- datatable_import_rmd(source_rmd_path = source_rmd_path,
                       source_rmd_line = dER$sRL,
                       destination_rmd_path = destination_rmd_path,
                       destination_rmd_line = destination_rmd_line,
                       settings = settings,
                       datatable_name = datatable_name,
                       ids_vector = ids_vector,
                       reps_vector = reps_vector,
                       dt_length = dt_length,
                       summarise_reps = summarise_reps,
                       exportHeader = dER$exportHeader)

  return(drl) # return the new destination_rmd_line - located after the newly inserted datatable

} #### ________________________________ ####


#' Generate EXPORT Data Table and insert into Rmd
#'
#' This function generates an EXPORT datatable based on contents of `rmd_path`
#' up to `rmd_line`, & datatable contents of `datatable_name`. The EXPORT
#' datatable is inserted into `rmd_path` at `rmd_line`.
#'
#' `rmd_path` is confirmed to be a PRoject Note or Doc with the `settings` yml
#' list.
#'
#' @param source_rmd_path path to Source Rmd file: the file that contains the
#' datatables with `datatable_name` that is read to extract the IDs (& REPs),
#' and where the EXPORT datatable is written to.
#'
#' @param source_rmd_line line in Rmd file to read datatables up to, and where
#' the EXPORT datatable will be inserted.
#'
#' @param destination_rmd_path path to Destination Rmd file: the file where the
#' sample IDs will be IMPORTED to.
#'
#' @param settings projectmanagr settings yml data - to define prefix in links
#' and the export header in template.
#'
#' @param datatable_name String of data table name - MUST exist in rmd_path!
#'
#' @param destination_rel_path The relative path to the Destination Note where
#' these samples are EXPORT/IMPORTED to.
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
#' @param exportTemplate The Rmd file to use as a template to generate EXPORT
#' Content in the `source_rmd_path`.  Contains parameters for placement of the
#' Export Header `{{DT_EXPORT_HEADER_TEMPLATE}}` and export datatable(s)
#' `{{DT_EXPORT_DATATABLE}}` in the content.  The Export Header is filled with
#' the `DatatableExportHeader` parameter from `settings` in Organisation, concatenated
#' to the `datatable_name`.  Any matches to this header that exist in `source_rmd_path`
#' file, causes the header name to be iterated with a number suffix.  This ensures
#' the EXPORT of samples in a project note are easy to identify in RStudio Rmd outlines
#' *as `DatatableExportHeader` by default is a markdown section header!)
#' Default is "Datatables-Export--Template.Rmd" from projectmanagr templates, and the
#' content of this file can be modified to suit the users needs.
#'
datatable_export_rmd <- function(source_rmd_path, source_rmd_line, destination_rmd_path,
                                 settings, datatable_name, ids_vector="", reps_vector="",
                                 dt_length=100, summarise_reps=FALSE,
                                 exportTemplate="Datatables-Export-Template.Rmd") {

  cat( "\nprojectmanagr::datatable_export_rmd():\n" )

  # confirm rmd_path : in org && Project Doc or Note (subdir of programme) && absolute + normalised
  source_rmd_path <- confirm_rmd_path(source_rmd_path)
  destination_rmd_path <- confirm_rmd_path(destination_rmd_path)

  # define the relative path from source and destination
  destination_rel_path <- fs::path_rel(destination_rmd_path, start=fs::path_dir(source_rmd_path))
  destination_rel_path_link <- paste0("[", get_prefix(destination_rel_path, settings),
                                 "](", destination_rel_path, ")")

  cat( "  Reading Rmd...\n" )
  source_rmd_contents <- read_file(source_rmd_path)
  sr_contents <- source_rmd_contents[1:source_rmd_line] # trimmed to selected line

  #### datatable export ####

  data_tables <- datatable_export(
    contents = sr_contents,
    datatable_name = datatable_name,
    destination_rel_path_link = destination_rel_path_link,
    ids_vector = ids_vector,
    reps_vector = reps_vector,
    dt_length = dt_length,
    summarise_reps = summarise_reps  )


  #### export template ####

  eFT <- dt_export_fill_template(source_rmd_path, exportTemplate, settings,
                                           sr_contents, datatable_name, data_tables)

  #### insert export template contents & write file ####

  # if source line is blank, find last line in source contents with content and
  # insert export content just after this element
   # ie. maintain any blank lines in source contents!
  sRL <- compute_previous_line_index(source_rmd_line, source_rmd_contents)
  sRC <- insert_dt_to_contents(eFT$exportContent, source_rmd_contents, sRL, sRL)
  # start & end are same line

  write_file(sRC, source_rmd_path)

  return( list(sRL=sRL, exportHeader=eFT$exportHeader) ) # return the NEW source_rmd_line & exportHeader!

}



#' Generate IMPORT Data Table and insert into Rmd
#'
#' This function generates an IMPORT datatable based on contents of `source_rmd_path`
#' up to `source_rmd_line`, & datatable contents with `datatable_name`. The IMPORT
#' datatable is inserted into `destination_rmd_path` at `destination_rmd_line`.
#'
#' Both `source_rmd_path` and `destination_rmd_path` are confirmed to be
#' Project Notes or Docs with the `settings` yml list.
#'
#' @param source_rmd_path path to Source Rmd file: the file that contains the
#' datatables with `datatable_name` that is read to extract the IDs (& REPs).
#'
#' @param source_rmd_line line in Rmd file to read datatables up to.
#'
#' @param destination_rmd_path path to Destination Rmd file: the file where the
#' new IMPORT datatable is written to.
#'
#' @param destination_rmd_line line in Rmd file to insert the new IMPORT table.
#'
#' @param settings projectmanagr settings yml data - to define prefix in links.
#'
#' @param datatable_name String of data table name - MUST exist in rmd_path!
#'
#' @param destination_rel_path The relative path to the Destination Note where
#' these samples are EXPORT/IMPORTED to.
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
#' @param exportHeader String of markdown header written to source note where the export
#' datatable was written. This is used to link directly to this section from the import
#' datatable IMPORT col values - the source_re_path_link - using html link syntax.
#' Can navigate this link in R markdown using the `navigate_markdown_link()` function
#' in `projectmanagr`
#'
datatable_import_rmd <- function(source_rmd_path, source_rmd_line, destination_rmd_path,
                                 destination_rmd_line, settings, datatable_name,
                                 ids_vector="", reps_vector="", dt_length=100,
                                 summarise_reps=FALSE, exportHeader) {

  cat( "\nprojectmanagr::datatable_import_rmd():\n" )

  # confirm rmd_path : in org && Project Doc or Note (subdir of programme) && absolute + normalised
  source_rmd_path <- confirm_rmd_path(source_rmd_path)
  destination_rmd_path <- confirm_rmd_path(destination_rmd_path)

  # define the relative path link from source and destination
  source_rel_path <- fs::path_rel(source_rmd_path, start=fs::path_dir(destination_rmd_path))
  source_rel_path <- link_add_section(source_rel_path, exportHeader)
  source_rel_path_link <- paste0("[", get_prefix(source_rel_path, settings),
                                 "](", source_rel_path, ")")

  cat( "  Reading Files...\n" )
  source_rmd_contents <- read_file(source_rmd_path)
  destination_rmd_contents <- read_file(destination_rmd_path)

  #### datatable import ####

  data_tables <- datatable_import(
    source_contents = source_rmd_contents[1:source_rmd_line],
    destination_contents = destination_rmd_contents,
    datatable_name = datatable_name,
    source_rel_path_link = source_rel_path_link,
    ids_vector = ids_vector,
    reps_vector = reps_vector,
    dt_length = dt_length,
    summarise_reps = summarise_reps  )

  # insert into destination!
  destination_rmd_contents <- insert_dt_to_contents(
                                  data_tables, destination_rmd_contents,
                                  destination_rmd_line, destination_rmd_line) # start & end are same line

  write_file(destination_rmd_contents, destination_rmd_path)

  # return the line index AFTER insertion of data_tables
  return( (destination_rmd_line + length(data_tables)) )

} #### ________________________________ ####





#' Insert data_tables vector into rmd_contents, between rmd_startline & rmd_endline
#'
#' Removes content between `rmd_startline` and `rmd_endline`
insert_dt_to_contents <- function(data_tables, rmd_contents, rmd_startline, rmd_endline) {
  # write these to the file:
  cat( "\n  write data table(s) to Rmd at line: ", rmd_startline )
  rmd_contents <- c(rmd_contents[1:(rmd_startline-1)], data_tables,
                    rmd_contents[(rmd_endline+1):length(rmd_contents)])
  # rmd_line-1 to REMOVE CURRENT LINE

  rmd_contents # return contents
}


dt_export_fill_template <- function(source_rmd_path, exportTemplate, settings,
                                    sr_contents, datatable_name, data_tables) {

  # get template
  orgPath <- find_org_directory(source_rmd_path)
  tempPath <- get_template_dir(orgPath)
  exportContents <- read_file( paste0( tempPath, .Platform$file.sep, exportTemplate) )
  #exportContents <- read_file("/home/sjwest/_ORG/01-COMPUTING/prog-lang/R/r-dev/projectmanagr-dev/projectmanagr/inst/templates/Datatables-Export-Template.Rmd")

  # identify exportContents header that make export head (content above datatable) unique to source note contents
  rEH <- recurse_export_head(exportContents, settings, datatable_name, sr_contents)

  # add the datatable(s) to the valid export contents
  eCDT <- sub_template_param(rEH$eC, "{{DT_EXPORT_DATATABLE}}",
                             data_tables, orgPath)

  # return list of the valid export contents containing the datatable(s) PLUS the export Header generated
  list(exportContent=eCDT, exportHeader=rEH$exportHeader)
}


recurse_export_head <- function(exportContents, settings, datatable_name, sr_contents) {

  # define the Export Section Header
  exportHeader <- paste0(settings[["DatatableExportHeader"]], datatable_name)

  eC <- sub_template_param(exportContents, "{{DT_EXPORT_HEADER_TEMPLATE}}",
                           exportHeader, orgPath)

  # check if header already exists in contents:
  # if so, will add EXPORT dt here if samples are valid at this position
  # otherwise will add at source_rmd_line
  eCH <- eC[1:(grep("{{DT_EXPORT_DATATABLE}}", eC, fixed=TRUE)-1)]
  exportIndex <- match_vector(eCH, sr_contents)
  if( length(exportIndex) > 0 ) {
    # then there is a match - continue to iterate with different header names until no match is found
    i <- 2
    while(TRUE) {

      # define the Export Section Header
      exportHeader <- paste0(settings[["DatatableExportHeader"]], datatable_name, paste0(" ", i))

      eC <- sub_template_param(exportContents, "{{DT_EXPORT_HEADER_TEMPLATE}}",
                               exportHeader, orgPath)

      # check if header already exists in contents:
      # if so, will add EXPORT dt here if samples are valid at this position
      # otherwise will add at source_rmd_line
      eCH <- eC[1:(grep("{{DT_EXPORT_DATATABLE}}", eC, fixed=TRUE)-1)]
      exportIndex <- match_vector(eCH, sr_contents)

      if(length(exportIndex) > 0 ) {
        # STILL have a match of export content - so increment i
        i <- i + 1
      } else {
        # no match - so break out of the loop
        break
      }
    }
    # return the export contents with a valid header
    list(eC=eC, exportHeader=exportHeader)

  } else {
    # there is no match, so exit the while loop
    # return the export contents with a valid header
    list(eC=eC, exportHeader=exportHeader)
  }
}


