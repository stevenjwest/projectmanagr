
#' Create new Plaintext Data Table and insert into Rmd
#'
#' Creates a new Sample DataTable in specified Rmd file at specified line.
#' The Sample DataTable will contain an initial ID column containing the
#' IDs vector, and an optional set of extra data columns as specified in the
#' data_cols vector.  If the table exceeds dt_length characters (default 100),
#' then the table is split into multiple tables, with IDs as first col, and
#' subsequent data_cols given in subsequent tables.
#'
#' @param rmd_path path to Rmd file
#'
#' @param rmd_line line in Rmd file to insert table
#'
#' @param IDs Character vector of sample IDs
#'
#' @param data_cols Character vector of data table column titles
#'
#' @param datatable_name String of data table name - default "samples".
#'
#' @param dt_length Int of data table max length in characters - default 100.
#'
#' @export
datatable_create_rmd <- function( rmd_path, rmd_line, settings,
                                  IDs="", data_cols="",
                                  datatable_name = "samples", dt_length = 100 ) {

  cat( "\nprojectmanagr::datatable_create_rmd():\n" )

  # confirm rmd_path : in org && Project Doc or Note (subdir of programme) && absolute + normalised
  rmd_path <- confirm_rmd_path(rmd_path, settings)

  cat( "  Reading Rmd...\n" )
  rmd_contents <- read_file(rmd_path)

  data_tables <- datatable_create( IDs, data_cols, datatable_name, default_data_vals=list(), dt_length=dt_length )

  # write these to the file:
  cat( "\n  write data table(s) to Rmd at line: ", rmd_line )
  rmd_contents <- c( rmd_contents[1:(rmd_line-1)], data_tables, rmd_contents[(rmd_line+1):length(rmd_contents)] )
  # rmd_line-1 to REMOVE CURRENT LINE

  write_file(rmd_contents, rmd_path)

}


#' Create new Sample DataTable from TEMPLATE and insert into Rmd
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
#' @param rmd_startline line in Rmd file where TEMPLATE datatable starts
#'
#' @param rmd_endline line in Rmd file where TEMPLATE datatable ends
#'
#' @param IDs Character vector of sample IDs
#'
#' @param datatable_name String of data table name - default "samples".
#'
#' @param dt_length Int of data table max length in characters - default 100.
#'
#' @export
datatable_create_template_rmd <- function( rmd_path, rmd_startline, rmd_endline, settings,
                                           IDs, datatable_name = "samples", dt_length = 100 ) {

  cat( "\nprojectmanagr::datatable_create_template_rmd():\n" )


  # confirm rmd_path : in org && Project Doc or Note (subdir of programme) && absolute + normalised
  rmd_path <- confirm_rmd_path(rmd_path, settings)

  cat( "  Reading Rmd...\n" )
  rmd_contents <- read_file(rmd_path)

  # get the template
  template_table <- rmd_contents[ rmd_startline : rmd_endline ]

  # extract template_table string as list
  #template_list <- datatable_extract(template_table)

  # REPLACE IDs in the list
  #template_list[[1]] <- c(template_list[[1]][1], IDs)

  # and add MULTIPLES of current content in each other vector in list according to IDs length
  #for(i in 2:length(template_list) ) {
  #  template_list[[i]] <- c(template_list[[i]][1], rep(template_list[[i]][2], length(IDs) ) )
  #}

  #template_list <- template_list[2:length(template_list)]

  # get just the data_cols:
  #data_cols <- unlist( lapply(template_list, `[[`, 1) )


  template_data <- datatable_extract(template_table)

  # ids from template - should include special syntax <<IDS>>
  template_ids <- template_data[[1]][2:length(template_data[[1]])]

  # extract the data_col names and values from template_data
  data_cols <- unlist(lapply(template_data[2:length(template_data)], function(l) l[[1]]))
  data_col_values <- lapply(template_data[2:length(template_data)], function(l) l[2:length(l)])

  # adjust the IDs - replacing <<IDS>> with IDs from selected DT
  # also create a new list for each col
  ids_v <- c()
  for(id in IDs) {
    ids_v <- c(ids_v, sub("<<IDS>>", id, template_ids, fixed=TRUE))
  }
  ids <- ids_v

  # also adjust the data col values
  dcv_l <- list()
  for(dcvI in 1:length(data_col_values)) {
    dcv_l[[dcvI]] <-rep(data_col_values[[dcvI]], length(IDs))
  }
  default_data_vals <- dcv_l

  # remove IDs that already exist in previous dt of same name

  # read datatables upto startline
  dts <- datatable_read_vector(rmd_contents[1:rmd_startline])

  if( any(names(dts) == datatable_name) ) {
    # the datatable to be created does exist
    # so get the IDs that already exist:
    ids_exist <-dts[[datatable_name]][['ID']]
    # FIRST remove default data vals for each element in list
    for(ddvi in 1:length(default_data_vals) ) {
      default_data_vals[[ddvi]] <- default_data_vals[[ddvi]][ !(ids %in% ids_exist) ]
    }
    # and THEN remove these from ids
    ids <- ids[ !(ids %in% ids_exist) ]
  }
  # now ids has had EXISTING ids removed

  if( length(ids) == 0 ) {
    # all ids already defined - so FAIL with sensible error message
    stop( paste0("  All IDs already defined in existing datatables of this name: ", datatable_name) )

  }

  #### datatable create ####

  data_tables <- datatable_create(
    IDs= ids,
    data_cols= data_cols,
    datatable_name = datatable_name,
    default_data_vals = default_data_vals,
    dt_length = dt_length
  )

  # write these to the file:
  cat( "\n  write data table(s) to Rmd at line: ", rmd_startline )
  rmd_contents <- c( rmd_contents[1:(rmd_startline-1)], data_tables, rmd_contents[(rmd_endline+1):length(rmd_contents)] )

  write_file(rmd_contents, rmd_path)

}



#' Create new Sample Data Table and insert into Rmd
#'
#' Creates a new Sample DataTable in specified Rmd file at specified line.
#' The Sample DataTable will contain an initial ID column containing the
#' IDs vector, and an optional set of extra data columns as specified in the
#' data_cols vector.  If the table exceeds dt_length characters (default 100),
#' then the table is split into multiple tables, with IDs as first col, and
#' subsequent data_cols given in subsequent tables.
#'
#' @param rmd_path path to Rmd file
#'
#' @param rmd_line line in Rmd file to insert table
#'#'
#' @param data_cols Character vector of data table column titles
#'
#' @param datatable_name String of data table name - MUST exist in rmd_path!
#'
#' @param ids_vector A vector containing the IDs to add: Can be "ALL", a set of GROUP
#' IDs (which must already be declared in datatable name under a 'group-' data
#' column), or (a subset of) IDs from datatable.
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
datatable_add_data_samples_rmd <- function( rmd_path, rmd_line, data_cols, datatable_name, settings,
                                            ids_vector="", dt_length = 100, summarise_reps = FALSE  ) {

  cat( "\nprojectmanagr::datatable_add_data_samples_rmd():\n" )

  # confirm rmd_path : in org && Project Doc or Note (subdir of programme) && absolute + normalised
  rmd_path <- confirm_rmd_path(rmd_path, settings)

  cat( "  Reading Rmd...\n" )
  rmd_contents <- read_file(rmd_path)

  #### datatable_add_data_samples ####
  data_tables <- datatable_add_data_samples( rmd_contents[1:rmd_line], data_cols, datatable_name,
                                             ids_vector, default_data_vals=list(), dt_length, summarise_reps )

  # write these to the file:
  cat( "\n  write data table(s) to Rmd at line: ", rmd_line )
  rmd_contents <- c( rmd_contents[1:(rmd_line-1)], data_tables, rmd_contents[(rmd_line+1):length(rmd_contents)] )

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
#' @param rmd_startline line in Rmd file where TEMPLATE datatable starts
#'
#' @param rmd_endline line in Rmd file where TEMPLATE datatable ends
#'
#' @param IDs Character vector of sample IDs
#'
#' @param datatable_name String of data table name.
#'
#' @param dt_length Int of data table max length in characters - default 100.
#'
#' @param summarise_reps Whether to summarise reps of samples in the new datatable.
#'
#' @export
datatable_add_data_samples_template_rmd <- function( rmd_path, rmd_startline, rmd_endline,
                                                     IDs, datatable_name, settings,
                                                     dt_length = 100, summarise_reps = FALSE ) {

  cat( "\nprojectmanagr::datatable_add_data_samples_template_rmd():\n" )

  # confirm rmd_path : in org && Project Doc or Note (subdir of programme) && absolute + normalised
  rmd_path <- confirm_rmd_path(rmd_path, settings)

  cat( "  Reading Rmd...\n" )
  rmd_contents <- read_file(rmd_path)

  template_table <- rmd_contents[rmd_startline:rmd_endline]

  # extract as list
  template_list <- datatable_extract(template_table)

  # remove IDs col - first index
  template_list <- template_list[2:length(template_list)]

  # get the data_cols:
  data_cols <- unlist( lapply(template_list, `[[`, 1) )

  # get the default_data_vals
  default_data_vals <- lapply(template_list, `[[`, 2)

  # CHECK data_cols are all unique
  if( anyDuplicated(data_cols) != 0 ) {
    stop( paste0("  duplicate column headers: ", data_cols[duplicated(data_cols)]) )
  }


  #### datatable add data samples ####

  data_tables <- datatable_add_data_samples(
    contents = rmd_contents[1:(rmd_startline-1)],
    data_cols = data_cols,
    datatable_name = datatable_name,
    ids_vector = IDs,
    default_data_vals = default_data_vals,
    dt_length = dt_length,
    summarise_reps = summarise_reps
  )

  cat( "\n  write data table(s) to Rmd at line: ", rmd_startline )
  rmd_contents <- c( rmd_contents[1:(rmd_startline-1)], data_tables, rmd_contents[(rmd_endline+1):length(rmd_contents)] )

  write_file(rmd_contents, rmd_path)

}




#' Create new Sample Data Table and insert into Rmd
#'
#' Creates a new Sample DataTable in specified Rmd file at specified line.
#' The Sample DataTable will contain an initial ID column containing the
#' IDs vector, and an optional set of extra data columns as specified in the
#' data_cols vector.  If the table exceeds dt_length characters (default 100),
#' then the table is split into multiple tables, with IDs as first col, and
#' subsequent data_cols given in subsequent tables.
#'
#' @param rmd_path path to Rmd file
#'
#' @param rmd_line line in Rmd file to insert table
#'#'
#' @param var_names Names of variables to be added to first column.
#'
#' @param datatable_name The EXISTING datatable from which the IDs must be drawn.
#'
#' @param group_names Vector of sample IDs or GROUP NAMES from a group set, which will
#' constitute the remaining column headers.  These will typically be group
#' names.
#'
#' @param dt_length Int of data table max length in characters - default 100.
#'
#' @export
datatable_add_data_variables_rmd <- function( rmd_path, rmd_line, var_names,
                                              datatable_name, group_names, settings,
                                              dt_length = 100 ) {

  cat( "\nprojectmanagr::datatable_add_data_variables_rmd():\n" )

  # confirm rmd_path : in org && Project Doc or Note (subdir of programme) && absolute + normalised
  rmd_path <- confirm_rmd_path(rmd_path, settings)

  cat( "  Reading Rmd...\n" )
  rmd_contents <- read_file(rmd_path)

  data_tables <- datatable_add_data_variables( rmd_contents[1:rmd_line], var_names, datatable_name, group_names, dt_length )

  # write these to the file:
  cat( "\n  write data table(s) to Rmd at line: ", rmd_line )
  rmd_contents <- c( rmd_contents[1:(rmd_line-1)], data_tables, rmd_contents[(rmd_line+1):length(rmd_contents)] )

  rmd_file_conn <- file( rmd_path )
  writeLines(rmd_contents, rmd_file_conn)
  close(rmd_file_conn)

}





#' Create new Sample Data Table and insert into Rmd
#'
#' Creates a new Sample DataTable in specified Rmd file at specified line.
#' The Sample DataTable will contain an initial ID column containing the
#' IDs vector, and an optional set of extra data columns as specified in the
#' data_cols vector.  If the table exceeds dt_length characters (default 100),
#' then the table is split into multiple tables, with IDs as first col, and
#' subsequent data_cols given in subsequent tables.
#'
#' @param rmd_path path to Rmd file
#'
#' @param rmd_line line in Rmd file to insert table
#'
#' @param step_names Character vector of names of procedure steps to add to
#' timetable. Must NOT contain any spaces - use '-' or '_'.
#'
#' @param datatable_name The EXISTING datatable from which the IDs must be drawn.
#'
#' @param group_names Vector of sample IDs or GROUP NAMES from a group set, which will
#' constitute the remaining column headers.  These will typically be group
#' names. Must EXIST in contents and be declared from datatable_name IDs!
#'
#' @param dt_length Int of data table max length in characters - default 100.
#'
#' @export
datatable_add_data_timetable_rmd <- function( rmd_path, rmd_line, step_names,
                                              datatable_name, group_names, settings,
                                              dt_length = 100 ) {

  cat( "\nprojectmanagr::datatable_add_data_timetable_rmd():\n" )

  # confirm rmd_path : in org && Project Doc or Note (subdir of programme) && absolute + normalised
  rmd_path <- confirm_rmd_path(rmd_path, settings)

  cat( "  Reading Rmd...\n" )
  rmd_contents <- read_file(rmd_path)

  data_tables <- datatable_add_data_timetable( rmd_contents[1:rmd_line], step_names, datatable_name, group_names, dt_length )

  # write these to the file:
  cat( "\n  write data table(s) to Rmd at line: ", rmd_line )
  rmd_contents <- c( rmd_contents[1:(rmd_line-1)], data_tables, rmd_contents[(rmd_line+1):length(rmd_contents)] )

  rmd_file_conn <- file( rmd_path )
  writeLines(rmd_contents, rmd_file_conn)
  close(rmd_file_conn)

}



#' Add Groups to Data Table and insert into Rmd
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
#' @param rmd_line line in Rmd file to insert table
#'
#' @param group_names Character vector of data table column titles to add.
#' CANNOT BE BLANK!
#'
#' @param datatable_name String of data table name: This data table MUST ALREADY
#' EXIST/BE DECLARED BEFORE rmd_line in rmd_path.
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
#' @export
datatable_add_group_rmd <- function(rmd_path, rmd_startline, rmd_endline, group_names,
                                    datatable_name, groups, settings,
                                    dt_length = 100, summarise_reps = FALSE  ) {

  cat( "\nprojectmanagr::datatable_group_rmd():\n" )

  # confirm rmd_path : in org && Project Doc or Note (subdir of programme) && absolute + normalised
  rmd_path <- confirm_rmd_path(rmd_path, settings)

  cat( "  Reading Rmd...\n" )
  rmd_contents <- read_file(rmd_path)

  data_tables <- datatable_add_group(
    contents = rmd_contents[1:rmd_startline],
    group_names = group_names,
    datatable_name = datatable_name,
    groups = groups,
    dt_length = dt_length,
    summarise_reps = summarise_reps
  )

  # deal with case of selecting group declarations
  if( rmd_startline < rmd_endline ) {
    # have made a selection
    cont_sel <- rmd_contents[rmd_startline:rmd_endline]
    # just adjust the startline to be first line with COMMENTS
    if( any(grepl(">>>>", cont_sel, fixed=TRUE)) ) {
      startLine <- ( grep(">>>>", cont_sel, fixed=TRUE)[1] + rmd_startline - 1)
    } else{
      startLine <- (length(cont_sel) + rmd_startline - 1) # or essentially rmd_endline
    }
  }

  # write these to the file:
  cat( "\n  write data table(s) to Rmd at line: ", rmd_startline )
  rmd_contents <- c( rmd_contents[1:(startLine-1)], data_tables, rmd_contents[(rmd_endline+1):length(rmd_contents)] )

  rmd_file_conn <- file( rmd_path )
  writeLines(rmd_contents, rmd_file_conn)
  close(rmd_file_conn)

}




#' Add Groups to Sample DataTable from TEMPLATE and insert into Rmd
#'
#' Creates a Sample DataTable in specified Rmd file from the template that
#' should be present between specified start & end line.
#'
#' The template Sample DataTable will contain an initial ID column containing
#' a default IDs vector (which will be replaced with IDs from selected existing
#' datatable), and all group columns will have their initial data
#' value replicated along the length of the datatable, serially assigning groups
#' to sample IDs.
#'
#' @param rmd_path path to Rmd file
#'
#' @param rmd_startline line in Rmd file where TEMPLATE datatable starts
#'
#' @param rmd_endline line in Rmd file where TEMPLATE datatable ends
#'
#' @param IDs Character vector of sample IDs
#'
#' @param datatable_name String of data table name.
#'
#' @param dt_length Int of data table max length in characters - default 100.
#'
#' @param summarise_reps Whether to summarise reps of samples in the new datatable.
#'
#' @export
datatable_add_group_template_rmd <- function( rmd_path, rmd_startline, rmd_endline,
                                              IDs, datatable_name, dt_length = 100,
                                              summarise_reps = FALSE ) {

  cat( "\nprojectmanagr::datatable_add_group_template_rmd():\n" )

  # confirm rmd_path : in org && Project Doc or Note (subdir of programme) && absolute + normalised
  rmd_path <- confirm_rmd_path(rmd_path, settings)

  cat( "  Reading Rmd...\n" )
  rmd_contents <- read_file(rmd_path)

  template_table <- rmd_contents[ rmd_startline : rmd_endline ]

  # extract as list
  template_list <- datatable_extract(template_table)

  # remove IDs col - first index
  template_list <- template_list[2:length(template_list)]

  # get the data_cols:
  data_cols <- unlist( lapply(template_list, `[[`, 1) )

  # get the default_data_vals
  default_data_vals <- lapply(template_list, `[[`, 2)

  # CHECK data_cols are all unique
  if( anyDuplicated(data_cols) != 0 ) {
    stop( paste0("  duplicate column headers: ", data_cols[duplicated(data_cols)]) )
  }


  #data_tables <- datatable_add_data_samples( rmd_contents[1:(rmd_startline-1)], data_cols, datatable_name,
  #                                           IDs, default_data_vals, dt_length, summarise_reps )

  data_tables <- datatable_add_group( IDs, template_table, datatable_name, dt_length )

  cat( "\n  write data table(s) to Rmd at line: ", rmd_startline )
  rmd_contents <- c( rmd_contents[1:(rmd_startline-1)], data_tables, rmd_contents[(rmd_endline+1):length(rmd_contents)] )

  write_file(rmd_contents, rmd_path)

}


#' Dispose of a set of Samples from Rmd
#'
#' This generates a new datatable that disposes samples - indicates they no
#' longer exist.
#'
#' Datatable comprises the dataframe NAME, sample IDs, reps (if applicable), and
#' the special column titled `dispose`.  In this column the datetime of disposal
#' (i.e. time of creation of dispose datatable) is written.
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
#' indicated as: 1:3,5,6:10,12,14:25 etc.  Default to TRUE
#'
#' @export
datatable_dispose_rmd <- function( rmd_path, rmd_line, datatable_name, settings,
                                   dt_length = 100, summarise_reps = TRUE ) {

  cat( "\nprojectmanagr::datatable_resample_rmd():\n" )

  # confirm rmd_path : in org && Project Doc or Note (subdir of programme) && absolute + normalised
  rmd_path <- confirm_rmd_path(rmd_path, settings)

  cat( "  Reading Rmd...\n" )
  rmd_contents <- read_file(rmd_path)

  data_tables <- datatable_dispose( rmd_contents[1:rmd_line], datatable_name, dt_length, summarise_reps )

  # write these to the file:
  cat( "\n  write data table(s) to Rmd at line: ", rmd_line )
  rmd_contents <- c( rmd_contents[1:(rmd_line-1)], data_tables, rmd_contents[(rmd_line+1):length(rmd_contents)] )

  rmd_file_conn <- file( rmd_path )
  writeLines(rmd_contents, rmd_file_conn)
  close(rmd_file_conn)

}




#' Resample a set of samples and insert into Rmd
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
#' @param dt_length Int of data table max length in characters - default 100.
#'
#' @export
datatable_resample_rmd <- function( rmd_path, rmd_line, datatable_name, settings,
                                    resample_vector, dt_length = 100 ) {

  cat( "\nprojectmanagr::datatable_resample_rmd():\n" )

  # confirm rmd_path : in org && Project Doc or Note (subdir of programme) && absolute + normalised
  rmd_path <- confirm_rmd_path(rmd_path, settings)

  cat( "  Reading Rmd...\n" )
  rmd_contents <- read_file(rmd_path)

  data_tables <- datatable_resample( rmd_contents[1:rmd_line], datatable_name, resample_vector, dt_length )

  # write these to the file:
  cat( "\n  write data table(s) to Rmd at line: ", rmd_line )
  rmd_contents <- c( rmd_contents[1:(rmd_line-1)], data_tables, rmd_contents[(rmd_line+1):length(rmd_contents)] )

  rmd_file_conn <- file( rmd_path )
  writeLines(rmd_contents, rmd_file_conn)
  close(rmd_file_conn)

}





#' Resample Sample from TEMPLATE and insert into Rmd
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
#' @param rmd_startline line in Rmd file where TEMPLATE datatable starts
#'
#' @param rmd_endline line in Rmd file where TEMPLATE datatable ends
#'
#' @param IDs Character vector of sample IDs
#'
#' @param datatable_name String of data table name.
#'
#' @param dt_length Int of data table max length in characters - default 100.
#'
#'
#' @export
datatable_resample_template_rmd <- function( rmd_path, rmd_startline, rmd_endline,
                                             datatable_name, settings,
                                             dt_length = 100 ) {

  cat( "\nprojectmanagr::datatable_resample_template_rmd():\n" )

  # confirm rmd_path : in org && Project Doc or Note (subdir of programme) && absolute + normalised
  rmd_path <- confirm_rmd_path(rmd_path, settings)

  cat( "  Reading Rmd...\n" )
  rmd_contents <- read_file(rmd_path)

  template_table <- rmd_contents[rmd_startline:rmd_endline]

  # extract as list
  template_list <- datatable_extract(template_table)

  # remove IDs col - first index
  template_list <- template_list[2:length(template_list)]

  # get resample_vector
  resample_vector <- unlist(strsplit(template_list[[1]][2], ' '))

  # get rep_vector
  rep_vector <- unlist(strsplit(template_list[[2]][2], ' '))

  # CHECK data_cols are all unique
  if( length(resample_vector) != length(rep_vector) ) {
    stop( paste0("  incorrect length of resample & rep vectors: ", resample_vector, " : ", rep_vector) )
  }


  #### datatable resample ####

  data_tables <- datatable_resample( rmd_contents[1:(rmd_startline-1)], datatable_name,
                                     resample_vector, rep_vector, dt_length)

  cat( "\n  write data table(s) to Rmd at line: ", rmd_startline )
  rmd_contents <- c( rmd_contents[1:(rmd_startline-1)], data_tables, rmd_contents[(rmd_endline+1):length(rmd_contents)] )

  write_file(rmd_contents, rmd_path)

}
