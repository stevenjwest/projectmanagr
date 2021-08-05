


#' Convert datetime (suffix `_dt`) columns to DURATIONS
#'
#' This extracts all columns in dt that end in `_dt` (the conventional suffix
#' in projectmanagr datatable columns for a DATETIME), and computes the
#' DURATIONS between these columns.
#'
#' The durations are returned in a new tibble, along with any non-datetime
#' columns, using the same naming convention as the original datatable, with
#' the `_dt` suffix replaced with `_dur`.
#'
#' The duration data is in the format `HH:MM`, indicating the duration in
#' hours:minutes of the step.
#'
#' @param dt a datatable (tibble) that contains datetime columns (suffix `_dt`)
#'
#' @param include_now Boolean to indicate whether the last datetime should have
#' its duration set up to the current time.  The last `_dt` column has its
#' duration calculated from the current time.  FALSE by default, meaning the
#' last datetime will not have a duration set.
#'
#' @param cumulative Boolean to indicate whether the cumulative duration from the
#' first datetime should be calculated, or individual durations for each step
#' calculated.  FALSE by default, meaning the datetimes are returned as
#' independent durations
#'
#' @returns A new tibble with all datetime columns replaced with duration
#' columns (suffix `_dur`), containing the duration data in`HH:MM` format.
#'
#' @export
datatable_datetimes_to_durations <- function( dt,
                                              include_now = FALSE,
                                              cumulative = FALSE ) {

  # get the datetime and other cols
  dt_dt <- dt %>% select( ends_with("_dt"))
  dt_out <- dt %>% select( -ends_with("_dt"))

  # new col names all suffixed with _dur
  col_names <- gsub("_dt", "_dur", names(dt_dt))

  # add these as char cols to output datatable
  newCol <- NA_character_
  #newCol <- as.POSIXct(NA)
  for(i in 1:length(col_names) ) {
    dt_out <- tibble::add_column( dt_out, newCol )
    names(dt_out)[names(dt_out) == "newCol"] <- col_names[i]
  }

  # create current datetime in same format as projectmanagr datetime insertions:
  now <- lubridate::now()
  now <- paste0( substr(now, 1, 4), "/", substr(now, 6, 7), "/", substr(now, 9, 10), ":", substr(now, 12, 16) )

  l <- 1 # initiate l

  for(i in 1:nrow(dt_dt)) {

    dts <- c()
    for(j in 1:ncol(dt_dt) ) {
      dts <- c(dts,  paste0( substr(dt_dt[[i,j]], 1, 4), "/",
                             substr(dt_dt[[i,j]], 6, 7), "/",
                             substr(dt_dt[[i,j]], 9, 10), ":",
                             substr(dt_dt[[i,j]], 12, 16) ))
    }

    #dts <- dt_dt %>% slice(i) %>% unlist(., use.names=FALSE)


    if(include_now == TRUE) {
      dts <- c(dts, now )
    }

    # loop through all columns in row i
    for(j in 1:(length(dts)-1) ) {

      k <- 1 # keep track of BLANK (NA) vals

      if( dts[j] != "NA/NA/NA:NA" ) { # check CURRENT time is not NA

        while( dts[(j+k)] == "NA/NA/NA:NA" ) { # check NEXT time is not NA
          k <- k + 1
        }

        if(cumulative == TRUE) {
          l <- 1
        } else {
          l <- j
        }

        mins <- lubridate::as.duration(
          lubridate::interval(lubridate::ymd_hm(dts[l]),
                              lubridate::ymd_hm(dts[(j+k)])) ) %>% as.numeric('min')

        dt_out[ names(dt_out) == col_names[j] ][[1]][i] <- paste0(as.integer(mins/60), ":", mins%%60)

      }
    }
  }

  # return
  dt_out

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
datatable_insert_from_dataframe <- function( df,
                                             dt_function = "CREATE", dt_length = 120 ) {

  DATATABLE_SPACER_CHAR <- "="

  if(dt_function == "CREATE" || dt_function == "ADD_DATA" || dt_function == "RESAMPLE") {
    dt_function_next <- "ADD_DATA"
  } else if( dt_function == "GROUP" ) {
    dt_function_next <- "GROUP" # create further group tables
  } else if( dt_function == "EXPORT" ) {
    dt_function_next <- "EXPORT" # create further export tables
  } else if( dt_function == "IMPORT" ) {
    dt_function_next <- "IMPORT" # create further import tables
  } else {
    stop( paste0("  dt_function MUST be CREATE ADD_DATA RESAMPLE GROUP EXPORT or IMPORT : ", dt_function))
  }

  # returns name of ORIGINAL VARIABLE!
  name <- rlang::enexpr(df)

  # get the current active doc and metadata
  context <- rstudioapi::getSourceEditorContext()
  rmd_line <- context$selection[[1]]$range$start[1]
  rmd_path <- normalizePath( context$path )
  rmd_contents <- context$contents

  # SAVE the document before processing!
  rstudioapi::documentSave(id = context$id)


  cat( "\nprojectmanagr::datatable_insert_from_dataframe():\n" )

  # if not an absolute path:
  if( R.utils::isAbsolutePath(rmd_path) == FALSE ) {
    rmd_path <- R.utils::getAbsolutePath(rmd_path )
  }

  # CONFIRM rmd_path is a project doc or note:
  # Check rmd_path is a sub-dir in a Programme DIR, which is a sub-dir to the root of an ORGANISATION:
  # run dirname TWICE as want to ensure rmd_path is a sub-dir in a Programme!
  orgPath <- dirname( dirname(rmd_path) )

  orgPath <- findOrgDir(orgPath)

  if(orgPath == "" ) {
    # the search reached the root of the filesystem without finding the Organisation files,
    # therefore, rmd_path is not inside a PROGRAMME sub-dir!
    stop( paste0("  rmd_path is not a Project Doc or Note - not in a sub-dir of a PROGRAMME Directory: ", rmd_path) )
  }
  # now, orgPath should be the root dir of the organisation

  # build the datatable text vector
  col_names <- names(df)[2:length(names(df))]

  # data - create blank list
  data <- list()
  for(i in 2:length(df) ) {
    data[[i-1]] <- df[[i]]  # concat each data vector to list
  }
   #data <- lapply(df, function(x) x[x != ""])
   #data <- data[2:length(data)]

  ID_col <- "ID"
  IDs <- df$ID
  data_cols <- col_names
  data <- data
  dt_function <- dt_function
  datatable_name <- name
  MAX_DATATABLE_LENGTH <- dt_length
  DATATABLE_SPACER_CHAR <- "="

  data_tables <- build_datatable_from_dataframe(ID_col, IDs, data_cols, data, dt_function,
                                 datatable_name, MAX_DATATABLE_LENGTH,
                                 DATATABLE_SPACER_CHAR)

  #ID_col <- "ID"
  #IDs <- df$ID
  #data_cols <- col_names
  #data <- data
  #dt_function <- "CREATE"
  #datatable_name <- name
  #MAX_DATATABLE_LENGTH <- dt_length
  #DATATABLE_SPACER_CHAR

  # write these to the file:
  cat( "\n  write data table(s) to Rmd at line: ", rmd_line )
  rmd_contents <- c( rmd_contents[1:rmd_line], data_tables, rmd_contents[(rmd_line+1):length(rmd_contents)] )

  rmd_file_conn <- file( rmd_path )
  writeLines(rmd_contents, rmd_file_conn)
  close(rmd_file_conn)


}





