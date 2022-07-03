#' Create new Sample Data Table and insert into Rmd
#'
#' Creates a new Sample DataTable in specified Rmd file at specified line.
#' The Sample DataTable will contain an initial ID column containing the
#' IDs vector, and an optional set of extra data columns as specified in the
#' data_cols vector.  If the table exceeds dt_length characters (default 120),
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
#' @param dt_length Int of data table max length in characters - default 120.
#'
#' @export
datatable_create_rmd <- function( rmd_path, rmd_line,
                              IDs="", data_cols="",
                              datatable_name = "samples", dt_length = 120 ) {

  cat( "\nprojectmanagr::datatable_create_rmd():\n" )

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

  # normalize path - remove HOME REF ~
  rmd_path <- normalizePath(rmd_path)

  cat( "  Reading Rmd...\n" )

  # read rmd_path file:
  rmd_file_conn <- file( rmd_path )
  rmd_contents <- readLines( rmd_file_conn )
  close(rmd_file_conn)

  data_tables <- datatable_create( IDs, data_cols, datatable_name, dt_length )

  # write these to the file:
  cat( "\n  write data table(s) to Rmd at line: ", rmd_line )
  rmd_contents <- c( rmd_contents[1:rmd_line], data_tables, rmd_contents[(rmd_line+1):length(rmd_contents)] )

  rmd_file_conn <- file( rmd_path )
  writeLines(rmd_contents, rmd_file_conn)
  close(rmd_file_conn)


}


#' Create new Sample Data Table
#'
#' Creates a new Sample DataTable vector. The Sample DataTable will contain an
#' initial ID column containing the IDs vector, and an optional set of extra
#' data columns as specified in the data_cols vector.  If the table exceeds
#' `dt_length` characters (default 120), then the table is split into multiple
#' tables, with IDs as first col, and subsequent data_cols given in subsequent
#' tables.
#'
#' @param IDs Character vector of sample IDs
#'
#' @param data_cols Character vector of data table column titles
#'
#' @param datatable_name String of data table name - default "samples".
#'
#' @param dt_length Int of data table max length in characters - default 120.
#'
#' @export
datatable_create <- function( IDs="", data_cols="",
                              datatable_name = "samples", dt_length = 120 ) {

  cat( "\nprojectmanagr::datatable_create():\n" )

  # CHECK data_cols are all unique
  if( anyDuplicated(data_cols) != 0 ) {
    stop( paste0("  duplicate column headers: ", data_cols[duplicated(data_cols)]) )
  }
  ### NAMED CONSTANTS ###
  DATATABLE_SPACER_CHAR <- "="
  obs_default_val_3 <- "VAL" # fill all data_cols with this value
  obs_default_val_5 <- "VALUE" # fill all data_cols with this value
  obs_default_val_8 <- "DATA_VAL" # fill all data_cols with this value
  obs_default_val_dt <- "INSERT__DATETIME" # this occupies 16 chars
  # same length as datetime String: 2021/02/26:18:35

  # set IDs to a value if needed:
  if( IDs[1] == "" ) {
    IDs <- obs_default_val_8
  }

  # determine widths of data cols
  data_col_wds <- c()
  for(i in 1:length(data_cols) ) {
    # if _dt col - must be 18 long to fit datetime: 2021/02/26:17:42
    if( endsWith(data_cols[i], "_dt") ) {
      data_col_wds[i] <- pmax(nchar(data_cols[i])+4, 18)
    } else {
      data_col_wds[i] <- pmax(nchar(data_cols[i])+4, 5) #pmax ensures min col width is 5!
    }
  }

  # use data_col_wds to calc correct length of default_data_vals:
  default_data_vals <- list()
  for( i in 1:length(data_cols) ) {
    # add default data vals to the data col for each ID
     # first compute the most appropriate default data val to add - based on width of column
    if( endsWith(data_cols[i], "_dt") ) {
      default_data_vals[[i]] <- rep(obs_default_val_dt, length(IDs) )
    } else if( data_col_wds[i] > 9 ) {
      default_data_vals[[i]] <- rep(obs_default_val_8, length(IDs) )
    } else if( data_col_wds[i] > 6 ) {
      default_data_vals[[i]] <- rep(obs_default_val_5, length(IDs) )
    } else {
      default_data_vals[[i]] <- rep(obs_default_val_3, length(IDs) )
    }
  }

  data_tables <- build_datatable("ID", IDs, data_cols, default_data_vals,
                                 "CREATE", datatable_name, dt_length,
                                 DATATABLE_SPACER_CHAR)

  # return
  data_tables

}



#' Create new Sample Data Table and insert into Rmd
#'
#' Creates a Sample DataTable in specified Rmd file from the template that
#' should be present between specified start & end line.
#'
#' The template Sample DataTable will contain an initial ID column containing
#' the IDs vector, and any other columns will have their initial data value
#' replicated along the length of the datatable.
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
#' @param dt_length Int of data table max length in characters - default 120.
#'
#' @export
datatable_create_template_rmd <- function( rmd_path, rmd_startline, rmd_endline,
                                  IDs="", datatable_name = "samples", dt_length = 120 ) {

  cat( "\nprojectmanagr::datatable_create_template_rmd():\n" )


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

  # normalize path - remove HOME REF ~
  rmd_path <- normalizePath(rmd_path)

  cat( "  Reading Rmd...\n" )

  # read rmd_path file:
  rmd_file_conn <- file( rmd_path )
  rmd_contents <- readLines( rmd_file_conn )
  close(rmd_file_conn)

  # get the template
  template_table <- rmd_contents[ rmd_startline : rmd_endline ]

  data_tables <- datatable_create_template( IDs, template_table, datatable_name, dt_length )

  # write these to the file:
  cat( "\n  write data table(s) to Rmd at line: ", rmd_startline )
  rmd_contents <- c( rmd_contents[1:(rmd_startline-1)], data_tables, rmd_contents[(rmd_endline+1):length(rmd_contents)] )

  rmd_file_conn <- file( rmd_path )
  writeLines(rmd_contents, rmd_file_conn)
  close(rmd_file_conn)

}


#' Create new Sample Data Table from Template
#'
#' Creates a new Sample DataTable vector. The Sample DataTable will contain an
#' initial ID column containing the IDs vector, and an optional set of extra
#' data columns as specified in the data_cols vector.  If the table exceeds
#' `dt_length` characters (default 120), then the table is split into multiple
#' tables, with IDs as first col, and subsequent data_cols given in subsequent
#' tables.
#'
#' @param IDs Character vector of sample IDs
#'
#' @param template_table A character vector of the template table, as presented
#' in the Rmd file.
#'
#' @param datatable_name String of data table name - default "samples".
#'
#' @param dt_length Int of data table max length in characters - default 120.
#'
#' @export
datatable_create_template <- function( IDs="", template_table="",
                              datatable_name = "samples", dt_length = 120 ) {

  cat( "\nprojectmanagr::datatable_create_template():\n" )

  # extract as list
  template_list <- extract_datatable(template_table)

  # REPLACE IDs in the list
  template_list[[1]] <- c(template_list[[1]][1], IDs)

  # and add MULTIPLES of current content in each other vector in list according to IDs length
  for(i in 2:length(template_list) ) {
    template_list[[i]] <- c(template_list[[i]][1], rep(template_list[[i]][2], length(IDs) ) )
  }

  template_list <- template_list[2:length(template_list)]

  # get just the data_cols:
  data_cols <- unlist( lapply(template_list, `[[`, 1) )


  ### NAMED CONSTANTS ###
  DATATABLE_SPACER_CHAR <- "="

  # CHECK data_cols are all unique
  if( anyDuplicated(data_cols) != 0 ) {
    stop( paste0("  duplicate column headers: ", data_cols[duplicated(data_cols)]) )
  }

  # set IDs to a value if needed:
  if( IDs[1] == "" ) {
    IDs <- obs_default_val_8
  }

  # determine widths of data cols
  data_col_wds <- c()
  for(i in 1:length(template_list) ) {
      data_col_wds[i] <- max(nchar(template_list[[i]]))+2
  }

  # use data_col_wds to calc correct length of default_data_vals:
  default_data_vals <- list()
  for( i in 1:length(template_list) ) {
    # add default data vals to the data col for each ID
     # first compute the most appropriate default data val to add - based on width of column
      default_data_vals[[i]] <- template_list[[i]][ 2:length(template_list[[i]]) ]
  }

  data_tables <- build_datatable("ID", IDs, data_cols, default_data_vals,
                                 "CREATE", datatable_name, dt_length,
                                 DATATABLE_SPACER_CHAR)

  # return
  data_tables

}


#' Build Datatable
#'
#' Creates a new Sample DataTable vector. The Sample DataTable will contain an
#' initial ID column containing the IDs vector, and an optional set of extra
#' data columns as specified in the data_cols vector.  If the table exceeds
#' `dt_length` characters (default 120), then the table is split into multiple
#' tables, with IDs as first col, and subsequent data_cols given in subsequent
#' tables.
#'
#' This method also supports adding Multi-Observations:  Multiple data points
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
#' @param dt_function The FUNCTION of the datatable to be built.
#'
#' @param datatable_name String of data table name - default "samples".
#'
#' @param MAX_DATATABLE_LENGTH Int of data table max length in characters.
#' Typically 120.
#'
#' @param DATATABLE_SPACER_CHAR Character used to underline column headers.
#' Typically '='.
#'
#' @export
build_datatable <- function( ID_col, IDs, data_cols, data, dt_function,
                             datatable_name, MAX_DATATABLE_LENGTH,
                             DATATABLE_SPACER_CHAR ) {


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
        stop( paste0("  Number of data points added is not an exact multiple of IDs: ", data[[a]], " IDs: ", IDs) )
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


  # form initial SKELETON of each table to insert
  # HEADER, IDs col (with correct spacing from max_multiple_list), FOOTER

  # create a list to hold the generated data_tables in
  data_table_list <- list()

  for( a in 1:length(max_multiple_list) ) { # length max_multiple_list give number of TABLES to create

    # add HEADER - header info plus datatable column heads
    if( a == 1 ) {
      data_table_list[[a]] <- c(
        "+===============================================================================",
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
        "+===============================================================================",
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


    # concat data_table and add FOOTER
    data_table_list[[a]] <- c(
      data_table_list[[a]],
      data_table_ids,
      "",
      "+===============================================================================",
      "",
      "",
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

        #cat( "\n  data col::", data_cols[i], " index: ", i )
        #cat( "\n    data_col_wds:", data_col_wds[i] )
        #cat( "\n    col_spacers_len:", col_spacers_len )

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

  # return
  data_tables

}




OLD_datatable_build <- function() {


  # create a list to hold the generated data_tables in
  data_table_list <- list()
  data_table_id <- c()

  ### CREATE INITIAL DATATABLE LAYOUT WITH IDS IN PLACE ###


  # Create initial datatable for ids

  # add HEADER - header info plus datatable column heads
  data_table_id <- c(
    "+===============================================================================",
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

  # add IDs
  cat( "\n  add IDs" )
  data_table_ids <- character( (length(IDs)*(1+max_multiple)) ) # start with an EMPTY POPULATED vector
   # x(1+max_multiple) as enter the ID THEN number of blank lines in max_multiple to separate IDs in the table
    # gives correct spacing of IDs for adding multi observations in the data list
  for(i in 1:length(IDs) ) {

    # index must use max_multiple to calc position of IDs
    data_table_ids[(i*(1+max_multiple))-(max_multiple)] <- paste0(
      "    ",
      strrep(" ", ceiling( (ID_col_wd - nchar(IDs[i]))/2) ),
      IDs[i],
      strrep(" ", floor( (ID_col_wd - nchar(IDs[i]))/2) ),
      "  " ) # an ID - with correct spacing

  }

  # concat data_table and add FOOTER
  data_table_id <- c(
    data_table_id,
    data_table_ids,
    "",
    "+===============================================================================",
    "",
    "",
    ""    )

  # now copy data_table_id into data_table_list in first index and build the data table:
  data_table_list[[1]] <- data_table_id

  # setup some params
  col_spacers_len <- nchar(paste0("    ",
                                  strrep(DATATABLE_SPACER_CHAR, ID_col_wd),
                                  "  ") ) # set initially to current length of datatable - increment as cols are added
  col_spacers_len_id <- col_spacers_len # also cache the original length in case further datatables are added
  dt_index <- 1 # increment as more datatables are added

  #cat( "\n    data_col_wds:", data_col_wds )

  # only add cols if there are any to add!
  if( data_cols[1] != "" ) {

    cat( "\n  add data columns" )

    for( i in 1:length(data_cols) ) {

      cat( "\n    ", data_cols[i] )

      # first calc the number of chars to add to datatable
      col_spacers_len <- col_spacers_len + (data_col_wds[i]+2) #+2 for 2 spaces at end

      #cat( "\n  data col::", data_cols[i], " index: ", i )
      #cat( "\n    data_col_wds:", data_col_wds[i] )
      #cat( "\n    col_spacers_len:", col_spacers_len )

      # check this doesnt exceed the max:
      if( col_spacers_len > MAX_DATATABLE_LENGTH ) {
        #cat("\n MAX MET AT: ", i)
        # if it does, create a new datatable in the list, and set params to use this new DT
        dt_index <- (dt_index +1)
        data_table_list[[dt_index]] <- data_table_id
        data_table_list[[dt_index]][4] <- paste0("    ",datatable_name,"  :  ", dt_function_next) # set datatable header to ADD_DATA
        # prevents syntax error during the reading of data tables!  datatable_read_rmd()
        col_spacers_len <- col_spacers_len_id
      }

      # add the data col and the default data vals for each ID

      # col title is index 7
      data_table_list[[dt_index]][7] <- paste0(

        data_table_list[[dt_index]][7], # keep what is already in this String!

        strrep(" ", ceiling( (data_col_wds[i] - nchar(data_cols[i]))/2) ),

        data_cols[i],

        strrep(" ", floor( (data_col_wds[i] - nchar(data_cols[i]))/2) ),

        "  " ) # Col Title : - with correct spacing

      # col title is index 8
      data_table_list[[dt_index]][8] <- paste0(

        data_table_list[[dt_index]][8], # keep what is already in this String!

        strrep(DATATABLE_SPACER_CHAR, data_col_wds[i]),

        "  ") # Spacers under data col - '=' by default

      # add group vals across IDs by default
       # this will stagger vals in data across length of IDs
          # useful for adding all group vals across IDs!
      if( length(data[[i]]) != length(IDs) ) {
        dv <- c()
        for( j in 1:length(IDs) ) {
          dv[j] <- data[[i]][(j-1)%%length(data[[i]]) +1]
        }
      } else {
        dv <- data[[i]]
      }

      for( j in 1:length(IDs) ) {

        data_table_list[[dt_index]][9+(j*2)-1] <- paste0(

          data_table_list[[dt_index]][9+(j*2)-1], # keep what is already in this String!

          strrep(" ", ceiling( (data_col_wds[i] - nchar(dv[j]))/2) ),

          dv[j],

          strrep(" ", floor( (data_col_wds[i] - nchar(dv[j]))/2) ),

          "  " ) # a data entry at ID [9+(j*2)-1] - this gives ID line index : add correct spacing to line entry

      }

    }
  }

  # data_table_list has one or more data_tables with correct spacing, and width does not exceed MAX_DATATABLE_LENGTH

  # generate single vector containing all data tables:
  data_tables <- unlist(data_table_list)

  # return
  data_tables

}



#' Create new Sample Data Table and insert into Rmd
#'
#' Creates a new Sample DataTable in specified Rmd file at specified line.
#' The Sample DataTable will contain an initial ID column containing the
#' IDs vector, and an optional set of extra data columns as specified in the
#' data_cols vector.  If the table exceeds dt_length characters (default 120),
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
#' @param dt_length Int of data table max length in characters - default 120.
#'
#' @param summarise_reps Boolean to indicate whether reps should be summarised in
#' the datatable.  If FALSE each ID/rep is on a separate line in the new datatable,
#' otherwise if TRUE, all reps are summarised using r vector index syntax on
#' one line in the new datatable.  i.e. each ID is listed ONCE and the reps are
#' indicated as: 1:3,5,6:10,12,14:25 etc.  Default to FALSE.
#'
#' @export
datatable_add_data_samples_rmd <- function( rmd_path, rmd_line, data_cols, datatable_name,
                                            ids_vector="", dt_length = 120, summarise_reps = FALSE  ) {

  cat( "\nprojectmanagr::datatable_add_data_samples_rmd():\n" )

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

  # normalize path - remove HOME REF ~
  rmd_path <- normalizePath(rmd_path)

  cat( "  Reading Rmd...\n" )

  # read rmd_path file:
  rmd_file_conn <- file( rmd_path )
  rmd_contents <- readLines( rmd_file_conn )
  close(rmd_file_conn)

  data_tables <- datatable_add_data_samples( rmd_contents[1:rmd_line], data_cols, datatable_name,
                                             ids_vector, dt_length, summarise_reps )

  # write these to the file:
  cat( "\n  write data table(s) to Rmd at line: ", rmd_line )
  rmd_contents <- c( rmd_contents[1:rmd_line], data_tables, rmd_contents[(rmd_line+1):length(rmd_contents)] )

  rmd_file_conn <- file( rmd_path )
  writeLines(rmd_contents, rmd_file_conn)
  close(rmd_file_conn)

}


#' Add new Samples Data to Data Table
#'
#' Adds new Sample data to an EXISTING Data Table in specified Rmd file at
#' specified line.  The Sample DataTable will contain an initial ID column
#' containing all the EXISTING IDs (IDs that have been resampled or exported
#' will automatically be EXCLUDED), and an optional set of extra data columns as
#' specified in the data_cols vector.  If the table exceeds dt_length characters
#' (default 120), then the table is split into multiple tables, with IDs as
#' first col, and subsequent data_cols given in subsequent tables.
#'
#' The function assumes all EXISTING REPS of the current samples are going to
#' be resampled: if more than one rep exists for any of the samples, this table
#' will add the first rep column and fill it with all the EXISTING rep numbers.
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
#' IDs (which must already be declared in datatable name under a 'group-' data
#' column), or (a subset of) IDs from datatable.
#'
#' @param dt_length Int of data table max length in characters - default 120.
#'
#' @param summarise_reps Boolean to indicate whether reps should be summarised in
#' the datatable.  If FALSE each ID/rep is on a separate line in the new datatable,
#' otherwise if TRUE, all reps are summarised using r vector index syntax on
#' one line in the new datatable.  i.e. each ID is listed ONCE and the reps are
#' indicated as: 1:3,5,6:10,12,14:25 etc.  Default to FALSE.
#'
#' @export
datatable_add_data_samples <- function( contents, data_cols, datatable_name,
                                        ids_vector="", dt_length = 120, summarise_reps = FALSE  ) {


  # DEFINE NAMED CONSTANTS
  # GET DATATABLES AND CHECK VALIDITY
  # DEFINE IDs and REPs


  cat( "\nprojectmanagr::datatable_add_data_samples():\n" )

  # CHECK data_cols are all unique
  if( anyDuplicated(data_cols) != 0 ) {
    stop( paste0("  duplicate column headers: ", data_cols[duplicated(data_cols)]) )
  }

  ### NAMED CONSTANTS ###
  DATATABLE_SPACER_CHAR <- "="
  obs_default_val_3 <- "VAL" # fill all data_cols with this value
  obs_default_val_5 <- "VALUE" # fill all data_cols with this value
  obs_default_val_8 <- "DATA_VAL" # fill all data_cols with this value
  obs_default_val_dt <- "INSERT__DATETIME" # this occupies 16 chars
    # same length as datetime String: 2021/02/26:18:35

  # parse all lines in contents to extract all datatables:
  datatables <- datatable_read_vector(contents)

  # identify datatable_name and extract its ID column
   # FIRST check datatable_name EXISTS in rmd_contents!
  if( any(names(datatables) == datatable_name) == FALSE ) {
    # no datatable of name datatable_name to add data to - STOP
    stop( paste0("  No datatable of this name exists in vector: ", datatable_name ) )
  }
   # ?? THEN check col ID exists ??:
  if( any( names( datatables[[ datatable_name ]] ) == "ID" ) == FALSE ) {
    stop( paste0("  Column ID missing from datatable: ", datatable_name ) )
  }

  # create boolean rep_exists initiate as FALSE - so group/ALL processing is handled correctly below with datatable creation!
  rep_exists <- FALSE

  # DEFINE IDs:
  if( length(ids_vector)== 1 && ids_vector == "" ) {
    # if this is BLANK, define IDs as ALL IDs that EXIST:

    # FIRST split whether rep col exists && summairse_reps is TRUE:
    rep_exists <- any( names( datatables[[ datatable_name ]]) == "rep" )

    if( summarise_reps == FALSE || rep_exists == FALSE) {

      # get ALL EXISTING IDs - all that have NOT been exported, disposed, resampled
      if( rep_exists == TRUE ) { # also get EXISTING REPs if rep_exists
        ID_rep <- check_divisions_ids_reps( datatables[[datatable_name]] )
        IDs <- ID_rep$IDs
        REPs <- ID_rep$REPs
      } else {
        IDs <- check_divisions_ids( datatables[[datatable_name]] )
      }

    } else if( summarise_reps == TRUE && rep_exists == TRUE ) {
       # rep_exists is TRUE - so need to process IDs and REPs accordingly

      # get ALL EXISTING IDs & REPs - all that have NOT been exported, disposed, resampled
      ID_rep <- check_divisions_ids_reps( datatables[[datatable_name]] )
      IDs <- ID_rep$IDs
      REPs <- ID_rep$REPs

      # NOW process IDs and REPs so there is ONE PER ID - summarise REPs!
      ID_rep <- summarise_id_rep(IDs, REPs)
      IDs <- ID_rep$IDs
      REPs <- ID_rep$REPs

    }

  } else {

    # ids_vector contains either ALL, <group-names>, <ID-names>
    # CHECK they are VALID

    gdt <- dplyr::select(datatables[[datatable_name]], dplyr::starts_with("group-"))
    iddt <- datatables[[ datatable_name ]]$ID

    if( length(ids_vector)==1 && ids_vector[1] == "ALL" ) {
      # all good - set IDs to ALL
      IDs <- ids_vector[1]

      # NEED TO SUMMARISE REPS IF THEY EXIST:
      rep_exists <- any( names( datatables[[ datatable_name ]]) == "rep" )

      if(rep_exists == TRUE) {
        summarise_reps <- TRUE
        REPs <- "ALL" # use special summary syntax - ALL - for the rep col!
      }

    } else {
      # check each ids_vector - first through all IDs, then through all groups

      if( any(iddt == ids_vector[1]) ) { # CHECK IDs - only check first index to prevent subtle bug
            # comparing vectors of differnet lengths does so with recycling, which may miss the value!

        pass <- TRUE
        for(i in 1:length(ids_vector) ) {
          if(any(iddt == ids_vector[i]) == FALSE) {
            pass <- FALSE
          }
        }

        if(pass == FALSE) {
          stop( paste0("  Non-existant ID in ids_vector: ", ids_vector ) )
        }

        # if this passes, set IDs to ids_vector
        IDs <- ids_vector # THIS MAY BE A SUBSET OF IDs!!

        # NEED TO GET REPS IF THEY EXIST:
        rep_exists <- any( names( datatables[[ datatable_name ]]) == "rep" )

        if( rep_exists == TRUE) { # for each ID in IDs, get the VALID reps:
           # first get all IDs/REPs
          ID_rep <- check_divisions_ids_reps( datatables[[datatable_name]] )
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
        }

        if(summarise_reps == TRUE) {
          # summarise the REPs variable (and IDs variable!)
          ID_rep <- summarise_id_rep(IDs, REPs)
          IDs <- ID_rep$IDs
          REPs <- ID_rep$REPs
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

      } else {
        # no matches for ID or GROUPs - FAIL
        stop( paste0("  Could not identify ids_vector as IDs or group names: ", ids_vector ) )
      }

    }

  }

  # CHECK none of data_cols already exists in datatables[[datatable_name]]
  dc <- c(data_cols, names(datatables[[datatable_name]]) )
  # CHECK data_cols are all unique
  if( anyDuplicated(dc) != 0 ) {
    stop( paste0("  Column headers already exist in datatable: ", dc[duplicated(dc)]) )
  }


  # THEN check col rep exists - if so must copy the REPS to the new datatable
   # col rep is what is defined when a resampling generates many SECTIONS or REPS
   # its NOT the same as reps!
  # THIS ENSURES REPS ARE SUPPORTED IN ADD DATA TO SAMPLES!
  if( rep_exists == TRUE ) {
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

  # use data_col_wds to calc correct length of default_data_vals:
  default_data_vals <- list()
  for( i in 1:length(data_cols) ) {
    # add default data vals to the data col for each ID
    # first compute the most appropriate default data val to add - based on width of column
    if( data_cols[i] == "rep" ) {

      #if( summarise_reps == TRUE && rep_exists == TRUE ) { # summarising reps to r vector format
        # format stored in string - REPs
        default_data_vals[[i]] <- REPs

      #} else { # just use the rep col from datatable:
       # default_data_vals[[i]] <- datatables[[ datatable_name ]][[ "rep" ]]
      #}

    } else if( endsWith(data_cols[i], "_dt") ) {
      default_data_vals[[i]] <- rep(obs_default_val_dt, length(IDs) )
    } else if( data_col_wds[i] > 9 ) {
      default_data_vals[[i]] <- rep(obs_default_val_8, length(IDs) )
    } else if( data_col_wds[i] > 6 ) {
      default_data_vals[[i]] <- rep(obs_default_val_5, length(IDs) )
    } else {
      default_data_vals[[i]] <- rep(obs_default_val_3, length(IDs) )
    }
  }

  data_tables <- build_datatable("ID", IDs, data_cols, default_data_vals,
                                 "ADD_DATA", datatable_name, dt_length,
                                 DATATABLE_SPACER_CHAR)

  # return
  data_tables

}


#'
#'
#'
summarise_id_rep <- function(IDs, REPs) {

  # convert reps to r vector syntax : 1:3,5,8:10 etc
  IDs_unique <- unique(IDs)
  IDs_dup <- table(IDs) # gets the COUNTS of duplicated IDs - use to loop through each ID value

  REPs_unique <- c() # use to store the new reps string

  start_index <- 1
  seq_seen <- FALSE
  for( g in 1:length(IDs_dup) ) { # loop each unique ID val

    REPs_unique <- c(REPs_unique, REPs[start_index]) # ALWAYS will have the first val - must exist

    for(h in (start_index+1):sum(IDs_dup[1:g]) ) { # loop through each INSTANCE of ID - to look at the REPs val for this ID

      if( ( as.numeric(REPs[h]) )  ==  ( as.numeric(REPs[h-1]) + 1 ) ) {
        # integers form a SEQUENCE
        # mark a sequence has been seen:
        seq_seen <- TRUE

        if( h == sum(IDs_dup[1:g]) ) {
          #cat("end")
          # FINALLY - deal with the end of sequence if seq_seen:
          if(seq_seen == TRUE) {
            REPs_unique[g] <- paste0(REPs_unique[g], ":", REPs[h])
          }
        }
      } else if( ( as.numeric(REPs[h]) )  !=  ( as.numeric(REPs[h-1]) + 1 ) ) {
        # integers BREAK a sequence
        # will add a COMMA and the current val in REPs (REPs[h]), but only after adding the seq to the string

        if(seq_seen == FALSE) {
          REPs_unique[g] <- paste0(REPs_unique[g], ",", REPs[h])
        } else {
          REPs_unique[g] <- paste0(REPs_unique[g], ":", REPs[h-1], ",", REPs[h])
        }
        seq_seen <- FALSE # reset to FALSE - as no seq has been seen in this new run!
      } else if( h == sum(IDs_dup[1:g]) ) {

        # FINALLY - deal with the end of sequence if seq_seen:
        if(seq_seen == TRUE) {
          REPs_unique[g] <- paste0(REPs_unique[g], ":", REPs[h-1], ",", REPs[h])
        }
      }

    }

    start_index <- sum(IDs_dup[1:g]) + 1 # now restart start_index to start of NEXT IDs_dup val!

  }

  # NOW IDs_unique gives each existing ID only ONCE
  # AND REPs_unique gives the r vector formatted string of reps : 1:3,5,8:10

  # set these to the original variables
  IDs <- IDs_unique
  REPs <- REPs_unique

  # return as named list:
  lst <- list(IDs, REPs)
  names(lst) <- c("IDs", "REPs")
  lst

}



#' Add Groups to Data Table and insert into Rmd
#'
#' Adds Groups to an EXISTING Data Table in specified Rmd file at specified line.
#' The Groups DataTable will contain an initial ID column containing all
#' sample IDs, and a series of extra data columns named using the
#' group_names vector.
#'
#' If the table exceeds dt_length characters
#' (default 120), then the table is split into multiple tables, with IDs as
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
#' @param dt_length Int of data table max length in characters - default 120.
#'
#' @param summarise_reps Boolean to indicate whether reps should be summarised in
#' the datatable.  If FALSE each ID/rep is on a separate line in the new datatable,
#' otherwise if TRUE, all reps are summarised using r vector index syntax on
#' one line in the new datatable.  i.e. each ID is listed ONCE and the reps are
#' indicated as: 1:3,5,6:10,12,14:25 etc.  Default to FALSE.
#'
#' @export
datatable_add_group_rmd <- function(rmd_path, rmd_line, group_names, datatable_name,
                                    groups, dt_length = 120, summarise_reps = FALSE  ) {

  cat( "\nprojectmanagr::datatable_group_rmd():\n" )

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

  # normalize path - remove HOME REF ~
  rmd_path <- normalizePath(rmd_path)

  cat( "  Reading Rmd...\n" )

  # read rmd_path file:
  rmd_file_conn <- file( rmd_path )
  rmd_contents <- readLines( rmd_file_conn )
  close(rmd_file_conn)

  data_tables <- datatable_add_group( rmd_contents[1:rmd_line], group_names, datatable_name, groups, dt_length, summarise_reps )

  # write these to the file:
  cat( "\n  write data table(s) to Rmd at line: ", rmd_line )
  rmd_contents <- c( rmd_contents[1:rmd_line], data_tables, rmd_contents[(rmd_line+1):length(rmd_contents)] )

  rmd_file_conn <- file( rmd_path )
  writeLines(rmd_contents, rmd_file_conn)
  close(rmd_file_conn)

}


#' Add Groups to Data Table
#'
#' Adds Groups to an EXISTING Data Table in the character vector `contents`.
#' The Groups DataTable will contain an initial ID column containing all
#' sample IDs, and a series of extra data columns named using the
#' group_names vector.
#'
#' If the table exceeds dt_length characters
#' (default 120), then the table is split into multiple tables, with IDs as
#' first col, and subsequent group_names given in subsequent tables.
#'
#' @param contents character vector containing current document with
#' existing data tables.
#'
#' @param group_names Character vector of data table column titles to add.
#' CANNOT BE BLANK!  MUST start all group names with `group-`.
#'
#' @param datatable_name String of data table name: This data table MUST ALREADY
#' EXIST/BE DECLARED BEFORE rmd_line in rmd_path.
#'
#' @param groups A list of vectors containing the group labels for each group
#' defined in group_names
#'
#' @param dt_length Int of data table max length in characters - default 120.
#'
#' @param summarise_reps Boolean to indicate whether reps should be summarised in
#' the datatable.  If FALSE each ID/rep is on a separate line in the new datatable,
#' otherwise if TRUE, all reps are summarised using r vector index syntax on
#' one line in the new datatable.  i.e. each ID is listed ONCE and the reps are
#' indicated as: 1:3,5,6:10,12,14:25 etc.  Default to FALSE.
#'
#' @export
datatable_add_group <- function( contents, group_names, datatable_name,
                                  groups, dt_length = 120, summarise_reps = FALSE   ) {


  cat( "\nprojectmanagr::datatable_add_groups():\n" )

  # check group_names - each entry starts with `group-` - if not FAIL:
  if( any( startsWith(group_names, "group-") == FALSE ) == TRUE ) {
    # stop this function and inform user to add prefix `group-`
    stop( paste0("  group names must start with prefix `group-`: ",
                 group_names[startsWith(group_names, "group-") == FALSE]) )
  }

  data_cols <- group_names
  # CHECK data_cols are all unique
  if( anyDuplicated(data_cols) != 0 ) {
    stop( paste0("  duplicate column headers: ", data_cols[duplicated(data_cols)]) )
  }

  ### NAMED CONSTANTS ###
  DATATABLE_SPACER_CHAR <- "="
  #obs_default_val_3 <- "VAL" # fill all data_cols with this value
  #obs_default_val_5 <- "VALUE" # fill all data_cols with this value
  #obs_default_val_8 <- "DATA_VAL" # fill all data_cols with this value
  #obs_default_val_dt <- "INSERT__DATETIME" # this occupies 16 chars
  # same length as datetime String: 2021/02/26:18:35

  # parse all lines in contents to extract all datatables:
  datatables <- datatable_read_vector(contents)

  # identify datatable_name and extract its ID column
  # FIRST check datatable_name EXISTS in rmd_contents!
  if( any(names(datatables) == datatable_name) == FALSE ) {
    # no datatable of name datatable_name to add data to - STOP
    stop( paste0("  No datatable of this name exists in vector: ", datatable_name ) )
  }

  # ?? THEN check col ID exists ??:
  if( any( names( datatables[[ datatable_name ]] ) == "ID" ) == FALSE ) {
    stop( paste0("  Column ID missing from datatable: ", datatable_name ) )
  }


  # get IDs/REPs

  # FIRST split whether rep col exists && summairse_reps is TRUE:
  rep_exists <- any( names( datatables[[ datatable_name ]]) == "rep" )

  if( summarise_reps == FALSE || rep_exists == FALSE) {

    # get ALL EXISTING IDs - all that have NOT been exported, disposed, resampled
    if( rep_exists == TRUE ) { # also get EXISTING REPs if rep_exists
      ID_rep <- check_divisions_ids_reps( datatables[[datatable_name]] )
      IDs <- ID_rep$IDs
      REPs <- ID_rep$REPs
    } else {
      IDs <- check_divisions_ids( datatables[[datatable_name]] )
    }

  } else if( summarise_reps == TRUE && rep_exists == TRUE ) {
    # rep_exists is TRUE - so need to process IDs and REPs accordingly

    # get ALL EXISTING IDs & REPs - all that have NOT been exported, disposed, resampled
    ID_rep <- check_divisions_ids_reps( datatables[[datatable_name]] )
    IDs <- ID_rep$IDs
    REPs <- ID_rep$REPs

    # NOW process IDs and REPs so there is ONE PER ID - summarise REPs!
    ID_rep <- summarise_id_rep(IDs, REPs)
    IDs <- ID_rep$IDs
    REPs <- ID_rep$REPs

  }


  # CHECK none of data_cols already exists in datatables[[datatable_name]]
  dc <- c(data_cols, names(datatables[[datatable_name]]) )
  # CHECK data_cols are all unique
  if( anyDuplicated(dc) != 0 ) {
    stop( paste0("  Column headers already exist in datatable: ", dc[duplicated(dc)]) )
  }


  # THEN check col rep exists - if so must copy the REPS to the new datatable
  # col rep is what is defined when a resampling generates many SECTIONS or REPS
  # its NOT the same as reps!
  # THIS ENSURES REPS ARE SUPPORTED IN ADD DATA TO SAMPLES!
  if( any( names( datatables[[ datatable_name ]] ) == "rep" ) == TRUE ) {
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

      } else if( endsWith(data_cols[i], "_dt") ) {
        default_data_vals[[i]] <- rep(obs_default_val_dt, length(IDs) )
      } else if( data_col_wds[i] > 9 ) {
        default_data_vals[[i]] <- rep(obs_default_val_8, length(IDs) )
      } else if( data_col_wds[i] > 6 ) {
        default_data_vals[[i]] <- rep(obs_default_val_5, length(IDs) )
      } else {
        default_data_vals[[i]] <- rep(obs_default_val_3, length(IDs) )
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


  data_tables <- build_datatable("ID", IDs, data_cols, groups, "GROUP",
                                 datatable_name, dt_length,
                                 DATATABLE_SPACER_CHAR)

  # return
  data_tables

}


datatable_Add_Groups2 <- function () {

  IDs <- datatables[[ datatable_name ]]$ID

  ### CREATE INITIAL DATATABLE LAYOUT WITH IDS IN PLACE ###

  # calc width of ID col
  # if IDs is blank string - convert to 8 SPACE default val:
  if( IDs[1] == "" ) {
    IDs <- obs_default_val_8
  }
  ID_col_wd <- max(nchar(IDs)) + 2

  # Create initial datatable for ids

  # add HEADER - header info plus datatable column heads
  data_table_id <- c(
    "+===============================================================================",
    "",
    "",
    paste0("    ",datatable_name,"  :  GROUP"),
    "",
    "",
    paste0("    ",
           strrep(" ", ceiling( (ID_col_wd - nchar("ID"))/2) ),
           "ID",
           strrep(" ", floor( (ID_col_wd - nchar("ID"))/2) ),
           "  " ), # Col Title : ID - with correct spacing
    paste0("    ",
           strrep(DATATABLE_SPACER_CHAR, ID_col_wd),
           "  " ), # Spacers under ID - '=' by default
    ""
  )

  # add IDs
  cat( "\n  add IDs" )
  data_table_ids <- character( (length(IDs)*2) ) # start with an EMPTY POPULATED vector
  # x2 as enter the ID THEN a blank line to separate IDs in the table
  for(i in 1:length(IDs) ) {

    data_table_ids[(i*2)-1] <- paste0(
      "    ",
      strrep(" ", ceiling( (ID_col_wd - nchar(IDs[i]))/2) ),
      IDs[i],
      strrep(" ", floor( (ID_col_wd - nchar(IDs[i]))/2) ),
      "  " ) # an ID - with correct spacing

  }

  # concat data_table and add FOOTER
  data_table_id <- c(
    data_table_id,
    data_table_ids,
    "",
    "+===============================================================================",
    "",
    "",
    ""    )

  # now copy data_table_id into data_table_list in first index and build the data table:
  data_table_list[[1]] <- data_table_id

  # determine widths of data cols
  data_col_wds <- c()
  for(i in 1:length(data_cols) ) {
    # if _dt col - must be 18 long to fit datetime: 2021/02/26:17:42
    if( endsWith(data_cols[i], "_dt") ) {
      data_col_wds[length(data_col_wds)+1] <- pmax(nchar(data_cols[i])+4, 18)
    } else {
      data_col_wds[length(data_col_wds)+1] <- pmax(nchar(data_cols[i])+4, 5) #pmax ensures min col width is 5!
    }
  }

  # setup some params
  col_spacers_len <- nchar(paste0("    ",
                                  strrep(DATATABLE_SPACER_CHAR, ID_col_wd),
                                  "  ") ) # set initially to current length of datatable - increment as cols are added
  col_spacers_len_id <- col_spacers_len # also cache the original length in case further datatables are added
  dt_index <- 1 # increment as more datatables are added

  #cat( "\n    data_col_wds:", data_col_wds )

  # only add cols if there are any to add!
  if( data_cols[1] != "" ) {

    cat( "\n  add data columns:" )

    for( i in 1:length(data_cols) ) {

      cat( "\n    ", data_cols[i] )

      # first calc the number of chars to add to datatable
      col_spacers_len <- col_spacers_len + (data_col_wds[i]+2) #+2 for 2 spaces at end

      #cat( "\n  data col::", data_cols[1], " index: ", i )
      #cat( "\n    data_col_wds:", data_col_wds[i] )
      #cat( "\n    col_spacers_len:", col_spacers_len )

      # check this doesnt exceed the max:
      if( col_spacers_len > MAX_DATATABLE_LENGTH ) {
        # if it does, create a new datatable in the list, and set params to use this new DT
        dt_index <- (dt_index +1)
        data_table_list[[dt_index]] <- data_table_id
        data_table_list[[dt_index]][4] <- paste0("    ",datatable_name,"  :  ADD_DATA") # set datatable header to ADD_DATA
        # prevents syntax error during the reading of data tables!  datatable_read_rmd()
        col_spacers_len <- col_spacers_len_id
      }

      # add the data col and the default data vals for each ID

      # col title is index 7 in char vector
      data_table_list[[dt_index]][7] <- paste0(

        data_table_list[[dt_index]][7], # keep what is already in this String!

        strrep(" ", ceiling( (data_col_wds[i] - nchar(data_cols[i]))/2) ),

        data_cols[i],

        strrep(" ", floor( (data_col_wds[i] - nchar(data_cols[i]))/2) ),

        "  " ) # Col Title : - with correct spacing

      # col title is index 8
      data_table_list[[dt_index]][8] <- paste0(

        data_table_list[[dt_index]][8], # keep what is already in this String!

        strrep(DATATABLE_SPACER_CHAR, data_col_wds[i]),

        "  ") # Spacers under data col - '=' by default


      # add default data vals to the data col for each ID

      # first compute the most appropriate default data val to add - based on width of column
      if( data_col_wds[i] > 9 ) {
        default_data_val <- obs_default_val_8
      } else if( data_col_wds[i] > 6 ) {
        default_data_val <- obs_default_val_5
      } else {
        default_data_val <- obs_default_val_3
      }

      # then add the data vals

      # add group vals across IDs by default
      if(groups[[i]][1] != "") {
          group <- groups[[i]]
      } else {
        group <- default_data_val # set to default data val if not groups vals available
      }
      for( j in 1:length(IDs) ) {

        dv <- group[(j-1)%%length(group) +1] # this alternates through vals in group through the for j loop

        data_table_list[[dt_index]][9+(j*2)-1] <- paste0(

          data_table_list[[dt_index]][9+(j*2)-1], # keep what is already in this String!

          strrep(" ", ceiling( (data_col_wds[i] - nchar(dv))/2) ),

          dv,

          strrep(" ", floor( (data_col_wds[i] - nchar(dv))/2) ),

          "  " ) # a data entry at ID [9+(j*2)-1] - this gives ID line index : add correct spacing to line entry

      }

    }
  }

  # data_table_list has one or more data_tables with correct spacing, and width does not exceed MAX_DATATABLE_LENGTH

  # generate single vector containing all data tables:
  data_tables <- unlist(data_table_list)

  # return
  data_tables
}



#' Create new Sample Data Table and insert into Rmd
#'
#' Creates a new Sample DataTable in specified Rmd file at specified line.
#' The Sample DataTable will contain an initial ID column containing the
#' IDs vector, and an optional set of extra data columns as specified in the
#' data_cols vector.  If the table exceeds dt_length characters (default 120),
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
#' @param dt_length Int of data table max length in characters - default 120.
#'
#' @export
datatable_add_data_variables_rmd <- function( rmd_path, rmd_line, var_names, datatable_name, group_names,
                                              dt_length = 120 ) {

  cat( "\nprojectmanagr::datatable_add_data_variables_rmd():\n" )

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

  # normalize path - remove HOME REF ~
  rmd_path <- normalizePath(rmd_path)

  cat( "  Reading Rmd...\n" )

  # read rmd_path file:
  rmd_file_conn <- file( rmd_path )
  rmd_contents <- readLines( rmd_file_conn )
  close(rmd_file_conn)

  data_tables <- datatable_add_data_variables( rmd_contents[1:rmd_line], var_names, datatable_name, group_names, dt_length )

  # write these to the file:
  cat( "\n  write data table(s) to Rmd at line: ", rmd_line )
  rmd_contents <- c( rmd_contents[1:rmd_line], data_tables, rmd_contents[(rmd_line+1):length(rmd_contents)] )

  rmd_file_conn <- file( rmd_path )
  writeLines(rmd_contents, rmd_file_conn)
  close(rmd_file_conn)

}


#' Add a variable-first data table
#'
#' for procedure data
#'
#' First column contains a list of procedures, subsequent column titles are
#' typically group IDs, and data values are in cells.
#'
#' @param contents character vector containing current document with
#' existing data tables.
#'
#' @param var_names Names of variables to be added to first column.
#'
#' @param datatable_name The EXISTING datatable from which the IDs must be drawn.
#'
#' @param group_names Vector of sample IDs or GROUP NAMES from a group set, which will
#' constitute the remaining column headers.  These will typically be group
#' names.
#'
#' @param dt_length Int of data table max length in characters - default 120.
#'
#' @export
datatable_add_data_variables <- function(contents, var_names, datatable_name, group_names,
                                         dt_length = 120  ) {


  cat( "\nprojectmanagr::datatable_add_data_variables():\n" )


  #cat( "\n  var_names: ", var_names )
  #cat( "\n  datatable_name: ", datatable_name )
  #cat( "\n  group_names: ", group_names )

  data_cols <- var_names
  # CHECK data_cols are all unique
  if( anyDuplicated(data_cols) != 0 ) {
    stop( paste0("  duplicate column headers: ", data_cols[duplicated(data_cols)]) )
  }

  ### NAMED CONSTANTS ###
  DATATABLE_SPACER_CHAR <- "="
  obs_default_val_3 <- "VAL" # fill all data_cols with this value
  obs_default_val_5 <- "VALUE" # fill all data_cols with this value
  obs_default_val_8 <- "DATA_VAL" # fill all data_cols with this value
  obs_default_val_dt <- "INSERT__DATETIME" # this occupies 16 chars
  # same length as datetime String: 2021/02/26:18:35

  # parse all lines in contents to extract all datatables:
  datatables <- datatable_read_vector(contents)

  # identify datatable_name

  # FIRST check datatable_name EXISTS in rmd_contents!
  if( any(names(datatables) == datatable_name) == FALSE ) {
    # no datatable of name datatable_name to add data to - STOP
    stop( paste0("  No datatable of this name exists in vector: ", datatable_name ) )
  }
  # TODO
  # THEN check the table has not been exported or resampled:

  # ?? THEN check col ID exists ??:
  if( any( names( datatables[[ datatable_name ]] ) == "ID" ) == FALSE ) {
    stop( paste0("  Column ID missing from datatable: ", datatable_name ) )
  }

  # save to local var:
  dt <- datatables[[ datatable_name ]]
  gdt <- dplyr::select(dt, dplyr::starts_with("group-")) # get all group cols in one dt

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

    cat( "  group_names are IDs\n" )

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
     stop( paste0("  group_names is invalid: Must be ALL, <all-sample-IDs> or names from EXISTING GROUP: ", group_names))
   }

  }

  # remove an NA from group_names
  group_names <- group_names[!is.na(group_names)]


  # CHECK none of data_cols already exists in dt
  dc <- c(data_cols, names(dt) )
  # CHECK data_cols are all unique
  if( anyDuplicated(dc) != 0 ) {
    stop( paste0("  Column headers already exist in datatable: ", dc[duplicated(dc)]) )
  }

  # determine default widths of data cols
  data_col_wds <- c()
  for(i in 1:length(group_names) ) {
    # if _dt col - must be 18 long to fit datetime: 2021/02/26:17:42
    if( any(endsWith(data_cols, "_dt")) ) { # if any data_col is dt add the dt length to wds
      data_col_wds[i] <- 18
    } else {
      data_col_wds[i] <- pmax(nchar(group_names[i])+2, 5) #pmax ensures min col width is 5!
    }
  }

  # use data_col_wds to calc correct length of default_data_vals:
  default_data_vals <- list()
  for( i in 1:length(group_names) ) {
    # add default data vals to the data col for each ID
    # first compute the most appropriate default data val to add - based on width of column
    if( any(endsWith(data_cols, "_dt"))) { # if any data_col is _dt add the dt default val
      default_data_vals[[i]] <- rep(obs_default_val_dt, length(IDs) )
    } else if( data_col_wds[i] > 9 ) {
      default_data_vals[[i]] <- rep(obs_default_val_8, length(IDs) )
    } else if( data_col_wds[i] > 6 ) {
      default_data_vals[[i]] <- rep(obs_default_val_5, length(IDs) )
    } else {
      default_data_vals[[i]] <- rep(obs_default_val_3, length(IDs) )
    }
  }

  data_tables <- build_datatable("variables", IDs, group_names, default_data_vals,
                                 "ADD_DATA", datatable_name, dt_length,
                                 DATATABLE_SPACER_CHAR)

  # return
  data_tables

}



#' Create new Sample Data Table and insert into Rmd
#'
#' Creates a new Sample DataTable in specified Rmd file at specified line.
#' The Sample DataTable will contain an initial ID column containing the
#' IDs vector, and an optional set of extra data columns as specified in the
#' data_cols vector.  If the table exceeds dt_length characters (default 120),
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
#' @param dt_length Int of data table max length in characters - default 120.
#'
#' @export
datatable_add_data_timetable_rmd <- function( rmd_path, rmd_line, step_names, datatable_name, group_names,
                                              dt_length = 120 ) {

  cat( "\nprojectmanagr::datatable_add_data_timetable_rmd():\n" )

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

  # normalize path - remove HOME REF ~
  rmd_path <- normalizePath(rmd_path)

  cat( "  Reading Rmd...\n" )

  # read rmd_path file:
  rmd_file_conn <- file( rmd_path )
  rmd_contents <- readLines( rmd_file_conn )
  close(rmd_file_conn)

  data_tables <- datatable_add_data_timetable( rmd_contents[1:rmd_line], step_names, datatable_name, group_names, dt_length )

  # write these to the file:
  cat( "\n  write data table(s) to Rmd at line: ", rmd_line )
  rmd_contents <- c( rmd_contents[1:rmd_line], data_tables, rmd_contents[(rmd_line+1):length(rmd_contents)] )

  rmd_file_conn <- file( rmd_path )
  writeLines(rmd_contents, rmd_file_conn)
  close(rmd_file_conn)

}

#' Create a time table - for staggered timings of procedures
#'
#' First column contains a list of timings, subsequent column titles are
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
#' @param group_names Vector of sample IDs or GROUP NAMES from a group set, which will
#' constitute the remaining column headers.  These will typically be group
#' names. Must EXIST in contents and be declared from datatable_name IDs!
#'
#' @param dt_length Int of data table max length in characters - default 120.
#'
#' @export
datatable_add_data_timetable <- function(contents, step_names, datatable_name, group_names,
                                         dt_length = 120 ) {

  cat( "\nprojectmanagr::datatable_add_data_timetable():\n" )

  # for timetable will add DEFAULT first and last column:
   # first col: timetable
    # will fill with RELATIVE TIMINGS of the protocol
   # last col: change_dt
    # will fill with ACTUAL DATETIME of execution of the step in the protocol

  # STEP NAMES will be added as data_cols when read and added to samples df!
  data_cols <- step_names
  # CHECK data_cols are all unique
  if( anyDuplicated(data_cols) != 0 ) {
    stop( paste0("  duplicate column headers: ", data_cols[duplicated(data_cols)]) )
  }
  # check step_names have no spaces
  if( any( grepl(" ", step_names) ) ) {
    stop( paste0("  step_names contains a space: ", step_names) )
    }

  ### NAMED CONSTANTS ###
  DATATABLE_SPACER_CHAR <- "="
  obs_default_val_3 <- "VAL" # fill all data_cols with this value
  obs_default_val_5 <- "VALUE" # fill all data_cols with this value
  obs_default_val_8 <- "DATA_VAL" # fill all data_cols with this value
  obs_default_val_dt <- "INSERT__DATETIME" # this occupies 16 chars
  # same length as datetime String: 2021/02/26:18:35

  # parse all lines in contents to extract all datatables:
  datatables <- datatable_read_vector(contents)

  # identify datatable_name

  # FIRST check datatable_name EXISTS in rmd_contents!
  if( any(names(datatables) == datatable_name) == FALSE ) {
    # no datatable of name datatable_name to add data to - STOP
    stop( paste0("  No datatable of this name exists in vector: ", datatable_name ) )
  }
  # TODO
  # THEN check the table has not been exported or resampled:

  # ?? THEN check col ID exists ??:
  if( any( names( datatables[[ datatable_name ]] ) == "ID" ) == FALSE ) {
    stop( paste0("  Column ID missing from datatable: ", datatable_name ) )
  }

  # save to local var:
  dt <- datatables[[ datatable_name ]]
  gdt <- dplyr::select(dt, dplyr::starts_with("group-")) # get all group cols in one dt

  # Set IDs (values to go in first col) to values 0:00 0:10, 0;20 .. up to number of step_names
  IDs <- c("0:00", paste0( "0:", seq(10, ((length(step_names)-1)*10), 10) ) )

  # CHECK the group_names exist in the datatable
  # either they are sample IDs, in which case they are from the $ID col
  # or it is the special group `ALL` - so just check this
  # or the group names are the names from a group declared in samples datatable
  if( length(group_names)==1 && group_names[1] == "ALL" ) {

     # no need to use timetable with ALL - so STOP and suggest var-first datatable
    cat( "  group_names is 'ALL'\n" )
    stop("  Cannot add timetable to ALL - use variable-first or sample-first layout: ")

  } else if( length(group_names) == length(dt$ID) && all( sort(group_names) ==  sort(dt$ID) ) ) {

    # SHOULD NOT ALLOW THIS
     # gets complicated to handle if IDs have reps
      # so can only add ALL or groups to this kind of datatable..
    stop( paste0("  group names CANNOT be IDs for timetable layout - create new group col and use this for timetable layout."))

    cat( "  group_names are IDs\n" )

  } else {

    # check all group cols
    group_names_match_group_cols <- c()
    for( i in 1:length(gdt) ) {
      group_names_match_group_cols[i] <- all( unique(gdt[i]) == group_names )
    }

    if( any(group_names_match_group_cols) == TRUE ) {
      # group_names represent a group
      cat( "  group_names is a group: ", names(gdt)[group_names_match_group_cols], "\n" )
    }
    else {
      # group_names is INVALID
      stop( paste0("  group_names is invalid: Must be ALL, <all-sample-IDs> or names from EXISTING GROUP: ", group_names))
    }

  }


  # CHECK none of data_cols already exists in dt
  dc <- c(data_cols, names(dt) )
  # CHECK data_cols are all unique
  if( anyDuplicated(dc) != 0 ) {
    stop( paste0("  Column headers already exist in datatable: ", dc[duplicated(dc)]) )
  }

  # determine default widths of data cols
  data_col_wds <- c()
  for(i in 1:length(group_names) ) {
    # if _dt col - must be 18 long to fit datetime: 2021/02/26:17:42
    if( endsWith(group_names[i], "_dt") ) {
      data_col_wds[i] <- 18
    } else {
      data_col_wds[i] <- pmax(
        nchar(group_names[i])+2,
        max(nchar(step_names))+2,
        5) #pmax ensures min col width is 5!
    }
  }

  # use data_col_wds to calc correct length of default_data_vals:
  default_data_vals <- list()
  for( i in 1:length(group_names) ) {
    # add default data vals to the data col for each ID
      # this is just the step_names for each col!
    default_data_vals[[i]] <- step_names
  }

  # now add the last col - change_dt
  group_names <- c(group_names, "change_dt")
  default_data_vals[[ length(group_names) ]] <- rep(obs_default_val_dt, length(IDs) )

  # create timetable:
  data_tables <- build_datatable("timetable", IDs, group_names, default_data_vals,
                                 "ADD_DATA", datatable_name, dt_length,
                                 DATATABLE_SPACER_CHAR)

  # return
  data_tables

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
#' @param dt_length Int of data table max length in characters - default 120.
#'
#' @param summarise_reps Boolean to indicate whether reps should be summarised in
#' the datatable.  If FALSE each ID/rep is on a separate line in the new datatable,
#' otherwise if TRUE, all reps are summarised using r vector index syntax on
#' one line in the new datatable.  i.e. each ID is listed ONCE and the reps are
#' indicated as: 1:3,5,6:10,12,14:25 etc.  Default to TRUE
#'
#' @export
datatable_dispose_rmd <- function( rmd_path, rmd_line, datatable_name,
                                   dt_length = 120, summarise_reps = TRUE ) {

  cat( "\nprojectmanagr::datatable_resample_rmd():\n" )

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

  # normalize path - remove HOME REF ~
  rmd_path <- normalizePath(rmd_path)

  cat( "  Reading Rmd...\n" )

  # read rmd_path file:
  rmd_file_conn <- file( rmd_path )
  rmd_contents <- readLines( rmd_file_conn )
  close(rmd_file_conn)

  data_tables <- datatable_dispose( rmd_contents[1:rmd_line], datatable_name, dt_length, summarise_reps )

  # write these to the file:
  cat( "\n  write data table(s) to Rmd at line: ", rmd_line )
  rmd_contents <- c( rmd_contents[1:rmd_line], data_tables, rmd_contents[(rmd_line+1):length(rmd_contents)] )

  rmd_file_conn <- file( rmd_path )
  writeLines(rmd_contents, rmd_file_conn)
  close(rmd_file_conn)

}



#' Dispose of a set of Samples
#'
#' This generates a new datatable that disposes of samples - indicates they no
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
#' @param dt_length Int of data table max length in characters - default 120.
#'
#' @param summarise_reps Boolean to indicate whether reps should be summarised in
#' the datatable.  If FALSE each ID/rep is on a separate line in the new datatable,
#' otherwise if TRUE, all reps are summarised using r vector index syntax on
#' one line in the new datatable.  i.e. each ID is listed ONCE and the reps are
#' indicated as: 1:3,5,6:10,12,14:25 etc.  Default to FALSE.
#'
#' @export
datatable_dispose <- function( contents, datatable_name, dt_length = 120, summarise_reps = TRUE ) {

  cat( "\nprojectmanagr::datatable_dispose():\n" )

  # parse all lines in contents to extract all datatables:
  datatables <- datatable_read_vector(contents)

  # define data cols : dispose ONLY
  data_cols <- c("dispose")

  ### NAMED CONSTANTS For this function ###
  DATATABLE_SPACER_CHAR <- "="
  cdt <- get_current_datetime_string()

  # Check datatable_name EXISTS in rmd_contents!
  if( any(names(datatables) == datatable_name) == FALSE ) {
    # no datatable of name datatable_name to add data to - STOP
    stop( paste0("  No datatable of this name exists in vector: ", datatable_name ) )
  }

  # ?? THEN check col ID exists ??:
  if( any( names( datatables[[ datatable_name ]] ) == "ID" ) == FALSE ) {
    stop( paste0("  Column ID missing from datatable: ", datatable_name ) )
  }


  # DEFINE IDs (& REPs if necessary):

  # determine if the samples have any REPS:
  reps_exist <- (any( names( datatables[[ datatable_name ]] ) == "rep" ) == TRUE)

  if( reps_exist == TRUE ) {

    # CHECK DIVISIONS of IDs and REPs:
     # check and remove all samples/reps that no longer exist in datatable
     # resample, dispose, export, split
    div_list <- check_divisions_ids_reps( datatables[[ datatable_name ]] )
     # get IDs and REPs
    IDs <- div_list[[1]]
    REPs <- div_list[[2]]

    if( summarise_reps == TRUE ) {

      # NOW process IDs and REPs so there is ONE PER ID - summarise REPs!
      ID_rep <- summarise_id_rep(IDs, REPs)
      IDs <- ID_rep$IDs
      REPs <- ID_rep$REPs

    }

    # add rep col to start of data_cols to add to new datatable
    data_cols <- c("rep", data_cols)

    # set all default_data_vals for cols
    # 1 rep
    # 2 dispose
    default_data_vals <- list()
    default_data_vals[[1]] <- REPs # first col is rep - fill with REPs
    default_data_vals[[2]] <- rep( cdt, length(IDs) ) # dispose col - fill with current datetime UTC


  } else if( reps_exist == FALSE ) {

    # CHECK DIVISIONS of IDs ONLY:
    # check and remove all samples/reps that no longer exist in datatable
    # resample, dispose, export, split
    IDs <- check_divisions_ids( datatables[[ datatable_name ]] )

    # set all default_data_vals for cols
    # 1 dispose
    default_data_vals <- list()
    default_data_vals[[1]] <- rep( cdt, length(IDs) ) # dispose col - fill with current datetime UTC

  }

  # build datatable to insert into Rmd:
  # using the OLD datatable_name - declaration of RESAMPLE table will END this datatable for IDs/reps in it
   # its still ADD_DATA - just using the dispose col!
  data_tables <- build_datatable("ID", IDs, data_cols, default_data_vals,
                                 "ADD_DATA", datatable_name, dt_length,
                                 DATATABLE_SPACER_CHAR)

  # return
  data_tables


}



get_current_datetime_string <- function() {

  # get datetime
  # using UTC for consistency - this may mean the time is DIFFERENT to the current time!
  datetime <- lubridate::now("UTC") #Sys.time()

  # round to nearest minute:
  datetime <- round(datetime,"mins")

  # convert to POSIXlt:
  datetime <- as.POSIXlt(datetime)

  # round to nearest 5 min - not using now keep round to nearest 1min!
  #datetime$min <- (datetime$min + 5/2) %/% 5 * 5

  # format datetime to use "/" and have a ":" between date and time
  datetime_split <- strsplit(as.character(datetime), " ")
  datetime_split[[1]][1] <- gsub("-", "/", datetime_split[[1]][1] )

  datetime_colon <- paste0( datetime_split[[1]][1], ":", datetime_split[[1]][2] )

  # remove seconds:
  datetime_colon <- substr(datetime_colon, 1, nchar(datetime_colon)-3)

  # return
  datetime_colon
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
#' @param dt_length Int of data table max length in characters - default 120.
#'
#' @export
datatable_resample_rmd <- function( rmd_path, rmd_line, datatable_name,
                                    resample_vector, dt_length = 120 ) {

  cat( "\nprojectmanagr::datatable_resample_rmd():\n" )

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

  # normalize path - remove HOME REF ~
  rmd_path <- normalizePath(rmd_path)

  cat( "  Reading Rmd...\n" )

  # read rmd_path file:
  rmd_file_conn <- file( rmd_path )
  rmd_contents <- readLines( rmd_file_conn )
  close(rmd_file_conn)

  data_tables <- datatable_resample( rmd_contents[1:rmd_line], datatable_name, resample_vector, dt_length )

  # write these to the file:
  cat( "\n  write data table(s) to Rmd at line: ", rmd_line )
  rmd_contents <- c( rmd_contents[1:rmd_line], data_tables, rmd_contents[(rmd_line+1):length(rmd_contents)] )

  rmd_file_conn <- file( rmd_path )
  writeLines(rmd_contents, rmd_file_conn)
  close(rmd_file_conn)

}

#' Resample a set of samples
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
#' the deleted samples will no longer be resampled when read into tibbles.
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
#' @param dt_length Int of data table max length in characters - default 120.
#'
#' @export
datatable_resample <- function( contents, datatable_name, resample_vector,
                                dt_length = 120 ) {

  cat( "\nprojectmanagr::datatable_resample():\n" )

  # parse all lines in contents to extract all datatables:
  datatables <- datatable_read_vector(contents)

  # define data cols : resample and reps
  data_cols <- c("resample", "reps")

  ### NAMED CONSTANTS For this function ###
  DATATABLE_SPACER_CHAR <- "="

  # Check datatable_name EXISTS in rmd_contents!
  if( any(names(datatables) == datatable_name) == FALSE ) {
    # no datatable of name datatable_name to add data to - STOP
    stop( paste0("  No datatable of this name exists in vector: ", datatable_name ) )
  }

  # ?? THEN check col ID exists ??:
  if( any( names( datatables[[ datatable_name ]] ) == "ID" ) == FALSE ) {
    stop( paste0("  Column ID missing from datatable: ", datatable_name ) )
  }

  # CHECK resample_vector
   # all entries must NOT contain an UNDERSCORE
    # UNDERSCORE is used to separate the RESAMPLINGS in the new datatable name
    # May use this to define the resamplings
  if( any( grepl("_", resample_vector) ) ) {
    stop( paste0("  resample_vector cannot contain any UNDERSCORES - use `-`: ", resample_vector[grepl("_", resample_vector)] ) )
  }


  # DEFINE IDs (& REPs if necessary):

  # determine if the samples have any REPS:
  reps_exist <- (any( names( datatables[[ datatable_name ]] ) == "rep" ) == TRUE)

  if( reps_exist == TRUE ) {

    # CHECK DIVISIONS of IDs and REPs:
      # check and remove all samples/reps that no longer exist in datatable
      # resample, dispose, export, split
    div_list <- check_divisions_ids_reps( datatables[[ datatable_name ]] )
     # get IDs and REPs
    IDs <- div_list[[1]]
    REPs <- div_list[[2]]

    # add rep col to start of data_cols to add to new datatable
    data_cols <- c("rep", data_cols)

    # set all default_data_vals for cols
     # 1 rep
     # 2 resample
     # 3 reps
    default_data_vals <- list()
    default_data_vals[[1]] <- REPs # first col is rep - fill with REPs
    default_data_vals[[2]] <- rep(resample_vector, length(IDs) ) # resample col - fill each ID with resample vector
    default_data_vals[[3]] <- rep("1", (length(IDs)*length(resample_vector)) ) # reps - fill be default with "1"


  } else if( reps_exist == FALSE ) {

    # CHECK DIVISIONS of IDs ONLY:
     # check and remove all samples/reps that no longer exist in datatable
     # resample, dispose, export, split
    IDs <- check_divisions_ids( datatables[[ datatable_name ]] )

    # set all default_data_vals for cols
     # 1 resample
     # 2 reps
    default_data_vals <- list()
    default_data_vals[[1]] <- rep(resample_vector, length(IDs) ) # resample col - fill each ID with resample vector
    default_data_vals[[2]] <- rep("1", (length(IDs)*length(resample_vector)) ) # reps - fill be default with "1"

  }

  # build datatable to insert into Rmd:
  # using the OLD datatable_name - declaration of RESAMPLE table will END this datatable for IDs/reps in it
  data_tables <- build_datatable("ID", IDs, data_cols, default_data_vals,
                                 "RESAMPLE", datatable_name, dt_length,
                                 DATATABLE_SPACER_CHAR)

  # return
  data_tables


}




#' Split Samples into separate datatables
#'
#' This is NOW DEPRECATED - not using SPLIT as this interferes with the FIND function:
#' find will summarise sample IDs and REPs from each Rmd into each sub-sample, which
#' is dictated by the resampling vector chain - which indicates WHAT TISSUES ARE
#' AVAILABLE!  By corrupting this with SPLIT codes, this becomes harder to use?
#'
#' Further, the get_history function will ELIMINATE any blank columns when reading
#' back to collect data on samples/reps so this is not really a problem?
#'
#' JUST USE GROUP - no need to split!
#'
datatable_split_rmd <- function(rmd_path, rmd_line, datatable_name,
                                split_vector, dt_length = 120 ) {

  cat( "\nprojectmanagr::datatable_resample_rmd():\n" )

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

  # normalize path - remove HOME REF ~
  rmd_path <- normalizePath(rmd_path)

  cat( "  Reading Rmd...\n" )

  # read rmd_path file:
  rmd_file_conn <- file( rmd_path )
  rmd_contents <- readLines( rmd_file_conn )
  close(rmd_file_conn)

  data_tables <- datatable_split( rmd_contents[1:rmd_line], datatable_name, split_vector, dt_length )

  # write these to the file:
  cat( "\n  write data table(s) to Rmd at line: ", rmd_line )
  rmd_contents <- c( rmd_contents[1:rmd_line], data_tables, rmd_contents[(rmd_line+1):length(rmd_contents)] )

  rmd_file_conn <- file( rmd_path )
  writeLines(rmd_contents, rmd_file_conn)
  close(rmd_file_conn)


}




#' Split Samples into separate datatables
#'
#' This is NOW DEPRECATED - not using SPLIT as this interferes with the FIND function:
#' find will summarise sample IDs and REPs from each Rmd into each sub-sample, which
#' is dictated by the resampling vector chain - which indicates WHAT TISSUES ARE
#' AVAILABLE!  By corrupting this with SPLIT codes, this becomes harder to use?
#'
#' Further, the get_history function will ELIMINATE any blank columns when reading
#' back to collect data on samples/reps so this is not really a problem?
#'
#' JUST USE GROUP - no need to split!
#'
datatable_split <- function(contents, datatable_name, split_vector,
                            dt_length = 120 ) {

  cat( "\nprojectmanagr::datatable_resample():\n" )

  # parse all lines in contents to extract all datatables:
  datatables <- datatable_read_vector(contents)

  # define data cols : resample and reps
  data_cols <- c("resample", "reps")

  ### NAMED CONSTANTS For this function ###
  DATATABLE_SPACER_CHAR <- "="

  # Check datatable_name EXISTS in rmd_contents!
  if( any(names(datatables) == datatable_name) == FALSE ) {
    # no datatable of name datatable_name to add data to - STOP
    stop( paste0("  No datatable of this name exists in vector: ", datatable_name ) )
  }

  # ?? THEN check col ID exists ??:
  if( any( names( datatables[[ datatable_name ]] ) == "ID" ) == FALSE ) {
    stop( paste0("  Column ID missing from datatable: ", datatable_name ) )
  }

  # CHECK split_vector
  # all entries must NOT contain an UNDERSCORE
  # UNDERSCORE is used to separate the RESAMPLINGS in the new datatable name
  # May use this to define the resamplings
  if( any( grepl("_", split_vector) ) ) {
    stop( paste0("  split_vector cannot contain any UNDERSCORES - use `-`: ", split_vector[grepl("_", split_vector)] ) )
  }


  # DEFINE IDs (& REPs if necessary):

  # determine if the samples have any REPS:
  reps_exist <- (any( names( datatables[[ datatable_name ]] ) == "rep" ) == TRUE)

  if( reps_exist == TRUE ) {

    # CHECK DIVISIONS of IDs and REPs:
    # check and remove all samples/reps that no longer exist in datatable
    # resample, dispose, export, split
    div_list <- check_divisions_ids_reps( datatables[[ datatable_name ]] )
    # get IDs and REPs
    IDs <- div_list[[1]]
    REPs <- div_list[[2]]

    # add rep col to start of data_cols to add to new datatable
    data_cols <- c("rep", data_cols)

    # set all default_data_vals for cols
    # 1 rep
    # 2 resample
    # 3 reps
    default_data_vals <- list()
    default_data_vals[[1]] <- REPs # first col is rep - fill with REPs
    default_data_vals[[2]] <- rep(split_vector, length(IDs) ) # resample col - fill each ID with resample vector
    default_data_vals[[3]] <- rep("1", (length(IDs)*length(split_vector)) ) # reps - fill be default with "1"


  } else if( reps_exist == FALSE ) {

    # CHECK DIVISIONS of IDs ONLY:
    # check and remove all samples/reps that no longer exist in datatable
    # resample, dispose, export, split
    IDs <- check_divisions_ids( datatables[[ datatable_name ]] )

    # set all default_data_vals for cols
    # 1 resample
    # 2 reps
    default_data_vals <- list()
    default_data_vals[[1]] <- rep(split_vector, length(IDs) ) # resample col - fill each ID with resample vector
    default_data_vals[[2]] <- rep("1", (length(IDs)*length(split_vector)) ) # reps - fill be default with "1"

  }

  # build datatable to insert into Rmd:
  # using the OLD datatable_name - declaration of RESAMPLE table will END this datatable for IDs/reps in it
  data_tables <- build_datatable("ID", IDs, data_cols, default_data_vals,
                                 "SPLIT", datatable_name, dt_length,
                                 DATATABLE_SPACER_CHAR)

  # return
  data_tables


}



#'
#' old code - not used!
old_resample <- function() {

  # SHOULD CHECK INDIVIDUAL ENTRIES HERE
  # selected datatable may already have DIVISIONS :
    # resample and reps cols
    # dispose col
    # export col
    # split col
  # if so dont just fail - check if they have been filled for the selected IDs!
    # THEN only generate a new resample datatable with remaining (EXISTING) sample IDs!

  # Its more convenient to add separate resamplings of tissue blocks in separate tables for set of samples
    # For example, may resample one SUBSET of samples as a PILOT : possibly in different ways!
      # THEN later may resample the REMAINING SAMPLES for full experiment
    # Resample function must support this - therefore must handle the resampling of samples from the same datatable
      # in different resample tables
    # also need method for returning all EXISTING SAMPLES/ROWS in all DATATABLES for doing this check

  # CHECK if the selected samples/datatable already contains a resampling col:

  dtIDs <- datatables[[ datatable_name ]]$ID
  IDs <- c()
  reps_existing <- c()

  if( all(IDs == "ALL") ) {

    # CHECK if datatable IDs have been RESAMPLED or EXPORTED
    dc <- c("resample", names(datatables[[datatable_name]]) )

    if( anyDuplicated(dc) != 0 ) {
      dtReps <- datatables[[ datatable_name ]]$rep
      # if any cols are duplicated means SOME/ALL(?) samples from this datatable have been resampled before!
      # so now check each sample ID - if it has NOT been resampled, add it to IDs
      for( i in 1:length(dtIDs) ) {
        if( is.na( datatables[[datatable_name]][["resample"]][i] ) == TRUE  ) {
          IDs <- c(IDs, dtIDs[i])
          reps_existing <- c(reps_existing, dtReps[i])
        }
      }
    } else {
      # all IDs in datatable are VALID - so copy them to IDs
      IDs <- datatables[[ datatable_name ]]$ID
    }

  } else { # else check IDs is valid
     # this is a cursory check - need to check IDs are NOT RESAMPLED - performed later
    for( i in 1:length(IDs) ) {
      if( any( dtIDs == IDs[i]) == FALSE ) {
        # no datatable of name datatable_name to add data to - STOP
        stop( paste0("  No ID of this name exists in datatable: ", IDs[i] ) )
      }
    }

    # check if any have been resampled:
    dc <- c("resample", names(datatables[[datatable_name]]) )
    if( anyDuplicated(dc) != 0 ) {
      # if any cols are duplicated means samples from this datatable have been resampled before!
      # so now check each sample ID - if it has been resampled, compare this to the IDs col
      for( i in 1:length(dtIDs) ) {
        if( is.na( datatables[[datatable_name]][["resample"]][i] ) == FALSE  ) {
          if( any( IDs == dtIDs[i]) == TRUE ) {
            # if dtIDs[i] is NOT blank in resample col AND is in the IDs vector, STOP:
            stop( paste0("  Sample ID has been previous resampled: ", dtIDs[i] ) )
          }
        }
      }
    }

  }

  # CHECK resample_vector
   # all entries must NOT contain an UNDERSCORE
   # uNDERSCORE is used to separate the RESAMPLINGS in the new datatable name
   # May use this to define the resamplings
  if( any( grepl("_", resample_vector) ) ) {
    stop( paste0("  resample_vector cannot contain any UNDERSCORES - use `-`: ", resample_vector[grepl("_", resample_vector)] ) )
  }

  # CHECK if rep_vector is length 1 or length of IDs
  if( (length(rep_vector) == 1 || length(rep_vector) == length(IDs)) == FALSE ) {
    stop( paste0("  rep_vector must be length 1 or length of IDs to be resampled: ", rep_vector) )
  }

  # make sure rep_vector is actually the length of resample_vector!
  if( length(rep_vector) == 1) {
    rep_vector <- rep(rep_vector, length(resample_vector))
  }

  # THEN check if col rep exists - if so must copy the REPS to the new datatable
   # col rep is what is defined when a resampling generates many SECTIONS or REPS
    # its NOT the same as reps!
   # THIS ENSURES REPS ARE SUPPORTED IN ADD DATA TO SAMPLES!
  if( any( names( datatables[[ datatable_name ]] ) == "rep" ) == TRUE ) {

    # add rep col to data_cols to add!
    data_cols <- c("rep", data_cols)

    # set all default_data_vals for rep resample and reps
    default_data_vals <- list()
    #default_data_vals[[1]] <- rep(reps_existing, each = length(resample_vector) ) # reps_existing is ALREADY LENGTH OF IDs!
     # lengthen EACH - 1111 2222 etc
    default_data_vals[[1]] <- reps_existing # set rep number for each ID val
    default_data_vals[[2]] <- rep(resample_vector, length(IDs) )
    default_data_vals[[3]] <- rep(rep_vector, length(IDs) )

  } else {

    # set default_data_vals for resample and reps
    default_data_vals <- list()
    default_data_vals[[1]] <- rep(resample_vector, length(IDs) )
    default_data_vals[[2]] <- rep(rep_vector, length(IDs) )

  }

  # build datatable to insert into Rmd:
   # using the OLD datatable_name - declaration of RESAMPLE table will END this datatable for IDs/reps in it
  data_tables <- build_datatable("ID", IDs, data_cols, default_data_vals,
                                 "RESAMPLE", datatable_name, dt_length,
                                 DATATABLE_SPACER_CHAR)

  # return
  data_tables

}



#' Find EXISTING Samples in Datatables
#'
#' Search through path recursively to find all Rmd files that contain datatables.
#' Summarise all EXISTING samples that are declared in them (not resampled,
#' exported, disposed).
#'
#' Summary tibble includes following columns:
#'
#' * ID - sample ID
#'
#' * SAMPLE: Composite of all subsampling - what the sample is
#'
#' * COUNT: Number of reps of the sample available (sections)
#'
#' * EXP: The experiment PREFIX ID and EXPERIMENT TITLE - title can help identify
#' where the tissue comes from/its properties - as using functional names for
#' experiments eg. `NRF~005-001~_SLEIGH_CNS_GARS_01_SC-LUM` know tissue is from
#' James Sleigh, and its GARS mutant or WT control.
#'
#' * PATH: The absolute path to the Experiment DIR ??
#'
#' * LOCATION : values of `_location` col IF LAST COL DECLARED - where is the
#' sample currently?  Only defined last if currently in storage!
#'
#' * CONDITION : value of last `_condition` col - what is sample in?
#'
#' If further metadata is needed - can use `datatable_get_sample_data_history`
#' and filter for relevant information.
#'
#' @return a tibble containing summary of all EXISTING samples
#'
datatable_find <- function( path, datatable_name_prefix = 'samples' ) {

  cat( "\nprojectmanagr::datatable_find():\n" )

  context <- rstudioapi::getSourceEditorContext()
  context$path <- path.expand(context$path) # expand "~" HOME

  # get all Project Notes inside subProgDir
  # they all contain "~_" in filename
  samplesList <- list.files(subProgDir, full.names = TRUE, recursive=TRUE)
  samplesList <- samplesList[ regexpr("~_", samplesList) > 0  ]

  if( length(samplesList) == 0 ) {
    # check if the subProgDir is defined from the orgDir:
    subProgDir <- paste0(findOrgDir(context$path), .Platform$file.sep, subProgDir )

    # get all Project Notes inside subProgDir
    # they all contain "~_" in filename
    samplesList <- list.files(subProgDir, full.names = TRUE, recursive=TRUE)
    samplesList <- samplesList[ regexpr("~_", samplesList) > 0  ]
  }

  # check subProgDir - make sure its in a org DIR
  subProgDir <- checkProgSubDir(subProgDir)


  # define a new Summary tibble to hold all samples:
  # MUST define a STANDARD TEMPLATE to hold SUMMARY DATA on samples
  # From this summary information, should be possible to select samples, or further explore them
  ID <- ""      # ID: Each Sample ID
  SAMPLE <- ""  # SAMPLE: COMPOSITE of all subsampling columns: CNS-RT-MB etc.
  COUNT <- integer()  # COUNT: How many REPS are there of this sample?
  PREFIX <- ""     # EXP: Fill with the Experiment Prefix ID
  TITLE <- ""   # TITLE: Fill with Experiment Title - will contain the LAB_TREATMENT
  #PATH <- ""    # PATH: Put the absolute PATH to the Project Note Rmd to Navigate to
  LOCATION <- "" # LOCATION of the sample, if in storage
  CONDITION <- "" # CONDITIONS of sample - what is it in?

  samples_summary <- tibble::tibble(ID, SAMPLE, COUNT, PREFIX, TITLE, LOCATION, CONDITION)

  for(s in samplesList) {
    # for each Rmd

    # get all datatables
    dts <- projectmanagr::datatable_read_rmd(s)

    # apply datatable_name_prefix filter
    dts <- dts[startsWith(names(dts), datatable_name_prefix)]

    for(d_index in 1:length(dts) ) {
      # for each dt

      # get the datatable, its name, and col_names
      d <- dts[[d_index]]
      d_name <- names(dts)[d_index]
      col_names <- names(d)

      # remove all samples that have been:
      # resampled
      if(is.element("resample", col_names) ) {
        d <- d[is.na(d$resample),] # remove all cols that are NOT NA - resampled
      }
      # exported
      if(is.element("export", col_names) ) {
        d <- d[is.na(d$export),] # remove all cols that are NOT NA - exported
      }
      # disposed
      if(is.element("dispose", col_names) ) {
        d <- d[is.na(d$dispose),] # remove all cols that are NOT NA - disposed
      }

      # summarise across reps
      #colsnorep <- col_names[!"rep" == col_names] # want to group by all EXCEPT rep
      #dg <- dplyr::group_by_at(d, colsnorep )
      dg <- dplyr::group_by_at(d, "ID" ) # group by the ID - this is ALWAYS THE FIRST COL!
      dc <- dplyr::summarise(dg, count=dplyr::n() )
      dc <- dplyr::ungroup(dc)

      if(endsWith(col_names[length(col_names)], "_location") ){
        dg <- dplyr::group_by_at(d, "ID" ) # group by the ID - this is ALWAYS THE FIRST COL!
        #dc <- dplyr::summarise(dg, count=dplyr::n(), location=dplyr::distinct() )
        dc <- dplyr::summarise(dg,
                               count=dplyr::n(),
                               location=as.character(dplyr::distinct(dg[col_names[length(col_names)]])) )
        dc <- dplyr::ungroup(dc)

      }
      else {
        LOC=""
      }

      # edit t - mutate to form ID, SAMPLE, COUNT, EXP
      dc <- dplyr::transmute(
        dc,
        ID=ID,
        SAMPLE=d_name,
        COUNT=count,
        PREFIX=projectmanagr::getProjectPrefixFromPath( s ),
        TITLE=basename(s),
        LOCATION=location )

    }

  }



}



#' Export a set of samples from summary datatable
#'
#' This function will write an EXPORT table to the end of
#'
datatable_export <- function() {

}



#' Import a set of samples and reps from a summary datatable
#'
#'
#'
datatable_import <- function() {

}



#' Get History of samples
#'
#' Retrieves all sample data, traversing all divisions (resample, export).
#'
#' Passes back through all Rmd's through imports, and identifies the CREATE
#' datatable for each sample.  Then collects all data, moving through all
#' resampling and exports, and omitting any blank columns.
#'
#' Returns a list of tibbles that contains all sample data, collating samples
#' where the data columns match.
#'
datatable_get_sample_data_history <- function( df ) {

}



#' Read datatables from ACTIVE RMD in RStudio
#'
#' Function to read all projectmanagr datatables declared in the active plain
#' text document (typically an R Markdown doc), and return them as tibbles.
#' Extracts all datatables between `+===` markers, and checks the validity of
#' each datatable declared.
#'
#' Each datatable is named, and each contains samples which must all possess a
#' unique ID.  Datatables are created in CREATE, and have data added to them
#' in ADD_DATA tables.  IDs from a given datatable can be put into new groups
#' using a GROUP table.
#'
#' Adding data can use various layouts:
#'
#' * Sample-first layout:  Typically used for measurements made on each sample
#' individually - sample IDs are in first col, subsequent cols contain the
#' measurement data.
#'
#' * Variable-first layout: Typically used to add datetimes of procedures -
#' procedure titles are laid out in first col, and subsequent cols are GORUPS
#' which are processed together.  This allows timing data to be added cleanly
#' and easily across all samples in a group (including the special group ALL).
#'
#' * TIMETABLE: Used for measuring the actual timings used during optimisation
#' of a protocol.  When time is varied in a protocol it is very difficult to
#' plan and track this.  The timetable provides a convenient layout for planning
#' the groups and the timings of changes over the procedure where timing is being
#' optimised.  It then allows the ACTUAL TIMINGS to be inserted into the table
#' as the procedures are followed - and these are linked to the original samples,
#' making this data available in further analyses.
#'
#' Samples in datatables can be subsampled - destroying the original sample,
#' and creating one or more sub-samples.  The parent samples can no longer
#' have data added to them, only the existing sub-samples.
#'
#' Samples in datatables are be exported to destination notes, and imported
#' from source notes.  If exported from this source note, the samples no longer
#' exist in the current note - they cannot be manipulated in this note.
#'
#' If imported from a previous source note, the samples now exist in this note.
#' They can have data added to them, grouped, subsampled, and exported.
#'
#' @export
datatable_read <- function() {

  datatable_read_rmd( rstudioapi::getSourceEditorContext()$path )

}

#' Read datatables upto SELECTION from ACTIVE RMD in RStudio
#'
#' Function to read all projectmanagr datatables up to cursor selection declared
#' in the active plain text document (typically an R Markdown doc), and return
#' them as tibbles. Extracts all datatables between `+===` markers, and checks
#' the validity of each datatable declared.
#'
#' Each datatable is named, and each contains samples which must all possess a
#' unique ID.  Datatables are created in CREATE, and have data added to them
#' in ADD_DATA tables.  IDs from a given datatable can be put into new groups
#' using a GROUP table.
#'
#' Adding data can use various layouts:
#'
#' * Sample-first layout:  Typically used for measurements made on each sample
#' individually - sample IDs are in first col, subsequent cols contain the
#' measurement data.
#'
#' * Variable-first layout: Typically used to add datetimes of procedures -
#' procedure titles are laid out in first col, and subsequent cols are GORUPS
#' which are processed together.  This allows timing data to be added cleanly
#' and easily across all samples in a group (including the special group ALL).
#'
#' * TIMETABLE: Used for measuring the actual timings used during optimisation
#' of a protocol.  When time is varied in a protocol it is very difficult to
#' plan and track this.  The timetable provides a convenient layout for planning
#' the groups and the timings of changes over the procedure where timing is being
#' optimised.  It then allows the ACTUAL TIMINGS to be inserted into the table
#' as the procedures are followed - and these are linked to the original samples,
#' making this data available in further analyses.
#'
#' Samples in datatables can be subsampled - destroying the original sample,
#' and creating one or more sub-samples.  The parent samples can no longer
#' have data added to them, only the existing sub-samples.
#'
#' Samples in datatables are be exported to destination notes, and imported
#' from source notes.  If exported from this source note, the samples no longer
#' exist in the current note - they cannot be manipulated in this note.
#'
#' If imported from a previous source note, the samples now exist in this note.
#' They can have data added to them, grouped, subsampled, and exported.
#'
#' @export
datatable_read_to_selection <- function() {

  row <- rstudioapi::getSourceEditorContext()$selection[[1]]$range$start[1]
  rmd_path <- normalizePath( rstudioapi::getSourceEditorContext()$path )

  # read rmd_path file:
  rmd_file_conn <- file( rmd_path )
  rmd_contents <- readLines( rmd_file_conn )
  close(rmd_file_conn)

  # read datatables upto selection
  datatables <- datatable_read_vector(rmd_contents[1:row] )

  # return
  datatables
}



#' Read datatables upto LINE in ACTIVE RMD in RStudio
#'
#' Function to read all projectmanagr datatables up to line number
#' in the active plain text document (typically an R Markdown doc), and return
#' them as tibbles. Extracts all datatables between `+===` markers, and checks
#' the validity of each datatable declared.
#'
#' Each datatable is named, and each contains samples which must all possess a
#' unique ID.  Datatables are created in CREATE, and have data added to them
#' in ADD_DATA tables.  IDs from a given datatable can be put into new groups
#' using a GROUP table.
#'
#' Adding data can use various layouts:
#'
#' * Sample-first layout:  Typically used for measurements made on each sample
#' individually - sample IDs are in first col, subsequent cols contain the
#' measurement data.
#'
#' * Variable-first layout: Typically used to add datetimes of procedures -
#' procedure titles are laid out in first col, and subsequent cols are GORUPS
#' which are processed together.  This allows timing data to be added cleanly
#' and easily across all samples in a group (including the special group ALL).
#'
#' * TIMETABLE: Used for measuring the actual timings used during optimisation
#' of a protocol.  When time is varied in a protocol it is very difficult to
#' plan and track this.  The timetable provides a convenient layout for planning
#' the groups and the timings of changes over the procedure where timing is being
#' optimised.  It then allows the ACTUAL TIMINGS to be inserted into the table
#' as the procedures are followed - and these are linked to the original samples,
#' making this data available in further analyses.
#'
#' Samples in datatables can be subsampled - destroying the original sample,
#' and creating one or more sub-samples.  The parent samples can no longer
#' have data added to them, only the existing sub-samples.
#'
#' Samples in datatables are be exported to destination notes, and imported
#' from source notes.  If exported from this source note, the samples no longer
#' exist in the current note - they cannot be manipulated in this note.
#'
#' If imported from a previous source note, the samples now exist in this note.
#' They can have data added to them, grouped, subsampled, and exported.
#'
#' @export
datatable_read_to_line <- function(line) {

  rmd_path <- normalizePath( rstudioapi::getSourceEditorContext()$path )

  # read rmd_path file:
  rmd_file_conn <- file( rmd_path )
  rmd_contents <- readLines( rmd_file_conn )
  close(rmd_file_conn)

  # read datatables upto selection
  datatables <- datatable_read_vector(rmd_contents[1:line] )

  # return
  datatables
}

#' Read datatables in Rmd
#'
#' Function to read all projectmanagr datatables declared in a plain text
#' document (typically an R Markdown doc), and return them as tibbles. Extracts
#' all datatables between `+===` markers, and checks the validity of each
#' datatable declared.
#'
#' Each datatable is named, and each contains samples which must all possess a
#' unique ID.  Datatables are created in CREATE, and have data added to them
#' in ADD_DATA tables.  IDs from a given datatable can be put into new groups
#' using a GROUP table.
#'
#' Adding data can use various layouts:
#'
#' * Sample-first layout:  Typically used for measurements made on each sample
#' individually - sample IDs are in first col, subsequent cols contain the
#' measurement data.
#'
#' * Variable-first layout: Typically used to add datetimes of procedures -
#' procedure titles are laid out in first col, and subsequent cols are GORUPS
#' which are processed together.  This allows timing data to be added cleanly
#' and easily across all samples in a group (including the special group ALL).
#'
#' * TIMETABLE: Used for measuring the actual timings used during optimisation
#' of a protocol.  When time is varied in a protocol it is very difficult to
#' plan and track this.  The timetable provides a convenient layout for planning
#' the groups and the timings of changes over the procedure where timing is being
#' optimised.  It then allows the ACTUAL TIMINGS to be inserted into the table
#' as the procedures are followed - and these are linked to the original samples,
#' making this data available in further analyses.
#'
#' Samples in datatables can be subsampled - destroying the original sample,
#' and creating one or more sub-samples.  The parent samples can no longer
#' have data added to them, only the existing sub-samples.
#'
#' Samples in datatables are be exported to destination notes, and imported
#' from source notes.  If exported from this source note, the samples no longer
#' exist in the current note - they cannot be manipulated in this note.
#'
#' If imported from a previous source note, the samples now exist in this note.
#' They can have data added to them, grouped, subsampled, and exported.
#'
#' @param rmd_path Path to RMarkdown Document.
#'
#' @export
datatable_read_rmd <- function(rmd_path) {

  cat( "\nprojectmanagr::datatable_read_rmd():\n" )

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

  # normalize path - remove HOME REF ~
  rmd_path <- normalizePath(rmd_path)

  # read rmd_path file:
  rmd_file_conn <- file( rmd_path )
  rmd_contents <- readLines( rmd_file_conn )
  close(rmd_file_conn)

  # read datatables
  datatables <- datatable_read_vector(rmd_contents)

  # return
  datatables

}


#' Read and return all ACTIVE datatables
#'
#' Return all datatables which contain sample IDs that are ACTIVE : they have
#' not been resampled or exported.
#'
#' Will exclude any tables that contain the `resample` or `export` columns only.
#' It is true they may not be filled for all sample IDs, but in these cases any
#' further resampling or exporting should be performed in the original table
#' in order to keep all the data log for resampling or exporting of sample IDs
#' from one datatable in one place.
#'
datatable_read_active_vector <- function(rmd_contents) {

  # get datatables
  dts <- datatable_read_vector(rmd_contents)

  # then FILTER each row of datatables
    # NO resample col OR resample col is BLANK for row

  for( i in 1:length(dts) ) {
    dplyr::filter()
  }
    # NO export col OR export col is BLANK for row


  # return filtered dts:
  filtered_dts

}



#' Read Datatables and check validity from character vector
#'
#' @export
datatable_read_vector <- function(rmd_contents) {

  # identify the lines that begin with "+==="
  indices <- which( startsWith(rmd_contents, "+===") )

  # from indices extract the actual tables - they sit between the first and second dividers
  if((length(indices) %% 2) == 1) {
    stop( paste0("  datatable divider numbers are odd - syntax error in the Rmd: ", length(indices)) )
  }

  # extract the data tables - that sit between the first and second dividers
  tables <- list()
  for(i in 1:(length(indices)/2) ) {
    end <- (i*2)
    start <- (i*2)-1
    tables[[i]] <- rmd_contents[ indices[start]:indices[end] ]
  }

  # store each datatable CREATED or IMPORTED in this list
  datatables <- list()

  # store each grouptable in this list - keep separate as will lookup which groups sample IDs belong to as needed
  grouptables <- list()

  # parse each data table to build in-memory representation of them:
   # samples must be in CREATE or IMPORT tables FIRST
   # samples can be GROUPED - save group tables and apply any further group refs to the samples directly

  for(i in 1:length(tables) ) {
  #for(i in 1:35 ) {

    # parse each table IN ORDER
     # tables[[i]][4] contains the table NAME then FUNCTION
     # eg. mice  :  CREATE , mice  :  ADD_DATA
    # extract each to new strings:
    table_name <- trimws(strsplit( as.character(tables[[i]][4]), ":")[[1]][1])
    table_function <- trimws(strsplit( as.character(tables[[i]][4]), ":")[[1]][2])


    if( table_function == "CREATE" ) {

        # CREATE : just parse the tables character vector to create a tibble
      datatables[[ table_name ]] <- parse_datatable_create(tables[[i]])


    } else if( table_function == "ADD_DATA" ) {

      # DEAL WITH NEW SYNTAX - declaring multiple datatable names in one:
      table_names <- unlist(strsplit( table_name, " ") )
       # remove blank elements - happens if the table_names were separated by MORE THAN ONE SPACE
      table_names <- table_names[table_names != ""]

      for(f in 1:length(table_names) ) {

        t_n <- table_names[f]

        # FIRST check that table_name ALREADY EXISTS in datatables
        if( any(names(datatables) == t_n) == FALSE ) {
          # no datatable of name table_name to add data to - STOP
          stop( paste0("  No datatable exists to add data to: ", t_n, " table index: ", i) )
        }

          # ADD_DATA : parse tables character vector and CHECK AGAINST EXISTING datatables to ADD DATA TO TABLE
        datatables[[ t_n ]] <- parse_datatable_add_data( tables[[i]], datatables[[ t_n ]] )

      }


    } else if( table_function == "GROUP" ) {

      # FIRST check that table_name ALREADY EXISTS in datatables
      if( any(names(datatables) == table_name) == FALSE ) {
        # no datatable of name table_name to add data to - STOP
        stop( paste0("  No datatable exists to add data to: ", table_name, " table index: ", i) )
      }

      # GROUP : parse tables character vector and CHECK AGAINST EXISTING datatables to ADD GROUP DATA TO TABLE
      datatables[[ table_name ]] <- parse_datatable_group(tables[[i]], datatables[[ table_name ]])

    } else if( table_function == "RESAMPLE" ) {

      # FIRST check that table_name ALREADY EXISTS in datatables
      if( any(names(datatables) == table_name) == FALSE ) {
        # no datatable of name table_name to add data to - STOP
        stop( paste0("  No datatable exists to add data to: ", table_name, " table index: ", i) )
      }

      # RESAMPLE : parse tables character vector and the datatables list,
       # return the new datatables list with NEW DATATABLES ADDED:
      datatables <- parse_datatable_resample(tables[[i]], datatables, table_name)

    } else if( table_function == "IMPORT" ) {

      # IMPORT : parse as CREATE: parse the tables character vector to create a tibble
      datatables[[ table_name ]] <- parse_datatable_create(tables[[i]])


    } else if( table_function == "EXPORT" ) {

      # DEAL WITH NEW SYNTAX - declaring multiple datatable names in one:
      table_names <- unlist(strsplit( table_name, " ") )
      # remove blank elements - happens if the table_names were separated by MORE THAN ONE SPACE
      table_names <- table_names[table_names != ""]

      for(f in 1:length(table_names) ) {

        t_n <- table_names[f]

        # FIRST check that table_name ALREADY EXISTS in datatables
        if( any(names(datatables) == t_n) == FALSE ) {
          # no datatable of name table_name to add data to - STOP
          stop( paste0("  No datatable exists to add data to: ", t_n, " table index: ", i) )
        }

        # EXPORT : just add data to the relevant datatable!
          # ADD_DATA : parse tables character vector and CHECK AGAINST EXISTING datatables to ADD DATA TO TABLE
        datatables[[ t_n ]] <- parse_datatable_add_data( tables[[i]], datatables[[ t_n ]] )

      }

    }

  }

  # return
  datatables

}


#' CREATE a tibble from a datatable Rmd vector
#'
#' This datatable has the FUNCTION : CREATE
#'
#' It MUST have IDs in the first column.  It can have subsequent columns with
#' further data.
#'
#'
parse_datatable_create <- function(dt_vector) {

  # CHECK VALIDITY FIRST:

  # column headers are at index 7: remove lead/lag whitespace, split on space, remove blank vals
  headers <- lapply(strsplit( as.character( trimws(dt_vector[7]) ), " "), function(x){x[!x ==""]})[[1]]
  headers_length <- length(headers)

  if(headers[1] != "ID" ) {
    # first col MUST be ID - if not, STOP!
    stop( paste0("  First column header is not ID: ", headers[1]))

  }

  # if the column delims (vector index 8) contains any characters other than ' ' and '=' then STOP
  if( !grepl("^[= ]+$", dt_vector[8], perl=T) ) {
    stop( paste0("  Column Delimiter row has a syntax error: ", dt_vector[8]))
  }

  # index 9 SHOULD ALWAYS BE a blank line - or at least only SPACES
    # TEST that all blank lines are blank where they should be? trimws() to trim all SPACES
  if( trimws(paste0(dt_vector[2],dt_vector[3],dt_vector[5],dt_vector[6],dt_vector[9])) !="") {
    stop( paste0("  Datatable has a syntax error: ", rmd_path) )
  }

  # convert dt_vector into a LIST of character vectors
   # FIRST entry in each vector is the datatable column header
    # Each subsequent entry is an individual data point
    # where multi-observations are added - they are in the SAME vector element, SPACE SEPARATED
    # where data vals are MISSING - a BLANK ELEMENT is inserted
  data <- extract_datatable(dt_vector)

  # CREATE tibble from the data LIST

  # first CREATE a tibble using the FIRST vector in data - ID!
  dt <- tibble::tibble(data[[1]][2:length(data[[1]])])
  names(dt) <- data[[1]][1] # IMMEDIATELY repair the name of first col - to ID!

  # now add all subsequent cols, if they exist!
  if(length(data) > 1)  {

    #newCol <- NA_character_ # for adding new cols - all will be character vectors

    for(i in 2:length(data) ) {

      dt <- add_data_col_type(dt, data[[i]][1], data[[i]][2:length( data[[i]] ) ]  )
      #dt <- tibble::add_column( dt, newCol )
      #names(dt)[names(dt) == "newCol"] <- data[[i]][1] # and SET THE NAME IMMEDIATELY!
      #dt[[ data[[i]][1] ]] <- data[[i]][2:length( data[[i]] ) ]

    }
  }

  # return dataframe
  dt

}



#' Add data of col type to datatable
#'
#' Accepts an existing dataframe, colname and data to insert.  Computes
#' the type from the first value in data (data[1]): numeric, datetime, date,
#' or character, then adds the correct column, renames the col to `name`, and
#' finally converts data to correct datatype and inserts into datatable.
#'
#' DT row length MUST equal the length of data.  `data` and `name` must be
#' character vectors.
#'
add_data_col_type <- function(dt, name, data, indices="") {

  add_col <- TRUE
  if( any(names(dt) == name) == TRUE ) {
    add_col <- FALSE
  }

  if( !is.na( suppressWarnings( as.double(data[1]) ) ) ) {

    newCol <- NA_real_

    if(add_col == TRUE) { # only add column if it doesnt already exist!
      dt <- tibble::add_column( dt, newCol )
      names(dt)[names(dt) == "newCol"] <- name # set name IMMEDIATELY
    }

    if( all(indices == "") ) {
      dt[[ name ]] <- as.double(data)
    } else {
      dt[[ name ]][indices] <- as.double(data)
    }
    #dt[[ name ]][dt_col_vec == match_vec] <- as.double(data)

  } else if( !is.na( suppressWarnings(lubridate::ymd_hm(data[1])) ) ) {

    newCol <- as.POSIXct(NA)

    if(add_col == TRUE) { # only add column if it doesnt already exist!
      dt <- tibble::add_column( dt, newCol )
      names(dt)[names(dt) == "newCol"] <- name # set name IMMEDIATELY
    }

    if( all(indices == "") ) {
      dt[[ name ]] <- lubridate::ymd_hm(data)
    } else {
      dt[[ name ]][indices] <- lubridate::ymd_hm(data)
    }

  } else if( !is.na( suppressWarnings(lubridate::ymd(data[1])) ) ) {

    newCol <- lubridate::as_date(NA)

    if(add_col == TRUE) { # only add column if it doesnt already exist!
      dt <- tibble::add_column( dt, newCol )
      names(dt)[names(dt) == "newCol"] <- name # set name IMMEDIATELY
    }

    if( all(indices == "") ) {
      dt[[ name ]] <- lubridate::ymd(data)
    } else {
      dt[[ name ]][indices] <- lubridate::ymd(data)
    }

  } else {

    newCol <- NA_character_

    if(add_col == TRUE) { # only add column if it doesnt already exist!
      dt <- tibble::add_column( dt, newCol )
      names(dt)[names(dt) == "newCol"] <- name # set name IMMEDIATELY
    }

    if( all(indices == "") ) {
      dt[[ name ]] <- data
    } else {
      dt[[ name ]][indices] <- data
    }

  }

  dt

}



#' ADD DATA to a tibble from a datatable vector
#'
#' This datatable has the FUNCTION : ADD_DATA
#'
#' It can have IDs, variables, or timetable in the first column.
#'
#' It will have subsequent columns with further data DEPENDING on the first col:
#'
#' * ID: subsequent cols are variables - can be added directly to dt.
#'
#'
#' * variables:
#'
#'
#' * timetable:
#'
#'   dt_vector <- tables[[i]]
#'   dt <- datatables[[ table_name ]]
#'
parse_datatable_add_data <- function(dt_vector, dt) {

  # CHECK VALIDITY FIRST:

  # column headers are at index 7: remove lead/lag whitespace, split on space, remove blank vals
  headers <- lapply(strsplit( as.character( trimws(dt_vector[7]) ), " "), function(x){x[!x ==""]})[[1]]
  headers_length <- length(headers)


  # if the column delims (vector index 8) contains any characters other than ' ' and '=' then STOP
  if( !grepl("^[= ]+$", dt_vector[8], perl=T) ) {
    stop( paste0("  Column Delimiter row has a syntax error: ", dt_vector[8]))
  }

  # index 9 SHOULD ALWAYS BE a blank line - or at least only SPACES
  # TEST that all blank lines are blank where they should be? trimws() to trim all SPACES
  if( trimws(paste0(dt_vector[2],dt_vector[3],dt_vector[5],dt_vector[6],dt_vector[9])) !="") {
    stop( paste0("  Datatable has a syntax error: ", dt_vector[1:9]) )
  }


  # convert dt_vector into a LIST of character vectors
   # FIRST entry in each vector is the datatable column header
    # Each subsequent entry is an individual data point
    # where multi-observations are added - they are in the SAME vector element, SPACE SEPARATED
    # where data vals are MISSING - a BLANK ELEMENT is inserted
  data <- extract_datatable(dt_vector)

  # ADD DATA to tibble from the data LIST
  # this depends on what FORMAT the datatable was in in the Rmd
  # samples first - first col is ID
  # variables first - first col is variables
  # timetable - first col is timetable


  if(headers[1] == "ID" ) {
    # this is a SAMPLE-FIRST data table!
     # as this is the format samples data will be stored in, just need to add the data cols

    # to deal with adding to ALL or GROUPS should collect some data from dt & data variables

    # get all group cols in one dt
    gdt <- dplyr::select(dt, dplyr::starts_with("group-"))


    if(headers[2] == "rep" ) { # if second col is rep, the first col MUST be IDs! No ALL or group-IDs
      # if there is a rep col, just have to SKIP this entry in data - so start at index 3:length(Data)

      #newCol <- NA_character_ # for adding new cols - all will be character vectors

      # extract data_cols
      data_cols <- c()
      for( i in 3:length(data ) ) { # SKIP index 1+2 - this IS ID COL & REP COL - which already exist in dt!
        data_cols[(i-2)] <- data[[i]][1]
      }

      # extract group_names - first entry in each list entry 3:length
       # NOT USED!
      group_names <- c()
      for(i in 3:length(data) ) {
        #group_names[(2-1)] <- data[[1]][i]
        group_names <- unique( data[[1]][3:length(data[[1]])] )
      }

      for(i in 3:length(data) ) { # SKIP index 1+2 - this IS ID COL & REP COL - which already exist in dt!

        # check dt doesnt already contain col of same name?
        # NOT NEEDED - as may add data to SAME COLS but DIFFERENT SAMPLE IDs!
        #if( any(names(dt) == data[[i]][1] ) ) {
        #  stop( paste0("  Data col already exists in datatable: ", data[[i]][1],
        #               " dt name: ", trimws(strsplit( as.character(dt_vector[4]), ":")[[1]][1])))
        #}

        #dt <- tibble::add_column( dt, newCol )
        #names(dt)[names(dt) == "newCol"] <- data[[i]][1] # and SET THE NAME IMMEDIATELY!

        # ADD ALL DATA TO DT: only if lengths are the same
         # also should CHECK the ID and rep values match between data and dt!
         # need to check each ID and rep in case only a SUBSET of IDs or REPS have data added to them!
        indices <- c()
        summarise_reps <- FALSE
        expanded_data <- c()

        for( j in 2:length(data[[i]]) ) {

          ID <- data[[1]][j]
          rep <- data[[2]][j]

          # expand ID and rep if they are "ALL"
          if( length(ID) == 1 && ID == "ALL" ) {

            summarise_reps <- TRUE # ALL is one form of summarising reps
              # will be adding multiple data as expanded_data to dt...

            ID_rep <- check_divisions_ids_reps(dt)
              # using check_divisions - to OMIT any IDs/REPs that have been divided
                # resampled, disposed, exported..
            ID <- ID_rep$IDs
            rep <- ID_rep$REPs

            for(k in 1:length(ID) ) {
              # need to loop through each ID now! index [k]
              index <- match( TRUE, (dt[["ID"]] == ID[k] & dt[["rep"]] == as.character(rep[k]) ) )
              indices <- c( indices, index )
            }
            # AND fill new vector - expanded_data - with REPLICATED DATA
            expanded_data <- c(expanded_data, rep(data[[i]][j], length(ID) ) )


          } else if( rep == "ALL" ) { # expand if rep contains keyword ALL




          } else if( grepl(",", rep) || grepl(":", rep) ) { # expand if rep uses the r vector indexing syntax: 1:3,4,8:10 etc

            summarise_reps <- TRUE
            # expand it in the c() function
            rep <- eval(parse(text = paste0("c(",rep,")") )  )

            # and expand ID by replicating it by length of rep now:
            ID <- rep(ID, length(rep) )
            for(k in 1:length(ID) ) {
              # need to loop through each ID now! index [k]
              index <- match( TRUE, (dt[["ID"]] == ID[k] & dt[["rep"]] == as.character(rep[k]) ) )
              indices <- c( indices, index )
            }
            # AND fill new vector - expanded_data - with REPLICATED DATA
            expanded_data <- c(expanded_data, rep(data[[i]][j], length(ID) ) )

          } else { # ID and rep are single vals - so just process as normal!
            index <- match( TRUE, (dt[["ID"]] == ID & dt[["rep"]] == rep) )
            indices <- c( indices, index )

          }
          #dt[[ data[[i]][1] ]][ index ] <- data[[i]][j]
        }
        #this checks each ID and rep index and inserts the data CORRECTLY!

        if( summarise_reps == TRUE ) { # need to rep each element in data[[i]][2:length(data[[i]])] by rep count!
          dt <- add_data_col_type(dt, data[[i]][1], expanded_data, indices)

        } else { # can add the data as it exists
          dt <- add_data_col_type(dt, data[[i]][1], data[[i]][2:length(data[[i]])], indices)
        }

      }

    } else {
      # if no rep col, add all cols from data from 2:length(data) - SKIP ID COL ONLY

      newCol <- NA_character_ # for adding new cols - all will be character vectors

      # extract data_cols
      data_cols <- c()
      for( i in 2:length(data ) ) {
        data_cols[(i-1)] <- data[[i]][1]
      }

      # extract group_names - first entry in each list entry 2:length
      group_names <- c()
      for(i in 2:length(data[[1]]) ) {
        group_names[(i-1)] <- data[[1]][i]
      }

      # add each data column to dt
      for(i in 2:length(data) ) { # SKIP index 1 - this IS ID COL - which already exists in dt!

        # check dt doesnt already contain col of same name?
        # NOT NEEDED - as may add data to SAME COLS but DIFFERENT SAMPLE IDs!
        #if( any(names(dt) == data[[i]][1] ) ) {
        #  stop( paste0("  Data col already exists in datatable: ", data[[i]][1],
        #               " dt name: ", trimws(strsplit( as.character(dt_vector[4]), ":")[[1]][1])))
        #}


        # converts group_names to vector same length as dt[["ID"]]
        group_names_comp_ids <- rep(sort(group_names), ((length(dt[["ID"]]) / length(sort(group_names)))+1) )[1:length(dt[["ID"]])]

        # process depending on if the group_names are
        # ALL : set each sample ID from dt to the corresponding value
        # sample-IDs : for each sample ID in data, set the correct value to row
        # group-col : each row that contains the group_name, set the corresponding value

        # ADD DATA ACROSS ALL DATA IF ID IS ALL
        if( length(group_names)==1 && group_names[1] == "ALL" ) {

          # first get all VALID IDs
          ID <- check_divisions_ids(dt)

          # then get indices of these from dt:
          indices <- c()
          for( j in 1:length(ID) ) {
            index <- match( TRUE, (dt[["ID"]] == ID[j]) )
            indices <- c(indices, index)
          }

          # then add data col, but only add data at indices
          dt <- add_data_col_type(dt, data[[i]][1], rep(data[[i]][2], length( indices )), indices )

        }
        else if( any( sort(group_names_comp_ids) == sort(dt[["ID"]]) ) ) { # group_names are IDs - so fill by ID index!

          # the 'group_names' are actual IDs - either all of them or a subset
           # so just add each datum where the ID in data[[i]] matches the ID in dt[["ID"]]
          indices <- c()
          for( j in 2:length(data[[i]]) ) {
            ID <- data[[1]][j]
            index <- match( TRUE, (dt[["ID"]] == ID) )
            indices <- c(indices, index)
            #dt[[ data[[i]][1] ]][ (dt[["ID"]] == ID) ] <- data[[i]][j]
          }

          dt <- add_data_col_type(dt, data[[i]][1], data[[i]][2:length(data[[i]])], indices)

        }
        else {  # group_names correspond to a group-col in dt?

          # FIRST - deal with edge case - no group cols
          if( ncol(gdt) == 0 ) {
            stop( paste0("  Non-existant GROUPS in datatable: ", group_names ) )
          }

          # ADD DATA TO ID ROWS ACCORDING TO GROUP ASSIGNMENT

          # get index of the group dt first
          gdtindex <- 0
          for(j in 1:length(gdt) ) {
            if( any( sort(gdt[[j]][ !is.na(gdt[[j]])]) == group_names[1] ) ) { # compare to first group name only
                 # eliminates subtle bug due to vector recycling..
              gdtindex <- j
            }
          }

          # now check each group_names exists in this gdt vector
          pass <- TRUE
          gdt2 <- gdt[[gdtindex]][!is.na(gdt[[gdtindex]])]
          gnindex <- 0
          for(k in 1:length(group_names) ) {
            if(any(gdt2 == group_names[k]) == FALSE) {
              pass <- FALSE
              gnindex <- k
            }
          }

          if(pass == FALSE) {
            stop( paste0("  Non-existant GROUP value in group_names: ", group_names[gnindex] ) )
          }

          # now identified and checked all group_names, can add to dt the data to each ID according to its group identity
          indices <- c()
          IDs <- c()
          for( j in 2:length(data[[i]]) ) {
            ID <- data[[1]][j]
             # index <- match( TRUE, (gdt[[gdtindex]] == ID) )
            indcs <- which( (gdt[[gdtindex]] == ID) )
            indices <- c(indices, indcs)
            IDs <- c( IDs, rep(data[[i]][j], length(indcs)) )
            #dt[[ data[[i]][1] ]][ (gdt[[gdtindex]] == ID) ] <- data[[i]][j]
          }

          dt <- add_data_col_type( dt, data[[i]][1], IDs, indices )
           # dt <- add_data_col_type( dt, data[[i]][1], data[[i]][2:length(data[[i]])], (gdt[[gdtindex]] == ID) )


        }

      }

  }

  } else if( headers[1] == "variables" ) {
    # this is a VARIABLE-FIRST data table!
     # requires careful handling:
      # data: first list entry is "variables" : then a vector containing each data_col
      # subsequent list entries - first element is an ID
       # This is one of: ALL, a group-ID, a sample-ID
         # if it ALL - need to add each data_col to ALL IDs in dt
         # if its a group-ID - add data to each ID that is part of selected group
         # if its sample-ID just add data to row in new col where sample ID matches dt[["ID"]] col!

    # get all group cols in one dt
    gdt <- dplyr::select(dt, dplyr::starts_with("group-"))

    # extract data_cols
    data_cols <- c()
    for( i in 2:length(data[[1]] ) ) {
      data_cols[(i-1)] <- data[[1]][i]
    }
    # extract group_names - first entry in each list entry 2:length
    group_names <- c()
    for(i in 2:length(data) ) {
      group_names[(i-1)] <- data[[i]][1]
    }

    # converts group_names to vector same length as dt[["ID"]]
    group_names_comp_ids <- rep(sort(group_names), ((length(dt[["ID"]]) / length(sort(group_names)))+1) )[1:length(dt[["ID"]])]

    # process depending on if the group_names are
     # ALL : set each sample ID from dt to the corresponding value
     # sample-IDs : for each sample ID in data, set the correct value to row
     # group-col : each row that contains the group_name, set the corresponding value
    if( length(group_names)==1 && group_names[1] == "ALL" ) {

      # group_names is special case: ALL

      # here want to add the value in ALL across ALL IDs for each data_col
      newCol <- NA_character_ # for adding new cols - all will be character vectors

      # loop through all data_cols from the new dt_vector
      for( i in 1:length(data_cols) ) {

        # create data_Vector
        data_vector <- rep(data[[2]][(i+1)], length(dt$ID) ) # only one col in data at [[2]]! ALL!

        # add data to dt:
         #dt <- tibble::add_column( dt, newCol )
         #names(dt)[names(dt) == "newCol"] <- data_cols[i] # and SET THE NAME IMMEDIATELY!
         #dt[[ data_cols[i] ]] <- data_vector # and then add data
        dt <- add_data_col_type( dt, data_cols[i], data_vector )

      }

    } else if( any( sort(group_names_comp_ids) == sort(dt[["ID"]]) ) ) { # group_names are IDs - so fill by ID index!

      # group_names are all the IDs
      ### SHOULD THIS BE MADE INVALID?
      ### HOW DOES THIS PARSE SAMPLE IDS WITH REPS ?!?!?!

      # here want to add the values in data across all sample IDs for each data_col
      newCol <- NA_character_ # for adding new cols - all will be character vectors

      for( i in 1:length(data_cols) ) {

        # create new data column
         #dt <- tibble::add_column( dt, newCol )
         #names(dt)[names(dt) == "newCol"] <- data_cols[i] # and SET THE NAME IMMEDIATELY!

        #indices <- c()
        for( j in 2:length(data) ) { # loop through each ID

          ID <- data[[j]][1] # get ID
          #index <- match( TRUE, (dt[["ID"]] == ID) )
          #indices <- c(indices, index)

          #dt[[ data[[1]][i+1] ]][ (dt[["ID"]] == ID) ] <- data[[j]][i+1]

        #}

          dt <- add_data_col_type(
            dt,
            data[[1]][i+1],
            data[[j]][i+1],
                #unlist(lapply(data[2:length(data)], function(x) x[i+1])), #data[[2:length(data)]][i+1],
            (dt[["ID"]] == ID)  )

        }

      }
      # This adds all data for each ID that EXISTS in data list into dt
       # any IDs omitted are kept as NA data values
       # IDs can be decalred in ANY ORDER TOO


    } else {

      # group_names are from a group col??

      # get index of the group dt first
      gdtindex <- 0
      for(j in 1:length(gdt) ) {
        group_vals <- sort(gdt[[j]][ !is.na(gdt[[j]])])
        # converts group_names to vector same length as group_vals
        group_names_comp_group_vals <- rep(sort(group_names), ((length(group_vals) / length(sort(group_names)))+1) )[1:length(group_vals)]
        if( any( group_vals == group_names_comp_group_vals ) ) {
          gdtindex <- j
        }
      }

      # now check each group_names exists in this gdt vector
      pass <- TRUE
      gnindex <- 0
      for(k in 1:length(group_names) ) {
        if(any(gdt[[gdtindex]] == group_names[k]) == FALSE) {
          pass <- FALSE
          gnindex <- k
          break
        }
      }

      if(pass == FALSE) {
        stop( paste0("  Non-existant GROUP value in group_names: ", group_names[gnindex] ) )
      }
      # now identified and checked all group_names, can add to dt the data cols to each ID according to its group identity

      newCol <- NA_character_ # for adding new cols - all will be character vectors

      for( i in 1:length(data_cols) ) {

        # create new data column
         #dt <- tibble::add_column( dt, newCol )
         #names(dt)[names(dt) == "newCol"] <- data_cols[i] # and SET THE NAME IMMEDIATELY!

          # data_cols[i] and data[[1]][i+1] are the SAME?!

        #indices <- c()
        for( j in 2:length(data) ) { # loop through each GROUP

          GID <- data[[j]][1] # get Group ID
          #index <- match( TRUE, (gdt[[gdtindex]] == GID) )
          #indices <- c(indices, index)
          #indices <- match( TRUE, (gdt[[gdtindex]] == GID) )
          #dt[[ data[[1]][i+1] ]][ (gdt[[gdtindex]] == GID) ] <- data[[j]][i+1]
          datac <- unlist(lapply(data[2:length(data)], function(x) x[i+1]))[(j-1)]
          indices <- (gdt[[gdtindex]] == GID)

          dt <- add_data_col_type(
            dt,
            data[[1]][i+1],
            datac, #data[[2:length(data)]][i+1],
            indices
          )

        }


      }
      # This adds all data for each group that EXISTS in data list into dt
       # any groups omitted are kept as NA data values
       # this also supports declaring more than one ID/row in a group!

    }


  } else if( headers[1] == "timetable" ) {
    # this is a TIMETABLE
     # requires careful handling:
      # data: first list entry is "variables" : then a vector containing each data_col
      # subsequent list entries - first element is an ID
      # This is one of: ALL, a group-ID, a sample-ID
      # if it ALL - need to add each data_col to ALL IDs in dt
      # if its a group-ID - add data from each

    # get all group cols in one dt
    gdt <- dplyr::select(dt, dplyr::starts_with("group-"))

    # extract group_names - first entry in each list entry 2:length
    group_names <- c()
    for(i in 2: (length(data)-1) ) {     # the last 'group' is always `change_dt` so remove it:
      group_names[(i-1)] <- data[[i]][1]
    }

    # extract data_cols - data_cols will be the non-blank values in first col in data
     # AFTER the first one - first col is 'timetable' and only contains the timepoints NOT data_col names!
    data_cols <- c()
    z <- 1
    for(x in 1:length(group_names) ) {
      for(y in 2:length(data[[(x+1)]]) ) {
        if( data[[(x+1)]][y] != "") {
          data_cols[z] <- data[[(x+1)]][y]
          z <- z+1
        }
      }
    }
    data_cols <- unique(data_cols)

    # for timetable, each row contains:

      # the steps : these are the COLUMN TITLES that go into the datatable

      # the datetime : this is the data that should be put into the new column

      # groups : each column in timetable will be a group
        # from this the STEP is read (COL NAME), and from THAT ROW the datetime is read (DATA)

      # key problem in parsing a timetable is EACH GROUP WILL DECLARE STEPS IN SEPARATE ROWS!

      # so, should FIRST make blank char vectors to fill dt
        # THEN, go through this and add DATETIME data as the STEPS are identified in GROUP COLS from timetable

    newCol <- as.POSIXct(NA) # for adding new cols - all will be POSIXct
    # add each col_title as new character col to dt - fill with BLANK "" values:
    for(x in 1:length(data_cols) ) {
      # add data to dt:
      dt <- tibble::add_column( dt, newCol )
      names(dt)[names(dt) == "newCol"] <- data_cols[x] # and SET THE NAME IMMEDIATELY!
    }

    # next work through each GROUP and ROW, and each time a DATA_COL comes up set the group indexes in that col to the datetime

    dt_data <- data[[ length(data) ]] # get the datetime data from data list - its the last item on the list - change_dt col!

     # process depending on if the group_names are
      # ALL : set each sample ID from dt to the corresponding value
      # sample-IDs : for each sample ID in data, set the correct value to row
      # group-col : each row that contains the group_name, set the corresponding value
    if( length(group_names) == 1  &&  group_names[1] == "ALL" ) {

      # group_names is special case: ALL
      group_identities <- dt[["ID"]] # get group identities - just for length of dt col!

      group_name <- group_names[1] # this is ALL
      group_data <- data[[ 2 ]] # group data is in one vector in data list - at index 2
      for( y in 2:length(group_data) ) {
        # start index 2 - first entry is the group_data TITLE!
        if(group_data[y] != "") {
          # group_data[y] will be a DATA_COL if not blank
          # at that data col in dt, should add the data on each row where group_name (i.e the ID) is valid
          for(z in 1:length(group_identities) ) {
              dt[[ group_data[y] ]][z] <- lubridate::ymd_hm( dt_data[y] )
              #print( paste0("group_data[y]: ", group_data[y], " z: ", z, "dt_data[y]: ", dt_data[y]) )
          }
        }
      }

    } else if( length(group_names) == length(dt$ID) && all( sort(group_names) ==  sort(dt$ID) ) ) {

      # group_names are all the IDs

      # group_identities indexes the rows in dt where each ID exists:
      group_identities <- dt[["ID"]]

      # work through each group_name - which are each ID(/row) in dt
      for( x in 1:length(group_names) ) {
        group_name <- group_names[x]
        group_index <- (x+1) # index in data list
        group_data <- data[[ group_index ]]
        # look at each entry in group_data
        for( y in 2:length(group_data) ) {
          # start index 2 - first entry is the group_data TITLE!
          if(group_data[y] != "") {
            # group_data[y] will be a DATA_COL if not blank
             # at that data col in dt, should add the data on each row where group_name (i.e the ID) is valid
            for(z in 1:length(group_identities) ) {
              if(group_identities[z] == group_name) {
                dt[[ group_data[y] ]][z] <- lubridate::ymd_hm( dt_data[y] )
                #print( paste0("group_data[y]: ", group_data[y], " z: ", z, "dt_data[y]: ", dt_data[y]) )
              }
            }
          }
        }
      }

    } else {

      # group_names are from a group col??

      # check all group cols
      group_names_match_group_cols <- c()
      for( i in 1:length(gdt) ) {
        gnu <- unique( gdt[i][ !is.na(gdt[i]) ] )
        if( length(gnu) != length(group_names) ) {
          group_names_match_group_cols[i] <- FALSE
        } else {
          group_names_match_group_cols[i] <- all( gnu == group_names )
        }
      }

      if( any(group_names_match_group_cols) == TRUE ) {

        # group_names represent a group that exists in dt!

        # can get the column name from the group dt:
        data_col_name <- names(gdt)[group_names_match_group_cols]

        # next work through each GROUP and ROW, and each time a COL TITLE comes up set the group indexes in that col to the datetime
        group_identities <- dt[[data_col_name]]


        data_index <- c()
        # get the first element from each vector in the data list in one new vector
         # this is used for indexing the groups
        for(a in 1:length(data) ) {
          data_index[a] <- data[[a]][1]
        }

        for( x in 1:length(group_names) ) {
          group_name <- group_names[x]
          group_index <- (x+1)
          group_data <- data[[ group_index ]]
          # look at each entry in group_data
          for( y in 2:length(group_data) ) {
            # start index 2 - first entry is the group_data TITLE!
            if(group_data[y] != "") {
              # group_data[y] will be a DATA_COL!
              # at that data col in dt, should add the data at each index where group_name is valid
              for(z in 1:length(group_identities) ) {
                if(!is.na(group_identities[z]) && group_identities[z] == group_name) {
                  dt[[ group_data[y] ]][z] <- lubridate::ymd_hm( dt_data[y] )
                }
              }
            }
          }

        }

      } else {
        # group_names is INVALID
        stop( paste0("  group_names is invalid: Must be ALL, <all-sample-IDs> or names from EXISTING GROUP: ", group_names))
      }

    }


  } else {
    stop( paste0("  not a valid datatable - first column must be 'ID', 'variables' or 'timetable'"))
  }

  # return
  dt

}


#'
#'
#'
parse_datatable_group <- function(dt_vector, dt) {

  # CHECK VALIDITY FIRST:

  # column headers are at index 7: remove lead/lag whitespace, split on space, remove blank vals
  headers <- lapply(strsplit( as.character( trimws(dt_vector[7]) ), " "), function(x){x[!x ==""]})[[1]]
  headers_length <- length(headers)


  # if the column delims (vector index 8) contains any characters other than ' ' and '=' then STOP
  if( !grepl("^[= ]+$", dt_vector[8], perl=T) ) {
    stop( paste0("  Column Delimiter row has a syntax error: ", dt_vector[8]))
  }

  # index 9 SHOULD ALWAYS BE a blank line - or at least only SPACES
  # TEST that all blank lines are blank where they should be? trimws() to trim all SPACES
  if( trimws(paste0(dt_vector[2],dt_vector[3],dt_vector[5],dt_vector[6],dt_vector[9])) !="") {
    stop( paste0("  Datatable has a syntax error: ", dt_vector[1:9]) )
  }


  # convert dt_vector into a LIST of character vectors
  # FIRST entry in each vector is the datatable column header
  # Each subsequent entry is an individual data point
  # where multi-observations are added - they are in the SAME vector element, SPACE SEPARATED
  # where data vals are MISSING - a BLANK ELEMENT is inserted
  data <- extract_datatable(dt_vector)

  # ADD DATA to tibble from the data LIST
   # this depends on what FORMAT the datatable was in in the Rmd
   # samples first - first col is ID
   # variables first - first col is variables
   # timetable - first col is timetable


  if(headers[1] == "ID" ) {
    # this is a SAMPLE-FIRST data table!
    # as this is the format samples data will be stored in, just need to add the data cols

    if(headers[2] == "rep" ) { # if second col is rep, the first col MUST be IDs! No ALL or group-IDs
      # if there is a rep col, just have to SKIP this entry in data - so start at index 3:length(Data)

      #newCol <- NA_character_ # for adding new cols - all will be character vectors

      # extract data_cols
      data_cols <- c()
      for( i in 3:length(data ) ) { # SKIP index 1+2 - this IS ID COL & REP COL - which already exist in dt!
        data_cols[(i-2)] <- data[[i]][1]
      }

      # extract group_names - first entry in each list entry 3:length
      # NOT USED!
      group_names <- c()
      for(i in 3:length(data) ) {
        #group_names[(2-1)] <- data[[1]][i]
        group_names <- unique( data[[1]][3:length(data[[1]])] )
      }

      for(i in 3:length(data) ) { # SKIP index 1+2 - this IS ID COL & REP COL - which already exist in dt!

        # check dt doesnt already contain col of same name?
        if( any(names(dt) == data[[i]][1] ) ) {
          stop( paste0("  Data col already exists in datatable: ", data[[i]][1],
                       " dt name: ", trimws(strsplit( as.character(dt_vector[4]), ":")[[1]][1])))
        }

        #dt <- tibble::add_column( dt, newCol )
        #names(dt)[names(dt) == "newCol"] <- data[[i]][1] # and SET THE NAME IMMEDIATELY!

        # ADD ALL DATA TO DT: only if lengths are the same
        # also should CHECK the ID and rep values match between data and dt!
        # need to check each ID and rep in case only a SUBSET of IDs or REPS have data added to them!
        indices <- c()
        summarise_reps <- FALSE
        expanded_data <- c()

        for( j in 2:length(data[[i]]) ) {

          ID <- data[[1]][j]
          rep <- data[[2]][j]

          # expand ID and rep if they are "ALL"
          if( length(rep) == 1 && rep == "ALL" ) {

            summarise_reps <- TRUE # ALL is one form of summarising reps
            # will be adding multiple data as expanded_data to dt...

            ID_rep <- check_divisions_ids_reps(dt)
            # using check_divisions - to OMIT any IDs/REPs that have been divided
            # resampled, disposed, exported..
            ID <- ID_rep$IDs
            rep <- ID_rep$REPs

            for(k in 1:length(ID) ) {
              # need to loop through each ID now! index [k]
              index <- match( TRUE, (dt[["ID"]] == ID[k] & dt[["rep"]] == as.character(rep[k]) ) )
              indices <- c( indices, index )
            }
            # AND fill new vector - expanded_data - with REPLICATED DATA
            expanded_data <- c(expanded_data, rep(data[[i]][j], length(ID) ) )


          } else if( grepl(",", rep) || grepl(":", rep) ) { # expand if rep uses the r vector indexing syntax: 1:3,4,8:10 etc

            summarise_reps <- TRUE
            # expand it in the c() function
            rep <- eval(parse(text = paste0("c(",rep,")") )  )

            # and expand ID by replicating it by length of rep now:
            ID <- rep(ID, length(rep) )
            for(k in 1:length(ID) ) {
              # need to loop through each ID now! index [k]
              index <- match( TRUE, (dt[["ID"]] == ID[k] & dt[["rep"]] == as.character(rep[k]) ) )
              indices <- c( indices, index )
            }
            # AND fill new vector - expanded_data - with REPLICATED DATA
            expanded_data <- c(expanded_data, rep(data[[i]][j], length(ID) ) )

          } else { # ID and rep are single vals - so just process as normal!
            index <- match( TRUE, (dt[["ID"]] == ID & dt[["rep"]] == rep) )
            indices <- c( indices, index )

          }
          #dt[[ data[[i]][1] ]][ index ] <- data[[i]][j]
        }
        #this checks each ID and rep index and inserts the data CORRECTLY!

        if( summarise_reps == TRUE ) { # need to rep each element in data[[i]][2:length(data[[i]])] by rep count!
          dt <- add_data_col_type(dt, data[[i]][1], expanded_data, indices)

        } else { # can add the data as it exists
          dt <- add_data_col_type(dt, data[[i]][1], data[[i]][2:length(data[[i]])], indices)
        }

      }

    } else {

      newCol <- NA_character_ # for adding new cols - all will be character vectors
      for(i in 2:length(data) ) { # SKIP index 1 - this IS ID COL - which already exists in dt!

        # check dt doesnt already contain col of same name?
        if( any(names(dt) == data[[i]][1] ) ) {
          stop( paste0("  Data col already exists in datatable: ", data[[i]][1],
                       " dt name: ", trimws(strsplit( as.character(dt_vector[4]), ":")[[1]][1])))
        }

        dt <- tibble::add_column( dt, newCol )
        names(dt)[names(dt) == "newCol"] <- data[[i]][1] # and SET THE NAME IMMEDIATELY!
        # ADD ALL DATA TO DT: only if lengths are the same
        # also should CHECK the ID values match between data and dt?
        # need to check each ID in case only a SUBSET of IDs have data added to them!
        for( j in 2:length(data[[i]]) ) {
          ID <- data[[1]][j]
          index <- match( TRUE, (dt[["ID"]] == ID) )
          dt[[ data[[i]][1] ]][ index ] <- data[[i]][j]
        }
        #above checks each ID index and inserts the data CORRECTLY!

          #dt[[ data[[i]][1] ]] <- data[[i]][2:length( data[[i]] ) ] # and then add data

      }
    }

  } else {
    stop( paste0("  not a valid group datatable - first column must be 'ID'"))
  }

  # return
  dt

}


#' RESAMPLE a tibble from a datatable Rmd vector
#'
#' This dt_vector has the FUNCTION : CREATE
#'
#' It MUST have IDs in the first column, then `resample` and `reps` cols.
#'
#' It will add these resample and reps cols to the PARENT datatable, THEN will
#' create NEW DATATABLES - one for each resample element - which are added to
#' the `datatables` list and returned.
#'
parse_datatable_resample <- function(dt_vector, datatables, table_name) {

  # CHECK VALIDITY FIRST:

  # column headers are at index 7: remove lead/lag whitespace, split on space, remove blank vals
  headers <- lapply(strsplit( as.character( trimws(dt_vector[7]) ), " "), function(x){x[!x ==""]})[[1]]
  headers_length <- length(headers)


  # headers_length MUST be 3, and headers MUST be "ID" "resample" and "reps" in this order
  if( (headers_length == 3) ) {
    datatables <- parse_datatable_resample_single(dt_vector, datatables, table_name)
  } else if( headers_length == 4 ) {
    # OR headers_length CAN be 4, and headers MUST be "ID" "rep" "resample" and "reps" in this order
    datatables <- parse_datatable_resample_reps(dt_vector, datatables, table_name)
  } else {
    stop( paste0("  Resample datatable MUST have only 3 or 4 cols: ", dt_vector[1:9]) )
  }

  # return
  datatables

}


#' Parse a Resample datatable with single SOURCE sample IDs
#'
#' This will generate the new datatables from a resample dt_vector that contains
#' single SOURCE sample IDS - it may still generate multiple REPS of subsamples!
#'
#'
parse_datatable_resample_single <- function(dt_vector, datatables, table_name) {

  # CHECK VALIDITY FIRST:

  # column headers are at index 7: remove lead/lag whitespace, split on space, remove blank vals
  headers <- lapply(strsplit( as.character( trimws(dt_vector[7]) ), " "), function(x){x[!x ==""]})[[1]]
  headers_length <- length(headers)


  # if the column delims (vector index 8) contains any characters other than ' ' and '=' then STOP
  if( !grepl("^[= ]+$", dt_vector[8], perl=T) ) {
    stop( paste0("  Column Delimiter row has a syntax error: ", dt_vector[8]))
  }

  # index 9 SHOULD ALWAYS BE a blank line - or at least only SPACES
  # TEST that all blank lines are blank where they should be? trimws() to trim all SPACES
  if( trimws(paste0(dt_vector[2],dt_vector[3],dt_vector[5],dt_vector[6],dt_vector[9])) !="") {
    stop( paste0("  Datatable has a syntax error: ", dt_vector[1:9]) )
  }

  # headers_length MUST be 3, and headers MUST be "ID" "resample" and "reps" in this order
  if((headers[[1]] != "ID")  ) {
    stop( paste0("  Resample datatable first col MUST be ID: ", dt_vector[1:9]) )
  }

  # headers_length MUST be 3, and headers MUST be "ID" "resample" and "reps" in this order
  if((headers[[2]] != "resample")  ) {
    stop( paste0("  Resample datatable second col MUST be resample: ", dt_vector[1:9]) )
  }

  # headers_length MUST be 3, and headers MUST be "ID" "resample" and "reps" in this order
  if((headers[[3]] != "reps")  ) {
    stop( paste0("  Resample datatable third col MUST be reps: ", dt_vector[1:9]) )
  }


  # convert dt_vector into a LIST of character vectors
  # FIRST entry in each vector is the datatable column header
  # Each subsequent entry is an individual data point
  # where multi-observations are added - they are in the SAME vector element, SPACE SEPARATED
  # where data vals are MISSING - a BLANK ELEMENT is inserted
  data <- extract_datatable(dt_vector)

  # CREATE tibbles from the data list - using the resample vectors

  # first get all the UNIQUE resample vector elements
  sub_dts_u <- unique( unlist( strsplit(data[[2]][ 2:length(data[[2]]) ], " ") ) )
  # and all UNIQUE reps elements
  sub_reps_u <- unique( unlist( strsplit(data[[3]][ 2:length(data[[3]]) ], " ") ) )

  # extract IDs DT codes and REPS to new lists
  sub_ids <- as.list(data[[1]][ 2:length(data[[1]]) ] )
  sub_dts <- strsplit(data[[2]][ 2:length(data[[2]]) ], " ")
  sub_reps <- strsplit(data[[3]][ 2:length(data[[3]]) ], " ")


  # CHECK the ids exist in the parent datatable
  par_ids <- datatables[[ table_name ]][[ "ID" ]]

  if(all( (0 < lapply(sub_ids, match, par_ids, nomatch=0) ) ) == FALSE ) {
    stop( paste0("  IDs in resample table do not exist in parent table: ",
                 sub_ids[(0 < lapply(sub_ids, match, par_ids, nomatch=0) ) == FALSE] ) )
  }


  # IF THE DATATABLE HAS NOT ALREADY BEEN RESAMPLED:
   # ADD resample and reps cols to the PARENT datatable
   # and CREATE ALL NEW sub datatables
  # ELSE
   # ADD TO the resample and reps cols in the PARENT DATATABLE
   # and ADD ID to any EXISTING sub datatables that have the same resampling!
   # datatables[[ table_name ]]

  # CHECK if datatable IDs have been RESAMPLED or EXPORTED
  if( any( names(datatables[[table_name]]) == "resample" ) ) {

    # samples from this datatable have been resampled before!
     # so should ADD TO the resample and reps cols that already exist in parent datatable:

    for(i in 2:length(data) ) { # SKIP index 1 - this IS ID COL - which already exists in dt!

      # add data in each row where ID is equal to data[[1]] ID
       # this handles if only a SUBSET of ID are being resampled in this table
      for( j in 2:length(data[[i]]) ) { # 2:length is all values
        index_id <- match(data[[1]][j], datatables[[ table_name ]][["ID"]]) # data[[1]] is ID vector!
        # FAIL if datatables[[ table_name ]][[ data[[i]][1] ]][ index_id ] is NOT NA
        if( is.na(datatables[[ table_name ]][[ data[[i]][1] ]][ index_id ]) == FALSE ) {
          # this data val is ALREADY FILLED - must be trying to resample an ID that has already been resampled!
          stop( paste0("  Sample has previously been resampled: ", datatables[[ table_name ]][["ID"]][index_id]))
        }

        datatables[[ table_name ]][[ data[[i]][1] ]][ index_id ] <- data[[i]][j] # and then add data val at index j
      }

    }


    # EXTRACT the data values to form new subsample tibbles:

    # lists to store IDs and reps in:
    sub_ids_list <- list()
    sub_reps_list <- list()

    # loop through each DT code:
    for( a in 1:length(sub_dts_u) ) {
      # loop through each sub_dts - list element
      sub_dts_ind <- 1
      for( b in 1:length(sub_dts) ) {
        # & vector element
        for( c in 1:length(sub_dts[[b]]) ) {

          # if matches the uniqe val
          if(sub_dts_u[a] == sub_dts[[b]][c] ) {
            #print(sub_dts_u[a])
            #print(sub_ids[[b]])
            #print(sub_reps[[b]][c])

            # generate SEQUENCE of reps:
            reps <- seq( as.numeric(sub_reps[[b]][c]) )
            reps_length <- length(reps)
            # add to sub_ids_list & sub_reps_list:
            if(sub_dts_ind == 1) {
              sub_ids_list[[a]] <- rep(sub_ids[[b]], reps_length)
              sub_reps_list[[a]] <- reps
            } else {
              sub_ids_list[[a]] <- c(sub_ids_list[[a]], rep(sub_ids[[b]], reps_length))
              sub_reps_list[[a]] <- c(sub_reps_list[[a]], reps)
            }
            sub_dts_ind <- sub_dts_ind + 1

          }

        }
      }
    }


    # NOW - ADD TO subsamples or CREATE new subsample tibbles and ADD to datatables
     # FOR EACH datatable create either:
      # WITHOUT rep col - if all reps are 1
      # or WITH rep col - if ANY REP is above 1

    # get unique reps vals from each new subsample from
    sub_reps_list_u <- lapply(sub_reps_list, unique)

    rep <- NA_character_ # for adding new cols - all will be character vectors

    for( a in 1:length(sub_dts_u) ) {

      # get the new subtable name
      subtable_name <- paste0(table_name, "_", sub_dts_u[a])

      # CHECK if this subtable ALREADY EXISTS:
       # note it MAY NOT ALREADY EXIST - as although samples have already been resampled from the parent datatable
        # the NEW SUBSAMPLES do not have to be the same as the previous subsamples!

      if( is.na( match( subtable_name, names(datatables) ) ) == FALSE  ) {
        # subtable already exists - so add the ID to this table

        # if the reps are 1 do NOT add a rep col:
        if(length(sub_reps_list_u[[a]]) == 1 && as.numeric(sub_reps_list_u[[a]][1]) == 1) {


          # the datatable MAY HAVE a `rep` column, or not - CHECK this
          if( any(names(datatables[[ subtable_name]]) == "rep" ) ) {
            # current dt contains a rep col - i.e previous subsamples contained more than 1 rep!
             # so should enter the rep value into the existing column - it will be 1, but should still add it!

            #datatables[[ subtable_name ]]
            datatables[[ subtable_name ]] <- tibble::add_row( datatables[[ subtable_name ]],
                                                              ID = sub_ids_list[[a]],
                                                              rep = as.character(sub_reps_list[[a]]) )
            # this adds the ID to ID col and rep to rep col - any other columns are set to NA by default

          } else {
            # reps does NOT exist - so jut add ID

            #datatables[[ subtable_name ]]
            datatables[[ subtable_name ]] <- tibble::add_row( datatables[[ subtable_name ]],
                           ID = sub_ids_list[[a]]  )
            # this adds the ID to ID column - any other columns are set to NA by default
          }

        } else { # if reps are more than one, add a rep col to the existing subsample DT IF NEEDED

          # the datatable MAY HAVE a `rep` column, or not - CHECK this
          if( any(names(datatables[[ subtable_name]]) == "rep" ) ) {
            # reps already exists, so add the ID and reps values in new row:
            datatables[[ subtable_name ]] <- tibble::add_row( datatables[[ subtable_name ]],
                                                              ID = sub_ids_list[[a]],
                                                              rep = as.character(sub_reps_list[[a]]) )
          } else {
            # reps does NOT exist - so FIRST CREATE THE NEW COLUMN!
            datatables[[ subtable_name ]] <- tibble::add_column( datatables[[ subtable_name ]], rep = "1" )
            # will automatically be called rep!
             # also automatically filled with 1s - as all samples in a subsample dt with NO REP COL will have rep of 1!
            datatables[[ subtable_name ]] <- dplyr::relocate( datatables[[ subtable_name ]], ID, rep )
              # this changes the order of the datatable so ID then rep cols are first
            # and now just add the ID and reps row(s):
            datatables[[ subtable_name ]] <- tibble::add_row( datatables[[ subtable_name ]],
                                                              ID = sub_ids_list[[a]],
                                                              rep = as.character(sub_reps_list[[a]]) )
          }

        }
      } else {
        # subtable DOES NOT exist - so CREATE THE NEW SUBSAMPLE TABLE

        # if the reps are 1 do NOT add a rep col:
        if(length(sub_reps_list_u[[a]]) == 1 && as.numeric(sub_reps_list_u[[a]][1]) == 1) {

          subtable_name <- paste0(table_name, "_", sub_dts_u[a])
          datatables[[ subtable_name ]] <- tibble::tibble(sub_ids_list[[a]])
          names( datatables[[ subtable_name ]] ) <- "ID" # first col is ID

        } else { # if reps are more than one, add a rep col!

          subtable_name <- paste0(table_name, "_", sub_dts_u[a])
          datatables[[ subtable_name ]] <- tibble::tibble(sub_ids_list[[a]])
          names( datatables[[ subtable_name ]] ) <- "ID" # first col is ID

          datatables[[ subtable_name ]] <- tibble::add_column( datatables[[ subtable_name ]], rep ) # will automatically be called rep!
          datatables[[ subtable_name ]][[ 'rep' ]] <- as.character(sub_reps_list[[a]])
        }

      }

    } # end for a - sub_dts


  } else {

    # no resample column exists - this is the FIRST RESAMPLING of this datatable:
     # so ADD resample and reps, and CREATE ALL THE NEW subsample DTs

    # need to CREATE the new data cols in parent sample DT - resample and reps
    newCol <- NA_character_ # for adding new cols - all will be character vectors
    for(i in 2:length(data) ) { # SKIP index 1 - this IS ID COL - which already exists in dt!

      datatables[[ table_name ]] <- tibble::add_column( datatables[[ table_name ]], newCol )
      names(datatables[[ table_name ]])[names(datatables[[ table_name ]]) == "newCol"] <- data[[i]][1] # and SET THE NAME IMMEDIATELY!

      # and then add data in each row where ID is equal to data[[1]] ID
        # this handles if only a SUBSET of ID are being resampled in this table
      for( j in 2:length(data[[i]]) ) { # 2:length is all values
        index_id <- match(data[[1]][j], datatables[[ table_name ]][["ID"]]) # data[[1]] is ID vector!
        # FAIL if datatables[[ table_name ]][[ data[[i]][1] ]][ index_id ] is NOT NA
        if( is.na(datatables[[ table_name ]][[ data[[i]][1] ]][ index_id ]) == FALSE ) {
          # this data val is ALREADY FILLED - must be trying to resample an ID that has already been resampled!
          stop( paste0("  Sample has previously been resampled: ", datatables[[ table_name ]][["ID"]][index_id]))
        }

        datatables[[ table_name ]][[ data[[i]][1] ]][ index_id ] <- data[[i]][j] # and then add data val at index j
      }

    }


    # EXTRACT the data values to form new subsample tibbles:

    # lists to store IDs and reps in:
    sub_ids_list <- list()
    sub_reps_list <- list()

    # loop through each DT code:
    for( a in 1:length(sub_dts_u) ) {
      # loop through each sub_dts - list element
      sub_dts_ind <- 1
      for( b in 1:length(sub_dts) ) {
        # & vector element
        for( c in 1:length(sub_dts[[b]]) ) {

          # if matches the uniqe val
          if(sub_dts_u[a] == sub_dts[[b]][c] ) {
            #print(sub_dts_u[a])
            #print(sub_ids[[b]])
            #print(sub_reps[[b]][c])

            # generate SEQUENCE of reps:
            reps <- seq( as.numeric(sub_reps[[b]][c]) )
            reps_length <- length(reps)
            # add to sub_ids_list & sub_reps_list:
            if(sub_dts_ind == 1) {
              sub_ids_list[[a]] <- rep(sub_ids[[b]], reps_length)
              sub_reps_list[[a]] <- reps
            } else {
              sub_ids_list[[a]] <- c(sub_ids_list[[a]], rep(sub_ids[[b]], reps_length))
              sub_reps_list[[a]] <- c(sub_reps_list[[a]], reps)
            }
            sub_dts_ind <- sub_dts_ind + 1

          }

        }
      }
    }

    # CREATE new subsample tibbles and ADD to datatables
     # FOR EACH datatable create either:
      # WITHOUT rep col - if all reps are 1
      # or WITH rep col - if ANY REP is above 1

    # get unique reps vals from each new subsample from
    sub_reps_list_u <- lapply(sub_reps_list, unique)

    rep <- NA_character_ # for adding new cols - all will be character vectors

    for( a in 1:length(sub_dts_u) ) {

      if(length(sub_reps_list_u[[a]]) == 1 && as.numeric(sub_reps_list_u[[a]][1]) == 1) {

        subtable_name <- paste0(table_name, "_", sub_dts_u[a])
        datatables[[ subtable_name ]] <- tibble::tibble(sub_ids_list[[a]])
        names( datatables[[ subtable_name ]] ) <- "ID" # first col is ID

      } else {

        subtable_name <- paste0(table_name, "_", sub_dts_u[a])
        datatables[[ subtable_name ]] <- tibble::tibble(sub_ids_list[[a]])
        names( datatables[[ subtable_name ]] ) <- "ID" # first col is ID

        datatables[[ subtable_name ]] <- tibble::add_column( datatables[[ subtable_name ]], rep ) # will automatically be called rep!
        datatables[[ subtable_name ]][[ 'rep' ]] <- as.character(sub_reps_list[[a]])
      }

    }

  }

  # return
  datatables

}


#' Parse a Resample datatable with reps SOURCE sample IDs
#'
#' This will generate the new datatables from a resample dt_vector that contains
#' set of REPS SOURCE sample IDS.  This means the SOURCE SAMPLES consist of two or more
#' REPLICATES.  To uniquely identify the replicates, a second `rep` column is presented
#' next to the `ID` column to denote each REP within each ID.
#'
#' When resampling samples which contain REPS, each ID/REP will be given as a new entry,
#' and `resample` and `reps` columns will be added.  The resample vector is given in
#' each ID/REP row - with each resample code given in adjoining rows for a given ID/REP.
#'
#' Further reps can be made of each ID/REP from the parent sample!  And these reps are
#' entered into the `reps` column.
#'
#' Importantly, when the subsample datatable/tibbles are made they are named according to
#' the parent resample CODE PLUS the parent resample rep, separated by a dash.
#'
#' eg. From `samples_SC-CER` datatable, if these samples were resampled with code
#' `100µm`, and ID `1001` had 4 reps created, if this was further resampled with
#' a new vector (eg. it was split into `LT` and `RT` sections), then in
#' `samples_SC-CER_100µm` each ID/REP would have its own line (1001/1 1001/2 1001/3
#' 1001/4), and each entry would have a resample column with `LT` and `RT` in it.
#'
#' THEN when this resample table is read, new datatable tibbles will be created, with
#' names: `samples_SC-CER_100µm-1_LT` `samples_SC-CER_100µm-1_RT`
#' `samples_SC-CER_100µm-2_LT` `samples_SC-CER_100µm-2_RT` etc.  These will
#' contain ID `1001` and represent each new subsample of each 100µm rep!
#'
parse_datatable_resample_reps <- function(dt_vector, datatables, table_name) {

  # CHECK VALIDITY FIRST:

  # column headers are at index 7: remove lead/lag whitespace, split on space, remove blank vals
  headers <- lapply(strsplit( as.character( trimws(dt_vector[7]) ), " "), function(x){x[!x ==""]})[[1]]
  headers_length <- length(headers)


  # if the column delims (vector index 8) contains any characters other than ' ' and '=' then STOP
  if( !grepl("^[= ]+$", dt_vector[8], perl=T) ) {
    stop( paste0("  Column Delimiter row has a syntax error: ", dt_vector[8]))
  }

  # index 9 SHOULD ALWAYS BE a blank line - or at least only SPACES
  # TEST that all blank lines are blank where they should be? trimws() to trim all SPACES
  if( trimws(paste0(dt_vector[2],dt_vector[3],dt_vector[5],dt_vector[6],dt_vector[9])) !="") {
    stop( paste0("  Datatable has a syntax error: ", dt_vector[1:9]) )
  }

  # headers_length MUST be 3, and headers MUST be "ID" "resample" and "reps" in this order
  if((headers[[1]] != "ID")  ) {
    stop( paste0("  Resample datatable first col MUST be ID: ", dt_vector[1:9]) )
  }

  # headers_length MUST be 3, and headers MUST be "ID" "resample" and "reps" in this order
  if((headers[[2]] != "rep")  ) {
    stop( paste0("  Resample datatable second col MUST be rep: ", dt_vector[1:9]) )
  }

  # headers_length MUST be 3, and headers MUST be "ID" "resample" and "reps" in this order
  if((headers[[3]] != "resample")  ) {
    stop( paste0("  Resample datatable third col MUST be resample: ", dt_vector[1:9]) )
  }

  # headers_length MUST be 3, and headers MUST be "ID" "resample" and "reps" in this order
  if((headers[[4]] != "reps")  ) {
    stop( paste0("  Resample datatable fourth col MUST be reps: ", dt_vector[1:9]) )
  }


  # convert dt_vector into a LIST of character vectors
  # FIRST entry in each vector is the datatable column header
  # Each subsequent entry is an individual data point
  # where multi-observations are added - they are in the SAME vector element, SPACE SEPARATED
  # where data vals are MISSING - a BLANK ELEMENT is inserted
  data <- extract_datatable(dt_vector)

  # CREATE tibbles from the data list - using the resample vectors

  # first get all the UNIQUE resample vector elements
  sub_dts_u <- unique( unlist( strsplit(data[[3]][ 2:length(data[[3]]) ], " ") ) )
  # and all UNIQUE reps elements
  sub_reps_u <- unique( unlist( strsplit(data[[4]][ 2:length(data[[4]]) ], " ") ) )
  # AND the UNIQUE parent rep elements
  sub_rep_u <- unique( unlist( strsplit(data[[2]][ 2:length(data[[2]]) ], " ") ) )

  # extract IDs DT codes and REPS to new lists
  sub_ids <- as.list(data[[1]][ 2:length(data[[1]]) ] )
  sub_rep <- as.list(data[[2]][ 2:length(data[[2]]) ] )
  sub_dts <- strsplit(data[[3]][ 2:length(data[[3]]) ], " ")
  sub_reps <- strsplit(data[[4]][ 2:length(data[[4]]) ], " ")


  # CHECK the ids exist in the parent datatable
  par_ids <- datatables[[ table_name ]][[ "ID" ]]

  if(all( (0 < lapply(sub_ids, match, par_ids, nomatch=0) ) ) == FALSE ) {
    stop( paste0("  IDs in resample table do not exist in parent table: ",
                 sub_ids[(0 < lapply(sub_ids, match, par_ids, nomatch=0) ) == FALSE] ) )
  }


  # IF THE DATATABLE HAS NOT ALREADY BEEN RESAMPLED:
    # ADD resample and reps cols to the PARENT datatable
      # and CREATE ALL NEW sub datatables
  # ELSE
    # ADD TO the resample and reps cols in the PARENT DATATABLE
      # and ADD ID to any EXISTING sub datatables that have the same resampling!

    # datatables[[ table_name ]]


  # CHECK if datatable IDs have been RESAMPLED or EXPORTED
  if( any( names(datatables[[table_name]]) == "resample" ) ) {

    # samples from this datatable have been resampled before!
    # so should ADD TO the resample and reps cols that already exist in parent datatable:

    for(i in 3:length(data) ) {
      # SKIP index 1 - this IS ID COL - which already exists in dt!
      # AND SKIP INDEX 2 - this is the rep COL - which already exists in dt!

      # add data in each row where ID is equal to data[[1]] ID
       # this handles if only a SUBSET of ID are being resampled in this table
      for( j in 2:length(data[[i]]) ) { # 2:length is all values
        #index_id <- match(data[[1]][j], datatables[[ table_name ]][["ID"]]) # data[[1]] is ID vector!
        ID <- data[[1]][j]
        rep <- data[[2]][j]
        index <- match( TRUE, (datatables[[ table_name ]][["ID"]] == ID & datatables[[ table_name ]][["rep"]] == rep) )

        # FAIL if datatables[[ table_name ]][[ data[[i]][1] ]][ index ] is NOT NA
        if( is.na(datatables[[ table_name ]][[ data[[i]][1] ]][ index ]) == FALSE ) {
          # this data val is ALREADY FILLED - must be trying to resample an ID that has already been resampled!
          stop( paste0("  Sample has previously been resampled: ", datatables[[ table_name ]][["ID"]][index]))
        }

        datatables[[ table_name ]][[ data[[i]][1] ]][ index ] <- data[[i]][j] # and then add data val at index j
      }

    }


    # NOW CREATE NEW / ADD TO SUBSAMPLE TIBBLES - and add to datatables
    rep <- NA_character_ # rep col - will be character vectors
    ID <- NA_character_ # ID col - will be character vectors

    for( a in 1:length(sub_ids) ) { # loop through sub_ids/rep /dts/reps
      sid <- sub_ids[[a]]
      srep <- sub_rep[[a]]

      # for each sub_ids / sub_rep there can be multiple sub_dts / sub_reps
      for( b in 1:length(sub_dts[[a]]) ) {

        # each [[a]][b] val in sub_dts / sub_reps is a NEW TIBBLE

        # SPLIT whether sub_reps is just 1 for ALL VALS, or not
        if( length(sub_reps_u) == 1 && as.numeric(sub_reps_u[1]) == 1 ) {

          # only create tibbles with ID cols: NO REPS in this subsampling at all!
          subtable_name <- paste0(table_name, "-", srep, "_", sub_dts[[a]][b])

          # this tibble MAY ALREADY EXIST - if an ID of same REP NUM has been created
          if( is.null(datatables[[ subtable_name ]]) == TRUE ) {

            # if the tibble doesnt exist, then create it, add ID col, and add current ID
            datatables[[ subtable_name ]] <- tibble::tibble()
            datatables[[ subtable_name ]] <- tibble::add_column(datatables[[ subtable_name ]], ID)
            datatables[[ subtable_name ]] <- tibble::add_row(datatables[[ subtable_name ]], ID = sid)

          } else { # the tibble DOES EXIST - so just add current ID to ID col of existing tibble
            datatables[[ subtable_name ]] <- tibble::add_row(datatables[[ subtable_name ]], ID = sid)
          }

        } else {

          # create tibbles with ID and REPS col:

          # only create tibbles with ID cols: NO REPS in this subsampling at all!
          subtable_name <- paste0(table_name, "-", srep, "_", sub_dts[[a]][b])
          sreps <- as.numeric(sub_reps[[a]][b])
          srepseq <- seq(sreps)

          # this tibble MAY ALREADY EXIST - if an ID of same REP NUM has been created
          if( is.null(datatables[[ subtable_name ]]) == TRUE ) {

            # if the tibble doesnt exist, then create it, add ID col, and add current ID
            datatables[[ subtable_name ]] <- tibble::tibble()
            datatables[[ subtable_name ]] <- tibble::add_column(datatables[[ subtable_name ]], ID)
            datatables[[ subtable_name ]] <- tibble::add_column(datatables[[ subtable_name ]], rep)
            for(c in 1:length(srepseq) ) {
              datatables[[ subtable_name ]] <- tibble::add_row(
                datatables[[ subtable_name ]],
                ID = sid,
                rep = as.character(srepseq[c]) )
            }

          } else { # the tibble DOES EXIST - so just add current ID and REP col of existing tibble

            for(c in 1:length(srepseq) ) {
              datatables[[ subtable_name ]] <- tibble::add_row(
                datatables[[ subtable_name ]],
                ID = sid,
                rep = as.character(srepseq[c]) )
            }

          } # else tibble EXISTS
        } # else tibbles IDs/REPs
      } # for b
    } # for a


    # OLD CODE - very complex and DOESNT WORK!!

    if(FALSE) {
    # EXTRACT the data values to form new subsample tibbles:

    # lists to store IDs and reps in:
    sub_ids_list <- list()
    sub_sub_ids_list <- list()
    sub_rep_list <- list()
    sub_reps_list <- list()
    sub_sub_reps_list <- list()

    # loop through each DT code
    for( a in 1:length(sub_dts_u) ) {
      # loop through each sub_dts - list element
      sub_dts_ind <- 1
      sub_sub_ids_list[[a]] <- list() # FIRST INDEX in sub_sub is the dts_u
      sub_sub_reps_list[[a]] <- list() # FIRST INDEX in sub_sub is the dts_u

      for( b in 1:length(sub_dts) ) { # this index is also the same length as sub_rep
        # & vector element     # REMEMBER the sub_rep number defines subsample tables too!
        for( c in 1:length(sub_dts[[b]]) ) {

          # if matches the uniqe val
          if(sub_dts_u[a] == sub_dts[[b]][c] ) {
            #print(sub_dts_u[a])
            #print(sub_ids[[b]])
            #print(sub_reps[[b]][c])
            #print(sub_dts_ind)

            # generate SEQUENCE of reps:
            reps <- seq( as.numeric(sub_reps[[b]][c]) )
            reps_length <- length(reps)


            # add to sub_ids_list & sub_reps_list:
            if(sub_dts_ind == 1) { # create if first index
              sub_ids_list[[a]] <- rep(sub_ids[[b]], reps_length)
              sub_rep_list[[a]] <- sub_rep[[b]]
              sub_reps_list[[a]] <- reps

              # treat sub_sub separately - as indices are added sub_dts_u FIRST
              # sceond indices of first list are filled FIRST
              if( as.numeric(sub_rep[[b]]) > length(sub_sub_ids_list[[a]]) ) {
                sub_sub_ids_list[[a]][[ as.numeric(sub_rep[[b]]) ]] <- rep(sub_ids[[b]], reps_length)
                sub_sub_reps_list[[a]][[ as.numeric(sub_rep[[b]]) ]] <- reps
              } else {
                sub_sub_ids_list[[a]][[ as.numeric(sub_rep[[b]]) ]] <- c(sub_sub_ids_list[[a]][[ as.numeric(sub_rep[[b]]) ]],
                                                                         rep(sub_ids[[b]], reps_length))
                sub_sub_reps_list[[a]][[ as.numeric(sub_rep[[b]]) ]] <- c(sub_sub_reps_list[[a]][[ as.numeric(sub_rep[[b]]) ]],
                                                                          reps)
              }
            } else { # add to with subsequent indices
              sub_ids_list[[a]] <- c(sub_ids_list[[a]], rep(sub_ids[[b]], reps_length))
              sub_rep_list[[a]] <- c(sub_rep_list[[a]], sub_rep[[b]])
              sub_reps_list[[a]] <- c(sub_reps_list[[a]], reps)

              # treat sub_sub separately - as indices are added sub_dts_u FIRST
              # sceond indices of first list are filled FIRST
              if( as.numeric(sub_rep[[b]]) > length(sub_sub_ids_list[[a]]) ) {
                sub_sub_ids_list[[a]][[ as.numeric(sub_rep[[b]]) ]] <- rep(sub_ids[[b]], reps_length)
                sub_sub_reps_list[[a]][[ as.numeric(sub_rep[[b]]) ]] <- reps
              } else {
                sub_sub_ids_list[[a]][[ as.numeric(sub_rep[[b]]) ]] <- c(sub_sub_ids_list[[a]][[ as.numeric(sub_rep[[b]]) ]],
                                                                         rep(sub_ids[[b]], reps_length))
                sub_sub_reps_list[[a]][[ as.numeric(sub_rep[[b]]) ]] <- c(sub_sub_reps_list[[a]][[ as.numeric(sub_rep[[b]]) ]],
                                                                          reps)
              }
            }
            sub_dts_ind <- sub_dts_ind + 1

          }

        }
      }
    }


    # NOW - ADD TO subsamples or CREATE new subsample tibbles and ADD to datatables
    # FOR EACH datatable create either:
    # WITHOUT rep col - if all reps are 1
    # or WITH rep col - if ANY REP is above 1


    # get unique reps vals from each new subsample from
    sub_reps_list_u <- lapply(sub_reps_list, unique)

    rep <- NA_character_ # for adding new cols - all will be character vectors

    for( a in 1:length(sub_dts_u) ) {
      for( b in 1:length(sub_rep_u) ) {

      # only add if sub_sub_ids_list[[a]][[b]] is NOT NULL:
      # this can be NULL if the rep does not contain this particular subsample code!
      if( is.null( sub_sub_ids_list[[a]][[b]] ) == TRUE ) {
        # DO NOT ADD DATA

      } else {

        # get the new subtable name
        subtable_name <- paste0(table_name, "-", sub_rep_u[b], "_", sub_dts_u[a])

        # CHECK if this subtable ALREADY EXISTS:
        # note it MAY NOT ALREADY EXIST - as although samples have already been resampled from the parent datatable
        # the NEW SUBSAMPLES do not have to be the same as the previous subsamples!

        if( is.na( match( subtable_name, names(datatables) ) ) == FALSE  ) {
          # subtable already exists - so add the ID to this table

          # if the reps are 1 do NOT add to a rep col:
          if(length(sub_reps_list_u[[a]]) == 1 && as.numeric(sub_reps_list_u[[a]][1]) == 1) {


            # the datatable MAY HAVE a `rep` column, or not - CHECK this
            if( any(names(datatables[[ subtable_name]]) == "rep" ) ) {
              # current dt contains a rep col - i.e previous subsamples contained more than 1 rep!
              # so should enter the rep value into the existing column - it will be 1, but should still add it!

              #datatables[[ subtable_name ]]
              # only add if sub_sub_ids_list[[a]][[b]] is NOT NULL:
                # this can be NULL if the rep does not contain this particular subsample code!

              datatables[[ subtable_name ]] <- tibble::add_row( datatables[[ subtable_name ]],
                                                                ID = sub_sub_ids_list[[a]][[b]],
                                                                rep = as.character(sub_sub_reps_list[[a]][[b]]) )
              # this adds the ID to ID col and rep to rep col - any other columns are set to NA by default

            } else {
              # reps does NOT exist - so jut add ID

              #datatables[[ subtable_name ]]
              datatables[[ subtable_name ]] <- tibble::add_row( datatables[[ subtable_name ]],
                                                                ID = sub_sub_ids_list[[a]][[b]]  )
              # this adds the ID to ID column - any other columns are set to NA by default
            }

          } else { # if reps are more than one, add a rep col to the existing subsample DT IF NEEDED

            # the datatable MAY HAVE a `rep` column, or not - CHECK this
            if( any(names(datatables[[ subtable_name]]) == "rep" ) ) {
              # reps already exists, so add the ID and reps values in new row:
              datatables[[ subtable_name ]] <- tibble::add_row( datatables[[ subtable_name ]],
                                                                ID = sub_sub_ids_list[[a]][[b]],
                                                                rep = as.character(sub_sub_reps_list[[a]][[b]]) )
            } else {
              # reps does NOT exist - so FIRST CREATE THE NEW COLUMN!
              datatables[[ subtable_name ]] <- tibble::add_column( datatables[[ subtable_name ]], rep = "1" )
              # will automatically be called rep!
              # also automatically filled with 1s - as all samples in a subsample dt with NO REP COL will have rep of 1!
              datatables[[ subtable_name ]] <- dplyr::relocate( datatables[[ subtable_name ]], ID, rep )
              # this changes the order of the datatable so ID then rep cols are first
              # and now just add the ID and reps row(s):
              datatables[[ subtable_name ]] <- tibble::add_row( datatables[[ subtable_name ]],
                                                                ID = sub_sub_ids_list[[a]][[b]],
                                                                rep = as.character(sub_sub_reps_list[[a]][[b]]) )
            }

          }

        } else {

          # subtable DOES NOT exist - so CREATE THE NEW SUBSAMPLE TABLE

          # if the reps are 1 do NOT add a rep col:
          if(length(sub_reps_list_u[[a]]) == 1 && as.numeric(sub_reps_list_u[[a]][1]) == 1) {
            # only create tibbles with ID cols: NO REPS in this subsampling!

            # now for each UNIQUE DT code AND UNQIUE sub_rep_u create a tibble
            # must fill this with ONLY the IDs that have the given sub_rep_list[[a]][b] value!
            #for( a in 1:length(sub_dts_u) ) {
             # for( b in 1:length(sub_rep_u) ) {

                #print( paste0(table_name, "-", sub_rep_u[b], "_", sub_dts_u[a]) )
                # define the new subtable_name:
                subtable_name <- paste0(table_name, "-", sub_rep_u[b], "_", sub_dts_u[a])
                # create the tibble with the correct IDs:
                datatables[[ subtable_name ]] <- tibble::tibble(sub_sub_ids_list[[a]][[b]])
                names( datatables[[ subtable_name ]] ) <- "ID" # first col is ID
                # OMG this actually works!  MIND BENDING!!!
              #}

            #}

          } else { # if reps are more than one, add a rep col!

            # create tibbles with ID and rep cols
            rep <- NA_character_ # for adding new cols - all will be character vectors
            # now for each UNIQUE DT code, create a tibble
            #for( a in 1:length(sub_dts_u) ) {
             # for( b in 1:length(sub_rep_u) ) {

                subtable_name <- paste0(table_name, "-", sub_rep_u[b], "_", sub_dts_u[a])
                # create the tibble with the correct IDs:
                datatables[[ subtable_name ]] <- tibble::tibble(sub_sub_ids_list[[a]][[b]])
                names( datatables[[ subtable_name ]] ) <- "ID" # first col is ID

                datatables[[ subtable_name ]] <- tibble::add_column( datatables[[ subtable_name ]], rep ) # will automatically be called rep!
                datatables[[ subtable_name ]][[ 'rep' ]] <- as.character( sub_sub_reps_list[[a]][[b]] )
                # WORKS with sub_ub ids and reps! :O
              #}
            #}
          }
        }
    } # end if NULL

    } # end for b - sub_rep_u
    } # end for a - sub_dts

    }


  } else {  # no resample column exists - this is the FIRST RESAMPLING of this datatable:

      # so ADD resample and reps, and CREATE ALL THE NEW subsample DTs

    # CREATE resample and reps in parent sample DT

    newCol <- NA_character_ # for adding new cols - all will be character vectors
    for(i in 3:length(data) ) { # resample and reps are indices 3,4 in data

      # SKIP index 1 - this IS ID COL - which already exists in dt!
      # AND SKIP INDEX 2 - this is the rep COL - which already exists in dt!

      datatables[[ table_name ]] <- tibble::add_column( datatables[[ table_name ]], newCol )
      names(datatables[[ table_name ]])[names(datatables[[ table_name ]]) == "newCol"] <- data[[i]][1] # and SET THE NAME IMMEDIATELY!

      # and then add data in each row where ID is equal to data[[1]] ID
      # this handles if only a SUBSET of ID are being resampled in this table
      for( j in 2:length(data[[i]]) ) { # 2:length is all values
        # index_id <- match(data[[1]][j], datatables[[ table_name ]][["ID"]]) # data[[1]] is ID vector!
        ID <- data[[1]][j]
        rep <- data[[2]][j]
        index <- match( TRUE, (datatables[[ table_name ]][["ID"]] == ID & datatables[[ table_name ]][["rep"]] == rep) )

        # FAIL if datatables[[ table_name ]][[ data[[i]][1] ]][ index ] is NOT NA
        if( is.na(datatables[[ table_name ]][[ data[[i]][1] ]][ index ]) == FALSE ) {
          # this data val is ALREADY FILLED - must be trying to resample an ID that has already been resampled!
          stop( paste0("  Sample has previously been resampled: ", datatables[[ table_name ]][["ID"]][index]))
        }

        datatables[[ table_name ]][[ data[[i]][1] ]][ index ] <- data[[i]][j] # and then add data val at index j
      }

    } # THIS WORKS FINE - adds resample and reps cols to parent DT


    # NOW CREATE NEW / ADD TO SUBSAMPLE TIBBLES - and add to datatables
    rep <- NA_character_ # rep col - will be character vectors
    ID <- NA_character_ # ID col - will be character vectors

    for( a in 1:length(sub_ids) ) { # loop through sub_ids/rep /dts/reps
      sid <- sub_ids[[a]]
      srep <- sub_rep[[a]]

      # for each sub_ids / sub_rep there can be multiple sub_dts / sub_reps
      for( b in 1:length(sub_dts[[a]]) ) {

        # each [[a]][b] val in sub_dts / sub_reps is a NEW TIBBLE

         # SPLIT whether sub_reps is just 1 for ALL VALS, or not
        if( length(sub_reps_u) == 1 && as.numeric(sub_reps_u[1]) == 1 ) {

          # only create tibbles with ID cols: NO REPS in this subsampling at all!
          subtable_name <- paste0(table_name, "-", srep, "_", sub_dts[[a]][b])

          # this tibble MAY ALREADY EXIST - if an ID of same REP NUM has been created
          if( is.null(datatables[[ subtable_name ]]) == TRUE ) {

            # if the tibble doesnt exist, then create it, add ID col, and add current ID
            datatables[[ subtable_name ]] <- tibble::tibble()
            datatables[[ subtable_name ]] <- tibble::add_column(datatables[[ subtable_name ]], ID)
            datatables[[ subtable_name ]] <- tibble::add_row(datatables[[ subtable_name ]], ID = sid)

          } else { # the tibble DOES EXIST - so just add current ID to ID col of existing tibble
            datatables[[ subtable_name ]] <- tibble::add_row(datatables[[ subtable_name ]], ID = sid)
          }

        } else {

          # create tibbles with ID and REPS col:

          # only create tibbles with ID cols: NO REPS in this subsampling at all!
          subtable_name <- paste0(table_name, "-", srep, "_", sub_dts[[a]][b])
          sreps <- as.numeric(sub_reps[[a]][b])
          srepseq <- seq(sreps)

          # this tibble MAY ALREADY EXIST - if an ID of same REP NUM has been created
          if( is.null(datatables[[ subtable_name ]]) == TRUE ) {

            # if the tibble doesnt exist, then create it, add ID col, and add current ID
            datatables[[ subtable_name ]] <- tibble::tibble()
            datatables[[ subtable_name ]] <- tibble::add_column(datatables[[ subtable_name ]], ID)
            datatables[[ subtable_name ]] <- tibble::add_column(datatables[[ subtable_name ]], rep)
            for(c in 1:length(srepseq) ) {
              datatables[[ subtable_name ]] <- tibble::add_row(
                datatables[[ subtable_name ]],
                ID = sid,
                rep = as.character(srepseq[c]) )
            }

          } else { # the tibble DOES EXIST - so just add current ID and REP col of existing tibble

            for(c in 1:length(srepseq) ) {
              datatables[[ subtable_name ]] <- tibble::add_row(
                datatables[[ subtable_name ]],
                ID = sid,
                rep = as.character(srepseq[c]) )
            }

          } # else tibble EXISTS
        } # else tibbles IDs/REPs
      } # for b
    } # for a


    # OLD CODE - very complex and DOESNT WORK!!

    if(FALSE) {
    # EXTRACT the data values to form new subsample tibbles:

    # lists to store IDs and reps in:
    sub_ids_list <- list()
    sub_sub_ids_list <- list()
    sub_rep_list <- list()
    sub_reps_list <- list()
    sub_sub_reps_list <- list()

    # loop through each DT code
    for( a in 1:length(sub_dts_u) ) {
      # loop through each sub_dts - list element
      sub_dts_ind <- 1
      sub_sub_ids_list[[a]] <- list() # FIRST INDEX in sub_sub is the dts_u
      sub_sub_reps_list[[a]] <- list() # FIRST INDEX in sub_sub is the dts_u

      for( b in 1:length(sub_dts) ) { # this index is also the same length as sub_rep
        # & vector element     # REMEMBER the sub_rep number defines subsample tables too!
        for( c in 1:length(sub_dts[[b]]) ) {

          # if matches the uniqe val
          if(sub_dts_u[a] == sub_dts[[b]][c] ) {
            print(sub_dts_u[a])
            print(sub_ids[[b]])
            print(sub_reps[[b]][c])
            print(sub_dts_ind)

            # generate SEQUENCE of reps:
            reps <- seq( as.numeric(sub_reps[[b]][c]) )
            reps_length <- length(reps)
            # add to sub_ids_list & sub_reps_list:
            if(sub_dts_ind == 1) { # create if first index

              sub_ids_list[[a]] <- rep(sub_ids[[b]], reps_length)
              sub_rep_list[[a]] <- sub_rep[[b]]
              sub_reps_list[[a]] <- reps

              # treat sub_sub separately - as indices are added sub_dts_u FIRST
               # sceond indices of first list are filled FIRST
              if( as.numeric(sub_rep[[b]]) > length(sub_sub_ids_list[[a]]) ) {
                sub_sub_ids_list[[a]][[ as.numeric(sub_rep[[b]]) ]] <- rep(sub_ids[[b]], reps_length)
                sub_sub_reps_list[[a]][[ as.numeric(sub_rep[[b]]) ]] <- reps
              } else {
                sub_sub_ids_list[[a]][[ as.numeric(sub_rep[[b]]) ]] <- c(sub_sub_ids_list[[a]][[ as.numeric(sub_rep[[b]]) ]],
                                                                         rep(sub_ids[[b]], reps_length))
                sub_sub_reps_list[[a]][[ as.numeric(sub_rep[[b]]) ]] <- c(sub_sub_reps_list[[a]][[ as.numeric(sub_rep[[b]]) ]],
                                                                          reps)
              }
            } else { # add to with subsequent indices
              sub_ids_list[[a]] <- c(sub_ids_list[[a]], rep(sub_ids[[b]], reps_length))
              sub_rep_list[[a]] <- c(sub_rep_list[[a]], sub_rep[[b]])
              sub_reps_list[[a]] <- c(sub_reps_list[[a]], reps)

              # treat sub_sub separately - as indices are added sub_dts_u FIRST
               # sceond indices of first list are filled FIRST
              if( as.numeric(sub_rep[[b]]) > length(sub_sub_ids_list[[a]]) ) {
                sub_sub_ids_list[[a]][[ as.numeric(sub_rep[[b]]) ]] <- rep(sub_ids[[b]], reps_length)
                sub_sub_reps_list[[a]][[ as.numeric(sub_rep[[b]]) ]] <- reps
              } else {
                sub_sub_ids_list[[a]][[ as.numeric(sub_rep[[b]]) ]] <- c(sub_sub_ids_list[[a]][[ as.numeric(sub_rep[[b]]) ]],
                                                                         rep(sub_ids[[b]], reps_length))
                sub_sub_reps_list[[a]][[ as.numeric(sub_rep[[b]]) ]] <- c(sub_sub_reps_list[[a]][[ as.numeric(sub_rep[[b]]) ]],
                                                                          reps)
              }
            }
            sub_dts_ind <- sub_dts_ind + 1

          }

        } # for c
      } # for b
    } # for a


    # REMOVE all NULL values from sub_sub lists
    ssrl2 <- list()
    for(a in 1:length(sub_sub_reps_list) ) {
      ssrl2[[a]] <- sub_sub_reps_list[[a]][-which(sapply(sub_sub_reps_list[[a]], is.null))]
    }
    sub_sub_reps_list <- ssrl2

    ssil2 <- list()
    for(a in 1:length(sub_sub_ids_list) ) {
      ssil2[[a]] <- sub_sub_ids_list[[a]][-which(sapply(sub_sub_ids_list[[a]], is.null))]
    }
    sub_sub_ids_list <- ssil2

    # CREATE new subsample tibbles and ADD to datatables
    # either WITHOUT rep col - if all reps are 1
    # or WITH rep col - if ANY REP is above 1

    # sub_rep_list defines the set of SUBSAMPLES that denote each parent rep!

    if( length(sub_reps_u) == 1 && as.numeric(sub_reps_u[1]) == 1 ) {
      # only create tibbles with ID cols: NO REPS in this subsampling!

      # now for each UNIQUE DT code AND UNQIUE sub_rep_u create a tibble
       # must fill this with ONLY the IDs that have the given sub_rep_list[[a]][b] value!
      for( a in 1:length(sub_dts_u) ) {
        for( b in 1:length(sub_rep_u) ) {

          # only add if sub_sub_ids_list[[a]][[b]] is NOT NULL:
          # this can be NULL if the rep does not contain this particular subsample code!
          if( is.null( sub_sub_ids_list[[a]][[b]] ) == TRUE ) {
            # DO NOT ADD DATA

          } else {

            #print( paste0(table_name, "-", sub_rep_u[b], "_", sub_dts_u[a]) )
            # define the new subtable_name:
            subtable_name <- paste0(table_name, "-", sub_rep_u[b], "_", sub_dts_u[a])
            # create the tibble with the correct IDs:
            datatables[[ subtable_name ]] <- tibble::tibble(sub_sub_ids_list[[a]][[b]])
            names( datatables[[ subtable_name ]] ) <- "ID" # first col is ID
             # OMG this actually works!  MIND BENDING!!!
          }
        }
      }


    } else { # add ID and REP cols!

      # create tibbles with ID and rep cols
      rep <- NA_character_ # for adding new cols - all will be character vectors
      # now for each UNIQUE DT code, create a tibble
      for( a in 1:length(sub_dts_u) ) {
        for( b in 1:length(sub_rep_u) ) {

          # only add if sub_sub_ids_list[[a]][[b]] is NOT NULL:
          # this can be NULL if the rep does not contain this particular subsample code!
          if( is.null( sub_sub_ids_list[[a]][[b]] ) == TRUE ) {
            # DO NOT ADD DATA

          } else {

            subtable_name <- paste0(table_name, "-", sub_rep_u[b], "_", sub_dts_u[a])
            # create the tibble with the correct IDs:
            datatables[[ subtable_name ]] <- tibble::tibble(sub_sub_ids_list[[a]][[b]])
            names( datatables[[ subtable_name ]] ) <- "ID" # first col is ID

            datatables[[ subtable_name ]] <- tibble::add_column( datatables[[ subtable_name ]], rep ) # will automatically be called rep!
            datatables[[ subtable_name ]][[ 'rep' ]] <- as.character( sub_sub_reps_list[[a]][[b]] )
              # WORKS with sub_ub ids and reps! :O

          }
        } # for b
      } # for a

    } # end else add ID and rep cols

    }

  } # end else no resampling cols exist

  # return
  datatables

}


#' Extract datatable from character vector to LIST
#'
#' This extracts the data as presented in Rmd datatable format into a LIST,
#' where each element of the list is a Character Vector.  Each Character Vector
#' has FIRST ELEMENT as the column Header, then all subsequent elements are the
#' data values from the datatable.
#'
#' Any multi-observation data values are put into the SAME ELEMENT, using a
#' space to separate them.
#'
#' This list is then ready to be put into a new tibble: Each vector will be a
#' column in the tibble.
#'
extract_datatable <- function(dt_vector) {

  # column headers are at index 7: remove lead/lag whitespace, split on space, remove blank vals
  headers <- lapply(strsplit( as.character( trimws(dt_vector[7]) ), " "), function(x){x[!x ==""]})[[1]]
  headers_length <- length(headers)

  # column DELIMITERS are at index 8 - need to extract the INDICES to calc where any MULTI-OBSERVATIONS lie
  # multi-observations are laid out vertically - so on the next line.
  # to parse these, must know WHERE they sit relative to the column delimiters - to put the data in the correct col!
  delim_indices <- get_delim_indices(dt_vector[8])

  # from index 10 want to extract each data value
  data <- list()
  for(k in 1:length(headers) ) { data[[k]] <- headers[k] } # add headers to data list

  for(j in 10:length(dt_vector)-1 ) { # start at 10 - first line where data is declared!
    # get the data at line:
    d <- lapply(strsplit( as.character( trimws(dt_vector[j]) ), " "), function(x){x[!x ==""]})[[1]]
    # if character(0), ignore this line
    if( identical(d, character(0)) ) {
      # line is blank - IGNORE
    } else { # parse the line

      if( length(d) == headers_length ) {
        # the data row contains the same number of elements as headers
        # therefore can assume these all fit IN ORDER under the headers
        # add all data to data list:
        for(k in 1:length(d) ) { data[[k]] <- c(data[[k]], d[k]) }

      } else {
        # if length of d does NOT contain same num elements as headers
        # CAN be:
         # row contains one or more multi-observation cols - i.e the data should be added to the PREVIOUS DATA ENTRY
         # OR a row contains data that has some BLANK ENTRIES
        # These can be distinguished by whether the FIRST COL has an entry
         # FIRST COL must be ID or VARIABLE name -
            # will be BLANK if the row is multi-obv
            # OR will be FILLED with ID/VARIABLE name if row is NEW ROW with BLANK ENTRIES

        # so get delim indices of values
        di <- get_delim_indices(dt_vector[j])
        # get the COL INDICES - the col index under which each data points delim index fits under
        ci <- get_col_indices(di, delim_indices)

        if( is.na( match(1, ci) ) ) { # does NOT contain any data in COL 1
          # loop through ci - and ADD data to LAST entry - with space delimiter
          for(a in 1:length(ci) ) {
            data[[ci[a]]][length(data[[ci[a]]])] <- paste( data[[ci[a]]][length(data[[ci[a]]])], d[a] )
          }
        } else {
          # DOES contain data in COL 1 : add data as a NEW ROW
          # first format d to include blank entries in the vector where necessary
          dn <- rep("", headers_length) # create vector with blank lines
          for(k in 1:length(ci) ) {
            dn[ ci[k] ] <- d[k]
          }
          # THEN add the new dn vector - with "" blank entries in correct places - to data list:
          for(k in 1:length(dn) ) { data[[k]] <- c(data[[k]], dn[k]) }
        }

      }
    } # end parse line
  } # end for j

  # return:
  data

}



get_col_indices <- function(di, delim_indices, dt_length = 120) {

  delim_indices <- c(delim_indices, dt_length) # combine
  ci <- c()
  for(i in 1:length(di) ) {
    for(j in 1:length(delim_indices) ) {
      if(di[i] < delim_indices[j]) {
        ci <- c(ci, (j-1) ) # save the PREVIOUS delim_indices index!
        break
      }
    }
  }

  ci

}



#' extract the START of each column delimiter index
#'
#' String defines column widths: "    ======  =============  ==================  "
#'
#' This function extracts the indices where "=" starts after spaces.
#' More generally, it extracts indices where any NON-SPACE char starts after spaces.
#'
#' EG above returns c(5, 13, 28).
#'
#' Used for determining which COL a string from a multi-observation belongs to
#' As in this case, not each col is filled with a value!
#'
#' May also use for VALIDATING which cols data belongs to on a regular line...?
#'
#'
get_delim_indices <- function(delim) {

  delim_indices <- c()
  blank <- FALSE

  for(i in 1:nchar(delim) ) {

    s <- substr(delim, i, i)

    if(s != " " && blank==TRUE) {
      delim_indices <- c(delim_indices, i)
    }

    if(s != " " ) {
      blank <- FALSE
    }

    if(s == " ") {
      blank <- TRUE
    }

  }

  delim_indices

}



#' Build Datatable from Dataframe
#'
#' This handles multi columns correctly - when spaces exist between strings in
#' one data value, the data is split and correctly placed into a datatable
#' on separate rows.
#'
#' Handles dataframes with differnet numbers of multi-rows across different IDs.
#'
#' Creates a new Sample DataTable vector. The Sample DataTable will contain an
#' initial ID column containing the IDs vector, and an optional set of extra
#' data columns as specified in the data_cols vector.  If the table exceeds
#' `dt_length` characters (default 120), then the table is split into multiple
#' tables, with IDs as first col, and subsequent data_cols given in subsequent
#' tables.
#'
#' This method also supports adding Multi-Observations:  Multiple data points
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
#' @param dt_function The FUNCTION of the datatable to be built.
#'
#' @param datatable_name String of data table name - default "samples".
#'
#' @param MAX_DATATABLE_LENGTH Int of data table max length in characters.
#' Typically 120.
#'
#' @param DATATABLE_SPACER_CHAR Character used to underline column headers.
#' Typically '='.
#'
#' @export
build_datatable_from_dataframe <- function( ID_col, IDs, data_cols, data, dt_function,
                                            datatable_name, MAX_DATATABLE_LENGTH,
                                            DATATABLE_SPACER_CHAR ) {

  # CONVERT all data values in data to char
  data <- lapply(data, as.character)
  data <- lapply(data, function(x) replace(x, is.na(x), "-")) # replace an NA with blank char

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

  # Now - compute which cols will fit into tables
  # keep summing until row lengths exceed MAX_DATATABLE_LENGTH
  # THEN split the summing into a second list item
  row_length_list <- list(col_spacers_len)
  row_list_index <- 1
  initial_row_length <- col_spacers_len
  current_row_length <- col_spacers_len

  for( i in 1:length(data_col_wds) ) {

    current_row_length <- current_row_length + data_col_wds[i] # calc new row length

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


  # calc number of LINES for each ID in EACH TABLE
  # data contains a list of vectors to add
  # if each vector are of lengths MULTIPLES of the IDs, then will be adding MUTLI-COLS to the table
  # if so, need to compute this NOW and factor this into the spacing of rows in table!
  # check each element in each vector - split at spaces - if more than one object, these go on multi-rows/lines
  data_length <- lapply(data, length) # length of each vector
  ID_length <- length(IDs) # number of IDs

  lst <- list(1)
  # store each max_multiple for each table in a separate list element
  #max_multiple <- 0
  # start at 0 so max_multiple_list is always set to at least 1!

  multiple_list <- list()
  # also store each multiple value in a list of vectors
  multiple <- 1

  num_prev_data_cols <- 0

  for(a in 1:length(row_length_list) ) { # row_length_list list length - number of TABLES

    multiple_list[[a]] <- lst
    #max_multiple_list[[a]] <- lst

    for(b in 2:length(row_length_list[[a]]) ) { # (2:length) num data cols in this table - as FIRST INDEX is always the length of IDS
      for(c in 1:length(IDs) ) {
        # data_length index calc'd from row_length_list as:
        # num_prev_data_cols : number of data cols parsed in previous for a loops
        # added to ad end of each for a loop with number of cols traversed
        # (b-1) as row_length_list ALWAYS starts with the ID length in each list element
        # BUT data_length does not contain ID length!

        # NO NEED TO CHECK - individual rows may have different number of lines!  Going to capture this data in nested lists
        # first CHECK that data_lengths are EXACT MULTIPLES of ID_length:
        #if( (data_length[[ (num_prev_data_cols) + (b-1) ]] %% ID_length) != 0 ) {
        # if modulus is not 0, data_length is NOT an exact multiple of ID_length
        # stop( paste0("  Number of data points added is not an exact multiple of IDs: ", data[[a]], " IDs: ", IDs) )
        #}

        # NOW calculate the MAX multiple - what is the max integer difference between data and IDs lengths?
        #multiple <- data_length[[ (num_prev_data_cols) + (b-1) ]] / ID_length

        # ABOVE NO LONGER WORKS - need to calculate the multiples FROM THE DATA - string split at ' '
        multiple <- length(strsplit(  as.character( data[[ (num_prev_data_cols) + (b-1) ]][c] ) , ' ')[[1]])


        if(c == 1 ) {
          multiple_list[[a]][[b-1]] <- multiple
        } else {
          multiple_list[[a]][[b-1]][c] <- multiple
        }
        # add multiple to list:
        #if(b == 2) { # when b is 2, multiple_list at index a is NULL - so just add multiple to it
        #if( a > 1 ) {
        # add new sublist FIRST
        # multiple_list[[a]] <- lst
        #}
        #multiple_list[[a]][[b-1]][c] <- multiple
        #}else { # otherwise CONCAT current list item with multiple:
        # multiple_list[[a]][[b-1]][c] <- c( unlist(multiple_list[[a]][[b-2]]), multiple)
        #}

        # add multiple to max list IF exceeds previous max:
        #if(multiple >= max_multiple) {
        #  if( c == 1 ) {
        #    max_multiple_list[[a]] <- multiple # set max_multiple to current table index
        #    max_multiple <- multiple # and set max_multiple to keep track of the max in THIS ITERATION
        #  } else {
        #    max_multiple_list[[a]][c] <- multiple # set max_multiple to current table index
        #    max_multiple <- multiple # and set max_multiple to keep track of the max in THIS ITERATION
        #  }
        #}

      }
      # end of c

    }
    # end of b:
    # keep track of index of data_cols:
    num_prev_data_cols <- num_prev_data_cols + (length(row_length_list[[a]]) -1)
    # reset max_multiple:
    #max_multiple <- 0
  }


  max_multiple_list <- list()
  # NOW compute the maximum in each Vector in nested multiple_list - gives number of rows to add to each ID
  for(a in 1:length(row_length_list) ) {
    #max_multiple_list[[a]] <- lst
    for(b in 1:length(multiple_list[[a]][[1]])) {
      if( b == 1 ) {
        max_multiple_list[[a]] <- max( unlist(lapply(multiple_list[[a]], `[[`, b)) )
      } else {
        max_multiple_list[[a]][b] <- max( unlist(lapply(multiple_list[[a]], `[[`, b)) )
      }
    }
  }

  # cached data for each datatable in max_multiple_list and multiple_list
  # max_multiple_list returns the single LARGEST number of multiples for IDs data vals in a given table
  # multiple_list give a vector as each list element, with each number in the vector equal to the multiples for IDs in that data col


  # form initial SKELETON of each table to insert
  # HEADER, IDs col (with correct spacing from max_multiple_list), FOOTER

  # create a list to hold the generated data_tables in
  data_table_list <- list()

  for( a in 1:length(max_multiple_list) ) { # length max_multiple_list give number of TABLES to create

    # add HEADER - header info plus datatable column heads
    if( a == 1 ) {
      data_table_list[[a]] <- c(
        "+===============================================================================",
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
        "+===============================================================================",
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

    cat( "\n  add IDs - table: ", a )
    ids_spacer <- paste(replicate(col_spacers_len, " " ), collapse="" )
    #data_table_ids <- replicate( (length(IDs)*(1+max_multiple_list[[a]][1])), ids_spacer ) # start with POPULATED vector
    data_table_ids <- replicate( sum( (max_multiple_list[[a]] + 1) ), ids_spacer )
    # fill with SPACER equal to sum of the max multiples for each ID/ROW
    # gives correct spacing of IDs for adding multi observations in the data list
    for(i in 1:length(IDs) ) {

      # index must use max_multiple_list[[a]][1:i] to calc position of IDs
      #data_table_ids[(i*(1+max_multiple_list[[a]][1]))-(max_multiple_list[[a]][1])] <- paste0(
      data_table_ids[ sum( max_multiple_list[[a]][1:i]+1 ) - (max_multiple_list[[a]][i]) ] <- paste0(
        "    ",
        strrep(" ", ceiling( (ID_col_wd - nchar(IDs[i]))/2) ),
        IDs[i],
        strrep(" ", floor( (ID_col_wd - nchar(IDs[i]))/2) ),
        "  " ) # an ID - with correct spacing

    }


    # concat data_table and add FOOTER
    data_table_list[[a]] <- c(
      data_table_list[[a]],
      data_table_ids,
      "",
      "+===============================================================================",
      "",
      "",
      ""    )

  }


  # Now add each data_col,
  # according to the pre-defined number of cols calculated above in row_length_list
  # and the number of MULTI-ROWS computed in multiple_list / max_multiple_list

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

        #cat( "\n  data col::", data_cols[i], " index: ", i )
        #cat( "\n    data_col_wds:", data_col_wds[i] )
        #cat( "\n    col_spacers_len:", col_spacers_len )

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
        for( j in 1:( sum( (max_multiple_list[[a]] + 1) ) ) ) {
          # IDs start at index 10 : j+9
          data_table_list[[dt_index]][ j + 9 ] <- paste0(data_table_list[[dt_index]][ j + 9 ],
                                                         paste(replicate(nchar_row, " " ), collapse="" ) )
        }

        # add data to rows:
        #l <- 1
        for( j in 1:length(IDs) ) {

          # compute line index and line in table index
          index_line <- sum( max_multiple_list[[a]][1:j]+1 ) - (max_multiple_list[[a]][j])
          index_table <- 9 + index_line

          # SPLIT the data at spaces in case inserting multiple lines
          dat <- unlist(strsplit(data[[i]][j], ' '))
          # get the correct multiple with dat[k]

          for( k in 1:multiple_list[[a]][[(b-1)]][j] ) { # multiple_list at [[a]][(b-1)] is the NUMBER OF MULTIPLES for this data_col!

            # index must use max_multiple_list[[a]][1:i] to calc position of IDs
            #data_table_ids[(i*(1+max_multiple_list[[a]][1]))-(max_multiple_list[[a]][1])] <- paste0(
            #data_table_ids[ sum( max_multiple_list[[a]][1:i]+1 ) - (max_multiple_list[[a]][i]) ] <- paste0(
            #  [ sum( max_multiple_list[[a]][1:j]+1 ) - (max_multiple_list[[a]][j]) ]
            # adjust what is in the data_table at correct index:
            #data_table_list[[dt_index]][ 9 + (j*(1+max_multiple_list[[a]][1])) - (max_multiple_list[[a]][1]) + (k-1) ] <- paste0(
            data_table_list[[dt_index]][ index_table + (k-1) ] <- paste0(

              # keep what is already PREVIOUSLY in this String!
              # substr removes the extra blank lines added above
              substr( data_table_list[[dt_index]][ index_table + (k-1) ],
                      1,
                      (nchar(data_table_list[[dt_index]][ index_table + (k-1) ])-nchar_row) ),

              strrep(" ", ceiling( (data_col_wds[i] - nchar(as.character(dat[k]) ))/2) ),

              as.character(dat[k]),

              strrep(" ", floor( (data_col_wds[i] - nchar( as.character(dat[k]) ))/2) ),

              "  " ) # a data entry at ID [9+(j*2)-1] - this gives ID line index : add correct spacing to line entry
            # count iterations in j and k with l
            #print( paste0("  l: ", l, "  data[[i]][l]: ", data[[i]][l]) )
            #l <- l + 1
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

  # return
  data_tables

}




#' Return a datatable Rmd character vector from a tibble
#'
#' Assumes the tibble has initial col ID with all IDs in it. Spaces the
#' columns based on the col header and contents widths.
#'
#'
#' NOT CLEAR THIS FUNCTION HANDLES VARIALBE MULTI-ROWS CORRECTLY!
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
build_datatable_from_tibble <- function(tb, datatable_name, dt_function = "CREATE", dt_length = 120 ) {

  cat( "\nprojectmanagr::build_datatable_from_tibble():\n" )

  ### NAMED CONSTANTS ###
  MAX_DATATABLE_LENGTH <- dt_length
  DATATABLE_SPACER_CHAR <- "="

  # check tb has initial col called ID
  if( names(tb[1]) != "ID" ) {
    stop( paste0("  first col of tibble is not ID: ", names(tb[1]) ) )
  }

  # get length of table - num_rows
  num_rows <- length(tb[[1]])

  # set IDs and data_cols
  data_cols <-  names(tb) # get ALL incl ID

  # create a list to hold the generated data_tables in
  data_table_list <- list()
  data_table_id <- c()

  ### CREATE INITIAL DATATABLE LAYOUT ###

  # calc widths of all data cols
  data_col_wds <- c()
  for(i in 1:length(data_cols) ) {
    # if _dt col - must be 18 long to fit datetime: 2021/02/26:17:42
    if( endsWith(data_cols[i], "_dt") ) {
      data_col_wds[length(data_col_wds)+1] <- pmax(
        nchar(data_cols[i])+4,
        max(nchar(unlist(strsplit(as.character(tb[[i]]), ' '))))+2,
        18)
    } else {
      # data col wds need to be the max of the header, the data vals, or 5
      data_col_wds[length(data_col_wds)+1] <- pmax(
        nchar(data_cols[i])+4,
        max(nchar(unlist(strsplit(as.character(tb[[i]]), ' '))))+2,
        5) # pmax ensures min col width is 5!
    }
  }

  # if datatables are SPLIT when created for Rmd, need to track the data col widths and START INDICES
  # use a list of vectors;
  data_col_wds_list <- list() # list index for datatable, vector index for data col
  data_col_ind_list <- list() # list index for datatable, vector index for data col
  data_col_df_index_list <- list() # index of col in DF: list index for datatable, vector index for data col

  # Create initial Rmd datatable char vector for filling:

  # add HEADER - header info
  data_table_id <- c(
    "+===============================================================================",
    "",
    "",
    paste0("    ",datatable_name,"  :  ", dt_function),
    "",
    "",
    "    " ) # add the FIRST LINE of datatable with 4 SPACES

  # therefore initialise spacers_len to 4:
  col_spacers_len <- 4
  col_spacers_len_id <- col_spacers_len # also cache the original length in case further datatables are added
  dt_index <- 1 # increment this as more datatables are added
  idt <- 1 # increment as data_cols parsed, reset when new datatable added

  # now copy data_table_id into data_table_list in first index and build the data table:
  data_table_list[[1]] <- data_table_id

  # NOW just add data col headers:
  # only add as many headers that can fit in one datatable - determined by comparing dt_length and col_spacers_len
  # here can calc how many datatables will be needed!

  for( i in 1:length(data_cols) ) {

    cat( "\n\n\n    ", data_cols[i], "\n" )

    # first calc the number of chars to add to datatable
    col_spacers_curr <- col_spacers_len # cache current length
    col_spacers_len <- col_spacers_len + (data_col_wds[i]+2) #+2 for 2 spaces at end

    cat( "\n  data col::", data_cols[1], " index: ", i )
    cat( "\n    data_col_wds:", data_col_wds[i] )
    cat( "\n    col_spacers_len:", col_spacers_len )

    # check this doesnt exceed the max:
    if( col_spacers_len > MAX_DATATABLE_LENGTH ) {

      cat( " MAX : ", col_spacers_len, " i: ", i, "\n")
      # if it does, create a new datatable in the list, and set params to use this new DT
      dt_index <- (dt_index +1)
      data_table_list[[dt_index]] <- data_table_id
      data_table_list[[dt_index]][4] <- paste0("    ",datatable_name,"  :  ADD_DATA") # set datatable header to ADD_DATA
      # prevents syntax error during the reading of data tables!  datatable_read_rmd()

      # DONT FORGET TO ADD ID COL to new datatable!
      # col title is index 7, ID is index 1
      data_table_list[[dt_index]][7] <- paste0(

        data_table_list[[dt_index]][7], # keep what is already in this String!

        strrep(" ", ceiling( (data_col_wds[1] - nchar(data_cols[1]))/2) ),

        data_cols[1],

        strrep(" ", floor( (data_col_wds[1] - nchar(data_cols[1]))/2) ),

        "  " ) # Col Title : - with correct spacing
      # col_spacers_len will be original PLUS the ID col spacer
      col_spacers_len <- col_spacers_len_id + (data_col_wds[1]+2)

      idt <- 1 # reset the datatableindex for data_cols
      # save data col width in the list for col widths ACROSS datatables
      # first is ID so save this:
      data_col_wds_list[dt_index] <- data_col_wds[1]
      # and save ID START INDEX of data_col!
      data_col_ind_list[dt_index] <- col_spacers_len_id
      data_col_df_index_list[[dt_index]] <- 1

      # now increament idt to 2 - so adding to these lists at index 2 remaining data_col params at index i
      idt <- 2
      # reset col_spacers_curr and len too
      col_spacers_curr <- col_spacers_len_id + (data_col_wds[1]+2)
      col_spacers_len <- col_spacers_len + (data_col_wds[i]+2) #+2 for 2 spaces at end

    }

    # save data col width in the list for col widths ACROSS datatables
    if(idt == 1) {
      data_col_wds_list[[dt_index]] <- data_col_wds[i]
      # and save CURRENT START INDEX of data_col!
      data_col_ind_list[[dt_index]] <- col_spacers_curr
      data_col_df_index_list[[dt_index]] <- i
    } else {
      data_col_wds_list[[dt_index]] <- c(data_col_wds_list[[dt_index]], data_col_wds[i])
      # and save CURRENT START INDEX of data_col!
      data_col_ind_list[[dt_index]] <- c(data_col_ind_list[[dt_index]], col_spacers_curr)
      data_col_df_index_list[[dt_index]] <- c(data_col_df_index_list[[dt_index]], i)
    }


    # add the data col and the default data vals for each ID

    # col title is index 7
    data_table_list[[dt_index]][7] <- paste0(

      data_table_list[[dt_index]][7], # keep what is already in this String!

      strrep(" ", ceiling( (data_col_wds[i] - nchar(data_cols[i]))/2) ),

      data_cols[i],

      strrep(" ", floor( (data_col_wds[i] - nchar(data_cols[i]))/2) ),

      "  " ) # Col Title : - with correct spacing

    idt <- (idt+1) # increment this with i

  }

  # now use data_col_wds_list & data_col_ind_list to place the DATATABLE_SPACER_CHARs under col headers

  for( i in 1:dt_index ) {

    data_table_list[[i]] <- c(data_table_list[[i]], "    ") # add new line with 4 spaces!

    for( j in 1:length(data_col_ind_list[[i]]) ) {

      # col title is index 8
      data_table_list[[i]][8] <- paste0(

        data_table_list[[i]][8], # keep what is already in this String!

        strrep(DATATABLE_SPACER_CHAR, data_col_wds_list[[i]][j]),

        "  ") # Spacers under data col - '=' by default

    }

  }

  # Now use the data_col_wds_list to place the data under col headers

  # variable to store the current vector index to write to FOR EACH DATATABLE
  vec_indices <- rep(10, dt_index)

  for(a in 1:num_rows ) {

    # add each row to each dt_index:
    for( i in 1:dt_index ) {

      # for each new row / dt_index, add a BLANK line then a new line with 4 spaces!
      data_table_list[[i]] <- c(data_table_list[[i]], "", "    ")

      # use vec_indices at index i:
      vec_ind <- vec_indices[i]

      # reset max_data_val_length to 1 for each dt index
      max_dv_len <- 1

      # then iterate through all data cols for this dt_index
      for( j in 1:length(data_col_ind_list[[i]]) ) {

        #default_data_val <- df[j][(vec_ind-vec_ind_diff)]

        default_data_val <- tb[[ data_col_df_index_list[[i]][j] ]][ a ]

        if( is.na(default_data_val) ) {
          # set to BLANK ""
          default_data_val <- ""
        }

        # need to SPLIT this by spaces - if its a multi-observation!
        if(default_data_val != "" ) {
          default_data_val <- unlist( strsplit( as.character(default_data_val), ' ') )
        }

        # keep track of MAX DATA_VAL LENGTH!
        if( length(default_data_val) > max_dv_len) {
          max_dv_len <- length(default_data_val)
        }

        # get the length of the current line - this is used for adding spacing to any multi-obv!
        line_len <- nchar( data_table_list[[i]][vec_ind] )

        # then place each one on a new line:
        for( k in 1:length(default_data_val) ) {

          if(k == 1 ) { # add the first data_val using standard code:

            data_table_list[[i]][vec_ind] <- paste0(

              data_table_list[[i]][vec_ind], # keep what is already in this String!

              strrep(" ", ceiling( (data_col_wds_list[[i]][j] - nchar(default_data_val[k]))/2) ),

              default_data_val[k],

              strrep(" ", floor( (data_col_wds_list[[i]][j] - nchar(default_data_val[k]))/2) ),

              "  " ) # a data entry at ID [9+(j*2)-1] - this gives ID line index : add correct spacing to line entry

          } else { # k > 1 -> so need to add the extra line CORRECTLY

            vec_ind2 <- vec_ind + (k-1) # the new index

            if( is.na(data_table_list[[i]][vec_ind2]) == TRUE ) {
              # in this case, the line doesnt exist, so need to add all the space up to the current col

              data_table_list[[i]][vec_ind2] <- paste0(

                strrep(" ", line_len), # ADD previous char count as whitespace

                strrep(" ", ceiling( (data_col_wds_list[[i]][j] - nchar(default_data_val[k]))/2) ),

                default_data_val[k],

                strrep(" ", floor( (data_col_wds_list[[i]][j] - nchar(default_data_val[k]))/2) ),

                "  " ) # a data entry at ID [9+(j*2)-1] - this gives ID line index : add correct spacing to line entry

            } else {
              # in this case, the line ALREADY exist, so need to add only the EXTRA space up to the current col

              data_table_list[[i]][vec_ind2] <- paste0(

                data_table_list[[i]][vec_ind2], # MUST NOW keep what is already in this String!

                strrep(" ", line_len - nchar(data_table_list[[i]][vec_ind2]) ), # ADD previous char count MINUS CURRENT LINE COUNT as whitespace

                strrep(" ", ceiling( (data_col_wds_list[[i]][j] - nchar(default_data_val[k]))/2) ),

                default_data_val[k],

                strrep(" ", floor( (data_col_wds_list[[i]][j] - nchar(default_data_val[k]))/2) ),

                "  " ) # a data entry at ID [9+(j*2)-1] - this gives ID line index : add correct spacing to line entry

            } # end line EXISTS

          } # end k > 1

        } # end data_val length

      } #end data_col

      # BEFORE moving to the NEXT ROW, set the index!
      # max_dv_len is always AT LEAST 1, so just need to add 1 for the blank line inserted at start of dt_index loop for each dt..
      vec_indices[i] <- vec_ind + 1 + max_dv_len

    } # end dt_index

  } # end a : num_rows


  # add the footer to each datatable:

  # add each row to each dt_index:
  for( i in 1:dt_index ) {

    # concat data_table and add FOOTER
    data_table_list[[ i ]] <- c(
      data_table_list[[ i ]],
      "",
      "",
      "+===============================================================================",
      "",
      "",
      ""    )

  }

  # data_table_list has one or more data_tables with correct spacing, and width does not exceed MAX_DATATABLE_LENGTH

  # generate single character vector containing all rmd data tables:
  data_tables <- unlist(data_table_list)

  # return
  data_tables

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


