
#' Read all complete Datatables - between to delimiters
#'
#' Delimiters defined in `datatable_get_delimiter()`
#'
#' @param rmd_contents String vector containing datatables between delimiters
#'
#' @return A named list of tibbles.
#'
#' @export
datatable_read_vector <- function(rmd_contents, notify=TRUE) {

  # identify the lines that begin with : datatable_get_delimiter()
  indices <- which( startsWith(rmd_contents, datatable_get_delimiter()) )

  if( length(indices) == 0 ) { # skip parsing rmd_contents if no datatables!
    if(notify) {
      cat( paste0("  no datatables in contents" ) )
    }
    return(list()) # return a blank list
  } else if( length(indices) < 2 ) { # there are no datatables
    return(list()) # return a blank list
  } else {
    # from indices extract the actual tables - they sit between the first and second dividers
    if( (length(indices) %% 2) == 1 ) { # dont fail, just do not read the odd index
      indices <- indices[1:(length(indices)-1)]
    }
    read_data_tables(rmd_contents, indices)
  }
}


#' Read Data Tables
#'
#' Read plaintext data tables into tibbles.
#'
#' @param rmd_contents Character Vector containing Data Tables to read.
#'
#' @param indices Line indices of table delimiters : from `datatable_get_delimiter()`
#'
#' @return A named list of tibbles.
read_data_tables <- function(rmd_contents, indices) {

  tables <- extract_data_tables(rmd_contents, indices) # that sit between the first and second dividers
  datatables <- list() # store each datatable CREATED or IMPORTED in this list
  grouptables <- list() # store each grouptable in this list - keep separate as will lookup which groups sample IDs belong to as needed

  # parse each data table to build in-memory representation of them:
  # samples must be in CREATE or IMPORT tables FIRST
  # samples can be GROUPED - save group tables and apply any further group refs to the samples directly

  for(i in 1:length(tables) ) { # parse each table IN ORDER
    #for(i in 1:2 ) {
    # i <- 5

    # extract each to new strings:
    table_name <- extract_table_name(tables[[i]])
    table_function <- extract_table_function(tables[[i]])
    table_start_line <- indices[(i*2-1)]

    if( table_name != "TEMPLATE" ) { # only parse if datatable is not template

      if( table_function == "CREATE" || table_function == "IMPORT" ) {
        # using parse_datatable_create() for CREATE IMPORT datatables

        # CREATE : just parse the tables character vector to create a tibble
        datatables[[ table_name ]] <- parse_datatable_create(tables[[i]], datatables, table_name, table_start_line)


      } else if( (table_function == "ADD_DATA") || (table_function == "GROUP")
                 || (table_function == "EXPORT") || (table_function == "DISPOSE") ) {
        # using parse_datatable_add_data() for ADD_DATA GROUP EXPORT DISPOSE datatables

        # DEAL WITH NEW SYNTAX - declaring multiple datatable names in one:
        table_names <- unlist(strsplit( table_name, " ") )
        # remove blank elements - happens if the table_names were separated by MORE THAN ONE SPACE
        table_names <- table_names[table_names != ""]

        for(f in 1:length(table_names) ) {

          table_name <- check_table_name(table_names[f], datatables, i, table_start_line)

          # ADD_DATA : parse tables character vector and CHECK AGAINST EXISTING datatables to ADD DATA TO TABLE
          datatables[[ table_name ]] <- parse_datatable_add_data(tables[[i]], datatables, table_name, table_start_line)

        }

      } else if( table_function == "RESAMPLE" ) {

        # FIRST check that table_name ALREADY EXISTS in datatables
        if( any(names(datatables) == table_name) == FALSE ) {
          # no datatable of name table_name to add data to - STOP
          stop( paste0("  No datatable exists to add data to: ", table_name, " table index: ", i) )
        }

        # RESAMPLE : parse tables character vector and the datatables list,
        # return the new datatables list with NEW DATATABLES ADDED:
        datatables <- parse_datatable_resample(tables[[i]], datatables, table_name, table_start_line)

      } # end if EXPORT
    } # end if TEMPLATE
  } # for i

  datatables # return
}

#' Extract Data Tables
#'
#' Separate each data table character vector into a new list.
#'
#' @param rmd_contents Character Vector containing Data Tables to read.
#'
#' @param indices Line indices of table delineators : `+====`.
#'
#' @return List of character vectors, each containing a data table.
extract_data_tables <- function(rmd_contents, indices) {
  # extract all data tables - that sit between the first and second dividers
  tables <- list()
  for(i in 1:(length(indices)/2) ) {
    end <- (i*2)
    start <- (i*2)-1
    tables[[i]] <- rmd_contents[ indices[start]:indices[end] ]
  }
  tables # return
}

#' Extract Table Name
#'
#' @param table_vector A character vector containing a data table.
#'
#' @return the plaintext datatable name from `table_vector`
extract_table_name <- function(table_vector) {
  trimws(strsplit( as.character(table_vector[4]), ":")[[1]][1]) # line 4 contains table name & function
}

#' Extract Table Function
#'
#' @param table_vector A character vector containing a data table.
#'
#' @return the plaintext datatable function from `table_vector`
extract_table_function <- function(table_vector) {
  trimws(strsplit( as.character(table_vector[4]), ":")[[1]][2]) # line 4 contains table name & function
}


#' Extract Template Named Table
#'
#' Returns the name. eg from NAMED TEMPLATE TABLE below:
#'
#'`    TEMPLATE  :  CREATE  :  fix-solution-wts`
#'
#' Will return "fix-solution-wts" - returns NA if no name exists.
#'
#' @param table_vector A character vector containing a data table.
#'
#' @return the plaintext datatable function from `table_vector`
extract_template_named_table <- function(table_vector) {
  nt <- trimws(strsplit( as.character(table_vector[4]), ":")[[1]][3]) # line 4 contains table name & function
}

#' Check Table Name
#'
#' Ensure `table_name` exists in datatables list, otherwise fail gracefully.
check_table_name <- function(table_name, datatables, i, table_start_line) {
  # FIRST check that table_name ALREADY EXISTS in datatables
  if( any(names(datatables) == table_name) == FALSE ) {
    # no datatable of name table_name to add data to - STOP
    stop( paste0("  No datatable exists to add data to: ", table_name, " table index: ", i, "\n rmd line: ", table_start_lines) )
  }
  table_name # return
}


#' CREATE a tibble from a datatable Rmd vector
#'
#' The `dt_vector` has the FUNCTION : CREATE
#'
#' MUST have IDs in the first column.  Can have subsequent columns with
#' further data.
#'
#' @param dt_vector String vector containing the datatable to parse : should be
#' of FUNCTION CREATE.
#'
#' @param datatables List of existing datatables extracted before the datatable
#' in `dt_vector`.
#'
#' @param table_name Name of datatable in `dt_vector`.
#'
#' @param table_start_line The line in RMD where the table starts - to locate it
#' if any errors are generated.
#'
parse_datatable_create <- function(dt_vector, datatables, table_name, table_start_line) {

  # CHECK VALIDITY FIRST:
  check_datatable_validity(dt_vector, table_start_line)
  headers <- extract_headers(dt_vector)
  check_datatable_header_ids( headers )

  # convert dt_vector into a LIST of character vectors
  data <- datatable_extract(dt_vector)
    # col HEADER: FIRST entry in each vector
    # col DATA : remaining entries
    # Adding multi-observations: in the SAME vector element, SPACE SEPARATED
    # Missing data vals: BLANK ELEMENT is inserted

  if( headers[2] == "rep" ) { # deal with reps appropriately - may exist in IMPORT dt

    #### CREATE tibble : reps ####
    id_rep <- extract_ids_reps(data, table_name, table_start_line) # ID col is first vector
    eird <- expand_id_rep_data(id_rep$IDs, id_rep$REPs, data, tibble::tibble())
    dt <- create_tibble("ID", eird$IDs)
    dt <- add_data_col_type(dt, "rep", eird$REPs)
    data <- eird$DATA

    if(length(data) > 2)  { # Add all subsequent cols, if they exist
      for(i in 3:length(data) ) {
        dt <- add_data_col_type(dt, data[[i]][1], data[[i]][2:length( data[[i]] ) ]  )
        # data[[i]][1] is DATA COL NAME
        # data[[i]][1] is DATA COL VALUES
      }
    }

  } else {

    #### CREATE tibble : no reps ####
    IDs <- extract_ids(data, table_name, table_start_line) # ID col is first vector
    dt <- create_tibble("ID", IDs)

    if(length(data) > 1)  { # Add all subsequent cols, if they exist
      for(i in 2:length(data) ) {
        dt <- add_data_col_type(dt, data[[i]][1], data[[i]][2:length( data[[i]] ) ]  )
        # data[[i]][1] is DATA COL NAME
        # data[[i]][1] is DATA COL VALUES
      }
    }
  }

  # COMBINE dt with EXISTING DATATABLE of SAME NAME - if exists && no clashes with IDs && all cols are present
  dt <- combine_with_existing(dt, datatables, table_name, table_start_line)

  # return dataframe
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
parse_datatable_add_data <- function(dt_vector, datatables, table_name, table_start_line) {

  # CHECK VALIDITY FIRST:
  check_datatable_validity(dt_vector, table_start_line)

  # convert dt_vector into a LIST of character vectors
  data <- datatable_extract(dt_vector)
  # col HEADER: FIRST entry in each vector
  # col DATA : remaining entries
  # Adding multi-observations: in the SAME vector element, SPACE SEPARATED
  # Missing data vals: BLANK ELEMENT is inserted

  # extract headers - and parse data according to content
  headers <- extract_headers(dt_vector)

  # ADD DATA to tibble from the data LIST
  # this depends on what FORMAT the datatable was in in the Rmd
  # samples first - first col is ID
  # variables first - first col is variables
  # timetable - first col is timetable

  if( headers[1] == "ID" ) {

    #### ADD_DATA variables ####
    dt <- parse_datatable_add_data_variables(data, headers, datatables, table_name, table_start_line)

  } else if( headers[1] == "variables" ) {

    #### ADD_DATA ids ####
    dt <- parse_datatable_add_data_ids(data, headers, datatables, table_name, table_start_line)

  } else if( headers[1] == "timetable") {

    #### ADD_DATA timetable ####
    dt <- parse_datatable_add_data_timetable(data, headers, datatables, table_name, table_start_line)

  } else {

    # unrecognised first header - STOP
    stop(paste0("  Unrecognised first header column : ", header[1],
                "\n    must be 'ID' 'variables' or 'timetable'",
                "\n table name: ", table_name, "  RMD line: ", table_start_line) )
  }

  dt # return

}


#' Parse ADD_DATA variables layout
#'
#' SAMPLE-FIRST datatable : IDs in first col, new data in remaining cols
#'
parse_datatable_add_data_variables <- function(data, headers, datatables, table_name, table_start_line) {

  # this is a SAMPLE-FIRST data table! IDs in first col (REPS in second), new data in remaining cols
  dt <- datatables[[table_name]]

  if( headers[2] == "rep" ) { # IDs & REPs present : new data is in all vectors 3:length

    #### IDs + REPs ####

    dataADD <- data[3:length(data)] # separate new data from IDs and rep
    group_names <- data[[1]][2:length(data[[1]])] # get the ID data
    rep_names <- data[[2]][2:length(data[[2]])] # get the rep data

    if( length(group_names)==1 && group_names[1] == "ALL" ) { # fill each data col with data from all valid ID indices

      dt <- add_data_variables_all_rep(dt, dataADD, table_name, table_start_line)

    } else if( all(group_names %in% dt$ID) ) { # group_names are IDs - so fill by VALID ID/REP indices in group_names/rep_names

      dt <- add_data_variables_id_rep(dt, group_names, rep_names, dataADD, table_name, table_start_line)

    } else { # group_names correspond to a group-col in dt? Endure data only added to VALID ID/REP indices in dt

      # this situation will never exist?  As adding to group IDs will not have a rep column?!
      #group_col_name <- find_group_col(group_names, get_group_cols(dt), table_name, table_start_line)
      #add_data_variables_group(dt, group_names, group_col_name, dataADD)
    }
  } else { # IDs ONLY present : new data is in all vectors 2:length

    #### IDs ONLY ####

    dataADD <- data[2:length(data)] # separate new data from IDs
    group_names <- data[[1]][2:length(data[[1]])] # get the ID data

    if( length(group_names)==1 && group_names[1] == "ALL" ) { # fill each data col with data from all valid ID indices

      dt <- add_data_variables_all(dt, dataADD, table_name, table_start_line)

    } else if( all(group_names %in% dt$ID) ) { # group_names are IDs - so fill by ID indices

      dt <- add_data_variables_id(dt, group_names, dataADD, table_name, table_start_line)

    } else {  # group_names correspond to a group-col in dt?

      group_col_name <- find_group_col(group_names, get_group_cols(dt), table_name, table_start_line)
      dt <- add_data_variables_group(dt, group_names, group_col_name, dataADD, table_name, table_start_line)
    }
  }
  dt # return
}

#' Parse Datatable Add Data IDs
#'
#' PARSE ADD_DATA datatable with IDs as column names.  The first column header is
#' set to `variables` to indicate this layout.
#'
#' This function rearranges the extracted `data` and `headers`, and then parses
#' this through `parse_datatable_add_data_variables()`
#'
parse_datatable_add_data_ids <- function(data, headers, datatables, table_name, table_start_line) {

  #### Check & Transpose Inputs ####
  # re-arrange data + headers & parse via parse_datatable_add_data_variables
  data2 <- lapply(purrr::transpose(data), unlist)
  data2[[1]][1] <- "ID" # set first col name to ID from variables
  headers2 <- lapply(data2, '[', 1)
  group_names <- data2[[1]][2:length(data2[[1]])]

  # fail gracefully if dt has rep col and "IDs" are actual IDs from dt
  if( all(group_names %in% datatables[[table_name]]$ID) && names(datatables[[table_name]])[2] == "rep" ) {
    stop(paste0("  cannot parse id-col ADD_DATA datatable to tibble with reps - ids : ",
                paste(data2[[1]][2:length(data2[[1]])], collapse=" "), "\n table name: ", table_name,
                "  RMD line: ", table_start_line) )
  }

  #### parse through add_data_variables ####
  parse_datatable_add_data_variables(data2, headers2, datatables, table_name, table_start_line)

}

#' Parse Datatable Add Data TIMETABLE
#'
#' PARSE ADD_DATA datatable in Timetable Format: The first column header is set
#' to `timetable`, which has planned times for condition changes.  Subsequent
#' columns are `group` names that must already exist in the `datatables` list
#' under `table_name.`  The final column is `<col_name>_dt`, and datetimes are
#' inserted here to match conditions declared in the `group` names columns.
#'
#' This function rearranges the extracted `data` and `headers`, and then parses
#' this through `parse_datatable_add_data_variables()`
#'
parse_datatable_add_data_timetable <- function(data, headers, datatables, table_name, table_start_line) {

  #### Check & Transpose Inputs ####
  data2 <- list()
  # extract the group names to first vector
  col_head <- unlist(lapply(data, '[[', 1))
  data2[[1]] <- c("ID", col_head[2:(length(col_head)-1)] )
  # extract conditions and datetimes to other vectors
  COL <- data[[length(data)]]
  GROUPS <- data[2:(length(data)-1)]
  data2[[2]] <- c(gsub('_dt', '_con', COL[1]), rep("", length(GROUPS))) # _con column
  data2[[3]] <- c(COL[1], rep("", length(GROUPS))) # _dt column
  # fill _con and _dt cols
  for( dc in 2:length(COL) ) {
    for( gv in 1:length(GROUPS) ) {
      if( GROUPS[[gv]][dc] != "" ) { # add condition and datetimes
        data2[[2]][(gv+1)] <- paste(data2[[2]][(gv+1)], GROUPS[[gv]][dc]) # condition
        data2[[3]][(gv+1)] <- paste(data2[[3]][(gv+1)], COL[dc]) # datetime
      }
    }
  }
  data2[[2]] <- trimws(data2[[2]]) # remove excess whitespace
  data2[[3]] <- trimws(data2[[3]]) # remove excess whitespace
  headers2 <- lapply(data2, '[', 1)
  group_names <- data2[[1]][2:length(data2[[1]])]

  # fail gracefully if dt has rep col and "IDs" are actual IDs from dt
  if( all(group_names %in% datatables[[table_name]]$ID) && names(datatables[[table_name]])[2] == "rep" ) {
    stop(paste0("  cannot parse id-col ADD_DATA datatable to tibble with reps - ids : ",
                paste(data2[[1]][2:length(data2[[1]])], collapse=" "), "\n table name: ", table_name,
                "  RMD line: ", table_start_line) )
  }

  #### parse through add_data_variables ####
  parse_datatable_add_data_variables(data2, headers2, datatables, table_name, table_start_line)

}


parse_datatable_resample <- function(dt_vector, datatables, table_name, table_start_line) {

  headers <- extract_headers(dt_vector)
  headers_length <- length(headers)

  if( (headers_length == 3) ) { # headers_length MUST be 3, and headers MUST be "ID" "resample" and "reps" in this order

    resample_check_headers_id(headers, table_name, table_start_line)
    datatables <- parse_datatable_resample_single(dt_vector, datatables, table_name, table_start_line)

  } else if( headers_length == 4 ) { # OR headers_length CAN be 4, and headers MUST be "ID" "rep" "resample" and "reps" in this order

    resample_check_headers_id_rep(headers, table_name, table_start_line)
    datatables <- parse_datatable_resample_reps(dt_vector, datatables, table_name, table_start_line)

  } else {
    # unrecognised header length - STOP
    stop(paste0("  Resample datatable cols must be ID [rep] resample reps - current cols: ",
                paste(headers, collapse=" "),
                "\n table name: ", table_name, "  RMD line: ", table_start_line) )
  }

  # return
  datatables

}


resample_check_headers_id <- function(headers, table_name, table_start_line) {
  if((headers[[1]] != "ID")  ) {
    stop( paste0("  Resample datatable first col MUST be ID - current cols: ", paste(headers, collapse=" "),
                 "\n table name: ", table_name, "  RMD line: ", table_start_line) )
  }

  # headers_length MUST be 3, and headers MUST be "ID" "resample" and "reps" in this order
  if((headers[[2]] != "resample")  ) {
    stop( paste0("  Resample datatable second col MUST be resample - current cols: ", paste(headers, collapse=" "),
                 "\n table name: ", table_name, "  RMD line: ", table_start_line) )
  }

  # headers_length MUST be 3, and headers MUST be "ID" "resample" and "reps" in this order
  if((headers[[3]] != "reps")  ) {
    stop( paste0("  Resample datatable third col MUST be reps - current cols: ", paste(headers, collapse=" "),
                 "\n table name: ", table_name, "  RMD line: ", table_start_line) )
  }
}


resample_check_headers_id_rep <- function(headers, table_name, table_start_line) {

  if((headers[[1]] != "ID")  ) {
    stop( paste0("  Resample datatable first col MUST be ID - current cols: ", paste(headers, collapse=" "),
                 "\n table name: ", table_name, "  RMD line: ", table_start_line) )
  }


  if((headers[[2]] != "rep")  ) {
    stop( paste0("  Resample datatable second col MUST be rep: ", paste(headers, collapse=" "),
                 "\n table name: ", table_name, "  RMD line: ", table_start_line) )
  }

  if((headers[[3]] != "resample")  ) {
    stop( paste0("  Resample datatable third col MUST be resample: ", paste(headers, collapse=" "),
                 "\n table name: ", table_name, "  RMD line: ", table_start_line) )
  }

  if((headers[[4]] != "reps")  ) {
    stop( paste0("  Resample datatable fourth col MUST be reps: ", paste(headers, collapse=" "),
                 "\n table name: ", table_name, "  RMD line: ", table_start_line) )
  }
}

#' Parse a Resample datatable with single SOURCE sample IDs
#'
#' This will generate the new datatables from a resample dt_vector that contains
#' single SOURCE sample IDS - it may still generate multiple REPS of subsamples!
#'
#'
parse_datatable_resample_single <- function(dt_vector, datatables, table_name, table_start_line) {

  dt <- datatables[[table_name]]

  data <- datatable_extract(dt_vector)
  id_names <- data[[1]][2:length(data[[1]])] # get the ID data
  resample_names <- data[[2]][2:length(data[[1]])] # get the resample strings
  reps_names <- data[[3]][2:length(data[[2]])] # get the resample reps

  datatables <- resample_id(datatables, id_names, resample_names, reps_names,
                            table_name, table_start_line)

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
parse_datatable_resample_reps <- function(dt_vector, datatables, table_name, table_start_line) {

  dt <- datatables[[table_name]]

  data <- datatable_extract(dt_vector)
  id_names <- data[[1]][2:length(data[[1]])] # get the ID data
  rep_names <- data[[2]][2:length(data[[2]])] # get the rep data
  resample_names <- data[[3]][2:length(data[[1]])] # get the resample strings
  reps_names <- data[[4]][2:length(data[[2]])] # get the resample reps

  datatables <- resample_id_rep(datatables, id_names, rep_names, resample_names,
                                reps_names, table_name, table_start_line)

}

#' extract headers from plaintext datatable
extract_headers <- function(dt_vector) {
  # column headers are at index 7: remove lead/lag whitespace, split on space, remove blank vals
  lapply(strsplit( as.character( trimws(dt_vector[7]) ), " "), function(x){x[!x ==""]})[[1]]
}


#' check syntax of dt_vector for validity
check_datatable_validity <- function(dt_vector, table_start_line) {

  if( !grepl("^[= ]+$", dt_vector[8], perl=T) ) {# column delims (vector index 8) contains only ' ' and '=' otherwise STOP
    stop( paste0("  Column Delimiter row has a syntax error: ", dt_vector[8]))
  }

  if( trimws(paste0(dt_vector[2],dt_vector[3],dt_vector[5],
                    dt_vector[6],dt_vector[9])) !="") { # lines 2,3,5,6,9 should be BLANK otherwise STOP
    stop( paste0("  Datatable has a syntax error - RMD line: ", table_start_line) )
  }
}


check_datatable_header_ids <- function(headers) {
  if(headers[1] != "ID" ) {# first col MUST be ID otherwise STOP
    stop( paste0("  First column header is not ID: ", headers[1]))
  }
}


#' Extract IDs
#'
#' Extract IDs vector from the data list - IDs should be in FIRST VECTOR,
#' and ID values should be in 2:length of that vector.
#'
#' Fails gracefully if any IDs are repeated.
#'
#' @param data A list containing vectors of data, with ID in the first vector.
#'
#' @param table_name name of data table.
#'
#' @param table_start_line Line data table starts on in character vector.
#'
#' @return the plaintext datatable function from `table_vector`
extract_ids <- function(data, table_name, table_start_line) {
  IDs <- data[[1]][2:length(data[[1]])] # ID col is first vector
  if( any(duplicated(IDs)) ) {
    stop( paste0("  IDs has duplicates: ", paste(IDs, collapse=" "), "\n datatable: ", table_name, " - rmd line: ", table_start_line) )
  }
  IDs # return
}

extract_ids_reps <- function(data, table_name, table_start_line) {
  IDs <- data[[1]][2:length(data[[1]])] # ID col is first vector
  REPs <- data[[2]][2:length(data[[2]])] # REP col is second vector
  lst <- list(IDs, REPs)
  names(lst) <- c("IDs", "REPs")
  lst
}

create_tibble <- function(data_col_name, data_vector) {
  # CREATE a tibble using vector
  dt <- tibble::tibble(data_vector) # CREATE a tibble using vector
  names(dt) <- data_col_name # IMMEDIATELY repair the name of first col - to ID!
  dt # return
}

#' Add data of col type to datatable
#'
#' Accepts an existing dataframe, colname and data to insert.  Adds data
#' as type `char` to avoid issues when adding data with default or error values
#' from combining with furhter datatables with specific data types.
#'
#' User should convert cols to appropriate datatypes as needed in analyses.
#'
#' This function adds a char column, renames the col to `data_col_name`, and
#' finally converts data to char datatype and inserts into datatable.
#'
#' DT row length MUST equal the length of data.  `data` and `data_col_name` must be
#' character vectors.
#'
#' Can optionally use indices to insert data into specific row indices in new col
#' in dt.
#'
add_data_col_type <- function(dt, data_col_name, data, indices="") {

  add_col <- TRUE
  if( any(names(dt) == data_col_name) == TRUE ) {
    add_col <- FALSE
  }

  newCol <- NA_character_ # newCol will be a character column

  if(add_col == TRUE) { # only add column if it doesnt already exist!
    dt <- tibble::add_column( dt, newCol )
    names(dt)[names(dt) == "newCol"] <- data_col_name # set name IMMEDIATELY
  }

  if( all(indices == "") ) {
    dt[[ data_col_name ]] <- data
  } else {
    dt[[ data_col_name ]][indices] <- data
  }

  dt # return modified datatable

}


combine_with_existing <- function(dt, datatables, table_name, table_start_line) {

  # check if datatables contains a datatable of same name
  if( length(datatables) > 0 ) { # only if the datatables is not blank
    dt_names <- names(datatables) # get names

    if( any(dt_names == table_name) ) { # if the table already exists
      # combine dt with this datatable
      dt_existing <- datatables[[table_name]]

      # some basic checks
      # NO IDS MATCH between new dt and existing dt
      if( any(dt$ID %in% dt_existing$ID) ) {
        stop( paste0("  Existing Datatable has ID: ", paste(dt$ID[(dt$ID %in% dt_existing$ID)], collapse=' '),
                     "\n datatable: ", table_name, " - rmd line: ", table_start_line) )
      }
      # ALL COLS do match - cols in dt all exist in dt_existing
      if( any( !(names(dt) %in% names(dt_existing)) ) ) {
        stop( paste0("  Existing Datatable does not have Column: ",
                     paste(names(dt)[( !(names(dt) %in% names(dt_existing)) )], collapse=" "),
                     "\n datatable: ", table_name, " - rmd line: ", table_start_line) )
      }

      # INTEGRATE dt into dt_existing
      dt <- dplyr::bind_rows(dt_existing, dt)
      # this automatically fills all non-existant cols in dt_existing that are not in dt with NAs
      # correct behaviour!

    } # dt_names == table_name
  } # datatables > 0
  dt # return
}



#' Get row indices from ID & REP values
#'
#' Return all indices of all matches of CONCAT of (`id_names` & `rep_names`) in
#' CONCAT of (`dt[["ID]]` & `dt[["rep"]]`), in the order they exist in
#' (`id_names` & `rep_names`).
#'
#' `sep` is used to create a unique separator between ID and rep, to avoid any
#' errors where ID ends with numbers, which could create overlappiny ID-rep pairs.
#' `sep` is ".-." by default, an unlikely string to find in an ID.
#'
get_indices_in_col_id_rep <- function(dt, id_names, rep_names, sep=".-.") {
  group_names <- paste0(id_names, sep, rep_names) # create concat of ID and REPs for group names
  id_rep <- paste0(dt[["ID"]], sep, dt[["rep"]]) # create concat of all ID and REPs in dt
  ind <- list() # to save to
  for(gn in 1:length(group_names) ) {
    ind[[gn]] <- grep(group_names[gn], id_rep)
  }
  # which & %in% does not preserve the ORDER of the matches
  #which( (paste0(dt[["ID"]], sep, dt[["rep"]]) %in% paste0(id_names, sep, rep_names)) )
  unlist(ind)
}

add_data_variables_all_rep <- function(dt, dataADD, table_name, table_start_line, sep=".-.") {

  # ensure all indices are valid - STOP if all IDs-REPs have been disposed, resamples, exported
  if( length(check_divisions_ids_reps(dt)[['REPs']]) == 0 ) {
    stop( paste0("  ALL IDs are exported/resampled/disposed: ",
                 "\n  table name: ", table_name,
                 " RMD line: ", table_start_line) )
  }

  # ensure all indices are valid - remove all indices where row has been disposed, resamples, exported
  ID_rep <- check_divisions_ids_reps(dt)
  indices <- get_indices_in_col_id_rep(dt, ID_rep$IDs, ID_rep$REPs) # get indices of valid IDs from dt

  errorString <- ""
  assignedErrorString <- ""
  IDsREPs <- paste0(ID_rep$IDs, sep, ID_rep$REPs) # create concat of ID and REPs for group names


  for(da in 1:length(dataADD)) { # insert data at indices

    if( any(names(dt) == dataADD[[da]][1]) ) { # if dt already has existing col name - ERROR CHECK

      assignedIDsREPs <- check_assigned_ids_reps(dt, dataADD[[da]][1])
      id_rep <- paste0(assignedIDsREPs$IDs, sep, assignedIDsREPs$REPs) # create concat of all ID and REPs in dt
      if( any(IDsREPs %in% id_rep) ) { # if any ID-REP has already had data added - STOP
        assignedErrorString <- c(assignedErrorString, "\n    col name: ", dataADD[[da]][1],
                         " - ID(s) - REP(s): ", paste(IDsREPs[IDsREPs %in% id_rep], collapse=' ') )

      } else { # check if IDs-REPs have a combination of resample/dispose/export AND assigned data

        #uaIDindices <- get_indices_in_col(dt, unassignedIDs, "ID")
        aIDindices <- get_indices_in_col_id_rep(dt, assignedIDsREPs$IDs, assignedIDsREPs$REPs) # get indices of valid IDs from dt
        idS <- indices[ !(aIDindices %in% indices) ]
        if( length(idS) == 0 ) { # log error if IDs to assign data to are exported/disposed/resamples
          errorString <- c(errorString, "\n    col name: ", dataADD[[da]][1])

        } else {
          dt <- add_data_col_type(dt, dataADD[[da]][1],
                                  rep(dataADD[[da]][2], length( idS )), idS )
        }
      }
    } else {

      dt <- add_data_col_type(dt, dataADD[[da]][1],
                              rep(dataADD[[da]][2], length( indices )), indices )
    }
  }

  # check errors or return dt
  if( length(assignedErrorString) > 1 ) {
    stop( paste0("  ALL IDs have values assigned to existing data col: ",
                 paste(assignedErrorString, collapse=' '),
                 "\n  table name: ", table_name,
                 " RMD line: ", table_start_line) )
  } else if( length(errorString) > 1 ) {
    stop( paste0("  ALL IDs have values assigned to existing data col or are exported/resampled/disposed: ",
                 paste(errorString, collapse=' '),
                 "\n  table name: ", table_name,
                 " RMD line: ", table_start_line) )
  } else {
    dt # return
  }
}


resample_id_rep <- function(datatables, id_names, rep_names, resample_names,
                            reps_names, table_name, table_start_line, sep=".-.") {

  dt <- datatables[[table_name]]
  dataADD <- list(c("resample", resample_names), c("reps", reps_names) )

  # expand rep_names with id_names & data - in case reps uses summary syntax (1:3,5,8:10 etc)
  ird <- expand_id_rep_data(id_names, rep_names, dataADD, dt)
  IDs <- ird$IDs
  REPs <- ird$REPs
  dataADD <- ird$DATA

  # create concat of IDs and REPs
  IDsREPs <- paste0(IDs, sep, REPs)

  id_rep_error_dup_div(IDsREPs, sep, dt, table_name, table_start_line)

  indices <- get_indices_in_col_id_rep(dt, IDs, REPs) # get indices of valid IDs from dt
  errorString <- ""

  for(da in 1:length(dataADD)) { # insert data at indices
    col_name <- dataADD[[da]][1]
    col_data <- dataADD[[da]][2:length(dataADD[[da]])]
    if( any(names(dt) == col_name) ) { # if dt already has existing col name
      errorString <- id_rep_error_ass(dt, col_name, IDsREPs, errorString, sep) # record any error
    }
    # still assign all values - to parse through all data cols and IDs
    dt <- add_data_col_type(dt, col_name, col_data, indices)
  }

  if( length(errorString) > 1 ) {
    stop( paste0("  IDs have values assigned to existing data col: ", paste(errorString, collapse=' '),
                 "\n  table name: ", table_name, " RMD line: ", table_start_line) )
  } else {

    datatables[[table_name]] <- dt # reassign dt
    datatables <- resample_create_datatables_id_rep(datatables, table_name, IDs, REPs, dataADD)
    datatables # and return

  }
}

#' Create new subsample datatables
#'
#' Using syntax : `table_name`-`<parent-rep-num>`_`<subsample-string>`
#'
#' Tables are also filled with subsample IDs and REPs - expanded by the rep number.
resample_create_datatables_id_rep <- function(datatables, table_name, IDs, REPs, dataADD) {

  reps_in_col <- resample_reps_in_col_rep(dataADD, REPs)

  for( id in 1:length(IDs) ) {# loop through each parent row
    rep <- REPs[id] # get the rep number
    ID <- IDs[id] # get current ID - for adding to sub-table

    rsString <- dataADD[[1]][(id+1)] # get resample string - +1 to remove col name
    rsStrs <- unlist(strsplit(rsString, split=' ')) # split at spaces

    repString <- dataADD[[2]][(id+1)] # get rep string for sub-table - +1 to remove col name
    repStrs <- unlist(strsplit(repString, split=' ')) # split at spaces - NB will match rsStrs in length!

    for( ri in 1:length(rsStrs) ) { # loop through each resample string + sub-table rep
      resample <- rsStrs[ri]
      # expand ID and repStrs[ri] by integer in repStrs[ri]
      # form sequence from 1..repStrs[ri]
      REPseq <- as.character(seq(as.integer(repStrs[ri])))
      # expand ID by length of REPseq
      IDex <- rep(ID, length(REPseq))
      # create new table name
      subtable_name <- paste0(table_name, "-", rep, "_", resample)
      rep_rs <- paste0(rep, "_", resample)
      if( subtable_name %in% names(datatables) ) { # if exists just add IDex + REPseq as rows

        if( reps_in_col[[rep_rs]] == TRUE ) { # add rep col if reps exists across IDs for given rep + resample string

          datatables[[ subtable_name ]] <- subsample_add_id_rep_data(datatables, subtable_name, IDex, REPseq)

        } else { # only add ID

          datatables[[ subtable_name ]] <- subsample_add_id_data_rep(datatables, subtable_name, IDex, REPseq)

        }
      } else { # create the tibble and then add rows of IDex (+ REPseq)

        datatables[[ subtable_name ]] <- create_tibble("ID", IDex)
        if( reps_in_col[[rep_rs]] == TRUE ) { # add rep col if reps exists across IDs for given rep + resample string
          datatables[[ subtable_name ]] <- add_data_col_type(datatables[[ subtable_name ]],
                                                             "rep", REPseq)
        }
      }
    } # end ri
  } # end id
  datatables # return list with new tables added
}


subsample_add_id_data_rep <- function(datatables, subtable_name, IDex, REPseq) {
  if( any( names(datatables[[ subtable_name]]) == "rep" ) ) { # rep col exists - add ID and rep rows
    datatables[[ subtable_name ]] <- tibble::add_row(datatables[[ subtable_name ]],
                                                     ID = IDex, rep = REPseq)
  } else {
    datatables[[ subtable_name ]] <- tibble::add_row(datatables[[ subtable_name ]],
                                                     ID = IDex)
  }
  datatables[[ subtable_name]] # return
}

subsample_add_id_rep_data <- function(datatables, subtable_name, IDex, REPseq) {

  if( any(names(datatables[[ subtable_name ]]) == "rep") ) { # rep col exists - add ID and rep rows
    datatables[[ subtable_name ]] <- tibble::add_row(datatables[[ subtable_name ]],
                                                     ID = IDex, rep = REPseq)
  } else { # EXISTING datatable does not have rep col!  So need to add one, and add 1 to each EXISTING ID
    if( length(names(datatables[[ subtable_name ]])) > 1 ) {
      # split the dt into ID and other cols (if existing)
      dtID <- dplyr::select(datatables[[ subtable_name ]], ID)
      col_names <- names(datatables[[ subtable_name ]])
      colREM <- col_names[2:length(col_names)]
      dtREM <- dplyr::select(datatables[[ subtable_name ]], dplyr::all_of(colREM))
      # add rep to dtID - fill with "1"s for each ID
      dtID <- add_data_col_type(dtID, "rep", rep("1", length(dtID$ID)))
      datatables[[ subtable_name ]] <- merge(dtID, dtREM)
    } else {
      # just add new rep col alongside ID, and fill rep with "1"s
      datatables[[ subtable_name ]] <- add_data_col_type(datatables[[ subtable_name ]],
                                                         "rep", rep("1", length(datatables[[ subtable_name ]]$ID)))
    }
    # finally add the new ID and REP rows
    datatables[[ subtable_name ]] <- tibble::add_row(datatables[[ subtable_name ]],
                                                     ID = IDex, rep = REPseq)
  }

  datatables[[ subtable_name ]] # return

}

#' Determine whether single or multiple reps across IDs for each Reample String.
#'
#' Take into account the IDs & REPs of existing samples!
#'
#' Returns result as a named boolean list - each boolean indices whether the
#' Resample String has more than one rep in AT LEAST one ID.
resample_reps_in_col_rep <- function(dataADD, REPs) {

  # create boolean vector to determine which resample string have more than one rep per ID-REP
  repStrings <- unique(REPs) # get all possible rep values
  rsStringv <- dataADD[[1]][2:length(dataADD[[1]])] # extract resample and rep string vectors
  repStringv <- dataADD[[2]][2:length(dataADD[[2]])]
  reps_in_col <- list() # named boolean list to store results
  for(rps in 1:length(repStrings) ) {
    repString <- repStrings[rps] # current repString - use to store result of this rep in named list
    lp <- REPs==repString # get boolean mask of vector
    # mask the resample and rep vectors
    rsStringlp <- rsStringv[lp]
    repStringlp <- repStringv[lp]
    # split masked resample and rep vectors into lists
    rsStringl <- strsplit(rsStringlp, split=' ')
    repStringl <- strsplit(repStringlp, split=' ')
    repStringl <- lapply(repStringl, as.integer)
    # define unique resample strings that remain - to loop over
    resampleStrings <- unique(unlist(rsStringl))
    # define the repStrings STEP for indexing
    #rpsi <- ((rps-1)*(length(repStrings)-1))
    rpsi <- length(reps_in_col) # let list count its STEP!
    for(rss in 1:length(resampleStrings) ) { # loop through each unique resample string
      resampleString <- resampleStrings[rss]
      ll <- lapply(rsStringl, '==', resampleString) # define boolean mask for resample string
      reps <- unlist(Map(`[`, repStringl, ll)) # apply to reps string
      if( all(reps == 1)) { # if all reps are 1 for all IDs in a given REP return FALSE (no reps in col)
        reps_in_col[[(rpsi+rss)]] <- FALSE
      } else { # otherwise return TRUE (REPs exist in col)
        reps_in_col[[(rpsi+rss)]] <- TRUE
      } # and name the list entry using the repString plus resampleString
      names(reps_in_col)[(rpsi+rss)] <- paste0(repString, "_", resampleString)
    }
  }
  reps_in_col # return
}

add_data_variables_id_rep <- function(dt, group_names, rep_names, dataADD, table_name,
                                      table_start_line, sep=".-.") {


  # expand rep_names with group_names & data - in case reps uses summary syntax (1:3,5,8:10 etc)
  ird <- expand_id_rep_data(group_names, rep_names, dataADD, dt)
  group_names <- ird$IDs
  rep_names <- ird$REPs
  dataADD <- ird$DATA

  # create concat of ID and REPs for group names
  IDsREPs <- paste0(group_names, sep, rep_names)

  id_rep_error_dup_div(IDsREPs, sep, dt, table_name, table_start_line)

  indices <- get_indices_in_col_id_rep(dt, group_names, rep_names) # get indices of valid IDs from dt
  errorString <- ""

  for(da in 1:length(dataADD)) { # insert data at indices
    col_name <- dataADD[[da]][1]
    col_data <- dataADD[[da]][2:length(dataADD[[da]])]
    if( any(names(dt) == col_name) ) { # if dt already has existing col name
      errorString <- id_rep_error_ass(dt, col_name, IDsREPs, errorString, sep) # record any error
    }
    # still assign all values - to parse through all data cols and IDs
    dt <- add_data_col_type(dt, col_name, col_data, indices)
  }

  if( length(errorString) > 1 ) {
    stop( paste0("  IDs have values assigned to existing data col: ", paste(errorString, collapse=' '),
                 "\n  table name: ", table_name, " RMD line: ", table_start_line) )
  } else {
    dt # return
  }
}

#' Check Errors in IDs + REPs
#'
#' 1. Ensure no duplicated ID+REP identifier.
#'
#' 2. Ensure no ID+REP identifier has been previously resampled, disposed, exported.
#'
id_rep_error_dup_div <- function(IDsREPs, sep, dt, table_name, table_start_line) {

  if( any(duplicated(IDsREPs)) ) { # if any ID-REP combinations match there is a syntax error
    stop( paste0(" ID-REP pair duplicated in datatable: ",
                 IDsREPs[duplicated(IDsREPs)], "\n  table name: ", table_name,
                 " RMD line: ", table_start_line) )
  }
  ID_rep <- check_divisions_ids_reps(dt) # get all VALID IDs & REPs
  id_rep <- paste0(ID_rep$IDs, sep, ID_rep$REPs) # create concat of all ID and REPs in dt
  if( any(!(IDsREPs %in% id_rep)) ) { # any group_names are not in validIDs STOP
    stop( paste0(" ID-REP value(s) have been exported/resampled/disposed, or do not exist: \n    ",
                 paste(IDsREPs[!(IDsREPs %in% id_rep)], collapse=' '),
                 "\n    table name: ", table_name, " RMD line: ", table_start_line, "\n") )
  }
}

#' Check Errors in IDs + REPs
#'
#' 1. Ensure assigned data in dt:col_name does not clash with IDREPs
#'
id_rep_error_ass <- function(dt, col_name, IDsREPs, errorString, sep='.-.') {
  # identify which ID+REP rows have data assigned
  assignedIDsREPs <- check_assigned_ids_reps(dt, col_name)
  id_rep <- paste0(assignedIDsREPs$IDs, sep, assignedIDsREPs$REPs) # create ID+REP identifiers
  if( any(IDsREPs %in% id_rep) ) { # if any ID-REP has alreayd had data added - STOP
    errorString <- c(errorString, "\n    col name: ", col_name,
                     " - ID(s)-REP(s): ", paste(IDsREPs[IDsREPs %in% id_rep], collapse=' ') )
  }
  errorString # return
}

#' Return all indices of all matches of all `group_names` in
#' `dt[[group_col_names]]` in the order they exist in `group_names`.
#'
get_indices_in_col <-function(dt, group_names, group_col_name) {
  ind <- list()
  for(gn in 1:length(group_names) ) {
    ind[[gn]] <- grep(group_names[gn], dt[[group_col_name]])
  }
  unlist(ind)
  # which & %in% does not preserve the ORDER of the matches
  #which(dt[[group_col_name]] %in% group_names) # can return indices of ALL matches
  #match(group_names, dt[[group_col_name]]) # can just use match without for loop!
  #indices <- c()
  #for( j in 1:length(ID) ) {
  #  index <- match( TRUE, (dt$ID == ID[j]) )
  #  indices <- c(indices, index)
  #}
  #indices
}

#' Return all indices of all matches of all `group_names` in
#' `dt[[group_col_names]]` in the order they exist in `group_names`.
#'
#' Return as a list - with each element containing all indexes that match the
#' corresponding element in `group_names`
get_indexes_in_col <-function(dt, group_names, group_col_name) {
  ind <- list()
  for(gn in 1:length(group_names) ) {
    ind[[gn]] <- grep(group_names[gn], dt[[group_col_name]])
  }
  ind
}

#' Expand IDs & REPs Strings
#'
#' Expanding REPs in summarised form to a sequence of integers:
#'
#' Example: "1:3,5,7:9" summary string of REPS should yield the following
#' integer sequence:
#' `1 2 3 5 7 8 9`
#'
#' Expanding `IDs` and `DATA` alongside these, and returning as a named list.
#'
expand_id_rep_data <- function(IDs, REPs, DATA, dt) {

  # ensure REPs are characters
  REPs <- as.character(REPs)
  IDs_v <- c() # use to store ids vector
  REPs_v <- c() # use to store reps vector
  DATA_l <- lapply(DATA, '[', 1) # extract headers only to list

  for(i in 1:length(IDs) ) {
    ID <- IDs[i]
    REP <- REPs[i]

    rep_s <- strsplit(REP, ",") # split at commas

    for(r in 1:length(rep_s)) {
      rep_strs <- rep_s[[r]]
      if(rep_strs[1]=="ALL") {# expand reps to all existing and valid reps for ID
        #rep_int <- "ALL"
        ID_rep <- check_divisions_ids_reps(dt) # get all VALID IDs & REPs
        rep_int <- as.integer( ID_rep$REPs[ID_rep$IDs==ID])
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
      REPs_v <- c(REPs_v, rep_int)
      IDs_v <- c(IDs_v, rep(ID, length(rep_int)))
      for(d in 1:length(DATA)) {
        DATA_l[[d]] <- c(DATA_l[[d]], rep(DATA[[d]][(i+1)], length(rep_int))) # i+1 as first element is data title
      }
    }
  }

  lst <- list(IDs_v, REPs_v, DATA_l)
  names(lst) <- c("IDs", "REPs", "DATA")
  lst

}


add_data_variables_all <- function(dt, dataADD, table_name, table_start_line) {

  # ensure all indices are valid - remove all indices where row has been disposed, resamples, exported
  if( length(check_divisions_ids(dt)) == 0) {
    stop( paste0("  ALL IDs are exported/resampled/disposed: ",
                 "\n  table name: ", table_name,
                 " RMD line: ", table_start_line) )
  }

  indices <- get_indices_in_col(dt, check_divisions_ids(dt), "ID") # get indices of valid IDs from dt
  errorString <- ""
  assignedErrorString <- ""

  for(da in 1:length(dataADD)) { # insert data at indices

    assignedIDs <- check_assigned_ids(dt, dataADD[[da]][1])
    unassignedIDs <- dt$ID[!(dt$ID %in% assignedIDs)]
    if( length(unassignedIDs) == 0 ) { # log error if no IDs to assign data to
      assignedErrorString <- c(assignedErrorString, "\n    col name: ", dataADD[[da]][1])
    } else {
      uaIDindices <- get_indices_in_col(dt, unassignedIDs, "ID")
      idS <- uaIDindices[uaIDindices %in% indices]
      if( length(idS) == 0 ) { # log error if IDs to assign data to are exported/disposed/resamples
        errorString <- c(errorString, "\n    col name: ", dataADD[[da]][1])
      } else {
        dt <- add_data_col_type(dt, dataADD[[da]][1],
                                rep(dataADD[[da]][2], length( idS )), idS )
      }
    }
  }

  # check errors or return dt
  if( length(assignedErrorString) > 1 ) {
    stop( paste0("  ALL IDs have values assigned to existing data col: ",
                 paste(assignedErrorString, collapse=' '),
                 "\n  table name: ", table_name,
                 " RMD line: ", table_start_line) )
  } else if( length(errorString) > 1 ) {
    stop( paste0("  ALL IDs have values assigned to existing data col or are exported/resampled/disposed: ",
                 paste(errorString, collapse=' '),
                 "\n  table name: ", table_name,
                 " RMD line: ", table_start_line) )
  } else {
    dt # return
  }
}

add_data_variables_id <- function(dt, IDs, dataADD, table_name, table_start_line) {

  id_error_dup_div(IDs, dt, table_name, table_start_line)

  indices <- get_indices_in_col(dt, IDs, "ID")
  errorString <- ""

  for(da in 1:length(dataADD)) { # insert data at indices
    col_name <- dataADD[[da]][1]
    col_data <- dataADD[[da]][2:length(dataADD[[da]])]
    if( any(names(dt) == col_name ) ) { # if dt already has existing col name
      errorString <- id_error_ass(dt, col_name, IDs, errorString)
    }
    # still assign all values - to parse through all data cols and IDs
    dt <- add_data_col_type(dt, col_name, col_data, indices)
  }

  if( length(errorString) > 1 ) {
    stop( paste0("  IDs have values assigned to existing data col: ", paste(errorString, collapse=' '),
                 "\n  table name: ", table_name, " RMD line: ", table_start_line) )
  } else {
    dt # return
  }
}

resample_id <- function(datatables, id_names, resample_names,
                            reps_names, table_name, table_start_line) {

  dt <- datatables[[table_name]]
  IDs <- id_names
  dataADD <- list(c("resample", resample_names), c("reps", reps_names) )

  id_error_dup_div(IDs, dt, table_name, table_start_line)

  indices <- get_indices_in_col(dt, IDs, "ID")
  errorString <- ""

  for(da in 1:length(dataADD)) { # insert data at indices
    col_name <- dataADD[[da]][1]
    col_data <- dataADD[[da]][2:length(dataADD[[da]])]
    if( any(names(dt) == col_name ) ) { # if dt already has existing col name
      errorString <- id_error_ass(dt, col_name, IDs, errorString)
    }
    # still assign all values - to parse through all data cols and IDs
    dt <- add_data_col_type(dt, col_name, col_data, indices)
  }


  if( length(errorString) > 1 ) {
    stop( paste0("  IDs have values assigned to existing data col: ", paste(errorString, collapse=' '),
                 "\n  table name: ", table_name, " RMD line: ", table_start_line) )
  } else {

    datatables[[table_name]] <- dt # reassign dt
    datatables <- resample_create_datatables_id(datatables, table_name, IDs, dataADD)
    datatables # and return

  }
}

#' Create new subsample datatables
#'
#' Using syntax : `table_name`-`<parent-rep-num>`_`<subsample-string>`
#'
#' Tables are also filled with subsample IDs and REPs - expanded by the rep number.
resample_create_datatables_id <- function(datatables, table_name, IDs, dataADD) {

  reps_in_col <- resample_reps_in_col(dataADD)

  for( id in 1:length(IDs) ) {# loop through each parent row

    ID <- IDs[id] # get current ID - for adding to sub-table

    rsString <- dataADD[[1]][(id+1)] # get resample string - +1 to remove col name
    rsStrs <- unlist(strsplit(rsString, split=' ')) # split at spaces

    repString <- dataADD[[2]][(id+1)] # get rep string for sub-table - +1 to remove col name
    repStrs <- unlist(strsplit(repString, split=' ')) # split at spaces - NB will match rsStrs in length!

    for( ri in 1:length(rsStrs) ) { # loop through each resample string + sub-table rep
      resample <- rsStrs[ri]
      # expand ID and repStrs[ri] by integer in repStrs[ri]
      # form sequence from 1..repStrs[ri]
      REPseq <- as.character(seq(as.integer(repStrs[ri])))
      # expand ID by length of REPseq
      IDex <- rep(ID, length(REPseq))
      # create new table name
      subtable_name <- paste0(table_name, "_", resample)
      if( subtable_name %in% names(datatables) ) { # if exists just add IDex + REPseq as rows

        if( reps_in_col[[resample]] == TRUE ) {
          datatables[[ subtable_name ]] <- subsample_add_id_rep_data(datatables, subtable_name, IDex, REPseq)
        } else {
          datatables[[ subtable_name ]] <- tibble::add_row(datatables[[ subtable_name ]],
                                                           ID = IDex)
        }

      } else { # create the tibble and then add rows of IDex + REPseq

        datatables[[ subtable_name ]] <- create_tibble("ID", IDex)
        if( reps_in_col[[resample]] == TRUE ) {
          datatables[[ subtable_name ]] <- add_data_col_type(datatables[[ subtable_name ]],
                                                             "rep", REPseq)
        }
      }
    }
  }

  datatables # return list with new tables added
}

#' Determine whether single or multiple reps across IDs for each Reample String.
#'
#' Returns result as a named boolean list - each boolean indices whether the
#' Resample String has more than one rep in AT LEAST one ID.
resample_reps_in_col <- function(dataADD) {
  # first create boolean vector to determine which resample string have more than one rep
  rsStringv <- dataADD[[1]][2:length(dataADD[[1]])]
  repStringv <- dataADD[[2]][2:length(dataADD[[2]])]
  rsStringl <- strsplit(rsStringv, split=' ')
  repStringl <- strsplit(repStringv, split=' ')
  repStringl <- lapply(repStringl, as.integer)
  resampleStrings <- unique(unlist(rsStringl))
  reps_in_col <- list() # named boolean list
  for(rss in 1:length(resampleStrings) ) { # loop through each unique resample string
    resampleString <- resampleStrings[rss]
    ll <- lapply(rsStringl, '==', resampleString)
    reps <- unlist(Map(`[`, repStringl, ll))
    if( all(reps == 1)) {
      reps_in_col[[rss]] <- FALSE
    } else {
      reps_in_col[[rss]] <- TRUE
    }
    names(reps_in_col)[rss] <- resampleString
  }
  reps_in_col# return
}

id_error_dup_div <- function(IDs, dt, table_name, table_start_line) {

  if( any(duplicated(IDs)) ) { # if any ID-REP combinations match there is a syntax error
    stop( paste0(" ID pair duplicated in datatable: ",
                 IDs[duplicated(IDs)], "\n  table name: ", table_name,
                 " RMD line: ", table_start_line) )
  }

  validIDs <- check_divisions_ids(dt) # get all VALID IDs : not exported, resampled, disposed
  if( any(!(IDs %in% validIDs)) ) { # any IDs are not in validIDs STOP
    stop( paste0(" ID value has been exported, resampled, or disposed: ",
                 paste(IDs[!(IDs %in% validIDs)], collapse=' '), "\n  table name: ", table_name,
                 " RMD line: ", table_start_line) )
  }
}


id_error_ass <- function(dt, col_name, IDs, errorString) {
  assignedIDs <- check_assigned_ids(dt, col_name)
  if( any(IDs %in% assignedIDs) ) {
    errorString <- c(errorString, "\n    col name: ", col_name,
                     " - ID(s): ", IDs[IDs %in% assignedIDs])
  }
}

add_data_variables_group <- function(dt, group_names, group_col_name, dataADD, table_name, table_start_line) {

  indexes <- get_indexes_in_col(dt, group_names, group_col_name)
  errorString <- ""

  # ERROR CHECK
  # ensure all indices are valid - remove all indices where row has been disposed, resamples, exported
  validIndices <- get_indices_in_col(dt, check_divisions_ids(dt), "ID")
  for(x in 1:length(indexes) ) {
    if( any(!(indexes[[x]] %in% validIndices)) ) { # any IDs are not in validIDs : STOP
      stop( paste0(" ID in group has been exported, resampled, or disposed: ",
                   paste(dt$ID[indexes[[x]][!(indexes[[x]] %in% validIndices)]], collapse=' '),
                   "\n  table name: ", table_name,
                   " RMD line: ", table_start_line) )
    }
    indexes[[x]] <- indexes[[x]][indexes[[x]] %in% validIndices]
  }

  for(da in 1:length(dataADD)) { # insert data at indices

    for(ix in 1:length(indexes) ) { # still assign all values - to parse through all data cols and IDs

      if( any(names(dt) == dataADD[[da]][1] ) ) { # if dt already has existing col name
        assignedIDs <- check_assigned_ids(dt, dataADD[[da]][1])
        if( any(dt$ID[indexes[[ix]]] %in% assignedIDs) ) {
          errorString <- c(errorString, "\n    col name: ", dataADD[[da]][1],
                           " - ID(s): ", dt$ID[indexes[[ix]]][dt$ID[indexes[[ix]]] %in% assignedIDs])
        }
      }
      dt <- add_data_col_type(dt, dataADD[[da]][1],
                              rep(dataADD[[da]][(ix+1)], length( indexes[[ix]] )), indexes[[ix]]) # dataADD: ix+1 as first index is TITLE
    }
  }

  if( length(errorString) > 1 ) {
    stop( paste0("  IDs have values assigned to existing data col: ",
                 paste(errorString, collapse=' '),
                 "\n  table name: ", table_name,
                 " RMD line: ", table_start_line) )
  } else {
    dt # return
  }

}

#' return assigned IDs : that already have data in col
#'
check_assigned_ids <- function(dt, datacol) {
  dt[["ID"]][(!is.na(dt[[datacol]]))]
}

#' return assigned IDs : that already have data in col
#'
check_assigned_ids_reps <- function(dt, datacol) {
  IDs <- dt[["ID"]][(!is.na(dt[[datacol]]))]
  REPs <- dt[["rep"]][(!is.na(dt[[datacol]]))]
  # return as named list:
  lst <- list(IDs, REPs)
  names(lst) <- c("IDs", "REPs")
  lst
}

get_group_cols <- function(dt) {
  dplyr::select(dt, dplyr::starts_with("group"))
}

find_group_col <- function(group_names, gdt, table_name, table_start_line) {

  if(length(gdt) == 0 ) {
    stop( paste0("  Non-existant GROUP or IDs datatable: ", paste(group_names, collapse=' '),
                 "\n  table name: ", table_name, " RMD line: ", table_start_line) )
  }
  gnindex <- 0
  for(gi in 1:length(gdt)) { # identify group col
    if( all(group_names %in% gdt[[gi]]) ) { gnindex <- gi }
  }
  if(gnindex == 0) {
    stop( paste0("  Non-existant GROUP value in group_names: ", group_names[gnindex],
                 "\n  table name: ", table_name, " RMD line: ", table_start_line) )
  }
  names(gdt)[gnindex] # return group col names
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
#' @param dt_vector Character vector containing a projectmanagr datatable.
#'
#' @export
datatable_extract <- function(dt_vector) {

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
    } else if( startsWith(trimws(dt_vector[j]), '>')) {
      # line is comment - IGNORE
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

  # correct data in import or export columns - remove whitespace
  headers <- lapply(data, '[', 1)
  if( any( headers == "import") ) {
    import_col <- unlist(data[headers == "import"])
    import_col <- gsub(' ', '', import_col, fixed=TRUE)
    data[headers == "import"] <- list(import_col)
  }
  if( any( headers == "export") ) {
    export_col <- unlist(data[headers == "export"])
    export_col <- gsub(' ', '', export_col, fixed=TRUE)
    data[headers == "export"] <- list(export_col)
  }

  data # return

}


#' Split Multi-Observtional Data
#'
#' `datatable_extract` returns a list of data, where multi-observational data
#' is returned in the same list index (column) and vector index (ID), but the
#' data has been concatenated into one string - space-separated.
#'
#' This function splits multi-observational data.  Now the FIRST LIST index will
#' indicate the column, and the SECOND LIST index will indicate the ID.  The
#' vector index will indicate the multi-observational data for a given column & ID
#'
#'
split_multi_obs_data <- function(data) {
  lapply(data, strsplit, split=' ')
}


get_col_indices <- function(di, delim_indices, dt_length = 100) {

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

