
#' Read Datatables and check validity from character vector
#'
#' @export
datatable_read_vector <- function(rmd_contents) {

  # identify the lines that begin with "+==="
  indices <- which( startsWith(rmd_contents, "+===") )

  if( length(indices) < 2 ) {
    # there are no datatables
    # return a blank list
    return( list() )
  }

  # from indices extract the actual tables - they sit between the first and second dividers
  if( (length(indices) %% 2) == 1 ) {
    #stop( paste0("  datatable divider numbers are odd - syntax error in the Rmd: ", length(indices)) )
    # dont fail, just do not read the odd index
    indices <- indices[1:(length(indices)-1)]
  }

  if( length(indices) == 0 ) {
    # skip parsing rmd_contents if no datatables!
    cat( paste0("  no datatables in note" ) )

  } else {

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
      #for(i in 1:5 ) {

      # parse each table IN ORDER
      # tables[[i]][4] contains the table NAME then FUNCTION
      # eg. mice  :  CREATE , mice  :  ADD_DATA
      # extract each to new strings:
      table_name <- trimws(strsplit( as.character(tables[[i]][4]), ":")[[1]][1])
      table_function <- trimws(strsplit( as.character(tables[[i]][4]), ":")[[1]][2])

      if( table_name != "TEMPLATE" ) { # only parse if datatable is not template

        if( table_function == "CREATE" ) {

          # CREATE : just parse the tables character vector to create a tibble
          datatables[[ table_name ]] <- parse_datatable_create(tables[[i]], datatables, table_name)


        } else if( (table_function == "ADD_DATA") || (table_function == "GROUP") ) {
          # using parse_datatable_add_data() for both ADD_DATA and GROUP datatables

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
            datatables[[ t_n ]] <- parse_datatable_add_data( tables[[i]], datatables[[ t_n ]], t_n )

          }


          #} else if( table_function == "GROUP" ) {

          # FIRST check that table_name ALREADY EXISTS in datatables
          #  if( any(names(datatables) == table_name) == FALSE ) {
          # no datatable of name table_name to add data to - STOP
          #    stop( paste0("  No datatable exists to add data to: ", table_name, " table index: ", i) )
          #  }

          # GROUP : parse tables character vector and CHECK AGAINST EXISTING datatables to ADD GROUP DATA TO TABLE
          #  datatables[[ table_name ]] <- parse_datatable_group(tables[[i]], datatables[[ table_name ]])

          # DEPRECATED - using parse_datatable_add_data to parse GROUP datatables now tro handle adding GROUPS across different IDs spread
          # across separate datatables..


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

          } # for f

        } # end if EXPORT

      } # end if TEMPLATE

    } # for i

    # return
    datatables

  }

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

  orgPath <- find_org_directory(orgPath)

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


#' CREATE a tibble from a datatable Rmd vector
#'
#' This datatable has the FUNCTION : CREATE
#'
#' It MUST have IDs in the first column.  It can have subsequent columns with
#' further data.
#'
#'
parse_datatable_create <- function(dt_vector, dt_list, table_name) {

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
  data <- datatable_extract(dt_vector)

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

  # check if dt_list contains a datatable of same name
  if( length(dt_list) > 0 ) { # if the dt_list is not blank
    dt_names <- names(dt_list) # get names

    if( any(dt_names == table_name) ) { # if the table already exists
      # combine dt with this datatable
      dt_existing <- dt_list[[table_name]]

      # some basic checks
      # NO IDS MATCH between new dt and existing dt
      for(id in dt$ID) {
        if( any(id == dt_existing$ID) ) {
          stop( paste0("  Existing Datatable has ID: ", id, " datatable: ", table_name) )
        }
      } # all IDs checked
      # ALL COLS do match - cols in dt all exist in dt_existing
      for(col in names(dt) ) {
        if( any(col == names(dt_existing) ) == FALSE ) {
          stop( paste0("  Existing Datatable does not have Column: ", col) )
        }
      }

      # now INTEGRATE dt into dt_existing
      dt <- dplyr::bind_rows(dt_existing, dt)
      # this automatically fills all non-existant cols in dt_existing that are not in dt with NAs
      # correct behaviour!

    } # dt_names == table_name
  } # dt_list > 0

  # return dataframe
  dt

}



#' Add data of col type to datatable
#'
#' Accepts an existing dataframe, colname and data to insert.  Adds data
#' as type `char` to avoid issues when adding data with default or error values
#' from combining with furhter datatables with specific data types.
#'
#' User should convert cols to appropriate datatypes as needed in analyses.
#'
#' This function adds a char column, renames the col to `name`, and
#' finally converts data to char datatype and inserts into datatable.
#'
#' DT row length MUST equal the length of data.  `data` and `name` must be
#' character vectors.
#'
#' Can optionally use indices to insert data into specific row indices in new col
#' in dt.
#'
add_data_col_type <- function(dt, name, data, indices="") {

  add_col <- TRUE
  if( any(names(dt) == name) == TRUE ) {
    add_col <- FALSE
  }

  # newCol will be a character column
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

  dt # return the modified datatable

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
parse_datatable_add_data <- function(dt_vector, dt, table_name) {

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
  data <- datatable_extract(dt_vector)

  # ADD DATA to tibble from the data LIST
  # this depends on what FORMAT the datatable was in in the Rmd
  # samples first - first col is ID
  # variables first - first col is variables
  # timetable - first col is timetable


  if( headers[1] == "ID" ) {
    # this is a SAMPLE-FIRST data table!
    # as this is the format samples data will be stored in, just need to add the data cols

    # to deal with adding to ALL or GROUPS should collect some data from dt & data variables

    # get all group cols in one dt
    gdt <- dplyr::select(dt, dplyr::starts_with("group-"))


    if( headers[2] == "rep" ) { # if second col is rep, the first col MUST be IDs! No ALL or group-IDs
      # process all reps from each ID TOGETHER

      newCol <- NA_character_ # for adding new cols - all will be character vectors

      # extract data_cols
      data_cols <- c()
      for( i in 3:length(data ) ) { # there is a rep col - SKIP this entry in data
        # so start at index 3:length(data)
        data_cols[(i-2)] <- data[[i]][1]
      }

      # extract group_names - first entry in each list entry 3:length
      # NOT USED!  if adding to a group there will be no rep col!
      #group_names <- c() # these are the IDs?
      #for(i in 3:length(data) ) { # there is a rep col - SKIP this entry in data
      # so start at index 3:length(data)
      #group_names[(2-1)] <- data[[1]][i]
      #}
      #group_names <- unique( data[[1]][2:length(data[[1]])] )

      # loop through all cols
      for(i in 3:length(data) ) { # there is a rep col - SKIP entries 1 & 2 in data
        # start at index 3:length(data)

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
        summarise_reps <- FALSE

        ID_rep <- check_divisions_ids_reps(dt)
        # using check_divisions - to OMIT any IDs/REPs that have been divided
        # removes resampled, disposed, exported..
        ID <- ID_rep$IDs # this vector contains each ID as is in dt but MINUS all resampled, dispose, exported
        rep <- ID_rep$REPs # this vector contains each rep for each ID in ID as is in dt but MINUS all resampled, dispose, exported
        # using both ID and rep can uniquely identify positions to insert data in dt for additions
        # this will ensure no data is added to cols where the sample doesnt exist

        indices <- c() # the indices where data should be added to dt - indexes according to ID and rep values

        # create expanded_data vector - will hold data for IDs/reps
        if( any(names(dt) == data[[i]][1] ) ) {
          # if the col already exists extract the col at dt and put this into expanded_data
          expanded_data <- dt[[ data[[i]][1] ]]
        } else {
          # else just fill the expanded_data with default value NA_char
          expanded_data <- c(rep(newCol, length(ID)))
        }

        # extract each ID/REP pair into a list of vectors :
        # each list entry is a vector of the ID, followed by each rep entry
        ids_reps <- extract_ids_reps(dt, data)

        # Deal with special case where ids/reps are both ALL : expand BOTH ID and rep if ID is set to "ALL"
        if( length(ids_reps) == 1 && ids_reps[[1]][1] == "ALL" ) {

          # check the reps is also all
          if( ids_reps[[1]][2] != "ALL" ) {
            #this is a syntax error
            stop( paste0("  Datatable ID col is ALL but rep is not : ", table_name) )
          }

          summarise_reps <- TRUE # ALL is one form of summarising reps
          # will be adding multiple data as expanded_data to dt...

          for(k in 1:length(ID) ) {
            # need to loop through each ID now! index [k]
            index <- match( TRUE, (dt[["ID"]] == ID[k] & dt[["rep"]] == as.character(rep[k]) ) )
            indices <- c( indices, index )
          }

          # AND fill new vector - expanded_data - with REPLICATED DATA
          expanded_data <- c(rep(data[[i]][2], length(ID) ) )


        } else { # deal with each ID separately

          # loop through each ID and deal with each reps declaration
          for( j in 1:length(ids_reps) ) {

            # get all the reps entries for this id
            id <- ids_reps[[j]][1] # id val
            reps <- ids_reps[[j]][2:length(ids_reps[[j]])] # all reps strings/vectors

            if( any(reps == "ALL") ) { # if ANY reps are all - FIRST  set expanded_data for all indices of id
              # still want to cater for reps set to ALL, but STILL support additional data added to other cols

              # get all indices in ID where it is id
              indices <- grep(TRUE, ID == id)

              # get the index where ID is id and REP is All in data AND copy this into expanded_data at correct indices
              all_index <- grep(TRUE, ( data[[1]] == id & data[[2]] == "ALL" ) )
              expanded_data[indices] <- parse_data_type(data[[i]][all_index], expanded_data, indices)

            }

            # for every reps val, if not ALL, fill expanded_data
            for( r in reps ) {
              if( r != "ALL" ) {

                # expand r into int vector
                rep_vector <- eval(parse(text = paste0("c(",r,")") )  )

                # compute index of data to add from data
                all_index <- grep(TRUE, ( data[[1]] == id & data[[2]] == r ) )

                for( rv in rep_vector ) {
                  # need to loop through each rep_vector val
                  index <- grep( TRUE, ( ID == id & rep == as.character(rv) ) )
                  # if id and rep and not both present, the index is not added - excluding additions to non-existent samples

                  # add to expanded_data at index
                  expanded_data[index] <- parse_data_type(data[[i]][all_index], expanded_data, index)
                } # for rv rep_vector

              } # if not ALL
            } # for r in reps


          } # for j in ids_reps (ID)

        } # else ids_reps is not ALL

        # set dt with expanded data - dt, headername, data
        dt <- add_data_col_type(dt, data[[i]][1], expanded_data)


        #for( j in 2:length(data[[i]]) ) { # loop through all entries in the data col
        # skip index 1 as this is the col HEADER

        # ID <- data[[1]][j]
        #rep <- data[[2]][j]

        # expand BOTH ID and rep if ID is set to "ALL"
        #if( length(ID) == 1 && ID == "ALL" ) {

        # summarise_reps <- TRUE # ALL is one form of summarising reps
        # will be adding multiple data as expanded_data to dt...

        #ID_rep <- check_divisions_ids_reps(dt)
        # using check_divisions - to OMIT any IDs/REPs that have been divided
        # resampled, disposed, exported..
        #ID <- ID_rep$IDs
        #rep <- ID_rep$REPs

        #for(k in 1:length(ID) ) {
        # need to loop through each ID now! index [k]
        # index <- match( TRUE, (dt[["ID"]] == ID[k] & dt[["rep"]] == as.character(rep[k]) ) )
        #indices <- c( indices, index )
        #}
        # AND fill new vector - expanded_data - with REPLICATED DATA
        #expanded_data <- c(expanded_data, rep(data[[i]][j], length(ID) ) )


        #} else if( rep == "ALL" ) { # expand data to fill all ID reps if rep contains keyword ALL

        #} else if( grepl(",", rep) || grepl(":", rep) ) { # expand if rep uses the r vector indexing syntax: 1:3,4,8:10 etc

        #summarise_reps <- TRUE
        # expand it in the c() function
        #rep <- eval(parse(text = paste0("c(",rep,")") )  )
        # separate space-separated segmentd with a comma to parse via c()
        #rep <- eval(parse(text = paste0("c(",gsub(" ", ", ", rep, fixed = TRUE),")") )  )

        # and expand ID by replicating it by length of rep now:
        #ID <- rep(ID, length(rep) )
        #for(k in 1:length(ID) ) {
        # need to loop through each ID now! index [k]
        # index <- match( TRUE, (dt[["ID"]] == ID[k] & dt[["rep"]] == as.character(rep[k]) ) )
        #indices <- c( indices, index )
        #}
        # AND fill new vector - expanded_data - with REPLICATED DATA
        #expanded_data <- c(expanded_data, rep(data[[i]][j], length(ID) ) )

        #} else { # ID and rep are single vals - so just process as normal!
        # index <- match( TRUE, (dt[["ID"]] == ID & dt[["rep"]] == rep) )
        #indices <- c( indices, index )

        #}
        #dt[[ data[[i]][1] ]][ index ] <- data[[i]][j]
        #}
        #this checks each ID and rep index and inserts the data CORRECTLY!

        #if( summarise_reps == TRUE ) { # need to rep each element in data[[i]][2:length(data[[i]])] by rep count!
        # dt <- add_data_col_type(dt, data[[i]][1], expanded_data, indices)

        #} else { # can add the data as it exists
        # dt <- add_data_col_type(dt, data[[i]][1], data[[i]][2:length(data[[i]])], indices)
        #}

      } # loop all cols

    } else { # NO REP COL
      # add all cols from data from 2:length(data) - SKIP ID COL ONLY

      newCol <- NA_character_ # for adding new cols - all will be character vectors

      # extract data_cols
      data_cols <- c() # use for loop : Error in data[[2:length(data)]] : recursive indexing failed at level 2
      for( i in 2:length(data) ) {
        data_cols[(i-1)] <- data[[i]][1]
      }

      # extract group_names - first entry in each list entry 2:length
      group_names <- data[[1]][2:length(data[[1]])]

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
          # here comparing 'group ids' with KNOWN EXISTING sample IDs from existing dt!

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



extract_ids_reps <- function(dt, data) {

  #ids_reps <- as.list(rep("", length(unique(dt$ID)))) # make a list with blank entries the length of all unique IDs
  ids_reps <- as.list(rep("", length(unique(data[[1]][2:length(data[[1]])]))))
  # make a list with blank entries the length of all unique IDs IN DATA

  ids_index <- 1 # start ids_index at 1 : index for the LIST
  ids_reps_index <- 1 # start ids_reps_index at 1 : index for the VECTOR at ids_index

  id_current <- data[[1]][2] # start with initial id

  for( a in 2:length(data[[1]]) ) {

    if( data[[1]][a] != id_current) { # if this is a new id
      # incremwnet ids_index
      ids_index <- ids_index +1
      # reset ids_reps_index
      ids_reps_index <- 1
    }
    # set current id
    id_current <- data[[1]][a]

    if(ids_reps[[ids_index]][1] != data[[1]][a]) {

      # if the ID in data at a has not been added, add the ID
      ids_reps[[ids_index]][ids_reps_index] <- data[[1]][a]
      ids_reps_index <- ids_reps_index +1

      # plus its rep col
      ids_reps[[ids_index]][ids_reps_index] <- data[[2]][a]
      ids_reps_index <- ids_reps_index +1

    } else {

      # just add the reps col
      ids_reps[[ids_index]][ids_reps_index] <- data[[2]][a]
      ids_reps_index <- ids_reps_index +1
    }

  }

  # return
  ids_reps

}




#' Parse data type
#'
#' Ensure data added to an existing column (expanded_data) at indices is of the same
#' type as any existing data.
#'
parse_data_type <- function(data, expanded_data, indices) {

  # check if datetime
  if( lubridate::is.instant(expanded_data[indices[1]]) == TRUE ) {
    # parse all entries in data through lubridate ymd_hm
    data <- lubridate::ymd_hm(data)
  } else if( lubridate::is.Date(expanded_data[indices[1]]) == TRUE ) { # check if Date
    # parse all entries in data through lubridate ymd
    data <- lubridate::ymd(data)
  } else if( !is.na( suppressWarnings( as.double(data[1]) ) ) ) { # check if double
    # parse all entries in data as double
    data <- as.double(data)
  } else {
    # parse all entries in data as character
    data <- as.character(data)
  }

  # return
  data
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
  data <- datatable_extract(dt_vector)

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
        #if( any(names(dt) == data[[i]][1] ) ) {
        #  stop( paste0("  Data col already exists in datatable: ", data[[i]][1],
        #               " dt name: ", trimws(strsplit( as.character(dt_vector[4]), ":")[[1]][1])))
        #}

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
  data <- datatable_extract(dt_vector)

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
  data <- datatable_extract(dt_vector)

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

    } # end IF FALSE

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

  # return:
  data

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


