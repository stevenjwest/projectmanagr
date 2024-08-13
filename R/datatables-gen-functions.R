
#' Create new Plaintext Data Table
#'
#' Creates a new Plaintext DataTable vector. The DataTable will contain an
#' initial ID column containing the `IDs` vector, and an optional set of extra
#' data columns as specified in the `data_cols` vector.  If the table exceeds
#' `dt_length` characters (default 100), then the table is split into multiple
#' tables, with IDs as first col, and subsequent data_cols given in subsequent
#' tables.
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
datatable_create <- function( IDs="", data_cols="", datatable_name = "samples",
                              default_data_vals=list(), dt_length = 100 ) {

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

  # ensure default_data_vals are all characters
  default_data_vals <- lapply(default_data_vals, as.character)

  # use data_col_wds to calc correct length of default_data_vals:
  if( length(default_data_vals) == 0 ) {
    # default_data_vals is a BLANK LIST - so fill with default_data_vals

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


  } else { # default_data_vals has been supplied with values

    # just check these are valid
    if( length(default_data_vals) != length(data_cols)) {
      stop( paste0("  number of default data vals does not match data_cols length: ", default_data_vals, " ", data_cols) )
    }

    # FIRST get list with single string of max length per col
    ddv <- list(1)
    for( i in 1:length(data_cols) ) {
      d <- unlist(strsplit(default_data_vals[[i]], ' '))
      ddv[[i]] <- d[ which.max( nchar(d) ) ]
    }

    # and identify data_col_wds based on data_cols && default_data_vals
    data_col_wds <- c()
    for(i in 1:length(data_cols) ) {
      data_col_wds[i] <- max(nchar(c(data_cols[[i]], ddv[[i]]) ))+2
    }

    # finally to handle multi-obs data - split at any SPACE in default_data_vals
    # build_data : supports adding multi-obs data
    for( i in 1:length(data_cols) ) {
      default_data_vals[[i]] <- unlist(strsplit(default_data_vals[[i]], ' '))
    }

  }


  #### build datatable ####
  data_tables <- build_datatable("ID", IDs, data_cols, default_data_vals,
                                 "CREATE", datatable_name, dt_length,
                                 DATATABLE_SPACER_CHAR)

  # return
  data_tables

}


#' Add new Samples Data to Data Table
#'
#' Adds new Sample data to an EXISTING Data Table in specified Rmd file at
#' specified line.  The Sample DataTable will contain an initial ID column
#' containing all the EXISTING IDs (IDs that have been resampled or exported
#' will automatically be EXCLUDED), and an optional set of extra data columns as
#' specified in the data_cols vector.  If the table exceeds dt_length characters
#' (default 100), then the table is split into multiple tables, with IDs as
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
#' @param default_data_vals List of default data values to add to data cols.
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
datatable_add_data_samples <- function( contents, data_cols, datatable_name,
                                        ids_vector="", default_data_vals=list(),
                                        dt_length = 100, summarise_reps = FALSE  ) {


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

  # define datatable - the datatable named datatable_name
  datatable <- datatables[[ datatable_name ]]

  # ?? THEN check col ID exists ??:
  if( any( names( datatable ) == "ID" ) == FALSE ) {
    stop( paste0("  Column ID missing from datatable: ", datatable_name ) )
  }

  # create boolean rep_exists initiate as FALSE - so group/ALL processing is handled correctly below with datatable creation!
  rep_exists <- FALSE

  #### DEFINE IDs:

  if( length(ids_vector)== 1 && ids_vector == "" ) { # if this is BLANK, define IDs as ALL IDs that EXIST:

    # determine whether reps exist in datatable : ie. the datatable was created by a subsampling with more than 1 rep
    rep_exists <- any( names( datatable) == "rep" )

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

      # NOW process IDs and REPs so there is ONE PER ID - summarise REPs!
      ID_rep <- summarise_id_rep(IDs, REPs)
      IDs <- ID_rep$IDs
      REPs <- ID_rep$REPs

    }

  } else {

    # ids_vector contains either ALL, <group-names>, <ID-names>
    # CHECK they are VALID

    gdt <- dplyr::select(datatable, dplyr::starts_with("group-"))
    iddt <- datatable$ID

    if( length(ids_vector)==1 && ids_vector[1] == "ALL" ) {
      # all good - set IDs to ALL
      IDs <- ids_vector[1]

      # NEED TO SUMMARISE REPS IF THEY EXIST:
      rep_exists <- any( names( datatable) == "rep" )

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
        rep_exists <- any( names( datatable) == "rep" )

        if( rep_exists == TRUE) { # for each ID in IDs, get the VALID reps:
          # first get all IDs/REPs
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
            ID_rep <- summarise_id_rep(IDs, REPs)
            IDs <- ID_rep$IDs
            REPs <- ID_rep$REPs
          }

        } else { # there are no reps, just check_divisions for IDs and only use VALID IDs

          # first get all IDs/REPs
          ID <- check_divisions_ids( datatable )
          # now filter through ID_rep$IDs/REPs saving only ones which match IDs
          IDs2 <- c()
          for(o in 1:length( ID ) ) {
            if( any(ID[o] == IDs) ) {
              IDs2 <- c(IDs2, ID[o])
            }
          }
          # and set IDs and REPs to these new filtered values
          IDs <- IDs2

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

  } #### END DEFINE IDs


  # CHECK none of data_cols already exists in datatable
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
        ID_rep <- summarise_id_rep(IDs, REPs)
        IDs <- ID_rep$IDs
        REPs <- ID_rep$REPs
      }
    } # end rep_exists
  } # end CHECK data_cols unique


  # use data_col_wds to calc correct length of default_data_vals:
  if( length(default_data_vals) == 0 ) { # default_data_vals is a BLANK LIST - so fill with default_data_vals

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
        #                       max(nchar(datatable[[ "rep" ]])),
        #                      5)
        #}
      } else if( endsWith(data_cols[i], "_dt") ) {
        data_col_wds[i] <- pmax(nchar(data_cols[i])+4, 18)
      } else {
        data_col_wds[i] <- pmax(nchar(data_cols[i])+4, 5) #pmax ensures min col width is 5!
      }
    }

    # compute default data values
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
    # confirm the number of default_data_vals at first LIST level is the same as data_cols
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
        ddv[[i]] <- rep(default_data_vals[[i]], length(IDs) )
      }
    }
    default_data_vals <- ddv # default_data_vals should have correct number of reps using default vals passed to this function

    # finally to handle multi-obs data - split at any SPACE in default_data_vals
    # build_data : supports adding multi-obs data
    for( i in 1:length(data_cols) ) {
      default_data_vals[[i]] <- unlist(strsplit(default_data_vals[[i]], ' '))
    }

  }

  #### build datatable ####
  data_tables <- build_datatable("ID", IDs, data_cols, default_data_vals,
                                 "ADD_DATA", datatable_name, dt_length,
                                 DATATABLE_SPACER_CHAR)

  # return
  data_tables

}


#' Add a variable-first data table
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
#' @param group_names Vector of sample IDs or GROUP NAMES from a group set, which will
#' constitute the remaining column headers.  These will typically be group
#' names.
#'
#' @param dt_length Int of data table max length in characters - default 100.
#'
#' @export
datatable_add_data_variables <- function(contents, var_names, datatable_name, group_names,
                                         dt_length = 100  ) {


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

  #### build datatable ####
  data_tables <- build_datatable("variables", IDs, group_names, default_data_vals,
                                 "ADD_DATA", datatable_name, dt_length,
                                 DATATABLE_SPACER_CHAR)

  # return
  data_tables

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
#' @param dt_length Int of data table max length in characters - default 100.
#'
#' @export
datatable_add_data_timetable <- function(contents, step_names, datatable_name, group_names,
                                         dt_length = 100 ) {

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

  #### build datatable ####
  data_tables <- build_datatable("timetable", IDs, group_names, default_data_vals,
                                 "ADD_DATA", datatable_name, dt_length,
                                 DATATABLE_SPACER_CHAR)

  # return
  data_tables

}


#' Add Groups to Data Table
#'
#' Adds Groups to an EXISTING Data Table in the character vector `contents`.
#' The Groups DataTable will contain an initial ID column containing all
#' sample IDs, and a series of extra data columns named using the
#' group_names vector.
#'
#' If the table exceeds dt_length characters
#' (default 100), then the table is split into multiple tables, with IDs as
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
#' @param dt_length Int of data table max length in characters - default 100.
#'
#' @param summarise_reps Boolean to indicate whether reps should be summarised in
#' the datatable.  If FALSE each ID/rep is on a separate line in the new datatable,
#' otherwise if TRUE, all reps are summarised using r vector index syntax on
#' one line in the new datatable.  i.e. each ID is listed ONCE and the reps are
#' indicated as: 1:3,5,6:10,12,14:25 etc.  Default to FALSE.
#'
#' @export
datatable_add_group <- function( contents, group_names, datatable_name,
                                 groups, dt_length = 100, summarise_reps = FALSE   ) {


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

  # FIRST split whether rep col exists && summarise_reps is TRUE:
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


  datatable <- datatables[[datatable_name]]
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
        ID_rep <- summarise_id_rep(IDs, REPs)
        IDs <- ID_rep$IDs
        REPs <- ID_rep$REPs
      }
    } # end rep_exists
  } # end CHECK data_cols unique


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

  #### build datatable ####
  data_tables <- build_datatable("ID", IDs, data_cols, groups, "GROUP",
                                 datatable_name, dt_length,
                                 DATATABLE_SPACER_CHAR)

  # return
  data_tables

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
#' @param dt_length Int of data table max length in characters - default 100.
#'
#' @export
datatable_resample <- function( contents, datatable_name, resample_vector, rep_vector = c(1),
                                dt_length = 100 ) {

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

  # check rep_vector is length 1 or length equal to resample_vector
  if( !(length(rep_vector) == 1) && !(length(rep_vector) == length(resample_vector)) ) {
    stop( paste0("  rep_vector must be length 1 or same length as resample_vector - rep_vector: ",
                 rep_vector, " resample_vector: ", resample_vector ) )
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
    if( length(rep_vector) == 1) {
      default_data_vals[[3]] <- rep(rep_vector, (length(IDs)*length(resample_vector)) ) # reps - fill with value of rep_vector
    } else { # rep_vector is same length as resample_vector - will affiliate these to each resample_vector across IDs
      default_data_vals[[3]] <- rep(rep_vector, (length(IDs)) ) # reps - fill be default with "1"
    }


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
    if( length(rep_vector) == 1) {
      default_data_vals[[2]] <- rep(rep_vector,(length(IDs)*length(resample_vector)) ) # reps - fill with value of rep_vector
    } else { # rep_vector is same length as resample_vector - will affiliate these to each resample_vector across IDs
      default_data_vals[[2]] <- rep(rep_vector, (length(IDs)) ) # reps - fill be default with "1"
    }

  }

  # build datatable to insert into Rmd:
  # using the OLD datatable_name - declaration of RESAMPLE table will END this datatable for IDs/reps in it
  #### build datatable ####
  data_tables <- build_datatable("ID", IDs, data_cols, default_data_vals,
                                 "RESAMPLE", datatable_name, dt_length,
                                 DATATABLE_SPACER_CHAR)

  # return
  data_tables


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
#' @param dt_length Int of data table max length in characters - default 100.
#'
#' @param summarise_reps Boolean to indicate whether reps should be summarised in
#' the datatable.  If FALSE each ID/rep is on a separate line in the new datatable,
#' otherwise if TRUE, all reps are summarised using r vector index syntax on
#' one line in the new datatable.  i.e. each ID is listed ONCE and the reps are
#' indicated as: 1:3,5,6:10,12,14:25 etc.  Default to FALSE.
#'
#' @export
datatable_dispose <- function( contents, datatable_name, dt_length = 100, summarise_reps = TRUE ) {

  cat( "\nprojectmanagr::datatable_dispose():\n" )

  # parse all lines in contents to extract all datatables:
  datatables <- datatable_read_vector(contents)

  # define data cols : dispose ONLY
  data_cols <- c("dispose")

  ### NAMED CONSTANTS For this function ###
  DATATABLE_SPACER_CHAR <- "="
  cdt <- get_datetime()

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

  #### build datatable ####
  data_tables <- build_datatable("ID", IDs, data_cols, default_data_vals,
                                 "ADD_DATA", datatable_name, dt_length,
                                 DATATABLE_SPACER_CHAR)

  # return
  data_tables


}



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
#' `dt_length` characters (default 100), then the table is split into multiple
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
#' Typically 100.
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
build_datatable_from_tibble <- function(tb, datatable_name, dt_function = "CREATE", dt_length = 100 ) {

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







