


#### _______________________________________________ ####

#' Find EXISTING Samples in Datatables
#'
#' Search through path recursively to find all Project Note Rmd files that
#' contain datatables. Summarise all EXISTING samples that are declared in them
#' (not resampled, exported, disposed).
#'
#' SUMMARY tibble includes following columns:
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
#' * LOCATION : values of last `_loc` col - where was the samples last known
#'   location?  Only defined last if currently in storage!
#'
#' * CONDITION : value of last `_con` col - what was the samples last
#'   known condition?
#'
#' If further metadata is needed - can use `datatable_get_sample_data_history()`
#' and filter for relevant information.
#'
#' @param path Path in a projectmanagr organisation to search projectmanagr Rmd files.
#' @param datatable_name_prefix Prefix for datatables, default `samples`
#' @param updateProgress Function to use with shiny progress bar - null by default.
#'
#' @return a tibble containing summary of all EXISTING samples, with attr "path" - the parent DIR
#' from which the samples were acquired.
#'
#' @export
datatable_find <- function( path, datatable_name_prefix = 'samples', updateProgress = NULL ) {

  cat( "\nprojectmanagr::datatable_find():\n" )

  # if not an absolute path:
  if( R.utils::isAbsolutePath(path) == FALSE ) {
    path <- R.utils::getAbsolutePath(path )
  }

  # check path is in a projectmanagr org
  orgPath <- find_org_directory(path)

  if(orgPath == "" ) {
    # the search reached the root of the filesystem without finding the Organisation files,
    # therefore, rmd_path is not inside a PROGRAMME sub-dir!
    stop( paste0("  path is not in a projectmanagr Organisation Directory: ", path) )
  }
  # now, orgPath should be the root dir of the organisation

  # get config templates settings yml
  confPath <- get_config_dir(orgPath)
  tempPath <- get_template_dir(orgPath)
  settings <- get_settings_yml(orgPath)

  # normalize path - remove HOME REF ~
  path <- normalizePath(path)

  # get all Project Docs/Notes inside path - they all contain "~_" in filename and end with .Rmd
  if( file.info(path)$isdir == FALSE ) {
    samplesList <- path
  } else {
    samplesList <- list.files(path, pattern="*.Rmd", full.names = TRUE, recursive=TRUE)
    samplesList <- samplesList[ (regexpr("~_", samplesList) > 0) & (regexpr(".Rmd", samplesList) > 0) ]
  }

  samplesListLength <- length(samplesList) # for updateProgress


  # define a new Summary tibble to hold all samples:
  # MUST define a STANDARD TEMPLATE to hold SUMMARY DATA on samples
  # From this summary information, should be possible to select samples, or further explore them
  ID <- ""      # ID: Each Sample ID
  SAMPLE <- ""  # SAMPLE: COMPOSITE of all subsampling columns: CNS-RT-MB etc.
  PREFIX <- ""     # EXP: Fill with the Experiment Prefix ID
  TITLE <- ""   # TITLE: Fill with Experiment Title - will contain the LAB_TREATMENT
  #PATH <- ""    # PATH: Put the absolute PATH to the Project Note Rmd to Navigate to
  LOCATION <- "" # LOCATION of the sample - where is it?
  CONDITION <- "" # CONDITION of sample - what is it in?
  DATETIME <- "" # DATETIME sample was moved to current CONDITION/LOCATION
  COUNT <- integer()  # COUNT: How many REPS are there of this sample?
  IMPORT <- integer()  # IMPORT: How many REPS to import from this sample?

  #samples_summary <- tibble::tibble(ID, SAMPLE, COUNT, PREFIX, TITLE, LOCATION, CONDITION)
  samples_summary <- tibble::tibble(PREFIX, TITLE, ID, SAMPLE,
                                    CONDITION, LOCATION, DATETIME,
                                    COUNT, IMPORT)


  # define new LIST for holding COL_NAMES from each note (PREFIX) plus sample DATATABLE
  COL_NAMES <- list()

  for(s in samplesList) {
    # for each Rmd
    cat( "\n  reading file : ", basename(s), "\n" )

    if (is.function(updateProgress)) {
      progressFraction <- match(s, samplesList) / samplesListLength
      text <- paste0("Rmd file : ", basename(s) )
      updateProgress(value = progressFraction, detail = text)
    }

    rmd_contents <- read_file(s)
    dts <- datatable_read_vector(rmd_contents)

    if( is.null(dts) | length(dts) == 0 ) {
      # if dts is a blank list or null - skip this loop
      next
    }

    # apply datatable_name_prefix filter
    dts <- dts[startsWith(names(dts), datatable_name_prefix)]

    for(d_index in 1:length(dts) ) {
    #for(d_index in 1:16) {
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

      # ONLY CONTINUE if any rows remain
      if( nrow(d) > 0 ) {

        # summarise across reps
        #colsnorep <- col_names[!"rep" == col_names] # want to group by all EXCEPT rep
        #dg <- dplyr::group_by_at(d, colsnorep )
        dg <- dplyr::group_by_at(d, "ID" ) # group by the ID - this is ALWAYS THE FIRST COL!
        dc <- dplyr::summarise(dg, count=dplyr::n() ) # summarise the reps - count ID entries to retrieve number of reps
        dc <- dplyr::ungroup(dc) # remove group_by


        #### for each sample ID ####


        #### GET LAST KNOWN LOCATION ####

        #if( endsWith(col_names[length(col_names)], "_loc") ){ # only retrieve location if LAST ENTRY
        if( length(grep("_loc", col_names) ) > 0 ) { # retrieve last location IF PRESENT

          # copy last location vector to LOC
          LOC <- get_last_unique_col_suffix(dg, "_loc")

        } else {
          LOC <- "-" # else just fill with BLANK VAL: -
        }


        #### GET LAST KNOWN CONDITION ####

        if( length(grep("_con", col_names) ) > 0 ){ # retrieve last condition IF PRESENT

          # copy last condition vector to CON
          CON <- get_last_unique_col_suffix(dg, "_con")

        } else {
          CON <- "-" # else just fill with BLANK VAL: -
        }


        #### GET LAST KNOWN DATETIME ####

        if( length(grep("_dt", col_names) ) > 0 ){ # retrieve last dt IF PRESENT

          # copy last condition vector to DT
          DT <- get_last_unique_col_suffix(dg, "_dt")

        } else {
          DT <- "-" # else just fill with BLANK VAL: -
        }

        # edit t - mutate to form ID, SAMPLE, COUNT, EXP
         # ID, SAMPLE, COUNT, PREFIX, TITLE, LOCATION, CONDITION
        dc <- dplyr::transmute(
          dc,
          PREFIX=get_project_prefix_from_path( s,settings ),
          TITLE=get_name_from_file_name(basename(s), settings ),
          ID=ID,
          SAMPLE=d_name,
          CONDITION=CON,
          LOCATION=LOC,
          DATETIME=DT,
          COUNT=count,
          IMPORT=rep(0,length(count)))

        # bind dc to samples_summary
        samples_summary <- dplyr::bind_rows(samples_summary, dc)

        # and store the col_names from d to the COL_NAMES list
        if( length(COL_NAMES) == 0 ) {
          # append FIRST col_names vector
          COL_NAMES[[1]] <- col_names
          # and set the name of this first entry to PREFIX plus SAMPLE
          names(COL_NAMES) <- paste0(get_prefix( s, settings ), "~_", d_name)

        } else {
          # append col_names vector to END of COL_NAMES list
          COL_NAMES[[length(COL_NAMES)+1]] <- col_names
          # and set the name of this last entry to PREFIX plus SAMPLE (no +1 as COL_NAMES is now longer!)
          names(COL_NAMES)[length(COL_NAMES)] <- paste0(get_prefix( s, settings ), "~_", d_name)
        }
      }
    } # for d
  } # for s

  # add attribute to samples_summary - path, and col_names
  attr(samples_summary, "path") <- path
  attr(samples_summary, "col_names") <- COL_NAMES

  # return samples_summary
  samples_summary

}



#' Get last unique data col value based on suffix
#'
#' @param dg datatable grouped by ID desired to summarise by.
#'
#' @param col_suffix The suffix to filter col names by.
#'
#' @return vector of unique values for each group-by ID from last data col
#' with specified suffixs
get_last_unique_col_suffix <- function(dg, col_suffix = "_loc") {

  col_names <- names(dg)

  # get the last location col index
  loc_indices <- grep(col_suffix, col_names)
  loc_index <- loc_indices[ length(loc_indices)]

  # rename this col to a known string - to summarise it by group
  dggg <- dg
  names(dggg)[loc_index] <- "KNOWNCOLNAME"


  # summarise the table - space-separate col values for each ID
  dl <- dplyr::summarise(dggg, NEWCOLNAME=paste( unique(KNOWNCOLNAME), collapse=' '))

  # retrieve NEWCOLNAME vector
  LOC <- dl$NEWCOLNAME

  # col may contain multiple values - space-separated
  # only want last value - so extract last space-separated value from each element in vector
  LOC <- unlist(lapply(strsplit(LOC, ' '), function(x) {x[length(x)]} ) )

  LOC # return

}


#' Export a set of samples from summary datatable
#'
#' This function will write an EXPORT table to the end of each project note Rmd
#' in samples_summary table, exporting the samples and reps declared in the
#' samples_summary table.  Only samples which have IMPORT col set to above 1
#' are exported - the number in this col declares the number of reps to be
#' exported.
#'
#' @param samples_summary A summary tibble from `datatable_find()`, with the
#' final IMPORT column curated to include the number of reps to export from
#' each sample/rep.
#'
#' @param destination_note_path Path within a projectmanagr organisation where
#' the exported samples/reps will be moved to.
#'
#' @param datetime The datetime of export - if blank, current datetime is
#' generated automatically.
#'
#' @param summarise_reps If true all export datatables generated with summarise
#' the exported reps, so each sample ID appears once in export datatable, and
#' reps are abbreviated as 1:3, 4:5, etc (depending on which reps are exported).
#' True by default.
#'
#' @param dt_length Int of data table max length in characters - default 100.
#'
#' @return List of all character vectors of EXPORT datatables inserted into
#' notes, with attr "path" that contains the absolute path of the source note.
#'
#' @export
datatable_export <- function(samples_summary, destination_note_path,
                             datetime="", summarise_reps = TRUE, dt_length = 100) {

  cat( "\nprojectmanagr::datatable_export():\n" )

  ### NAMED CONSTANTS ###
  DATATABLE_SPACER_CHAR <- "="

  # filter samples_summary - remove any rows where IMPORT is 0
  samples_summary <- dplyr::filter(samples_summary, IMPORT > 0 )

  #  samples_summary Rmd : found in parent DIR path (attr in sampled_summary) from PREFIX~_TITLE
  path <- attr(samples_summary, "path")

  # if not an absolute path:
  if( R.utils::isAbsolutePath(path) == FALSE ) {
    path <- R.utils::getAbsolutePath(path )
  }

  # check path is in a projectmanagr org
  orgPath <- find_org_directory(path)

  if(orgPath == "" ) {
    # the search reached the root of the filesystem without finding the Organisation files,
    # therefore, rmd_path is not inside a PROGRAMME sub-dir!
    stop( paste0("  path is not in a projectmanagr Organisation Directory: ", path) )
  }
  # now, orgPath should be the root dir of the organisation

  # normalize path - remove HOME REF ~
  path <- normalizePath(path)

  # make datetime obj for export
  if( datetime == "" ) {
    export_datetime <- get_datetime()
    export_date <- get_date()
  } else{
    export_datetime <- datetime
    export_date <- substr(export_datetime, 1, 10)
  }

  # create list to keep all_data_tables
  all_data_tables <- list(NULL)
  all_data_tables_index <- 1

  # identify each UNIQUE Rmd
  prefixes <- unique(samples_summary$PREFIX)

  for(i in 1:length(prefixes) ) {

    # define rmd_path
    prefix <- prefixes[i]
    title <- unique(samples_summary$TITLE)[i]
     # use list.files to find the file in path - in case it is in a sub-dir
    rmd_path <- list.files(path, pattern=paste0("*", prefix, "~_", title, ".Rmd"), full.names = TRUE, recursive=TRUE)[1]

    # get all datatables from Rmd
    rmd_file_conn <- file( rmd_path )
    rmd_contents <- readLines( rmd_file_conn )
    close(rmd_file_conn)

    # read datatables
    dts <- datatable_read_vector(rmd_contents)

    # confirm the samples/reps EXIST in dts!
    # get samples_summary for this prefix/title
    sam_sum <- dplyr::filter(samples_summary, PREFIX == prefix)

    # loop through each SAMPLE dt in sam_sum to check
    sum_dts <- unique(sam_sum$SAMPLE)
    for(j in 1:length(sum_dts) ) {

      dt <- dts[[sum_dts[j]]] # get dt for correct sample set
      sam_sum_sam <- dplyr::filter(sam_sum, SAMPLE == sum_dts[j]) # filter sam_sum to current samples

      ids <- sam_sum_sam$ID
      for( k in 1:length(ids) ) {

          dt_id_vector <- dt$ID[ dt$ID == ids[k] ] # get every instance of id in dt - NUMBER is num reps AVAILABLE
          # check ID exists
          if( identical(dt_id_vector, character(0)) ) {
            # if equal to character(0) the ID doesnt exist
            stop( paste0("  ID does not exist in datatable : ", id, " dt : ", sum_dts[j], " Rmd : ", basename(rmd_path)))
          }

          sum_rep <- sam_sum_sam$IMPORT[k]
          # check there are enough reps
          if( length(dt_id_vector) < sum_rep ) {
            # length of dt_id_vectors equals num reps AVAILABLE - ensure its equal or more than sum_rep
            # if LESS THAN - STOP
            stop( paste0("  ID reps not enough in datatable for IMPORT request - reps requested: ", sum_rep,
                         " reps available : ", length(dt_id_vector), " id : ", id, " dt : ", sum_dts[j], " Rmd : ", basename(rmd_path)))
          }
      } # for k
    } # for j
  } # for i

  # reaching here without stop - successfully passed QC
  cat( "\n  All samples exist for export...\n" )

  # build the export datatable for each RMD and Datatable
  for(i in 1:length(prefixes) ) {

    # reset data_tables with each new Rmd
    data_tables <- list(NULL)
    data_tables_index <- 1

    # define rmd_path
    prefix <- prefixes[i]
    title <- unique(samples_summary$TITLE)[i]
    # use list.files to find the file in path - in case it is in a sub-dir
    rmd_path <- list.files(path, pattern=paste0("*", prefix, "~_", title, ".Rmd"), full.names = TRUE, recursive=TRUE)[1]

    cat( "\n  export from file : ", basename(rmd_path) )
    # get all datatables from Rmd
    rmd_file_conn <- file( rmd_path )
    rmd_contents <- readLines( rmd_file_conn )
    close(rmd_file_conn)

    # read datatables
    dts <- datatable_read_vector(rmd_contents)

    # get samples_summary for this prefix/title
    sam_sum <- dplyr::filter(samples_summary, PREFIX == prefix)

    # loop through each SAMPLE dt in sam_sum
    sum_dts <- unique(sam_sum$SAMPLE)
    for(j in 1:length(sum_dts) ) {

      dt <- dts[[sum_dts[j]]] # get dt for correct sample set
      sam_sum_sam <- dplyr::filter(sam_sum, SAMPLE == sum_dts[j]) # filter sam_sum to current samples

      IDs <- rep(sam_sum_sam$ID, sam_sum_sam$IMPORT)

      rel_link <- R.utils::getRelativePath(destination_note_path, relativeTo=rmd_path)
      rel_link <- substring(rel_link, first=4, last=nchar(rel_link)) # remove first `../`
      rel_link <- substr(rel_link, 1, regexpr("~_", rel_link)-1) # remove file NAME

      if( any(names(dt) == "rep") ) { # dt is a datatable with REPS - so must handle these appropriately

        if( summarise_reps == TRUE ) { # if summarising reps, just insert ONE ID plus a summary rep vector

          rep_vals <- c()
          for( ID_VAL in unique(IDs) ) { # for each UNIQUE ID
            dt_id <- dplyr::filter(dt, ID == ID_VAL)
            num_rep_requested <- length( grep(ID_VAL, IDs)) # for each requested, get a rep value
            rep_val <- dt_id$rep[1:num_rep_requested]
            first_rep_val <- rep_val[1]
            last_rep_val <- rep_val[length(rep_val)]
            rep_val <- paste0( first_rep_val, ":", last_rep_val)
            rep_vals <- c(rep_vals, rep_val )
          }
          IDs <- unique(IDs)

        } else {
          # first, identify identities of EXISTING reps to allocate in EXPORT DT
          rep_vals <- c()
          for( ID_VAL in unique(IDs) ) { # for each UNIQUE ID
            dt_id <- dplyr::filter(dt, ID == ID_VAL)
            num_rep_requested <- length( grep(ID_VAL, IDs)) # for each requested, get a rep value
            rep_vals <- c(rep_vals, dt_id$rep[1:num_rep_requested] )
          }
        }

        #data_cols <- c("export", "export_dt")
        data_cols <- c("rep", "export") # ensure the rep col is present!
        data_vals <- list(NULL)

        # rep vals
        data_vals[[1]] <- rep_vals
        # export vals
        data_vals[[2]] <- rep( rel_link, length(IDs) )
        # export dt
        #data_vals[[2]] <- rep( export_datetime, length(IDs) )

      } else {

        #data_cols <- c("export", "export_dt")
        data_cols <- c("export")

        data_vals <- list(NULL)
        # export vals
        data_vals[[1]] <- rep( rel_link, length(IDs) )
        # export dt
        #data_vals[[2]] <- rep( export_datetime, length(IDs) )
      }

      datatable_name <- sum_dts[j]

      #### build datatable ####
      data_tables[[data_tables_index]] <- build_datatable("ID", IDs, data_cols, data_vals,
                                     "EXPORT", datatable_name, dt_length,
                                     DATATABLE_SPACER_CHAR)

      data_tables_index <- data_tables_index + 1


      # add data_table to all_data_tables
      all_data_tables[[all_data_tables_index]] <- data_tables[[(data_tables_index-1)]]
      # add attr "path" to all_data_tables
      attr(all_data_tables[[all_data_tables_index]], "path") <- rmd_path
      # increment index
      all_data_tables_index <- all_data_tables_index + 1

    } # for j

    # add export tables to end of rmd_path
    export_vector <- c()
    last_line_index <- computeLastLineIndex(rmd_contents, sep=TRUE)

    if( all(unlist(strsplit(rmd_contents[last_line_index-1] , "")) == '-') == TRUE ) {
      # if the string is a sequence of --- : then DO NOT ADD dashes to START of export vector
      export_vector <- c(export_vector, "", "", "")
    } else {
      # ADD DASHES to start of export vector
      export_vector <- c(export_vector, "", "", "", "------", "", "", "")
    }

    # compose header for EXPORT section
    export_vector <- c(export_vector,
                       paste0("# EXPORT : ", export_date ),
                       "", "",
                       paste0("Export samples to ", substr(basename(destination_note_path), 1,
                                                           nchar(basename(destination_note_path))-4 ) ),
                       "",""
                       )

    # add each data_table
    for( d in data_tables) {
      export_vector <- c(export_vector, "", d)
    }

    # add footer
    export_vector <- c(export_vector, "", "------", "", "", "")


    # save rmd vector to rmd_path
    cat( "\n    write export data table(s) to Rmd at line: ", last_line_index )
    rmd_contents <- c( rmd_contents[1:(last_line_index-1)],
                       export_vector,
                       rmd_contents[last_line_index:length(rmd_contents)] )

    rmd_file_conn <- file( rmd_path )
    writeLines(rmd_contents, rmd_file_conn)
    close(rmd_file_conn)

  } # for i

  # return ALL export data_tables with attr "path" for each
  all_data_tables

}



#' Import a set of samples and reps from a summary datatable
#'
#'
#' Export a set of samples from summary datatable
#'
#' This function will write an IMPORT table to the end of each project note Rmd
#' in samples_summary table, exporting the samples and reps declared in the
#' samples_summary table.  Only samples which have IMPORT col set to 1 or above
#' are exported - the number in this col declares the number of reps to be
#' exported.
#'
#' @param samples_summary A summary tibble from datatable_find, with the final
#' IMPORT column curated to include the number of reps to export from each
#' sample/rep.
#'
#' @param export_datatables A list of generated data_tables that were exported -
#' to retrieve any rep indexes as needed for generating the import datatable(s).
#'
#' @param destination_note_path Path within a projectmanagr organisation where
#' the imported samples/reps will be moved to.
#'
#' @param destination_note_line Line within `destination_note_path` where the
#' imported samples/reps will be copied to.
#'
#' @param datetime The datetime of import - if blank, current datetime is
#' generated automatically.
#'
#' @param summarise_reps If true all import datatables generated with summarise
#' the imported reps, so each sample ID appears once in import datatable, and
#' reps are abbreviated as 1:3, 4:5, etc (depending on which reps are imported).
#' FALSE by default - so each rep is on its own line.
#'
#' @param dt_length Int of data table max length in characters - default 100.
#'
#' @export
datatable_import <- function(export_datatables, destination_note_path, destination_note_line,
                             datetime="", summarise_reps = FALSE,
                             dt_length = 100) {


  cat( "\nprojectmanagr::datatable_import():\n" )

  ### NAMED CONSTANTS ###
  DATATABLE_SPACER_CHAR <- "="

  # if not an absolute path:
  if( R.utils::isAbsolutePath(destination_note_path) == FALSE ) {
    destination_note_path <- R.utils::getAbsolutePath(destination_note_path )
  }

  # check destination_note_path is in a projectmanagr org
  orgPath <- find_org_directory(destination_note_path)

  if(orgPath == "" ) {
    # the search reached the root of the filesystem without finding the Organisation files,
    # therefore, rmd_path is not inside a PROGRAMME sub-dir!
    stop( paste0("  path is not in a projectmanagr Organisation Directory: ", path) )
  }
  # now, orgPath should be the root dir of the organisation

  # normalize path - remove HOME REF ~
  destination_note_path <- normalizePath(destination_note_path)

  # make datetime obj for export
  if( datetime == "" ) {
    import_datetime <- get_datetime()
    import_date <- get_date()
  } else{
    import_datetime <- datetime
    import_date <- substr(import_datetime, 1, 10)
  }

  # create new import_datatables list
  import_datatables <- list(NULL)

  # for each dt_vector in export_datatables
  for( i in 1:length(export_datatables) ) {

    # first make the relative link from destination_path to source_path for this dt
    source_path_rmd <- attr(export_datatables[[i]], "path")
    rel_link <- R.utils::getRelativePath(source_path_rmd, relativeTo=destination_note_path)
    rel_link <- substring(rel_link, first=4, last=nchar(rel_link)) # remove first `../`
    rel_link <- substr(rel_link, 1, regexpr("~_", rel_link)-1) # remove file NAME


    # grab the data from the dt
    indices <- which( startsWith(export_datatables[[i]], "+===") )
    tables <- tables <- export_datatables[[i]][indices[1]:indices[2]]
    data <- projectmanagr::datatable_extract(tables)

    # if data[[2]][1] is "rep" need to deal with reps
    if( data[[2]][1] == "rep" ) {

      # depends on the format of the data how to deal with it
      if( summarise_reps == FALSE ) {
        # so EXPAND reps - this works if there is only one rep!
        IDs <- data[[1]][2:length(data[[1]])]
        REPs <- data[[2]][2:length(data[[2]])]
        IDs2 <- c()
        REPs2 <- c()
        imports <- c()
        for( j in 1:length(REPs) ) {
          r <- eval(parse(text = paste0("c(",REPs[j],")") )  )
          REPs2 <- c(REPs2, as.character(r) )
          IDs2 <- c(IDs2, rep( as.character(IDs[j]), length(r)) )
          imports <- c(imports, rep(rel_link, length(r)) )
        }

        # adjust data to fill with expanded IDs/REPs and imports data
        data[[3]][1] <- "import" # replace export header with import
        data[[1]] <- c(data[[1]][1], IDs2)
        data[[2]] <- c(data[[2]][1], REPs2)
        data[[3]] <- c(data[[3]][1], imports)


      } else { # summarise_reps is TRUE

        # summarise reps if needed
        IDs <- data[[1]][2:length(data[[1]])]
        IDs_unique <- unique(IDs)
        if( length(IDs_unique) == length(IDs) ) {
          # the reps are already summarised!
          # so just modify the export header to import, and its vals
          data[[3]][1] <- "import" # alter header
          imports <- rep(rel_link, length(IDs))
          data[[3]] <- c(data[[3]][1], imports)

        } else { # IDs and REPs need to be summarised
          REPs <- data[[2]][2:length(data[[2]])]
          list_ids_reps <- summarise_id_rep(IDs, REPS)

          data[[1]] <- c(data[[1]][1], list_ids_reps$IDs)
          data[[2]] <- c(data[[2]][1], list_ids_reps$REPs)
          data[[3]][1] <- "import" # alter header
          imports <- rep(rel_link, length(list_ids_reps$IDs))
          data[[3]] <- c(data[[3]][1], imports)

        }

      } # end if summarise_reps

      # generate new import datatable from data - with ID REP and IMPORT cols
      IDs <- data[[1]][2:length(data[[1]])]
      data_cols <- c(data[[2]][1], data[[3]][1])
      data_vals <- list(data[[2]][2:length(data[[2]])], data[[3]][2:length(data[[3]])])
      datatable_name <-  trimws( substr(tables[4], 1, regexpr(":", tables[4])-1 ) )

      cat("Build Datatable Reps i : ", i)
      #### build datatable ####
      import_datatables[[i]] <- build_datatable("ID", IDs, data_cols, data_vals,
                                                "IMPORT", datatable_name, dt_length,
                                                DATATABLE_SPACER_CHAR)

    } else { # data has no reps

      # modify data - col 2 is import, and fill with new rel_link
      data[[2]][1] <- "import"
      data[[2]] <- c(data[[2]][1], rep(rel_link, length(data[[1]])-1) ) # -1 as first entry in data[[1]] is the header

      # generate new import datatable from data - with ID and IMPORT cols
      IDs <- data[[1]][2:length(data[[1]])]
      data_cols <- c(data[[2]][1])
      data_vals <- list(data[[2]][2:length(data[[2]])])
      datatable_name <-  trimws( substr(tables[4], 1, regexpr(":", tables[4])-1 ) )

      cat("Build Datatable Reps i : ", i)
      import_datatables[[i]] <- build_datatable("ID", IDs, data_cols, data_vals,
                                                "IMPORT", datatable_name, dt_length,
                                                DATATABLE_SPACER_CHAR)

    } # end data reps

  } # end for i in export_datatables

  # build IMPORT SECTION to insert into destination_rmd

  rmd_file_conn <- file( destination_note_path )
  rmd_contents <- readLines( rmd_file_conn )
  close(rmd_file_conn)

  import_vector <- c("","","")

  # add each data_table
  for( d in import_datatables) {
    import_vector <- c(import_vector, "", d)
  }

  # write these to the file:
  cat( "\n    write import data table(s) to Rmd at line: ", destination_note_line )
  rmd_contents <- c( rmd_contents[1:destination_note_line],
                     import_vector,
                     rmd_contents[(destination_note_line+1):length(rmd_contents)] )

  rmd_file_conn <- file( destination_note_path )
  writeLines(rmd_contents, rmd_file_conn)
  close(rmd_file_conn)

}



#' Load all sample data from summary datatable
#'
#' Retrieves all sample data, traversing all divisions (resample, export) of
#' all samples in a summary datatable, generated by datatable_find()
#'
#' Will return all columns that are COMMON amongst all samples in datatable.
#'
#' Passes back through all Rmd's through imports, and identifies the CREATE
#' datatable for each sample.  Then collects all data, moving through all
#' resampling and exports, and omitting any blank columns.
#'
#' Returns a list of tibbles that contains all sample data, collating samples
#' where the data columns match.
#'
datatable_load_data <- function( dt ) {

}


#### _______________________________________________ ####







