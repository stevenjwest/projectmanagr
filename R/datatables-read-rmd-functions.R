

#' Read datatables in Rmd
#'
#' Function to read all projectmanagr datatables declared across one or more
#' plain text documents (typically an R Markdown doc), and return them as
#' tibbles.
#'
#' 1. Extracts all datatables between `datatable_get_delimiter()` and checks the
#' validity of each datatable declared:
#'
#' 2. Datatables have NAMES, and contains samples which must all possess a
#' unique ID.
#'
#' 3. Datatables have FUNCTIONS - sample IDs are created in CREATE, and have
#' data added to them in ADD_DATA tables.  IDs from a given datatable can be put
#' into new groups using a GROUP table, resampled into subsamples in RESAMPLE
#' tables, disposed in DISPOSE tables, and exported to a new file with EXPORT
#' tables.
#'
#' 4. Samples may also be generated via IMPORT tables, which links with
#' corresponding EXPORT tables in linked notes.  This function reads the IMPORT
#' tables, and traverses back to the source files to extract all existing meta
#' data for these sample IDs up to the point of EXPORT from the source note.
#'
#'
#' @param rmd_path Path to RMarkdown Document.
#'
#' @return a NAMED LIST of tibbles containing all data.  Names correspond to
#'  the names of the datatables in the file.  Pivot tables from IMPORT IDs are
#'  generated for each source file - using the file name (or minimal unique
#'  path) as the NAME for this table (which will be present in the `import`
#'  column alongsidethe ID in its original table from the first file).
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

  # retrograde reading across IMPORT columns

  # return
  datatables

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





#' Read and return all ACTIVE IDs in datatables
#'
#' Excludes all IDs that have been RESAMPLED DISPOSED or EXPORTED.
#'
datatable_read_rmd_active_ids <- function(rmd_contents, rmd_line) {

  # get datatables
  dts <- datatable_read_vector(rmd_contents[1:rmd_line])

  # then FILTER each row of datatables
  # NO resample col OR resample col is BLANK for row

  for( i in 1:length(dts) ) {
    dplyr::filter()
  }
  # NO export col OR export col is BLANK for row


  # return filtered dts:
  filtered_dts

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



#' Find EXISTING Samples in Datatables
#'
#' Search through path recursively to find all Project Note Rmd files that
#' contain datatables. Summarise all EXISTING samples & sub-samples that are
#' declared in them (not EXPORTED or DISPOSED - any that have been resampled,
#' will list the EXISTING sub-samples!).
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
OLDdatatable_find <- function( path, datatable_name_prefix = 'samples', updateProgress = NULL ) {

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

    pnPrefix <- get_project_prefix_from_path(s, settings)
    pnTitle <- get_name_from_file_name(basename(s), settings )

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

        # edit datatable - mutate to form ID, SAMPLE, COUNT, EXP
        # ID, SAMPLE, COUNT, PREFIX, TITLE, LOCATION, CONDITION
        dc <- dplyr::transmute(
          dc,
          PREFIX = pnPrefix,
          TITLE = pnTitle,
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



#' Find EXISTING Samples in Datatables
#'
#' Search through path recursively to find all Project Note Rmd files that
#' contain datatables. Summarise all EXISTING samples & sub-samples that are
#' declared in them (not EXPORTED or DISPOSED - any that have been resampled,
#' will list the EXISTING sub-samples!).
#'
#' SUMMARY tibble includes following columns:
#'
#' * ID - sample ID
#'
#' * SAMPLE: Composite of all subsampling Strings - what the sample is
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
#' @param path Path dirTree in a projectmanagr organisation to search for
#' projectmanagr Project Note files.
#'
#' @param settings List from `.config/settings.yml` file.
#'
#' @param summary_dt
#'
#' @param updateProgress Function to use with shiny progress bar - null by default.
#'
#' @return a tibble containing summary of all EXISTING samples, with attr "paths" -
#' the parent DIR from which the samples were acquired.
#'
#' @export
datatable_find <- function(datapath, settings, updateProgress=NULL) {

  summary_dt <- gen_summary_dt()
  # define new LIST for holding COL_NAMES from each note (PREFIX) plus sample DATATABLE
  COL_NAMES <- list()
  paths <- list()

  projectNotePaths <- get_project_note_paths(datapath, settings)
  pnpRel <- fs::path_rel(projectNotePaths, start=datapath)

  summary_dt <- parse_project_note_find_dts(summary_dt, COL_NAMES, paths, projectNotePaths,
                                            settings, pnpRel, updateProgress)

  # return summary_dt
  summary_dt

}



gen_summary_dt <- function() {
  # generate a blank table to initialise addin with
  #PREFIX <- ""     # EXP: Fill with the Experiment Prefix ID
  #TITLE <- ""   # TITLE: Fill with Experiment Title - will contain the LAB_TREATMENT
  FILENAME <- "" # filename where datatables are read from
  ID <- ""      # ID: Each Sample ID
  SAMPLE <- ""  # SAMPLE: COMPOSITE of all subsampling columns: CNS-RT-MB etc.
  #PATH <- ""    # PATH: Put the absolute PATH to the Project Note Rmd to Navigate to
  LOCATION <- "" # LOCATION of the sample - where is it?
  CONDITION <- "" # CONDITION of sample - what is it in?
  DATETIME <- "" # DATETIME sample was moved to current CONDITION/LOCATION
  COUNT <- integer()  # COUNT: How many REPS are there of this sample?
  IMPORT <- integer()  # IMPORT: How many REPS to import from this sample?
  PATH <- "" # relative path to project note from the datapath

  #samples_summary <- tibble::tibble(ID, SAMPLE, COUNT, PREFIX, TITLE, LOCATION, CONDITION)
  # tibble::tibble(PREFIX, TITLE, ID, SAMPLE, LOCATION, CONDITION, DATETIME, COUNT, IMPORT)
  tibble::tibble(FILENAME, ID, SAMPLE, LOCATION, CONDITION, DATETIME, COUNT, IMPORT, PATH)
}


parse_project_note_find_dts <- function(summary_dt, COL_NAMES,  paths, projectNotePaths,
                                        settings, pnpRel, updateProgress) {

  projectNotePathsLength <- length(projectNotePaths)

  pdf <- list(summary_dt=summary_dt, COL_NAMES=COL_NAMES, paths=paths)

  for(si in 1:length(projectNotePaths) ) {
    s <- projectNotePaths[si]
    sR <- pnpRel[si]
    # for each Rmd
    cat( "\n  reading file : ", fs::path_file(s), "\n" )

    if (is.function(updateProgress)) { # show progress of processing notes
      progressFraction <- match(s, projectNotePaths) / projectNotePathsLength
      text <- paste0("Rmd file : ", fs::path_file(s) )
      updateProgress(value = progressFraction, detail = text)
    }

    rmd_contents <- read_file(s)
    dts <- datatable_read_vector(rmd_contents)

    if( is.null(dts) | length(dts) == 0 ) { # if dts is a blank list or null - skip this loop
      next
    }

    pdf <- parse_dts_pn_find_dts(pdf$summary_dt, pdf$COL_NAMES, pdf$paths, s, dts, settings, sR)

  } # for s

  # add attribute to summary_dt - path, and col_names
  attr(pdf$summary_dt, "paths") <- pdf$paths
  attr(pdf$summary_dt, "col_names") <- pdf$COL_NAMES

  # return summary_dt
  pdf$summary_dt
}


parse_dts_pn_find_dts <- function(summary_dt, COL_NAMES, paths, s, dts, settings, sR) {

  # apply datatable_name_prefix filter - not used anymore
  # dts <- dts[startsWith(names(dts), datatable_name_prefix)]

  pnPrefix <- get_project_prefix_from_path(s, settings)
  pnTitle <- get_name_from_file_name(basename(s), settings )

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

      ID <- unique(d$ID)

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

      # edit datatable - mutate to form ID, SAMPLE, COUNT, EXP
      # ID, SAMPLE, COUNT, PREFIX, TITLE, LOCATION, CONDITION
      # dc <- dplyr::transmute( # using variables from dc to fill columns - while omitting original dc cols
      #   dc,
      #   PREFIX = pnPrefix,
      #   TITLE = pnTitle,
      #   ID=ID,
      #   SAMPLE=d_name,
      #   CONDITION=CON,
      #   LOCATION=LOC,
      #   DATETIME=DT,
      #   COUNT=count,
      #   IMPORT=rep(0,length(count)))

      # using mutate with .keep "none" as transmute is superseded
      # need to wrap in relocate to re-order the cols - as ID set to first col with mutate as it already exists!
      dd <- dplyr::relocate(dplyr::mutate( # using variables from dc to fill columns - while omitting original dc cols
        dc,
        #PREFIX = pnPrefix,
        #TITLE = pnTitle,
        FILENAME=fs::path_file(s),
        ID=ID,
        SAMPLE=d_name,
        CONDITION=CON,
        LOCATION=LOC,
        DATETIME=DT,
        COUNT=count,
        IMPORT=rep(0,length(count)),
        PATH=sR,
        .keep = "none"),
        ID,
        .after = FILENAME)


      # bind dc to summary_dt
      summary_dt <- dplyr::bind_rows(summary_dt, dd)

      # and store the col_names from d to the COL_NAMES list
      if( length(COL_NAMES) == 0 ) { # if blank list add to first index
        # append FIRST col_names vector
        COL_NAMES[[1]] <- col_names
        # and set the name of this first entry to PREFIX plus SAMPLE
        names(COL_NAMES) <- paste0(sR, ":::", d_name)

      } else { # append col_names vector to END of COL_NAMES list
        COL_NAMES[[length(COL_NAMES)+1]] <- col_names
        # and set the name of this last entry to PREFIX plus SAMPLE (no +1 as COL_NAMES is now longer!)
        names(COL_NAMES)[length(COL_NAMES)] <- paste0(sR, ":::", d_name)
      }

      # and add project note path to paths
      if( length(paths) == 0 ) { # if blank list add to first index
        # append FIRST col_names vector
        paths[[1]] <- s
        # and set the name of this first entry to PREFIX plus SAMPLE
        names(paths) <- paste0(sR, ":::", d_name)

      } else { # append col_names vector to END of COL_NAMES list
        paths[[length(paths)+1]] <- s
        # and set the name of this last entry to PREFIX plus SAMPLE (no +1 as COL_NAMES is now longer!)
        names(paths)[length(paths)] <- paste0(sR, ":::", d_name)
      }

    }
  } # for d

  # return named list
  list(summary_dt=summary_dt, COL_NAMES=COL_NAMES, paths=paths)

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
datatable_load_data <- function( summary_dt ) {

}
