
#' Create Sample Tibble
#'
#' Create a new Tibble to store samples data in.  User must pass
#' in a character vector that contains each Sample ID: A unique
#' string to identify each sample.
#'
#' Assumes this function is run from inside RStudio R code chunk,
#' inside a Project Note from ProjectManagr:  The data is automatically
#' saved to the associated DATA_DIR of the projectmanagr Project Note -
#' defined as the Directory named with the PREFIX of the Project Note
#' (the string before the ~_ separator).
#'
#' If the DATA_DIR does not exist, this function will fail - therefore this
#' function will only work with ProjectManagr Project Notes (or Rmd files
#' with a similar naming convention and layout).
#'
#' The samples data is saved to CSV in the DATA_DIR using a standard filename,
#' "__samples.csv".  This method will fail if this file already exists in
#' the DATA_DIR
#'
#' @param ID A Character Vector that uniquely identifies each Sample.
#'
#' @param overwrite Set to TRUE to overwrite the contents of a previously saved samples CSV.
#'  Otherwise, this method will fail when trying to create new samples.  Can set to FALSE if
#'  failing to overwrite is required.
#'
#' @param writeToDisk Boolean to indicate whether the newly formed samples DataFrames is saved
#' to disk.  TRUE by default - set to FALSE to test run this function.
#'
#' @return A newly created and saved TIBBLE : saved to the Project Note DIR, and with the "path"
#' attribute set to '__samples.csv'.
#'
#' @export
sample_create <- function( ID, overwrite = TRUE, writeToDisk = TRUE ) {

  cat( "\nprojectmanagr::sample_create():\n" )

  # check ID is a character vector, and contains unique values
  if(is.character(ID) == FALSE) {
    ID <- as.character(ID)
    if(is.character(ID) == FALSE) {
      stop( paste0("  ID is not a character vector: ", ID) )
    }
  }

  if(length(ID) == 0) {
    stop( paste0("  ID is of length 0: ", ID) )
  }

  if( any( duplicated(ID) ) ) {
    stop( paste0("  ID contains duplicate values: ", ID[duplicated(ID)]) )
  }

  # get the currently active source editor context - hoepfully a projectmanagr Project Note!
  context <- rstudioapi::getSourceEditorContext()
  context$path <- path.expand(context$path) # expand "~" HOME

  # get/check project note DIR:
  DATA_DIR <- check_project_note_dir(context$path)

  DATA_DIR_ABS <- paste0( dirname(context$path), .Platform$file.sep, DATA_DIR)

  # define the samples csv file
  samples_filename <- "__samples.csv"
  samples_path <- paste0(DATA_DIR_ABS, .Platform$file.sep, samples_filename)

  if( file.exists(samples_path) == TRUE && overwrite == FALSE ) {
    stop( "  Sample file already exists: __samples.csv" )
  }

  # define the samples SUMMARY csv file
   # ACTUALLY OMITTING THIS - will just use the sample_find() function to search for summary info when needed
  #samples_summary_filename <- "__SUMMARY-samples.csv"
  #samples_summary_path <- paste0(DATA_DIR, .Platform$file.sep, samples_summary_filename)

  #if( file.exists(samples_summary_path) == TRUE && overwrite == FALSE ) {
  #  stop( "  Sample file already exists: __SUMMARY-samples.csv" )
  #}

  # Create samples tibble:
  samples <- tibble::tibble(ID)

  # Create samples SUMMARY tibble:
  #SAMPLE <- "" # String of all subsampling - no subsampling yet!
  #COUNT <- as.integer(1) # each sample is an individual unique entity - no reps/sections yet!
  #EXP <- basename( dirname(samples_path) ) # Experiment Prefix ID
  # Experiment Title - should follow convention: LAB_TREATMENT_INDEX
  #exp <- list.files( dirname( dirname(samples_path) ) )
  #exp <- exp[endsWith(exp,".Rmd")]
  #TITLE <- substr(exp, regexpr("~_", exp)+2, regexpr(".Rmd", exp)-1)
  #PATH <- dirname( samples_path ) # absolute path to samples file

  #samples_summary <- tibble::tibble(ID,SAMPLE,COUNT,EXP,TITLE,PATH )

  # Write samples and samples_summary to CSV:
  if(writeToDisk == TRUE) {
    readr::write_csv(samples, samples_path )
    cat( "\n  Written samples CSV:", samples_path, "\n\n" )

    #readr::write_csv(samples_summary, samples_summary_path )
    #cat( "\n  Written samples SUMMARY CSV:", samples_summary_path, "\n\n" )
  }

  # write filename to path attr:
  attr(samples, "path") <- samples_filename

  samples

}


#' Add Data to Samples
#'
#' This function receives a sample DataFrame (a Tibble which contains a 'path' attribute - this
#' is the name of the file in the Project Note's Data Dir), and a series of vectors (...).  After checking
#' the validity, each vector is inserted into the sample DataFrame and a new copy returned.
#'
#' - Each data vector must be supplied in the form 'col-name = data-vector'.
#'
#' By default, all samples in sample DataFrame have data added - the data-vector must be of length 1, or
#' of the length of all samples.
#'
#' However, subsets of IDs and replicates can be selected by setting the IDs and rep_select ARGs:
#'
#' - IDs - can be set to a Character Vector containing the subset of IDs to add data to  Any
#'   remaining IDs will have their data columns columns left BLANK.
#'
#' - rep_select - can be set to a List of numeric vectors to represent the selection of
#'   replicates to add data to.  Each numeric vector in the list represents the SELECTION
#'   INDICES for each ID (in order of IDs in the Data Frame if all IDs, otherwise using
#'   the IDs ordering from the IDs ARG above).
#'
#' To check the results of adding data, its possible to set the writeToDisk boolean to FALSE: in
#' this case the data additions are made to a copy of the original sampleDataFrame, and it is
#' returned for inspection.  Once the inputs have been checked, the actual process (with writing
#' of CSVs to Disk) can be performed with writeToDisk set to TRUE - its default value.
#'
#' The function is assumed to be run inside an R Chunk in an Rmd file that is a Project Note from the
#' projectmanagr package.  It assumes the document is saved to disk, it contains a PREFIX, separated from
#' the Note title with the '~_' separator, and that an adjacent Data DIR exists next to the Note Rmd file,
#' titled with the PREFIX - this is where the sampleDataFrame is assumed to exist, and where the new Data
#' Frames will be written.
#'
#' @param sampleDataFrame a tibble opened with sample_load, imported with sample_import, or created
#' with sample_create.  The tibble MUST have a 'path' attribute assigned to it, and this MUST point
#' to the tibbles location relative to the Project Note DIR.  Use sample_load() to load Single tibbles.
#'
#' #' @param ... one or more vectors to insert into the dataframe. This data must be supplied in the
#' form 'col-name = data-vector'. The function will fail if the col-name already exists.
#'
#' @param IDs Character vector (`c("MS01", "MS02")`) indicating which subset of IDs to add data to.
#' By default data is added to "all".  The function will fail if any of the selected IDs (and
#' replicates) have been exported or re-sampled (as they no longer exist!).
#'
#' @param rep_select List of numeric vectors (`list(c(1:3), c(2:4)`) to represent the selection
#'  of replicates from the sampleDataFrame. This vector is used to select SECTIONS from a
#'  sampleDataFrame which has previously been re-sampled with replicate subsamples (see rep_create
#'  in sample_resample function).  The list can always be of length 1, in which case the same
#'  replicates are selected for ALL IDs. Otherwise it must be the same length as ID from the
#'  sampleDataFrame, in which case each vector in the list selects a unique set of replicates
#'  from each ID in the sampleDataFrame; or if the IDs parameter has been set, a list the same
#'  length as IDs can be passed, with each vector in the list selecting a unique set of replicates
#'  from the set of IDs passed.  The function will fail if any of the selected replicates have been
#'  re-sampled or exported already. Uses the KEY WORD "all" to select all replicates by default.
#'
#'  @param overwrite Set to TRUE to overwrite the contents of a previous data column with new input.
#'  Otherwise, this method will fail when trying to add the same data column(s) again.
#'
#' @param writeToDisk Boolean to indicate whether the edits made to sampleDataFrame and the newly formed
#' DataFrames are saved to disk.  TRUE by default - set to FALSE to test run this function.
#'
#' @return A newly created and saved TIBBLE : saved to the Project Note DIR, showing the new
#' data associated with the Sample data.
#'
#' @export
sample_add_data <- function( sampleDataFrame, ..., IDs = "all", rep_select = "all",
                              overwrite = FALSE, writeToDisk = TRUE ) {

  cat( "\nprojectmanagr::sample_add_data():\n" )

  # get the currently active source editor context - hopefully a projectmanagr Project Note!
  context <- rstudioapi::getSourceEditorContext()
  context$path <- path.expand(context$path) # expand "~" HOME

  # get/check project note DIR:
  DATA_DIR <- check_project_note_dir(context$path)

  # check sampleDataFrame:
  check_sample_data_frame(sampleDataFrame, DATA_DIR)

  # get the filename from the sampleDataFrame list:
  filename <- attr(sampleDataFrame, "path")

  # COPY samples tibble and its row count:
  samples <- sampleDataFrame
  rows <- nrow(samples)
  colNames <- names(samples)

  # create new summary table - with each ID listing how many REPLICATES/sections it has:
  # plus add the cumulative sum of these in a second column:
  samples_ID <- dplyr::group_by(samples, ID)
  section_nums <- dplyr::summarise(samples_ID, sections = dplyr::n() )
   # make sure to re-arrange by ORIGINAL order of ID in samples!
    # BEFORE computing the cumulative sum!
  section_nums <- dplyr::arrange( section_nums, order(match( unique(samples$ID), section_nums$ID)) )
  section_nums <- dplyr::mutate(section_nums, sections_cum = cumsum(sections) )

  # Check IDs:
  IDs <- check_IDs(IDs, samples$ID)
    # The IDs value is now a vector containing the sample IDs to process!

  # Check rep_select:
  rep_select <- check_rep_select(rep_select, IDs, section_nums)
    # rep_select is now set to a list of vectors to process

  # Check if IDs/rep_select have been exported or resampled:
   # CANNOT ADD DATA TO SAMPLES THAT DO NOT EXIST (have been resampled or exported!)
  check_valid_selection(samples, IDs, rep_select, section_nums)

  # COLLECT DATA TO ADD:

  # create a named list of the argument values:
  varList <- list(...)
  varNames <- names(varList)

  # Check varNames:
  check_vars(varList, varNames, colNames, IDs, rep_select, section_nums, overwrite)
    # checks varList and varNames are valid RELATIVE TO Selected IDs and REPS!

  # Create a BLANK COLUMNS first in samples:
  for(a in 1:length(varList) ) {

    # add appropriate column type - initialise with NA:
    if( lubridate::is.POSIXct(varList[[a]]) == TRUE ) {
      newCol <- as.POSIXct(NA)
    } else if( lubridate::is.Date(varList[[a]]) == TRUE  ) {
      newCol <- as.Date(NA)
    }else if( typeof(varList[[a]]) == "double" ) {
      newCol <- NA_real_
    } else if( typeof(varList[[a]]) == "integer" ) {
      newCol <- NA_integer_
    } else if( typeof(varList[[a]]) == "character" ) {
      newCol <- NA_character_
    } else if( typeof(varList[[a]]) == "logical" ) {
      newCol <- NA
    } else {
      stop(
        paste0("  Arg type not recognised (not date, double, integer, character, logical: ", varList[[a]] ) )
    }

    if( ! is.element(varNames[a], colNames) ) { # ONLY add the new column if it doesnt exist already!
      samples <- tibble::add_column( samples, newCol )
      names(samples)[names(samples) == "newCol"] <- varNames[a] # and SET THE NAME IMMEDIATELY!
    }

    # for each ID in IDs, add data!
    for( i in 1:length(IDs) ) {

      # get the number of sections, and cumulative sections:
      section_num <- section_nums$sections[ section_nums$ID == IDs[i] ]
      section_last <- section_nums$sections_cum[ section_nums$ID == IDs[i] ]
      section_first <- section_last - (section_num - 1) # -1 as the first section is ON section_first

      # get the current rep_select vector - check all selected replicates are VALID and AVAILABLE:
      reps <- rep_select[[i]]

      # convert reps into indices:
      rep_indices <- (reps - 1) + section_first # -1 as the first section is ON section_first

      # Add values for each rep index:
      for( j in rep_indices) {

        if( length(varList[[a]]) == 1 ) { # if varList data is length 1, just add it as it is

          samples[[varNames[a]]][j] <- varList[[a]]

        } else if( length(varList[[a]]) == length(IDs) ) { # so add by IDs index

          samples[[varNames[a]]][j] <- varList[[a]][i]

        } else { # varList must be of length [lengths(rep_select)] - so add by rep_indices

          samples[[varNames[a]]][j] <- varList[[a]][j]

        }
      }
    }
  }

  if( writeToDisk == TRUE ) {
    # Write the samples Tibble to CSV:
    readr::write_csv(samples, paste0(DATA_DIR, .Platform$file.sep, filename) )
    cat( "\n  Written samples CSV:", paste0(DATA_DIR, .Platform$file.sep, filename), "\n\n" )
  }

  # ADD path attribute
  attr(samples, "path") <- filename
  # access with: attr(samples[[1]], "path")

  # return the tibble
  samples

}


#' Add Data to Samples by Group Values
#'
#' This function receives a sample DataFrame (a Tibble which contains a 'path' attribute - this
#' is the name of the file in the Project Note's Data Dir), and a series of vectors (...).  After checking
#' the validity, each vector is inserted into the sample DataFrame and a new copy returned.
#'
#' - Each data vector must be supplied in the form 'col-name = data-vector'.
#'
#' This function supports the addition of data by group:
#'
#' - `by_group` must be set to a character vector of length TWO: the group column name and group column
#' value, respectively.
#'
#' - Data is then added to the columns only where group_col_name's value is group_col_value.
#'
#' To check the results of adding data, its possible to set the writeToDisk boolean to FALSE: in
#' this case the data additions are made to a copy of the original sampleDataFrame, and it is
#' returned for inspection.  Once the inputs have been checked, the actual process (with writing
#' of CSVs to Disk) can be performed with writeToDisk set to TRUE - its default value.
#'
#' The function is assumed to be run inside an R Chunk in an Rmd file that is a Project Note from the
#' projectmanagr package.  It assumes the document is saved to disk, it contains a PREFIX, separated from
#' the Note title with the '~_' separator, and that an adjacent Data DIR exists next to the Note Rmd file,
#' titled with the PREFIX - this is where the sampleDataFrame is assumed to exist, and where the new Data
#' Frames will be written.
#'
#' @param sampleDataFrame a tibble opened with sample_load or created with sample_create.  The tibble MUST
#' have a 'path' attribute assigned to it, and this MUST point to the tibbles location relative to the
#' Project Note DIR.  Use sample_load() to load Single tibbles.
#'
#' #' @param ... one or more vectors to insert into the dataframe. This data must be supplied in the
#' form 'name = vector'.
#'
#' @param by_group Character vector of length 2 that specifies the group column name followed by
#' the group character ID/value.
#'
#'  @param overwrite Set to TRUE to overwrite the contents of a previous data column with new input.
#'  Otherwise, this method will fail when trying to add the same data column(s) again.
#'
#' @param writeToDisk Boolean to indicate whether the edits made to sampleDataFrame and the newly formed
#' DataFrames are saved to disk.  TRUE by default - set to FALSE to test run this function.
#'
#' @return A newly created and saved TIBBLE : saved to the Project Note DIR, showing the new
#' data associated with the Sample data.
#'
#' @export
sample_add_data_by_group <- function( sampleDataFrame, ..., by_group = c("group_col_name", "group_col_value"),
                                       overwrite = FALSE, writeToDisk = TRUE ) {

  cat( "\nprojectmanagr::sample_add_data_by_group():\n" )

  if( by_group[1] == "group_col_name" ) {
    stop(
      paste0( "  You have not specified by_group",
              " - if not adding data by group, use sample_add_data() function!" )  )
  }

  # get the currently active source editor context - should be a projectmanagr Project Note!
  context <- rstudioapi::getSourceEditorContext()
  context$path <- path.expand(context$path) # expand "~" HOME

  # get/check project note DIR:
  DATA_DIR <- check_project_note_dir(context$path)

  # check sampleDataFrame:
  check_sample_data_frame(sampleDataFrame, DATA_DIR)

  # get the filename from the sampleDataFrame list:
  filename <- attr(sampleDataFrame, "path")

  # COPY samples tibble and its row count:
  samples <- sampleDataFrame
  rows <- nrow(samples)
  colNames <- names(samples)

  # Check by_group:
  check_group(by_group, samples)

  # COLLECT DATA TO ADD:
   # create a named list of the argument values:
  varList <- list(...)
  varNames <- names(varList)

  # Check varNames:
  check_vars_group(varList, varNames, colNames, by_group, overwrite)
   # checks varList and varNames are valid RELATIVE TO group selection and overwrite bool

  for(a in 1:length(varList) ) {

    # add appropriate column type - initialise with NA:
    if( lubridate::is.POSIXct(varList[[a]]) == TRUE ) {
      newCol <- as.POSIXct(NA)
    } else if( lubridate::is.Date(varList[[a]]) == TRUE  ) {
      newCol <- as.Date(NA)
    }else if( typeof(varList[[a]]) == "double" ) {
      newCol <- NA_real_
    } else if( typeof(varList[[a]]) == "integer" ) {
      newCol <- NA_integer_
    } else if( typeof(varList[[a]]) == "character" ) {
      newCol <- NA_character_
    } else if( typeof(varList[[a]]) == "logical" ) {
      newCol <- NA
    } else {
      stop(
        paste0("  Arg type not recognised (not date, double, integer, character, logical: ", varList[[a]] ) )
    }

    if( ! is.element(varNames[a], colNames) ) { # ONLY add the new column if it doesnt exist already!
      samples <- tibble::add_column( samples, newCol )
      names(samples)[names(samples) == "newCol"] <- varNames[a] # and SET THE NAME IMMEDIATELY!
    }

    # for each row in samples, add data...
    for( j in 1:length(samples[[varNames[a]]]) ) {

      if( samples[[ by_group[1] ]][j] == by_group[2] ) {
        # if by_group column equals by_group value, add varList val to varNames col:
        samples[[varNames[a]]][j] <- varList[[a]]
      }
    }
  }

  if( writeToDisk == TRUE ) {
    # Write the samples Tibble to CSV:
    readr::write_csv(samples, paste0(DATA_DIR, .Platform$file.sep, filename) )
    cat( "\n  Written samples CSV:", paste0(DATA_DIR, .Platform$file.sep, filename), "\n\n" )
  }

  # RE-ADD path attribute
  attr(samples, "path") <- filename
  # access with: attr(samples[[1]], "path")

  # return the tibble
  samples

}


#' Re-Sample Sample Data
#'
#' This function receives a `sampleDataFrame` (a Tibble which contains a 'path' attribute - this
#' is the name of the file in the Project Note's Data Dir), a `resample_vector` that contains each
#' new sub-sample, and a `resample_dt` that contains the datetime of resampling.
#'
#' The sample DataFrame is re-sampled:
#'
#' - new columns to indicate the resampling are created (`resample`, `resample_dt`), and filled (with
#'   a space-separated String of the `resample_vector` and the `resample_dt`, respectively).
#'
#' By default, all samples in `sampleDataFrame` will be resampled. However, subsets of IDs and replicates
#' can be selected by setting the IDs and rep_select ARGs:
#'
#' - `IDs` - set to a Character Vector containing a subset of IDs to apply the re-sampling to.
#'   Any remaining IDs will have their resample/resample_dt columns left BLANK.
#'
#' - `rep_select` - can be set to a List of Integer/numeric vectors to select replicates for re-sampling.
#'   Each vector in the list represents each ID (in order of IDs in the Data Frame if all IDs, otherwise
#'   using the IDs ordering from the IDs ARG).
#'
#' The `resample_vector` should contain a set of user-defined LABELS for each new subsample.  For example,
#' if a mouse brain is re-sampled, one may end up with a series of individually identifiable
#' tissue blocks, which could be labelled: LT-FB, RT-FB, LT-MB, RT-MB, etc.  A space-separated
#' list of these labels will be placed in the `resample` column of the `sampleDataFrame`.
#'
#' When performing the resampling, it is possible to create REPLICATES for each new component, by setting
#' the `rep_create` ARG:
#'
#' - `rep_create` - set to a numeric vector containing the number of replicates of EACH SAMPLE during the
#'   resampling. This is used to encode the SECTIONING of a Sample into identical (same thickness) sections.
#'
#' For example, a `CNS_LT-FB` block of all samples could be sectioned into 350um thick sections: this would
#' be represented by performing a re-sampling, setting `resample_vector` to c("350um"), and `rep_create`
#' to a numeric vector - each element containing the number of sections for each ID (following the order
#' in the original `sampleDataFrame`).
#'
#' For each item in the `resample_vector`, a NEW DATA FRAME is created: Each new dataframe will be filled
#' with the `ID`, any previous subsampling columns, and a new subsampling column (`subsample_INDEX`) that
#' contains the new subsample labelling code (i.e. LT-FB, RT-FB, LT-MB, RT-MB, etc).  Only samples selected
#' with `IDs` and `rep_select` will be moved to the new subsample data frames.  When `rep_create` is
#' used to create replicates, a `subsample_INDEX_rep` column will be generated in the new DataFrame(s),
#' filled with the replicate number (i.e. a series of integers: 1, 2, 3 etc.).  Each new row now represents
#' the new replicate of the subsample.
#'
#' To check the results of a given resampling, its possible to set the writeToDisk boolean to FALSE: in this
#' case the computations are performed, the edits made to a copy of the original sampleDataFrame, and copies
#' of the old and new DataFrames are all returned for inspection.  Once the inputs are known to be correct,
#' the actual resampling (with writing of CSVs to Disk) can be performed with writeToDisk set to TRUE - its
#' default value.
#'
#' The function is assumed to be run inside an R Chunk in an Rmd file that is a Project Note from the
#' projectmanagr package.  It assumes the document is saved to disk, it contains a PREFIX, separated from
#' the Note title with the '~_' separator, and that an adjacent Data DIR exists next to the Note Rmd file,
#' titled with the PREFIX - this is where the `sampleDataFrame` is assumed to exist, and where the new Data
#' Frames will be written.
#'
#' @param sampleDataFrame a tibble opened with sample_load or created with sample_create.  The tibble MUST
#' have a 'path' attribute assigned to it, and this MUST point to the tibbles location relative to the
#' Project Note DIR.  Use sample_load() to load Single tibble for resampling.
#'
#' @param resample_vector Character vector containing identifiers for the new Samples.  Use '-' and NOT
#' '_' to separate components within a single identifier.
#'
#' @param resample_dt a Datetime of the re-sampling event, can be a single string (for all IDs), or a vector
#' with a new datetime for each ID.
#'
#' @param IDs Character vector indicating which subset of IDs to apply the re-sampling to. By default
#'  the re-sampling is applied to "all".  The function will fail if any of the selected IDs (and
#'  replicates) have been re-sampled or exported already.
#'
#' @param rep_select List of integer/numeric vectors to represent the selection of replicates from the
#'  sampleDataFrame. This vector is used to select SECTIONS from a sampleDataFrame which has previously
#'  been re-sampled with replicate subsamples (see rep_create below).  The list can always be of length 1,
#'  in which case the same replicates are selected for ALL IDs. Otherwise it must be the same length as
#'  ID from the sampleDataFrame, in which case each vector in the list selects a unique set of replicates
#'  from each ID in the sampleDataFrame; or if the IDs parameter has been set, a list the same length
#'  as IDs can be passed, with each vector in the list selecting a unique set of replicates from the
#'  set of IDs passed.  The function will fail if any of the selected replicates have been re-sampled or
#'  exported already. Uses the KEY WORD "all" to select all replicates by default.
#'
#' @param rep_create Integer vector representing how many replicates of each subsample are to be created.
#'   Used to denote SECTIONING.  Set to 1 by default - in which case NO resample_rep column is create. The
#'   vector can always be of length 1, in which case the same replicates are created for ALL IDs. Otherwise
#'   it must be the same length as ID from the sampleDataFrame, in which case each value in the vector
#'   represented the unique number of replicates created; or if the IDs parameter has been set, a vector
#'   the same length as IDs could be passed, with each value in the vector representing the unique number
#'   of replicates created for the set of IDs passed.
#'
#' @param writeToDisk Boolean to indicate whether the edits made to sampleDataFrame and the newly formed
#' DataFrames are saved to disk.  TRUE by default - set to FALSE to test run the re-sampling.
#'
#' @return A newly created and saved list of TIBBLES : one for each value in resample_vector.
#'
#' @export
sample_resample <- function( sampleDataFrame, resample_vector, resample_dt,
                              IDs = "all", rep_select = "all", rep_create = c(1),
                               overwrite = FALSE, writeToDisk = TRUE ) {

  cat( "\nprojectmanagr::sample_resample():\n" )

  # get the currently active source editor context - hoepfully a projectmanagr Project Note!
  context <- rstudioapi::getSourceEditorContext()
  context$path <- path.expand(context$path) # expand "~" HOME
  #orgpath <- findOrgDir(context$path)

  # get/check project note DIR:
  DATA_DIR <- check_project_note_dir(context$path)

  # check sampleDataFrame:
  check_sample_data_frame(sampleDataFrame, DATA_DIR)

  # get the filename from the sampleDataFrame list:
  filename <- attr(sampleDataFrame, "path")

  # COPY samples tibble and its row count:
  samples <- sampleDataFrame
  rows <- nrow(samples)
  colNames <- names(samples)

  # create new summary table - with each ID listing how many REPLICATES/sections it has:
   # plus add the cumulative sum of these in a second column:
  samples_ID <- dplyr::group_by(samples, ID)
  section_nums <- dplyr::summarise(samples_ID, sections = dplyr::n() )
   # make sure to re-arrange by ORIGINAL order of ID in samples!
    # BEFORE computing the cumulative sum!
  section_nums <- dplyr::arrange( section_nums, order(match( unique(samples$ID), section_nums$ID)) )
  section_nums <- dplyr::mutate(section_nums, sections_cum = cumsum(sections) )

  # Check IDs:
  IDs <- check_IDs(IDs, samples$ID)
    # The IDs value is now a vector containing the sample IDs to process!

  # Check rep_select:
  rep_select <- check_rep_select(rep_select, IDs, section_nums)
    # rep_select is now set to a list of vectors to process

  # Check rep_create:
  check_rep_create(rep_create, IDs)

  # check if IDs/rep_select have been exported or resampled:
    # CANNOT ADD DATA TO SAMPLES THAT DO NOT EXIST (have been resampled or exported!)
  check_valid_selection(samples, IDs, rep_select, section_nums)

  # Check resample_vector - convert to space-separated string:
  resample_vector_string <- check_resample_vector(resample_vector)

  # Check resample_dt is a datetiem obj, check the number of dt objects is valid
    # the NUMBER of dt objects should be 1, length if IDs, or length of sum of rep_select lengths
  resample_dt_vector <- check_resample_dt(resample_dt)

  # get all col names that start with subsample_ and import_
    # this PRESERVES THE COLUMN ORDERING!
  subsample_import_col_names <- colNames[startsWith(colNames, "subsample_") | startsWith(colNames, "import_") ]

  # now select the subsamples and imports column - PRESERVING THE ORDER
  subsamples_imports <- dplyr::select(samples, subsample_import_col_names)

  # get all subsamples columns in new tibble:
  subsamples <- dplyr::select(samples, dplyr::starts_with("subsample_"))

  # get all imports columns in new tibble:
  imports <- dplyr::select(samples, dplyr::starts_with("import_"))

  # computed if the CURRENT samples DataFrame already has export or resample columns:
  resample_col <- FALSE
  export_col <- FALSE
  if( any(colNames == "export") == TRUE ) {
    export_col = TRUE
  }
  if( any(colNames == "resample") == TRUE ) {
    resample_col = TRUE
  }

  # Create the resample and resample_dt columns:
    # if resample column doesnt exist, create it and fill it with BLANK values:
  resample <- ""

  if( resample_col == FALSE ) {
    # add resample column, fill with BLANK, if it doesnt exits
    samples <- tibble::add_column(samples, resample)
  }

  # DITTO resample_dt - fill with NA datetime:
  resample_dt <- as.POSIXct(NA) # use NA instead of BLANK

  if( any(colNames == "resample_dt") == FALSE ) {
    # add resample_dt column, fill with BLANK, if it doesnt exits
    samples <- tibble::add_column(samples, resample_dt)
  }

  # count each rep - in case need to index resample_dt for EACH REP!
  rep_count <- 1

  # for each ID in IDs, Set the relevant indices from rep_select to
    # resample_vector_string resample_dt_vector
  for( i in 1:length(IDs) ) {

    # get the number of sections, and cumulative sections:
    section_num <- section_nums$sections[ section_nums$ID == IDs[i] ]
    section_last <- section_nums$sections_cum[ section_nums$ID == IDs[i] ]
    section_first <- section_last - (section_num - 1) # -1 as the first section is ON section_first

    # get the current rep_select vector - check all selected replicates are VALID and AVAILABLE:
      # NB if rep_select was all, its now a vector containing section_num vector!
    reps <- rep_select[[i]]

    if( all(reps <= section_num) == FALSE) {
      # not enough sections to select reps!
      stop( paste0("  Not enough replicates in ID: ", IDs[i], " Replicate number: ",  section_num) )
    }

    # convert reps into indices:
    rep_indices <- (reps - 1) + section_first # -1 as the first section is ON section_first

    # check each index in resample and resample_dt - if BLANK and NA, then add the values
    for( j in rep_indices) {

      if( resample_col && (samples$resample[j] != "") ) {
        stop( paste0("  Selected sample has already been re-sampled: ", IDs[i], " index: ",  reps[j] ) )
      }

      if( export_col && (samples$export[j] != "") ) {
        stop( paste0("  Selected sample has already been exported: ", IDs[i], " index: ",  reps[j] ) )
      }

      samples$resample[j] <- resample_vector_string
      cat( "\n  Re-Sampled: ", IDs[i], " index: ",  reps[j], "\n\n" )

      if( is.na(samples$resample_dt[j]) == FALSE  ) {
        stop( paste0("  Selected sample has already been re-sampled: ", IDs[i], " index: ",  reps[j] ) )
      }

      if( length(resample_dt_vector) == 1 ) {

        # only one date supplied, use this for all rows
        samples$resample_dt[j] <- resample_dt_vector

      } else if( length(resample_dt_vector) == length(IDs) ) {

        # one date for each ID, use ID index
        samples$resample_dt[j] <- resample_dt_vector[i]

      } else if( length(resample_dt_vector) == sum( lengths(rep_select) )   ) {

        # one date for each item in rep_select, so use rep_count (index starting from 1, which
         # increments by one with each rep/ID step) (NB: rep_indices contains the specific location
          # in the DF for the row to write to, and is not necessarily a sequence from 1!
        samples$resample_dt[j] <- resample_dt_vector[rep_count]

      } else {

        stop( paste0("  resample_dt is not of correct length to add to dataframe: ", resample_dt_vector) )

      }
      # increment rep_count
      rep_count <- rep_count + 1

    }

  }

  # The original sample DataFrame has now been filled correctly with re-sampling:
    # SAVE:
  if( writeToDisk == TRUE ) {
    # Write the samples Tibble to CSV:
    readr::write_csv(samples, paste0(DATA_DIR, .Platform$file.sep, filename) )
    cat( "\n  Written samples CSV:", paste0(DATA_DIR, .Platform$file.sep, filename), "\n\n" )
  }

  ### CREATE SUB-SAMPLES ###

  # process rep_create:
   # if rep_create is bigger than one in LENGTH OR VALUE, set create_rep to TRUE
    # this will ensure the subsample_INDEX_rep column is made when needed
  if(rep_create == 1 && length(rep_create) == 1) {
    create_rep <- FALSE
  } else {
    create_rep <- TRUE
  }
  # if length 1, rep() to length of IDs
  # use value in rep_create at index for ID to replicate the rows in new subsample DataFrame
  if( length(rep_create) == 1 ) {
    rep_create <- rep( rep_create, length(IDs) )
  }

  # generate the tibble names from current sampleDataFrame filename, and resample_vector names:
   # USE "_" to separate filename from resample_vector!
  newDataFrameNames <- paste0( substr(filename, 1, nchar(filename)-4), "_", resample_vector, ".csv" )

  # generate blank list to add dataFrame-lists to:
  samplesDataFrame <- list()

  # Add the ORIGINAL sampleDataFrame to this new list - so the user can check it after writing:
  samplesDataFrame[[ filename ]] <- samples

  # get ID col:
  ID <- samples$ID

  # get each subsample column from samples:
  subsamples <- dplyr::select(samples, dplyr::starts_with("subsample_"))
  # this returns a TIBBLE

  # compute the number of UNIQUE subsample columns:
  subsample_col_names <- names(subsamples) # get the character vector of subsample col names
  # remove each item that ENDS WITH _rep (this will be a SECOND column associated with ONE resampling)
  subsample_col_names_unique <- subsample_col_names[!endsWith(subsample_col_names, "_rep")]
  # the length of subsample_col_names_unique is the number of UNIQUE subsamplings
  subsample_index = length(subsample_col_names_unique) + 1 # get NEXT index

  # Strings used as placeholders for initial name of new columns
  arg <- "new_subsample"
  arg_rep <- "new_subsample_rep"


  # For each newDataFrameNames value, create a new tibble, add to a list, and add to samplesDataFrame:

  for(i in 1:length(newDataFrameNames) ) {

    new_subsample <- resample_vector[i]
    # each tibble will fill subsample_INDEX with appropriate resample_vector value
    new_subsample_rep <- 0 # initially fill _rep with INTEGER

    # Create a tibble:
    if(create_rep == FALSE) { # if no _rep col needed:
      subsample <- tibble::tibble(ID, subsamples_imports, new_subsample)
    } else { # if adding _rep col:
      subsample <- tibble::tibble(ID, subsamples_imports, new_subsample, new_subsample_rep)
    }

    # rename new_subsample with "subsample_INDEX":
    if(subsample_index == 1 ) {
      new_subsample_name <- "subsample_01"
      new_subsample_rep_name <- "subsample_01_rep"
    } else if(subsample_index < 10) {
      new_subsample_name <- paste0("subsample_0", subsample_index)
      # get _rep name too:
      new_subsample_rep_name <- paste0("subsample_0", subsample_index, "_rep")
    } else {
      new_subsample_name <- paste0("subsample_", subsample_index)
      # get _rep name too:
      new_subsample_rep_name <- paste0("subsample_", subsample_index, "_rep")
    }

    names(subsample)[names(subsample) == arg] <- new_subsample_name

    # edit _rep col name, if being added:
    if(create_rep == TRUE) {
      names(subsample)[names(subsample) == arg_rep] <- new_subsample_rep_name
    }

    # generate blank subsample tibble with correct columns, will fill this with cols from subsample
    subsample_blank <- dplyr::filter(subsample, FALSE)


    # Use section_nums DataFrame from before for number of replicates, first/last indices:
    # access ID and sections with:  section_nums$ID section_nums$sections

    # for each ID in IDs, copy the rows from indices in rep_select to
    # subsample_blank
    for( a in 1:length(IDs) ) {

      # get the number of sections, and cumulative sections:
      section_num <- section_nums$sections[ section_nums$ID == IDs[a] ]
      section_last <- section_nums$sections_cum[ section_nums$ID == IDs[a] ]
      section_first <- section_last - (section_num - 1) # -1 as the first section is ON section_first

      # get the current rep_select vector
      # NB if rep_select was 'all', its now a vector containing section_num vector!
      reps <- rep_select[[a]]

      # convert reps into indices:
      rep_indices <- (reps - 1) + section_first # -1 as the first section is ON section_first

      # copy each index in subsample to subsample_blank data frame:
      for( b in rep_indices) {

        for( c in 1:rep_create[a] ) { # duplicate rows by rep_create - represents the REPLICATION

          if(create_rep == TRUE) {
            # if are creating reps, need to set _rep column to c:
            # this will produce an increment in the _rep column - 1, 2, 3 etc
            subsample[b,length(subsample)] <- c
          }

          # now copy selected row to subsample_blank:
          subsample_blank <- dplyr::bind_rows(subsample_blank, subsample[b, ])

        }
      }
    }


    # Save the new tibble:

    # Write to CSV:
    readr::write_csv(subsample_blank, paste0(DATA_DIR, .Platform$file.sep, newDataFrameNames[i]) )
    cat( "\n  Written subsample CSV:", paste0(DATA_DIR, .Platform$file.sep, newDataFrameNames[i]), "\n\n" )

    # add 'path' attribute to subsample_blank
    attr(subsample_blank, "path") <- newDataFrameNames[ (i) ]

    # Add sampleDataFrame to the samplesDataFrame list:
    samplesDataFrame[[ newDataFrameNames[ (i) ] ]] <- subsample_blank

  }


  ### RE-WRITE SUMMARY CSV ###
   # omtting this for now - just use sample_find() to get summary info!

  # read summary samples:
  #summary_samples <- readr::read_csv( paste0(DATA_DIR, .Platform$file.sep, "__SUMMARY-samples.csv"),
   #                           col_types = readr::cols( ID = readr::col_character() )  )

  # define the DATA_DIR relative to the orgpath:
  #DATA_DIR_PATH <- paste0( dirname(context$path) , .Platform$file.sep, DATA_DIR )
  #DATA_DIR_PATH_REL_ORG <- substr(DATA_DIR_PATH, nchar(orgpath)+2, nchar(DATA_DIR_PATH))

  # use sample_find function to return the new summary_samples
  #summary_samples <- sample_find( DATA_DIR_PATH_REL_ORG )

  # write the new summary_samples DF:
  #readr::write_csv(summary_samples,
  #                  paste0(DATA_DIR, .Platform$file.sep, "__SUMMARY-samples.csv"))

  # Return the Named List containing the Tibbles:
  samplesDataFrame

}




#' Find Samples Recursively inside a Directory from Rmd
#'
#' This will identify all samples CSVs inside a Directory recursively, presenting
#' a summary Tibble containing each EXISTING sample - data including:
#'
#' * ID - sample ID
#' * SAMPLE: Composite of all subsampling
#' * COUNT: Number of reps of the sample (sections)
#' * EXP: The experiment PREFIX ID
#' * TITLE: Fill with Experiment Title - will contain the LAB_TREATMENT
#' * PATH: The absolute path to the Expeirment DIR
#'
#' @param subProgDir a directory that is inside a Programme, which will be searched recursively
#' to find all files that contain the string glob `__sample*`
#'
#' @export
sample_find <- function(subProgDir) {

  cat( "\nprojectmanagr::sample_find():\n" )

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
  EXP <- ""     # EXP: Fill with the Experiment Prefix ID
  TITLE <- ""   # TITLE: Fill with Experiment Title - will contain the LAB_TREATMENT
  PATH <- ""    # PATH: Put the absolute PATH to the Project Note Rmd to Navigate to

  samples_summary <- tibble::tibble(ID, SAMPLE, COUNT, EXP, TITLE, PATH)

  for(s in samplesList) {

    # read CSV
    t <- readr::read_csv(s, col_types = readr::cols( ID = readr::col_character()) )

    # trim the tibble to contain only ID, subsample, resample, export columns:
    #  -dplyr::ends_with("_rep"),
    t <- dplyr::select(
      t,
      ID,
      dplyr::starts_with("subsample"),
      dplyr::matches("resample"), -dplyr::matches("resample_dt"),
      dplyr::matches("export"), -dplyr::matches("export_dt") )

    # get all colnames
    cols <- colnames(t)

    # remove all rows with resample col that != NA
    if( any(cols == "resample") ) {
      t <- dplyr::filter(t, is.na(resample)) # keep any which are NA - they STILL EXIST!
      # note although written as "" blank, when saved and loaded by read_csv, blank cols become NA
    }

    # remove all rows with export col that != NA
    if( any(cols == "export") ) {
      t <- dplyr::filter(t, is.na(export)) # keep any which are NA - they STILL EXIST!
      # note although written as "" blank, when saved and loaded by read_csv, blank cols become NA
    }

    # remove _rep cols, replace with count:
    #if( any(endsWith(cols, "_rep")) ) {
    colsnorep <- cols[!endsWith(cols, "_rep")] # want to group by all EXCEPT _rep
    tg <- dplyr::group_by_at(t, colsnorep )
    t <- dplyr::summarise(tg, count=dplyr::n() )
    t <- dplyr::ungroup(t)
    #}

    cols <- colnames(t)
    subscols <- cols[grepl("subsample", cols)]
    # combine any subsample cols if they exist:
    if(length(subscols) == 0) {
      t <- dplyr::mutate(t, subsample="")
    } else if(length(subscols) == 1) {
      t <- dplyr::rename(t, subsample=subsample_01)
    } else if(length(subscols) > 1) {
      t <- tidyr::unite(t, "subsample", subscols)
    }
    # any subsample cols are combined into one col - subsample

    exp <- list.files( dirname(dirname(s)))
    exp <- exp[endsWith(exp,".Rmd")]
    exp <- substr(exp, regexpr("~_", exp)+2, regexpr(".Rmd", exp)-1)

    # edit t - mutate to form ID, SAMPLE, COUNT, EXP
    t <- dplyr::transmute(
      t,
      ID=ID,
      SAMPLE=subsample,
      COUNT=count,
      EXP=basename( dirname(s) ),
      TITLE=exp,
      PATH = dirname( s ) )

    # bind samples_summary with t:
    samples_summary <- dplyr::bind_rows(samples_summary, t)
  }

  # return the samples_summary
  samples_summary

}




#' Find Samples Recursively inside a Directory from CSVs
#'
#' This will identify all samples CSVs inside a Directory recursively, presenting
#' a summary Tibble containing each EXISTING sample - data including:
#'
#' * ID - sample ID
#' * SAMPLE: Composite of all subsampling
#' * COUNT: Number of reps of the sample (sections)
#' * EXP: The experiment PREFIX ID
#' * TITLE: Fill with Experiment Title - will contain the LAB_TREATMENT
#' * PATH: The absolute path to the Expeirment DIR
#'
#' @param subProgDir a directory that is inside a Programme, which will be searched recursively
#' to find all files that contain the string glob `__sample*`
#'
#' @export
sample_find_csv <- function(subProgDir) {

  cat( "\nprojectmanagr::sample_find_csv():\n" )

  context <- rstudioapi::getSourceEditorContext()
  context$path <- path.expand(context$path) # expand "~" HOME

  # get all files that begin with __samples - these are the files to process:
  samplesList <- list.files(subProgDir, pattern="__samples*",
                            full.names = TRUE, recursive=TRUE)

  if( length(samplesList) == 0 ) {
    # check if the subProgDir is defined from the orgDir:
    subProgDir <- paste0(findOrgDir(context$path), .Platform$file.sep, subProgDir )

    # get all files that begin with __samples - these are the files to process:
    samplesList <- list.files(subProgDir, pattern="__samples*",
                              full.names = TRUE, recursive=TRUE)
  }

  # check subProgDir - make sure its in a org DIR
  subProgDir <- checkProgSubDir(subProgDir)

  # only keep files which have "__samples" at the start of their name:
  samplesList <- samplesList[ startsWith(basename(samplesList), "__samples") ]

  # define a new Summary tibble to hold all samples:
   # MUST define a STANDARD TEMPLATE to hold SUMMARY DATA on samples
    # From this summary information, should be possible to select samples, or further explore them
  ID <- ""      # ID: Each Sample ID
  SAMPLE <- ""  # SAMPLE: COMPOSITE of all subsampling columns: CNS-RT-MB etc.
  COUNT <- integer()  # COUNT: How many REPS are there of this sample?
  EXP <- ""     # EXP: Fill with the Experiment Prefix ID
  TITLE <- ""   # TITLE: Fill with Experiment Title - will contain the LAB_TREATMENT
  PATH <- ""    # PATH: Put the absolute PATH to the Project Note Rmd to Navigate to

  samples_summary <- tibble::tibble(ID, SAMPLE, COUNT, EXP, TITLE, PATH)

  for(s in samplesList) {

    # read CSV
    t <- readr::read_csv(s, col_types = readr::cols( ID = readr::col_character()) )

    # trim the tibble to contain only ID, subsample, resample, export columns:
      #  -dplyr::ends_with("_rep"),
    t <- dplyr::select(
        t,
        ID,
        dplyr::starts_with("subsample"),
        dplyr::matches("resample"), -dplyr::matches("resample_dt"),
        dplyr::matches("export"), -dplyr::matches("export_dt") )

    # get all colnames
    cols <- colnames(t)

    # remove all rows with resample col that != NA
    if( any(cols == "resample") ) {
        t <- dplyr::filter(t, is.na(resample)) # keep any which are NA - they STILL EXIST!
        # note although written as "" blank, when saved and loaded by read_csv, blank cols become NA
    }

    # remove all rows with export col that != NA
    if( any(cols == "export") ) {
        t <- dplyr::filter(t, is.na(export)) # keep any which are NA - they STILL EXIST!
        # note although written as "" blank, when saved and loaded by read_csv, blank cols become NA
    }

    # remove _rep cols, replace with count:
    #if( any(endsWith(cols, "_rep")) ) {
    colsnorep <- cols[!endsWith(cols, "_rep")] # want to group by all EXCEPT _rep
    tg <- dplyr::group_by_at(t, colsnorep )
    t <- dplyr::summarise(tg, count=dplyr::n() )
    t <- dplyr::ungroup(t)
    #}

    cols <- colnames(t)
    subscols <- cols[grepl("subsample", cols)]
    # combine any subsample cols if they exist:
    if(length(subscols) == 0) {
      t <- dplyr::mutate(t, subsample="")
    } else if(length(subscols) == 1) {
      t <- dplyr::rename(t, subsample=subsample_01)
    } else if(length(subscols) > 1) {
      t <- tidyr::unite(t, "subsample", subscols)
    }
    # any subsample cols are combined into one col - subsample

    exp <- list.files( dirname(dirname(s)))
    exp <- exp[endsWith(exp,".Rmd")]
    exp <- substr(exp, regexpr("~_", exp)+2, regexpr(".Rmd", exp)-1)

    # edit t - mutate to form ID, SAMPLE, COUNT, EXP
    t <- dplyr::transmute(
                  t,
                  ID=ID,
                  SAMPLE=subsample,
                  COUNT=count,
                  EXP=basename( dirname(s) ),
                  TITLE=exp,
                  PATH = dirname( s ) )

    # bind samples_summary with t:
    samples_summary <- dplyr::bind_rows(samples_summary, t)
  }

  # return the samples_summary
  samples_summary

}


#' Load Sample data from Project Note DIR
#'
#' Return a list of all subsamples that exist in the current note.
#'
#' @export
sample_load <- function() {

  context <- rstudioapi::getSourceEditorContext()
  context$path <- path.expand(context$path) # expand "~" HOME

  DATA_DIR <- check_project_note_dir(context$path)

  DATA_DIR_PATH <- paste0(dirname(context$path), .Platform$file.sep, DATA_DIR)

  sample_load_dir(DATA_DIR_PATH)

}


#' Load Sample Data from DIR
#'
#' Loads sample data from directory `sampledir`.  The loaded datasets will contain all data
#' for each EXISTING subsample in the DIR.  Separate sub-samples will be returned in separate tibbles,
#' as different sub-samples may have different data columns.
#'
#'
#' @export
sample_load_dir <- function( sampledir ) {

  context <- rstudioapi::getSourceEditorContext()
  context$path <- path.expand(context$path) # expand "~" HOME

  # get all samples CSVs:
    #  all files that begin with __samples
  samples_list <- list.files(sampledir, pattern="__samples",
                            full.names = TRUE, recursive=TRUE)

  if( length(samples_list) == 0 ) {
    # check if the sampledir is defined from the orgDir:
    sampledir <- paste0(findOrgDir(context$path), .Platform$file.sep, sampledir )

    # get all files that begin with __samples - these are the files to process:
    samples_list <- list.files(sampledir, pattern="__samples",
                              full.names = TRUE, recursive=TRUE)
  }

  # check sampledir - make sure its in a org DIR
  sampledir <- checkProgSubDir(sampledir)

   # only keep files which have "__samples" at the start of their name:
  samples_list <- samples_list[ startsWith(basename(samples_list), "__samples") ]

  # order from shortest to longest file names
  samples_list <- samples_list[order(nchar(basename(samples_list)), samples_list)]

  samples_names <- basename(samples_list)
  samples_names <- substr( samples_names, 1, regexpr(".csv", samples_names)-1  )

  samples_names <- paste0(basename(dirname(samples_list)),"/",samples_names)

  sample_tibbles <- list() # start with blank list
  # load all samples tibbles
  for(s in 1:length(samples_list) ) {

    # read tibble:
     # add to list using the samples_name - can retrieve tibble using this string!
    sample_tibbles[[ samples_names[s] ]] <- readr::read_csv(
                            samples_list[s],
                             col_types = readr::cols( ID = readr::col_character())
                            )
  }

  # starting at the END NODES - i.e. EXISTING SAMPLES
   # want to traverse back along the resampling route, and add the data columns from each previous level
  concat_sample_tibbles <- list() # start with blank list

  for(s in 1:length(sample_tibbles) ) {

    # get all colnames
    cols <- colnames(sample_tibbles[[s]])

    # remove all rows with resample col that != NA
    if( any(cols == "resample") ) {
      concat_sample_tibbles[[ samples_names[s] ]] <- dplyr::filter(sample_tibbles[[s]], is.na(resample) ) # keep any which are NA - they STILL EXIST!
    } else {
      concat_sample_tibbles[[ samples_names[s] ]] <- sample_tibbles[[s]]
    }

    # remove all rows with export col that != NA
    if( any(cols == "export") ) {
      concat_sample_tibbles[[ samples_names[s] ]] <- dplyr::filter(concat_sample_tibbles[[ samples_names[s] ]], is.na(export)) # keep any which are NA - they STILL EXIST!
    }

    # remove 'resample' and 'resample_dt' cols in current tibble IF EXIST:
    if (any( cols == "resample") ) {
      concat_sample_tibbles[[ samples_names[s] ]] <- dplyr::select(
        concat_sample_tibbles[[ samples_names[s] ]],
        -c("resample", "resample_dt")
      )
    }

    # get all subsample columns, put in REVERSE ORDER (MAX_INDEX -> 01),
     # and add data from each parent tibble

    sub_cols <- cols[ startsWith(cols, "subsample_") & !endsWith(cols, "_rep") ]

    if( length(sub_cols) == 0 ) {
      # do nothing

    } else if( length(sub_cols) == 1 ) {
      # there has only been one re-sampling, and the data for this is in the current tibble
      # so just need to extract the data from "__samples" dataframe to put into this tibble

      concat_sample_tibbles[[ samples_names[s] ]] <- dplyr::left_join(
                        concat_sample_tibbles[[ samples_names[s] ]],
                        sample_tibbles[[ paste0(basename(dirname(samples_list[s])),"/","__samples") ]],
                        by = "ID"
                   )

      # remove 'resample' and 'resample_dt' cols
      concat_sample_tibbles[[ samples_names[s] ]] <- dplyr::select(
                          concat_sample_tibbles[[ samples_names[s] ]],
                          -c("resample", "resample_dt")
                    )



    } else { # more than one subsample col - process each in tern

      for( c in (length(sub_cols)-1):1 ) {
        # ignore the last subsample col - as this represents the most recent subsample!

        col_str <- ""
        for(i in c:1) {
          col_str <-paste0( sample_tibbles[[s]][sub_cols[i]][[1]][1], "_", col_str)
        }
        col_str <- substr(col_str, 1, nchar(col_str)-1) # this is now a concat of all subsamples
        # add '__samples_ to front to make it a reference for sample_tibbles
        col_str <- paste0("__samples_", col_str)

        # before joining remove all subsample_XX cols from concat_sample_tibbles[[ samples_names[s] ]]
        # sub_cols to remove are sub_cols[1:c]
        #concat_sample_tibbles[[ samples_names[s] ]] <- dplyr::select(
         # concat_sample_tibbles[[ samples_names[s] ]],
        #  -dplyr::matches(sub_cols[1:c])
        #)

        concat_sample_tibbles[[ samples_names[s] ]] <- dplyr::left_join(
          concat_sample_tibbles[[ samples_names[s] ]],
          dplyr::select(sample_tibbles[[ paste0(basename(dirname(samples_list[s])),"/",col_str) ]],
                        -dplyr::matches(sub_cols[1:c]) ),
          by = "ID"
        )

        # remove 'resample' and 'resample_dt' cols
        concat_sample_tibbles[[ samples_names[s] ]] <- dplyr::select(
          concat_sample_tibbles[[ samples_names[s] ]],
          -c("resample", "resample_dt")
        )

      }

      # all re-sampling data has been added to the current tibble
      # so just need to extract the data from "__samples" dataframe to put into this tibble

      concat_sample_tibbles[[ samples_names[s] ]] <- dplyr::left_join(
        concat_sample_tibbles[[ samples_names[s] ]],
        sample_tibbles[[ paste0(basename(dirname(samples_list[s])),"/","__samples") ]],
        by = "ID"
      )

      # remove 'resample' and 'resample_dt' cols
      concat_sample_tibbles[[ samples_names[s] ]] <- dplyr::select(
        concat_sample_tibbles[[ samples_names[s] ]],
        -c("resample", "resample_dt")
      )

    }

  }

  # finally, filter concat_sample_tibbles
   # only return tibbles that have any rows
  tibbles <- list()
  for(i in 1:length(concat_sample_tibbles) ) {
    if(nrow(concat_sample_tibbles[[i]]) > 0 ) {
      tibbles[[ names(concat_sample_tibbles)[i] ]] <- concat_sample_tibbles[[i]]
      attr(tibbles[[names(concat_sample_tibbles)[i]]], "path") <- samples_list[i]
    }
  }

  # return the filtered tibbles list:
  tibbles

}


#' Load Sample Data from Summary Tibble
#'
#' Loads all data for the samples in the Summary Tibble.
#'
#' This needs completing: sample_load_dir() call bug? Filter rows that match samples_summary?
#'
#' @export
sample_load_from_summary <- function( samples_summary ) {

  # for each unique samples_summary$PATH
   # load dir:
  tibbles <- sample_load_dir( unique(samples_summary$PATH) )

  # filter each tibble returned to only contain the IDs and resamplings listed
   # in samples_summary

  # FIRT keep only tibbles that match the summary EXP'/__samples_'SAMPLE value:
  tibbles_names <- unique( paste0(samples_summary$EXP, "/__samples_", samples_summary$SAMPLE))

  # replace any ending with /__samples_ with /__samples
  tibbles_names[ endsWith(tibbles_names, "/__samples_")] <- paste0(
    substr(
      tibbles_names[ endsWith(tibbles_names, "/__samples_")],
      1,
      regexpr("/__samples_", tibbles_names[ endsWith(tibbles_names, "/__samples_")] )
      ),
    "__samples"  )

   # only keep tibbles which have the names in tibbles_names
  tibbles <- tibbles[ tibbles_names ]

  # SECOND keep only ROWS that match summary rows
   # actually this is not possible as summary has summarised across REPS!

  # so just return the filtered tibbles
  tibbles

}


#' Import Samples from SOURCE Project Note to DESTINATION Project Note
#'
#' Imports all samples listed in the `samples_summary` tibble into the current note from their
#' source notes.  The `samples_summary` tibble must be a tibble derived from the `sample_find`
#' function, but with an additional column added, REPS, to indicate how many REPS of each sample
#' to import.
#'
#' NOTE: After initial import, the function creates a special samples file - __IMPORT_samples.csv
#' If this file exists already, this function just returns this file
#' this ensures the method will run with no errors repeatedly
#' With all future calls to projectmanagr::sample_ functions, the data will be saved as __samples.csv
#' To import another set of samples, use 'reimport = TRUE' in sample_import function
#'
#'@export
sample_import <- function( samples_summary ) {

  cat( "\nprojectmanagr::sample_import():\n" )

  # get the currently active source editor context - hoepfully a projectmanagr Project Note!
  context <- rstudioapi::getSourceEditorContext()
  context$path <- path.expand(context$path) # expand "~" HOME

  # get/check project note DIR:
  DATA_DIR <- check_project_note_dir(context$path)

  # define the new samples CSV path in the current notes' DATA_DIR:
  import_samples_path <- paste0( dirname(context$path),
                                 .Platform$file.sep, DATA_DIR,
                                 .Platform$file.sep, "__IMPORT_samples.csv" )

  if( file.exists(import_samples_path) ) {
    # if the file already exists, just open it and write and return the `__samples.csv` file
     # from it
     # this is for reproducible running of this method!
    import_samples <- readr::read_csv( import_samples_path,
                    col_types = readr::cols( ID = readr::col_character() )
                            )

    # also, return the tibble from this method the tibble that will form the `__samples.csv' file
     # exclude the import and import_dt cols?  Keep the SAMPLE and REP cols?
    samples <- dplyr::select(import_samples, ID, SAMPLE, REP)
    samples_filename <- "__samples.csv"
    samples_path <- paste0( dirname(context$path),
                            .Platform$file.sep, DATA_DIR,
                            .Platform$file.sep, samples_filename )
    attr(samples, "path") <- samples_filename
    readr::write_csv(samples, samples_path)

    # return samples:
    samples

  } else {

  # check samples_summary:
  samples_summary <- check_samples_summary( samples_summary )
    # no need to check the REPS are valid, as the samples_summary MUST return total rep COUNT
     # and this is compared with the User entered REPS selection column

  # identify each samples filename:
  samples_paths <- get_samples_paths( samples_summary )
  # and identify project_note_paths, one for each samples_paths:
  project_note_paths <- get_project_note_paths( samples_paths )
    # NB: if samples are taken from separate sub-samples from the SAME proejct note, the project
     # note will appear TWICE in this list, but this is OK, as will want to add the export code
     # for EACH SUBSAMPLE separately

  # create the new tibble with Columns: ID and SAMPLE (composite of all subsampling)
  ID <- ""
  SAMPLE <- ""
  REP <- as.integer(1)
  import <- ""
  import_dt <- ""
  import_samples <- tibble::tibble(ID)
  import_samples <- tibble::add_column( import_samples, SAMPLE )
  import_samples <- tibble::add_column( import_samples, REP )
  import_samples <- tibble::add_column( import_samples, import )
  import_samples <- tibble::add_column( import_samples, import_dt )
   # make a BLANK TIBBLE
  import_samples <- dplyr::filter(import_samples, FALSE)

  # loop through each samples, perform the export and import for selected samples from each path:
  for( a in 1:length(samples_paths) ) {

    # define the export_vector_string export_dt_vector - goes into the export CSV
     # and import_vector_string import_dt_vector - goes into the import CSV
    export_vector_string <- R.utils::getRelativePath(import_samples_path,
                                                     relativeTo = samples_paths[a] )
    import_vector_string <- R.utils::getRelativePath( samples_paths[a],
                                                      relativeTo = import_samples_path )
    export_dt_vector <- get_datetime()
    import_dt_vector <- export_dt_vector

    # filter samples_summary so it contains only the samples from current samples_paths:
    summary <- dplyr::filter(samples_summary,
                             PATH == dirname(samples_paths[a]) &
                              SAMPLE == get_sample_from_path(samples_paths[a]) )

    # create export_content string to insert into project note Rmd:
    export_content <- c("","","",paste0("# EXPORT ", summary$SAMPLE[1]), "", "",
                        paste0("```{r, export-0", a, "}"), "" )

    # read sample CSV:
    t <- readr::read_csv(
      samples_paths[a],
      col_types = readr::cols( ID = readr::col_character())
    )

    # add to export_content too:
     # first define the string that will be used to refer to sample data loaded from csv:
    sample_data_var <- gsub("-", "_",
                        substr( basename(samples_paths[a]), 3, regexpr(".csv", basename(samples_paths[a]) )-1 ) )
    # then add code that loads the CSV and stores it to this var:
    export_content <- c(export_content,
                        " # read sample CSV:",
                        paste0(
                          sample_data_var,
                          ' <- projectmanagr::sample_load_csv(filename="', basename(samples_paths[a]),'")'),
                        "" )

    # get all colnames
    cols <- colnames(t)

    if( any(cols == "export") == FALSE ) {
      # add export and export_dt columns, fill with NA
      export <- NA_character_
      export_dt <- NA_character_ #as.POSIXct(NA)
      t <- tibble::add_column(t, export)
      t <- tibble::add_column(t, export_dt)
    }

    # KEEP all the resampled and exported cols - so can index the ACTUAL Reps in the import_samples
      # determine if current export tibble has been resampled at all:
    resampled <- FALSE
    if( any(cols == "resample") ) {
      resampled <- TRUE
    }

    # create new summary table - with each ID listing how many REPLICATES/sections it has:
     # plus add the cumulative sum of these in a second column:
      # this will provide the initial INDEX to search through t
    samples_ID <- dplyr::group_by(t, ID)
    section_nums <- dplyr::summarise(samples_ID, sections = dplyr::n() )
     # make sure to re-arrange by ORIGINAL order of ID in t!
      # BEFORE computing the cumulative sum!
    section_nums <- dplyr::arrange( section_nums, order(match( unique(t$ID), section_nums$ID)) )
    section_nums <- dplyr::mutate(section_nums, sections_cum = cumsum(sections) )

    # create IDs and rep_select VARs
    IDs <- summary$ID
    rep_select <- list()

    # for each ID and each REPS selected in summary, fill export and export_dt
    for( i in 1:length(summary$ID) ) {

      # create rep_select_ID to collate the reps selected for this ID to add to list rep_select
      rep_select_ID <- c()

      # get the number of REPS that must be exported:
      reps_exp <- summary$REPS[i]

      # get the indices for searching t
      section_num <- section_nums$sections[ section_nums$ID == summary$ID[i] ]
      section_last <- section_nums$sections_cum[ section_nums$ID == summary$ID[i] ]
      section_first <- section_last - (section_num - 1)

      # search export col in t, any where it is empty, FILL:
      for(j in section_first:section_last) {

        # check the export col is free
        if( is.na(t$export[j]) ) {
          # if not exported (export col is NA), can EXPORT this rep:

          # ALSO check if there is a resampled col, and is free
          if(  ( resampled==FALSE ) ||( resampled==TRUE && is.na(t$resample[j]) )  ) {
            # if not resampled (resampled is NA, or not existing), can EXPORT this rep:

            # first add the export and export_dt cols
            t$export[j] <- export_vector_string
            t$export_dt[j] <- export_dt_vector

            # concat the REP index to rep_select_ID:
            rep_select_ID <- c(rep_select_ID, (j-section_first+1) )

            # then add the ID, SAMPLE, REP, import, import_dt to import_samples tibble:
             # create new tibble ti - ID, SAMPLE, REPS, import, import_dt
            ti <- tibble::tibble(ID=summary$ID[i],
                                  SAMPLE=summary$SAMPLE[i],
                                  REP=(j-section_first+1),
                                  import=import_vector_string,
                                  import_dt=import_dt_vector )

            # bind samples_summary with ti:
            import_samples <- dplyr::bind_rows(import_samples, ti)

            reps_exp <- reps_exp -1
            if(reps_exp == 0 ) {
              # all desired reps have been exported - BREAK to next ID (for loop i)
              break
            }
          } # if resampled
        } # if export
      } # for j

      # finally, add rep_select_ID to list rep_select
      rep_select[[i]] <- rep_select_ID

    } # for i

    # save t:
    readr::write_csv(t, samples_paths[a] )
    cat( "\n  Written export to CSV:", samples_paths[a], "\n\n" )

    # FINALLY:
    # Write the EXPORT Rmd and code to the SOURCE Rmd Project Note:
     # add to export_content sample_add_data adding the export and export_dt to relevant IDs and rep_select
      # first create IDs_input and resp_select_input strings
    IDs_input <- create_IDs_input(IDs)
    rep_select_input <- create_rep_select_input(rep_select)
    export_content <- c(export_content,
                        "",
                        " # add export and export_dt to IDs and rep_select",
                        "",
                        paste0(
                          sample_data_var,
                          " <- projectmanagr::sample_add_data("
                          ),
                        "", paste0("  ", sample_data_var, "," ),
                        "", paste0("  ", 'export = "', export_vector_string, '",' ),
                        "", paste0("  ", 'export_dt = "', export_dt_vector, '",' ),
                        "", IDs_input,
                        "", rep_select_input,
                        "", "  overwrite = TRUE # ensure this method runs even if the cols exist",
                        "", paste0("  ", ")" ),
                        "", sample_data_var,
                        "",
                        "```", "", "")

    # Write to Rmd at end of file:
    source_contents <- readLines( project_note_paths[a] )
    line <- computeLastLineIndex(source_contents)
    # Insert export_content to source_contents:
    source_contents <- c(source_contents[1:(line-1)],
                         export_content,
                         source_contents[(line+1):length(source_contents)])

    writeLines(source_contents, project_note_paths[a])


  } # for a - samples_paths


  #save the import_samples tibble to new DEST NOTE DIR:
  readr::write_csv(import_samples, import_samples_path)

  # also, return the tibble from this method the tibble that will form the `__samples.csv' file
   # exclude the import and import_dt cols?  Keep the SAMPLE and REP cols?
  samples <- dplyr::select(import_samples, ID, SAMPLE, REP)
  samples_filename <- "__samples.csv"
  samples_path <- paste0( dirname(context$path),
                                 .Platform$file.sep, DATA_DIR,
                                 .Platform$file.sep, samples_filename )
  attr(samples, "path") <- samples_filename
  readr::write_csv(samples, samples_path)

  # return samples:
  samples

  } # end else if import_samples_path does not exist

}


create_IDs_input <- function(IDs) {
  IDs_input <- paste0("  ", 'IDs = c("')
  for(i in 1:length(IDs) ) {
    IDs_input <- paste0(IDs_input, IDs[i], '", "')
  }
  IDs_input <- substr(IDs_input, 1, nchar(IDs_input)-3)
  IDs_input <- paste0(IDs_input, "),")
  IDs_input
}

create_rep_select_input <- function(rep_select) {
  rep_select_input <- paste0("  ", 'rep_select = list( ')

  for(i in 1:length(rep_select) ) {
    # accessing the LIST in IDs order
    rep_select_input <- paste0(rep_select_input, 'c(')
    for(j in 1:length(rep_select[[i]]) ) {
      rep_select_input <- paste0(rep_select_input, rep_select[[i]][j], ', ')
    }
    rep_select_input <- substr(rep_select_input, 1, nchar(rep_select_input)-2)
    rep_select_input <- paste0(rep_select_input, "), ")
  }
  rep_select_input <- substr(rep_select_input, 1, nchar(rep_select_input)-2)
  rep_select_input <- paste0(rep_select_input, " ), ")
  rep_select_input
}


#' Find Samples Recursively inside a Directory
#'
#' This will identify all samples CSVs inside a Direcotry recursively, presenting
#' a summary Tibble containing each EXISTING sample - data including:
#'
#' * ID - sample ID
#' * SAMPLE: Composite of all subsampling
#' * COUNT: Number of reps of the sample (sections)
#' * EXP: The experiment PREFIX ID
#' * PATH: The absolute path to the Expeirment DIR
#'
#' @param subProgDir a directory that is inside a Programme, which will be searched recursively
#' to find all files that contain the string glob `__sample*`
#'
sample_find_2 <- function(subProgDir) {

  context <- rstudioapi::getSourceEditorContext()
  context$path <- path.expand(context$path) # expand "~" HOME

  subProgDir <- paste0(findOrgDir(context$path), .Platform$file.sep, subProgDir )

  # check subProgDir
  subProgDir <- checkProgSubDir(subProgDir)

  # get all files that begin with __samples - these are the files to process:
  samplesList <- list.files(subProgDir, pattern="__samples*",
                            full.names = TRUE, recursive=TRUE)

  # identify all Sample DIRs and process all the _sample* files that exist in each together:
  sampleDirs <- unique( dirname(samplesList) )

  # define a new tibble to hold all samples:
  # MUST define a STANDARD TEMPLATE to hold SUMMARY DATA on samples
  # ID: Each Sample ID
  # SAMPLE: COMPOSITE of all subsampling columns: CNS-RT-MB etc.
  # COUNT: How many REPS are there of this sample?
  # EXP: Fill with the Experiment Prefix ID
  # TITLE: Fill with Experiment Title - will contain the LAB_TREATMENT
  # PATH: Put the absolute PATH to the Project Note Rmd to Navigate to
  # From this summary information, should be possible to select samples, or further explore them
  ID <- ""
  SAMPLE <- ""
  COUNT <- integer()
  EXP <- ""
  TITLE <- ""
  PATH <- ""
  samples_summary <- tibble::tibble(ID, SAMPLE, COUNT, EXP, TITLE, PATH)

  for(s in sampleDirs) {

    # get the sample files that are in one sampleDir:
    sampleDirList <- samplesList[startsWith(samplesList, s)]

    # order the sampleDirList by length of each element - short to long:
    sampleDirList <- sampleDirList[order(nchar(sampleDirList), sampleDirList)]

    # create a blank list to put the tibbles
    sampleTibbleList <- vector("list", length(sampleDirList))

    # open and filter sample tibbles:
    for(c in 1:length(sampleDirList) ) {

      t <- readr::read_csv(sampleDirList[c],
                           col_types = readr::cols( ID = readr::col_character()) )

      # trim the tibble to contain only ID, subsample, resample, export columns:
      #  -dplyr::ends_with("_rep"),
      t <- dplyr::select(
        t,
        ID,
        dplyr::starts_with("subsample"),
        dplyr::matches("resample"), -dplyr::matches("resample_dt"),
        dplyr::matches("export"), -dplyr::matches("export_dt") )

      # get all colnames
      cols <- colnames(t)

      # remove all rows with resample col that != NA
      if( any(cols == "resample") ) {
        t <- dplyr::filter(t, is.na(resample)) # keep any which are NA - they STILL EXIST!
      }

      # remove all rows with export col that != NA
      if( any(cols == "export") ) {
        t <- dplyr::filter(t, is.na(export)) # keep any which are NA - they STILL EXIST!
      }

      # remove _rep cols, replace with count:
      #if( any(endsWith(cols, "_rep")) ) {
      colsnorep <- cols[!endsWith(cols, "_rep")] # want to group by all EXCEPT _rep
      tg <- dplyr::group_by_at(t, colsnorep )
      t <- dplyr::summarise(tg, count=dplyr::n() )
      t <- dplyr::ungroup(t)
      #}

      cols <- colnames(t)
      subscols <- cols[grepl("subsample", cols)]
      # combine any subsample cols if they exist:
      if(length(subscols) == 0) {
        t <- dplyr::mutate(t, subsample="")
      } else if(length(subscols) == 1) {
        t <- dplyr::rename(t, subsample=subsample_01)
      } else if(length(subscols) > 1) {
        t <- tidyr::unite(t, "subsample", subscols)
      }
      # any subsample cols are combined into one col - subsample

      # save trimmed tibble to list
      #sampleTibbleList[[c]] <- t

      exp <- list.files( dirname(dirname(sampleDirList[c])))
      exp <- exp[endsWith(exp,".Rmd")]
      exp <- substr(exp, regexpr("~_", exp)+2, regexpr(".Rmd", exp)-1)

      # edit t - mutate to form ID, SAMPLE, COUNT, EXP
      t <- dplyr::transmute(t,
                            ID=t$ID,
                            SAMPLE=subsample,
                            COUNT=count,
                            EXP=basename( dirname(sampleDirList[c]) ),
                            TITLE=exp,
                            PATH = dirname( sampleDirList[c] ) )
      # bind samples_summary with t:
      samples_summary <- dplyr::bind_rows(samples_summary, t)

    }

  }

  # return the samples_summary
  samples_summary

}


#' Load a Sample Dataset
#'
#' Load a Sample Dataset from inside a Project Note from ProjectManagr:
#' The data is automatically loaded from the associated DATA_DIR of the
#' projectmanagr Project Note - defined as the Directory named as the
#' PREFIX to the Project Note (the string before the ~_ separator).
#'
#' The loaded dataset is a tibble inside a Named List, which ensures
#' the tibble remains NAMED - all other sample_ functions use this named
#' tibble to save the data to the correct file when saving data, so it
#' is important to use this function for loading a dataset.
#'
#' @param sampledir the path to the Dir containing the samples.
#'
#' @return The loaded TIBBLE with the "path" attribute set to the sample filename.
#'
#'@export
sample_load_csv <- function( sampledir, filename="__samples.csv" ) {

  # Load data
  # MAKE SURE ID is passed as a CHARACTER!
  samples <- readr::read_csv( paste0(sampledir, .Platform$file.sep, filename),
                              col_types = readr::cols( ID = readr::col_character()))

  # ADD path attribute
  attr(samples, "path") <- filename
  # access with: attr(samples[[1]], "path")

  # return the tibble
  samples

}

#' Load a Sample Dataset
#'
#' Load a Sample Dataset from inside a Project Note from ProjectManagr:
#' The data is automatically loaded from the associated DATA_DIR of the
#' projectmanagr Project Note - defined as the Directory named as the
#' PREFIX to the Project Note (the string before the ~_ separator).
#'
#' The loaded dataset is a tibble inside a Named List, which ensures
#' the tibble remains NAMED - all other sample_ functions use this named
#' tibble to save the data to the correct file when saving data, so it
#' is important to use this function for loading a dataset.
#'
#' @param filename the filename of the dataset to load.
#'
#' @return The loaded TIBBLE with the "path" attribute set to the sample filename.
#'
#'@export
sample_load_csv <- function(filename="__samples.csv") {

  # get the currently active source editor context - hoepfully a projectmanagr Project Note!
  context <- rstudioapi::getSourceEditorContext()

  # get/check project note DIR:
  DATA_DIR <- check_project_note_dir(context$path)

  # Load data
    # MAKE SURE ID is passed as a CHARACTER!
  samples <- readr::read_csv( paste0(DATA_DIR, .Platform$file.sep, filename),
                              col_types = readr::cols( ID = readr::col_character()))

  samples

  # ADD path attribute
  attr(samples, "path") <- filename
    # access with: attr(samples[[1]], "path")

  # return the tibble
  samples

  # wrap into list, and name the dataframe:
  #sampleDataFrame <- list(samples)
  #names(sampleDataFrame) <- filename

  #sampleDataFrame

}



#' Save a Sample Dataset
#'
#' Saves teh sample dataset, using the "path" attribute set to the sampleDataFrame during
#' sample_load or sample_create.
#'
#' @param sampleDataFrame a tibble opened with sample_load or created with sample_create.
#'
#' @export
sample_save <- function(sampleDataFrame) {

  readr::write_csv(  paste0( DATA_DIR, .Platform$file.sep, attr(sampleDataFrame, "path") )  )

}



#' Add Sample Data
#'
#' This function receives a sampleDataFrame (a special Tibble which contains a 'path' attribute - this
#' is the name of the file in the Project Note's Data Dir), and a series of vectors (...).  After checking
#' the validity, each vector is inserted into the sampleDataFrame and a new copy returned.
#'
#' @param sampleDataFrame a tibble opened with sample_load or created with sample_create.  The tibble MUST
#' have a 'path' attribute assigned to it, and this MUST point to the tibbles location relative to the
#' Project Note DIR.  Use sample_load() to load Single tibble for adding data.
#'
#' @param ... one or more vectors to insert into the dataframe. This data must be supplied in the
#' form 'name = vector'.
#'
#' @return The newly created and saved TIBBLE with correct 'path' attribute.
#'
#' @export
sample_add_data_old <- function( sampleDataFrame, ... ) {

  cat( "\nprojectmanagr::sample_add_data():\n" )

  # get the currently active source editor context - hoepfully a projectmanagr Project Note!
  context <- rstudioapi::getSourceEditorContext()

  # get/check project note DIR:
  DATA_DIR <- check_project_note_dir(context$path)

  # check sampleDataFrame:
  check_sample_data_frame(sampleDataFrame, DATA_DIR)

  # get the filename from the sampleDataFrame list:
  filename <- attr(sampleDataFrame, "path")

  # extract samples tibble and its row count:
  samples <- sampleDataFrame
  rows <- nrow(samples)

  colNames <- names(samples)


  # create a named list of the argument values:
  argg <- list(...)
  varNames <- names(argg)

  # check varNames
  for( i in 1:length(varNames) ) {

    if(is.element(varNames[i], colNames) ) {
      stop( paste0("  Column with name Vector already exists - are you adding the data twice?: ", i) )
    }

    # Check each Vector in argg:

    # does it have the correct length?  same as tibble nrows, or length of 1?
    if(rows != length(i) && length(i) != 1 ) {
      # the vector in i is not of correct length to add to dataframe
      stop( paste0("  Vector is not of correct length to add to dataframe: ", i) )
    }

    if( varNames[i] == "" ) { # varNames contains BLANK if not using 'colName=vector' syntax
      stop(paste0(" Vector names are missing - add vectors with 'colName=vector' syntax: ", varNames[i]))
    }

  }



  # Add each vector to the tibble, and rename the column to the vector name:
  for(i in 1:length(argg) ) {

    samples <- tibble::add_column(samples, argg[[i]])

    #arg <- deparse(substitute(argg[[i]]))
    arg <- "argg[[i]]"

    names(samples)[names(samples) == arg] <- varNames[i]

  }

  # Write to CSV:
  readr::write_csv(samples, paste0(DATA_DIR, .Platform$file.sep, filename) )
  cat( "\n  Written samples CSV:", paste0(DATA_DIR, .Platform$file.sep, filename), "\n\n" )

  # ADD path attribute
  attr(samples, "path") <- filename
  # access with: attr(samples[[1]], "path")

  # return the tibble
  samples

}


#' Export Sample Data
#'
#' This function receives a sampleDataFrame (a NAMED list that contains a Single NAMED Tibble - the name
#' is the name of the file in the Project Note's Data Dir), an export_dest that contains a character
#' vector - to identify which Project Note to export to, and an export_dt - a datetime string vector.
#'
#' - Obtain the export_dest by using the projectmanagr ADDIN: ProjectManagr: Add Hyper Link: This will paste
#' the relative link to any other open Rmd in RStudio the user selects.  Use this result to set export_dest.
#'
#' From this input data, the selected IDs from the sampleDataFrame are exported - new columns to indicate
#' the export are created (export, export_dt), and filled (with the DESTINATION Project Note RELATIVE PATH,
#'  and the export_dt, respectively).
#'
#' By default, all samples in sampleDataFrame will be exported. However, subsets of IDs and replicates can
#' be selected by setting the IDs and rep_select ARGs:
#'
#' - IDs - can be set to a Character Vector containing the subset of IDs to export.  Any
#'   remaining IDs will have their export/export_dt columns left BLANK.
#'
#' - rep_select - can be set to a List of Integer/numeric vectors to represent the selection of replicates
#'   to apply the export to.  Each vector in the list represents each ID (in order of IDs in the Data
#'   Frame if all IDs, otherwise using the IDs ordering from the IDs ARG).
#'
#' When exporting, the export_dest contains the destination Project Note (and its DIR).  This should be the
#' RELATIVE PATH from the current Project Note to the destination Project Note.  This will be saved in the
#' export column in the current Sample DataFrame, and the Source Project Note relative path will be computed
#' and saved to the new sample DataFrame fromed in the Exported Proejct Note's DIR.
#'
#' The export process will create a NEW DATA FRAME in the destination Project Note DIR:  This dataframe
#' will be filled with the ID, all previous subsampling and import columns, and a new import column
#' (import_INDEX) that contains the source Project Note relative path for this import.  For each rep_select
#' made, the relevant previous data is filled into the new Data Frame.
#'
#' To check the results of a given export, its possible to set the writeToDisk boolean to FALSE: in this
#' case the computations are performed, the edits made to a copy of the original sampleDataFrame, and copies
#' of the old and new DataFrames are all returned for inspection.  Once the inputs are known to be correct,
#' the actual exporting (with writing of CSVs to Disk) can be performed with writeToDisk set to TRUE - its
#' default value.
#'
#' The function is assumed to be run inside an R Chunk in an Rmd file that is a Project Note from the
#' projectmanagr package.  It assumes the document is saved to disk, it contains a PREFIX, separated from
#' the Note title with the '~_' separator, and that an adjacent Data DIR exists next to the Note Rmd file,
#' titled with the PREFIX - this is where the sampleDataFrame is assumed to exist, and where the new Data
#' Frames will be written.
#'
#'
#' @param sampleDataFrame a tibble opened with sample_load or created with sample_create.  The tibble MUST
#' have a 'path' attribute assigned to it, and this MUST point to the tibbles location relative to the
#' Project Note DIR.  Use sample_load() to load Single tibble for exporting
#'
#' @param export_dest Character String containing the relative path of the destination Project Note.
#' This can be the relative path to the Project Note Rmd file, or its DATA_DIR (just the Prefix!)
#'
#' @param export_dt a Datetime of the export event, can be a single string (for all IDs), or a vector
#' with a new datetime for each ID.
#'
#' @param IDs Character vector indicating which subset of IDs to apply the export to. By default
#'  the export is applied to "all".  The function will fail if any of the selected IDs (and
#'  replicates) have been exported or re-sampled already.
#'
#' @param rep_select List of integer/numeric vectors to represent the selection of replicates from the
#'  sampleDataFrame. This vector is used to select SECTIONS from a sampleDataFrame which has previously
#'  been re-sampled with replicate subsamples (see rep_create in sample_resample function).  The list can
#'  always be of length 1, in which case the same replicates are selected for ALL IDs. Otherwise it must
#'  be the same length as ID from the sampleDataFrame, in which case each vector in the list selects a
#'  unique set of replicates from each ID in the sampleDataFrame; or if the IDs parameter has been set,
#'  a list the same length as IDs can be passed, with each vector in the list selecting a unique set of
#'  replicates from the set of IDs passed.  The function will fail if any of the selected replicates
#'  have been re-sampled or exported already. Uses the KEY WORD "all" to select all replicates by default.
#'
#' @param writeToDisk Boolean to indicate whether the edits made to sampleDataFrame and the newly formed
#' DataFrames are saved to disk.  TRUE by default - set to FALSE to test run the export.
#'
#' @return A newly created and saved list of TIBBLES : the ORIGINAL Tibble containing the export data, and
#' the NEW TIBBLE saved to the destination Project Note, showing the newly imported Sample data.
#'
#' @export
sample_export <- function(sampleDataFrame, export_dest, export_dt,
                          IDs = "all", rep_select = "all",
                          writeToDisk = TRUE ) {

  cat( "\nprojectmanagr::sample_export():\n" )


  ### EXPORT sampleDataFrame ###

  # get the currently active source editor context - hoepfully a projectmanagr Project Note!
  context <- rstudioapi::getSourceEditorContext()
  context$path <- path.expand(context$path) # expand "~" HOME

  # get/check project note DIR:
  DATA_DIR <- check_project_note_dir(context$path)

  # check sampleDataFrame:
  check_sample_data_frame(sampleDataFrame, DATA_DIR)

  if( endsWith(export_dest, ".Rmd" )  == FALSE ) {
    # assume this is the DATA_DIR!
    # try to find the .Rmd file associated with the Prefix:
    data_dir_dest <- basename(export_dest)
    files_list <- list.files(dirname(export_dest))
    data_file_dest <- files_list[ startsWith(files_list, data_dir_dest) & endsWith(files_list, ".Rmd") ]
    export_dest <- paste0(dirname(export_dest), .Platform$file.sep, data_file_dest)
  }

  # compute import_dest - relative link pointing to the note data is exported FROM:
  # get the import ABSOLUTE PATH first
  import_abs <- R.utils::getAbsolutePath( paste0(dirname(context$path), "../", export_dest))
  import_dest <- R.utils::getRelativePath( context$path, relativeTo=import_abs)
  import_dest <- substring(import_dest, first=4, last=nchar(import_dest))

  DATA_DIR_IMPORT <- check_project_note_dir(import_abs)


  # get the filename from the sampleDataFrame list:
  filename <- attr(sampleDataFrame, "path")


  # COPY samples tibble and its row count:
  samples <- sampleDataFrame
  rows <- nrow(samples)

  colNames <- names(samples)

  # get all col names that start with subsample_ and import_
    # this PRESERVES THE COLUMN ORDERING!
  subsample_import_cols <- colNames[startsWith(colNames, "subsample_") | startsWith(colNames, "import_") ]
  # now select the subsamples and imports column - PRESERVING THE ORDER
  subsamples_imports <- dplyr::select(samples, subsample_import_cols)

      # get all imports columns in new tibble:
      imports <- dplyr::select(samples, dplyr::starts_with("import_"))

      if(length(imports) == 0) {
        import_index <- 1
      } else {
        # compute the number of UNIQUE subsample columns:
        import_col_names <- names(imports) # get the character vector of import col names
        import_index <- length(import_col_names) + 1 # get NEXT index
      }

      # String used as placeholder for initial name of new column in the IMPORTED DATASET
      arg <- "new_import"

  # Check export_dest is character and length 1:
  if(is.character(export_dest) == FALSE || length(export_dest) > 1 ) {
    stop( paste0("  'export_dest' not a character vector of length 1: ", export_dest) )
  }

  # check export_dest points to an EXISTING FILE!
  if( file.exists(export_dest) == FALSE ) {
    stop( paste0("  'export_dest' does not point to an existing file: ", export_dest) )
  }

      # check export_dest is a project note?


  # convert export_dt to datetime object - to add to export_dt column!:
  export_dt_vector <- lubridate::as_datetime(export_dt)

  # check export_dt_vector is valid datetime:
  if( lubridate::is.instant(export_dt_vector) == FALSE ) {
    stop( paste0("  'export_dt' not a datetime object: ", export_dt_vector) )
  }



  # Check IDs:

  if(IDs[1] != "all") {
    # calculate the row indices for the IDs presented:
    ID_indices <- match(IDs, samples$ID)
    if( anyNA(ID_indices) ) {
      stop( paste0("  IDs contains values that do not exist in dataFrame: ", IDs ) )
    }
    # ALSO just make sure IDs only contains UNIQUE values:
    IDs_count <- length(IDs)
    IDs_orig <- IDs
    IDs <- unique(IDs)
    if( IDs_count != length(IDs) ) {
      stop( paste0("  IDs contains a duplicate: ", IDs_orig ) )
    }
    # if this passes, IDs now contains valid IDs that are all unique
  } else {
    IDs <- unique(samples$ID)
    # set IDs to the UNIQUE SET of sample$ID -
    # if there are replicates in the current sample DataFrame, the ID vector will contain
    # replicates of the ID value!
  }
  # The IDs value is now a vector containing the sample IDs to process!



  # Check rep_select:

  if(rep_select[1] != "all") {
    # rep_select should be a list of integer vectors:
    # check:
    if( typeof(rep_select) != "list" ) {
      stop( paste0("  rep_select MUST be of type list - current type: ", typeof(rep_select) ) )
    }
    if(  any( sapply(rep_select, mode) != "numeric" )  ) {
      stop( paste0("  rep_select MUST contain type numeric." ) )
    }

    if( length(rep_select) == 1 ) { # LENGTH OF THE LIST IS ONE - it contains ONE INT VECTOR
      # always valid - will select rep_select number of replicates from each ID:
      # REPLICATE rep_select by number of IDs:
      if(IDs == "all") {
        rep_select <- rep(rep_select, length(samples$ID) )
      } else {
        rep_select <- rep(rep_select, length(IDs) )
      }

    } else if( length(rep_select) == length(IDs) ) {
      # if using all IDs, rep_select must be the length of UNIQUE samples$ID
      # IDs is the length of UNIQUE sample$ID - see above
      # if using selected IDs, rep_select must be the length of IDs

    } else {
      # else, the values for rep_select are invalid!
      stop( paste0("  Length of rep_select is invalid - check ID length or IDs selection?: ", rep_select ) )
    }

  }

  # compute if the CURRENT samples DataFrame already has export or resample columns:
  resample_col <- FALSE
  export_col <- FALSE
  if( any(colNames == "export") == TRUE ) {
    export_col = TRUE
  }
  if( any(colNames == "resample") == TRUE ) {
    resample_col = TRUE
  }


  # Create the export and export_dt columns:

  # if export column doesnt exist, create it and fill it with BLANK values:
  export <- ""

  if( any(colNames == "export") == FALSE ) {
    # add export column, fill with BLANK, if it doesnt exits
    samples <- tibble::add_column(samples, export)
  }


  # DITTO export_dt - fill with NA datetime:
  export_dt <- as.POSIXct(NA) # use NA instead of BLANK

  if( any(colNames == "export_dt") == FALSE ) {
    # add export_dt column, fill with BLANK, if it doesnt exits
    samples <- tibble::add_column(samples, export_dt)
  }


  # work through all IDs and rep_select values
  # For selected IDs and rep_select, CHECK if export ALREADY EXISTS (i.e. its not BLANK)
  # IF SO - stop this function
  # IF NOT - fill export and export_dt with the values:
  # export_dest export_dt_vector

  # create new summary table - with each ID listing how many REPLICATES/sections it has:
  # plus add the cumulative sum of these in a second column:
  samples_ID <- dplyr::group_by(samples, ID)
  section_nums <- dplyr::summarise(samples_ID, sections = dplyr::n() )
  # make sure to re-arrange by ORIGINAL order of ID in samples!
  # BEFORE computing the cumulative sum!
  section_nums <- dplyr::arrange( section_nums, order(match( unique(samples$ID), section_nums$ID)) )
  section_nums <- dplyr::mutate(section_nums, sections_cum = cumsum(sections) )


  # if rep_select is all - set a LIST containing a SET of Vectors that are a SEQUENCE from 1:(sections)
  # order is preserved with IDs!
  if(rep_select[1] == "all") {
    rep_select <- list()
    for(i in 1:length(section_nums$sections) ) {
      ss <- seq(section_nums$sections[i])
      rep_select[[i]] <- ss
    }
  }

  # access ID and sections with:  section_nums$ID section_nums$sections


  # for each ID in IDs, Set the relevant indices from rep_select to
  # resample_vector_string resample_dt_vector
  # export_dest export_dt_vector
  for( i in 1:length(IDs) ) {

    # get the number of sections, and cumulative sections:
    section_num <- section_nums$sections[ section_nums$ID == IDs[i] ]
    section_last <- section_nums$sections_cum[ section_nums$ID == IDs[i] ]
    section_first <- section_last - (section_num - 1) # -1 as the first section is ON section_first

    # get the current rep_select vector - check all selected replicates are VALID and AVAILABLE:
    # NB if rep_select was all, its now a vector containing section_num vector!
    reps <- rep_select[[i]]

    if( all(reps <= section_num) == FALSE) {
      # not enough sections to select reps!
      stop( paste0("  Not enough replicates in ID: ", IDs[i], " Replicate number: ",  section_num) )
    }

    # convert reps into indices:
    rep_indices <- (reps - 1) + section_first # -1 as the first section is ON section_first

    # check each index in resample and resample_dt - if BLANK and NA, then add the values
    for( j in rep_indices) {

      print( paste("j: ", j) )

      if( resample_col && (samples$resample[j] != "") ) {
        stop( paste0("  Selected sample has already been re-sampled: ", IDs[i], " index: ",  j ) )
      }

      if( export_col && is.na(samples$export[j]) == FALSE && (samples$export[j] != "") ) {
        stop( paste0("  Selected sample has already been exported: ", IDs[i], " index: ",  j ) )
      }

      samples$export[j] <- export_dest # this is a relative link pointing to the note data is exported TO
      #cat( "\n  Exported: ", IDs[i], " index: ",  j, "\n\n" )

      if( is.na(samples$export_dt[j]) == FALSE  ) {
        stop( paste0("  Selected sample has already been exported: ", IDs[i], " index: ",  reps[j] ) )
      }

      if(length(export_dt_vector) == 1) {
        samples$export_dt[j] <- export_dt_vector
      }
      else {
        samples$export_dt[j] <- export_dt_vector[j]
      }

    }

  }


  # The original sample DataFrame has now been filled correctly with re-sampling:
  # SAVE:

  if( writeToDisk == TRUE ) {
    # Write the samples Tibble to CSV:
    readr::write_csv(samples, paste0(DATA_DIR, .Platform$file.sep, filename) )
    cat( "\n  Written samples CSV:", paste0(DATA_DIR, .Platform$file.sep, filename), "\n\n" )
  }

  # re-load to prevent DISPLAY bug in export column:
  samples <- projectmanagr::sample_load(filename = paste0(filename))



  ### CREATE NEW IMPORTED DATASET ###


  # generate the tibble names from current sampleDataFrame filename,  using DATA_DIR to identify it
  newDataFrameNames <- paste0( substr(filename, 1, nchar(filename)-4), "-", DATA_DIR, ".csv" )


  # generate blank list to add dataFrame-lists to:
  samplesDataFrame <- list()


  # Add the ORIGINAL sampleDataFrame to this new list - so the user can check it after writing:
  samplesDataFrame[[ filename ]] <- samples


  # get ID col:
  ID <- samples$ID


  # create a new tibble for IMPORT DATA:

  new_import <- import_dest # this is a relative link pointing to the note data is exported FROM
    # fill import_INDEX with appropriate import_vector value

    import <- tibble::tibble(ID, subsamples_imports, new_import)


    # rename new_import with "import_INDEX":
    if(import_index == 1 ) {
      new_import_name <- "import_01"
    } else if(import_index < 10) {
      new_import_name <- paste0("import_0", import_index)
    } else {
      new_import_name <- paste0("import_", import_index)
    }

    names(import)[names(import) == arg] <- new_import_name

    # generate blank subsample tibble with correct columns, will fill this with cols from subsample
    import_blank <- dplyr::filter(import, FALSE)


    # Use section_nums DataFrame from before for number of replicates, first/last indices:
    # access ID and sections with:  section_nums$ID section_nums$sections

    # for each ID in IDs, copy the rows from indices in rep_select to
    # import_blank
    for( a in 1:length(IDs) ) {

      # get the number of sections, and cumulative sections:
      section_num <- section_nums$sections[ section_nums$ID == IDs[a] ]
      section_last <- section_nums$sections_cum[ section_nums$ID == IDs[a] ]
      section_first <- section_last - (section_num - 1) # -1 as the first section is ON section_first

      # get the current rep_select vector
      # NB if rep_select was 'all', its now a vector containing section_num vector!
      reps <- rep_select[[a]]

      # convert reps into indices:
      rep_indices <- (reps - 1) + section_first # -1 as the first section is ON section_first

      # copy each index in subsample to subsample_blank data frame:
      for( b in rep_indices) {

          # now copy selected row to subsample_blank:
          import_blank <- dplyr::bind_rows(import_blank, import[b, ])

      }
    }


    # Save the new tibble to the EXPORT DESTINATION DATA_DIR:

    import_samples <- paste0(dirname(import_abs), .Platform$file.sep,
                             DATA_DIR_IMPORT, .Platform$file.sep, "IMPORT_samples.csv")


    # CHECK IF AN IMPORT_* file already exists:
      # if so, just add the data from this to the import file
      # if not, then write the new file as: IMPORT_sample
        # DO NOT ADD the resampling - as this import file may end up containing
        # imports from other experiments too, which may have different sampling!

      # WILL NEED TO DEAL WITH subsample_ and import_ columns appropriately though!

    if( file.exists(import_samples) == TRUE ) {

    } else {

      # Just need to Write to CSV:
      readr::write_csv(import_blank, import_samples )
      cat( "\n  Written subsample CSV:", import_samples, "\n\n" )

    }

    newDataFrameNames <- "IMPORT_samples.csv" # the new dataframe is given this standard name..
    # add 'path' attribute to import_blank
    attr(import_blank, "path") <- newDataFrameNames[ (1) ]

    # Add sampleDataFrame to the samplesDataFrame list:
    samplesDataFrame[[ newDataFrameNames[ (1) ] ]] <- import_blank


  # Return the Named List containing the Tibbles - old export and new import data:
  samplesDataFrame


}


#' Import a Sample Dataset
#'
#' When a Sample is EXPORTED from a Project Note into a New Project Note,
#' a CSV file is copied into the new Project Note's dir that contains the
#' base import data.
#'
#' To maintain the functional paradigm (where no state is stored, and the
#' sample data files can be writtne to without reference to its state), any
#' exported data is imported into the new Project Note in a special CSV file
#' prefixed with `IMPORT_`.
#'
#' This method will open the `IMPORT_` file, and return the tibble, but the
#' path attribute is set to the filename OMITTING the `IMPORT_` prefix.
#'
#' - This is to allow the manipulation of the samples data (adding data etc)
#' but to also allow all the data processing to be run continuously, by
#' starting from the raw IMPORT data when running all Rmd code chunks.
#'
#' @param filename the filename of the dataset to load.  The dataset filename, if exported
#' from a Project Note in projectmanagr with sample_export() WILL be Prefixed with
#' `IMPORT_`.  The default value `IMPORT_samples.csv` is likely its name!
#'
#' @return The loaded TIBBLE with the "path" attribute set to imported sample filename:
#' this is the fielname with the `IMPORT_` prefix removed.
#'
#'@export
sample_import_2 <- function(filename="IMPORT_samples.csv") {

  # check filename is PREFIXED with IMPORT_
  if( substr(filename,1, 7) != "IMPORT_" ) {
    stop( paste0("  filename does not start with IMPORT_: ",  filename) )
  }

  # get the currently active source editor context - hoepfully a projectmanagr Project Note!
  context <- rstudioapi::getSourceEditorContext()

  # get/check project note DIR:
  DATA_DIR <- check_project_note_dir(context$path)

  filepath <- paste0(DATA_DIR, .Platform$file.sep, filename)

  if(file.exists(filepath) == FALSE) {
    stop( paste0("  Imported samples does not exist in Project Note : ",  DATA_DIR) )
  }

  # Load data
  # MAKE SURE ID is passed as a CHARACTER!
  samples <- readr::read_csv( filepath,
                              col_types = readr::cols( ID = readr::col_character()))

  samples

  # ADD path attribute - use the filename OMITTING the `IMPORT_` prefix:
  attr(samples, "path") <- substr(filename, 8, nchar(filename))
  # access with: attr(samples[[1]], "path")

  # save the new samples as CSV:
  readr::write_csv( samples, paste0( DATA_DIR, .Platform$file.sep, attr(samples, "path") )  )

  # return the tibble
  samples

}


#' Filter DataFrame to removed Re-Sampled and Exported Samples
#'
#' This returns a new copy of sampleDataFrame with all samples that have been resampled or exported
#' removed.  This is a convenience function for sample_add_data, as data can only be added to a
#' Sample DataFrame for samples that still exist within it!
#'
#' Not sure this is necessary for sample_add_data?  As could count number of rows which are blank in both
#' resample and export columns, if exist, and use this to check against input data vector lengths.  If
#' they match, can just create a new blank column, and fill rows with blank resample & export with the
#' correct values.
#'
#' However, this function may be useful in viewing AVAILABLE SAMPLES that remain in a DataFrame!
#'
#' @param sampleDataFrame A named list containing a SINGLE sample dataframe as a tibble.  If more than one
#' dataframe exists in the list, this method will fail (to guarantee the dataframe being processed is the
#' intended one).  Use sample_load() to load Single dataframe list for resampling.
#'
#'
sample_get_available <- function(sampleDataFrame) {

}


#' get all available samples from dataframes in dirtree
#'
#'
sample_get_available_all <- function(dirtree) {

}


#' Collate Sample Data
#'
#' This will return a composite dataframe that contains every data entry for a given sample,
#' extending back through all resampling and exporting, therefore providing access to all
#' conditions and metadata associated with the samples in a given sample TIBBLE.
#'
#'
sample_data_collate <- function(sampleDataFrame) {
  # check for subsample or import columns:

}


get_samples_paths <- function(samples_summary) {

  samples_paths <- unique(
    paste0(samples_summary$PATH,
           .Platform$file.sep,
           "__samples_", samples_summary$SAMPLE,
           ".csv")  )

  # replace any ending with /__samples_.csv with /__samples.csv
  samples_paths[ endsWith(samples_paths, "/__samples_.csv")] <- paste0(
    substr(
      samples_paths[ endsWith(samples_paths, "/__samples_.csv")],
      1,
      regexpr("/__samples_", samples_paths[ endsWith(samples_paths, "/__samples_.csv")] )
    ),
    "__samples.csv"  )

  # return the paths:
  samples_paths

}

#' Return Sample Code from path
#'
#'
#'
get_sample_from_path <- function(samples_path) {

  samples_name <- basename( samples_path )

  if(samples_name == "__samples.csv") {
    samples_name <- ""
  } else {
    samples_name <- substr(samples_name, 11, regexpr(".csv",samples_name )-1)
  }

  samples_name

}

#' convert samples_paths to project note paths
#'
#' samples_paths are __sample CSV files inside the Project DIR directly,
#' so just return the projectnote path from the DIR path - which is the parent
#' of the samples_apths.
get_project_note_paths <- function(samples_paths) {

  getProjectNotePathFromDir( dirname(samples_paths) )

}


get_datetime <- function() {

  # get datetime
  datetime <- Sys.time()

  # round to nearest minute:
  datetime <- round(datetime,"mins")

  # round to nearest 5 min
  datetime <- as.POSIXlt(datetime)
  datetime$min <- (datetime$min + 5/2) %/% 5 * 5

  # format datetime to use "/" and have a ":" between date and time
  datetime_split <- strsplit(as.character(datetime), " ")
  datetime_split[[1]][1] <- gsub("-", "/", datetime_split[[1]][1] )

  datetime_colon <- paste0( datetime_split[[1]][1], ":", datetime_split[[1]][2] )

  # remove seconds:
  datetime_colon <- substr(datetime_colon, 1, nchar(datetime_colon)-3)

  datetime_colon

}


#' Check Project Note DIR exists from path
#'
#' From a putative projectNotePath, extract the Project Note DIR name from this (the prefix of the
#' file name - up to '~_'), check this
#'
check_project_note_dir <- function(projectNotePath) {

  DATA_DIR <- substr(basename(projectNotePath), 1, regexpr("~_", basename(projectNotePath))-1 )
  # this will be a blank string if "~_" does not exist in basename(context$path)

  if(file.exists( paste0(dirname(projectNotePath), .Platform$file.sep, DATA_DIR) ) == FALSE ) {
    # the WORKING DIR in R Chunks is the CURRENT DOC
     # so this checks the DIR exists and that it exists next to the current sourceEditorContext file!
    stop( paste0("  Data Dir does not exist: ", paste0(DATA_DIR) ) )
  }

  # reutrn DATA_DIR:
  DATA_DIR

}


#' Check sampleDataFrame
#'
#' Checks the sampleDataFrame is a tibble, has attribute 'path', exists in current Rmd's DATA_DIR.
#'
#'
check_sample_data_frame <- function(sampleDataFrame, DATA_DIR) {

  if( tibble::is_tibble(sampleDataFrame) == FALSE ) {
    stop(
      paste0("  sampleDataFrame MUST be a Tibble: Use sample_load() to load samples as a Tibble." ) )
  }

  if(  is.null( attr(sampleDataFrame, "path") )  ) {
    stop(
      paste0("  sampleDataFrame MUST contain its 'path' attribute:  Use sample_load() to load a Single dataframe list." ) )
  }

  # check the sampleDataFrame already EXISTS in DATA_DIR:
  if(  file.exists( paste0(DATA_DIR, .Platform$file.sep, attr(sampleDataFrame, "path") ) ) == FALSE  ) {
    stop(
      paste0("  Passed sampleDataFrame does not exist in Project Note DIR: ", paste0(DATA_DIR, .Platform$file.sep, attr(sampleDataFrame, "path") ) ) )
  }

}



check_samples_summary <- function( samples_summary ) {

  # CHECK samples_summary:
  # it contains all columns expected in this tibble:
  # "ID"     "SAMPLE" "COUNT"  "EXP"    "TITLE"  "PATH"   "REPS"
  if(
    (length(colnames(samples_summary))==6 &&
      all(colnames(samples_summary) ==c("ID","SAMPLE","COUNT","EXP","TITLE","PATH" )) == FALSE )
     |
    (length(colnames(samples_summary))==7 &&
      all(colnames(samples_summary) ==c("ID","SAMPLE","COUNT","EXP","TITLE","PATH", "REPS" )) == FALSE )
      ) {
    stop( paste0("  passed tibble does not have correct colnames: ", colnames(samples_summary) ) )
  }

  # check if REPS exists - if not, and COUNT is 1 across all IDs, just add this col with 1 for each ID
  if( any(colnames(samples_summary) == "REPS") == FALSE ) {
    if( any( samples_summary$COUNT > 1 ) ) {
      stop( paste0("  passed tibble does not contain REPS column: ", colnames(samples_summary) ) )
    } else {
      REPS <- 1
      samples_summary <- dplyr::mutate(
                            selected_samples,
                            REPS
                          )
    }
  }

  # CHECK samples_summary -> REPS is <= COUNT for all rows
  if( all( samples_summary$REPS <= samples_summary$COUNT ) == FALSE ) {
    stop( paste0("  passed tibble REPS column value exceeds COUNT column value: ", samples_summary ) )
  }

  # return samples_summary in case added REPS
  samples_summary

}


check_vars <- function(vars, varNames, colNames, IDs, rep_select, section_nums, overwrite) {

  # check each varName in varNames
  for( i in 1:length(varNames) ) {

    varName <- varNames[i]
    var_vec <- vars[[i]]

    #if(is.element(varName, colNames) ) {
    # stop( paste0("  Column with name Vector already exists - are you adding the data twice?: ", i) )
    #} # DO NOT DO THIS - as want to add to existing column on occasion:
    #  If adding to the BLANK (NA) rows in that column
    #  If OVERWRITING the content in that column - use overwrite LOGICAL value for this

    # Check each Vector in vars - var_vec:

    # does  have the correct length?
    # MUST BE length of 1, length of IDs or length of rep_select

    if(  length(var_vec) != 1 &&
         length(var_vec) != length(IDs) &&
         length(var_vec) != sum( lengths(rep_select) )  ) {
      stop( paste0("  Vector is not of correct length to add to dataframe: ", varName) )
    }

    if( varName == "" ) { # varName is BLANK if not using 'colName=vector' syntax
      stop(paste0(" Vector names are missing - add vectors with 'colName=vector' syntax: ", varName))
    }


    # check the samples dataFrame - is it valid to enter the data to it?
    if(is.element(varName, colNames) ) { # only check samples if varName is already a column!

      # for each ID in IDs, check the relevant indices
      for( i in 1:length(IDs) ) {

        # get the number of sections, and cumulative sections:
        section_num <- section_nums$sections[ section_nums$ID == IDs[i] ]
        section_last <- section_nums$sections_cum[ section_nums$ID == IDs[i] ]
        section_first <- section_last - (section_num - 1) # -1 as the first section is ON section_first

        # get the current rep_select vector - check all selected replicates are VALID and AVAILABLE:
        # NB if rep_select was all, its now a vector containing section_num vector!
        reps <- rep_select[[i]]

        if( all(reps <= section_num) == FALSE) {
          # one of the reps is set to too high an index!
          stop( paste0("  Not enough replicates in ID: ", IDs[i], " Replicate number: ",  section_num) )
        }

        # convert reps into indices:
        rep_indices <- (reps - 1) + section_first # -1 as the first section is ON section_first

        # check each index in resample and export - if not BLANK/NA, stop!
        for( j in rep_indices) {

          if(  (! overwrite) && (! is.na( samples[[varName]][j] ) )  ) {
            stop( paste0("  Selected sample already contains data: ", IDs[i], " index: ",  j, " to overwrite - set overwrite to TRUE." ) )
          }

        }

      }
    }


  }

}


check_vars_group <- function(vars, varNames, colNames, by_group, overwrite) {

  # check each varName in varNames
  for( i in 1:length(varNames) ) {

    varName <- varNames[i]
    var_vec <- vars[[i]]

    #if(is.element(varName, colNames) ) {
    # stop( paste0("  Column with name Vector already exists - are you adding the data twice?: ", i) )
    #} # DO NOT DO THIS - as want to add to existing column with ADDITIONAL CALLS to add_data_by_group
    #  If adding to the BLANK (NA) rows in that column
    #  If OVERWRITING the content in that column - use overwrite LOGICAL value for this

    # Check each Vector in vars - var_vec:
      # does  have the correct length? MUST BE length of 1
    if(  length(var_vec) != 1  ) {
      stop( paste0("  Vector is not of correct length to add to dataframe: ", varName) )
    }

    if( varName == "" ) { # varName is BLANK if not using 'colName=vector' syntax
      stop(paste0(" Vector names are missing - add vectors with 'colName=vector' syntax: ", varName))
    }

    # check the samples dataFrame - is it valid to enter the data to it?
    if(is.element(varName, colNames) ) { # only check samples if varName is already a column!

      # for each ID in IDs, check the relevant indices
      for( j in 1:length(samples[[varName]]) ) {

        if(  (! overwrite) &&
              samples[[ by_group[1] ]][j] == by_group[2] &&
               (! is.na( samples[[varName]][j] ) )  ) {

          stop(
            paste0("  Sample in group already contains data: ", samples[[""]], " column: ",  varName, " to overwrite - set overwrite to TRUE." ) )

        }
      }
    }
  }
}


check_group <- function(by_group, samples) {

  if(length(by_group) != 2) {
    stop( paste0("  by_group not of length 2: ", by_group ) )
  }

  colNames <- names(samples)

  if( is.element(by_group[1], colNames) == FALSE ) {
    stop( paste0("  GROUP column does not exist in samples: ", by_group[1] ) )
  }

  group <- dplyr::select(samples, by_group[1])[[1]] # return vector

  if( is.element(by_group[2], group) == FALSE ) {
    stop( paste0("  GROUP values does not exist in GROUP column: ", by_group[1] ) )
  }


}


check_IDs <- function(IDs, originalIDs) {
  if(IDs[1] != "all") {
    # calculate the row indices for the IDs presented:
    ID_indices <- match(IDs, originalIDs)
    if( anyNA(ID_indices) ) {
      stop( paste0("  IDs contains values that do not exist in dataFrame: ", IDs ) )
    }
    # ALSO just make sure IDs only contains UNIQUE values:
    IDs_count <- length(IDs)
    IDs_orig <- IDs
    IDs <- unique(IDs)
    if( IDs_count != length(IDs) ) {
      stop( paste0("  IDs contains a duplicate: ", IDs_orig ) )
    }
    # if this passes, IDs now contains valid IDs that are all unique
  } else {
    IDs <- unique(originalIDs)
    # set IDs to the UNIQUE SET of sample$ID -
    # if there are replicates in the current sample DataFrame, the ID vector will contain
    # replicates of the ID value!
  }
  # The IDs value is now a vector containing the sample IDs to process!

  # return:
  IDs

}


check_rep_select <- function(rep_select, IDs, section_nums) {


  if(rep_select[1] != "all") {
    # rep_select should be a list of integer vectors:
    # check:
    if( typeof(rep_select) != "list" ) {
      stop( paste0("  rep_select MUST be of type list - current type: ", typeof(rep_select) ) )
    }
    if(  any( sapply(rep_select, mode) != "numeric" )  ) {
      stop( paste0("  rep_select MUST contain type numeric." ) )
    }

    if( length(rep_select) == 1 ) { # LENGTH OF THE LIST IS ONE - it contains ONE INT VECTOR
      # always valid - will select rep_select number of replicates from each ID:
      # REPLICATE rep_select by number of IDs:

      rep_select <- rep(rep_select, length(IDs) )

    } else if( length(rep_select) == length(IDs) ) {
      # if using all IDs, rep_select must be the length of UNIQUE samples$ID
      # IDs is the length of UNIQUE sample$ID - see above
      # if using selected IDs, rep_select must be the length of IDs

    } else {
      # else, the values for rep_select are invalid!
      stop( paste0("  Length of rep_select is invalid - check ID length or IDs selection?: ", rep_select ) )
    }

  }

  # if rep_select is all - set a LIST containing a SET of Vectors that are a SEQUENCE from 1:(sections)
  # order is preserved with IDs!
  if(rep_select[1] == "all") {

    rep_select <- list()

    for(i in 1:length(section_nums$sections) ) {
      ss <- seq(section_nums$sections[i])
      rep_select[[i]] <- ss

    }

  }

  # return:
  rep_select

}


check_rep_create <- function(rep_create, IDs) {

  if( mode(rep_create) != "numeric" ) {
    stop( paste0("  rep_create MUST be type numeric." ) )
  }

  if( length(rep_create) == 1 ) {
    # indicates ALL IDs will be replicated by the number in rep_create

  } else if( length(rep_create) == length(IDs) ) {
    # indicates each ID in IDs is replicated by the number specified at its index

  } else {
    # else, the values for rep_create are invalid!
    stop( paste0("  Length of rep_create is invalid - check ID length or IDs selection?: ", rep_create ) )
  }
}




check_valid_selection <- function( samples, IDs, rep_select, section_nums ) {

  # check and store if export or resample cols exist:
  colNames <- names(samples)
  resample_col <- FALSE
  export_col <- FALSE
  if( any(colNames == "export") == TRUE ) {
    export_col = TRUE
  }
  if( any(colNames == "resample") == TRUE ) {
    resample_col = TRUE
  }

  #if( export_col == TRUE || resample_col == TRUE ) {

  #} else {

    # need to check each ID and rep_select index in samples is VALID
    # i.e. the sample has not already been resampled or exported!


    # for each ID in IDs, check the relevant indices
    for( i in 1:length(IDs) ) {

      # get the number of sections, and cumulative sections:
      section_num <- section_nums$sections[ section_nums$ID == IDs[i] ]
      section_last <- section_nums$sections_cum[ section_nums$ID == IDs[i] ]
      section_first <- section_last - (section_num - 1) # -1 as the first section is ON section_first

      # get the current rep_select vector - check all selected replicates are VALID and AVAILABLE:
      # NB if rep_select was all, its now a vector containing section_num vector!
      reps <- rep_select[[i]]

      if( all(reps <= section_num) == FALSE) {
        # one of the reps is set to too high an index!
        stop( paste0("  Not enough replicates in ID: ", IDs[i], " Replicate number: ",  section_num) )
      }

      # convert reps into indices:
      rep_indices <- (reps - 1) + section_first # -1 as the first section is ON section_first

      # check each index in resample and export - if not BLANK/NA, stop!
      for( j in rep_indices) {

        # if selection has been resampled, stop the function:
        if( resample_col && (samples$resample[j] != "") ) {
          stop( paste0("  Selected sample has already been re-sampled: ", IDs[i], " index: ",  j ) )
        }

        # if selection has been exported, stop the function:
        if( export_col && is.na(samples$export[j]) == FALSE && (samples$export[j] != "") ) {
          stop( paste0("  Selected sample has already been exported: ", IDs[i], " index: ",  j ) )
        }

      }

    }

  #}


}



check_resample_vector <- function(resample_vector) {

  # Check resample_vector is character:
  if(is.character(resample_vector) == FALSE) {
    stop( paste0("  'resample_vector' not a character vector: ", resample_vector) )
  }

  # Convert resample_vector into space-separated string - to add to resample column!:
  resample_vector_string <- paste(resample_vector, collapse=" ")

  # return the string:
  resample_vector_string

}


check_resample_dt <- function(resample_dt) {

  # convert resample_dt to datetime object - to add to resample_dt column!:
  # the NUMBER of dt objects here should be 1, length if IDs, or length of sum of rep_select lengths
  resample_dt_vector <- lubridate::as_datetime(resample_dt)

  # check resample_dt_vector is valid datetime:
  if( lubridate::is.instant(resample_dt_vector) == FALSE ) {
    stop( paste0("  'resample_dt' not a datetime object: ", resample_dt_vector) )
  }

  resample_dt_vector

}
