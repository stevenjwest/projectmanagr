#' Search Dir Tree
#'
#' Searches all Project Docs & Notes in a Dir Tree within an Organisation,
#' returning a datatable with FILENAME, CONTEXT, LOCATION LINE columns.
#'
#' @param path Dir Tree Path for searching text files.
#'
#' @param searchTerm The sring to search for.
#'
#' @param updateProgress Object to show progress in shiny gadget.
#'
#' @param settings Settings list from projectmanagr config
#'
#' @param orgPath The organisation path where search takes place.
#'
#' @param ignoreCase Whether search should ignore case of searchTerm. TRUE by
#' default.
#'
#' @param fixed When fixed is TRUE, no regular expression patterns are parsed.
#' More efficient search is performed with fixed TRUE.
#'
search_dir_tree <- function(path, searchTerm, updateProgress = NULL,
                            settings, orgPath, ignoreCase=TRUE, fixed=TRUE) {

  cat( "\nprojectmanagr::search_dir_tree():\n" )
  cat("  ignoreCase : ", ignoreCase, "\n")
  cat("  fixed : ", fixed, "\n")


  #### instance variables ####

  # if not an absolute path:
  if( R.utils::isAbsolutePath(path) == FALSE ) {
    path <- R.utils::getAbsolutePath(path )
  }

  # normalize path - remove HOME REF ~
  path <- fs::path_expand(path)



  ##############################################################################

  # get config templates settings yml
  confPath <- get_config_dir(orgPath)
  tempPath <- get_template_dir(orgPath)
  settings <- get_settings_yml(orgPath)

  # get dirs in root to EXCLUDE from search
  volPath <- get_volumes_dir(orgPath, settings)
  sitePath <- get_site_dir(orgPath, settings)
  weeklyjournalPath <- get_weekly_journal_dir(orgPath, settings)

  # get all project notes in orgPath RECURSIVELY
  fileList <- list()
  filePaths <- get_file_list_to_project_notes(
    fileList, orgPath, settings,
    pathExclusions = c(confPath, volPath, sitePath, weeklyjournalPath) )
  # get all Project Docs/Notes inside path - they all contain "~_" in filename and end with .Rmd
  filePaths <- fs::path(unlist(filePaths))
  projDocsNotesList <- filePaths[ (regexpr(settings[["ProjectPrefixSep"]], filePaths) > 0) &
                              (regexpr(settings[["FileTypeSuffix"]], filePaths) > 0) ]

  ##############################################################################



  # get all Project Docs/Notes inside path - they all contain "~_" in filename and end with .Rmd
  #projDocsNotesList <- list.files(path, pattern=paste0("*", settings[["FileTypeSuffix"]]), full.names = TRUE, recursive=TRUE)
  #projDocsNotesList <- projDocsNotesList[ (regexpr(settings[["ProjectPrefixSep"]], projDocsNotesList) > 0) & (regexpr(settings[["FileTypeSuffix"]], projDocsNotesList) > 0) ]

  projDocsNotesListLength <- length(projDocsNotesList) # for updateProgress

  # generate a blank table to initialise addin with
  FILENAME <- ""
  CONTEXT <- ""
  LOCATION <- "" # storing location but not adding it to the table
  LINE <- "" # storing line byt not adding to table

  if( ignoreCase == TRUE ) { # use tolower() as ignore.case doesnt work with fixed in grep
    # convert search term to lower OUTSIDE for loop
    searchTerm <- tolower(searchTerm)
  }

  for(s in projDocsNotesList) {

    # for each Rmd
    fn <-  basename(s)
    cat( "\n  reading file : ", fn, "\n" )

    # update progress of search
    if (is.function(updateProgress)) {
      progressFraction <- match(s, projDocsNotesList) / projDocsNotesListLength
      text <- paste0("Rmd file : ", fn )
      updateProgress(value = progressFraction, detail = text)
    }

    # open rmd
    rmd_file_conn <- file( s )
    rmd_contents <- readLines( rmd_file_conn )
    close(rmd_file_conn)

    # grep for search term
    if( ignoreCase == TRUE ) { # use tolower() as ignore.case doesnt work with fixed in grep
      grepRes <- grep(searchTerm, tolower(rmd_contents), fixed=fixed)
    } else {
      grepRes <- grep(searchTerm, rmd_contents, fixed=fixed)
    }
    #grepRes <- grep(searchTerm, rmd_contents, ignore.case = ignoreCase, fixed=fixed)

    # add results to searchResults table for each grep'd line
    for(g in grepRes) {
      FILENAME <- c(FILENAME, fn)
      CONTEXT <- c(CONTEXT, rmd_contents[g])
      LOCATION <- c(LOCATION, s)
      LINE <- c(LINE, g)
    } # end g

  } # end s

  # create table from vectors
  FILENAME <- FILENAME[2:length(FILENAME)] # trim first val - blank ""
  CONTEXT <- CONTEXT[2:length(CONTEXT)]
  LOCATION <- LOCATION[2:length(LOCATION)]
  LINE <- LINE[2:length(LINE)]

  # form table
  searchResults <- tibble::tibble(FILENAME, CONTEXT)

  # return list
  list(searchResults, list(LOCATION, LINE) )

}
