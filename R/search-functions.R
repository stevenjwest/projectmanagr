#' Search Dir Tree
#'
#' Searches the Dir Tree within an Organisation, returning a datatable
#' with FILENAME, CONTEXT, LOCATION columns.
#'
search_dir_tree <- function(path, searchTerm, updateProgress = NULL, settings, orgPath) {

  cat( "\nprojectmanagr::search_dir_tree():\n" )


  #### instance variables ####

  # if not an absolute path:
  if( R.utils::isAbsolutePath(path) == FALSE ) {
    path <- R.utils::getAbsolutePath(path )
  }

  # normalize path - remove HOME REF ~
  path <- normalizePath(path)

  # get all Project Docs/Notes inside path - they all contain "~_" in filename and end with .Rmd
  samplesList <- list.files(path, pattern=paste0("*", settings[["FileTypeSuffix"]]), full.names = TRUE, recursive=TRUE)
  samplesList <- samplesList[ (regexpr(settings[["ProjectPrefixSep"]], samplesList) > 0) & (regexpr(settings[["FileTypeSuffix"]], samplesList) > 0) ]

  samplesListLength <- length(samplesList) # for updateProgress

  # generate a blank table to initialise addin with
  FILENAME <- ""
  CONTEXT <- ""
  LOCATION <- "" # storing location but not adding it to the table
  LINE <- "" # storing line byt not adding to table

  for(s in samplesList) {

    # for each Rmd
    fn <-  basename(s)
    cat( "\n  reading file : ", fn, "\n" )

    # update progress of search
    if (is.function(updateProgress)) {
      progressFraction <- match(s, samplesList) / samplesListLength
      text <- paste0("Rmd file : ", fn )
      updateProgress(value = progressFraction, detail = text)
    }

    # open rmd
    rmd_file_conn <- file( s )
    rmd_contents <- readLines( rmd_file_conn )
    close(rmd_file_conn)

    # grep for search term
    grepRes <- grep(searchTerm, rmd_contents)

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
