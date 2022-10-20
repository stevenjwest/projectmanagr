#' Rename a Project Doc Goal/Del/Task
#'
#' Renames a Project Doc Goal/Del/Task and updates this in all linked Project
#' Notes.
#'
#' @param oldProjectCompPath defines the path to the Project Comp - should point to the Project Comp Rmd file
#' @param newProjectCompName defines the NEW Project Comp File name
#' @param newProjectCompTitle defines the NEW Project Comp Title - written to its Rmd file.  By default this
#' is the oldProjectCompPath, with spaces replacing "-" and "_".
#'
#'@export
renameProjectDocGDT <- function( oldProjectCompPath, newProjectCompName, newProjectCompTitle="" ) {


  cat( "\nprojectmanagr::renameProjectComp():\n" )


  # check newProjectCompName contains NO SPACES:
  if( grepl("\\s+", newProjectCompName) ) {
    stop( paste0("  newProjectCompName contains a SPACE: ",newProjectCompName) )
  }

  # define the newProjectCompTitle:
  if( nchar(newProjectCompTitle)==0 ) {
    newProjectCompTitle <- gsub("-", " ", gsub("_", " ", newProjectCompName) )
  }

  ### EDITING THE PROJECT Comp: ###

  # Find Programme path
  progPath <- findProgDir(oldProjectCompPath)

  if( progPath == "" ) {
    # findProgDir returns a blank character vector if oldProjectCompPath is NOT in a PROGRAMME DIR
    stop( paste0("  oldProjectCompPath is not in a PROGRAMME Directory: ", oldProjectCompPath) )
  }

  # identify the orgPath:
  orgPath <- findOrgDir(oldProjectCompPath)

  # get old project Comp name:
  oldProjectCompName <- basename(oldProjectCompPath)


  ### FIRST - rename the project Comp file:

  # get Project Comp prefix:
  prefix <- getProjectPrefixFromName(oldProjectCompName)

  # define new project Comp path:
  newProjectCompPath <- paste0( dirname(oldProjectCompPath), .Platform$file.sep, prefix, "~_", newProjectCompName, ".Rmd" )

  # re-generate newProjectCompName with prefix and .Rmd suffix
  newProjectCompName <- basename(newProjectCompPath)

  # rename:
  done <- file.rename(oldProjectCompPath, newProjectCompPath )

  if(done == TRUE) {
    cat( "  Renamed Project Comp file from: ", oldProjectCompName , " to: ", newProjectCompName, "\n" )
  } else {
    stop( paste0("  Renamed Project Comp file unsuccessful: ", basename(newProjectCompPath) ) )
  }


  ### SECOND - open, get oldtitle, update with newTitle:
  fileConn <- file( newProjectCompPath )
  contents <- readLines( fileConn )
  close(fileConn)

  # find title line - via getProjCompTitle()
  line <- grep( paste0("title: '"), contents) # get first instance of line beginning with title:
  contents[line] <- sub(getProjCompTitle(contents), newProjectCompTitle, contents[line]) # replace old with new title

  # write subNote file to disk:
  fileConn <- file(newProjectCompPath)
  writeLines(contents, fileConn)
  close(fileConn)

  cat( "  Written new Project Comp title to Rmd: ", newProjectCompTitle, "\n" )

  ### THIRD - replace oldProjectCompName/Title with newProjectCompName/Title THROUGHOUT the Organisation:
  updateAllLinks(orgPath, oldProjectCompName, newProjectCompName) # auto-computes the titles now!
  #updateAllLinks( orgPath, oldProjectCompName, newProjectCompName, oldProjectCompTitle, newProjectCompTitle )

  # FINALLY - re-open the file
  rstudioapi::navigateToFile(newProjectCompPath)

}
