#' Rename a Project Comp
#'
#' Renames a Project Comp at oldProjectCompPath with file name (oldProjectCompPath), and newProjectCompTitle.
#'
#' This also modifies all LINKS to this file throughout the Organisation - updating them to use the new
#' project Comp name and title.
#'
#' NOTE - this DOES NOT modify the PREFIX for a Project Comp (set by the PROGRAMME)!
#'
#'
#' @param oldProjectCompPath defines the path to the Project Comp - should point to the Project Comp Rmd file
#' @param oldProjectCompPath defines the NEW Project Comp File name
#' @param newProjectCompTitle defines the NEW Project Comp Title - written to its Rmd file.  By default this
#' is the oldProjectCompPath, with spaces replacing "-" and "_".
#'
#'@export
renameProjectComp <- function( oldProjectCompPath, newProjectCompName, newProjectCompTitle="" ) {


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
  }
  else {
    stop( paste0("  Renamed Project Comp file unsuccessful: ", basename(newProjectCompPath) ) )
  }


  ### SECOND - open, get oldtitle, update with newTitle:
  fileConn <- file( newProjectCompPath )
  contents <- readLines( fileConn )
  close(fileConn)

  # find title line:
  line <- grep( paste0("title: '", prefix), contents)

  # compute old title - +2 as always have "~ " after prefix, -1 as have "'" at end of title
  oldProjectCompTitle <- substr(contents[line],
                               regexpr( paste0(prefix), contents[line]) + (nchar(prefix) + 2),
                               nchar(contents[line])-1)


  ### THIRD - replace oldProjectCompName/Title with newProjectCompName/Title THROUGHOUT the Organisation:

  updateAllLinks( orgPath, oldProjectCompName, oldProjectCompTitle, newProjectCompName, newProjectCompTitle )


}
