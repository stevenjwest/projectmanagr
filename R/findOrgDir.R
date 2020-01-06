#' Find Organisation Dir
#'
#' Searches fileSystemPath's parent directories to identify
#' a Project Organisation directory.  This is identified by
#' finding a 'config/' directory and a 'config/templates/' directory.
#'
#' If an Organisation path is identified, it is returned, otherwise
#' the function returns a BLANK string "".
#'
#'
findOrgDir <- function( fileSystemPath ) {

  # Check fileSystemPath is at the root of an ORGANISATION:

  # look for the .config/ and templates/ dirs:
  confPath <- paste(fileSystemPath, .Platform$file.sep, "config" , sep="")
  tempPath <- paste(confPath, .Platform$file.sep, "templates" , sep="")

  fileSystemPath2 <- "/" # use this as placeholder of PREVIOUS orgPath - if orgPath == orgPath2, then have not found config or template!

  while(  !( file.exists(confPath) && file.exists(tempPath) )  ) {
    fileSystemPath2 <- fileSystemPath # save in placeholder
    fileSystemPath <- dirname(fileSystemPath)
    if( fileSystemPath2 == fileSystemPath ) {
      fileSystemPath <- ""
      break
    }
    confPath <- paste(fileSystemPath, .Platform$file.sep, "config" , sep="")
    tempPath <- paste(confPath, .Platform$file.sep, "templates", sep="")
  }

  fileSystemPath

}
