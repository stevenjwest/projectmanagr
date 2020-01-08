#' Check Programme Sub Dir
#'
#' Searches fileSystemPath's parent directories to identify
#' a Project Organisation directory.  This is identified by
#' finding a 'config/' directory and a 'config/templates/' directory.
#'
#' For the fileSystemPath to be successfully returned, the directory
#' MUST be at least in level below a PROGRAMME directory.
#'
#' If an Organisation path is identified, it is returned, otherwise
#' the function returns a BLANK string "".
#'
#'
checkProgSubDir <- function( fileSystemPath ) {

  # Check fileSystemPath is in a Programme DIR, a sub-dir to the root of an ORGANISATION:
  orgPath <- dirname(dirname(fileSystemPath))

  # look for the config/ and templates/ dirs:
  confPath = paste(orgPath, .Platform$file.sep, "config" , sep="")
  tempPath = paste(confPath, .Platform$file.sep, "templates" , sep="")

  orgPath2 <- "/" # use this as placeholder of PREVIOUS orgPath - if orgPath == orgPath2, then have not found config or template!

  while(  !( file.exists(confPath) && file.exists(tempPath) )  ) {
    orgPath2 <- orgPath # save in placeholder
    orgPath <- dirname(orgPath)
    if( orgPath2 == orgPath ) {
      fileSystemPath <- ""
      break
    }
    confPath <- paste(orgPath, .Platform$file.sep, "config" , sep="")
    tempPath <- paste(confPath, .Platform$file.sep, "templates", sep="")
  }


  fileSystemPath

}
