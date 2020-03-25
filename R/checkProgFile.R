#' Check Programme Dir
#'
#' Checks fileSystemPath is pointing to a PROGRAMME directory.
#' A Programme Directory is a direct sub-dir to a Organisation
#' directory, so this method establishes this is the case by checking
#' the parent Dir to fileSystemPath, to see if 'config/' and
#' 'config/templates/' exist.
#'
#' Secondly, it checks the 'PROJECTS/' directory exists in this
#' putative Programme Directory.
#'
#' If a Programme path is confirmed, it is returned, otherwise
#' the function returns a BLANK string "".
#'
#'
checkProgFile <- function( fileSystemPath ) {

  # Check fileSystemPath is in a Programme DIR, a sub-dir to the root of an ORGANISATION:
  orgPath <- dirname(fileSystemPath)

  # look for the config/ and templates/ dirs:
  confPath = paste(orgPath, .Platform$file.sep, "config" , sep="")
  tempPath = paste(confPath, .Platform$file.sep, "templates" , sep="")

  if(  !( file.exists(confPath) && file.exists(tempPath) )  ) {
    fileSystemPath <- ""
  }

  # fileSystemPath is therefore in a PROGRAMME DIR

  fileSystemPath

}
