#' Get File Type
#'
#' Determine if the current file is a PROJECT DOC (it is inside the PROJECTS/ Directory),
#' a HEADER GROUP NOTE (contains the string "-00~_"), or a SIMPLE or SUB Project Note (any
#' other file).
#'
#' Returns the relevant String:  DOC, HEAD, NOTE
#'
#'@param fileSystemPath the Absolute path to the file.
#'
#'
getFileType <- function( fileSystemPath ) {

  TYPE <- ""

  cat( "\ngetFileType - fileSystemPath: ", fileSystemPath, "\n" )

  cat( "\ngetFileType - basename(dirname(fileSystemPath)): ", basename(dirname(fileSystemPath)), "\n" )

  if(basename(dirname(fileSystemPath)) == "PROJECTS") {
    TYPE <- "DOC"
  }
  else if( grepl("-00~", fileSystemPath, fixed = TRUE) ) {
    TYPE <- "HEAD"
  }
  else {
    TYPE <- "NOTE"
  }

  TYPE

}
