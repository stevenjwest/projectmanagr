#' Get Sub Note Path
#'
#' Assumes projectDocPath is the path to the Project Doc.
#'
#' The headerNoteRelPath is assumed to be the RELATIVE path from projectDocPath
#' to the headerNote.
#'
#'
getSubNotePath <- function( projectDocPath, headerNoteRelPath, subNoteName ) {

  # compute the headerNotePath from projectDocPath and headerNoteRelPath (rel to ProjectDocPath!)
  headerNotePath <- computePath(projectDocPath, headerNoteRelPath)

  # extract headerNoteDir from path - from the ~_ separator, replace fileName with file.sep
  headerNoteDir <- paste( substring(headerNotePath, 1, regexpr("~_", headerNotePath, fixed=TRUE)-1 ), sep="")

  # get the NEXT subNote Prefix:
  subNotePrefix <- getNextGroupPrefix(headerNoteDir)

  # SubNote will be in the headerNoteDir:
  subNotePath <- paste(headerNoteDir, .Platform$file.sep, subNotePrefix, "~_", subNoteName, ".Rmd", sep="" )

  subNotePath


}
