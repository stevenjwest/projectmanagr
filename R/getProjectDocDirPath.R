#' Get Project Doc Dir Path
#'
#' Retrieves the projectDoc DIR from the projectDoc path:
#'
#' - Removes the projectDocTitle and EXT from the path, and returns
#' the ProjectDoc Dir - the path plus the projectDoc PREFIX.
#'
getProjectDocDirPath <- function( projectDocPath ) {

  substring(projectDocPath, first=1, last=regexpr("~_", projectDocPath, fixed=TRUE)-1 )

}
