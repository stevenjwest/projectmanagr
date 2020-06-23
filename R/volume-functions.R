#' Create Volume
#'
#' Create a new Volume in the volumes/ dir of the projectmanagr
#' Organisation.  The function Symlinks the dir at sourcePath to a new
#' Symlink made in the Organisation volumes/ directory, with
#' name volName.
#'
#' @param sourcePath Path to the source directory. If the dir at sourcePath
#' does not exist, this function will terminate.
#'
#' @param volName The naem of the new volume. If a volume called volName already
#' exists, it is not overwritten, and this function will terminate.
#'
#' @param orgPath Optional argument to specify the location of the organisation with
#' orgPath, which by default is the current working directory.
#'
#' @export
createVolume <- function( sourcePath, volName, orgPath=getwd() ) {

  cat( "\nprojectmanagr::createVolume():\n" )

  # check volName contains NO SPACES:
  if( grepl("\\s+", volName) ) {
    stop( paste0("  volName contains a SPACE: ", volName) )
  }

  # set/check orgPath to the org root dir:
  orgPath <- findOrgDir(orgPath)

  if(orgPath == "" ) {
    stop( paste0("  orgPath is not in a projectmanagr ROOT Directory: ", orgPath) )
  }

  if( file.exists(sourcePath) == FALSE ) {
    stop( paste0("  sourcePath does not exist: ", sourcePath) )
  }

  volPath <- paste0(orgPath, .Platform$file.sep, "volumes", .Platform$file.sep, volName)

  if( file.exists(volPath) == TRUE ) {
    stop( paste0("  volume already exists: ", volPath) )
  }

  # create symlink:
  done <- file.symlink(sourcePath, volPath )

  if(!done) {
    stop( paste0("  volumes symlink could not be created: ", volPath) )
  }

  cat( "  Made volume symlink: ", volPath, "\n" )

}


#' Move Project Note DIR to Volume
#'
#'
moveToVolume <- function(projectNotePath, volumeName) {

}


