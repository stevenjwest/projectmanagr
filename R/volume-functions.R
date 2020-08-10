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
#' @param volName The name of the new volume. If a volume called volName already
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
#' Creates the Project Note DIR in new volume, copies all contents from
#' the old Project Note DIR location into the new Project Note DIR location,
#' deletes the old Project Note DIR, and symlinks to the new Project Note DIR.
#'
#' Finally, changes the location of the volume in the Project Note itself.
#'
#'
moveToVolume <- function(volName, projectNotePath) {


  cat( "\nprojectmanagr::moveVolume():\n" )

  # check volName contains NO SPACES:
  if( grepl("\\s+", volName) ) {
    stop( paste0("  volName contains a SPACE: ", volName) )
  }

  # normalise path - remove "~" ref:
  projectNotePath <- normalizePath(projectNotePath)

  # check projectNotePath is a Project Note:
  type <- getFileType(projectNotePath)

  if(type != "NOTE" ) {
    stop( paste0("  projectNotePath is not a bottom level note: ", projectNotePath) )
  }

  # Get orgPath from projectNotePath:
  orgPath <- findOrgDir(projectNotePath)

  if(orgPath == "" ) {
    stop( paste0("  projectNotePath is not inside a projectmanagr ROOT Directory: ", projectNotePath) )
  }

  # get volumes path:
  volsPath <- paste0( orgPath, .Platform$file.sep, "volumes")
  volPath <- paste0( orgPath, .Platform$file.sep, "volumes", .Platform$file.sep, volName )

  # check volName EXISTS in volsPath:
  if( file.exists(volPath) == FALSE ) {
    stop( paste0("  volName does not exist: ", volName) )
  }

  # open the projectNote at projectNotePath:
  projectNoteFileConn <- file( projectNotePath )
  projectNoteContents <- readLines( projectNoteFileConn )
  close(projectNoteFileConn)

  # get the volumes path:
  # ASSUMES the vol path is +3 lines from the title DATA STORAGE:
  oldVolPathRel <- projectNoteContents[ ( grep("# DATA STORAGE:", projectNoteContents) + 3 ) ]

  # get the ABSOLUTE path:
  oldVolPathAbs <- R.utils::getAbsolutePath( paste0(projectNotePath, .Platform$file.sep, "..", .Platform$file.sep, oldVolPathRel) )

  # get the path from volumes directory
  # this will BEGIN with the NAME of the OLD VOLUME
  oldVolPathFromVols <- R.utils::getRelativePath(oldVolPathAbs, relativeTo=volsPath)

  # get the path to the data DIR INSIDE the Old Volume:
  oldVolPathFromVol <- substr( oldVolPathFromVols, regexpr(.Platform$file.sep, oldVolPathFromVols)+1, nchar(oldVolPathFromVols) )


  ### Create NEW Project Note DIR on volPath ###

  # generate abs Vol Path:
  newVolPathAbs <- paste0( volPath, .Platform$file.sep, oldVolPathFromVol )

  # create DIR:
  done <- dir.create(newVolPathAbs, recursive=TRUE) # write all DIRs necessary
  if(!done) {
    stop( paste0("  new Project Note Dir could not be created: ", newVolPathAbs) )
  }

  cat( "  Made new Project Note Dir: ", newVolPathAbs, "\n" )


  ### Copy all files form old to new volPath ###

  # copy files RECURSIVELY:
  done <- file.copy(from=oldVolPathAbs, to=dirname(newVolPathAbs), overwrite = TRUE, recursive = TRUE, copy.mode = TRUE)

  if(!done) {
    stop( paste0("  new Project Note Dir contents could not be copied - from: ", oldVolPathAbs) )
  }

  cat( "  Copied new Project Note Dir contents: ", newVolPathAbs, "\n" )


  ### Symlink to new volPath   ###

  # THEN make the symlink to this, putting the symlink in the projectNotePath:
  projectNotePrefix <- substr(basename(projectNotePath), 1, regexpr("~_", basename(projectNotePath))-1 )
  noteDirSymPath <- paste0( dirname(projectNotePath), .Platform$file.sep, projectNotePrefix )

  # remove old symlink:
  done <- file.remove(noteDirSymPath)

  if(!done) {
    stop( paste0("  Project Note symlink could not be removed: ", noteDirSymPath ) )
  }

  # write new symLink:
  symLink <- R.utils::getRelativePath( newVolPathAbs, relativeTo=projectNotePath)
  symLink <- substring(symLink, first=4, last=nchar(symLink)) # remove first `../`

  # symLink (the relative path to the DIR), write it at noteDirSymPath
  done <- file.symlink( symLink, noteDirSymPath )

  if(!done) {
    stop( paste0("  Project Note symlink could not be made to volume: ", noteDirSymPath, " ", symLink) )
  }

  cat( "  Symlinked new Project Note Dir: ", noteDirSymPath, "\n" )


  ### Write contents to Project Note   ###

  # write symLink to projectNote
  projectNoteContents[ ( grep("# DATA STORAGE:", projectNoteContents) + 3 ) ] <- symLink

  # Save projectNote at projectNotePath:
  projectNoteFileConn <- file( projectNotePath )
  writeLines(projectNoteContents, projectNoteFileConn )
  close(projectNoteFileConn)

  cat( "  Written SymLink to new Project Note: ", basename(projectNotePath), "\n" )


  ###    DELETE old volPath    ###

  # Delete files RECURSIVELY:
  #done <- unlink(oldVolPathAbs, recursive=TRUE)
  done <- R.utils::removeDirectory(oldVolPathAbs, recursive=TRUE)

  if(done != 0) {
    stop( paste0("  old Project Note Dir contents could not be deleted: ", oldVolPathAbs) )
  }

  cat( "  Deleted old Project Note Dir: ", oldVolPathAbs, "\n" )

}


