#' Add directory to Volume
#'
#' This function adds dirName to the selected volume (volName), under the current
#' Project Notes' DIR.  First, this method will generate a DIR the same name as
#' the Project Notes' DIR on volume - either directly (use_full_path is FALSE) or
#' with the full path to the Project Note DIR in the Organisation (if use_full_path
#' is TRUE).
#'
#' Next, the DIR dirName is created in the new Project Note DIR on volName.
#'
#' Finally, a Symlink is formed from the new DIR, dirName, on the volume, to inside the
#' Project Note DIR on the local filesystem.
#'
#' It is probably also wise to write to the Project Note, somewhere in its header,
#' the symlink path:  This could facilitate automated mounting of volumes if desired, and
#' at least serve as a record of where the data for this Project Note is.
#'
#' The default is to write the current Project Notes' DIR name directly to the volume.
#' However, if use_full_path is set to true, the full path from the ORGANISATION Root
#' through to the Project Notes' DIR will be written to the volume.
#'
#' This allows a Volume to be used for storing just one set of Project Notes data
#' directly into the volume location with a minimal directory tree, or to store a
#' mixture of different Project Notes data from different Project Note sets, Project
#' Docs, and Programmes, without any issues of name clash.
#'
#' @param dirName Name of dir to form in Project Note DIR.
#' @param volName Name of volume to store data on.  The volume must EXIST and be MOUNTED.
#' @param use_full_path If true, will write full path from Org root through to Project Note
#' DIR (prog-dir/proj-doc-dir/proj-note-dir/ path).  If false, the project note DIR will be
#' directly written to volume.  Direct writing of project note minimises DIR tree, whereas
#' writing full path eliminates possibility of name clash when writing multiple Project Notes
#' from different Programmes/Projects to the same volume.
#'
#' @export
volume_attach_dir <- function(dirName, volName,
                              use_full_path = FALSE ) {

  cat( "\nprojectmanagr::volume_add_dir():\n" )

  # check the currently active doc is a Project Note:
  context <- rstudioapi::getSourceEditorContext()

  # normalise path of active doc - make sure path is absolute!
  projectNote <- normalizePath(context$path)

  # get potential projectNoteDir:
  projectNoteDir <- substr(projectNote, 1, regexpr("~_", projectNote)-1 )
    # projectNoteDir will be BLANK is it doesnt contain the string: "~_"


  # Check projectNoteDir is not a blank string:
  if( nchar(projectNoteDir) == 0 ) {
    stop( paste0("  Current note does not contain a Project Note DIR - no ~_ separator identified: ", projectNote) )
  }

  # Check projectNoteDir contains NO SPACES:
  if( grepl("\\s+", projectNoteDir) ) {
    stop( paste0("  projectNoteDir contains a SPACE: ", projectNoteDir) )
  }

  # Check projectNoteDir EXISTS:
  if( file.exists(projectNoteDir) == FALSE ) {
    stop( paste0("  project Note DIR does not exist: ", projectNote) )
  }


  # Check dirName contains NO SPACES:
  if( grepl("\\s+", dirName) ) {
    stop( paste0("  dirName contains a SPACE: ", dirName) )
  }


  # Check volName contains NO SPACES:
  if( grepl("\\s+", volName) ) {
    stop( paste0("  volName contains a SPACE: ", volName) )
  }

  # Check projectNoteDir is a sub-dir in a Programme DIR,
    # which is a sub-dir to the root of an ORGANISATION:
    # run dirname TWICE as want to ensure projectNoteDir is a sub-dir in a Programme!
  orgPath <- dirname( dirname(projectNoteDir) )

  orgPath <- findOrgDir(orgPath)

  if( orgPath == "" ) {
    # the search reached the root of the filesystem without finding the Organisation files,
    # therefore, projectNoteDir is not inside a PROGRAMME sub-dir!
    stop( paste0("  projectNoteDir is not in a sub-dir of a PROGRAMME Directory: ", projectNoteDir) )
  }
  # now, orgPath should be the root dir of the organisation

  # define the volPath - orgPath - volumes - volName
  volPath <- paste0(orgPath, .Platform$file.sep, "volumes", .Platform$file.sep, volName)

  # check volPath exists:
  if( file.exists(volPath) == FALSE ) {
    stop( paste0("  volName DIR is not mounted in volumes: ", volName) )
  }


  # SPLIT the function:
    # IF use_full_path is TRUE, need to form the full path - from Org ROOT to Project Note
      # DIR - in the volumePath IF NECESSARY
    # IF use_full_path is FALSE, need to add a new dir of same name as the Project Note DIR
      # IF NECESSARY
  # in BOTH CASES the newly formed Project Note DIR in the volume is where new DIRs are generated
    # and where the symlinks in the local filesystem will point to

  if( use_full_path == TRUE ) {

    # Create the full path on volume:
    # volumes/[volName]/[programme-dir]/[proj-doc-dir]/[proj-note-dir]/

    projDirOrgPath <- paste0( substr(projectNoteDir,
                                     nchar(findOrgDir(projectNoteDir))+2,
                                     nchar(projectNoteDir) ),
                              .Platform$file.sep, basename(projectNoteDir) )

    noteDirPath <- paste0( volPath, .Platform$file.sep, projDirOrgPath )

    if( file.exists(noteDirPath) == TRUE ) {
      # proceed to next step -
        # as the project note DIR already exists down it full path on this volume!
      # set done to TRUE to indicate this:
      done <- TRUE
    } else {
      # create the full path in volumes down to the Project Note DIR
      done <- dir.create( noteDirPath, recursive = TRUE )
    }

    if(!done) {
      stop( paste0("  DIR could not be created on volume: ", noteDirPath) )
    }

    cat( "  Made Full Path DIR on volume: ", noteDirPath, "\n" )


  } else if( use_full_path == FALSE ) {


    # Create the Project Note DIR DIRECTLY on volume:
    # volumes/[volName]/[proj-note-dir]/

    noteDirPath <- paste0( volPath, .Platform$file.sep, basename(projectNoteDir) )

    if( file.exists(noteDirPath) == TRUE ) {
      # proceed to next step -
      # as the project note DIR already exists down it full path on this volume!
      # set done to TRUE to indicate this:
      done <- TRUE
    } else {
      # create the full path in volumes down to the Project Note DIR
      done <- dir.create( noteDirPath, recursive = TRUE )
      cat( "  Created DIR on volume: ", noteDirPath, "\n" )
    }

    if(!done) {
      stop( paste0("  DIR could not be created on volume: ", noteDirPath) )
    }

  }

  # the Project Note DIR has been created on the volume
    # its path is noteDirPath

  # NEXT - add the new sub-DIR inside this - dirName

  subDir <- paste0( noteDirPath, .Platform$file.sep, dirName )

  if( file.exists(subDir) == TRUE ) {
    stop( paste0("  DIR already exists in projectNote DIR: ", dirName) )
  }

    # create the DIR:
    done <- dir.create( subDir, recursive = TRUE )

    if(!done) {
      stop( paste0("  DIR could not be created on volume: ", subDir) )
    }

    cat( "  Created DIR on volume: ", subDir, "\n" )


    # THEN make the symlink to this, putting the symlink in the projectNotePath:
    noteDirSymPath = paste0( projectNoteDir, .Platform$file.sep, dirName )
    symLink <- R.utils::getRelativePath(subDir, relativeTo=noteDirSymPath)
    symLink <- substring(symLink, first=4, last=nchar(symLink)) # remove first `../`

    done <- file.symlink( symLink, noteDirSymPath )

    if(!done) {
      file.remove(subDir) # remove the dir
      stop( paste0("  Symlink could not be made to volume: ", noteDirSymPath, " ", subDir) )
    }

    cat( "  Made symlink: ", noteDirSymPath, "\n" )


}


#' Remove directory from Volume and copy back to local filesystem
#'
#'
#'
volume_detach_dir <- function() {



}


#' Move directory from current volume to another
#'
#'
#'
volume_move_dir <- function() {



}


#' Make Volume Directory
#'
#' This first creates the new sub-DIR on the selected volume - including the full path
#' from the ORGANISATION ROOT down through PROGRAMME > PROJECT DOC > DIR PROJECT NOTE DIR >
#' to the sub-DIR.
#'
#' Then it forms a SYMLINK from this new sub-DIR on selected volume to a symlink in
#' projectNoteDir.
#'
#' @param dirName Name for the DIR - sub-DIR in projectNoteDir on volume, and SYMLINK in
#' local projectNoteDir.  NO SPACES.
#' @param projectNoteDir The ABSOLUTE PATH to the projectNoteDir where the SYMLINK will
#' reside.  NO SPACES.
#' @param volName The NAME of the volume in volumes/ - Must EXIST and be VISIBLE
#' (i.e mounted). NO SPACES.
#'
volume_mkdir <- function(dirName, projectNoteDir, volName) {

  cat( "\nprojectmanagr::addProjectNote():\n" )

  # Check dirName contains NO SPACES:
  if( grepl("\\s+", dirName) ) {
    stop( paste0("  dirName contains a SPACE: ", dirName) )
  }

  # Check projectNoteDir contains NO SPACES:
  if( grepl("\\s+", projectNoteDir) ) {
    stop( paste0("  projectNoteDir contains a SPACE: ", projectNoteDir) )
  }

  # Check volName contains NO SPACES:
  if( grepl("\\s+", volName) ) {
    stop( paste0("  volName contains a SPACE: ", volName) )
  }

  # Find programme DIR from projectDocPath:
  progPath <- findProgDir(projectNoteDir)

  # Check projectNoteDir is a sub-dir in a Programme DIR, which is a sub-dir to the root of an ORGANISATION:
  # run dirname TWICE as want to ensure projectNoteDir is a sub-dir in a Programme!
  orgPath <- dirname( dirname(projectNoteDir) )

  orgPath <- findOrgDir(orgPath)

  if(orgPath == "" ) {
    # the search reached the root of the filesystem without finding the Organisation files,
    # therefore, projectNoteDir is not inside a PROGRAMME sub-dir!
    stop( paste0("  projectNoteDir is not in a sub-dir of a PROGRAMME Directory: ", projectNoteDir) )
  }
  # now, orgPath should be the root dir of the organisation

  # define the volPath - orgPath - volumes - volName
  volPath <- paste0(orgPath, .Platform$file.sep, "volumes", .Platform$file.sep, volName)

  projectNoteDir <- normalizePath(projectNoteDir)

  # Create the new sub-DIR in full path through volume:
    # volumes/[volName]/[path-from org-root-to-proj-note-dir]/[dirName]

  projDirOrgPath <- paste0( substr(projectNoteDir,
                                   nchar(findOrgDir(projectNoteDir))+2,
                                   nchar(projectNoteDir) ),
                            .Platform$file.sep, basename(projectNoteDir),
                            .Platform$file.sep, dirName )

  noteDirPath = paste( volPath, .Platform$file.sep, projDirOrgPath, sep="")

  if( file.exists(noteDirPath) == TRUE ) {
    stop( paste0("  DIR already exists in projectNote DIR: ", dirName) )
  }


  done <- dir.create( noteDirPath, recursive = TRUE )

  if(!done) {
    stop( paste0("  DIR could not be created on volume: ", noteDirPath) )
  }

  cat( "  Made DIR on volume: ", noteDirPath, "\n" )



  # THEN make the symlink to this, putting the symlink in the projectNotePath:
  noteDirSymPath = paste0( projectNoteDir, .Platform$file.sep, dirName )
  symLink <- R.utils::getRelativePath(noteDirPath, relativeTo=noteDirSymPath)
  symLink <- substring(symLink, first=4, last=nchar(symLink)) # remove first `../`

  done <- file.symlink( symLink, noteDirSymPath )

  if(!done) {
    file.remove(noteDirPath) # remove the dir
    stop( paste0("  Symlink could not be made to volume: ", noteDirSymPath, " ", noteDirPath) )
  }

  cat( "  Made symlink: ", noteDirSymPath, "\n" )


}



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


