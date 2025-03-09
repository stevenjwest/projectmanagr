
#' Move Data between Volumes
#'
#' Volume mounts are managed in the volumes/ DIR of the Organisation, logged in
#' the volumes/volumes.Rmd file.
#'
#' This is a convenience function to move or copy data associated with a project
#' note between volumes.
#'
#' * All data is associated with a project note, and the project note path must
#' be supplied.  The project note Rmd file contains a section (# DATA STORAGE)
#' that describes each sub-DIR that has previously been moved via this function,
#' and is initially read by this function.
#'
#' * Data is moved from a sub-DIR in the project note DIR.  Any data associated
#' with a Project Note MUST exist within the Project Note DIR - if the sub-DIR
#' has been moved to a volume, it will be symlinked to the Project Note DIR, and
#' therefore will EXIST as a sub-DIR in the Project Note DIR as long as the
#' volume is mounted.  The sub-DIR name is given in the from_dir arg - and this
#' sub-DIR MUST be available (exist and be mounted) in the Project Note DIR for
#' the transfer of data to take place!  So CHECK the volume is mounted for this
#' sub-DIR if its a symlink!
#'
#' * The Data is moved to a new volume.  The volume can be any existing volume
#' in the volumes/ DIR, or it can be the local volume (which is the Project Note
#' DIR itself!), in which case the current symlink is deleted in local.  All data
#' is moved recursively - first the directory structure is generated and then all
#' data files are copied over, and finally the copy is confirmed as complete by
#' comparing DIR/FILE TREES between source and destination.  Only once this is
#' confirmed is the source data deleted.
#'
#' * The data can be copied rather than moved, in which case the deletion is not
#' performed.
#'
#' * If the data has been moved to a volume other than local, a symlink is made
#' in local to the new volume. The symlink is given the same name as the
#' original DIR. If the data is moved to local, no symlink is needed. If the
#' data is COPIED from local to a volume, again no symlink is needed (as the
#' data will still exist locally!). The new copy on the volume is a SECONDARY
#' COPY - and will be updated to the PRIMARY COPY (on local) when
#' volume_update_copies() is run.
#'
#' * Finally, this operation is logged in the project note under the DATA
#' STORAGE Section:  If the sub-DIR is already listed, its listing is updated
#' with the new destination (overwriting the appropriate old DIR, or adding to
#' the destination list if copied).  If the sub-DIR does not exist on the list,
#' it is added and its destination(s) added as appropriate to the operation.
#'
#' The DATA STORAGE Section in a Project Note will list a subDIR name, followed
#' by its destination(s): Each destination is listed in a comma separated list
#' after subDIR name (separated from the list with a colon:).  The destination
#' can be local, or any symlink in the volumes/ dir.  If the data exists on
#' local, this is always listed first.  Otherwise the primary link to this data
#' via a symlink in local is listed first.  This is the primary data storage
#' location.  This is followed with secondary storage locations which can be
#' used to backup the data (HIGHLY RECOMMENDED!).
#'
#' * The secondary storage locations can be kept in sync with the primary data
#' storage location via volume_update_copies(), this method receives the project
#' note as input, and will update any data copies as needed.
#'
#'@param project_note_path Path to Project Note.
#'
#'@param from_dir The subDir to be moved, should exist in the project note DIR
#'as a DIR or symlink.
#'
#'@param to_volume The volume name to move to.  This must be an ACTIVE SYMLINK
#'in the volumes/ dir, or keyword 'local' to move the dir to the project note
#'local DIR.
#'
#'@param copy Boolean to indicate if the data should be copied or moved - data
#'is moved (deleted in source) by default.
#'
#'@export
volume_move_data <- function(project_note_path, from_dir, to_volume, copy = FALSE) {

  # check project note path is valid:
  project_note_path <- path.expand(project_note_path) # expand HOME ~
  project_note_path <- check_proj_note(project_note_path)
  if( nchar(project_note_path) == 0 ) {
    stop( paste0("  project_note_path is not valid: ", project_note_path) )
  }

  # define project_note_dir path:
  project_note_dir <- get_project_note_dir_path(project_note_path)

  # define the volume path:
  org_path <- find_org_directory(project_note_path)
  if( nchar(org_path) == 0 ) {
    stop( paste0("  org_path not identified from project_note_path: ", project_note_path) )
  }

  # define volume path
  vol_path <- paste0(org_path, .Platform$file.sep, "volumes", .Platform$file.sep, to_volume)
  if( file.exists(vol_path) == FALSE ) {
    if( to_volume != "local") {
    stop( paste0("  Volume does not exist: ", to_volume, " is it mounted?") )
    }
  }

  # define the path, from org root to this project notes dir, on volume:
  vol_path <- paste0(vol_path, .Platform$file.sep,
                     substr(project_note_dir, nchar(org_path)+2, nchar(project_note_dir)),
                     .Platform$file.sep, from_dir )

  # load project note contents:
  project_note_filecon <- file( project_note_path )
  project_note_contents <- readLines( project_note_filecon )
  close(project_note_filecon)

  # get project note storage list from contents:
  project_note_storage <- get_proj_note_storage(project_note_contents)

  # define from_dir path and check it EXISTS
  dir_path <- paste0(project_note_dir, .Platform$file.sep, from_dir)
  if( file.exists(dir_path) == FALSE ) {
    stop( paste0("  DIR does not exist - is the volume mounted? : ", from_dir) )
  }

  # DEAL WITH to_volume == local
    # if to_volume is local, then
      # from_dir must not exist in local dir (must be an ACTIVE SYMLINK to a volume)
      # copy MUST be FALSE (when moving to volume local, ALWAYS MOVE PRIMARY COPY)
    # check these are the case, otherwise STOP:
  if( to_volume == "local" ) {
    # check symlink:
    if( is.na(Sys.readlink(dir_path)) ) {
      stop( paste0("  DIR does not exist - is the volume mounted? : ", from_dir) )
    } else if( nchar(Sys.readlink(dir_path)) == 0 ) {
      # dir_path is NOT a symlink - STOP the function
      stop( paste0("  DIR already is in local : ", from_dir) )
    }

    # check copy:
    if(copy == TRUE) {
      stop( paste0("  Can only MOVE data dir to local - set copy to FALSE : ", from_dir) )
    }

    # if to_volume is local, will be moving the data dir FROM CURRENT SYMLINK to the
     # project_note_dir - to set the variables appropriately
    # adjust vol_path:
    dir_path <- paste0(Sys.readlink(dir_path) )
    vol_path <- paste0(project_note_dir, .Platform$file.sep, from_dir )
    # and DELETE the symlink - now vol_path!!:
    unlink(vol_path)
  }

  # get list of DIRS in source to build DIR TREE:
  dirs <- list.dirs(dir_path, full.names = FALSE)
  dirs_dir <- paste0(dir_path, .Platform$file.sep, dirs)
  dirs_vol <- paste0(vol_path, .Platform$file.sep, dirs)

  # list of files in source to copy:
  files <- list.files(dir_path, full.names = FALSE, recursive = TRUE )
  files_dir <- paste0(dir_path, .Platform$file.sep, files)
  files_vol <- paste0(vol_path, .Platform$file.sep, files)

  # create vol_path DIRS
  for(i in 1:length(dirs_vol) ) {
    rtn <- dir.create(dirs_vol[i], recursive=TRUE, showWarnings = FALSE)
    if(rtn == FALSE) {
      stop( paste0("  File Transfer Failure : ", dirs_vol[i], " already exists on ", to_volume,
                   ".  Are you trying to move data where it already exists?") )
    }
  }

  # copy files_dir to files_vol
   # add error check to this step!
  for(i in 1:length(files_dir) ) {
    copy_val <- file.copy(files_dir[i], files_vol[i])
    if(copy_val == TRUE) {
      cat( "  Copied file successfully: ", files_vol[i], "\n" )
    } else {
      # delete all copy progress to date on destination if possible:
      unlink(files_vol)
      unlink(dirs_vol, recursive=TRUE)
      # and return error message
      stop( paste0("  File Transfer Failure : ", files_vol[i], " removed transferred data from ", to_volume,
                   " and retained original - please check volume connection and try transfer again.") )
    }
  }

  # if no error has stopped the method, then can complete the movement of data:

  # delete data from source - if copy == FALSE:
  if(copy == FALSE ) {
    unlink(files_dir)
    unlink(dirs_dir, recursive=TRUE)
  }

  # add symlink to project_note_dir:
   # if copy == FALSE and if the data is NOT being transferred BACK to local (to_volume == local)
  if( copy == FALSE) {
    if( to_volume != 'local' ) {
      # add symlink using RELATIVE PATH
      dir_path_rel <- R.utils::getRelativePath(dirs_vol[1], relativeTo=dir_path)
      dir_path_rel <- substring(dir_path_rel, first=4, last=nchar(dir_path_rel))
      file.symlink(dir_path_rel, dir_path)
    }
  }

  # create project_note_storage list and write to project_note_contents:
  pns <- project_note_storage[[from_dir]] # get the current contents if they exist (otherwise this is null)
  if(copy == TRUE) {
    pns <- c(pns, to_volume) # when making a copy add current to_volume to the list
      # copy to END as this makes a NEW COPY and will not replace the PRIMARY COPY (in local or another volume)
  } else {
    pns[1] <- to_volume # REPLACE first item in pns if MOVING - Moving always acts on the PRIMARY COPY
  }
  project_note_storage[[from_dir]] <- pns
  project_note_contents <- set_proj_note_storage(project_note_contents, project_note_storage)

  # save project note contents:
  project_note_filecon <- file( project_note_path )
  writeLines( project_note_contents, project_note_filecon )
  close(project_note_filecon)

}


#' Get Storage Vector from Contents
#'
#' returned storage vector is a list of vectors: Each vector is named the directory name,
#' and the vector contents are each location the directory has been moved/copied to.
#'
#' Returns a blank list if no content in DATA STORAGE
#'
get_proj_note_storage <- function(project_note_contents) {

  start <- grepLineIndex("# DATA STORAGE:", project_note_contents)
  end <- grepLineIndexFrom("----", project_note_contents, start)

  # extract each line between start and end that has any text content
  contents <- c()
  for( i in (start+1):(end-1) ) {
    if( nchar(trimws(project_note_contents[i]) ) > 0  ) {
      contents <- c(contents, trimws(project_note_contents[i]) )
    }
  }

  if( is.null(contents) ) {
    list()
  } else {

  # each line represents a directory with syntax - name : storage_location , storage_location
   # parse each into a list of vectors
  storage_vector <- list()
  storage_vector_names <- c()
  for( i in 1:length(contents) ) {
    split_contents <- unlist( strsplit(contents[i], "\\s+") )
    # check the contents are valid - must have : as SECOND ELEMENT
    if(split_contents[2] != ":" ) {
      stop( paste0("  Syntax Error in DATA STORAGE Section of Project Note: ", contents[i]) )
    }

    if( length(split_contents) > 3 ) {
      vector <- c()
      for( j in seq(3,length(split_contents),2) ) {
        vector <- c(vector, split_contents[j])
      }
    }
    else {
      vector <- c(split_contents[3])
    }

    storage_vector[[i]] <- vector
    storage_vector_names <- c(storage_vector_names, split_contents[1])

  }

  names(storage_vector) <- storage_vector_names

  storage_vector

  }

}

#' Set Storage Vector from Contents
#'
#'
set_proj_note_storage <- function(project_note_contents, project_note_storage) {

  vector <- c("","")
  dir_names <- names(project_note_storage)

  for(i in 1:length(dir_names) ) {
    pnsv <- project_note_storage[[i]]
    ch <- paste("    ", dir_names[i], ":")
    for(j in 1:length(pnsv)) {
      ch <- paste(ch, pnsv[j], ",")
    }
    ch <- substr(ch, 1, nchar(ch)-2) # remove ' ,'
    vector <- c(vector, ch, "" )
  }

  vector <- c(vector, "") # add extra blank line

  start <- grepLineIndex("# DATA STORAGE:", project_note_contents)
  end <- grepLineIndexFrom("----", project_note_contents, start)

  # set the contents between # DATA STORAGE and ---- to vector
  project_note_contents <- c( project_note_contents[1:start],
                              vector,
                              project_note_contents[end:length(project_note_contents)] )

  project_note_contents

}


#' Update data copies between volumes
#'
#' This updates any copies of data between volumes so they are all in sync with the latest data - ASSUMED to be
#' the PRIMARY COPY (i.e. the one which is in or symlinked to the project note dir!)
#'
#' This function ASSUMES that data copies are IMMUTABLE - data is not edited or deleted, only new data files/paths
#' are ADDED to a data copy.
#'
volume_update_copies <- function(project_note_path, from_dir) {

  # check project note path is valid:
  project_note_path <- path.expand(project_note_path) # expand HOME ~
  project_note_path <- check_proj_note(project_note_path)
  if( nchar(project_note_path) == 0 ) {
    stop( paste0("  project_note_path is not valid: ", project_note_path) )
  }

  # define the volume path:
  org_path <- find_org_directory(project_note_path)
  if( nchar(org_path) == 0 ) {
    stop( paste0("  org_path not identified from project_note_path: ", project_note_path) )
  }

  # define project_note_dir path:
  project_note_dir <- get_project_note_dir_path(project_note_path)

  # load project note contents:
  project_note_filecon <- file( project_note_path )
  project_note_contents <- readLines( project_note_filecon )
  close(project_note_filecon)

  # get project note storage list from contents:
  project_note_storage <- get_proj_note_storage(project_note_contents)

  # define from_dir path and check it EXISTS
  dir_path <- paste0(project_note_dir, .Platform$file.sep, from_dir)
  if( file.exists(dir_path) == FALSE ) {
    stop( paste0("  DIR does not exist - is the volume mounted? : ", from_dir) )
  }

  pns <- project_note_storage[[from_dir]] # get all locations of copies
  # define volume paths - all COPIES will be from index 2 to END
  vol_paths <- paste0(org_path, .Platform$file.sep, "volumes",
                      .Platform$file.sep, pns[2:length(pns)], .Platform$file.sep,
                      substr(project_note_dir, nchar(org_path)+2, nchar(project_note_dir)),
                      .Platform$file.sep, from_dir)
  for(i in 1:length(vol_paths) ) {
    if( file.exists(vol_paths[i]) == FALSE ) {
      stop( paste0("  Volume does not exist: ", vol_paths[i], " is it mounted?") )
    }
  }

  # update all dirs and files from dir_path in each vol_paths

  for(i in 1:length(vol_paths) ) {

    cat("  volume: ", pns[i])

    # get list of DIRS in source to build DIR TREE:
    dirs <- list.dirs(dir_path, full.names = FALSE)
    dirs_dir <- paste0(dir_path, .Platform$file.sep, dirs)
    dirs_vol <- paste0(vol_paths[i], .Platform$file.sep, dirs)

    # list of files in source to copy:
    files <- list.files(dir_path, full.names = FALSE, recursive = TRUE )
    files_dir <- paste0(dir_path, .Platform$file.sep, files)
    files_vol <- paste0(vol_paths[i], .Platform$file.sep, files)

    # create vol_path DIRS - if they dont exist
    for(j in 1:length(dirs_vol) ) {
      if(dir.exists(dirs_vol[j]) == FALSE) {
        rtn <- dir.create(dirs_vol[j], recursive=TRUE, showWarnings = FALSE)
        cat("    added DIR: ", dirs_vol[j], "\n")
        if(rtn == FALSE) {
          stop( paste0("  File Transfer Failure : ", dirs_vol[j], " already exists on ", pns[i],
                     ".  Are you trying to copy data where it already exists?") )
        }
      }
    }

    # copy files_dir to files_vol
    # add error check to this step!
    for(j in 1:length(files_dir) ) {
      copy_val = TRUE
      if(file.exists(files_vol[j]) == FALSE ) {
        copy_val <- file.copy(files_dir[j], files_vol[j])
        cat("    added file: ", files_vol[j], "\n")
      } else if( as.POSIXct(file.info(files_vol[j])$mtime) < as.POSIXct(file.info(files_dir[j])$mtime) ) {
        # if vol file is YOUNGER than dir file - dir file has been updated, so delete files_vol and replace
        file.remove(files_vol[j])
        copy_val <- file.copy(files_dir[j], files_vol[j])
        cat("    updated file: ", files_vol[j], "\n")
      }

      if(copy_val == TRUE) {
        #cat( "  Copied file successfully: ", files_vol[j], "\n" )
      } else {
        # and return error message
        stop( paste0("  File Copy Failure : ", files_vol[j], " removed transferred data from ", pns[i],
                     " and retained original - please check volume connection and try transfer again.") )
      }

    } # end j

  } # end i

}


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
#'
#' @param volName Name of volume to store data on.  The volume must EXIST and be MOUNTED.
#'
#' @param use_full_path If true, will write full path from Org root through to Project Note
#' DIR (prog-dir/proj-doc-dir/proj-note-dir/ path).  If false, the project note DIR will be
#' directly written to volume.  Direct writing of project note minimises DIR tree, whereas
#' writing full path eliminates possibility of name clash when writing multiple Project Notes
#' from different Programmes/Projects to the same volume.
#'
#' @export
volume_add_dir <- function(dirName, volName,
                              use_full_path = TRUE ) {

  cat( "\nprojectmanagr::volume_add_dir():\n" )

  # check the currently active doc is a Project Note:
  context <- rstudioapi::getSourceEditorContext()

  # normalise path of active doc - make sure path is absolute!
  projectNote <- fs::path_expand(context$path)

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

  orgPath <- find_org_directory(orgPath)

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
                                     nchar(find_org_directory(projectNoteDir))+2,
                                     nchar(projectNoteDir) )  )

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
volume_remove_dir <- function() {



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
#'
#' @param projectNoteDir The ABSOLUTE PATH to the projectNoteDir where the SYMLINK will
#' reside.  NO SPACES.
#'
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
  progPath <- find_prog_dir(projectNoteDir)

  # Check projectNoteDir is a sub-dir in a Programme DIR, which is a sub-dir to the root of an ORGANISATION:
  # run dirname TWICE as want to ensure projectNoteDir is a sub-dir in a Programme!
  orgPath <- dirname( dirname(projectNoteDir) )

  orgPath <- find_org_directory(orgPath)

  if(orgPath == "" ) {
    # the search reached the root of the filesystem without finding the Organisation files,
    # therefore, projectNoteDir is not inside a PROGRAMME sub-dir!
    stop( paste0("  projectNoteDir is not in a sub-dir of a PROGRAMME Directory: ", projectNoteDir) )
  }
  # now, orgPath should be the root dir of the organisation

  # define the volPath - orgPath - volumes - volName
  volPath <- paste0(orgPath, .Platform$file.sep, "volumes", .Platform$file.sep, volName)

  projectNoteDir <- fs::path_expand(projectNoteDir)

  # Create the new sub-DIR in full path through volume:
    # volumes/[volName]/[path-from org-root-to-proj-note-dir]/[dirName]

  projDirOrgPath <- paste0( substr(projectNoteDir,
                                   nchar(find_org_directory(projectNoteDir))+2,
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
#' DEPRECATED - now using volumes.Rmd to manage volumes on filesystem.
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
#' NO LONGER export - deprecated
createVolume <- function( sourcePath, volName, orgPath=getwd() ) {

  cat( "\nprojectmanagr::createVolume():\n" )

  # check volName contains NO SPACES:
  if( grepl("\\s+", volName) ) {
    stop( paste0("  volName contains a SPACE: ", volName) )
  }

  # set/check orgPath to the org root dir:
  orgPath <- find_org_directory(orgPath)

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
#' DEPRECATED - use volume_move_data instead!
#'
#' Creates the Project Note DIR in new volume, copies all contents from
#' the old Project Note DIR location into the new Project Note DIR location,
#' deletes the old Project Note DIR, and symlinks to the new Project Note DIR.
#'
#' Finally, changes the location of the volume in the Project Note itself.
#'
#'  NO LONGER export - deprecated
moveToVolume <- function(volName, projectNotePath) {


  cat( "\nprojectmanagr::moveVolume():\n" )

  # check volName contains NO SPACES:
  if( grepl("\\s+", volName) ) {
    stop( paste0("  volName contains a SPACE: ", volName) )
  }

  # normalise path - remove "~" ref:
  projectNotePath <- fs::path_expand(projectNotePath)

  # check projectNotePath is a Project Note:
  type <- getFileType(projectNotePath)

  if(type != "NOTE" ) {
    stop( paste0("  projectNotePath is not a bottom level note: ", projectNotePath) )
  }

  # Get orgPath from projectNotePath:
  orgPath <- find_org_directory(projectNotePath)

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


