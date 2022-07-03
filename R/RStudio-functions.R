
#' Get RStudio Open Document IDs
#'
#' Returns an ordered list of all open RStudio Documents.  The list contains a set
#' of VECTORS that contain the RStudio Document ID [1] and the RStudio Document absolute
#' path [2].
#'
#' The order of the open RStudio documents in the list matches the order they are open
#' in RStudio.
#'
#' fileList[[i]][1] - RStudio i'th Document ID.
#'
#' fileList[[i]][2] - RStudio i'th Document Absolute Path, or null if the file is not saved to disk.
#'
#' NOTE: This method returns open documents in the BASE RStudio Session, and not in any opened project.
#'
#'@export
getRStudioOpenDocIDs <- function() {

  #cat( "\nprojectmanagr::getRstudioDocIDs():\n" )

  rstudio_internal_state_dir <- getRStudioInternalStateDir()

  # first, check if ~/.local/share/rstudio/sources exists
  if( file.exists( paste0(rstudio_internal_state_dir, .Platform$file.sep, "sources") ) ) {

    # check if a DIR exists which STARTS WITH "s-" (remainder is unique RStudio session ID)
    if ( file.exists(  Sys.glob( paste0(rstudio_internal_state_dir, .Platform$file.sep, "sources", .Platform$file.sep, "s-*")  )  )  ) {

      # get file list
      fileIDs <- list.files(  Sys.glob( paste0(rstudio_internal_state_dir, .Platform$file.sep, "sources", .Platform$file.sep, "s-*")  )  )
      # look in the "s-*" DIR for every file that does NOT end in "-contents"
      fileIDs <- fileIDs[lapply(fileIDs,function(x) length(grep("-contents",x,value=FALSE))) == 0]
      # nor have name "lock_file"
      fileIDs <- fileIDs[lapply(fileIDs,function(x) length(grep("lock_file",x,value=FALSE))) == 0]

      # convert to full path:
      filePath <- paste0(Sys.glob( paste0(rstudio_internal_state_dir, .Platform$file.sep, "sources", .Platform$file.sep, "s-*")  ), .Platform$file.sep, fileIDs)

      # inside remaining files, if saved to disk, will be a line starting 'path" : "[path_to_file]",'
      # if not saved to disk, will contain the line '"path" : null,'
      # Extract the value of path for each entry in filePath:
      fileList <- list()
      relVector <- vector()
      for (i in 1:length(filePath) ) {

        # put ID into fileList
        fileList[[i]] <- fileIDs[i]

        # open file
        FileConn <- file( filePath[i] )
        lines <- readLines( FileConn, warn = FALSE )
        close(FileConn)

        # extract path string:
        pathLine <- lines[grepl("    \"path\"*", lines)]

        if( grepl("\"path\" : null,", pathLine) || grepl("\"path\": null,", pathLine) ) {
          # path is null - file has not been saved - store "null" in fileList:
          fileList[[i]][2] <- "null"
        }
        else {
          # copy path into fileList - second index
          fileList[[i]][2] <- substring(pathLine, regexpr(": \"", pathLine)+3, nchar(pathLine)-2)
        }

        # extract relative order string:
        relOrderLine <- lines[grepl("    \"relative_order\"*", lines)]
        # copy order into relVector:
        relVector[i] <- as.integer( substring(relOrderLine, regexpr("order", relOrderLine)+8, nchar(relOrderLine)-1) )

      }

    }
    else {

      # no files match pattern : paste0(rstudio_internal_state_dir, .Platform$file.sep, "sources", .Platform$file.sep, "s-*")
      stop( paste0("No Active RStudio Session") )

    }
  }
  else {

    # no dir : paste0(rstudio_internal_state_dir, .Platform$file.sep, "sources")
    stop( paste0("RStudio Internal State Directory not found : update ORG/config/settings.yml rstudioInternalStateDir parameter \n  see https://support.rstudio.com/hc/en-us/articles/200534577-Resetting-RStudio-Desktop-s-State") )

  }

  # finally, re-order list based on relVector:
  fileList <- fileList[ order( relVector ) ]

  # return fileList:
  fileList


}



#' Get RStudio Internal State Dir
#'
#' Returns this from the settings or the default depending on OS.type (unix or Windows)
#'
#'@export
getRStudioInternalStateDir <- function() {

  # get rstudio_internal_state_dir from config OR use default
    if( .Platform$OS.type == "unix") {
      rstudio_internal_state_dir <- "~/.local/share/rstudio" # path on LINUX/MAC OS
    } else {
      rstudio_internal_state_dir <- paste0("%localappdata%", .Platform$file.sep, "RStudio") # path on WINDOWS ???
    }
  # return
  rstudio_internal_state_dir
}


#' Get RStudio Internal State Dir
#'
#' Returns this from the settings or the default depending on OS.type (unix or Windows)
#'
getRStudioInternalStateDir2 <- function(path) {

  # get rstudio_internal_state_dir from config OR use default
  orgPath <- findOrgDir(path)
  if(orgPath == "" ) {
    if( .Platform$OS.type == "unix") {
      rstudio_internal_state_dir <- "~/.local/share/rstudio" # path on LINUX/MAC OS
    } else {
      rstudio_internal_state_dir <- paste0("%localappdata%", .Platform$file.sep, "RStudio") # path on WINDOWS ???
    }
  } else {

    confPath <- paste0( orgPath, .Platform$file.sep, "config" )

    # get the rstudio internal state parameters directory from settings
    settingsFile = paste( confPath, .Platform$file.sep, "settings.yml", sep="" )
    settingsContents <- yaml::yaml.load( yaml::read_yaml( settingsFile ) )
    rstudio_internal_state_dir <- settingsContents$rstudioInternalStateDir
  }

  # return
  rstudio_internal_state_dir
}


#' Set WD to RStudio Active Doc parent Directory
#'
#'
#'@export
setwdActiveDoc <- function( ) {

  context <- rstudioapi::getSourceEditorContext() # use this to always get the active document in the source editor

  cat( "\nprojectmanagr::setwdActiveDoc():\n" )

  setwd( dirname(context$path) )

  cat( "  Set work dir: ", dirname(context$path), "\n" )

}





