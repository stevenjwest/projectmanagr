
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
get_rstudio_open_doc_IDs <- function() {

  #cat( "\nprojectmanagr::get_rstudio_open_doc_IDs():\n" )

  rstudioInternalStateDir <- get_rstudio_internal_state_dir()

  # first, check if ~/.local/share/rstudio/sources exists
  if( file.exists( paste0(rstudioInternalStateDir, .Platform$file.sep, "sources") ) ) {

    # check if a DIR exists which STARTS WITH "s-" (remainder is unique RStudio session ID)
    # first handle NEW rstudio session directory layout
    if( !identical(character(0),
                  Sys.glob( paste0(rstudioInternalStateDir, .Platform$file.sep, "sources", .Platform$file.sep, "session-*")  ) )) {

      if( length(Sys.glob( paste0(rstudioInternalStateDir, .Platform$file.sep, "sources", .Platform$file.sep, "session-*")  )) > 1 ) {
        stop( "cannot determine rstudio files with more than one session open - close inactive session(s).")
      }

      if ( file.exists(  Sys.glob( paste0(rstudioInternalStateDir, .Platform$file.sep, "sources", .Platform$file.sep, "session-*")  )  )  ) {

        # get file list
        fileIDs <- list.files(  Sys.glob( paste0(rstudioInternalStateDir, .Platform$file.sep, "sources", .Platform$file.sep, "session-*")  )  )
        # look in the "session-*" DIR for every file that does NOT end in "-contents"
        fileIDs <- fileIDs[lapply(fileIDs,function(x) length(grep("-contents",x,value=FALSE))) == 0]
        # nor have name "lock_file"
        fileIDs <- fileIDs[lapply(fileIDs,function(x) length(grep("lock_file",x,value=FALSE))) == 0]

        # convert to full path:
        filePath <- paste0(Sys.glob( paste0(rstudioInternalStateDir, .Platform$file.sep, "sources", .Platform$file.sep, "session-*")  ), .Platform$file.sep, fileIDs)

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

        # no files match pattern : paste0(rstudioInternalStateDir, .Platform$file.sep, "sources", .Platform$file.sep, "session-*")
        stop( paste0("No Active RStudio Session") )

      }


    } else if( !identical(character(0),
                          Sys.glob( paste0(rstudioInternalStateDir, .Platform$file.sep, "sources", .Platform$file.sep, "s-*")  ) ) ) {

      # next handle OLD rstudio session directory layout
      if ( file.exists(  Sys.glob( paste0(rstudioInternalStateDir, .Platform$file.sep, "sources", .Platform$file.sep, "s-*")  )  )  ) {

        # get file list
        fileIDs <- list.files(  Sys.glob( paste0(rstudioInternalStateDir, .Platform$file.sep, "sources", .Platform$file.sep, "s-*")  )  )
        # look in the "s-*" DIR for every file that does NOT end in "-contents"
        fileIDs <- fileIDs[lapply(fileIDs,function(x) length(grep("-contents",x,value=FALSE))) == 0]
        # nor have name "lock_file"
        fileIDs <- fileIDs[lapply(fileIDs,function(x) length(grep("lock_file",x,value=FALSE))) == 0]

        # convert to full path:
        filePath <- paste0(Sys.glob( paste0(rstudioInternalStateDir, .Platform$file.sep, "sources", .Platform$file.sep, "s-*")  ), .Platform$file.sep, fileIDs)

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

        # no files match pattern : paste0(rstudioInternalStateDir, .Platform$file.sep, "sources", .Platform$file.sep, "s-*")
        stop( paste0("No Active RStudio Session") )

      }
    } else {
      stop( paste0("No RStudio Session identified in : ", paste0(rstudioInternalStateDir, .Platform$file.sep, "sources"), " update ORG/config/settings.yml rstudioInternalStateDir parameter \n  see https://support.rstudio.com/hc/en-us/articles/200534577-Resetting-RStudio-Desktop-s-State" ) )
    }
  }
  else {

    # no dir : paste0(rstudioInternalStateDir, .Platform$file.sep, "sources")
    stop( paste0("RStudio Internal State Directory not found : update ORG/config/settings.yml rstudioInternalStateDir parameter \n  see https://support.rstudio.com/hc/en-us/articles/200534577-Resetting-RStudio-Desktop-s-State") )

  }

  # finally, re-order list based on relVector:
  fileList <- fileList[ order( relVector ) ]

  # return fileList:
  fileList


}


#' RStudio Docs filtered and Reordered
#'
#' Filtered to ensure only files that are in the `filePath` orgPath and are
#' project notes (not unknown files or project docs).
#'
#' Reordered to list all files from the right of the selected file (`filePath`).
#'
get_rstudio_doc_list_filtered_reordered <- function(filePath, settings) {

  orgPath <- normalizePath(find_org_directory(filePath))

  # get all open RStudio Doc paths:
  fileList <- get_rstudio_open_doc_IDs()

  foundActiveDoc = FALSE

  reorderedFileList = list() # all files to RIGHT of current active doc are stored here in order L>R
  firstFileList = list() # all files to LEFT of current active doc are stored here in order L>R
  numberedFileList <- list() # will fill the choices var for selectInput shiny widget
  # must be a list of numbers, with the names of each item the String that is presented in selectInput

  foundIndex <- 0

  for(i in 1:length(fileList) ) {

    if(foundActiveDoc == TRUE) {

      foundIndex <- foundIndex + 1

      reorderedFileList[[foundIndex]] <- fileList[[i]][2]

      numberedFileList[(i-1)] <- i-1 # AFTER skipped Active Doc, so set list[i-1] to i-1

    }
    else if( fileList[[i]][2] != filePath ) {
      firstFileList[[i]] <- fileList[[i]][2]

      numberedFileList[i] <- i # BEFORE skipping Active Doc, so set list[i] to i
    }
    else if( fileList[[i]][2] == filePath ) {

      foundActiveDoc = TRUE

      # This is the Active Doc - so DO NOT add an index to numberedFileList

    }

  }

  # concat the list of file paths, with files to the RIGHT of active doc FIRST:
  reorderedFileList <- c(reorderedFileList, firstFileList)

  if(length(reorderedFileList) == 0) {
    return(reorderedFileList) # return blank list
  }

  # determine file type and FILTER
  fileType <- lapply(reorderedFileList, get_file_type, settings)
  reorderedNoteList <- reorderedFileList[fileType != "DOC" & fileType != "UNKNOWN"]

  if(length(reorderedNoteList) == 0) {
    return(reorderedNoteList) # return blank list
  }

  # determine orgPath and FILTER
  orgPaths <- lapply(reorderedNoteList, find_org_directory)
  reorderedNoteOrgList <- reorderedNoteList[orgPaths == orgPath]

  if(length(reorderedNoteOrgList) == 0) {
    return(reorderedNoteOrgList) # return blank list
  }

  numFileList <- as.list(seq(length(reorderedNoteOrgList)))
  #names(numFileList) <- lapply(reorderedNoteOrgList, basename)
  names(numFileList) <- reorderedNoteOrgList

  numFileList

}



#' Get RStudio Internal State Dir
#'
#' Returns this from the settings or the default depending on OS.type (unix or Windows)
#'
#'@export
get_rstudio_internal_state_dir <- function() {

  # get rstudio_internal_state_dir from config OR use default
    if( .Platform$OS.type == "unix") {
      rstudioInternalStateDir <- "~/.local/share/rstudio" # path on LINUX/MAC OS
    } else {
      rstudioInternalStateDir <- paste0("%localappdata%", .Platform$file.sep, "RStudio") # path on WINDOWS ???
    }
  # return
  rstudioInternalStateDir
}


#' Get RStudio Config Dir
#'
#' Returns config directory depending on OS.type (unix or Windows) & rstudio
#' version (1.2 and prior OR 1.3+)
#'
#' See :
#'
#' https://support.posit.co/hc/en-us/articles/206382178
#' 'Customizing Keyboard Shortcuts in the RStudio IDE'
#'
#' https://support.posit.co/hc/en-us/articles/200534577-Resetting-RStudio-Desktop-s-State
#' 'Resetting RStudio Desktop's State'
#'
#'@export
get_rstudio_config_dir <- function() {

  # get config dir
  if( .Platform$OS.type == "unix") {

    # check latest rstudio version placement of config first
    if( file.exists( path.expand("~/.config/rstudio") ) ) {
      rstudioConfigDir <- path.expand("~/.config/rstudio")

    } else if( file.exists( path.expand("~/.R/rstudio/") ) ) {
      rstudioConfigDir <- path.expand("~/.R/rstudio/")

    } else if( file.exists( path.expand("~/AppData/Roaming/RStudio") ) ) {
      rstudioConfigDir <- path.expand("~/AppData/Roaming/RStudio")

    }
  } else { # windows

    # check latest rstudio version placement of config first
    if( file.exists( path.expand("~/AppData/Roaming/RStudio") ) ) {
      rstudioConfigDir <- path.expand("~/AppData/Roaming/RStudio")

    } else if( file.exists( path.expand( "%appdata%\\RStudio") ) ) {
      rstudioConfigDir <- path.expand("%appdata%\\RStudio")
    }
  }

  # return
  rstudioConfigDir
}


#' Set RStudio Keybindings to Addings
#'
#' Sets the default keybindings for projectmanagr in rstudio, via the
#' keybindings/addins.json file
#'
set_rstudio_keybindings_addins <- function() {

  # check if addins.json exists

  # if so add projectmanagr keybindings if they do not already exist in the file

  # otherwise copy the addins.json file from projectmanagr to set projectmanagr shortcuts

}


#' Get RStudio Internal State Dir
#'
#' Returns this from the settings or the default depending on OS.type (unix or Windows)
#'
getRStudioInternalStateDir2 <- function(path) {

  # get rstudioInternalStateDir from config OR use default
  orgPath <- find_org_directory(path)
  if(orgPath == "" ) {
    if( .Platform$OS.type == "unix") {
      rstudioInternalStateDir <- "~/.local/share/rstudio" # path on LINUX/MAC OS
    } else {
      rstudioInternalStateDir <- paste0("%localappdata%", .Platform$file.sep, "RStudio") # path on WINDOWS ???
    }
  } else {

    confPath <- paste0( orgPath, .Platform$file.sep, "config" )

    # get the rstudio internal state parameters directory from settings
    settingsFile = paste( confPath, .Platform$file.sep, "settings.yml", sep="" )
    settingsContents <- yaml::yaml.load( yaml::read_yaml( settingsFile ) )
    rstudioInternalStateDir <- settingsContents$rstudioInternalStateDir
  }

  # return
  rstudioInternalStateDir
}




