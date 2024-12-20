
#' Get RStudio Open Document IDs
#'
#' Returns an ordered list of all open RStudio Documents.  The list contains a set
#' of vectors that contain the RStudio Document ID [1] and the RStudio Document absolute
#' path [2].
#'
#' The order of the open RStudio documents in the list generally matches the order they are presented
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

    # check if a DIR exists which STARTS WITH "session-" (remainder is unique RStudio session ID)
    # first handle NEW rstudio session directory layout
    if( !identical(character(0),
                  Sys.glob( paste0(rstudioInternalStateDir, .Platform$file.sep, "sources", .Platform$file.sep, "session-*")  ) )) {


      # allow files across sessions to be returned
      if( length(Sys.glob( paste0(rstudioInternalStateDir, .Platform$file.sep, "sources", .Platform$file.sep, "session-*")  )) > 1 ) {
        stop( "cannot determine rstudio files with more than one session open - close inactive session(s).")
      }

      if ( all(file.exists(
        Sys.glob(paste0(rstudioInternalStateDir, .Platform$file.sep, "sources", .Platform$file.sep, "session-*"))  ))  ) {

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


get_context_path <- function() {
  context <- rstudioapi::getSourceEditorContext()
  context$path
}


get_context_contents <- function() {
  context <- rstudioapi::getSourceEditorContext()
  context$contents
}

get_context_id <- function() {
  context <- rstudioapi::getSourceEditorContext()
  context$id
}

get_context_row <- function() {
  context <- rstudioapi::getSourceEditorContext()
  row <- context$selection[[1]]$range$start[1]
  row
}

get_context_row_end <- function() {
  context <- rstudioapi::getSourceEditorContext()
  context$selection[[1]]$range$end[1]
}

save_context_doc <- function() {
  if( rstudioapi::isAvailable() ) { rstudioapi::documentSave(id = get_context_id()) }
}

save_all_doc <- function() {
  if( rstudioapi::isAvailable() ) { rstudioapi::documentSaveAll() }
}


#' Set RStudio Keybindings to Addins
#'
#' Sets the default keybindings for projectmanagr functions in rstudio, via the
#' keybindings/addins.json file.
#'
#' The template file can be found in every projectmanagr organisation root:
#'
#' `.config/addins.json`
#'
#' This file can be modified for a different default set of projectmanagr
#' functions, and different keyboard shortcuts.
#'
#'
#'
set_rstudio_keybindings_addins <- function(path=getwd()) {

  # Can edit via rstudio.prefs package : https://www.danieldsjoberg.com/rstudio.prefs/
  #
  # UNIX : ~/.R/rstudio/keybindings/addins.json
  # WINDOWS : C:/Users/sjobergd/AppData/Roaming/RStudio/keybindings/addins.json
  #
  # {HOME_DIR} :
  # * unix : .R/rstudio/keybindings/addins.json
  # * windows : AppData/Roaming/RStudio/keybindings.addins.json

  # get keybindings path
  keybindings_path <- fs::path(get_rstudio_config_dir(), "keybindings")
  if( !fs::dir_exists(keybindings_path) ) {
    fs::dir_create(keybindings_path)
  }

  # check if addins.json exists
  keybindings_json_path <- fs::path(keybindings_path, "addins.json")

  # read the current keybindings
  if( !fs::file_exists(keybindings_json_path)) {
    keybindings_json <- list() # generate blank list
  } else {
    keybindings_json <- jsonlite::read_json(keybindings_json_path)
  }

  # read the projectmanagr organisation - config - keybindings json template
  orgPath <- find_org_directory(path)
  if(orgPath == "" ) { # only if orgPath not identified
    stop( paste0("  path is not in an Organisation: ", path) )
  }
  confPath <- get_config_dir(orgPath)
  keybindings_json_template_path <- fs::path(confPath, "addins.json")
  keybindings_json_template <- jsonlite::read_json(keybindings_json_template_path)

  # add or replace all named list entries in template into keybindings_json
  keybindings_json <- rlist::list.merge(keybindings_json, keybindings_json_template)

  # write new keybindings ot rstudio config file
  jsonlite::write_json(keybindings_json, keybindings_json_path)

}


#' Get RStudio Config Dir
#'
#' Returns this from the settings or the default depending on OS.type (unix or Windows)
#'
#' Returns config directory depending on OS.type (unix or Windows) & rstudio
#' version (1.2 and prior OR 1.3+)
#'
#' * 1.3+ unix : ~/.config/rstudio/keybindings/rstudio_bindings.json
#'
#' This is where key configuration files are kept - eg. keybindings
#'
#' See :
#'
#' https://support.posit.co/hc/en-us/articles/206382178
#' 'Customizing Keyboard Shortcuts in the RStudio IDE'
#'
#'
get_rstudio_config_dir <- function() {
  if( .Platform$OS.type == "unix") {
    path <- rappdirs::user_config_dir("rstudio", os="unix")
  } else {
    path <- rappdirs::user_config_dir("RStudio", appauthor=NULL)
  }
  fs::path(path) # return
}


#' Get RStudio Internal State Dir
#'
#' Returns this from the settings or the default depending on OS.type (unix or Windows)
#'
#' https://support.posit.co/hc/en-us/articles/200534577-Resetting-RStudio-Desktop-s-State
#' 'Resetting RStudio Desktop's State'
#'
#' Accessing the RStudio-Desktop Directory (Internal State)
#'
#' RStudio Desktop stores its internal state in a hidden directory: includes
#' information about open documents, log files, and other state information
#'
#' @export
get_rstudio_internal_state_dir <- function() {

  if( .Platform$OS.type == "unix") {
    rstudioInternalStateDir <- "~/.local/share/rstudio" # path on LINUX/MAC OS
  } else {
    rstudioInternalStateDir <- paste0("%localappdata%", .Platform$file.sep, "RStudio") # path on WINDOWS ???
  }
  # return
  rstudioInternalStateDir
}

#' Get RStudio Internal State Dir
#'
#' Returns this from the settings or the default depending on OS.type (unix or Windows)
#'
get_rstudio_internal_state_dir2 <- function(path) {

  # get rstudioInternalStateDir from config OR use default
  orgPath <- find_org_directory(path)
  if(orgPath == "" ) {
    if( .Platform$OS.type == "unix") {
      rstudioInternalStateDir <- "~/.local/share/rstudio" # path on LINUX/MAC OS
    } else {
      rstudioInternalStateDir <- paste0("%localappdata%", .Platform$file.sep, "RStudio") # path on WINDOWS ???
    }
  } else {

    # get config templates settings yml
    confPath <- get_config_dir(orgPath)
    tempPath <- get_template_dir(orgPath)
    settings <- get_settings_yml(orgPath)
    rstudioInternalStateDir <- settings$rstudioInternalStateDir
  }

  # return
  rstudioInternalStateDir
}




