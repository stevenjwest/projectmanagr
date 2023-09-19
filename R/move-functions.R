
#' Move a Project Note to new Directory
#'
#' This Function moves a Project Note
#'
#'  adds a Protocol to a Project Note - protocols are found in the
#'  PROGRAMMES' protocols/ Dir.
#'
#' Inserted protocols add each Procedure defined under the '# PROTOCOL' section
#'  of the document. Notes sections (FALSE by default) and the Equipment section
#' (TRUE by default) can be optionally included in the inserted protocol.
#'
#' All links in the Protocol are UPDATED to work from the destination Project
#'  Note.
#'
#' All graphics included in a knitr::include_graphics() r code chunk are
#'  transferred to the new Project Note DIR and linked appropriately.
#'
#' @param protocolName The name of the Protocol Rmd file - WITHOUT extension.
#'
#' @param selection Selection object from the Project Note file where the protocol
#' is to be inserted.  Use `projectmanagr::cursor_selection()` or
#' `projectmanagr::user_selection()` to create this object.
#'
#' @param protocolInsertionTemplate Template file that contains boilerplate content
#' for protocol insertion into project note.
#'
#' @export
move_project_note <- function(protocolName, selection,
                            protocolInsertionTemplate="Protocol-Insertion-Template.Rmd") {

  cat( "\nprojectmanagr::move_project_file():\n" )


  #### Set Instance Variables ####

  projNoteRmdPath <- selection[["filePath"]] # presumed to be project note Rmd
  noteInsertionIndex <- selection[["originalLineNumber"]]

  # get orgPath
  orgPath <- find_org_directory(projNoteRmdPath)

  if(orgPath == "" ) { # only if orgPath not identified
    stop( paste0("  Cannot identify organisation directory: ", projNoteRmdPath) )
  }
  # now, orgPath should be the root dir of the organisation

  # set confPath + tempPath - these names are FIXED:
  confPath <- paste0( orgPath, .Platform$file.sep, "config" )
  tempPath <- paste0( confPath, .Platform$file.sep, "templates" )

  # load settings file for user defined settings
  settingsFile <- paste( confPath, .Platform$file.sep, "settings.yml", sep="" )
  settings <- yaml::yaml.load( yaml::read_yaml( settingsFile ) )

  # get the progPath:
  progPath <- find_prog_dir(projNoteRmdPath, settings)

  # get protocol directory path:
  #protocolsPath <- paste0(progPath, .Platform$file.sep, settings[["ProgrammeProtocolsDir"]])

  # define protocol Dir path + Rmd path
  protocolDirPath <- paste0( protocolsPath, .Platform$file.sep, protocolName)
  protocolRmdPath <- paste0( protocolDirPath, .Platform$file.sep, protocolName, ".Rmd")

  # define protocol title from name - remove - & _ and make first letter CAPITAL
  protocolTitle <- gsub("-", " ", gsub("_", " ", protocolName) )
  protocolTitle <- gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", protocolTitle, perl = TRUE)


  #### CHECK FOR ERRORS IN INPUT ####

  # Check protocolName contains NO SPACES:
  if( grepl("\\s+", protocolName) ) {
    stop( paste0("  protocolName contains a SPACE: ", protocolName) )
  }

  # check selection is a Project Note - simple or sub or head
  if( selection[["rmdType"]] != "NOTE"
      && selection[["rmdType"]] != "SUB"
      && selection[["rmdType"]] != "HEAD" ) {
    stop( paste0("  selection is not a Project Note: ", projNoteRmdPath) )
  }

  # Check protocolRmdPath doesnt exist
  if( file.exists(protocolRmdPath) == FALSE ) {
    stop( paste0("  protocol of this name doesn't exists: ", protocolRmdPath) )
  }


  #### Read Rmds ####

  protocolContents <- read_file(protocolRmdPath)
  protocolInsertionContents <- read_file( paste0( tempPath, .Platform$file.sep, protocolInsertionTemplate) )
  projNoteContents <- read_file(projNoteRmdPath)


  #### Create Links Protocol & Project Note ####

  # create link from protocol to project note
  protocolLink <- paste0(settings[["ProtocolLinkFormat"]],
                         create_hyperlink( protocolName, protocolRmdPath, projNoteRmdPath),
                         settings[["ProtocolLinkFormat"]])

  #### Get Protocol from file ####

  # compute location in protocolContents of protocol
  headerLine <- grep_line_index(load_param_vector(settings[["ProtocolHeader"]], orgPath),
                                     protocolContents)
  sopHeaderLine <- grep_line_index_from(load_param_vector(settings[["ProtocolSopHeader"]], orgPath),
                                     protocolContents, headerLine)
  # get footerline by finding ProtocolResultsLogHeader first then working BACK to find ProtocolFooter
  resultsLogLine <- grep_line_index_from(load_param_vector(settings[["ProtocolResultsLogHeader"]], orgPath),
                                         protocolContents, sopHeaderLine)
  pFooterVector <- load_param_vector(settings[["ProtocolFooter"]], orgPath)
  fl <- grep_line_index_from_rev(pFooterVector, protocolContents, resultsLogLine)
  footerLine <- fl - length(pFooterVector) # minus length of vector to remove actual footer line!


  #### Replace markup in Protocol Insertion Template ####

  protocolInsertionContents <- sub_template_param(protocolInsertionContents,
                                        "{{PROTOCOL_INSERTION_SEP}}",
                                        settings[["ProtocolInsertionSep"]],
                                        orgPath)

  protocolInsertionContents <- sub_template_param(protocolInsertionContents,
                                        "{{PROTOCOL_INSERTION_TITLE}}",
                                        paste0(settings[["ProtocolInsertionTitle"]], protocolTitle),
                                        orgPath)

  protocolInsertionContents <- sub_template_param(protocolInsertionContents,
                                                  "{{PROTOCOL_INSERTION_LINK}}",
                                                  protocolLink,
                                                  orgPath)

  protocolInsertionContents <- sub_template_param(protocolInsertionContents,
                                                  "{{PROTOCOL_INSERTION_CONTENT}}",
                                                  protocolContents[sopHeaderLine:footerLine],
                                                  orgPath)


  #### Add Protocol Insertion Template to Project Note

  projNoteContents <- insert_at_indices(projNoteContents,
                                        noteInsertionIndex, protocolInsertionContents)


  #### write Header Note ####

  write_file(projNoteContents, projNoteRmdPath)

  cat( "  Inserted Protocol into Project Note: ", projNoteRmdPath, "\n" )

}

