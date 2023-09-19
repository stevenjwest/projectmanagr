
#' Append a Goal Section to Project Doc
#'
#' Appends a new Goal Section to a Project Doc.  Searches through the Project
#' Doc contents to identify the last Goal Section, and inserts a new goal
#' section template, incremented from the last Goal Section number.
#'
#' This function will only work if the selection is in a project doc.  The goal
#' section will be placed in correction position and with correct spacing
#' according to the `goalSectionTemplate`.
#'
#' @param selection Selection object from the Project Doc file where the goal
#' section is to be inserted.  Use `projectmanagr::cursor_selection()` or
#' `projectmanagr::user_selection()` to create this object.
#'
#' @param goalSectionTemplate Template file that contains boilerplate content
#' for goal section insertion into project doc
#'
#' @export
insert_doc_goal_section <- function(selection,
                                    goalSectionTemplate="Project-Doc-Section-Goal-Template.Rmd") {


  cat( "\nprojectmanagr::insert_doc_goal_section():\n" )


  #### CHECK FOR ERRORS IN INPUT ####

  # check selection is a project DOC
  if( selection[["rmdType"]] != "DOC" ) {
    stop( paste0("  selection is not a Project DOC: ", selection[["filePath"]]) )
  }

  # check selection is in project DOC GDT section - this is the case if errorMessage is in selection
  #if( any(names(selection) == "errorMessage") ) {
  #  stop( paste0("  selection is not inside Project DOC GDTs: ", selection[["errorMessage"]]) )
  #}


  #### Set Instance Variables ####

  projDocPath <- selection$filePath

  # get orgPath
  orgPath <- find_org_directory(selection$filePath)

  if(orgPath == "" ) { # only if orgPath not identified
    stop( paste0("  file selection not in ProjectManagr organisation: ", selection$filePath) )
  }
  # now, orgPath should be the root dir of the organisation

  # set confPath + tempPath - these names are FIXED:
  confPath <- paste0( orgPath, .Platform$file.sep, "config" )
  tempPath <- paste0( confPath, .Platform$file.sep, "templates" )

  # load settings file for user defined settings
  settingsFile <- paste( confPath, .Platform$file.sep, "settings.yml", sep="" )
  settings <- yaml::yaml.load( yaml::read_yaml( settingsFile ) )


  #### Read Rmds ####

  projDocContents <- read_file(projDocPath)
  goalSectionContents <- read_file(paste0( tempPath, .Platform$file.sep, goalSectionTemplate))


  #### Replace markup in Template with values ####

  # remake selection object at last line - will return the final goal section position
  selection <- user_selection(projDocPath, length(projDocContents))

  # compute next goal number & add to goal section
  goalNum <- get_goal_number(selection[["goal"]], settings)
  goalNumNext <- goalNum + 1

  goalSectionContents <- sub_template_param(goalSectionContents, "{{GOAL_NUMBER}}",
                                            goalNumNext, orgPath)

  # modify goal/del/task sep, header, footer vals
  goalSectionContents <- sub_template_param(goalSectionContents, "{{GOAL_SEP}}",
                                            settings[["ProjectGoalSep"]], orgPath)
  goalSectionContents <- sub_template_param(goalSectionContents, "{{GOAL_HEADER}}",
                                            settings[["ProjectGoalHeader"]], orgPath)

  goalSectionContents <- sub_template_param(goalSectionContents, "{{DELIVERABLE_SEP}}",
                                            settings[["ProjectDeliverableSep"]], orgPath)
  goalSectionContents <- sub_template_param(goalSectionContents, "{{DELIVERABLE_HEADER}}",
                                            settings[["ProjectDeliverableHeader"]], orgPath)

  goalSectionContents <- sub_template_param(goalSectionContents, "{{TASK_SEP}}",
                                            settings[["ProjectTaskSep"]], orgPath)
  goalSectionContents <- sub_template_param(goalSectionContents, "{{TASK_HEADER}}",
                                            settings[["ProjectTaskHeader"]], orgPath)

  goalSectionContents <- sub_template_param(goalSectionContents, "{{TASK_FOOTER}}",
                                            settings[["ProjectTaskFooter"]], orgPath)

  # write Task Overview & Task Log values
  goalSectionContents <- sub_template_param(goalSectionContents, "{{PROJECT_TASK_OVERVIEW}}",
                                        settings[["ProjectTaskOverviewHeader"]], orgPath)
  goalSectionContents <- sub_template_param(goalSectionContents, "{{PROJECT_TASK_LOG}}",
                                        settings[["ProjectTaskLogHeader"]], orgPath)

  # compute location in projDocContents to insert goalSectionContents
  goalFooterLine <- grep_line_index_from(load_param_vector(settings[["ProjectGoalSep"]], orgPath),
                                         projDocContents, selection[["goalLine"]])

  # insert goal section template into projDoc at end of current goal section
  projDocContents <- insert_at_indices(projDocContents, goalFooterLine, goalSectionContents)

  write_file(projDocContents, projDocPath)

  cat(  "    Added Goal Section to Project Doc: ", basename(projDocPath), "\n" )

}



#' Append a Deliverable Section in Project Doc
#'
#' Appends a new Deliverable Section in a Project Doc based on the cursor
#' position.  Identifies the current Goal segment as selected by the cursor
#' position, and inserts a new deliverable section template into this segment,
#' incremented from the last Deliverable Section number.
#'
#' This function will only work if the selection is in a Project Doc.  The
#' deliverable section will be placed in correction position and with correct
#' spacing according to the `deliverableSectionTemplate`.
#'
#' @param selection Selection object from the Project Doc file where the
#' deliverable section is to be inserted.  Use
#' `projectmanagr::cursor_selection()` or `projectmanagr::user_selection()` to
#' create this object.
#'
#' @param deliverableSectionTemplate Template file that contains boilerplate
#' content for deliverable section insertion into project doc.
#'
#' @export
insert_doc_deliverable_section <- function(selection,
                                    deliverableSectionTemplate="Project-Doc-Section-Deliverable-Template.Rmd") {

  cat( "\nprojectmanagr::insert_doc_deliverable_section():\n" )


  #### CHECK FOR ERRORS IN INPUT ####

  # check selection is a project DOC
  if( selection[["rmdType"]] != "DOC" ) {
    stop( paste0("  selection is not a Project DOC: ", selection[["filePath"]]) )
  }

  # check selection is in project DOC GDT section - this is the case if errorMessage is in selection
  if( any(names(selection) == "errorMessage") ) {
    stop( paste0("  selection is not inside Project DOC GDTs: ", selection[["errorMessage"]]) )
  }


  #### Set Instance Variables ####

  projDocPath <- selection$filePath

  # get orgPath
  orgPath <- find_org_directory(selection$filePath)

  if(orgPath == "" ) { # only if orgPath not identified
    stop( paste0("  file selection not in ProjectManagr organisation: ", selection$filePath) )
  }
  # now, orgPath should be the root dir of the organisation

  # set confPath + tempPath - these names are FIXED:
  confPath <- paste0( orgPath, .Platform$file.sep, "config" )
  tempPath <- paste0( confPath, .Platform$file.sep, "templates" )

  # load settings file for user defined settings
  settingsFile <- paste( confPath, .Platform$file.sep, "settings.yml", sep="" )
  settings <- yaml::yaml.load( yaml::read_yaml( settingsFile ) )


  #### Read Rmds ####

  projDocContents <- read_file(projDocPath)
  deliverableSectionContents <- read_file(paste0( tempPath, .Platform$file.sep, deliverableSectionTemplate))


  #### Replace markup in Template with values ####

  # get end of goal section & make new selection here
  goalFooterLine <- grep_line_index_from(load_param_vector(settings[["ProjectGoalSep"]], orgPath),
                                         projDocContents, selection[["goalLine"]])

  selection <- user_selection(projDocPath, goalFooterLine) # selection now captures LAST deliverable in this goal section


  # compute next deliverable number & add to deliverable section
  delNum <- get_deliverable_number(selection[["deliverable"]], settings)
  delNumNext <- delNum + 1

  deliverableSectionContents <- sub_template_param(deliverableSectionContents,
                                                   "{{DELIVERABLE_NUMBER}}",
                                                   delNumNext, orgPath)

  # modify del/task sep, header, footer vals
  deliverableSectionContents <- sub_template_param(deliverableSectionContents, "{{DELIVERABLE_SEP}}",
                                            settings[["ProjectDeliverableSep"]], orgPath)
  deliverableSectionContents <- sub_template_param(deliverableSectionContents, "{{DELIVERABLE_HEADER}}",
                                            settings[["ProjectDeliverableHeader"]], orgPath)

  deliverableSectionContents <- sub_template_param(deliverableSectionContents, "{{TASK_SEP}}",
                                            settings[["ProjectTaskSep"]], orgPath)
  deliverableSectionContents <- sub_template_param(deliverableSectionContents, "{{TASK_HEADER}}",
                                            settings[["ProjectTaskHeader"]], orgPath)

  deliverableSectionContents <- sub_template_param(deliverableSectionContents, "{{TASK_FOOTER}}",
                                            settings[["ProjectTaskFooter"]], orgPath)

  # write Task Overview & Task Log values
  deliverableSectionContents <- sub_template_param(deliverableSectionContents, "{{PROJECT_TASK_OVERVIEW}}",
                                            settings[["ProjectTaskOverviewHeader"]], orgPath)
  deliverableSectionContents <- sub_template_param(deliverableSectionContents, "{{PROJECT_TASK_LOG}}",
                                            settings[["ProjectTaskLogHeader"]], orgPath)

  # insert deliverable section template into projDoc at end of current goal section
  projDocContents <- insert_at_indices(projDocContents, goalFooterLine, deliverableSectionContents)

  write_file(projDocContents, projDocPath)

  cat(  "    Added Deliverable Section to Project Doc: ", basename(projDocPath), "\n" )

}


#' Append a Task Section in Project Doc
#'
#' Appends a new Task Section in a Project Doc based on the cursor
#' position.  Identifies the current Goal segment as selected by the cursor
#' position, and inserts a new Task section template into this segment,
#' incremented from the last Deliverable Section number.
#'
#' This function will only work if the selection is in a Project Doc.  The
#' task section will be placed in correction position and with correct
#' spacing according to the `taskSectionTemplate`.
#'
#' @param selection Selection object from the Project Doc file where the
#' task section is to be inserted.  Use
#' `projectmanagr::cursor_selection()` or `projectmanagr::user_selection()` to
#' create this object.
#'
#' @param taskSectionTemplate Template file that contains boilerplate
#' content for task section insertion into project doc.
#'
#' @export
insert_doc_task_section <- function(selection,
                                    taskSectionTemplate="Project-Doc-Section-Task-Template.Rmd") {

  cat( "\nprojectmanagr::insert_doc_task_section():\n" )


  #### CHECK FOR ERRORS IN INPUT ####

  # check selection is a project DOC
  if( selection[["rmdType"]] != "DOC" ) {
    stop( paste0("  selection is not a Project DOC: ", selection[["filePath"]]) )
  }

  # check selection is in project DOC GDT section - this is the case if errorMessage is in selection
  if( any(names(selection) == "errorMessage") ) {
    stop( paste0("  selection is not inside Project DOC GDTs: ", selection[["errorMessage"]]) )
  }


  #### Set Instance Variables ####

  projDocPath <- selection$filePath

  # get orgPath
  orgPath <- find_org_directory(selection$filePath)

  if(orgPath == "" ) { # only if orgPath not identified
    stop( paste0("  file selection not in ProjectManagr organisation: ", selection$filePath) )
  }
  # now, orgPath should be the root dir of the organisation

  # set confPath + tempPath - these names are FIXED:
  confPath <- paste0( orgPath, .Platform$file.sep, "config" )
  tempPath <- paste0( confPath, .Platform$file.sep, "templates" )

  # load settings file for user defined settings
  settingsFile <- paste( confPath, .Platform$file.sep, "settings.yml", sep="" )
  settings <- yaml::yaml.load( yaml::read_yaml( settingsFile ) )


  #### Read Rmds ####

  projDocContents <- read_file(projDocPath)
  taskSectionContents <- read_file(paste0( tempPath, .Platform$file.sep, taskSectionTemplate))

  #### Replace markup in Template with values ####

  # get end of deliverable section & make new selection here
  deliverableFooterLine <- grep_line_index_from(load_param_vector(settings[["ProjectDeliverableSep"]], orgPath),
                                         projDocContents, selection[["delLine"]])

  selection <- user_selection(projDocPath, deliverableFooterLine) # selection now captures LAST task in this del section


  # compute next task number & add to task section
  taskNum <- get_task_number(selection[["task"]], settings)
  taskNumNext <- taskNum + 1

  taskSectionContents <- sub_template_param(taskSectionContents,
                                                   "{{TASK_NUMBER}}",
                                                   taskNumNext, orgPath)

  # modify task sep, header, footer vals
  taskSectionContents <- sub_template_param(taskSectionContents, "{{TASK_SEP}}",
                                                   settings[["ProjectTaskSep"]], orgPath)
  taskSectionContents <- sub_template_param(taskSectionContents, "{{TASK_HEADER}}",
                                                   settings[["ProjectTaskHeader"]], orgPath)
  taskSectionContents <- sub_template_param(taskSectionContents, "{{TASK_FOOTER}}",
                                                   settings[["ProjectTaskFooter"]], orgPath)

  # write Task Overview & Task Log values
  taskSectionContents <- sub_template_param(taskSectionContents, "{{PROJECT_TASK_OVERVIEW}}",
                                                   settings[["ProjectTaskOverviewHeader"]], orgPath)
  taskSectionContents <- sub_template_param(taskSectionContents, "{{PROJECT_TASK_LOG}}",
                                                   settings[["ProjectTaskLogHeader"]], orgPath)

  # insert task section template into projDoc at end of current deliverable section
  projDocContents <- insert_at_indices(projDocContents, deliverableFooterLine, taskSectionContents)

  write_file(projDocContents, projDocPath)

  cat(  "    Added Task Section to Project Doc: ", basename(projDocPath), "\n" )

  # return insert_position
  deliverableFooterLine

}



#' Insert a Protocol from Source Project Note into a new Project Note
#'
#' This Function adds a Protocol to a Project Note - protocols are created and
#'  formed in a Source Project Note.
#'
#' * Recommended to place all Project Notes that define Protocols into a
#'  Programme-wide directory with clear name (eg. protocol/), so all protocols
#'  for a programme can be easily and quickly located.
#'
#' All links in the Protocol are UPDATED to work from the destination Project
#'  Note.
#'
#' All graphics included in a knitr::include_graphics() r code chunk are
#'  transferred to the new Project Note DIR and linked appropriately.
#'
#' @param selectionSource Selection object from Project Note file containing
#'  the Protocol to be inserted.  The selection must be on the FIRST PROTOCOL
#'  DELIMITER that indicates the start of the Protocol. Use
#'  `projectmanagr::cursor_selection()` or `projectmanagr::user_selection()` to
#'  create this object.
#'
#' @param selectionDestination Selection object from the Project Note file where
#'  the protocol is to be inserted.  Use `projectmanagr::cursor_selection()` or
#'  `projectmanagr::user_selection()` to create this object.
#'
#' @param protocolInsertionTemplate Template file that contains boilerplate content
#' for protocol insertion into project note.
#'
#' @export
insert_protocol <- function(selectionSource, selectionDestination,
                            protocolInsertionTemplate="Protocol-Insertion-Template.Rmd") {

  cat( "\nprojectmanagr::insert_protocol():\n" )


  #### Set Instance Variables ####

  sourceNoteRmdPath <- selectionSource[["filePath"]] # presumed to be project note Rmd
  sourceNoteProtocolIndex <- selectionSource[["originalLineNumber"]]

  destNoteRmdPath <- selectionDestination[["filePath"]] # presumed to be project note Rmd
  noteInsertionIndex <- selectionDestination[["originalLineNumber"]]

  # get orgPath
  orgPath <- find_org_directory(sourceNoteRmdPath)
  orgPathDest <- find_org_directory(destNoteRmdPath)

  if(orgPath == "" ) { # only if orgPath not identified
    stop( paste0("  Cannot identify organisation directory: ", sourceNoteRmdPath) )
  } else if(orgPath != orgPathDest) {
    stop( paste0("  Different organisation directory for source and destination: ", sourceNoteRmdPath, " ", destNoteRmdPath) )
  }
  # orgPath is root dir of the organisation - same for both source and destination

  # set confPath + tempPath - these names are FIXED:
  confPath <- paste0( orgPath, .Platform$file.sep, "config" )
  tempPath <- paste0( confPath, .Platform$file.sep, "templates" )

  # load settings file for user defined settings
  settingsFile <- paste( confPath, .Platform$file.sep, "settings.yml", sep="" )
  settings <- yaml::yaml.load( yaml::read_yaml( settingsFile ) )

  # open protocol sep delimiter
  protocolSepContents <- read_file( paste0( tempPath, .Platform$file.sep, "PROTOCOL_SEP.txt") )

  # get the progPath:
  #progPath <- find_prog_dir(projNoteRmdPath, settings)

  # get protocol directory path:
  #protocolsPath <- paste0(progPath, .Platform$file.sep, settings[["ProgrammeProtocolsDir"]])

  # define protocol Dir path + Rmd path
  #protocolDirPath <- paste0( protocolsPath, .Platform$file.sep, protocolName)
  #protocolRmdPath <- paste0( protocolDirPath, .Platform$file.sep, protocolName, ".Rmd")

  # define protocol title from name - remove - & _ and make first letter CAPITAL
  #protocolTitle <- gsub("-", " ", gsub("_", " ", protocolName) )
  #protocolTitle <- gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", protocolTitle, perl = TRUE)


  #### CHECK FOR ERRORS IN INPUT ####

  # check selectionDestination is a Project Note - simple or sub or head
  if( selectionDestination[["rmdType"]] != "NOTE"
      && selectionDestination[["rmdType"]] != "SUB"
      && selectionDestination[["rmdType"]] != "HEAD" ) {
    stop( paste0("  selectionDestination is not a Project Note: ", projNoteRmdPath) )
  }

  # Check protocolRmdPath doesnt exist
  #if( file.exists(protocolRmdPath) == FALSE ) {
  #  stop( paste0("  protocol of this name doesn't exists: ", protocolRmdPath) )
  #}


  #### Read Source Rmd ####

  sourceNoteRmdContents <- read_file(sourceNoteRmdPath)


  #### Check Protocol Exists @ selectionSource ####

  # check protocol sep exists in sourceNoteRmdContents from sourceNoteProtocolIndex
  sourceNoteProtocolEnd <- sourceNoteProtocolIndex + length(protocolSepContents) - 1

  # each element in protocolSepContents must match each element in sourceNoteRmdContents from sourceNoteProtocolIndex
  if( ! all(sourceNoteRmdContents[sourceNoteProtocolIndex:sourceNoteProtocolEnd] == protocolSepContents) ) {
    stop( paste0("  sourceNoteRmdContents does not contain a protocol delimiter at sourceNoteProtocolIndex: ",
                 sourceNoteRmdContents[sourceNoteProtocolIndex:sourceNoteProtocolEnd], " delimiter: ", protocolSepContents) )
  }


  #### Read Destination & template Rmds ####

  destNoteRmdContents <- read_file(destNoteRmdPath)
  protocolInsertionContents <- read_file( paste0(tempPath, .Platform$file.sep, protocolInsertionTemplate) )


  #### Get Protocol from source note ####

  # compute location of protocol header & title in sourceNoteRmdContents
  protocolHeader <- get_protocol_header(sourceNoteRmdContents, sourceNoteProtocolIndex, settings, orgPath)
  protocolTitle <- get_protocol_title(sourceNoteRmdContents, sourceNoteProtocolIndex, settings, orgPath)

  # form a link to the Protocol in the Source Note from destination note
  protocolLink <- create_hyperlink_section(basename(sourceNoteRmdPath), protocolHeader, sourceNoteRmdPath, destNoteRmdPath)

  # get protocol contents
  protocolDelimiterIndices <- match_vector(protocolSepContents,
                                           sourceNoteRmdContents[sourceNoteProtocolIndex:length(sourceNoteRmdContents)])
  # the protocol of interest is between FIRST and SECOND delim indices
  protocolStartIndex <- sourceNoteProtocolIndex + length(protocolSepContents)
  protocolEndIndex <- sourceNoteProtocolIndex + protocolDelimiterIndices[2] - length(protocolSepContents)

  protocolContents <- sourceNoteRmdContents[protocolStartIndex:protocolEndIndex]


  #### Add Protocol to Insertion Template ####

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
                                                  protocolLink, orgPath)


  protocolInsertionContents <- sub_template_param(protocolInsertionContents,
                                                  "{{PROTOCOL_INSERTION_CONTENT}}",
                                                  protocolContents, orgPath)



  #### Add Protocol Insertion Template to Destination Project Note

  destNoteRmdContents <- insert_at_indices(destNoteRmdContents,
                                        noteInsertionIndex, protocolInsertionContents)


  #### write Project Note ####

  write_file(destNoteRmdContents, destNoteRmdPath)

  cat( "  Inserted Protocol into Project Note: ", destNoteRmdPath, "\n" )

}

