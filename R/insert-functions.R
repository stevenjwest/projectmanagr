
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



#' Insert Content from Source Project Note into a new Project Note
#'
#' This Function adds Insertable Content in to a Project Note. Insertable Content
#' are Document Sections between special syntax that exist in a Source Project
#' Note - created with `create_content()` function.
#'
#' * The special syntax in the Content `{{INSERTABLE_CONTENT_LINK}}` is replaced
#'  with a relative link from Destination to the Source Project Note.
#'
#' * All links in the Content are UPDATED to work from the destination Project
#'  Note.
#'
#' All graphics files included in `knitr::include_graphics()` r calls are
#'  linked to the new Project Note appropriately.
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
#' @export
insert_content <- function(selectionSource, selectionDestination) {

  cat( "\nprojectmanagr::insert_content():\n" )

  # @param contentInsertionTemplate Template file that contains boilerplate content
  # for content insertion into project note.
  # contentInsertionTemplate="Content-Insertion-Template.Rmd"


  #### Set Instance Variables ####

  sourceNoteRmdPath <- selectionSource[["filePath"]] # presumed to be project note Rmd
  sourceNoteContentIndex <- selectionSource[["originalLineNumber"]]

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

  # open content sep delimiter
  contentSepContents <- load_param_vector(settings[["ContentSep"]], orgPath)


  #### Read Source Rmd ####

  sourceNoteRmdContents <- read_file(sourceNoteRmdPath)


  #### Check content Exists @ selectionSource ####

  # check content sep exists in sourceNoteRmdContents from sourceNoteContentIndex
  sourceNoteContentEnd <- sourceNoteContentIndex + length(contentSepContents) - 1

  # each element in contentSepContents must match each element in sourceNoteRmdContents from sourceNoteContentIndex
  if( ! all(sourceNoteRmdContents[sourceNoteContentIndex:sourceNoteContentEnd] == contentSepContents) ) {
    stop( paste0("  sourceNoteRmdContents does not contain a content delimiter at sourceNoteContentIndex: ",
                 sourceNoteRmdContents[sourceNoteContentIndex:sourceNoteContentEnd], " delimiter: ", contentSepContents) )
  }


  #### Read Destination & template Rmds ####

  destNoteRmdContents <- read_file(destNoteRmdPath)

  # no longer using insertion template - just insert the content as extracted from source project note
  #contentInsertionContents <- read_file( paste0(tempPath, .Platform$file.sep, contentInsertionTemplate) )


  #### Get Content from source note ####

  # compute location of Content header & title in sourceNoteRmdContents
  contentHeader <- get_content_header(sourceNoteRmdContents, sourceNoteContentIndex, settings, orgPath)
  contentTitle <- get_content_title(sourceNoteRmdContents, sourceNoteContentIndex, settings, orgPath)

  # form a link to the Content in the Source Note from destination note
  contentLink <- create_hyperlink_section(basename(sourceNoteRmdPath), contentHeader, sourceNoteRmdPath, destNoteRmdPath)

  # get Content contents
  contentDelimiterIndices <- match_vector(contentSepContents,
                                           sourceNoteRmdContents[sourceNoteContentIndex:length(sourceNoteRmdContents)])
  # the Content of interest is between FIRST and SECOND delim indices
  contentStartIndex <- sourceNoteContentIndex + length(contentSepContents)
  contentEndIndex <- sourceNoteContentIndex + contentDelimiterIndices[2] - length(contentSepContents)

  contentContents <- sourceNoteRmdContents[contentStartIndex:contentEndIndex]


  #### Replace Template Params ####

  # replace {{INSERTABLE_CONTENT_LINK}} template param
  contentContents <- sub_template_param(contentContents, "{{INSERTABLE_CONTENT_LINK}}",
                                        contentLink, orgPath)

  # replace {{PREFIX}} template param with Destination Project Note Prefix
  contentContents <- sub_template_param(contentContents, "{{PREFIX}}",
                                        get_prefix(destNoteRmdPath, settings), orgPath)

  # every knitr::include_graphics link, replace the path!
  contentContents <- replace_knitr_include_graphics_link(contentContents, sourceNoteRmdPath,
                                                         destNoteRmdPath, settings, orgPath)

  #### Add Protocol Insertion Template to Destination Project Note

  destNoteRmdContents <- insert_at_indices(destNoteRmdContents,
                                        noteInsertionIndex, contentContents)


  #### write Project Note ####

  write_file(destNoteRmdContents, destNoteRmdPath)

  cat( "  Inserted Content into Project Note: ", destNoteRmdPath, "\n" )

}



#' Insert lines into a Project Note
#'
#' This Function adds `lines` to a Project Note.
#'
#' @param selection Selection object from Project Note.
#' @param lines character vector of lines to insert.
#'
#' @export
insert_lines <- function(selection, lines) {

  cat( "\nprojectmanagr::insert_lines():\n" )


  #### Set Instance Variables ####

  sourceNoteRmdPath <- selection[["filePath"]] # presumed to be project note Rmd
  sourceNoteLineIndex <- selection[["originalLineNumber"]]

  # get orgPath
  orgPath <- find_org_directory(sourceNoteRmdPath)

  if(orgPath == "" ) { # only if orgPath not identified
    stop( paste0("  Cannot identify organisation directory: ", sourceNoteRmdPath) )
  }
  # orgPath is root dir of the organisation - same for both source and destination

  # set confPath + tempPath - these names are FIXED:
  confPath <- paste0( orgPath, .Platform$file.sep, "config" )
  tempPath <- paste0( confPath, .Platform$file.sep, "templates" )

  # load settings file for user defined settings
  settingsFile <- paste( confPath, .Platform$file.sep, "settings.yml", sep="" )
  settings <- yaml::yaml.load( yaml::read_yaml( settingsFile ) )


  #### Read Source Rmd ####

  sourceNoteRmdContents <- read_file(sourceNoteRmdPath)


  #### Add lines to Project Note ####

  sourceNoteRmdContents <- insert_at_indices(sourceNoteRmdContents,
                                             sourceNoteLineIndex, lines)


  #### write Project Note ####

  write_file(sourceNoteRmdContents, sourceNoteRmdPath)

  cat( "  Inserted lines into Project Note: ", sourceNoteRmdPath, "\n" )

}

