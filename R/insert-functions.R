
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

  # get config templates settings yml
  confPath <- get_config_dir(orgPath)
  tempPath <- get_template_dir(orgPath)
  settings <- get_settings_yml(orgPath)


  #### Read Rmds ####

  projDocContents <- read_file(projDocPath)
  goalSectionContents <- read_file(paste0( tempPath, .Platform$file.sep, goalSectionTemplate))


  #### Replace markup in Template with values ####

  # remake selection object at last line - will return the final goal section position
  selection <- user_selection(projDocPath, length(projDocContents))

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

  # write Task Task Log header
  goalSectionContents <- sub_template_param(goalSectionContents, "{{PROJECT_TASK_LOG}}",
                                        settings[["ProjectTaskLogHeader"]], orgPath)

  # compute location in projDocContents to insert goalSectionContents
  goalFooterLine <- grep_line_index_from(load_param_vector(settings[["ProjectGoalSep"]], orgPath),
                                         projDocContents, selection[["goalLine"]], orgPath)

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

  # get config templates settings yml
  confPath <- get_config_dir(orgPath)
  tempPath <- get_template_dir(orgPath)
  settings <- get_settings_yml(orgPath)


  #### Read Rmds ####

  projDocContents <- read_file(projDocPath)
  deliverableSectionContents <- read_file(paste0( tempPath, .Platform$file.sep, deliverableSectionTemplate))


  #### Replace markup in Template with values ####

  # get end of goal section & make new selection here
  goalFooterLine <- grep_line_index_from(load_param_vector(settings[["ProjectGoalSep"]], orgPath),
                                         projDocContents, selection[["goalLine"]], orgPath)

  selection <- user_selection(projDocPath, goalFooterLine) # selection now captures LAST deliverable in this goal section


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

  # write Task Task Log header
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

  # get config templates settings yml
  confPath <- get_config_dir(orgPath)
  tempPath <- get_template_dir(orgPath)
  settings <- get_settings_yml(orgPath)


  #### Read Rmds ####

  projDocContents <- read_file(projDocPath)
  taskSectionContents <- read_file(paste0( tempPath, .Platform$file.sep, taskSectionTemplate))

  #### Replace markup in Template with values ####

  # get end of deliverable section & make new selection here
  deliverableFooterLine <- grep_line_index_from(load_param_vector(settings[["ProjectDeliverableSep"]], orgPath),
                                         projDocContents, selection[["delLine"]], orgPath)

  selection <- user_selection(projDocPath, deliverableFooterLine) # selection now captures LAST task in this del section

  # modify task sep, header, footer vals
  taskSectionContents <- sub_template_param(taskSectionContents, "{{TASK_SEP}}",
                                                   settings[["ProjectTaskSep"]], orgPath)
  taskSectionContents <- sub_template_param(taskSectionContents, "{{TASK_HEADER}}",
                                                   settings[["ProjectTaskHeader"]], orgPath)
  taskSectionContents <- sub_template_param(taskSectionContents, "{{TASK_FOOTER}}",
                                                   settings[["ProjectTaskFooter"]], orgPath)

  # write Task Task Log header
  taskSectionContents <- sub_template_param(taskSectionContents, "{{PROJECT_TASK_LOG}}",
                                                   settings[["ProjectTaskLogHeader"]], orgPath)

  # insert task section template into projDoc at end of current deliverable section
  projDocContents <- insert_at_indices(projDocContents, deliverableFooterLine, taskSectionContents)

  write_file(projDocContents, projDocPath)

  cat(  "    Added Task Section to Project Doc: ", basename(projDocPath), "\n" )

  # return insert_position
  deliverableFooterLine

}



#' Insert Content from Source Content File into a Project Note
#'
#' This Function adds Insertable Content in to a Project Note. Insertable Content
#' are text files that contain templated content for use in project notes -
#' created with `create_content()` function.
#'
#' * The declared parameter in the Content `{{INSERTABLE_CONTENT_LINK}}` is replaced
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
#'  DELIMITER that indicates the start of the Protocol declaration. Use
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

  # get config templates settings yml
  confPath <- get_config_dir(orgPath)
  tempPath <- get_template_dir(orgPath)
  settings <- get_settings_yml(orgPath)


  #### Extract Content Declaration parameters ####

  contentDeclaration <- get_content_declaration(sourceNoteRmdPath, selectionSource,
                                                  settings, orgPath)


  #### Read Destination Rmd ####

  destNoteRmdContents <- read_file(destNoteRmdPath)


  #### Get Content from source note ####

  contentSourceContents <- read_file(contentDeclaration$contentSource)


  #### Form Links ####

  # form a link to the Content in the Source Note from destination note
  contentLink <- create_hyperlink_section(basename(sourceNoteRmdPath),
                                          contentDeclaration$projectNoteContentHeader,
                                          sourceNoteRmdPath, destNoteRmdPath)

  # form relative link to Source Note from Content Declaration
  # if this appears in concent declaration need to update it when copying to destination note
  contentDeclToSourceLink <- create_hyperlink(basename(sourceNoteRmdPath),
                                              sourceNoteRmdPath, contentDeclaration$contentSource)


  #### Replace links in content ####

  # every knitr::include_graphics link, replace the path!
  contentSourceContents <- replace_knitr_include_graphics_link(
    contentSourceContents,
    contentDeclaration$contentSource,
    destNoteRmdPath)

  # replace all hyper links with relative path to destination note
  contentSourceContents <- replace_hyper_links(
    contentSourceContents,
    contentDeclaration$contentSource,
    destNoteRmdPath)



  #### Replace Template Params ####

  # replace {{INSERTABLE_CONTENT_LINK}} template param
  contentSourceContents <- sub_template_param(contentSourceContents, "{{INSERTABLE_CONTENT_LINK}}",
                                        contentLink, orgPath)

  # replace {{PREFIX}} template param with Destination Project Note Prefix
  contentSourceContents <- sub_template_param(contentSourceContents, "{{PREFIX}}",
                                        get_prefix(destNoteRmdPath, settings), orgPath)


  #### Add Content to Project Note ####

  destNoteRmdContents <- insert_at_indices(destNoteRmdContents,
                                        noteInsertionIndex, contentSourceContents)


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

  # get config templates settings yml
  confPath <- get_config_dir(orgPath)
  tempPath <- get_template_dir(orgPath)
  settings <- get_settings_yml(orgPath)


  #### Read Source Rmd ####

  sourceNoteRmdContents <- read_file(sourceNoteRmdPath)


  #### Add lines to Project Note ####

  sourceNoteRmdContents <- insert_at_indices(sourceNoteRmdContents,
                                             sourceNoteLineIndex, lines)


  #### write Project Note ####

  write_file(sourceNoteRmdContents, sourceNoteRmdPath)

  cat( "  Inserted lines into Project Note: ", sourceNoteRmdPath, "\n" )

}



#' Insert header link into subnote
#'
insert_header_link_subnote <- function(subNoteContents, headerNoteFileName,
                                       headerNoteRmdPath, subNoteRmdPath,
                                       headerNoteContentLinkContents,
                                       settings, orgPath) {


  #### Insert header link content into subnote ####

  headerNoteContentLink <- create_hyperlink( headerNoteFileName, headerNoteRmdPath, subNoteRmdPath)
  headerNoteContentLinkContents <- sub_template_param(headerNoteContentLinkContents,
                                                      "{{SUB_NOTE_CONTENT_LINK}}",
                                                      headerNoteContentLink, orgPath)

  noteContentsHeadIndex <- match_line_index( load_param_vector(settings[["SubNoteContentsHeader"]], orgPath),
                                             subNoteContents)
  noteContentsFootIndex <- grep_line_index_from( load_param_vector(settings[["SubNoteContentsFooter"]], orgPath),
                                                 subNoteContents, noteContentsHeadIndex, orgPath)

  subNoteContents <- insert_at_indices(subNoteContents, noteContentsFootIndex, headerNoteContentLinkContents)

  subNoteContents # return

}

#' Insert header link into subnote
#'

insert_subnote_link_header <- function(headerNoteRmdContents, subNoteFileName,
                           subNoteRmdPath, headerNoteRmdPath,
                           subNoteContentLinkContents,
                           settings, orgPath) {

  subNoteContentLink <- create_hyperlink( subNoteFileName, subNoteRmdPath, headerNoteRmdPath)
  subNoteContentLinkContents <- sub_template_param(subNoteContentLinkContents,
                                                   "{{HEADER_NOTE_CONTENT_LINK}}",
                                                   subNoteContentLink, orgPath)

  noteContentsHeadIndex <- match_line_index( load_param_vector(settings[["HeaderNoteContentsHeader"]], orgPath),
                                             headerNoteRmdContents)
  noteContentsFootIndex <- grep_line_index_from( load_param_vector(settings[["HeaderNoteContentsFooter"]], orgPath),
                                                 headerNoteRmdContents, noteContentsHeadIndex, orgPath)

  headerNoteRmdContents <- insert_at_indices(headerNoteRmdContents, noteContentsFootIndex, subNoteContentLinkContents)

  headerNoteRmdContents # return

}
