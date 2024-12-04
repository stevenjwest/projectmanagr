

#' Link Project Document GDT to Project Note
#'
#' Add a cross-reference between a Project Doc at the Goal-Del-Task as shown in
#' `selection` with the simple project note at `projNoteRmdPath`, using the
#' provided templates.
#'
#' @param selection List containing the Goal, Del, Task selected from the Project
#' Doc, as well as other useful information - lines of Task/Del/Goal, projectDoc
#' path content of selection line.  See `cursor_selection()` or `user_selection()`.
#'
#' @param projNoteRmdPath Path to existing Project Note Rmd, in same Org as the
#' Project Doc `selection` object.
#'
#' @param projNoteLinkTemplate Template with structure to add the Project Doc
#' Goal/Del/Task link in the Project Note.  Includes Project Doc title, link,
#' and then links to Goal / Del / Task, plus a Summary Info section (filled
#' with the content from `taskSectionHeaderTemplate`).
#'
#' @param projNoteLinkSummaryTemplate Template with structure to add underneath the
#' Project Doc Goal/Del/Task link in the Project Note.  Includes a 'summary' section
#' and a 'todo' section by default, linked to `NoteObjectivesTodoSectionHeader`
#' in `config/settings.yml`
#'
#' @param projNoteSummaryTemplate Template with structure to add Project Note
#' summary to Project Doc under Goal/Del/Task.
#'
#' @export
link_doc_project_note <- function(selection, projNoteRmdPath,
                               projNoteLinkTemplate="Project-Note-Link-Template.Rmd",
                               projNoteLinkSummaryTemplate="Project-Note-Link-Summary-Template.Rmd",
                               todoTemplate="Todo-Template.Rmd",
                               projNoteSummaryTemplate="Project-Note-Summary-Template.Rmd") {


  cat( "\nprojectmanagr::link_doc_project_note():\n" )


  #### Set Instance Variables ####

  projectDocPath <- selection[["filePath"]]
  # projectNotePath is the parent directory the projNoteRmdPath (Rmd file) sits in
  projectNotePath <- normalizePath( dirname(projNoteRmdPath))

  # Check projectNotePath is a sub-dir in a Programme DIR, which is a sub-dir to the root of an ORGANISATION:
  orgPath <- dirname( dirname(projectNotePath) ) # this should be the orgPath!
  orgPath <- find_org_directory(orgPath)

  if(orgPath == "" ) { # only if orgPath not identified
    stop( paste0("  projectNotePath is not in a sub-dir of a PROGRAMME Directory: ", projectNotePath) )
  }
  # now, orgPath should be the root dir of the organisation

  # get config templates settings yml
  confPath <- get_config_dir(orgPath)
  tempPath <- get_template_dir(orgPath)
  settings <- get_settings_yml(orgPath)


  #### CHECK FOR ERRORS IN INPUT ####

  # confirm projectNotePath is a valid path - contains "~_"
  if( !grepl(settings[["ProjectPrefixSep"]], projNoteRmdPath) ) {
    stop( paste0("  projectNotePath is not valid - no ProjectPrefixSep: ", basename(projNoteRmdPath) ) )
  }


  #### Read Rmds ####

  projNoteRmdContents <- read_file(projNoteRmdPath)

  projNoteLinkContents <- read_file( paste0( tempPath, .Platform$file.sep, projNoteLinkTemplate) )
  projNoteLinkSummaryContents <- read_file( paste0(tempPath, .Platform$file.sep, projNoteLinkSummaryTemplate) )
  todoContents <- read_file( paste0(tempPath, .Platform$file.sep, todoTemplate) )

  projNoteSummaryContents <- read_file( paste0( tempPath, .Platform$file.sep, projNoteSummaryTemplate) )

  projDocContents <- read_file(projectDocPath)


  #### Link Project Note and Project Doc ####

  linkFormed <- link_project_note_doc(selection, settings, projNoteRmdPath, projNoteRmdContents, projNoteLinkContents,
                  projNoteLinkSummaryContents, todoContents, projNoteSummaryContents, projDocContents, orgPath)

  if( linkFormed == FALSE ) {
    # return an error
    stop( paste0("  Linking Project Note Failed - link already exists."))
  }

}


#' Link Project Note to Project Doc GDT
#'
#' Single internal function to perform the linking between a project doc at GDT
#' and a simple project note.
#'
link_project_note_doc <- function(selection, settings, projNoteRmdPath, projNoteRmdContents,
                            projNoteLinkContents, projNoteLinkSummaryContents, todoContents,
                            projNoteSummaryContents, projDocContents, orgPath) {


  #### Set Instance Variables ####

  projectDocPath <- selection[["filePath"]] # selection is a project DOC
  # projectNotePath is the parent directory the projNoteRmdPath (Rmd file) sits in
  projectNotePath <- normalizePath( dirname(projNoteRmdPath))

  # create link from projDoc to projNote
  projNoteName <- substr(basename(projNoteRmdPath), 1, regexpr(".Rmd", basename(projNoteRmdPath))-1)
  projNoteLink <- paste0(settings[["NoteLinkFormat"]],
                         create_hyperlink( projNoteName, projNoteRmdPath, projectDocPath),
                         settings[["NoteLinkFormat"]])

  # compute location in projDocContents to insert the projNoteLink & summary - END OF LOG section
  logLine <- grep_line_index_from(load_param_vector(settings[["ProjectTaskLogHeader"]], orgPath),
                               projDocContents, selection[["taskLine"]], orgPath)
  taskFooterLine <- grep_line_index_from(load_param_vector(settings[["ProjectTaskFooter"]], orgPath),
                                      projDocContents, logLine, orgPath) # end of log section


  #### CHECK FOR ERRORS IN INPUT ####

  # Check that a link to the current projectNote in projDoc under GDT doesnt exist
  if( length( grep(projNoteLink, projDocContents[logLine:taskFooterLine], fixed=TRUE) ) > 0 ) {
    cat("  A link to this Project Note already exists in ProjDoc GDT: ", basename(projectDocPath), "\n" )
    return(FALSE)
    #stop( paste0("  A link to this Project Note already exists in ProjDoc GDT: ", basename(projNoteRmdPath) ) )
  }
  # search for subNoteLink - in case independent subNote link is being added
  subNoteLink <- paste0(settings[["SubNoteLinkFormat"]],
                         create_hyperlink( projNoteName, projNoteRmdPath, projectDocPath),
                         settings[["SubNoteLinkFormat"]])
  if( length( grep(subNoteLink, projDocContents[logLine:taskFooterLine], fixed=TRUE) ) > 0 ) {
    cat("  A link to this SubNote already exists in ProjDoc GDT: ", basename(projectDocPath), "\n" )
    return(FALSE)
    #stop( paste0("  A link to this SubNote already exists in ProjDoc GDT: ", basename(projNoteRmdPath) ) )
  }


  #### Fill ProjDoc GDT Templates for Project Note ####

  DocGDTList <- compute_doc_GDT_link(projectDocPath, projNoteRmdPath, settings,
                                          selection[["goal"]], selection[["deliverable"]],
                                          selection[["task"]])
  # returns list of DOC TITLE, LINK, GOAL, DEL, TASK in NAMED LIST

  # replace projNoteLinkSummaryContents & todoContents
  projNoteLinkSummaryContents <- note_link_summ_params(projNoteLinkSummaryContents,
                                                       todoContents, settings, orgPath)

  # replace markup in projNoteLinkContents
  projNoteLinkContents <- sub_note_link_params(projNoteLinkContents, settings, DocGDTList,
                                               projNoteLinkSummaryContents, orgPath)


  #### add Doc GOAL/DEL/TASK Link to Project Note ####

  # compute location in projNoteRmdContents to insert the GDT Link & summary
  noteObjHeadIndex <- match_line_index( load_param_vector(settings[["NoteObjectivesHeader"]], orgPath),
                                      projNoteRmdContents)
  noteObjFootIndex <- grep_line_index_from( load_param_vector(settings[["NoteObjectivesFooter"]], orgPath),
                                         projNoteRmdContents, noteObjHeadIndex, orgPath)

  projNoteRmdContents <- insert_at_indices(projNoteRmdContents, noteObjFootIndex, projNoteLinkContents)

  write_file(projNoteRmdContents, projNoteRmdPath)

  cat( "  Written Goal Del Task to Project Note file: ", basename(projNoteRmdPath), "\n" )


  #### Write Project Note link & summary to Project DOC ####

  # replace project note log sep
  projNoteSummaryContents <- sub_template_param(projNoteSummaryContents, "{{PROJECT_NOTE_LOG_SEP}}",
                                                settings[["ProjectTaskLogSep"]], orgPath)

  # replace proj note link
  projNoteSummaryContents <- sub_template_param(projNoteSummaryContents, "{{PROJECT_NOTE_LINK}}",
                                                projNoteLink, orgPath)

  # replace proj note summary
  summaryContents <- extract_summary_from_link_contents(projNoteLinkContents, settings, orgPath)

  #- if NoteObjectivesTodoSectionHeader is in summaryBullet, remove everything FROM THAT LINE
  #projNoteLinkSummaryContentsTrim <- projNoteLinkSummaryContents[1 : ifelse( any(grepl(settings[["NoteObjectivesTodoSectionHeader"]],
  #                                                                                     projNoteLinkSummaryContents, fixed=TRUE)),
  #                                                                           grep(settings[["NoteObjectivesTodoSectionHeader"]],
  #                                                                                projNoteLinkSummaryContents, fixed=TRUE)-1,
  #                                                                           length(projNoteLinkSummaryContents)) ]

  projNoteSummaryContents <- sub_template_param(projNoteSummaryContents, "{{PROJECT_NOTE_SUMMARY}}",
                                                summaryContents, orgPath)

  # insert summary into projDoc at end of task - last log entry
  projDocContents <- insert_at_indices(projDocContents, taskFooterLine, projNoteSummaryContents)

  write_file(projDocContents, projectDocPath)

  #cat( "    Written Project Note Link to Project Doc: ", basename(projectDocPath), "\n" )
  cat(  "    Written Project Note Link to Project Doc: ", basename(projectDocPath),
       " at GDT:", "\n  ", DocGDTList[["goal"]],"\n  ",
       DocGDTList[["del"]],"\n  ", DocGDTList[["task"]], "\n" )

  return(TRUE)

}


extract_summary_from_link_contents <- function(projNoteLinkContents, settings, orgPath) {

  # load the params to check their length
  taskLinkLine <-   load_param_vector(settings[["NoteTaskLinkLine"]], orgPath)
  todoSectionHeader <- load_param_vector(settings[["NoteObjectivesTodoSectionHeader"]], orgPath)

  # get indices : G D T & Todo
  pdli <- grep_line_index(paste0(settings[["ProjectLinkFormat"]], "["), projNoteLinkContents, orgPath) # projDoc link index
  gli <- grep_line_index_from(settings[["NoteGoalLinkLine"]], projNoteLinkContents, pdli, orgPath) # goal index
  dli <- grep_line_index_from(settings[["NoteDeliverableLinkLine"]], projNoteLinkContents, gli, orgPath) # del index
  tli <- grep_line_index_from(taskLinkLine, projNoteLinkContents, dli, orgPath) # task index
  # removed NoteObjectivesSummarySectionHeader - using tli as start of summary
  #summi <- grep_line_index_from(settings[["NoteObjectivesSummarySectionHeader"]], projNoteLinkContents, tli) # summary index
  summi <- tli
  todoi <- grep_line_index_from(todoSectionHeader, projNoteLinkContents, summi, orgPath) # todo index

  # extract summary - between tli and todoi NOT INCLUSIVE
  summStart <- (tli+length(taskLinkLine)) # get content starting AFTER taskLinkLine
  summEnd <- (todoi-1) # get content UP TO but not including TodoSectionHeader
  #cat(projNoteLinkContents[(tli+length(taskLinkLine)):(todoi-1)], sep='\n')
  summary <- projNoteLinkContents[summStart:summEnd]

  # edit summary headers
  summary <- note_summary_headers_quote_out(summary)

  summary # return

}


#' quote out all Markdown Headers
#'
#' Converts every line beginning with `#` in `summary` to `>#`
#'
#' This is used to quote-out the markdown headers in Project Note Summary Section,
#' so when it is inserted into the Project Doc, the Markdown Headers will not show
#' in the Project Note Outline in RStudio.  This function is used by `update()`
#' to ensure Project Note summary Markdown Headers are not present in the Doc GDT
#' summary for each Note.
#'
#' When the Project Note Rmd content is converted to html via `render()`, these
#' headers are treated specially - to ensure they are available as sub-headers
#' under a task when clicked on in the html outline (TODO!)
note_summary_headers_quote_out <- function(summary) {

  summary[startsWith(summary, '#')] <- paste0('>', summary[startsWith(summary, '#')])
  summary # return
}

#' quote out all Markdown Headers
#'
#' Converts every line beginning with `>#` in `summary` to `#`
#'
#' This is used to quote-in the markdown headers in Project Note Summary Section,
#' so it forms html headers when rendered to html.
#'
#' When the Project Note Rmd content is converted to html via `render()`, these
#' headers are treated specially - to ensure they are available as sub-headers
#' under a task when clicked on in the html outline (TODO!)
note_summary_headers_quote_in <- function(summary) {

  summary[startsWith(summary, '>#')] <- substring(summary[startsWith(summary, '>#')], 2)
  summary # return
}


#' Link Project Document GDT to Project Note Group (Header plus all Sub Notes)
#'
#' Add a cross-reference between a Project Doc at the Goal-Del-Task as shown in
#' `selection` with the group project note, with header note at
#' `headerNoteRmdPath`, using the provided templates.
#'
#' @param selection List containing the Goal, Del, Task selected from the Project
#' Doc, as well as other useful information - lines of Task/Del/Goal, projectDoc
#' path content of selection line.  See `cursor_selection()` or `user_selection()`.
#'
#' @param headerNoteRmdPath Path to existing Group Header Note Rmd, in same Org
#' as the Project Doc `selection` object.
#'
#' @param projNoteLinkTemplate Template with structure to add the Project Doc
#' Goal/Del/Task link in all Project Notes.  Includes Project Doc title, link,
#' and then links to Goal / Del / Task, plus a Summary Info section (filled
#' with the content from `taskSectionHeaderTemplate`).
#'
#' @param projNoteLinkSummaryTemplate Template with structure to add underneath the
#' Project Doc Goal/Del/Task link in all Sub Note.  Includes a 'summary' section
#' and a 'todo' section by default, linked to `NoteObjectivesTodoSectionHeader`
#' in `config/settings.yml`
#'
#' @param projNoteSummaryTemplate Template with structure to add Sub Note
#' summaries to Project Doc under Goal/Del/Task.
#'
#' @param subNoteSummaryTemplate Template with structure to add Project Sub Note
#' summary to Project Doc under Goal/Del/Task.
#'
#' @export
link_doc_group_note <- function(selection, headerNoteRmdPath,
                             projNoteLinkTemplate="Project-Note-Link-Template.Rmd",
                             projNoteLinkSummaryTemplate="Project-Note-Link-Summary-Template.Rmd",
                             todoTemplate="Todo-Template.Rmd",
                             projNoteSummaryTemplate="Project-Note-Summary-Template.Rmd",
                             subNoteSummaryTemplate="Project-Sub-Note-Summary-Template.Rmd") {


  cat( "\nprojectmanagr::link_doc_group_note():\n" )


  #### Set Instance Variables ####

  projectDocPath <- selection[["filePath"]]
  # projectNotePath is the parent directory the headerNoteRmdPath (Rmd file) sits in
  headerNoteDir <- normalizePath( dirname(headerNoteRmdPath))

  # Check headerNoteDir is a sub-dir in a Programme DIR, which is a sub-dir to the root of an ORGANISATION:
  orgPath <- dirname( dirname(headerNoteDir) ) # this should be the orgPath!
  orgPath <- find_org_directory(orgPath)

  if(orgPath == "" ) { # only if orgPath not identified
    stop( paste0("  headerNoteDir is not in a sub-dir of a PROGRAMME Directory: ", headerNoteDir) )
  }
  # now, orgPath should be the root dir of the organisation

  # get config templates settings yml
  confPath <- get_config_dir(orgPath)
  tempPath <- get_template_dir(orgPath)
  settings <- get_settings_yml(orgPath)


  #### CHECK FOR ERRORS IN INPUT ####

  # confirm headerNoteRmdPath is a valid path - contains "~_"
  if( !grepl(settings[["ProjectPrefixSep"]], headerNoteRmdPath) ) {
    stop( paste0("  headerNoteRmdPath is not valid - no ProjectPrefixSep: ", basename(headerNoteRmdPath) ) )
  }


  #### Read Rmds ####

  # read projNote
  headerNoteContents <- read_file(headerNoteRmdPath)

  headerNoteLinkContents <- read_file( paste0( tempPath, .Platform$file.sep, projNoteLinkTemplate) )

  projNoteLinkContents <- read_file( paste0( tempPath, .Platform$file.sep, projNoteLinkTemplate) )
  projNoteLinkSummaryContents <- read_file( paste0(tempPath, .Platform$file.sep, projNoteLinkSummaryTemplate) )
  todoContents <- read_file( paste0(tempPath, .Platform$file.sep, todoTemplate) )

  projNoteSummaryContents <- read_file( paste0( tempPath, .Platform$file.sep, projNoteSummaryTemplate) )
  subNoteSummaryContents <- read_file( paste0( tempPath, .Platform$file.sep, subNoteSummaryTemplate) )

  projDocContents <- read_file(projectDocPath)


  #### Link Group Note and Project Doc ####

  linkFormed <- link_group_note_doc(selection, settings, headerNoteRmdPath, headerNoteContents,
                headerNoteLinkContents, projNoteLinkContents, projNoteLinkSummaryContents, todoContents,
                projNoteSummaryContents, subNoteSummaryContents, projDocContents, orgPath)

  if( linkFormed == FALSE ) {
    # return an error
    stop( paste0("  Linking Project Note Failed - link already exists."))
  }

}


#' Link Group Note to Project Doc GDT
#'
#' Single internal function to perform the linking between a project doc at GDT
#' and a group note set.
#'
link_group_note_doc <- function(selection, settings, headerNoteRmdPath, headerNoteContents,
                          headerNoteLinkContents, projNoteLinkContents,
                          projNoteLinkSummaryContents, todoContents,
                          projNoteSummaryContents, subNoteSummaryContents,
                          projDocContents, orgPath) {


  #### Set Instance Variables ####

  projectDocPath <- selection[["filePath"]] # selection is a project DOC
  # groupNotePath is the parent directory the headerNoteRmdPath (Rmd file) sits in
  groupNotePath <- normalizePath( dirname(headerNoteRmdPath))
  # headerNoteDir : get prefix from headerNoteRmdPath and append to groupNotePath
  headerNoteDir <- paste0(groupNotePath, .Platform$file.sep,
                          substr(basename(headerNoteRmdPath),
                                 1,
                                 regexpr(settings[["ProjectPrefixSep"]], basename(headerNoteRmdPath))-1 ) )

  # get list of subnotes from headerNoteDir
  subNoteRmdPaths <- list.files(headerNoteDir)
  # filter - must be .Rmd && must contain settings[["ProjectPrefixSep"]]
  subNoteRmdPaths <- subNoteRmdPaths[ endsWith(subNoteRmdPaths, ".Rmd") ]
  subNoteRmdPaths <- subNoteRmdPaths[ grepl(settings[["ProjectPrefixSep"]], subNoteRmdPaths) ]
  # form the full path
  subNoteRmdPaths <- paste0(headerNoteDir, .Platform$file.sep, subNoteRmdPaths)


  #### Fill ProjDoc GDT Templates for HEADER Note ####

  DocGDTList <- compute_doc_GDT_link(projectDocPath, headerNoteRmdPath, settings,
                                          selection[["goal"]], selection[["deliverable"]], selection[["task"]])
  # returns list of DOC TITLE, LINK, GOAL, DEL, TASK in NAMED LIST

  # replace markup in headerNoteLinkContents
  headerNoteLinkContents <- sub_note_link_params(headerNoteLinkContents, settings,
                                                 DocGDTList, "", orgPath)


  #### Fill ProjDoc GDT Templates for SUB Note ####

  DocGDTList <- compute_doc_GDT_link(projectDocPath, subNoteRmdPaths[1], settings,
                                          selection[["goal"]], selection[["deliverable"]], selection[["task"]])
  # returns list of DOC TITLE, LINK, GOAL, DEL, TASK in NAMED LIST
  # using the first path in subNoteRmdPaths - there will ALWAYS be at least one subnote!

  # replace projNoteLinkSummaryContents summary & todo headers && todo template
  projNoteLinkSummaryContents <- note_link_summ_params(projNoteLinkSummaryContents,
                                                       todoContents, settings, orgPath)

  # replace markup in projNoteLinkContents
  projNoteLinkContents <- sub_note_link_params(projNoteLinkContents, settings, DocGDTList,
                                               projNoteLinkSummaryContents, orgPath)

  # replace project note log sep
  groupNoteSummaryContents <- sub_template_param(projNoteSummaryContents,
                                                 "{{PROJECT_NOTE_LOG_SEP}}",
                                                 settings[["ProjectTaskLogSep"]], orgPath)

  # header note link - no summary
  headerNoteName <- substr(basename(headerNoteRmdPath), 1, regexpr(".Rmd", basename(headerNoteRmdPath))-1)
  headerNoteLink <- paste0(settings[["HeaderLinkFormat"]],
                           create_hyperlink( headerNoteName, headerNoteRmdPath, projectDocPath),
                           settings[["HeaderLinkFormat"]])

  groupNoteSummaryContents <- sub_template_param(groupNoteSummaryContents,
                                                 "{{PROJECT_NOTE_LINK}}",
                                                 headerNoteLink, orgPath)

  # insert blank line into SUMMARY field for header note - no summary info
  groupNoteSummaryContents <- sub_template_param(groupNoteSummaryContents,
                                                 "{{PROJECT_NOTE_SUMMARY}}",
                                                 "", orgPath)

  # replace proj note summary
  summaryContents <- extract_summary_from_link_contents(projNoteLinkContents, settings, orgPath)

  # replace proj note summary - if NoteObjectivesTodoSectionHeader is in summaryBullet, remove everything FROM THAT LINE
  #subNoteLinkSummaryContentsTrim <- projNoteLinkSummaryContents[1 : ifelse( any(grepl(settings[["NoteObjectivesTodoSectionHeader"]],
  #                                                                                    projNoteLinkSummaryContents, fixed=TRUE)),
  #                                                                          grep(settings[["NoteObjectivesTodoSectionHeader"]],
  #                                                                               projNoteLinkSummaryContents, fixed=TRUE)-1,
  #                                                                          length(projNoteLinkSummaryContents)) ]


  # subnotes summaries
  for(s in subNoteRmdPaths) {

    subNoteName <- substr(basename(s), 1, regexpr(".Rmd", basename(s))-1)
    subNoteLink <- paste0(settings[["SubNoteLinkFormat"]],
                          create_hyperlink( subNoteName, s, projectDocPath),
                          settings[["SubNoteLinkFormat"]])

    snSummaryContents <- sub_template_param(subNoteSummaryContents, "{{PROJECT_NOTE_LINK}}",
                                            subNoteLink, orgPath)

    snSummaryContents <- sub_template_param(snSummaryContents, "{{PROJECT_NOTE_SUMMARY}}",
                                            summaryContents, orgPath)

    groupNoteSummaryContents <- c(groupNoteSummaryContents, snSummaryContents)

  }

  # compute location in projDocContents to insert groupNoteSummaryContents - END OF LOG section
  logLine <- grep_line_index_from(load_param_vector(settings[["ProjectTaskLogHeader"]], orgPath),
                               projDocContents, selection[["taskLine"]], orgPath)
  taskFooterLine <- grep_line_index_from(load_param_vector(settings[["ProjectTaskFooter"]], orgPath),
                                      projDocContents, logLine, orgPath) # end of log section


  #### CHECK FOR EXISTING LINKS ####

  # Check that a link to the current projectNote in projDoc under GDT doesnt exist
  if( length( grep(headerNoteLink, projDocContents[logLine:taskFooterLine], fixed=TRUE) ) > 0 ) {
    cat("  A link to this Header Note already exists in ProjDoc GDT: ", basename(projectDocPath), "\n" )
    return(FALSE)
    #stop( paste0("  A link to this Project Note already exists in ProjDoc GDT: ", basename(projNoteRmdPath) ) )
  }

  subsExistingLinks <- list()

  for(s in subNoteRmdPaths) {
    # search for each subNoteLink - in case independent subNote link has been added
    subNoteName <- substr(basename(s), 1, regexpr(".Rmd", basename(s))-1)
    subNoteLink <- paste0(settings[["NoteLinkFormat"]],
                          create_hyperlink( subNoteName, s, projectDocPath),
                          settings[["NoteLinkFormat"]])

    snli <- grep(subNoteLink, projDocContents[logLine:taskFooterLine], fixed=TRUE)

    if( length( snli ) > 0 ) {
      cat("  A link to this SubNote already exists in ProjDoc GDT: ", basename(s), "\n" )
      cat("    Removing independent link from Project Doc\n" )
      cat("    And will skip adding Doc GDT to this note - since it already exists\n" )
      # DO NOT RETURN FALSE - just DELETE independent subnote link from ProjDoc:
      snli <- logLine + snli[1] - 1 # gives index of link in ProjDocContents
      tlsf <- grep_line_index_from_rev(load_param_vector(settings[["ProjectTaskLogSep"]], orgPath),
                                       projDocContents, snli)
      tlsl <- grep_line_index_from(load_param_vector(settings[["ProjectTaskLogSep"]], orgPath),
                                projDocContents, snli, orgPath)
      # excise independent link from ProjDocContents
      projDocContents <- c( projDocContents[1:(tlsf-1)], projDocContents[tlsl:length(projDocContents)] )
      # and recompute taskFooterLine
      taskFooterLine <- grep_line_index_from(load_param_vector(settings[["ProjectTaskFooter"]], orgPath),
                                          projDocContents, logLine, orgPath) # end of log section
      # & SKIP THIS SUBNOTE when writing projDoc GDT to file below
      subsExistingLinks[[(length(subsExistingLinks)+1)]] <- s
    }
  }


  #### Write Project Group links & summary to Project DOC ####

  # fill projDocSummaryTemplateContents with content
  projDocContents <- insert_at_indices(projDocContents, taskFooterLine, groupNoteSummaryContents)

  write_file(projDocContents, projectDocPath)

  #cat( "  Written Group Note Link to Project Doc: ", basename(projectDocPath), "\n" )
  cat( "  Written Group Note Link to Project Doc: ", basename(projectDocPath),
       " at GDT:", "\n  ", DocGDTList[["goal"]],"\n  ",
       DocGDTList[["del"]],"\n  ", DocGDTList[["task"]], "\n" )


  #### add Doc GDT Link to HEADER Note ####

  # compute location in headerNoteContents to insert the GDT Link & summary
  noteObjHeadIndex <- match_line_index( load_param_vector(settings[["NoteObjectivesHeader"]], orgPath),
                                      headerNoteContents)
  noteObjFootIndex <- grep_line_index_from( load_param_vector(settings[["NoteObjectivesFooter"]], orgPath),
                                         headerNoteContents, noteObjHeadIndex, orgPath)

  headerNoteContents <- insert_at_indices(headerNoteContents, noteObjFootIndex, headerNoteLinkContents)

  write_file(headerNoteContents, headerNoteRmdPath)

  cat( "  Written Goal Del Task to Header Note file: ", basename(headerNoteRmdPath), "\n" )


  #### add Doc GDT Link to all SUB Notes ####

  # first REMOVE all subsExistingLinks from subNoteRmdPaths
  subNoteRmdPaths <- subNoteRmdPaths[!(subNoteRmdPaths %in% subsExistingLinks)]
   # only adding GDT link to subnotes that DO NOT ALREADY HAVE IT!

  # add GDT Link to each subnote
  for(s in subNoteRmdPaths) {

    # read projNote
    subNoteContents <- read_file(s)

    # compute location in subNoteContents to insert the GDT Link & summary
    noteObjHeadIndex <- match_line_index( load_param_vector(settings[["NoteObjectivesHeader"]], orgPath),
                                        subNoteContents)
    noteObjFootIndex <- grep_line_index_from( load_param_vector(settings[["NoteObjectivesFooter"]], orgPath),
                                           subNoteContents, noteObjHeadIndex, orgPath)

    subNoteContents <- insert_at_indices(subNoteContents, noteObjFootIndex, projNoteLinkContents)

    write_file(subNoteContents, s)

    cat( "    Written Goal Del Task to Sub Note file: ", basename(s), "\n" )
  }

  return(TRUE)

}



#' Link Project Document GDT to Group Sub Note
#'
#' Add a cross-reference between a Project Doc at the Goal-Del-Task as shown in
#' `selection` with the project subnote at `subNoteRmdPath`, using the
#' provided templates.
#'
#' @param selection List containing the Goal, Del, Task selected from the Project
#' Doc, as well as other useful information - lines of Task/Del/Goal, projectDoc
#' path content of selection line.  See `cursor_selection()` or `user_selection()`.
#'
#' @param subNoteRmdPath Path to existing Project SubNote Rmd, in same Org as the
#' Project Doc `selection` object.
#'
#' @param projNoteLinkTemplate Template with structure to add the Project Doc
#' Goal/Del/Task link in the Project Note.  Includes Project Doc title, link,
#' and then links to Goal / Del / Task, plus a Summary Info section (filled
#' with the content from `taskSectionHeaderTemplate`).
#'
#' @param projNoteLinkSummaryTemplate Template with structure to add underneath the
#' Project Doc Goal/Del/Task link in the Project Note.  Includes a 'summary' section
#' and a 'todo' section by default, linked to `NoteObjectivesTodoSectionHeader`
#' in `config/settings.yml`
#'
#' @param projNoteSummaryTemplate Template with structure to add Project Note
#' summary to Project Doc under Goal/Del/Task.
#'
#' @export
link_doc_sub_note <- function(selection, subNoteRmdPath,
                           projNoteLinkTemplate="Project-Note-Link-Template.Rmd",
                           projNoteLinkSummaryTemplate="Project-Note-Link-Summary-Template.Rmd",
                           todoTemplate="Todo-Template.Rmd",
                           projNoteSummaryTemplate="Project-Note-Summary-Template.Rmd" ) {


  cat( "\nprojectmanagr::link_doc_sub_note():\n" )


  #### Set Instance Variables ####

  projectDocPath <- selection[["filePath"]]
  # projectNotePath is the parent directory the subNoteRmdPath (Rmd file) sits in
  projectNotePath <- normalizePath( dirname(subNoteRmdPath))

  # Check projectNotePath is a sub-dir in a Programme DIR, which is a sub-dir to the root of an ORGANISATION:
  orgPath <- dirname( dirname(projectNotePath) ) # this should be the orgPath!
  orgPath <- find_org_directory(orgPath)

  if(orgPath == "" ) { # only if orgPath not identified
    stop( paste0("  projectNotePath is not in a sub-dir of a PROGRAMME Directory: ", projectNotePath) )
  }
  # now, orgPath should be the root dir of the organisation

  # get config templates settings yml
  confPath <- get_config_dir(orgPath)
  tempPath <- get_template_dir(orgPath)
  settings <- get_settings_yml(orgPath)


  #### CHECK FOR ERRORS IN INPUT ####

  # confirm projectNotePath is a valid path - contains "~_"
  if( !grepl(settings[["ProjectPrefixSep"]], subNoteRmdPath) ) {
    stop( paste0("  projectNotePath is not valid - no ProjectPrefixSep: ", basename(subNoteRmdPath) ) )
  }


  #### Read Rmds ####

  projNoteRmdContents <- read_file(subNoteRmdPath)

  projNoteLinkContents <- read_file( paste0( tempPath, .Platform$file.sep, projNoteLinkTemplate) )
  projNoteLinkSummaryContents <- read_file( paste0(tempPath, .Platform$file.sep, projNoteLinkSummaryTemplate) )
  todoContents <- read_file( paste0(tempPath, .Platform$file.sep, todoTemplate) )

  projNoteSummaryContents <- read_file( paste0( tempPath, .Platform$file.sep, projNoteSummaryTemplate) )

  projDocContents <- read_file(projectDocPath)


  #### Link Project Note and Project Doc ####

  link_project_note_doc(selection, settings, subNoteRmdPath, projNoteRmdContents, projNoteLinkContents,
                  projNoteLinkSummaryContents, todoContents, projNoteSummaryContents, projDocContents, orgPath)

}


#' Link Sub Note to Project Doc GDT
#'
#' Single internal function to perform the linking between all project doc GDT
#' links the `linkNoteRmdContents` and the `subNoteRmdPath`.
#'
#' Assumes these are both in the same project note group -
#' `linkNoteRmdContents` should be either the header note of the group, or a
#' subnote, and `subNoteRmdPath` should be a new subnote in the project note
#' group with no links. This function is used exclusively for the addition of
#' new sub-notes to a project note group.
#'
link_sub_note_doc <- function(selection, settings, subNoteRmdPath, subNoteContents,
            headerNoteRmdPath, headerNoteRmdContents, projNoteLinkContents,
            projNoteLinkSummaryContents, todoContents, projNoteSummaryContents,
            subNoteSummaryContents, linkNoteRmdPath, linkNoteRmdContents, orgPath) {


  #### Set Instance Variables ####

  # subNotePath is the parent directory the subNoteRmdPath (Rmd file) sits in
  subNotePath <- normalizePath( dirname(subNoteRmdPath))
  subNoteFileName <- basename(subNoteRmdPath)

  #### Extract ProjDocGDTs from linkNoteRmdContents ####

  # extract each project Doc + GDT from each objective
  DocGDTsList <- extract_note_obj_doc_link_GDT_summ(linkNoteRmdContents, linkNoteRmdPath,
                                                    settings, orgPath)


  #### For each DocGDT ####

  # compute the noteObjective Header index in subNoteContents
  noteObjHeadIndex <- match_line_index( load_param_vector(settings[["NoteObjectivesHeader"]], orgPath),
                                      subNoteContents)

  # replace projNoteLinkSummaryContents summary headers
  projNoteLinkSummaryContents <- note_link_summ_params(projNoteLinkSummaryContents,
                                                       todoContents, settings, orgPath)

  for( dGDT in DocGDTsList ) {


    ##### Fill ProjDoc GDT Templates for SUB Note #####

    DocGDTList <- compute_doc_GDT_link(dGDT[["projectDocFilePath"]], subNoteRmdPath, settings,
                                            dGDT[["goal"]], dGDT[["deliverable"]], dGDT[["task"]])
    # returns list of DOC TITLE, LINK, GOAL, DEL, TASK in NAMED LIST
    # using the first path in subNoteRmdPaths - there will ALWAYS be at least one subnote!

    # replace markup in projNoteLinkContents
    subNoteLinkContents <- sub_note_link_params(projNoteLinkContents, settings, DocGDTList,
                                                 projNoteLinkSummaryContents, orgPath)


    #### add Doc GDT Link to SUB Note ####

    # compute location in subNoteContents to insert the GDT Link & summary
    noteObjFootIndex <- grep_line_index_from( load_param_vector(settings[["NoteObjectivesFooter"]], orgPath),
                                           subNoteContents, noteObjHeadIndex, orgPath)

    subNoteContents <- insert_at_indices(subNoteContents, noteObjFootIndex, subNoteLinkContents)

  }

  write_file(subNoteContents, subNoteRmdPath)

  cat( "    Written GDTs to Sub Note file: ", basename(subNoteRmdPath), "\n" )

  # replace project note log sep
  projNoteSummaryContents <- sub_template_param(projNoteSummaryContents, "{{PROJECT_NOTE_LOG_SEP}}",
                                                settings[["ProjectTaskLogSep"]], orgPath)

  # replace proj note summary
  summaryContents <- extract_summary_from_link_contents(subNoteLinkContents, settings, orgPath)

  # # replace proj note summary - if NoteObjectivesTodoSectionHeader is in summaryBullet, remove everything FROM THAT LINE
  # projNoteLinkSummaryContentsTrim <- projNoteLinkSummaryContents[1 : ifelse( any(grepl(settings[["NoteObjectivesTodoSectionHeader"]],
  #                                                                                      projNoteLinkSummaryContents, fixed=TRUE)),
  #                                                                            grep(settings[["NoteObjectivesTodoSectionHeader"]],
  #                                                                                 projNoteLinkSummaryContents, fixed=TRUE)-1,
  #                                                                            length(projNoteLinkSummaryContents)) ]

  # replace in projNoteSummaryContents - in case any links are INDIVIDUAL (not group note)
   # this will be added at end of GDT section as individual project note link
  projNoteSummaryContents <- sub_template_param(projNoteSummaryContents, "{{PROJECT_NOTE_SUMMARY}}",
                                                summaryContents, orgPath)

  # replace in subNoteSummaryContents - for links that are GROUP NOTES
   # this will be added at end of the group note link in GDT section as subnote link
  subNoteSummaryContents <- sub_template_param(subNoteSummaryContents, "{{PROJECT_NOTE_SUMMARY}}",
                                               summaryContents, orgPath)

  for( dGDT in DocGDTsList ) {

    ##### Write Sub Note to each DocGDT #####

    # read projDoc
    projectDocPath <- dGDT[["projectDocFilePath"]]
    projDocContents <- read_file(projectDocPath)

    # find the GDT vector
    goalLine <- grep_line_index(dGDT[["goal"]], projDocContents, orgPath)
    delLine <- grep_line_index_from(dGDT[["deliverable"]], projDocContents, goalLine, orgPath)
    taskLine <- grep_line_index_from(dGDT[["task"]], projDocContents, delLine, orgPath)
    logLine <- grep_line_index_from(load_param_vector(settings[["ProjectTaskLogHeader"]], orgPath),
                                 projDocContents, taskLine, orgPath)
    taskFooterLine <- grep_line_index_from(load_param_vector(settings[["ProjectTaskFooter"]], orgPath),
                                        projDocContents, logLine, orgPath) # end of log section

    # determine if link exists as a group note link or as a single project note
    # find index of headernote link then the next ProjectTaskLogSep
    headerNoteName <- substr(basename(headerNoteRmdPath), 1, regexpr(".Rmd", basename(headerNoteRmdPath))-1)
    headerNoteLink <- paste0(settings[["HeaderLinkFormat"]],
                             create_hyperlink( headerNoteName, headerNoteRmdPath, projectDocPath),
                             settings[["HeaderLinkFormat"]])
    headLine <- grep(headerNoteLink, projDocContents[logLine:taskFooterLine], fixed=TRUE)

    if( length(headLine) > 0) {
      # if headLine identified, the link to this GDT must be as a group note under the header
      # get the headLine in full projDoc vector
      headLine <- grep_line_index_from(headerNoteLink, projDocContents, logLine, orgPath)
      # then the sepLine from this point
      sepLine <- grep_line_index_from(load_param_vector(settings[["ProjectTaskLogSep"]], orgPath),
                                   projDocContents, headLine, orgPath)
      # insert the new subnote at end of group note link
      insertionLine <- sepLine

      # replace sub note link
      projNoteName <- substr(basename(subNoteRmdPath), 1, regexpr(".Rmd", basename(subNoteRmdPath))-1)
      projNoteLink <- paste0(settings[["SubNoteLinkFormat"]],
                             create_hyperlink( projNoteName, subNoteRmdPath, projectDocPath),
                             settings[["SubNoteLinkFormat"]])
      snSummaryContents <- sub_template_param(subNoteSummaryContents, "{{PROJECT_NOTE_LINK}}",
                                              projNoteLink, orgPath)

      projDocContents <- insert_at_indices(projDocContents, insertionLine, snSummaryContents)

      write_file(projDocContents, projectDocPath)

      cat( "  Written Sub Note Link to Project Doc: ", basename(projectDocPath),
           " at GDT:", "\n  ", dGDT[["goal"]],"\n  ", dGDT[["deliverable"]],"\n  ", dGDT[["task"]], "\n" )

    } else {
      # if headLine not identified, the link to this GDT must be from a subnote individually linked
      # insert new subnote at end of the GDT section
      insertionLine <- taskFooterLine

      # replace sub note link
      projNoteName <- substr(basename(subNoteRmdPath), 1, regexpr(".Rmd", basename(subNoteRmdPath))-1)
      projNoteLink <- paste0(settings[["NoteLinkFormat"]],
                             create_hyperlink( projNoteName, subNoteRmdPath, projectDocPath),
                             settings[["NoteLinkFormat"]])
      snSummaryContents <- sub_template_param(projNoteSummaryContents, "{{PROJECT_NOTE_LINK}}",
                                              projNoteLink, orgPath)

      projDocContents <- insert_at_indices(projDocContents, insertionLine, snSummaryContents)

      write_file(projDocContents, projectDocPath)

      cat( "  Written Project Note Link to Project Doc: ", basename(projectDocPath),
           " at GDT:", "\n  ", dGDT[["goal"]],"\n  ", dGDT[["deliverable"]],"\n  ", dGDT[["task"]], "\n" )

    }
  } # end for
}

