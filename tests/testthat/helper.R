
#' create test org
#'
#' Create & remove test fixtures in reproducible temp dir using `withr::defer()`
#'
#' @param orgName name of org
#'
#' @param orgParentPath Temp dir to generate org
#'
#' @param env parent.frame for withr::deferred_run()
#'
local_create_org <- function(orgName, orgutime, orgParentPath,
                             env = parent.frame() ) {

  # record current state
  olddir <- getwd()

  # create local yaml file in orgParentPath to point to
  settingsYamlPath <- create_settings_yaml(orgParentPath)

  # create project org
  create_project_org(orgParentPath, orgName,
                     settingsYamlPath=settingsYamlPath, utime=orgutime)

  # define output variable
  orgDir <- fs::path(orgParentPath, orgName)

  # defer removal of orgDir
  withr::defer(fs::dir_delete(orgDir), envir = env)

  # defer removal of settings
  withr::defer(fs::file_delete(settingsYamlPath), envir = env)

  # return the org directory
  orgDir

}


create_settings_yaml <- function(orgParentPath) {

  projectmanagrPath <- find.package("projectmanagr", lib.loc = .libPaths())

  settingsYamlFile <- paste0( projectmanagrPath,
                              .Platform$file.sep, "config",
                              .Platform$file.sep, "settings.yml")

  settings <- yaml::read_yaml(settingsYamlFile)

  # modify separators containing '~' character- as not allowed in filenames tested in R packages for CRAN submission
  settings$ProjectPrefixSep <- "_--_" # from ~_
  settings$ProjectIndexSep <- "___" # from ~

  yaml::write_yaml(settings, fs::path(orgParentPath, "settings.yml"))

  fs::path_real(fs::path(orgParentPath, "settings.yml"))

}

#' create test programme
#'
#' Create & remove test fixtures in reproducible temp dir using `withr::defer()`
#'
#' @param progName name of programme
#'
#' @param orgDir dir to org
#'
#' @param env parent.frame for withr::deferred_run()
#'
local_create_prog <- function(progName, orgDir, progctime,
                             env = parent.frame() ) {

  # record current state
  olddir <- getwd()

  # create project programme
  create_programme(progName, orgDir, ctime=progctime)
  progDir <- fs::path(orgDir, progName)

  withr::defer(fs::dir_delete(progDir), envir = env)

  # return the prog directory
  progDir

}

#' create test project
#'
#' Create & remove test fixtures in reproducible temp dir using `withr::defer()`
#'
#' @param projectPrefix prefix of project
#'
#' @param projectName name of project
#'
#' @param progDir dir to programme
#'
#' @param env parent.frame for withr::deferred_run()
#'
local_create_project <- function(projectPrefix, projectName, progDir,
                              env = parent.frame() ) {

  # record current state
  olddir <- getwd()

  # create project doc
  create_project_doc(projectPrefix, projectName, progDir)

  # create paths to Rmd & dir
  projectRmd <- fs::path(progDir, paste0(projectPrefix, "_--_", projectName, ".Rmd") )
  projectDir <- fs::path(progDir, projectPrefix)

  # ensure Rmd & Dir are deleted when out of context
  withr::defer(fs::file_delete(projectRmd), envir = env)
  withr::defer(fs::dir_delete(projectDir), envir = env)

  # return the project Rmd
  projectRmd

}



local_modify_project_doc_gdt_titles <- function(settingsYml, projectRmd,
                                                GOAL_TITLE=" EXAMPLE GOAL TITLE",
                                                DEL_TITLE=" EXAMPLE DEL TITLE",
                                                TASK_TITLE=" EXAMPLE TASK TITLE") {

  # get the goal del task headers from settingsYml
  settingsYmlLines <- readLines(settingsYml)

  goalHeader <- settingsYmlLines[grep("ProjectGoalHeader:",settingsYmlLines)]
  goalHeader <- substring(goalHeader,
                          gregexpr('ProjectGoalHeader: ', goalHeader, fixed=TRUE)[[1]][1]+nchar('ProjectGoalHeader: ')+1,
                          nchar(goalHeader)-1)

  delHeader <- settingsYmlLines[grep("ProjectDeliverableHeader:",settingsYmlLines)]
  delHeader <- substring(delHeader,
                         gregexpr('ProjectDeliverableHeader: ', delHeader, fixed=TRUE)[[1]][1]+nchar('ProjectDeliverableHeader: ')+1,
                         nchar(delHeader)-1)

  taskHeader <- settingsYmlLines[grep("ProjectTaskHeader:",settingsYmlLines)]
  taskHeader <- substring(taskHeader,
                          gregexpr('ProjectTaskHeader: ', taskHeader, fixed=TRUE)[[1]][1]+nchar('ProjectTaskHeader: ')+1,
                          nchar(taskHeader)-1)

  # grep all GOAL DEL TASK lines from projDoc
  projectRmdLines <- readLines(projectRmd)

  goalLines <- grep(goalHeader, projectRmdLines)
  delLines <- grep(delHeader, projectRmdLines)
  taskLines <- grep(taskHeader, projectRmdLines)

  # modify LAST Goal Del Task in ProjectDoc to a unique and recognisable name in Doc
  projectRmdLines[goalLines[length(goalLines)]] <- paste0(goalHeader, GOAL_TITLE)
  projectRmdLines[delLines[length(delLines)]] <- paste0(delHeader, DEL_TITLE)
  projectRmdLines[taskLines[length(taskLines)]] <- paste0(taskHeader, TASK_TITLE)

  # write to ProjectRmd
  writeLines(projectRmdLines, projectRmd)

  # return taskLine - needed for selection of project Doc GDT!
  taskLines[length(taskLines)]

}




local_create_project_note_simple <- function(projectNoteName, projectNotePath,
                                             projectDocPath, taskLine,
                                             noteIndex="___001", env = parent.frame() ) {

  # record current state
  olddir <- getwd()

  # generate selection object via projectmanagr function
  selection <- user_selection(projectDocPath, taskLine)

  # create project note
  create_project_note(projectNoteName, projectNotePath, selection)

  # create paths to Rmd & dir
  projectNoteRmd <- fs::path(projectNotePath, paste0(basename(projectNotePath), noteIndex,"_--_", projectNoteName, ".Rmd") )
  projectNoteDir <- fs::path(projectNotePath, paste0(basename(projectNotePath), noteIndex) )

  # ensure Rmd & Dir are deleted when out of context
  withr::defer(fs::file_delete(projectNoteRmd), envir = env)
  withr::defer(fs::dir_delete(projectNoteDir), envir = env)

  # return the project Rmd
  projectNoteRmd


}



local_create_project_note_group <- function(groupNoteName, groupNotePath,
                                            projectDocPath, taskLine,
                                            subNoteName,
                                            env = parent.frame()) {

  # record current state
  olddir <- getwd()

  # generate selection object via projectmanagr function
  selection <- user_selection(projectDocPath, taskLine)

  # create group note
  create_group_note(groupNoteName, groupNotePath, selection, subNoteName)

  # create paths to Rmd & dir
  groupNoteRmd <- fs::path(groupNotePath, paste0(basename(groupNotePath), "___001-00", "_--_", groupNoteName, ".Rmd") )
  groupNoteDir <- fs::path(groupNotePath, paste0(basename(groupNotePath), "___001-00") )

  # ensure Rmd & Dir are deleted when out of context
  withr::defer(fs::file_delete(groupNoteRmd), envir = env)
  withr::defer(fs::dir_delete(groupNoteDir), envir = env)

  # return the group Rmd
  groupNoteRmd


}



local_get_project_doc_file_link_line <- function(projectDocRmd, groupNoteRmd, settings) {

  # grep all GOAL DEL TASK lines from projDoc
  projectDocRmdLines <- readLines(projectDocRmd)

  headerLinkLine <- grep( basename(groupNoteRmd), projectDocRmdLines, fixed=TRUE)

  headerLinkLine

}


local_create_project_note_sub <- function(subNoteName, subNotePath,
                                          projectDocRmd, headerLinkLine,
                                             env = parent.frame()) {

  # record current state
  olddir <- getwd()

  # generate selection object via projectmanagr function
  selection <- user_selection(projectDocRmd, headerLinkLine)

  # create project note
  create_sub_note(subNoteName, subNotePath, selection)

  # other ARGS
  subNoteTitle=""
  subNoteTemplate="Project-Sub-Note-Template.Rmd"
  headerNoteContentLinkTemplate="Project-Header-Note-Content-Link-Template.Rmd"
  subNoteContentLinkTemplate="Project-Sub-Note-Content-Link-Template.Rmd"
  projNoteLinkTemplate="Project-Note-Link-Template.Rmd"
  projNoteLinkSummaryTemplate="Project-Note-Link-Summary-Template.Rmd"
  todoTemplate="Todo-Template.Rmd"
  projNoteSummaryTemplate="Project-Note-Summary-Template.Rmd"
  subNoteSummaryTemplate="Project-Sub-Note-Summary-Template.Rmd"

  # create paths to Rmd & dir - 2ND SUBNOTE!
  subNoteRmd <- fs::path(subNotePath, paste0(basename(dirname(subNotePath)), "___001-002_--_", subNoteName, ".Rmd") )
  subNoteDir <- fs::path(subNotePath, paste0(basename(dirname(subNotePath)), "___001-002") )

  # ensure Rmd & Dir are deleted when out of context
  withr::defer(fs::file_delete(subNoteRmd), envir = env)
  withr::defer(fs::dir_delete(subNoteDir), envir = env)

  # return the project Rmd
  subNoteRmd


}




local_create_content <- function(contentName, contentDescription, contentSourcePath,
                                 projectNoteRmd, noteLine, contentTitle, env = parent.frame() ) {

  # record current state
  olddir <- getwd()

  # generate selection object via projectmanagr function
  selection <- user_selection(projectNoteRmd, noteLine)

  # other ARGS
  contentDeclarationTemplate="Content-Declaration-Template.Rmd"
  contentSourceTemplate="Content-Source-Template.Rmd"

  # create project note
  create_content(selection, contentName, contentDescription, contentSourcePath, contentTitle)

  # create paths to Rmd & dir
  contentDir <- fs::path(contentSourcePath, contentName)
  # contentRmd is in contentDir and basename is also contentName
  contentRmd <- fs::path(contentDir, paste0(contentName, ".Rmd"))

  # ensure Rmd & Dir are deleted when out of context
  withr::defer(fs::dir_delete(contentDir), envir = env)
  #withr::defer(fs::file_delete(contentRmd), envir = env)

  # return the content Rmd
  contentRmd


}



local_create_journal <- function(date, organisationPath, env = parent.frame() ) {

  # record current state
  olddir <- getwd()

  # other ARGS
  journalFileNameTemplate="{{YYYY}}-{{MM}}-{{DD}}_{{ORGNAME}}"
  journalTemplate="Weekly-Work-Journal-Template.Rmd"
  openJournal = FALSE # do not open when testing!

  # create project note
  create_weekly_journal(date, organisationPath, journalFileNameTemplate, journalTemplate, openJournal)

  # create paths to Rmd & dir
  journalDir <- fs::path(organisationPath, "weekly-journal")
  # journalRmd is in journalDir and basename is also contentName
  journalRmd <- fs::path(journalDir, paste0(date, "_", basename(organisationPath), ".Rmd"))

  # ensure Rmd & Dir are deleted when out of context
  withr::defer(fs::dir_delete(journalDir), envir = env)
  #withr::defer(fs::file_delete(journalRmd), envir = env)

  # return the journal Rmd
  journalRmd


}



local_rename_project_note_simple <- function(projectNotePath, newProjectNoteName,
                                             newProjectNoteTitle,replaceLinksFileExtensions,
                                             env = parent.frame() ) {


  # record current state
  olddir <- getwd()

  # rename project note
  rename_project_note(projectNotePath, newProjectNoteName,
                      newProjectNoteTitle,replaceLinksFileExtensions)

  # create paths to Rmd & dir
  projectNoteRmdRename <- fs::path(dirname(projectNotePath),
                                   paste0(basename(dirname(projectNotePath)),
                                          "___001_--_", newProjectNoteName, ".Rmd") )
  projectNoteDirRename <- fs::path(dirname(projectNotePath),
                                   paste0(basename(dirname(projectNotePath)),
                                          "___001") )

  # ensure Rmd & Dir are deleted when out of context
  withr::defer(fs::file_delete(projectNoteRmdRename), envir = env)
  withr::defer(fs::dir_delete(projectNoteDirRename), envir = env)

  # return the project Rmd
  projectNoteRmdRename

}




local_rename_project_doc <- function(projectDocPath, projectDocPrefix,
                                     newProjectDocName, newProjectDocTitle,
                                     replaceLinksFileExtensions,
                                     env = parent.frame() ) {


  # record current state
  olddir <- getwd()

  # rename project Doc
  rename_project_doc(projectDocPath, newProjectDocName,
                      newProjectDocTitle,replaceLinksFileExtensions)

  # create paths to Rmd & dir
  projectDocRmdRename <- fs::path(dirname(projectDocPath),
                                   paste0(projectDocPrefix,
                                          "_--_", newProjectDocName, ".Rmd") )
  projectDocDirRename <- fs::path(dirname(projectDocPath), projectDocPrefix )

  # ensure Rmd & Dir are deleted when out of context
  withr::defer(fs::file_delete(projectDocRmdRename), envir = env)
  withr::defer(fs::dir_delete(projectDocDirRename), envir = env)

  # return the project Doc Rmd
  projectDocRmdRename

}



