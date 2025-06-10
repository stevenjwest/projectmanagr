
#' Create temp dir Rsess
#'
#' Create Rsess/ in the temporary directory - on linux: /tmp/
#'
#' @return tmpdir full path
create_tmpdir_rsess <- function(env = parent.frame()) {
  tmpdir <- fs::path(dirname(tempdir()), "Rsess")
  #tmpdir <- fs::path('/', 'tmp', 'Rsess')
  # set a fixed tmpdir path!
  fs::dir_create(tmpdir)
  withr::defer(fs::dir_delete(tmpdir), envir = env) # delete once tests finish
  tmpdir # return
}


#' create test org
#'
#' Create & remove test fixtures in reproducible temp dir using `withr::defer()`
#'
#' @param orgName name of org
#'
#' @param authorValue name of author
#'
#' @param orgParentPath Temp dir to generate org
#'
#' @param syp Serttings Yaml Path - path to settings.yml to read, if blank will
#' read the projectmanagr package settings.yml. Used to insert custom settings
#' data.
#'
#' @param env parent.frame for withr::deferred_run()
#'
local_create_org <- function(orgName, authorValue, orgParentPath, syp,
                             env = parent.frame() ) {

  # record current state
  olddir <- getwd()

  if( syp != "" ) {

    # create local yaml file in orgParentPath to point to
    settingsYamlPath <- create_test_settings_yaml(syp)
     # also modify some key parameters:
     # ProjectPrefixSep : only - or _ used
     # ProjectIndexSep : only - or _ used

    # create project org
    create_project_org(orgParentPath, orgName, authorValue=authorValue,
                       settingsYamlPath=settingsYamlPath)

  } else {
    # create project org
    create_project_org(orgParentPath, orgName, authorValue=authorValue,
                       settingsYamlPath="")

  }

  # define output variable
  orgDir <- fs::path(orgParentPath, orgName)

  # defer removal of orgDir
  withr::defer(fs::dir_delete(orgDir), envir = env)

  if( syp != "" ) {
    # defer removal of settings
    withr::defer(fs::file_delete(settingsYamlPath), envir = env)
  }

  # return the org directory
  orgDir

}


create_test_settings_yaml <- function(orgParentPath) {

  projectmanagrPath <- find.package("projectmanagr", lib.loc = .libPaths())

  settingsYamlFile <- paste0( projectmanagrPath,
                              .Platform$file.sep, "config",
                              .Platform$file.sep, "settings.yml")

  settings <- yaml::yaml.load(yaml::read_yaml(settingsYamlFile))

  # modify separators containing '~' character
   # as not allowed in filenames tested in R packages for CRAN submission
  settings$ProjectPrefixSep <- "_--_" # from ~_
  settings$ProjectIndexSep <- "___" # from ~

  yaml::write_yaml(yaml::as.yaml(settings),
                   fs::path(orgParentPath, "settings.yml"))

  fs::path_real(fs::path(orgParentPath, "settings.yml"))

}


modify_test_settings_yaml <- function(orgPath) {

  settingsYamlFile <- fs::path(orgPath, ".config", "settings.yml")

  settings <- yaml::yaml.load( yaml::read_yaml( settingsYamlFile ) )

  # modify separators containing '~' character
  # as not allowed in filenames tested in R packages for CRAN submission
  settings$ProjectPrefixSep <- "_--_" # from ~_
  settings$ProjectIndexSep <- "___" # from ~

  yaml::write_yaml(yaml::as.yaml(settings), settingsYamlFile)

  fs::path_real(settingsYamlFile)

}

#' create test programme
#'
#' Create & remove test fixtures in reproducible temp dir using `withr::defer()`
#'
#' @param progName name of programme
#'
#' @param orgDir dir to org
#'
#' @param authorValue name of author
#'
#' @param env parent.frame for withr::deferred_run()
#'
local_create_prog <- function(progName, orgDir, authorValue,
                             env = parent.frame() ) {

  # record current state
  olddir <- getwd()

  # function args: for interactive testing
  programmeName <- progName
  organisationPath <- orgDir
  authorValue <- authorValue
  programmeTitle <- ""
  progTemplate <- "Programme-Template.Rmd"
  progSummaryTemplate <- "Programme-Summary-Template.Rmd"
  # create project programme
  create_programme(progName, orgDir, authorValue)

  progDir <- fs::path(orgDir, progName)

  withr::defer(fs::dir_delete(progDir), envir = env)

  # return the prog directory
  progDir

}


#' create test programme
#'
#' Create & remove test fixtures in reproducible temp dir using `withr::defer()`
#'
#' @param sectionName Character. The name of the new programme section. Must not
#'   contain spaces.
#' @param sectionParentPath Character. The file path to an existing organisation
#'   directory, within a programme.
#'
#' @param authorValue name of author
#'
#' @param env parent.frame for withr::deferred_run()
#'
local_create_prog_section <- function(sectionName, sectionParentPath, authorValue,
                                      env = parent.frame() ) {

  # record current state
  olddir <- getwd()

  # function parameters for interactive testing
  sectionName <- sectionName
  sectionParentPath <- sectionParentPath
  authorValue <- authorValue
  sectionTitle <- ""
  sectTemplate <- "Programme-Section-Template.Rmd"
  sectSummaryTemplate <- "Programme-Section-Summary-Template.Rmd"
  # create programme section
  create_programme_section(sectionName, sectionParentPath, authorValue)

  # create paths to Rmd & dir
  sectDir <- fs::path(sectionParentPath, sectionName)

  withr::defer(fs::dir_delete(sectDir), envir = env)

  # return the section directory
  sectDir

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
local_create_project <- function(projectDocPrefix, projectDocName, projectParentPath,
                                 authorValue, env = parent.frame() ) {

  # record current state
  olddir <- getwd()

  # create project doc
  # function parameters for interactive testing
  projectPrefix <- projectDocPrefix
  projectName <- projectDocName
  projectParentPath <- projectParentPath
  projectTitle=""
  projDocTemplate="Project-Doc-Template.Rmd"
  projDocSummaryTemplate="Project-Doc-Summary-Template.Rmd"
  # create project doc
  create_project_doc(projectPrefix, projectName, projectParentPath, authorValue)

  # create paths to Rmd & dir
  renamedProjectRmd <- fs::path(projectParentPath, paste0(projectPrefix, "_--_", projectName, ".Rmd") )
  projectRmd <- fs::path(projectParentPath, paste0(projectPrefix, "_--_", projectName, ".Rmd") )

  projectDir <- fs::path(projectParentPath, projectPrefix)

  # ensure Rmd & Dir are deleted when out of context
  withr::defer(fs::file_delete(renamedProjectRmd), envir = env)
  withr::defer(fs::dir_delete(projectDir), envir = env)

  # return the current project Rmd
  projectRmd

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
local_create_project_rename <- function(projectPrefix, projectName, progDir,
                                        authorValue, renamedDocName,
                                        env = parent.frame() ) {

  # record current state
  olddir <- getwd()

  # create project doc
  create_project_doc(projectPrefix, projectName, progDir, authorValue)

  # create paths to Rmd & dir - use new renamed Rmd file name for deferred deletion
  renamedProjectRmd <- fs::path(progDir, paste0(projectPrefix, "_--_", renamedDocName, ".Rmd") )
  projectRmd <- fs::path(progDir, paste0(projectPrefix, "_--_", projectName, ".Rmd") )

  projectDir <- fs::path(progDir, projectPrefix)

  # ensure Rmd & Dir are deleted when out of context
  # use new renamed Rmd file name for deferred deletion
  withr::defer(fs::file_delete(renamedProjectRmd), envir = env)
  withr::defer(fs::dir_delete(projectDir), envir = env)

  # return the current project Rmd
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
                                             projectDocRmd, taskLine, authorValue,
                                             noteIndex="___001",
                                             env = parent.frame() ) {

  # interactive testing
  #noteIndex="___001"

  # record current state
  olddir <- getwd()

  # generate selection object via projectmanagr function
  selection <- user_selection(projectDocRmd, taskLine)

  ### create project note ###
  create_project_note(projectNoteName, projectNotePath, selection, authorValue)

  # create paths to Rmd & dir
  projectNoteRmd <- fs::path(projectNotePath, paste0(basename(projectNotePath), noteIndex,"_--_", projectNoteName, ".Rmd") )
  renameNoteRmd <- fs::path(projectNotePath, paste0(basename(projectNotePath), noteIndex,"_--_", projectNoteName, ".Rmd") )

  projectNoteDir <- fs::path(projectNotePath, paste0(basename(projectNotePath), noteIndex) )

  # ensure Rmd & Dir are deleted when out of context
  withr::defer(fs::file_delete(renameNoteRmd), envir = env)
  withr::defer(fs::dir_delete(projectNoteDir), envir = env)

  # return the project Rmd
  projectNoteRmd


}


local_create_project_note_simple_rename <- function(projectNoteName, projectNotePath,
                                             projectDocRmd, taskLine,  authorValue,
                                             noteIndex="___001", renameNoteName="",
                                             env = parent.frame() ) {

  # interactive testing - set projectDocRmd
  #projectDocRmd <- projectDocRmd
  #noteIndex="___001"

  # record current state
  olddir <- getwd()

  # generate selection object via projectmanagr function
  selection <- user_selection(projectDocRmd, taskLine)

  # create project note
  create_project_note(projectNoteName, projectNotePath, selection,  authorValue)

  # create paths to Rmd & dir - ability to defer deletion of rename testing
  projectNoteRmd <- fs::path(projectNotePath, paste0(basename(projectNotePath), noteIndex,"_--_", projectNoteName, ".Rmd") )
  renameNoteRmd <- fs::path(projectNotePath, paste0(basename(projectNotePath), noteIndex,"_--_", renameNoteName, ".Rmd") )

  projectNoteDir <- fs::path(projectNotePath, paste0(basename(projectNotePath), noteIndex) )

  # ensure Rmd & Dir are deleted when out of context
  # use new renamed Rmd file name for deferred deletion
  withr::defer(fs::file_delete(renameNoteRmd), envir = env)
  withr::defer(fs::dir_delete(projectNoteDir), envir = env)

  # return the current project Rmd
  projectNoteRmd


}



local_create_project_note_group <- function(groupNoteName, groupNotePath,
                                            projectDocRmd, taskLine,
                                            subNoteName, authorValue,
                                            env = parent.frame()) {

  # record current state
  olddir <- getwd()

  # generate selection object via projectmanagr function
  selection <- user_selection(projectDocRmd, taskLine)

  # create group note
  create_group_note(groupNoteName, groupNotePath, selection,
                    subNoteName, authorValue)

  # create paths to Rmd & dir
  groupNoteRmd <- fs::path(groupNotePath, paste0(basename(groupNotePath), "___001-00", "_--_", groupNoteName, ".Rmd") )
  groupNoteDir <- fs::path(groupNotePath, paste0(basename(groupNotePath), "___001-00") )

  # ensure Rmd & Dir are deleted when out of context
  withr::defer(fs::file_delete(groupNoteRmd), envir = env)
  withr::defer(fs::dir_delete(groupNoteDir), envir = env)

  # return the group Rmd
  groupNoteRmd


}


local_create_project_note_group2 <- function(groupNoteName, groupNotePath,
                                            projectDocRmd, taskLine,
                                            subNoteName,  authorValue,
                                            env = parent.frame()) {

  # record current state
  olddir <- getwd()

  # generate selection object via projectmanagr function
  selection <- user_selection(projectDocRmd, taskLine)

  # create group note
  create_group_note(groupNoteName, groupNotePath, selection, subNoteName,  authorValue)

  # create paths to Rmd & dir
  groupNoteRmd <- fs::path(groupNotePath, paste0(basename(groupNotePath), "___002-00", "_--_", groupNoteName, ".Rmd") )
  groupNoteDir <- fs::path(groupNotePath, paste0(basename(groupNotePath), "___002-00") )

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
                                          authorValue,
                                          env = parent.frame()) {

  # interactive testing
  #subNoteName <- subNoteName2

  # record current state
  olddir <- getwd()

  # generate selection object via projectmanagr function
  selection <- user_selection(projectDocRmd, headerLinkLine)

  # create project note
  create_sub_note(subNoteName, subNotePath, selection, authorValue)

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

local_create_project_note_sub2 <- function(subNoteName, subNotePath,
                                          projectDocRmd, headerLinkLine,
                                          authorValue,
                                          env = parent.frame()) {

  # interactive testing
  #subNoteName <- subNoteName2

  # record current state
  olddir <- getwd()

  # generate selection object via projectmanagr function
  selection <- user_selection(projectDocRmd, headerLinkLine)

  # create project note
  create_sub_note(subNoteName, subNotePath, selection,  authorValue)

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
  subNoteRmd <- fs::path(subNotePath, paste0(basename(dirname(subNotePath)), "___002-002_--_", subNoteName, ".Rmd") )
  subNoteDir <- fs::path(subNotePath, paste0(basename(dirname(subNotePath)), "___002-002") )

  # ensure Rmd & Dir are deleted when out of context
  withr::defer(fs::file_delete(subNoteRmd), envir = env)
  withr::defer(fs::dir_delete(subNoteDir), envir = env)

  # return the project Rmd
  subNoteRmd


}


local_create_project_note_sub_head_sel <- function(subNoteName, subNotePath,
                                          headNotePath, headLine, authorValue,
                                          env = parent.frame()) {

  # record current state
  olddir <- getwd()

  # generate selection object via projectmanagr function
  selection <- user_selection(headNotePath, headLine)

  # create project note
  create_sub_note(subNoteName, subNotePath, selection, authorValue)

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
  subNoteRmd <- fs::path(subNotePath, paste0(basename(dirname(subNotePath)), "___001-003_--_", subNoteName, ".Rmd") )
  subNoteDir <- fs::path(subNotePath, paste0(basename(dirname(subNotePath)), "___001-003") )

  # ensure Rmd & Dir are deleted when out of context
  withr::defer(fs::file_delete(subNoteRmd), envir = env)
  withr::defer(fs::dir_delete(subNoteDir), envir = env)

  # return the project Rmd
  subNoteRmd


}


local_create_project_note_sub_head_sel2 <- function(subNoteName, subNotePath,
                                                   headNotePath, headLine,
                                                   authorValue,
                                                   env = parent.frame()) {

  # record current state
  olddir <- getwd()

  # generate selection object via projectmanagr function
  selection <- user_selection(headNotePath, headLine)

  # create project note
  create_sub_note(subNoteName, subNotePath, selection,  authorValue)

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
  subNoteRmd <- fs::path(subNotePath, paste0(basename(dirname(subNotePath)), "___002-003_--_", subNoteName, ".Rmd") )
  subNoteDir <- fs::path(subNotePath, paste0(basename(dirname(subNotePath)), "___002-003") )

  # ensure Rmd & Dir are deleted when out of context
  withr::defer(fs::file_delete(subNoteRmd), envir = env)
  withr::defer(fs::dir_delete(subNoteDir), envir = env)

  # return the project Rmd
  subNoteRmd


}




local_create_content <- function(contentName, contentDescription, contentSourcePath,
                                 projectNoteRmd, noteLine, contentTitle,
                                 env = parent.frame() ) {

  # record current state
  olddir <- getwd()

  # generate selection object via projectmanagr function
  selection <- user_selection(projectNoteRmd, noteLine)

  # other ARGS
  contentDeclarationTemplate="Content-Declaration-Template.Rmd"
  contentSourceTemplate="Content-Source-Template.Rmd"

  # create project note
  create_content(selection, contentName, contentDescription,
                 contentSourcePath, contentTitle)

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


local_create_template_section <- function(templateSectionName, templateSectionDir,
                                          contentRmd, contentLine, env = parent.frame() ) {

  # record current state
  olddir <- getwd()

  # generate selection object via projectmanagr function
  selection <- user_selection(contentRmd, contentLine)

  # other ARGS
  contentDeclarationTemplate="Content-Declaration-Template.Rmd"
  contentSourceTemplate="Content-Source-Template.Rmd"

  # create project note
  create_content(selection, contentName, contentDescription,
                 contentSourcePath, contentTitle)

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



local_create_journal <- function(date, organisationPath, authorValue,
                                 env = parent.frame() ) {

  # record current state
  olddir <- getwd()

  # other ARGS
  journalFileNameTemplate="{{YYYY}}-{{MM}}-{{DD}}_{{ORGNAME}}"
  journalTemplate="Work-Journal-Template.Rmd"

  # create weekly journal
  journalRmd <- create_daily_journal(date, organisationPath, authorValue,
                                     journalFileNameTemplate, journalTemplate)

  # define path to Rmd
  journalDir <- fs::path_dir(journalRmd)

  # ensure Rmd & Dir are deleted when out of context
  withr::defer(fs::dir_delete(journalDir), envir = env)

  # return the journal Rmd
  journalRmd


}


local_create_journal2 <- function(date, organisationPath, authorValue,
                                 env = parent.frame() ) {

  # record current state
  olddir <- getwd()

  # other ARGS
  journalFileNameTemplate="{{YYYY}}-{{MM}}-{{DD}}_{{ORGNAME}}"
  journalTemplate="Work-Journal-Template.Rmd"

  # create weekly journal
  journalRmd <- create_daily_journal(date, organisationPath, authorValue,
                                     journalFileNameTemplate, journalTemplate)

  # define path to Rmd
  journalDir <- fs::path_dir(journalRmd)

  # ensure Rmd & Dir are deleted when out of context
  #withr::defer(fs::dir_delete(journalDir), envir = env)

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


add_dispose_datatables <- function(rmd_path, rmd_line=575, datatable_name="smp") {

  dt_length = 100
  summarise_reps=FALSE
  all_reps=FALSE
  cdt="2024-09-10:1330B"
  # test function
  datatable_dispose_rmd(rmd_path, rmd_line, datatable_name,
                        dt_length, summarise_reps, all_reps, cdt)

  rm_lines(rmd_path, (rmd_line+8), (rmd_line+25))

}


add_create_datatables <- function(rmd_path, rmd_line=75, datatable_name="samples") {

  IDs=c(1001, 1002, 1003, 1004)
  data_cols=c("c", "cage", "genotype", "strain_breed_type", "dob_dt")# range of data col lengths
  default_data_vals=list()
  dt_length = 100
  expand=FALSE
  datatable_create_rmd(rmd_path, rmd_line, datatable_name, data_cols, IDs,
                       default_data_vals, dt_length, expand)

  # add second set of sample IDs via CREATE datatable with default_data_vals
  rmd_line= (rmd_line+20) # 95 # blank line after first CREATE datatable
  IDs=c(2001, 2002, 2003, 2004)
  default_data_vals=list(c("F", "F", "M", "M"),
                         c("CID101", "CID102", "CID103", "CID104"),
                         c("vgat:wt", "vgat:wt", "vgat:wt", "vgat:wt"),
                         c("c57bl1", "c57bl2", "c57bl3", "c57bl4"),
                         c("2024-08-21:12:11", "2024-08-21:12:12",
                           "2024-08-21:12:13", "2024-08-21:12:14"))
  datatable_create_rmd(rmd_path, rmd_line, datatable_name, data_cols, IDs,
                       default_data_vals, dt_length, expand)

  # add third set of sample IDs via CREATE datatable with default_data_vals
  # using expand is TRUE and default_data_vals vectors of length 1
  rmd_line= (rmd_line+20) # 115 # blank line after CREATE datatables
  IDs=c(3001, 3002, 3003, 3004)
  default_data_vals=list(c("F"),c("CID101"), c("vgat:wt"),c("c57bl1"),
                         c("2024-08-21:12:11"))
  expand=TRUE
  datatable_create_rmd(rmd_path, rmd_line, datatable_name, data_cols, IDs,
                       default_data_vals, dt_length, expand)

  # add fourth set of sample IDs via CREATE datatable with many data cols
  # so it spills into making a second datatable ADD_DATA
  rmd_line= (rmd_line+20) # 135 # blank line after CREATE datatables
  IDs=c(4001, 4002, 4003, 4004)
  data_cols=c("x","wt-g", "perfuse_dt", "perfusion_con", "group-fix",
              "postfix_dt", "postfix_con", "group-postfix")
  datatable_name=paste0(datatable_name, "2") # new dt name so can read these datatables without error
  default_data_vals=list()
  expand=FALSE
  datatable_create_rmd(rmd_path, rmd_line, datatable_name, data_cols, IDs,
                       default_data_vals, dt_length, expand)

}



add_resample_datatables <- function(rmd_path, rmd_line, datatable_name="samples") {

  # now TEST RESAMPLE FUNCTION
  # resampling to four sub-samples
  resample_vector=c("CNS", "SC-LUM", "DRG-L4-LT", "DRG-L4-RT")
  # variable reps
  rep_vector=c(4,3,1,1)
  dt_length = 100
  summarise_reps=TRUE
  all_reps=FALSE
  # test function
  datatable_resample_rmd(rmd_path, rmd_line, datatable_name, resample_vector,
                         rep_vector, dt_length, summarise_reps, all_reps)

}


add_group_lines <- function(rmd_path, rmd_line) {

  contents <- c(
    "* `group-postfix-time`",
    "",
    "    + `1day` : 1 day in postfix F4M1PB",
    "",
    "    + `3day` : 3 days in postfix F4M1PB",
    "",
    "    + `7day`  : 7 days in postfix F4M1PB",
    "",
    "* `group-postfix-temp`",
    "",
    "    + `RT` : in postfix F4M1PB at room temperature",
    "",
    "    + `4C` : in postfix F4M1PB at 4C",
    "",
    "",
    ">>>> ",
    ">>>>  COMPLETE List : __GROUP_TITLE__ && __GROUP_ID__",
    ">>>>          Copy __GROUP_TITLE__ && __GROUP_ID__ bullets as needed",
    ">>>> ",
    ">>>>  SELECT the __GROUP_TITLE__ and __GROUP_ID__ Bullets",
    ">>>>          ALSO Select these comments to delete them!",
    ">>>> ",
    ">>>>  run projectmanagr::addin_datatable_add_group() - CTRL + M,T,G",
    ">>>>          Select appropriate datatable - all available IDs are selected for adding groups",
    ">>>> ",
    ">>>>  GROUP Datatable generated under group bullets",
    ">>>>          Extracts information to fill datatable with group values",
    ">>>>          NOTE - group values may need adjusting for correct group attribution",
    ">>>> ",
    "")
  local_insert_lines(rmd_path, contents, rmd_line)
}


local_insert_lines <- function(rmd_path, contents, start_line) {

  rmd_contents <- read_file(rmd_path)

  rmd_contents <- c(rmd_contents[1:start_line], contents, rmd_contents[(start_line+1):length(rmd_contents)])

  write_file(rmd_contents, rmd_path)
}




add_template_create_datatables <- function(rmd_path, ins_line) {

  # insert datatables : samples_CNS CREATE & CREATE TEMPLATE (fix-solutions-wts)
  contents <- c(
    "",
    "+===============================================================================",
    "",
    "",
    "    TEMPLATE  :  CREATE  :  fix-solution-wts",
    "",
    "",
    "        ID          wt_g_formulation       ",
    "    =============  ==================      ",
    "",
    "     <<IDS>>-CTL        24.4818            ",
    "                                           ",
    "     <<IDS>>-CNS        22.2222            ",
    "                                           ",
    ">>>>                                                                       ",
    ">>>> Fill Datatable Template column values with appropriate defaults       ",
    ">>>>                                                                       ",
    ">>>> Place CURSOR on this line ...                                         ",
    ">>>>   run projectmanagr::addin_datatable_create() - CTRL + M,T,C          ",
    ">>>>     NAMED TEMPLATE Datatables assume all-IDs should be inserted       ",
    ">>>>                                                                       ",
    "",
    "+===============================================================================",
    "")
  local_insert_lines(rmd_path, contents, ins_line)

}


add_template_create_datatables_ids <- function(rmd_path, ins_line) {

  # insert datatables : samples_CNS CREATE & CREATE TEMPLATE (fix-solutions-wts)
  contents <- c(
    "",
    "+===============================================================================",
    "",
    "",
    "    TEMPLATE  :  CREATE  :  fix-sol-wts",
    "",
    "",
    "        ID          wt_g_formulation       ",
    "    =============  ==================      ",
    "",
    "     <<IDS>>-CTL        24.4818            ",
    "                                           ",
    "     <<IDS>>-CNS        22.2222            ",
    "                                           ",
    ">>>>                                                                       ",
    ">>>> Fill Datatable Template column values with appropriate defaults       ",
    ">>>>                                                                       ",
    ">>>> Place CURSOR on this line ...                                         ",
    ">>>>   run projectmanagr::addin_datatable_create() - CTRL + M,T,C          ",
    ">>>>     NAMED TEMPLATE Datatables assume all-IDs should be inserted       ",
    ">>>>                                                                       ",
    "",
    "+===============================================================================",
    "")
  local_insert_lines(rmd_path, contents, ins_line)

}


add_template_add_data_datatables <- function(rmd_path, ins_line) {

  # insert datatables : samples_CNS CREATE & CREATE TEMPLATE (fix-solutions-wts)
  contents <- c(
    "",
    "+===============================================================================",
    "",
    "",
    "    TEMPLATE  :  ADD_DATA  :  samples_CNS",
    "",
    "",
    "        ID          wt_g_formulation       ",
    "    =============  ==================      ",
    "",
    "     <<IDS>>            24.4818            ",
    "                                           ",
    ">>>>                                                                       ",
    ">>>> Fill Datatable Template column values with appropriate defaults       ",
    ">>>>                                                                       ",
    ">>>> Place CURSOR on this line ...                                         ",
    ">>>>   run projectmanagr::addin_datatable_create() - CTRL + M,T,C          ",
    ">>>>     NAMED TEMPLATE Datatables assume all-IDs should be inserted       ",
    ">>>>                                                                       ",
    "",
    "+===============================================================================",
    "",
    "+===============================================================================",
    "",
    "",
    "    TEMPLATE  :  ADD_DATA",
    "",
    "",
    "        ID          wt_g_form              ",
    "    =============  ==================      ",
    "",
    "     <<IDS>>            22.2222            ",
    "                                           ",
    ">>>>                                                                       ",
    ">>>> Fill Datatable Template column values with appropriate defaults       ",
    ">>>>                                                                       ",
    ">>>> Place CURSOR on this line ...                                         ",
    ">>>>   run projectmanagr::addin_datatable_create() - CTRL + M,T,C          ",
    ">>>>     NAMED TEMPLATE Datatables assume all-IDs should be inserted       ",
    ">>>>                                                                       ",
    "",
    "+===============================================================================",
    "")
  local_insert_lines(rmd_path, contents, ins_line)

}


add_dt_create_test <- function(rmd_path, ins_line) {

  # insert datatables : samples_CNS CREATE & CREATE TEMPLATE (fix-solutions-wts)
  contents <- c(
    "",
    "+===============================================================================",
    "",
    "",
    "    samples_CNS  :  CREATE",
    "",
    "",
    "      ID      x      wt_g         perfuse_dt         perfusion_con      group_fix    ",
    "    ======  =====  ========  ====================  =================  =============  ",
    "",
    "     1001     F      30.0      2020-01-01:12:01        F4M1PB_RT          fix_RT     ",
    "                                                                                     ",
    "     1002     F      30.0      2020-01-01:12:01        F4M1PB_RT          fix_RT     ",
    "                                                                                     ",
    "     1003     F      30.0      2020-01-01:12:01        F4M1PB_RT          fix_4C     ",
    "                                                                                     ",
    "     1004     F      30.0      2020-01-01:12:01        F4M1PB_RT          fix_4C     ",
    "                                                                                     ",
    "",
    "+===============================================================================",
    "")
    local_insert_lines(rmd_path, contents, ins_line)

}


add_dt_create_test_mice <- function(rmd_path, ins_line) {

  # insert datatables : samples_CNS CREATE & CREATE TEMPLATE (fix-solutions-wts)
  contents <- c(
    "",
    "+===============================================================================",
    "",
    "",
    "    mice  :  CREATE",
    "",
    "",
    "      ID      x      wt_g         perfuse_dt         perfusion_con      group_fix    ",
    "    ======  =====  ========  ====================  =================  =============  ",
    "",
    "     1001     F      30.0      2020-01-01:12:01        F4M1PB_RT          fix_RT     ",
    "                                                                                     ",
    "     1002     F      30.0      2020-01-01:12:01        F4M1PB_RT          fix_RT     ",
    "                                                                                     ",
    "     1003     F      30.0      2020-01-01:12:01        F4M1PB_RT          fix_4C     ",
    "                                                                                     ",
    "     1004     F      30.0      2020-01-01:12:01        F4M1PB_RT          fix_4C     ",
    "                                                                                     ",
    "",
    "+===============================================================================",
    "")
  local_insert_lines(rmd_path, contents, ins_line)

}

add_text_group_decl <- function(rmd_path, ins_line) {

  contents <- c(
    "",
    ">>>>",
    ">>>> Delete this Section if no Samples have been grouped",
    "",
    "* `group-ab-conc`",
    "",
    "    + `1µg/ml` :  ab conc in diluent",
    "",
    "    + `0.5µg/ml` :  ab conc in diluent",
    "",
    "* `group-ab-inc`",
    "",
    "    + `1DAY` :  ab inc time",
    "",
    "    + `3DAY` :  ab inc time",
    "",
    "",
    ">>>>",
    ">>>>  COMPLETE List : __GROUP_TITLE__ && __GROUP_ID__",
    ">>>>          Copy __GROUP_TITLE__ && __GROUP_ID__ bullets as needed",
    ">>>>",
    "")
  local_insert_lines(rmd_path, contents, ins_line)

}

add_template_dispose_datatables <- function(rmd_path, ins_line) {

  # insert datatables : samples_CNS CREATE & CREATE TEMPLATE (fix-solutions-wts)
  contents <- c(
    "",
    "+===============================================================================",
    "",
    "",
    "    samples_CNS  :  CREATE",
    "",
    "",
    "      ID      x      wt_g         perfuse_dt         perfusion_con      group_fix    ",
    "    ======  =====  ========  ====================  =================  =============  ",
    "",
    "     1001     F      30.0      2020-01-01:12:01        F4M1PB_RT          fix_RT     ",
    "                                                                                     ",
    "     1002     F      30.0      2020-01-01:12:01        F4M1PB_RT          fix_RT     ",
    "                                                                                     ",
    "     1003     F      30.0      2020-01-01:12:01        F4M1PB_RT          fix_RT     ",
    "                                                                                     ",
    "     1004     F      30.0      2020-01-01:12:01        F4M1PB_RT          fix_RT     ",
    "                                                                                     ",
    "",
    "+===============================================================================",
    "",
    "+===============================================================================",
    "",
    "",
    "    TEMPLATE  :  DISPOSE  :  samples_CNS",
    "",
    "",
    "        ID               dspose            ",
    "    =============  ==================      ",
    "",
    "     <<IDS>>        2024-10-02:1500B       ",
    "                                           ",
    ">>>>                                                                       ",
    ">>>> Fill Datatable Template column values with appropriate defaults       ",
    ">>>>                                                                       ",
    ">>>> Place CURSOR on this line ...                                         ",
    ">>>>   run projectmanagr::addin_datatable_create() - CTRL + M,T,C          ",
    ">>>>     NAMED TEMPLATE Datatables assume all-IDs should be inserted       ",
    ">>>>                                                                       ",
    "",
    "+===============================================================================",
    "",
    "",
    "+===============================================================================",
    "",
    "",
    "    TEMPLATE  :  DISPOSE  :  samples_CNS",
    "",
    "",
    "        ID               dispose           ",
    "    =============  ==================      ",
    "",
    "     <<IDS>>        2024-10-02:1500B       ",
    "                    2024-10-02:1500B       ",
    "                                           ",
    ">>>>                                                                       ",
    ">>>> Fill Datatable Template column values with appropriate defaults       ",
    ">>>>                                                                       ",
    ">>>> Place CURSOR on this line ...                                         ",
    ">>>>   run projectmanagr::addin_datatable_create() - CTRL + M,T,C          ",
    ">>>>     NAMED TEMPLATE Datatables assume all-IDs should be inserted       ",
    ">>>>                                                                       ",
    "",
    "+===============================================================================",
    "",
    "",
    "+===============================================================================",
    "",
    "",
    "    TEMPLATE  :  DISPOSE  :  samples_CNS",
    "",
    "",
    "        ID               dispose           ",
    "    =============  ==================      ",
    "",
    "     <<IDS>>        2024-10-02:1500B       ",
    "                                           ",
    ">>>>                                                                       ",
    ">>>> Fill Datatable Template column values with appropriate defaults       ",
    ">>>>                                                                       ",
    ">>>> Place CURSOR on this line ...                                         ",
    ">>>>   run projectmanagr::addin_datatable_create() - CTRL + M,T,C          ",
    ">>>>     NAMED TEMPLATE Datatables assume all-IDs should be inserted       ",
    ">>>>                                                                       ",
    "",
    "+===============================================================================",
    "",
    "")
  local_insert_lines(rmd_path, contents, ins_line)

}


add_ad_template_dispose_datatables <- function(rmd_path, ins_line) {

  # insert datatables : samples_CNS CREATE & CREATE TEMPLATE (fix-solutions-wts)
  contents <- c(
    "",
    "+===============================================================================",
    "",
    "",
    "    samples  :  CREATE",
    "",
    "",
    "      ID      x      wt_g         perfuse_dt         perfusion_con      group_fix    ",
    "    ======  =====  ========  ====================  =================  =============  ",
    "",
    "     1001     F      30.0      2020-01-01:12:01        F4M1PB_RT          fix_RT     ",
    "                                                                                     ",
    "     1002     F      30.0      2020-01-01:12:01        F4M1PB_RT          fix_RT     ",
    "                                                                                     ",
    "     1003     F      30.0      2020-01-01:12:01        F4M1PB_RT          fix_4C     ",
    "                                                                                     ",
    "     1004     F      30.0      2020-01-01:12:01        F4M1PB_RT          fix_4C     ",
    "                                                                                     ",
    "",
    "+===============================================================================",
    "",
    "",
    "",
    "+===============================================================================",
    "",
    "",
    "    TEMPLATE  :  DISPOSE  :  samples",
    "",
    "",
    "        ID               dispose           ",
    "    =============  ==================      ",
    "",
    "     <<IDS>>        2024-10-02:1500B       ",
    "                                           ",
    ">>>>                                                                       ",
    ">>>> Fill Datatable Template column values with appropriate defaults       ",
    ">>>>                                                                       ",
    ">>>> Place CURSOR on this line ...                                         ",
    ">>>>   run projectmanagr::addin_datatable_create() - CTRL + M,T,C          ",
    ">>>>     NAMED TEMPLATE Datatables assume all-IDs should be inserted       ",
    ">>>>                                                                       ",
    "",
    "+===============================================================================",
    "",
    "",
    "",
    "+===============================================================================",
    "",
    "",
    "    samples_CNS  :  CREATE",
    "",
    "",
    "      ID      x      wt_g         perfuse_dt         perfusion_con      group_fix    ",
    "    ======  =====  ========  ====================  =================  =============  ",
    "",
    "     1001     F      30.0      2020-01-01:12:01        F4M1PB_RT          fix_RT     ",
    "                                                                                     ",
    "     1002     F      30.0      2020-01-01:12:01        F4M1PB_RT          fix_RT     ",
    "                                                                                     ",
    "     1003     F      30.0      2020-01-01:12:01        F4M1PB_RT          fix_4C     ",
    "                                                                                     ",
    "     1004     F      30.0      2020-01-01:12:01        F4M1PB_RT          fix_4C     ",
    "                                                                                     ",
    "",
    "+===============================================================================",
    "",
    "",
    "+===============================================================================",
    "",
    "",
    "    TEMPLATE  :  DISPOSE",
    "",
    "",
    "        ID               dispose           ",
    "    =============  ==================      ",
    "",
    "     <<IDS>>        2024-10-02:1500B       ",
    "                                           ",
    ">>>>                                                                       ",
    ">>>> Fill Datatable Template column values with appropriate defaults       ",
    ">>>>                                                                       ",
    ">>>> Place CURSOR on this line ...                                         ",
    ">>>>   run projectmanagr::addin_datatable_create() - CTRL + M,T,C          ",
    ">>>>     NAMED TEMPLATE Datatables assume all-IDs should be inserted       ",
    ">>>>                                                                       ",
    "",
    "+===============================================================================",
    "",
    "",
    "")
  local_insert_lines(rmd_path, contents, ins_line)

}

add_ad2_template_dispose_datatables <- function(rmd_path, ins_line) {

  # insert datatables : samples_CNS CREATE & CREATE TEMPLATE (fix-solutions-wts)
  contents <- c(
    "",
    "+===============================================================================",
    "",
    "",
    "    TEMPLATE  :  DISPOSE",
    "",
    "",
    "        ID               dispose           ",
    "    =============  ==================      ",
    "",
    "     <<IDS>>        2024-10-02:1500B       ",
    "                                           ",
    ">>>>                                                                       ",
    ">>>> Fill Datatable Template column values with appropriate defaults       ",
    ">>>>                                                                       ",
    ">>>> Place CURSOR on this line ...                                         ",
    ">>>>   run projectmanagr::addin_datatable_create() - CTRL + M,T,C          ",
    ">>>>     NAMED TEMPLATE Datatables assume all-IDs should be inserted       ",
    ">>>>                                                                       ",
    "",
    "+===============================================================================",
    "",
    "",
    "")
  local_insert_lines(rmd_path, contents, ins_line)

}


add_template_resample_datatables <- function(rmd_path, ins_line) {

  # insert datatables : samples_CNS CREATE & CREATE TEMPLATE (fix-solutions-wts)
  contents <- c(
    "",
    "+===============================================================================",
    "",
    "",
    "    samples  :  CREATE",
    "",
    "",
    "      ID      x      wt_g         perfuse_dt         perfusion_con      group_fix    ",
    "    ======  =====  ========  ====================  =================  =============  ",
    "",
    "     1001     F      30.0      2020-01-01:12:01        F4M1PB_RT          fix_RT     ",
    "                                                                                     ",
    "     1002     F      30.0      2020-01-01:12:01        F4M1PB_RT          fix_RT     ",
    "                                                                                     ",
    "     1003     F      30.0      2020-01-01:12:01        F4M1PB_RT          fix_RT     ",
    "                                                                                     ",
    "     1004     F      30.0      2020-01-01:12:01        F4M1PB_RT          fix_RT     ",
    "                                                                                     ",
    "",
    "+===============================================================================",
    "",
    "",
    "+===============================================================================",
    "",
    "",
    "    TEMPLATE  :  RESAMPLE  :  samples",
    "",
    "",
    "        ID          rsample       reps    ",
    "    ============  =============  ========  ",
    "",
    "     <<IDS>>       CNS              1     ",
    "                   SC-LUM           1     ",
    "                   SC-THOR          1     ",
    "                   DRG-L4-LT        1     ",
    "                   DRG-L4-RT        1     ",
    "                                           ",
    ">>>>                                                                       ",
    ">>>> Fill Datatable Template column values with appropriate defaults       ",
    ">>>>                                                                       ",
    ">>>> Place CURSOR on this line ...                                         ",
    ">>>>   run projectmanagr::addin_datatable_create() - CTRL + M,T,C          ",
    ">>>>     NAMED TEMPLATE Datatables assume all-IDs should be inserted       ",
    ">>>>                                                                       ",
    "",
    "+===============================================================================",
    "",
    "",
    "+===============================================================================",
    "",
    "",
    "    TEMPLATE  :  RESAMPLE  :  samples",
    "",
    "",
    "        ID          resample       reps    ",
    "    ============  =============  ========  ",
    "",
    "     <<IDS>>       CNS              1     ",
    "                   SC-LUM           1     ",
    "                   SC-THOR          1     ",
    "                   DRG-L4-LT        1     ",
    "                   DRG-L4-RT              ",
    "                                           ",
    ">>>>                                                                       ",
    ">>>> Fill Datatable Template column values with appropriate defaults       ",
    ">>>>                                                                       ",
    ">>>> Place CURSOR on this line ...                                         ",
    ">>>>   run projectmanagr::addin_datatable_create() - CTRL + M,T,C          ",
    ">>>>     NAMED TEMPLATE Datatables assume all-IDs should be inserted       ",
    ">>>>                                                                       ",
    "",
    "+===============================================================================",
    "",
    "",
    "+===============================================================================",
    "",
    "",
    "    TEMPLATE  :  RESAMPLE  :  samples",
    "",
    "",
    "        ID          resample       reps    ",
    "    ============  =============  ========  ",
    "",
    "     <<IDS>>       CNS              1     ",
    "                   SC-LUM           1     ",
    "                   SC-THOR          1     ",
    "                   DRG-L4-LT        1     ",
    "                   DRG-L4-RT        1     ",
    "                                           ",
    ">>>>                                                                       ",
    ">>>> Fill Datatable Template column values with appropriate defaults       ",
    ">>>>                                                                       ",
    ">>>> Place CURSOR on this line ...                                         ",
    ">>>>   run projectmanagr::addin_datatable_create() - CTRL + M,T,C          ",
    ">>>>     NAMED TEMPLATE Datatables assume all-IDs should be inserted       ",
    ">>>>                                                                       ",
    "",
    "+===============================================================================",
    "")
  local_insert_lines(rmd_path, contents, ins_line)

}


add_ad_template_resample_datatables <- function(rmd_path, ins_line) {

  # insert datatables : samples_CNS CREATE & CREATE TEMPLATE (fix-solutions-wts)
  contents <- c(
    "",
    "+===============================================================================",
    "",
    "",
    "    samples  :  CREATE",
    "",
    "",
    "      ID      x      wt_g         perfuse_dt         perfusion_con      group_fix    ",
    "    ======  =====  ========  ====================  =================  =============  ",
    "",
    "     1001     F      30.0      2020-01-01:12:01        F4M1PB_RT          fix_RT     ",
    "                                                                                     ",
    "     1002     F      30.0      2020-01-01:12:01        F4M1PB_RT          fix_RT     ",
    "                                                                                     ",
    "     1003     F      30.0      2020-01-01:12:01        F4M1PB_4C          fix_4C     ",
    "                                                                                     ",
    "     1004     F      30.0      2020-01-01:12:01        F4M1PB_4C          fix_4C     ",
    "                                                                                     ",
    "",
    "+===============================================================================",
    "",
    "",
    "+===============================================================================",
    "",
    "",
    "    TEMPLATE  :  RESAMPLE  :  samples",
    "",
    "",
    "        ID          resample       reps    ",
    "    ============  =============  ========  ",
    "",
    "     <<IDS>>       CNS              1     ",
    "                   SC-LUM           1     ",
    "                   SC-THOR          1     ",
    "                   DRG-L4-LT        1     ",
    "                   DRG-L4-RT        1     ",
    "                                           ",
    ">>>>                                                                       ",
    ">>>> Fill Datatable Template column values with appropriate defaults       ",
    ">>>>                                                                       ",
    ">>>> Place CURSOR on this line ...                                         ",
    ">>>>   run projectmanagr::addin_datatable_create() - CTRL + M,T,C          ",
    ">>>>     NAMED TEMPLATE Datatables assume all-IDs should be inserted       ",
    ">>>>                                                                       ",
    "",
    "+===============================================================================",
    "",
    "",
    "",
    "+===============================================================================",
    "",
    "",
    "    samples2  :  CREATE",
    "",
    "",
    "      ID      x      wt_g         perfuse_dt         perfusion_con      group_fix    ",
    "    ======  =====  ========  ====================  =================  =============  ",
    "",
    "     1001     F      30.0      2020-01-01:12:01        F4M1PB_RT          fix_RT     ",
    "                                                                                     ",
    "     1002     F      30.0      2020-01-01:12:01        F4M1PB_RT          fix_RT     ",
    "                                                                                     ",
    "     1003     F      30.0      2020-01-01:12:01        F4M1PB_4C          fix_4C     ",
    "                                                                                     ",
    "     1004     F      30.0      2020-01-01:12:01        F4M1PB_4C          fix_4C     ",
    "                                                                                     ",
    "",
    "+===============================================================================",
    "",
    "",
    "+===============================================================================",
    "",
    "",
    "    TEMPLATE  :  RESAMPLE",
    "",
    "",
    "        ID          resample       reps    ",
    "    ============  =============  ========  ",
    "",
    "     <<IDS>>       CNS              1     ",
    "                   SC-LUM           1     ",
    "                   SC-THOR          1     ",
    "                   DRG-L4-LT        1     ",
    "                   DRG-L4-RT        1     ",
    "                                           ",
    ">>>>                                                                       ",
    ">>>> Fill Datatable Template column values with appropriate defaults       ",
    ">>>>                                                                       ",
    ">>>> Place CURSOR on this line ...                                         ",
    ">>>>   run projectmanagr::addin_datatable_create() - CTRL + M,T,C          ",
    ">>>>     NAMED TEMPLATE Datatables assume all-IDs should be inserted       ",
    ">>>>                                                                       ",
    "",
    "+===============================================================================",
    "",
    "",

    "")
  local_insert_lines(rmd_path, contents, ins_line)

}


add_ad2_template_resample_datatables <- function(rmd_path, ins_line) {

  # insert datatables : samples_CNS CREATE & CREATE TEMPLATE (fix-solutions-wts)
  contents <- c(
    "",
    "+===============================================================================",
    "",
    "",
    "    TEMPLATE  :  RESAMPLE",
    "",
    "",
    "        ID          resample       reps    ",
    "    ============  =============  ========  ",
    "",
    "     <<IDS>>       CNS              1     ",
    "                   SC-LUM           1     ",
    "                   SC-THOR          1     ",
    "                   DRG-L4-LT        1     ",
    "                   DRG-L4-RT        1     ",
    "                                           ",
    ">>>>                                                                       ",
    ">>>> Fill Datatable Template column values with appropriate defaults       ",
    ">>>>                                                                       ",
    ">>>> Place CURSOR on this line ...                                         ",
    ">>>>   run projectmanagr::addin_datatable_create() - CTRL + M,T,C          ",
    ">>>>     NAMED TEMPLATE Datatables assume all-IDs should be inserted       ",
    ">>>>                                                                       ",
    "",
    "+===============================================================================",
    "")
  local_insert_lines(rmd_path, contents, ins_line)

}

add_template_add_group_datatables <- function(rmd_path, ins_line) {

  # insert datatables : samples_CNS CREATE & CREATE TEMPLATE (fix-solutions-wts)
  contents <- c(
    "",
    "+===============================================================================",
    "",
    "",
    "    samples  :  CREATE",
    "",
    "",
    "      ID      x      wt_g         perfuse_dt         perfusion_con      group_fix    ",
    "    ======  =====  ========  ====================  =================  =============  ",
    "",
    "     1001     F      30.0      2020-01-01:12:01        F4M1PB_RT          fix_RT     ",
    "                                                                                     ",
    "     1002     F      30.0      2020-01-01:12:01        F4M1PB_RT          fix_RT     ",
    "                                                                                     ",
    "     1003     F      30.0      2020-01-01:12:01        F4M1PB_RT          fix_RT     ",
    "                                                                                     ",
    "     1004     F      30.0      2020-01-01:12:01        F4M1PB_RT          fix_RT     ",
    "                                                                                     ",
    "",
    "+===============================================================================",
    "",
    "",
    "+===============================================================================",
    "",
    "",
    "    TEMPLATE  :  RESAMPLE  :  samples",
    "",
    "",
    "        ID          group_ab_conc      group_solvent_inc    ",
    "    ============  =================  =====================  ",
    "",
    "     <<IDS>>         1mg/ml                1Hr              ",
    "                     2mg/ml                2Hr              ",
    "                                           4Hr              ",
    "                                                            ",
    ">>>>                                                                       ",
    ">>>> Fill Datatable Template column values with appropriate defaults       ",
    ">>>>                                                                       ",
    ">>>> Place CURSOR on this line ...                                         ",
    ">>>>   run projectmanagr::addin_datatable_create() - CTRL + M,T,C          ",
    ">>>>     NAMED TEMPLATE Datatables assume all-IDs should be inserted       ",
    ">>>>                                                                       ",
    "",
    "+===============================================================================",
    "")
  local_insert_lines(rmd_path, contents, ins_line)

}


add_template_add_data_samples_datatables <- function(rmd_path, ins_line) {

  # insert datatables : samples_CNS CREATE & CREATE TEMPLATE (fix-solutions-wts)
  contents <- c(
    "",
    "+===============================================================================",
    "",
    "",
    "    samples_CNS  :  CREATE",
    "",
    "",
    "      ID      x      wt_g         perfuse_dt         perfusion_con      group_fix    ",
    "    ======  =====  ========  ====================  =================  =============  ",
    "",
    "     1001     F      30.0      2020-01-01:12:01        F4M1PB_RT          fix_RT     ",
    "                                                                                     ",
    "     1002     F      30.0      2020-01-01:12:01        F4M1PB_RT          fix_RT     ",
    "                                                                                     ",
    "     1003     F      30.0      2020-01-01:12:01        F4M1PB_RT          fix_RT     ",
    "                                                                                     ",
    "     1004     F      30.0      2020-01-01:12:01        F4M1PB_RT          fix_RT     ",
    "                                                                                     ",
    "",
    "+===============================================================================",
    "",
    "",
    "+===============================================================================",
    "",
    "",
    "    TEMPLATE  :  ADD_DATA",
    "",
    "",
    "        ID       store_wash_dt       store_wash_con    store_wash_loc      ",
    "    =========  ==================  ================  ===================   ",
    "",
    "      <<IDS>>    INSERT__DATETIME    PBS_RT            L160:BENCH:shaker   ",
    "                 INSERT__DATETIME    PBS_RT            L160:BENCH:shaker   ",
    "                 INSERT__DATETIME    PBS_RT            L160:BENCH:shaker   ",
    "                                           ",
    ">>>>                                                                       ",
    ">>>> Fill Datatable Template column values with appropriate defaults       ",
    ">>>>                                                                       ",
    ">>>> Place CURSOR on this line ...                                         ",
    ">>>>   run projectmanagr::addin_datatable_create() - CTRL + M,T,C          ",
    ">>>>     NAMED TEMPLATE Datatables assume all-IDs should be inserted       ",
    ">>>>                                                                       ",
    "",
    "+===============================================================================",
    "")
  local_insert_lines(rmd_path, contents, ins_line)

}


local_add_todos_to_project_note <- function(projectNoteRmd) {

  rmd_contents <- read_file(projectNoteRmd)

  # Add todo items to rmd_contents
  # todays date is mcoked to be 2025-02-22
  rmd_contents <- c( rmd_contents[1:74],
    "",
    "",
    "",
    "### Standard Todo Header",
    "",
    "",
    "* This is a Standard Todo Section",
    "",
    "",
    "<!-- {#todo P4}",
    "this is a standard todo description with low priority P4",
    "the standard todo description can go over multiple lines",
    "And add more context & progress updates",
    "* This should be extracted verbatim",
    "    + this should be in a bulleted list!",
    "-->",
    "",
    "",
    "",
    "### SECOND Todo Header",
    "",
    "",
    "* This is a Second Standard Todo Section",
    "",
    "",
    "<!-- {#todo P3}",
    "this is a SECOND standard todo description with default priority P3",
    "to check ordering of todo items",
    "the standard todo description can go over multiple lines",
    "And add more context & progress updates -->",
    "",
    "",
    "--------",
    "--------",
    "",
    "",
    "",
    "### Deadline Todo Header",
    "",
    "",
    "* This is a Todo Section with deadline",
    "",
    "",
    "<!-- {#todo P3}",
    "this is a deadline todo description with default priority P3",
    "the standard todo description can go over multiple lines",
    "It has a deadline specified in s deadline tag at the end",
    "{#deadline 2025-02-27} -->",
    "",
    "",
    "",
    "",
    "### Deadline Todo Today Header",
    "",
    "",
    "* This is a Todo Section with deadline set for today",
    "",
    "",
    "<!-- {#todo P3}",
    "this is a deadline todo for today description",
    "the standard todo description can go over multiple lines",
    "It has a deadline specified in s deadline tag at the end",
    "{#deadline 2025-02-22} -->",
    "",
    "",
    "",
    "### Deadline Todo Expired Header",
    "",
    "",
    "* This is a Todo Section with deadline",
    "",
    "",
    "<!-- {#todo P3}",
    "this is an EXPIRED deadline todo description",
    "the standard todo description can go over multiple lines",
    "It has a deadline specified in s deadline tag at the end",
    "{#deadline 2025-02-20} -->",
    "",
    "",
    "------------------------------------",
    "------------------------------------",
    "",
    "",
    "",
    "### Scheduled Todo Header",
    "",
    "",
    "* This is a Todo Section with scheduled time",
    "",
    "",
    "<!-- {#todo P3}",
    "this is a scheduled todo description",
    "the standard todo description can go over multiple lines",
    "It has a schedule datetime specified in a schedule tag at the end",
    "{#schedule 2025-02-25:1400Z} -->",
    "",
    "",
    "",
    "### Scheduled Todo Today Header",
    "",
    "",
    "* This is a Todo Section with scheduled time for today",
    "",
    "",
    "<!-- {#todo P3}",
    "this is a scheduled todo for today description",
    "the standard todo description can go over multiple lines",
    "It has a schedule datetime specified in a schedule tag at the end",
    "{#schedule 2025-02-22:1300Z} -->",
    "",
    "",
    "",
    "### Scheduled Todo Expired Header",
    "",
    "",
    "* This is a Todo Section with scheduled time",
    "",
    "",
    "<!-- {#todo P3}",
    "this is an EXPIRED scheduled todo description",
    "the standard todo description can go over multiple lines",
    "It has a schedule datetime specified in a schedule tag at the end",
    "{#schedule 2025-02-20:1300Z} -->",
    "",
    "",
    "--------------------------------------------------------------------------------",
    "--------------------------------------------------------------------------------",
    "",
    "",
    rmd_contents[75:length(rmd_contents)]
  )

  write_file(rmd_contents, projectNoteRmd)

}


Olocal_add_todos_to_project_note <- function(projectNoteRmd) {

  rmd_contents <- read_file(projectNoteRmd)

  # Add todo items to rmd_contents
  # todays date is mcoked to be 2025-02-22
  rmd_contents <- c( rmd_contents[1:74],
                     "",
                     "",
                     "",
                     "### Standard Todo Header",
                     "",
                     "",
                     "* This is a Standard Todo Section",
                     "",
                     "",
                     "<!-- {#todo P3}",
                     "this is a standard todo description with default priority P3",
                     "the standard todo description can go over multiple lines",
                     "And add more context & progress updates",
                     "* This should be extracted verbatim",
                     "    + this should be in a bulleted list!",
                     "-->",
                     "",
                     "",
                     "",
                     "### SECOND Todo Header",
                     "",
                     "",
                     "* This is a Second Standard Todo Section",
                     "",
                     "",
                     "<!-- {#todo P3}",
                     "this is a SECOND standard todo description with default priority P3",
                     "to check ordering of todo items",
                     "the standard todo description can go over multiple lines",
                     "And add more context & progress updates -->",
                     "",
                     "",
                     "--------",
                     "--------",
                     "",
                     "",
                     "",
                     "### Deadline Todo Header",
                     "",
                     "",
                     "* This is a Todo Section with deadline",
                     "",
                     "",
                     "<!-- {#todo P3}",
                     "this is a deadline todo description with default priority P3",
                     "the standard todo description can go over multiple lines",
                     "It has a deadline specified in s deadline tag at the end",
                     "{#deadline 2025-02-27} -->",
                     "",
                     "",
                     "",
                     "### Second Deadline Todo Header",
                     "",
                     "",
                     "* This is a Todo Section with deadline",
                     "",
                     "",
                     "<!-- {#todo P3}",
                     "this is a 2nd deadline todo description",
                     "deadline is later than first -check ordering",
                     "the standard todo description can go over multiple lines",
                     "It has a deadline specified in s deadline tag at the end",
                     "{#deadline 2025-02-28} -->",
                     "",
                     "",
                     "### Third Deadline Todo Header",
                     "",
                     "",
                     "* This is a Todo Section with deadline",
                     "",
                     "",
                     "<!-- {#todo P3} Todo Title on Third Deadline Todo Header",
                     "this is a 3rd deadline todo description",
                     "deadline is later than first -check ordering",
                     "the standard todo description can go over multiple lines",
                     "It has a deadline specified in s deadline tag at the end",
                     "{#deadline 2025-02-26} -->",
                     "",
                     "",
                     "",
                     "### Deadline Todo Today Header",
                     "",
                     "",
                     "* This is a Todo Section with deadline set for today",
                     "",
                     "",
                     "<!-- {#todo P3}",
                     "this is a deadline todo for today description",
                     "the standard todo description can go over multiple lines",
                     "It has a deadline specified in s deadline tag at the end",
                     "{#deadline 2025-02-22} -->",
                     "",
                     "",
                     "",
                     "### Deadline Todo Expired Header",
                     "",
                     "",
                     "* This is a Todo Section with deadline",
                     "",
                     "",
                     "<!-- {#todo P3}",
                     "this is an EXPIRED deadline todo description",
                     "the standard todo description can go over multiple lines",
                     "It has a deadline specified in s deadline tag at the end",
                     "{#deadline 2025-02-20} -->",
                     "",
                     "",
                     "------------------------------------",
                     "------------------------------------",
                     "",
                     "",
                     "",
                     "### Scheduled Todo Header",
                     "",
                     "",
                     "* This is a Todo Section with scheduled time",
                     "",
                     "",
                     "<!-- {#todo P3}",
                     "this is a scheduled todo description",
                     "the standard todo description can go over multiple lines",
                     "It has a schedule datetime specified in a schedule tag at the end",
                     "{#schedule 2025-02-25:1400Z} -->",
                     "",
                     "",
                     "",
                     "### Scheduled Second Todo Header",
                     "",
                     "",
                     "* This is a Xecond Todo Section with scheduled time",
                     "",
                     "",
                     "<!-- {#todo P3}",
                     "this is a 2nd scheduled todo description",
                     "check ordering - 2nd scheduled todo is after first!",
                     "the standard todo description can go over multiple lines",
                     "It has a schedule datetime specified in a schedule tag at the end",
                     "{#schedule 2025-02-26:1400Z} -->",
                     "",
                     "",
                     "",
                     "### Scheduled Todo Today Header",
                     "",
                     "",
                     "* This is a Todo Section with scheduled time for today",
                     "",
                     "",
                     "<!-- {#todo P3}",
                     "this is a scheduled todo for today description",
                     "the standard todo description can go over multiple lines",
                     "It has a schedule datetime specified in a schedule tag at the end",
                     "{#schedule 2025-02-22:1300Z} -->",
                     "",
                     "",
                     "",
                     "### Scheduled Todo Header",
                     "",
                     "",
                     "* This is a Todo Section with scheduled time",
                     "",
                     "",
                     "<!-- {#todo P3}",
                     "this is a scheduled todo description",
                     "the standard todo description can go over multiple lines",
                     "It has a schedule datetime specified in a schedule tag at the end",
                     "{#schedule 2025-02-25:1200Z} -->",
                     "",
                     "",
                     "",
                     "### Scheduled Todo Expired Header",
                     "",
                     "",
                     "* This is a Todo Section with scheduled time",
                     "",
                     "",
                     "<!-- {#todo P3}",
                     "this is an EXPIRED scheduled todo description",
                     "the standard todo description can go over multiple lines",
                     "It has a schedule datetime specified in a schedule tag at the end",
                     "{#schedule 2025-02-20:1300Z} -->",
                     "",
                     "",
                     "------------------------------------",
                     "------------------------------------",
                     ##                     SCH-TODAY DEAD-TODAY SCH-PASS DEAD-PASS
                     ## Sch-Dead                 o        o         o         o
                     ## Sch-Dead S-T             x        o         o         o
                     ## Sch-Dead D-T             o        x         o         o
                     ## Sch-Dead S-P             o        o         x         o
                     ## Sch-Dead D-P             o        o         o         x
                     ## S-T D-T                  x        x         o         o
                     ## S-T D-P                  x        o         o         x
                     ## D-T S-P                  o        x         x         o
                     ## S-P D-P                  o        x         o         x
                     "",
                     "",
                     "",
                     "### Sch-Dead Todo Header",
                     "",
                     "",
                     "* This is a Todo Section with a scheduled time & deadline",
                     "",
                     "",
                     "<!-- {#todo P3}",
                     "this is a scheduled todo description",
                     "the standard todo description can go over multiple lines",
                     "It has a schedule datetime specified in a schedule tag",
                     "And a deadline specified in s deadline tag at the end",
                     "{#schedule 2025-02-28:1400Z}",
                     "{#deadline 2025-02-29} -->",
                     "",
                     "",
                     "",
                     "### Sch-Dead Todo ST Header",
                     "",
                     "",
                     "* This is a Todo Section for today with a scheduled time & deadline",
                     "",
                     "",
                     "<!-- {#todo P3}",
                     "this is a scheduled todo for today description",
                     "the standard todo description can go over multiple lines",
                     "It has a schedule datetime specified in a schedule tag",
                     "And a deadline specified in s deadline tag at the end",
                     "{#schedule 2025-02-22:0900Z}",
                     "{#deadline 2025-02-29} -->",
                     "",
                     "",
                     "",
                     "### Sch-Dead Todo DT Header",
                     "",
                     "",
                     "* This is a Todo Section for today with a scheduled time & deadline",
                     "",
                     "",
                     "<!-- {#todo P3}",
                     "this is a scheduled todo for today description",
                     "the standard todo description can go over multiple lines",
                     "It has a schedule datetime specified in a schedule tag",
                     "And a deadline specified in s deadline tag at the end",
                     "{#schedule 2025-02-29:1000Z}",
                     "{#deadline 2025-02-22} -->",
                     "",
                     "",
                     "",
                     "### Sch-Dead Todo SP Header",
                     "",
                     "",
                     "* This is a Todo Section for today with a scheduled time & deadline",
                     "",
                     "",
                     "<!-- {#todo P3}",
                     "this is a scheduled todo for today description",
                     "the standard todo description can go over multiple lines",
                     "It has a schedule datetime specified in a schedule tag",
                     "And a deadline specified in s deadline tag at the end",
                     "{#schedule 2025-02-20:1100Z}",
                     "{#deadline 2025-02-29} -->",
                     "",
                     "",
                     "",
                     "### Sch-Dead Todo DP Header",
                     "",
                     "",
                     "* This is a Todo Section for today with a scheduled time & deadline",
                     "",
                     "",
                     "<!-- {#todo P3}",
                     "this is a scheduled todo for today description",
                     "the standard todo description can go over multiple lines",
                     "It has a schedule datetime specified in a schedule tag",
                     "And a deadline specified in s deadline tag at the end",
                     "{#schedule 2025-02-29:1200Z}",
                     "{#deadline 2025-02-20} -->",
                     "",
                     "",
                     "",
                     "### Sch-Dead Todo ST DT Header",
                     "",
                     "",
                     "* This is a Todo Section for today with a scheduled time & deadline",
                     "",
                     "",
                     "<!-- {#todo P3}",
                     "this is a scheduled todo for today description",
                     "the standard todo description can go over multiple lines",
                     "It has a schedule datetime specified in a schedule tag",
                     "And a deadline specified in s deadline tag at the end",
                     "{#schedule 2025-02-22:1300Z}",
                     "{#deadline 2025-02-22} -->",
                     "",
                     "",
                     "",
                     "### Sch-Dead Todo ST DP Header",
                     "",
                     "",
                     "* This is a Todo Section for today with a scheduled time & deadline",
                     "",
                     "",
                     "<!-- {#todo P3}",
                     "this is a scheduled todo for today description",
                     "the standard todo description can go over multiple lines",
                     "It has a schedule datetime specified in a schedule tag",
                     "And a deadline specified in s deadline tag at the end",
                     "{#schedule 2025-02-22:1400Z}",
                     "{#deadline 2025-02-20} -->",
                     "",
                     "",
                     "",
                     "### Sch-Dead Todo DT SP Header",
                     "",
                     "",
                     "* This is a Todo Section for today with a scheduled time & deadline",
                     "",
                     "",
                     "<!-- {#todo P3}",
                     "this is a scheduled todo for today description",
                     "the standard todo description can go over multiple lines",
                     "It has a schedule datetime specified in a schedule tag",
                     "And a deadline specified in s deadline tag at the end",
                     "{#schedule 2025-02-20:1400Z}",
                     "{#deadline 2025-02-22} -->",
                     "",
                     "",
                     "",
                     "### Sch-Dead Todo SP DP Header",
                     "",
                     "",
                     "* This is a Todo Section for today with a scheduled time & deadline",
                     "",
                     "",
                     "<!-- {#todo P3}",
                     "this is a scheduled todo for today description",
                     "the standard todo description can go over multiple lines",
                     "It has a schedule datetime specified in a schedule tag",
                     "And a deadline specified in s deadline tag at the end",
                     "{#schedule 2025-02-20:1500Z}",
                     "{#deadline 2025-02-20} -->",
                     "",
                     "",
                     "",
                     "### Priority‑group test",
                     "",
                     "<!-- {#todo P1 #prio-grp}",
                     "P1 & prio‑grp description line",
                     "-->",
                     "",
                     "<!-- {#todo P2 #prio-grp}",
                     "dup prio‑grp description line (P2) SHOULD BE SKIPPED",
                     "-->",
                     "",
                     "<!-- {#todo P3 #prio-grp}",
                     "dup prio‑grp description line (P3) SHOULD BE SKIPPED",
                     "-->",
                     "",
                     "### Ordering test (no group)",
                     "",
                     "<!-- {#todo P5}  low priority no‑group",
                     "order test low (P5)",
                     "-->",
                     "",
                     "<!-- {#todo P1}  high priority no‑group",
                     "order test high (P1)",
                     "-->",
                     "",
                     "--------------------------------------------------------------------------------",
                     "--------------------------------------------------------------------------------",
                     "",
                     "",
                     rmd_contents[75:length(rmd_contents)]
  )

  write_file(rmd_contents, projectNoteRmd)

}

make_td <- function(priority = NA_integer_, groups = character(),
                    idx = 1L, heading = "Heading") {
  list(
    heading      = heading,
    title        = paste0("Item", idx),
    text         = paste("body", idx),
    deadline     = "",
    schedule     = "",
    pTagPriority = priority,
    groups       = groups
  )
}

local_add_todos_to_sub_note1 <- function(subNoteRmd) {

  rmd_contents <- read_file(subNoteRmd)

  # Add todo items to rmd_contents
  # todays date is 2025-02-22
  rmd_contents <- c( rmd_contents[1:48],
                     "### Test Sub Todo1",
                     "",
                     "",
                     "Test Todo text",
                     "",
                     "* This is a Test Todo",
                     "",
                     "    + This is a Test Todo sub-comment",
                     "",
                     "    + This is another Test Todo sub-comment",
                     "",
                     "",
                     "",
                     "#### [] SUB1 todo subtask 1",
                     "",
                     "",
                     "* Test Todo sub-task comments",
                     "",
                     "    + These stay in the project note TODO section",
                     "",
                     "    + and are NOT extracted with the todo subtask header",
                     "",
                     "",
                     "",
                     "#### [] SUB1 todo subtask 2",
                     "",
                     "",
                     "Test Todo sub-task comments",
                     "",
                     "* These stay in the project note TODO section",
                     "",
                     "* and are NOT extracted with the todo subtask header",
                     "",
                     "",
                     "",
                     "--------------------------------------------------------------------------------",
                     rmd_contents[61:length(rmd_contents)]
  )

  write_file(rmd_contents, subNoteRmd)

}

local_add_todos_to_sub_note2 <- function(subNoteRmd) {

  rmd_contents <- read_file(subNoteRmd)

  # Add todo items to rmd_contents
  # todays date is 2025-02-22
  rmd_contents <- c( rmd_contents[1:48],
                     "### Test Sub Todo2",
                     "",
                     "",
                     "Test Todo text",
                     "",
                     "* This is a Test Todo",
                     "",
                     "    + This is a Test Todo sub-comment",
                     "",
                     "    + This is another Test Todo sub-comment",
                     "",
                     "",
                     "",
                     "#### [] SUB2 todo subtask 1",
                     "",
                     "",
                     "* Test Todo sub-task comments",
                     "",
                     "    + These stay in the project note TODO section",
                     "",
                     "    + and are NOT extracted with the todo subtask header",
                     "",
                     "",
                     "",
                     "#### [] SUB2 todo subtask 2",
                     "",
                     "",
                     "Test Todo sub-task comments",
                     "",
                     "* These stay in the project note TODO section",
                     "",
                     "* and are NOT extracted with the todo subtask header",
                     "",
                     "",
                     "",
                     "--------------------------------------------------------------------------------",
                     rmd_contents[61:length(rmd_contents)]
  )

  write_file(rmd_contents, subNoteRmd)

}

local_add_todos_to_sub_note3 <- function(subNoteRmd) {

  rmd_contents <- read_file(subNoteRmd)

  # Add todo items to rmd_contents
  # todays date is 2025-02-22
  rmd_contents <- c( rmd_contents[1:48],
                     "### Test Sub Todo3",
                     "",
                     "",
                     "Test Todo text",
                     "",
                     "* This is a Test Todo",
                     "",
                     "    + This is a Test Todo sub-comment",
                     "",
                     "    + This is another Test Todo sub-comment",
                     "",
                     "",
                     "",
                     "#### [] SUB3 todo subtask 1",
                     "",
                     "",
                     "* Test Todo sub-task comments",
                     "",
                     "    + These stay in the project note TODO section",
                     "",
                     "    + and are NOT extracted with the todo subtask header",
                     "",
                     "",
                     "",
                     "#### [] SUB3 todo subtask 2",
                     "",
                     "",
                     "Test Todo sub-task comments",
                     "",
                     "* These stay in the project note TODO section",
                     "",
                     "* and are NOT extracted with the todo subtask header",
                     "",
                     "",
                     "",
                     "--------------------------------------------------------------------------------",
                     rmd_contents[61:length(rmd_contents)]
  )

  write_file(rmd_contents, subNoteRmd)
}



rm_lines <- function(rmd_path, start_line, end_line) {
  fileConn <- file(rmd_path)
  rmd_contents <- readLines(fileConn)
  close(fileConn)

  rmd_contents <- c(rmd_contents[1:start_line], rmd_contents[end_line:length(rmd_contents)])

  fileConn <- file(rmd_path)
  writeLines(rmd_contents, fileConn)
  close(fileConn)
}



gen_pn_cre <- function( settings, projectDocRmd, projectNotePath) {
  # create test Project Note for datatables : CREATE
  projectNoteNameCre <- "PN_cre"
  projectNoteRmdCre <- local_create_project_note_simple(projectNoteNameCre, projectNotePath,
                                                        projectDocRmd, taskLine)
  projectNoteDirCre <- get_project_note_dir_path(projectNoteRmdCre, settings)

  projectNoteRmdCre # return
}


find_add_create_dt1 <- function(rmd_path, settings, rmd_line=75, datatable_name="samples") {

  data_cols=c("sex","dob_dt","colony_genotype", "treatment", "cage_dt","cage_con","cage_loc")
  IDs=c(1001, 1002, 1003, 1004)
  dt_length = 100
  default_data_vals=list(c("F", "F", "M", "M"),
                         c("2020-01-01","2020-01-01","2020-01-01","2020-01-01"),
                         c("nNOS-cre:wt", "nNOS-cre:wt", "nNOS-cre:wt", "nNOS-cre:wt"),
                         c("NAIVE","NAIVE","NAIVE","NAIVE"),
                         c("2020-01-01:12:00", "2020-01-21:12:00","2020-01-01:12:00", "2020-01-21:12:00",
                           "2020-01-01:12:00", "2020-01-21:12:00","2020-01-01:12:00", "2020-01-21:12:00"),
                         c("IVC:(GM500)", "IVC:(GM500)","IVC:(GM500)", "IVC:(GM500)",
                           "IVC:(GM500)", "IVC:(GM500)","IVC:(GM500)", "IVC:(GM500)"),
                         c("SAAA-0300101", "SAAA-0300102","SAAA-0300101", "SAAA-0300102",
                           "SAAA-0300101", "SAAA-0300102","SAAA-0300101", "SAAA-0300102"))
  expand=FALSE
  datatable_create_rmd(rmd_path, rmd_line, settings, datatable_name, data_cols, IDs,
                       default_data_vals, dt_length, expand)


  # default_data_vals=list(
  #                        c("21.0", "31.0","22.0", "32.0",
  #                          "23.0", "33.0","24.0", "34.0"),
  #                        c("2020-01-15:12:00", "2020-03-15:12:00","2020-01-15:12:00", "2020-03-15:12:00",
  #                          "2020-01-15:12:00", "2020-03-15:12:00","2020-01-15:12:00", "2020-03-15:12:00"))

}

find_add_data_dt1 <- function(rmd_path, settings, rmd_line=75, datatable_name="samples") {

  data_cols=c("wts_g", "wts_g_dt")
  IDs=c(1001, 1002, 1003, 1004)
  dt_length = 100
  default_data_vals=list(
    c("21.0", "31.0","22.0", "32.0",
      "23.0", "33.0","24.0", "34.0"),
    c("2020-01-15:12:00", "2020-03-15:12:00","2020-01-15:12:00", "2020-03-15:12:00",
      "2020-01-15:12:00", "2020-03-15:12:00","2020-01-15:12:00", "2020-03-15:12:00"))
  datatable_add_data_samples_rmd(rmd_path, rmd_line, settings, datatable_name, data_cols, IDs,
                                 default_data_vals, dt_length)



}

dt_find_add_create_dt1 <- function(rmd_path, ins_line) {

  # insert datatables : samples_CNS CREATE & CREATE TEMPLATE (fix-solutions-wts)
  contents <- c(
    "+===============================================================================",
    "",
    "",
    "    mice  :  CREATE",
    "",
    "",
    "      ID      sex          dob_dt          colony_genotype      treatment           cage_dt        ",
    "    ======  =======  ==================  ===================  =============  ====================  ",
    "",
    "     1001      F         2020-01-01          nNOS-cre:wt          NAIVE        2020-01-01:12:00    ",
    "                                                                               2020-01-21:12:00    ",
    "                                                                                                   ",
    "     1002      F         2020-01-01          nNOS-cre:wt          NAIVE        2020-01-01:12:00    ",
    "                                                                               2020-01-21:12:00    ",
    "                                                                                                   ",
    "     1003      M         2020-01-01          nNOS-cre:wt          NAIVE        2020-01-01:12:00    ",
    "                                                                               2020-01-21:12:00    ",
    "                                                                                                   ",
    "     1004      M         2020-01-01          nNOS-cre:wt          NAIVE        2020-01-01:12:00    ",
    "                                                                               2020-01-21:12:00    ",
    "                                                                                                   ",
    "",
    "+===============================================================================",
    "",
    "+===============================================================================",
    "",
    "",
    "    mice  :  ADD_DATA",
    "",
    "",
    "      ID        cage_con         cage_loc      ",
    "    ======  ===============  ================  ",
    "",
    "     1001     IVC:(GM500)      SAAA-0300101    ",
    "              IVC:(GM500)      SAAA-0300102    ",
    "                                               ",
    "     1002     IVC:(GM500)      SAAA-0300101    ",
    "              IVC:(GM500)      SAAA-0300102    ",
    "                                               ",
    "     1003     IVC:(GM500)      SAAA-0300101    ",
    "              IVC:(GM500)      SAAA-0300102    ",
    "                                               ",
    "     1004     IVC:(GM500)      SAAA-0300101    ",
    "              IVC:(GM500)      SAAA-0300102    ",
    "                                               ",
    "",
    "+===============================================================================",
    "",
    "",
    "+===============================================================================",
    "",
    "",
    "    mice  :  ADD_DATA",
    "",
    "",
    "      ID      wts_g          wts_g_dt        ",
    "    ======  =========  ====================  ",
    "",
    "     1001      21.0      2020-01-15:12:00    ",
    "               31.0      2020-03-15:12:00    ",
    "",
    "     1002      22.0      2020-01-15:12:00    ",
    "               32.0      2020-03-15:12:00    ",
    "                                             ",
    "     1003      23.0      2020-01-15:12:00    ",
    "               33.0      2020-03-15:12:00    ",
    "                                             ",
    "     1004      24.0      2020-01-15:12:00    ",
    "               34.0      2020-03-15:12:00    ",
    "                                             ",
    "",
    "+===============================================================================",
    "",
    "")
  local_insert_lines(rmd_path, contents, ins_line)

}


dt_find_add_create_dt2 <- function(rmd_path, ins_line) {

  # insert datatables : samples_CNS CREATE & CREATE TEMPLATE (fix-solutions-wts)
  contents <- c(
    "+===============================================================================",
    "",
    "",
    "    mice  :  CREATE",
    "",
    "",
    "      ID      sex          dob_dt          colony_genotype      treatment           cage_dt        ",
    "    ======  =======  ==================  ===================  =============  ====================  ",
    "",
    "     2001      F         2020-01-01          nNOS-cre:wt          NAIVE        2020-01-01:12:00    ",
    "                                                                               2020-01-21:12:00    ",
    "                                                                                                   ",
    "     2002      F         2020-01-01          nNOS-cre:wt          NAIVE        2020-01-01:12:00    ",
    "                                                                               2020-01-21:12:00    ",
    "                                                                                                   ",
    "     2003      M         2020-01-01          nNOS-cre:wt          NAIVE        2020-01-01:12:00    ",
    "                                                                               2020-01-21:12:00    ",
    "                                                                                                   ",
    "     2004      M         2020-01-01          nNOS-cre:wt          NAIVE        2020-01-01:12:00    ",
    "                                                                               2020-01-21:12:00    ",
    "                                                                                                   ",
    "",
    "+===============================================================================",
    "",
    "+===============================================================================",
    "",
    "",
    "    mice  :  ADD_DATA",
    "",
    "",
    "      ID        cage_con         cage_loc      ",
    "    ======  ===============  ================  ",
    "",
    "     2001     IVC:(GM500)      SAAA-0300101    ",
    "              IVC:(GM500)      SAAA-0300102    ",
    "                                               ",
    "     2002     IVC:(GM500)      SAAA-0300101    ",
    "              IVC:(GM500)      SAAA-0300102    ",
    "                                               ",
    "     2003     IVC:(GM500)      SAAA-0300101    ",
    "              IVC:(GM500)      SAAA-0300102    ",
    "                                               ",
    "     2004     IVC:(GM500)      SAAA-0300101    ",
    "              IVC:(GM500)      SAAA-0300102    ",
    "                                               ",
    "",
    "+===============================================================================",
    "",
    "",
    "+===============================================================================",
    "",
    "",
    "    mice  :  ADD_DATA",
    "",
    "",
    "      ID      wts_g          wts_g_dt        ",
    "    ======  =========  ====================  ",
    "",
    "     2001      21.0      2020-01-15:12:00    ",
    "               31.0      2020-03-15:12:00    ",
    "",
    "     2002      22.0      2020-01-15:12:00    ",
    "               32.0      2020-03-15:12:00    ",
    "                                             ",
    "     2003      23.0      2020-01-15:12:00    ",
    "               33.0      2020-03-15:12:00    ",
    "                                             ",
    "     2004      24.0      2020-01-15:12:00    ",
    "               34.0      2020-03-15:12:00    ",
    "                                             ",
    "",
    "+===============================================================================",
    "",
    "")
  local_insert_lines(rmd_path, contents, ins_line)

}


dt_find_add_create_dt3 <- function(rmd_path, ins_line) {

  # insert datatables : samples_CNS CREATE & CREATE TEMPLATE (fix-solutions-wts)
  contents <- c(
    "+===============================================================================",
    "",
    "",
    "    mice  :  CREATE",
    "",
    "",
    "      ID      sex          dob_dt          colony_genotype      treatment           cage_dt        ",
    "    ======  =======  ==================  ===================  =============  ====================  ",
    "",
    "     3001      F         2020-01-01          nNOS-cre:wt          NAIVE        2020-01-01:12:00    ",
    "                                                                               2020-01-21:12:00    ",
    "                                                                                                   ",
    "     3002      F         2020-01-01          nNOS-cre:wt          NAIVE        2020-01-01:12:00    ",
    "                                                                               2020-01-21:12:00    ",
    "                                                                                                   ",
    "     3003      M         2020-01-01          nNOS-cre:wt          NAIVE        2020-01-01:12:00    ",
    "                                                                               2020-01-21:12:00    ",
    "                                                                                                   ",
    "     3004      M         2020-01-01          nNOS-cre:wt          NAIVE        2020-01-01:12:00    ",
    "                                                                               2020-01-21:12:00    ",
    "                                                                                                   ",
    "",
    "+===============================================================================",
    "",
    "+===============================================================================",
    "",
    "",
    "    mice  :  ADD_DATA",
    "",
    "",
    "      ID        cage_con         cage_loc      ",
    "    ======  ===============  ================  ",
    "",
    "     3001     IVC:(GM500)      SAAA-0300101    ",
    "              IVC:(GM500)      SAAA-0300102    ",
    "                                               ",
    "     3002     IVC:(GM500)      SAAA-0300101    ",
    "              IVC:(GM500)      SAAA-0300102    ",
    "                                               ",
    "     3003     IVC:(GM500)      SAAA-0300101    ",
    "              IVC:(GM500)      SAAA-0300102    ",
    "                                               ",
    "     3004     IVC:(GM500)      SAAA-0300101    ",
    "              IVC:(GM500)      SAAA-0300102    ",
    "                                               ",
    "",
    "+===============================================================================",
    "",
    "",
    "+===============================================================================",
    "",
    "",
    "    mice  :  ADD_DATA",
    "",
    "",
    "      ID      wts_g          wts_g_dt        ",
    "    ======  =========  ====================  ",
    "",
    "     3001      21.0      2020-01-15:12:00    ",
    "               31.0      2020-03-15:12:00    ",
    "",
    "     3002      22.0      2020-01-15:12:00    ",
    "               32.0      2020-03-15:12:00    ",
    "                                             ",
    "     3003      23.0      2020-01-15:12:00    ",
    "               33.0      2020-03-15:12:00    ",
    "                                             ",
    "     3004      24.0      2020-01-15:12:00    ",
    "               34.0      2020-03-15:12:00    ",
    "                                             ",
    "",
    "+===============================================================================",
    "",
    "")
  local_insert_lines(rmd_path, contents, ins_line)

}





