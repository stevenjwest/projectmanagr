
#' Rename a Project Note File and Update References
#'
#' Renames a project note file to a new name and optionally updates the title in
#' its content. Additionally, updates all references to the old project note
#' filename within the organization structure, ensuring consistent linking.
#'
#' @param projectNotePath Character string. The full path to the project note file
#' to be renamed.
#' @param newProjectNoteName Character string. The new name for the project note
#' file. Must not contain spaces.
#' @param newProjectNoteTitle Character string. The new title for the project note.
#' If empty, it will be auto-generated from \code{newProjectNoteName}. Defaults
#' to an empty string.
#' @param replaceLinksFileExtensions List. A list of file extensions where references
#' to the project note should be updated. Defaults to \code{list("Rmd")}.
#'
#' @details
#' This function performs the following tasks:
#' 1. Validates inputs, including ensuring no spaces in \code{newProjectNoteName}.
#' 2. Renames the project note file based on the specified \code{newProjectNoteName}.
#' 3. Updates the project note's title in its content if \code{newProjectNoteTitle}
#'   is provided.
#' 4. Updates all references to the old project note filename across the organization,
#'   restricted to the specified file extensions.
#'
#' The function automatically detects the organization root directory and other
#' configurations required to process the files.
#'
#' @return
#' The function does not return a value but modifies files in place. Changes include:
#' - Renaming the specified project note file.
#' - Updating the title in the renamed project note.
#' - Updating references to the renamed project note in other files.
#'
#' @examples
#' # Rename a project note and update references in Rmd files
#' rename_project_note(
#'   projectNotePath = "/path/to/project_note.Rmd",
#'   newProjectNoteName = "new_note_name",
#'   newProjectNoteTitle = "New Note Title",
#'   replaceLinksFileExtensions = list("Rmd")
#' )
#'
#' @seealso
#' - \code{\link{update_links_filenames}} for updating references to renamed files.
#' - \code{\link{find_org_directory}} for locating the organization root directory.
#' - \code{\link{get_settings_yml}} for retrieving configuration settings.
#'
#' @note
#' - Ensure that \code{newProjectNoteName} does not contain spaces.
#' - The function modifies files in place. It is recommended to backup the directory before use.
#' - The project note's title will be updated only if a matching title is found.
#'
#' @export
rename_project_note <- function( projectNotePath, newProjectNoteName,
                                 newProjectNoteTitle="",
                                 replaceLinksFileExtensions = list("Rmd") ) {


  cat( "\nprojectmanagr::rename_project_note():\n" )


  #### get org and settings ####

  projectNotePath <- fs::path_expand(projectNotePath)

  orgPath <- find_org_directory(projectNotePath)
  if(orgPath == "" ) { # only if orgPath not identified
    stop( paste0("  projectNotePath is not in a sub-dir of a PROGRAMME Directory: ", projectNotePath) )
  }
  # now, orgPath should be the root dir of the organisation

  # get config templates settings yml
  confPath <- get_config_dir(orgPath)
  tempPath <- get_template_dir(orgPath)
  settings <- get_settings_yml(orgPath)


  #### CHECK FOR ERRORS IN INPUT ####

  # check newProjectNoteName contains NO SPACES:
  if( grepl("\\s+", newProjectNoteName) ) {
    stop( paste0("  newProjectNoteName contains a SPACE: ",newProjectNoteName) )
  }

  # define the newProjectNoteTitle:
  if( nchar(newProjectNoteTitle)==0 ) {
    newProjectNoteTitle <- gsub("-", " ", gsub("_", " ", newProjectNoteName) )
  }

  fileType <- get_file_type(projectNotePath, settings)

  if( fileType == "UNKNOWN" ) {
    stop( paste0("  file path is not a project note: ",projectNotePath) )
  }

  if( fileType == "DOC" ) {
    stop( paste0("  file path is not a project note: ",projectNotePath) )
  }


  #### Set Instance Variables ####

  # get old project Note name:
  oldProjectNoteFileName <- basename(projectNotePath)

  # split oldProjectNoteFileName into PREFIX NAME and EXTENSION
  oldPrefix <- substr(oldProjectNoteFileName, 1,
                      regexpr(settings[["ProjectPrefixSep"]],
                              oldProjectNoteFileName, fixed=TRUE)-1 )
  oldProjectNoteFileNameExt <- tools::file_ext(oldProjectNoteFileName)
  oldName <- substr(oldProjectNoteFileName,
                    regexpr(settings[["ProjectPrefixSep"]],
                            oldProjectNoteFileName, fixed=TRUE)+(nchar(settings[["ProjectPrefixSep"]])),
                    regexpr(oldProjectNoteFileNameExt,
                            oldProjectNoteFileName, fixed=TRUE)-2) # first letter AND extension .

  # define new project Note path:
  newProjectNotePath <- paste0(dirname(projectNotePath), .Platform$file.sep,
                               oldPrefix, settings[["ProjectPrefixSep"]],
                               newProjectNoteName, ".", oldProjectNoteFileNameExt)

  # define newProjectNoteFileName
  newProjectNoteFileName <- basename(newProjectNotePath)

  # If projectTitle blank, fill with projectName, replacing all "_" and "-" with spaces
  if( nchar(newProjectNoteTitle)==0 ) {
    newProjectNoteTitle <- gsub("-", " ", gsub("_", " ", newProjectNoteName) )
  }

  # to identify the oldProjectNoteTitle in contents
  projectPrefixSepTitle <- gsub("-", " ", gsub("_", " ", settings[["ProjectPrefixSep"]]) )
  projectPrefixTitle <- paste0(oldPrefix, projectPrefixSepTitle)


  #### Rename project Note file ####

  # rename:
  done <- file.rename(projectNotePath, newProjectNotePath )

  if(done == TRUE) {
    cat( "  Renamed file from: ", oldProjectNoteFileName , " to: ", newProjectNoteFileName, "\n" )
  } else {
    stop( paste0("  Rename file unsuccessful: ", newProjectNoteFileName) )
  }


  #### Rename project Note Title ####

  fileConn <- file( newProjectNotePath )
  contents <- readLines( fileConn )
  close(fileConn)

  # find title line - via getProjNoteTitle()
  line <- grep( paste0("title: '", projectPrefixTitle), contents)[1] # get first instance of line beginning with title:
  oldTitle <- substr(contents[line],
                     regexpr(projectPrefixTitle,
                             contents[line], fixed=TRUE)+(nchar(projectPrefixTitle)),
                     nchar(contents[line]))
  oldTitle <- substr(oldTitle, 1, regexpr("'", oldTitle, fixed=TRUE)-1)
  contents[line] <- sub(oldTitle, newProjectNoteTitle, contents[line]) # replace old with new title

  # write project Note file to disk:
  fileConn <- file(newProjectNotePath)
  writeLines(contents, fileConn)
  close(fileConn)

  cat( "  Written new title to Rmd: ", newProjectNoteTitle, "\n" )


  #### update links ####

  # replace in links oldProjectNoteFileName with newProjectNoteName THROUGHOUT the Organisation:
  update_links_filenames(oldProjectNoteFileName, newProjectNoteName,
               orgPath, settings, replaceLinksFileExtensions)

}


#' Rename a Project Document and Update References
#'
#' Renames a project document to a new name and optionally updates the title
#' within its content. Additionally, updates references to the renamed document
#' across the organization structure for specified file types.
#'
#' @param projectDocPath Character string. The full path to the project document
#' file to be renamed.
#' @param newProjectDocName Character string. The new name for the project document
#' file. Must not contain spaces.
#' @param newProjectDocTitle Character string. The new title for the project document.
#' If not provided, it will be derived from \code{newProjectDocName}. Defaults to
#' an empty string.
#' @param replaceLinksFileExtensions List. A list of file extensions to search for
#' references to the project document. Defaults to \code{list("Rmd")}.
#'
#' @details
#' This function performs the following tasks:
#' 1. Validates the provided inputs, ensuring no spaces in \code{newProjectDocName}.
#' 2. Renames the specified project document to \code{newProjectDocName}.
#' 3. Updates the document's title in its content to \code{newProjectDocTitle}, if
#'   provided.
#' 4. Updates references to the renamed document in files across the organization,
#'   limited to the extensions specified in \code{replaceLinksFileExtensions}.
#'
#' The function assumes the project document resides within an organized directory
#' structure. It automatically identifies the root directory of the organization
#' and other configuration settings.
#'
#' @return
#' This function does not return a value. It modifies files in place by:
#' - Renaming the project document file.
#' - Updating the title in the renamed document.
#' - Updating references in other files across the organization.
#'
#' @examples
#' # Rename a project document and update references in Rmd files
#' rename_project_doc(
#'   projectDocPath = "/path/to/project_doc.Rmd",
#'   newProjectDocName = "new_doc_name",
#'   newProjectDocTitle = "New Document Title",
#'   replaceLinksFileExtensions = list("Rmd")
#' )
#'
#' @seealso
#' - \code{\link{update_links_filenames}} for updating references to renamed files.
#' - \code{\link{find_org_directory}} for locating the root directory of the organization.
#' - \code{\link{get_settings_yml}} for loading configuration settings.
#'
#' @note
#' - The \code{newProjectDocName} parameter must not contain spaces.
#' - This function modifies files directly on disk. Ensure backups are created before
#'   running.
#' - Only files with extensions specified in \code{replaceLinksFileExtensions} are
#'   updated for references.
#'
#' @export
rename_project_doc <- function( projectDocPath, newProjectDocName,
                                newProjectDocTitle="",
                                replaceLinksFileExtensions = list("Rmd")) {


  cat( "\nprojectmanagr::rename_project_doc():\n" )


  #### get org and settings ####

  projectDocPath <- fs::path_expand(projectDocPath)

  orgPath <- find_org_directory(projectDocPath)
  if(orgPath == "" ) { # only if orgPath not identified
    stop( paste0("  projectDocPath is not in a sub-dir of a PROGRAMME Directory: ", projectDocPath) )
  }
  # now, orgPath should be the root dir of the organisation

  # get config templates settings yml
  confPath <- get_config_dir(orgPath)
  tempPath <- get_template_dir(orgPath)
  settings <- get_settings_yml(orgPath)


  #### CHECK FOR ERRORS IN INPUT ####

  # check newProjectDocName contains NO SPACES:
  if( grepl("\\s+", newProjectDocName) ) {
    stop( paste0("  newProjectDocName contains a SPACE: ",newProjectDocName) )
  }

  # define the newProjectDocTitle:
  if( nchar(newProjectDocTitle)==0 ) {
    newProjectDocTitle <- gsub("-", " ", gsub("_", " ", newProjectDocName) )
  }

  fileType <- get_file_type(projectDocPath, settings)

  if( fileType == "UNKNOWN" ) {
    stop( paste0("  file path is not a project doc: ",projectDocPath) )
  }

  if( fileType == "NOTE" | fileType == "SUB" | fileType == "HEAD" ) {
    stop( paste0("  file path is not a project doc: ",projectDocPath) )
  }


  #### Set Instance Variables ####

  # get old project Doc name:
  oldProjectDocFileName <- basename(projectDocPath)

  # split oldProjectDocFileName into PREFIX NAME and EXTENSION
  oldPrefix <- substr(oldProjectDocFileName, 1,
                      regexpr(settings[["ProjectPrefixSep"]], oldProjectDocFileName, fixed=TRUE)-1 )
  oldProjectDocFileNameExt <- tools::file_ext(oldProjectDocFileName)
  oldName <- substr(oldProjectDocFileName,
                    regexpr(settings[["ProjectPrefixSep"]], oldProjectDocFileName, fixed=TRUE)+(nchar(settings[["ProjectPrefixSep"]])),
                    regexpr(oldProjectDocFileNameExt, oldProjectDocFileName, fixed=TRUE)-2) # first letter AND extension .

  # define new project Doc path:
  newProjectDocPath <- paste0(dirname(projectDocPath), .Platform$file.sep,
                               oldPrefix, settings[["ProjectPrefixSep"]],
                               newProjectDocName, ".", oldProjectDocFileNameExt)

  # define newProjectDocFileName
  newProjectDocFileName <- basename(newProjectDocPath)

  # If projectTitle blank, fill with projectName, replacing all "_" and "-" with spaces
  if( nchar(newProjectDocTitle)==0 ) {
    newProjectDocTitle <- gsub("-", " ", gsub("_", " ", newProjectDocName) )
  }

  # to identify the oldProjectDocTitle in contents
  projectPrefixSepTitle <- gsub("-", " ", gsub("_", " ", settings[["ProjectPrefixSep"]]) )
  projectPrefixTitle <- paste0(oldPrefix, projectPrefixSepTitle)


  #### Rename project Doc file ####

  # rename:
  done <- file.rename(projectDocPath, newProjectDocPath )

  if(done == TRUE) {
    cat( "  Renamed file from: ", oldProjectDocFileName , " to: ", newProjectDocFileName, "\n" )
  } else {
    stop( paste0("  Rename file unsuccessful: ", newProjectDocFileName) )
  }


  #### Rename project Doc Title ####

  contents <- read_file(newProjectDocPath)

  # find title line - via getProjDocTitle()
  line <- grep( paste0("title: '", projectPrefixTitle), contents)[1] # get first instance of line beginning with title:
  oldTitle <- substr(contents[line], regexpr(projectPrefixTitle, contents[line], fixed=TRUE)+(nchar(projectPrefixTitle)), nchar(contents[line]))
  oldTitle <- substr(oldTitle, 1, regexpr("'", oldTitle, fixed=TRUE)-1)
  contents[line] <- sub(oldTitle, newProjectDocTitle, contents[line]) # replace old with new title

  # write subDoc file to disk:
  write_file(contents, newProjectDocPath)

  cat( "  Written new title to Rmd: ", newProjectDocTitle, "\n" )


  #### update links ####

  # replace in links oldProjectDocFileName with newProjectDocName THROUGHOUT the Organisation:
  update_links_filenames(oldProjectDocFileName, newProjectDocName,
               orgPath, settings, replaceLinksFileExtensions)

}

#' Rename a Goal in a Project Document and Update References
#'
#' Renames a specific goal within a project document to a new name and updates
#' all references to the goal across the organization.
#'
#' @param goalSelection List. Contains information about the selected goal line
#'   in the project document.
#'   - \code{goalSelection$filePath}: The file path of the project document.
#'   - \code{goalSelection$goal}: The current header text of the goal.
#'   - \code{goalSelection$originalLine}: The original text of the selected line.
#'   - \code{goalSelection$originalLineNumber}: The line number of the goal in
#'     the project document.
#' @param newGoalName Character string. The new name for the goal header.
#'
#' @details
#' This function performs the following steps:
#' 1. Validates the provided goal selection to ensure it is within a valid project
#'    document and corresponds to a goal header.
#' 2. Updates the goal header in the specified project document.
#' 3. Updates all references to the renamed goal across files within the organisation.
#'
#' The function relies on a structured organizational directory and configuration
#' settings to locate and update references effectively.
#'
#' @return
#' This function does not return a value. It modifies files in place by:
#' - Updating the goal header in the project document.
#' - Updating references to the renamed goal in other files across the organization.
#'
#' @examples
#' # Rename a goal in a project document and update references
#' goalSelection <- list(
#'   filePath = "/path/to/project_doc.Rmd",
#'   goal = "### Goal: Improve User Experience",
#'   originalLine = "### Goal: Improve User Experience",
#'   originalLineNumber = 15
#' )
#' rename_project_doc_goal(goalSelection, "Enhance Accessibility")
#'
#' @seealso
#' - \code{\link{update_links}} for updating references to goals.
#' - \code{\link{get_goal_title}} for extracting goal titles.
#' - \code{\link{trim_goal_hash}} for processing goal headers.
#'
#' @note
#' - The \code{goalSelection} parameter must correspond to a valid goal in a
#'   project document.
#' - This function modifies files directly on disk. Ensure backups are created
#'   before running.
#'
#' @export
rename_project_doc_goal <- function(goalSelection, newGoalName) {

  cat( "\nprojectmanagr::rename_project_doc_goal():\n" )


  #### get org and settings ####

  projectDocPath <- fs::path_expand(goalSelection$filePath)

  orgPath <- find_org_directory(projectDocPath)
  if(orgPath == "" ) { # only if orgPath not identified
    stop( paste0("  projectDocPath is not in a sub-dir of a PROGRAMME Directory: ", projectDocPath) )
  }
  # now, orgPath should be the root dir of the organisation

  # get config templates settings yml
  confPath <- get_config_dir(orgPath)
  tempPath <- get_template_dir(orgPath)
  settings <- get_settings_yml(orgPath)


  #### CHECK FOR ERRORS IN INPUT ####

  # confirm the selected file is a project doc
  fileType <- get_file_type(projectDocPath, settings)
  if( fileType == "UNKNOWN" | fileType == "NOTE" | fileType == "SUB" | fileType == "HEAD" ) {
    stop( paste0("  file path is not a project doc: ",projectDocPath) )
  }

  # check the selected line is a goal
  if( (is.null(goalSelection[["goal"]])) ||
      (goalSelection[["goal"]] != goalSelection[["originalLine"]]) ) {
    stop( paste0("  selected project doc line is not a goal: ",projectDocPath) )
  }


  #### Set Instance Variables ####

  # get old project Doc name:

  oldgoalHeader <- goalSelection[["goal"]]
  oldgoalName <- get_goal_title(goalSelection[["goal"]], settings)
  oldgoalString <- trim_goal_hash(oldgoalHeader, settings)

  newgoalHeader <- paste(settings[["ProjectGoalHeader"]], newGoalName)
  newgoalString <- trim_goal_hash(newgoalHeader, settings)

  # to check for lines that link to the goal line:
  projectDocName <- basename(projectDocPath)
  oldgoalLinkSuffix <- paste0(projectDocName, '#',
                              gsub(' ', '-', tolower(paste(
                                settings[["ProjectGoalTitle"]],
                                oldgoalName)), fixed=TRUE),
                              ')')

  newgoalLinkSuffix <- paste0(projectDocName, '#',
                              gsub(' ', '-', tolower(paste(
                                settings[["ProjectGoalTitle"]],
                                newGoalName)), fixed=TRUE),
                              ')')


  #### Rename goal in Project Doc ####

  contents <- read_file(projectDocPath)

  # find title line - via getProjDocTitle()
  contents[goalSelection[["originalLineNumber"]]] <- newgoalHeader

  # write subDoc file to disk:
  write_file(contents, projectDocPath)

  cat( "  Written new goal to Project Doc: ",
       fs::path_rel(fs::path(projectDocPath), fs::path(orgPath)), "\n" )


  #### Update all links to goal ####

  update_links(oldgoalLinkSuffix, newgoalLinkSuffix, orgPath,
               settings, oldgoalString, newgoalString)


}



#' Rename a Project Doc Deliverable
#'
#' @param deliverableSelection Selection from projectmanagr Project Doc file,
#' made on the Deliverable to be renamed.
#'
#' @param newDeliverableName The new name to use for the selected Deliverable.
#'
#'@export
rename_project_doc_deliverable <- function(deliverableSelection, newDeliverableName) {


  cat( "\nprojectmanagr::rename_project_doc_deliverable():\n" )


  #### get org and settings ####

  projectDocPath <- fs::path_expand(deliverableSelection$filePath)

  orgPath <- find_org_directory(projectDocPath)
  if(orgPath == "" ) { # only if orgPath not identified
    stop( paste0("  projectDocPath is not in a sub-dir of a PROGRAMME Directory: ", projectDocPath) )
  }
  # now, orgPath should be the root dir of the organisation

  # get config templates settings yml
  confPath <- get_config_dir(orgPath)
  tempPath <- get_template_dir(orgPath)
  settings <- get_settings_yml(orgPath)


  #### CHECK FOR ERRORS IN INPUT ####

  # confirm the selected file is a project doc
  fileType <- get_file_type(projectDocPath, settings)
  if( fileType == "UNKNOWN" | fileType == "NOTE" | fileType == "SUB" | fileType == "HEAD" ) {
    stop( paste0("  file path is not a project doc: ",projectDocPath) )
  }

  # check the selected line is a deliverable
  if( (is.null(deliverableSelection[["deliverable"]])) ||
      (deliverableSelection[["deliverable"]] != deliverableSelection[["originalLine"]]) ) {
    stop( paste0("  selected project doc line is not a deliverable: ",projectDocPath) )
  }


  #### Set Instance Variables ####

  # get old project Doc name:

  oldDeliverableHeader <- deliverableSelection[["deliverable"]]
  oldDeliverableName <- get_deliverable_title(deliverableSelection[["deliverable"]], settings)
  oldDeliverableString <- trim_deliverable_hash(oldDeliverableHeader, settings)

  newDeliverableHeader <- paste(settings[["ProjectDeliverableHeader"]], newDeliverableName)
  newDeliverableString <- trim_deliverable_hash(newDeliverableHeader, settings)

  # to check for lines that link to the deliverable line:
  projectDocName <- basename(projectDocPath)
  oldDeliverableLinkSuffix <- paste0(projectDocName, '#',
                                 gsub(' ', '-', tolower(paste(
                                   settings[["ProjectDeliverableTitle"]],
                                   oldDeliverableName)), fixed=TRUE),
                                 ')')

  newDeliverableLinkSuffix <- paste0(projectDocName, '#',
                                     gsub(' ', '-', tolower(paste(
                                       settings[["ProjectDeliverableTitle"]],
                                       newDeliverableName)), fixed=TRUE),
                                     ')')


  #### Rename Deliverable in Project Doc ####

  contents <- read_file(projectDocPath)

  # find title line - via getProjDocTitle()
  contents[deliverableSelection[["originalLineNumber"]]] <- newDeliverableHeader

  # write subDoc file to disk:
  write_file(contents, projectDocPath)

  cat( "  Written new Deliverable to Project Doc: ", projectDocPath, "\n" )


  #### Update all links to Deliverable ####

  update_links(oldDeliverableLinkSuffix, newDeliverableLinkSuffix, orgPath,
               settings, oldDeliverableString, newDeliverableString)

}



#' Rename a Project Doc Task
#'
#' @param taskSelection Selection from projectmanagr Project Doc file, made on
#' the Task to be renamed.
#'
#' @param newTaskName The new name to use for the selected Task
#'
#'@export
rename_project_doc_task <- function(taskSelection, newTaskName) {


  cat( "\nprojectmanagr::rename_project_doc_task():\n" )


  #### get org and settings ####

  projectDocPath <- fs::path_expand(taskSelection$filePath)

  orgPath <- find_org_directory(projectDocPath)
  if(orgPath == "" ) { # only if orgPath not identified
    stop( paste0("  projectDocPath is not in a sub-dir of a PROGRAMME Directory: ", projectDocPath) )
  }
  # now, orgPath should be the root dir of the organisation

  # get config templates settings yml
  confPath <- get_config_dir(orgPath)
  tempPath <- get_template_dir(orgPath)
  settings <- get_settings_yml(orgPath)

  projectDocPrefix <- get_prefix(projectDocPath, settings)


  #### CHECK FOR ERRORS IN INPUT ####

  # confirm the selected file is a project doc
  fileType <- get_file_type(projectDocPath, settings)
  if( fileType == "UNKNOWN" | fileType == "NOTE" | fileType == "SUB" | fileType == "HEAD" ) {
    stop( paste0("  file path is not a project doc: ",projectDocPath) )
  }

  # check the selected line is a task
  if( (is.null(taskSelection[["task"]])) ||
      (taskSelection[["task"]] != taskSelection[["originalLine"]]) ) {
    stop( paste0("  selected project doc line is not a task: ",projectDocPath) )
  }


  #### Set Instance Variables ####

  # get old project Doc name:

  oldtaskHeader <- taskSelection[["task"]]
  oldtaskName <- get_task_title(taskSelection[["task"]], settings)
  oldtaskString <- trim_task_hash(oldtaskHeader, settings)

  newtaskHeader <- paste(settings[["ProjectTaskHeader"]], newTaskName)
  newtaskString <- trim_task_hash(newtaskHeader, settings)

  # to check for lines that link to the task line:
  projectDocName <- basename(projectDocPath)
  oldtaskLinkSuffix <- paste0(projectDocName, '#',
                                     gsub(' ', '-', tolower(paste(
                                       settings[["ProjectTaskTitle"]],
                                       oldtaskName)), fixed=TRUE),
                                     ')')

  newtaskLinkSuffix <- paste0(projectDocName, '#',
                                     gsub(' ', '-', tolower(paste(
                                       settings[["ProjectTaskTitle"]],
                                       newTaskName)), fixed=TRUE),
                                     ')')


  #### Rename task in Project Doc ####

  projectDocContents <- read_file(projectDocPath)

  # find title line - via getProjDocTitle()
  projectDocContents[taskSelection[["originalLineNumber"]]] <- newtaskHeader

  # write subDoc file to disk:
  write_file(projectDocContents, projectDocPath)

  cat( "  Written new task to Project Doc: ", projectDocPath, "\n" )


  #### Update all links to task ####

  # as need to update both the task link && GDT Header (which contains task string)
  # need to run a new function to update the whole GDT : from Old to New
   # updating the GDT TITLE & GDT TASK LINK LINES
  update_GDT_link_task(projectDocPath, projectDocContents, taskSelection,
                       newTaskName, settings)

  # ALSO run the update links in case task header is linked to in Org
  update_links(oldtaskLinkSuffix, newtaskLinkSuffix, orgPath,
               settings, oldtaskString, newtaskString)

}




#' Rename a Project File Section Header
#'
#' Section Headers defined in Markdown plaintext files, beginning with `#`. This
#' method ensures all links to a file header are appropriately updated across
#' the ProjectManagr Organisation.
#'
#' @param selection Selection from projectmanagr Project file, made on the
#' Header (starts with `#`) to be renamed.
#'
#' @param headerName The new name to use for the selected Header
#'
#'@export
rename_project_file_header <- function(selection, headerName) {


  cat( "\nprojectmanagr::rename_project_file_header():\n" )


  #### get org and settings ####

  orgPath <- find_org_directory(selection$filePath)
  if(orgPath == "" ) { # only if orgPath not identified
    stop( paste0("  Could not identify projectmanagr Organisation: ", selection$filePath) )
  }
  # now, orgPath should be the root dir of the organisation

  # get config templates settings yml
  confPath <- get_config_dir(orgPath)
  tempPath <- get_template_dir(orgPath)
  settings <- get_settings_yml(orgPath)


  #### CHECK FOR ERRORS IN INPUT ####


  # check the selected line is a header

  if( !(startsWith(selection[['originalLine']], '#')) ) {
    stop( paste0("  selected line is not a header: ",selection[['originalLine']]) )
  }


  #### Set Instance Variables ####

  # get old project Doc name:

  oldHeader <- selection[["originalLine"]]
  oldHeaderHash <- get_header_hash(selection[["originalLine"]])
  oldHeaderTitle <- trimws(get_header_title(selection[["originalLine"]]))

  # assumes headerName does not have leading ' '
  newHeader <- paste(oldHeaderHash, headerName)

  # to check for lines that link to the header line:
  projectFileName <- basename(selection$filePath)

  oldLinkSuffix <- paste0(projectFileName, '#',
                           gsub(' ', '-', tolower(paste(
                             oldHeaderTitle)), fixed=TRUE),
                           ')')

  newLinkSuffix <- paste0(projectFileName, '#',
                           gsub(' ', '-', tolower(paste(
                             headerName)), fixed=TRUE),
                           ')')


  #### Rename Header in Project File ####

  contents <- read_file(selection$filePath)

  # find title line - via getProjDocTitle()
  contents[selection[["originalLineNumber"]]] <- newHeader

  # write subDoc file to disk:
  write_file(contents, selection$filePath)

  cat( "  Written new Header to Project File: ", selection$filePath, "\n" )


  #### Update all links to Header ####

  update_links(oldLinkSuffix, newLinkSuffix, orgPath, settings,
               paste0(projectFileName, " : ", oldHeaderTitle),
               paste0(projectFileName, " : ", headerName) )

}
