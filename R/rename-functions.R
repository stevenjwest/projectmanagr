#' Rename a Project Note
#'
#' Renames a Project Note at projectNotePath with file name newProjectNoteName,
#' and replaces title with newProjectNoteTitle if set.
#'
#' This also modifies all LINKS to this file throughout the Organisation -
#' updating them to use the new project Note name and title.
#'
#' NOTE - this DOES NOT modify the PREFIX for a Project Note.
#'
#' @param projectNotePath defines the path to the Project Note - should point
#' to the Project Note Rmd file
#' @param newProjectNoteName defines the NEW Project Note File name
#' @param newProjectNoteTitle defines the NEW Project Note Title - written to
#' its Rmd file.  By default this is the projectNotePath, with spaces replacing
#' "-" and "_".
#'
#'@export
rename_project_note <- function( projectNotePath, newProjectNoteName,
                                 newProjectNoteTitle="",
                                 replaceLinksFileExtensions = list("Rmd") ) {


  cat( "\nprojectmanagr::rename_project_note():\n" )


  #### get org and settings ####

  projectNotePath <- normalizePath(projectNotePath)

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


#' Rename a Project Doc
#'
#' Renames a Project Doc at projectDocPath with file name newProjectDocName,
#' and replaces title with newProjectDocTitle if set.
#'
#' This also modifies all LINKS to this file throughout the Organisation -
#' updating them to use the new project Doc name and title.
#'
#' Doc - this DOES NOT modify the PREFIX for a Project Doc.
#'
#' @param projectDocPath defines the path to the Project Doc - should point
#' to the Project Doc Rmd file
#' @param newProjectDocName defines the NEW Project Doc File name
#' @param newProjectDocTitle defines the NEW Project Doc Title - written to
#' its Rmd file.  By default this is the projectDocPath, with spaces replacing
#' "-" and "_".
#'
#'@export
rename_project_doc <- function( projectDocPath, newProjectDocName,
                                newProjectDocTitle="",
                                replaceLinksFileExtensions = list("Rmd")) {


  cat( "\nprojectmanagr::rename_project_doc():\n" )


  #### get org and settings ####

  projectDocPath <- normalizePath(projectDocPath)

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


#' Rename a Project Doc Goal
#'
#' @param goalSelection Selection from projectmanagr Project Doc file, made on
#' the Goal to be renamed.
#'
#' @param newGoalName The new name to use for the selected Goal.
#'
#'@export
rename_project_doc_goal <- function(goalSelection, newGoalName) {

  cat( "\nprojectmanagr::rename_project_doc_goal():\n" )


  #### get org and settings ####

  projectDocPath <- normalizePath(goalSelection$filePath)

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

  cat( "  Written new goal to Project Doc: ", projectDocPath, "\n" )


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

  projectDocPath <- normalizePath(deliverableSelection$filePath)

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

  projectDocPath <- normalizePath(taskSelection$filePath)

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

  contents <- read_file(projectDocPath)

  # find title line - via getProjDocTitle()
  contents[taskSelection[["originalLineNumber"]]] <- newtaskHeader

  # write subDoc file to disk:
  write_file(contents, projectDocPath)

  cat( "  Written new task to Project Doc: ", projectDocPath, "\n" )


  #### Update all links to task ####

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
