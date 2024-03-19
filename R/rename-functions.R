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
                                 newProjectNoteTitle="", replaceLinksFileExtensions = list("Rmd") ) {


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
                      regexpr(settings[["ProjectPrefixSep"]], oldProjectNoteFileName, fixed=TRUE)-1 )
  oldProjectNoteFileNameExt <- tools::file_ext(oldProjectNoteFileName)
  oldName <- substr(oldProjectNoteFileName,
                    regexpr(settings[["ProjectPrefixSep"]], oldProjectNoteFileName, fixed=TRUE)+(nchar(settings[["ProjectPrefixSep"]])),
                    regexpr(oldProjectNoteFileNameExt, oldProjectNoteFileName, fixed=TRUE)-2) # first letter AND extension .

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
  oldTitle <- substr(contents[line], regexpr(projectPrefixTitle, contents[line], fixed=TRUE)+(nchar(projectPrefixTitle)), nchar(contents[line]))
  oldTitle <- substr(oldTitle, 1, regexpr("'", oldTitle, fixed=TRUE)-1)
  contents[line] <- sub(oldTitle, newProjectNoteTitle, contents[line]) # replace old with new title

  # write subNote file to disk:
  fileConn <- file(newProjectNotePath)
  writeLines(contents, fileConn)
  close(fileConn)

  cat( "  Written new title to Rmd: ", newProjectNoteTitle, "\n" )


  #### update links ####

  # replace in links oldProjectNoteFileName with newProjectNoteName THROUGHOUT the Organisation:
  update_links(oldProjectNoteFileName, newProjectNoteName,
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
                                newProjectDocTitle="", replaceLinksFileExtensions = list("Rmd")) {


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

  fileConn <- file( newProjectDocPath )
  contents <- readLines( fileConn )
  close(fileConn)

  # find title line - via getProjDocTitle()
  line <- grep( paste0("title: '", projectPrefixTitle), contents)[1] # get first instance of line beginning with title:
  oldTitle <- substr(contents[line], regexpr(projectPrefixTitle, contents[line], fixed=TRUE)+(nchar(projectPrefixTitle)), nchar(contents[line]))
  oldTitle <- substr(oldTitle, 1, regexpr("'", oldTitle, fixed=TRUE)-1)
  contents[line] <- sub(oldTitle, newProjectDocTitle, contents[line]) # replace old with new title

  # write subDoc file to disk:
  fileConn <- file(newProjectDocPath)
  writeLines(contents, fileConn)
  close(fileConn)

  cat( "  Written new title to Rmd: ", newProjectDocTitle, "\n" )


  #### update links ####

  # replace in links oldProjectDocFileName with newProjectDocName THROUGHOUT the Organisation:
  update_links(oldProjectDocFileName, newProjectDocName,
               orgPath, settings, replaceLinksFileExtensions)

}


#' Rename a Project Doc Goal/Deliverable/Task
#'
#' TODO
#'
#'@export
rename_project_doc_gdt <- function() {


}
