#' Rename a Project Document Prefix
#'
#' Renames a Project Doc Prefix at projectDocPath with new prefix newProjectDocPrefix
#'
#' This also modifies all LINKS to this file throughout the Organisation - updating them to use the new
#' project Comp name and title.
#'
#' @param projectDocPath defines the path to the Project Doc - should point to the Project Doc Rmd file
#' @param openRmd Whether to open the newlt renamed Rmd in rstudio - TRUE by default.
#'
#'@export
renameProjectDocPrefix <- function( projectDocPath, newProjectDocPrefix, openRmd = TRUE  ) {


  cat( "\nprojectmanagr::renameProjectDocPrefix():\n" )

  ### EDITING THE PROJECT Comp: ###

  # Find Programme path
  progPath <- findProgDir(projectDocPath)

  if( progPath == "" ) {
    # findProgDir returns a blank character vector if projectDocPath is NOT in a PROGRAMME DIR
    stop( paste0("  projectDocPath is not in a PROGRAMME Directory: ", projectDocPath) )
  }

  # confirm this is a projectDoc - must be in progPath / PROJECTS
  if( paste0(progPath, .Platform$file.sep, "PROJECTS") != dirname(projectDocPath) ) {
    stop( paste0("  projectDocPath is not a Project Doc (must be in PROJECTS/ dir): ", projectDocPath) )
  }

  # identify the orgPath:
  orgPath <- findOrgDir(projectDocPath)

  # get old project Doc name:
  projectDocName <- basename(projectDocPath)


  ### FIRST - rename the project Comp file:

  # get Project Comp prefix:
  prefix <- getProjectPrefixFromName(projectDocName)

  # check project prefix
  check_project_prefix <- checkProjectPrefix(projectDocPath, newProjectDocPrefix)

  if(check_project_prefix != "") {
    stop( check_project_prefix )
  }


  # get name WITHOUT PREFIX
  newProjectDocName <- substr(projectDocName, regexpr("~_", projectDocName)+2, nchar(projectDocName))

  # define new project DOC path:
  newProjectDocPath <- paste0( dirname(projectDocPath), .Platform$file.sep, newProjectDocPrefix, "~_", newProjectDocName )

  # re-generate newProjectDocName with prefix and .Rmd suffix
  newProjectDocName <- basename(newProjectDocPath)

  # rename:
  done <- file.rename(projectDocPath, newProjectDocPath )

  if(done == TRUE) {
    cat( "  Renamed Project Doc file from: ", projectDocName , " to: ", newProjectDocName, "\n" )
  } else {
    stop( paste0("  Renamed Project Doc file unsuccessful: ", basename(newProjectDocPath) ) )
  }

  ### SECOND - open, update prefix
  fileConn <- file( newProjectDocPath )
  contents <- readLines( fileConn )
  close(fileConn)

  # find title line - FIRST line beginning with "title: '"
  line <- grep( paste0("title: '"), contents)

  # update prefix ONLY in the title
  contents[line] <- paste0("title: '",
                           newProjectDocPrefix,
                           "~ ",
                           substr(contents[line],
                                  regexpr( "~ ", contents[line]) + 2,
                                  nchar(contents[line]) )  )

  # AND replace every instance of old prefix with new prefix
  contents <- gsub(prefix, newProjectDocPrefix, contents)

  # write the file
  projNoteFileConn <- file(newProjectDocPath)
  writeLines(contents, projNoteFileConn)
  close(projNoteFileConn)

  cat( "  Updated Project Doc file prefix in Rmd \n" )


  # NEXT - rename the Project Doc DIR

  # define old & new project DIR path
  oldProjectDocDir <- paste0( dirname(projectDocPath), .Platform$file.sep, prefix )
  newProjectDocDir <- paste0( dirname(projectDocPath), .Platform$file.sep, newProjectDocPrefix )

  done <- file.rename(oldProjectDocDir, newProjectDocDir)

  if(done == TRUE) {
    cat( "  Renamed Project Doc DIR from: ", basename(oldProjectDocDir) , " to: ", basename(newProjectDocDir), "\n" )
  } else {
    stop( paste0("  Renamed Project Doc Dir unsuccessful: ", basename(newProjectDocDir) ) )
  }


  ### NEXT - replace projectDocName/Title with newProjectDocName/Title THROUGHOUT the Organisation:
  updateAllLinks( orgPath, projectDocName, newProjectDocName )

  cat( "  Updated links to Project Doc file: \n" )


  ### NEXT - rename all Project Notes below the newly renamed Project Doc - update their PREFIX too!
  file_list <- list.files(path = newProjectDocDir, pattern = "*.Rmd",
                          all.files = TRUE, recursive = FALSE, include.dirs = FALSE)

  # only keep files with the Prefix~_Name syntax
  file_list <- file_list[grepl("~_", file_list)]

  # AND that start with the old prefix
  file_list <- file_list[ startsWith(file_list, prefix)]

  cat( "  Updating Project Notes - ", length(file_list), " : \n" )

  for(fl in file_list) {
    # rename each component
    # form the current path
    projectNotePath <- paste0(newProjectDocDir, .Platform$file.sep, fl)
    cat( "    Project Note: ", basename(projectNotePath), "\n" )
    # form the new prefix
    newProjectNotePrefix <- sub(prefix, newProjectDocPrefix, getProjectPrefixFromName(fl) )
    updateProjectNotePrefix(projectNotePath, newProjectNotePrefix )
  }

  # FINALLY - re-open the file
  if( openRmd == TRUE) {
    rstudioapi::navigateToFile(newProjectDocPath)
  }

}


checkProjectPrefix <- function(projectPath, newProjectPrefix) {

  prefix <- getProjectPrefixFromName(basename(projectPath))

  projectNoteName <- basename(projectPath)

  returnString <- ""

  if( grepl("~", prefix) == FALSE ) {
    # if prefix doesnt contain a ~ the prefix MUST be a ProjectDoc prefix

    # CHECK the new prefix is consistent with the old one - check the CHARACTERS at start of prefix match
    if( (sub("^([[:alpha:]]*).*", "\\1", newProjectPrefix) == sub("^([[:alpha:]]*).*", "\\1", prefix)) == FALSE ) {
      returnString <- paste0("  newProjectDocPrefix not consistent with old prefix: ", newProjectPrefix, ' old prefix: ', prefix)
    }

    # CHECK the new prefix is AVAILABLE
    projectDocDir <- dirname(projectPath)
    projectFiles <- Sys.glob( paste0(projectDocDir, .Platform$file.sep, "*~_*", "*.Rmd") )
    projectFilesPrefix <-sub("\\~_.*", "", basename(projectFiles))

    # check newProjectPrefix is available
    if(any(projectFilesPrefix == newProjectPrefix) == TRUE ) {
      returnString <- paste0("  newProjectDocPrefix already in use: ", newProjectPrefix)
    }

  } else {

    # it must be a project note

    # CHECK this is not a SUBNOTE prefix - do not allow renaming of subnote prefixes in this method, these must remain
    # consistent with the HEADER NOTE
    if( regexpr("-", prefix) != -1 && # if prefix contains a -
        substring(prefix, regexpr("-", prefix)+1) != "00" ) { # AND it doesnt end with 00 (header note)
      #initialRename == TRUE ) { # AND this is the initialRename of files
      returnString <- ( paste0("  Project Note is a SubNote - cannot rename prefix as must remain consistent with HEADER Note Prefix: ", projectNoteName) )
    }

    # CHECK the new prefix is consistent with the old one - check the CHARACTERS at start of prefix match
    if( (sub("^([[:alpha:]]*).*", "\\1", newProjectPrefix) == sub("^([[:alpha:]]*).*", "\\1", prefix)) == FALSE ) {
      returnString <- ( paste0("  newProjectNotePrefix not consistent with old prefix: ", newProjectPrefix, ' old prefix: ', prefix) )
    }


    # CHECK the new prefix is AVAILABLE
    projectNotePir <- dirname(projectPath) # in directory containing projectNote
    projectFiles <- Sys.glob( paste0(projectNotePir, .Platform$file.sep, "*~_*", "*.Rmd") )
    projectFilesPrefix <-sub("\\~_.*", "", basename(projectFiles))
    # trim any -00 from all prefixes to check just the START and SIMPLE/HEAD components
    projFilePrefixStartSimple <- substr(projectFilesPrefix, 1,
                                        ifelse(regexpr("-", projectFilesPrefix) == -1,
                                               nchar(projectFilesPrefix),
                                               regexpr("-", projectFilesPrefix)-1) )
    projNotePrefixStartSimple <- substr(newProjectPrefix, 1,
                                        ifelse(regexpr("-", newProjectPrefix) == -1,
                                               nchar(newProjectPrefix),
                                               regexpr("-", newProjectPrefix)-1) )
    if(any(projFilePrefixStartSimple == projNotePrefixStartSimple) == TRUE ) {
      returnString <- ( paste0("  newProjectNotePrefix already in use - select another: ", newProjectPrefix) )
    }

    # check that the START of the new PREFIX is consistent with the START of prefixes in directory
    startPrefix <- substr(projectFilesPrefix, 1, regexpr("~", projectFilesPrefix)-1)
    startPrefixNew <- substr(newProjectPrefix, 1, regexpr("~", newProjectPrefix)-1)
    if(all(startPrefix == startPrefixNew) == FALSE) {
      # determine odd one out
      if( all(startPrefix != startPrefixNew) == TRUE) {
        # startPrefixNew is the odd one out!
        returnString <- ( paste0("  newProjectNotePrefix start does not match DIR Prefix: ", newProjectPrefix, " dir prefix start: ", startPrefix[1]))
      } else {
        # there is a problem with prefix names in the directory
        returnString <- ( paste0("  There is a problem with prefixes in selected project note parent : ", projectNotePir))
      }
    }

    # check the END of the new PREFIX is a NUMBER if simple prefix
    midPrefixNew <- substr(newProjectPrefix, regexpr("~", newProjectPrefix)+1, nchar(projectFilesPrefix))
    # remove -00 if header
    if( grepl("-00", midPrefixNew) == TRUE) {
      midPrefixNew <- substr(midPrefixNew, 1, nchar(midPrefixNew)-3)
    }
    if( is.na(suppressWarnings( as.integer(midPrefixNew))) == TRUE ) {
      returnString <- ( paste0("  newProjectNotePrefix numbering is not a number: ", midPrefixNew) )

    }

    # check that the Project Note prefix TYPE is consistent with OLD TYPE - HEADER NOTE or SIMPLE NOTE
    if( (endsWith(prefix, "-00")) != (endsWith(newProjectPrefix, "-00")) ) {
      # if prefix and newPrefix do not both end with/out -00, they are not of the same type
      returnString <- ( paste0("  Prefixes are not of same type - current : ", prefix, " new : ", newProjectPrefix) )
    }

  }

  returnString
}


#' Update a Project Note Prefix
#'
#' Renames a Project Note Prefix at projectNotePath with new prefix newProjectNotePrefix consistent with
#' Project Doc Prefix.
#'
#' This also modifies all LINKS to this file throughout the Organisation - updating them to use the new
#' project Note name, and renames any subnotes to new Header Note Prefix if required.
#'
#' @param projectNotePath defines the path to the Project Note - should point to the Project Note Rmd file
#' @param newProjectNotePrefix defines the new prefix - this must be consistent with the current project note prefix
#' and also be available!
#'
updateProjectNotePrefix <- function( projectNotePath, newProjectNotePrefix ) {


  cat( "\n  projectmanagr::updateProjectNotePrefix(): ", newProjectNotePrefix, " : ", basename(projectNotePath), "\n" )

  ### EDITING THE PROJECT Comp: ###

  # Find Programme path
  progPath <- findProgDir(projectNotePath)

  if( progPath == "" ) {
    # findProgDir returns a blank character vector if projectNotePath is NOT in a PROGRAMME DIR
    stop( paste0("  projectNotePath is not in a PROGRAMME Directory: ", projectNotePath) )
  }

  # identify the orgPath:
  orgPath <- findOrgDir(projectNotePath)

  # get old project Doc name:
  projectNoteName <- basename(projectNotePath)


  ### FIRST - rename the project Comp file:

  # get Project Comp prefix:
  prefix <- getProjectPrefixFromName(projectNoteName)

  # CHECK this is not a SUBNOTE prefix - do not allow renaming of subnote prefixes, these must remain
   # consistent with the HEADER NOTE
  if( regexpr("-", prefix) != -1 && # if prefix contains a -
      substring(prefix, regexpr("-", prefix)+1) != "00" ) { # AND it doesnt end with 00 (header note)
              #initialRename == TRUE ) { # AND this is the initialRename of files
    stop( paste0("  Project Note is a SubNote - cannot rename prefix as must remain consistent with HEADER Note Prefix: ", projectNoteName) )
  }

  # CHECK the new prefix is consistent with the old one - check the CHARACTERS at start of prefix match
  if( (sub("^([[:alpha:]]*).*", "\\1", newProjectNotePrefix) == sub("^([[:alpha:]]*).*", "\\1", prefix)) == FALSE ) {
    stop( paste0("  newProjectNotePrefix not consistent with old prefix: ", newProjectNotePrefix, ' old prefix: ', prefix) )
  }


  # CHECK the new prefix is AVAILABLE
  projectNotePir <- dirname(projectNotePath) # in directory containing projectNote
  projectFiles <- Sys.glob( paste0(projectNotePir, .Platform$file.sep, "*~_*", "*.Rmd") )
  projectFilesPrefix <-sub("\\~_.*", "", basename(projectFiles))

  # check newProjectPrefix is available
  if(any(projectFilesPrefix == newProjectNotePrefix) == TRUE ) {
    stop( paste0("  newProjectNotePrefix already in use: ", newProjectNotePrefix) )
  }

  # get name WITHOUT PREFIX
  newProjectNoteName <- substr(projectNoteName, regexpr("~_", projectNoteName)+2, nchar(projectNoteName))

  # define new project Note path:
  newProjectNotePath <- paste0( dirname(projectNotePath), .Platform$file.sep,
                                newProjectNotePrefix, "~_", newProjectNoteName )

  # re-generate newProjectNoteName with prefix and .Rmd suffix
  newProjectNoteName <- basename(newProjectNotePath)

  # rename:
  done <- file.rename(projectNotePath, newProjectNotePath )

  if(done == TRUE) {
    cat( "  Renamed Project Note file from: ", projectNoteName , " to: ", newProjectNoteName, "\n" )
  }
  else {
    stop( paste0("  Renamed Project Note file unsuccessful: ", basename(newProjectNotePath) ) )
  }

  ### SECOND - open, update prefix in TITLE
  fileConn <- file( newProjectNotePath )
  contents <- readLines( fileConn )
  close(fileConn)

  # find title line - FIRST line beginning with "title: '"
  line <- grep( paste0("title: '"), contents)

  # update prefix ONLY in the title
  contents[line] <- paste0("title: '",
                           newProjectNotePrefix,
                           "~ ",
                           substr(contents[line],
                                  regexpr( "~ ", contents[line]) + 2,
                                  nchar(contents[line]) )  )

  # AND replace every instance of old prefix with new prefix
  contents <- gsub(prefix, newProjectNotePrefix, contents)

  # write the file
  projNoteFileConn <- file(newProjectNotePath)
  writeLines(contents, projNoteFileConn)
  close(projNoteFileConn)


  # NEXT - rename the Project Note DIR

  # define old & new project DIR path
  oldProjectNoteDir <- paste0( dirname(projectNotePath), .Platform$file.sep, prefix )
  newProjectNoteDir <- paste0( dirname(projectNotePath), .Platform$file.sep, newProjectNotePrefix )

  done <- file.rename(oldProjectNoteDir, newProjectNoteDir)

  if(done == TRUE) {
    cat( "  Renamed Project Note DIR from: ", basename(oldProjectNoteDir) , " to: ", basename(newProjectNoteDir), "\n" )
  }
  else {
    # TODO undo the project note prefix rename changes..
    stop( paste0("  Renamed Project Note Dir unsuccessful: ", basename(newProjectNoteDir) ) )
  }


  ### NEXT - replace projectDocName/Title with newProjectDocName/Title THROUGHOUT the Organisation:
  updateAllLinks( orgPath, projectNoteName, newProjectNoteName )


  ### NEXT - rename any Project Notes below the newly renamed Project Note - update their PREFIX too!
   # this is only true if the project note is a HEADER NOTE

  # confirm this is a headernote - newProjectNotePrefix endsWith "-00"
  if( endsWith(newProjectNotePrefix, "-00") == TRUE ) {

    file_note_list <- list.files(path = newProjectNoteDir, pattern = "*.Rmd",
                            all.files = TRUE, recursive = FALSE, include.dirs = FALSE)

    # only keep files with the Prefix~_Name syntax
    file_note_list <- file_note_list[grepl("~_", file_note_list)]

    # AND that start with the old prefix
    file_note_list <- file_note_list[ startsWith(file_note_list, prefix)]

    for(fnl in file_note_list) {
      # rename each component
      # form the current path
      subNotePath <- paste0(newProjectNoteDir, .Platform$file.sep, fnl)
      # form the new prefix - TRIM the header suffix -00 from each
      newSubNotePrefix <- sub( substr(prefix,1, nchar(prefix)-3),
                               substr(newProjectNotePrefix, 1, nchar(newProjectNotePrefix)-3),
                               getProjectPrefixFromName(fnl) )
      updateSubNotePrefix(subNotePath,
                              newSubNotePrefix )
    }

  } # end if HEADER NOTE prefix

}



#' Rename a Project Note Prefix
#'
#' Renames a Project Note Prefix at projectNotePath with new prefix newProjectNotePrefix.
#'
#' Project note prefix is either a Simple Project Note (eg. SJW01~001) or a HEADER NOTE (eg. SJW01~002-00).
#'
#' The renaming here pertains to the Project Note NUMBERING - after the ~ and before any - (eg. SJW01~002-00
#' to SJW01~003-00).
#'
#' Renaming of SubNote prefixes inside a HEADER NOTE Group is conducted with renameSubNotePrefix function.
#'
#' This also modifies all LINKS to this file throughout the Organisation - updating them to use the new
#' project Note name, and renames any subnotes to new Header Note Prefix if required.
#'
#' @param projectNotePath defines the path to the Project Note - should point to the Project Note Rmd file
#' @param newProjectNotePrefix defines the new prefix - this must be consistent with the current project note prefix
#' and also be available!
#' @param openRmd Whether to open the newlt renamed Rmd in rstudio - TRUE by default.
#'
#'@export
renameProjectNotePrefix <- function( projectNotePath, newProjectNotePrefix, openRmd=TRUE ) {


  cat( "\nprojectmanagr::renameProjectNotePrefix():\n" )

  ### EDITING THE PROJECT Comp: ###

  # Find Programme path
  progPath <- findProgDir(projectNotePath)

  if( progPath == "" ) {
    # findProgDir returns a blank character vector if projectNotePath is NOT in a PROGRAMME DIR
    stop( paste0("  projectNotePath is not in a PROGRAMME Directory: ", projectNotePath) )
  }

  # identify the orgPath:
  orgPath <- findOrgDir(projectNotePath)

  # get old project Doc name:
  projectNoteName <- basename(projectNotePath)


  ### FIRST - rename the project Comp file:

  # get Project Comp prefix:
  prefix <- getProjectPrefixFromName(projectNoteName)

  # check project prefix
  check_project_prefix <- checkProjectPrefix(projectNotePath, newProjectNotePrefix)

  if(check_project_prefix != "") {
    stop( check_project_prefix )
  }

  # get name WITHOUT PREFIX
  newProjectNoteName <- substr(projectNoteName, regexpr("~_", projectNoteName)+2, nchar(projectNoteName))

  # define new project Note path:
  newProjectNotePath <- paste0( dirname(projectNotePath), .Platform$file.sep,
                                newProjectNotePrefix, "~_", newProjectNoteName )

  # re-generate newProjectNoteName with prefix and .Rmd suffix
  newProjectNoteName <- basename(newProjectNotePath)

  # rename:
  done <- file.rename(projectNotePath, newProjectNotePath )

  if(done == TRUE) {
    cat( "  Renamed Project Note file from: ", projectNoteName , " to: ", newProjectNoteName, "\n" )
  }
  else {
    stop( paste0("  Renamed Project Note file unsuccessful: ", basename(newProjectNotePath) ) )
  }

  ### SECOND - open, update prefix in TITLE
  fileConn <- file( newProjectNotePath )
  contents <- readLines( fileConn )
  close(fileConn)

  # find title line - FIRST line beginning with "title: '"
  line <- grep( paste0("title: '"), contents)

  # update prefix ONLY in the title
  contents[line] <- paste0("title: '",
                           newProjectNotePrefix,
                           "~ ",
                           substr(contents[line],
                                  regexpr( "~ ", contents[line]) + 2,
                                  nchar(contents[line]) )  )

  # write the file
  projNoteFileConn <- file(newProjectNotePath)
  writeLines(contents, projNoteFileConn)
  close(projNoteFileConn)


  # NEXT - rename the Project Note DIR

  # define old & new project DIR path
  oldProjectNoteDir <- paste0( dirname(projectNotePath), .Platform$file.sep, prefix )
  newProjectNoteDir <- paste0( dirname(projectNotePath), .Platform$file.sep, newProjectNotePrefix )

  done <- file.rename(oldProjectNoteDir, newProjectNoteDir)

  if(done == TRUE) {
    cat( "  Renamed Project Note DIR from: ", basename(oldProjectNoteDir) , " to: ", basename(newProjectNoteDir), "\n" )
  }
  else {
    # TODO undo the project note prefix rename changes..
    stop( paste0("  Renamed Project Note Dir unsuccessful: ", basename(newProjectNoteDir) ) )
  }


  ### NEXT - replace projectDocName/Title with newProjectDocName/Title THROUGHOUT the Organisation:
  updateAllLinks( orgPath, projectNoteName, newProjectNoteName )


  ### NEXT - rename any Project Notes below the newly renamed Project Note - update their PREFIX too!
  # this is only true if the project note is a HEADER NOTE

  # confirm this is a headernote - newProjectNotePrefix endsWith "-00"
  if( endsWith(newProjectNotePrefix, "-00") == TRUE ) {

    file_note_list <- list.files(path = newProjectNoteDir, pattern = "*.Rmd",
                                 all.files = TRUE, recursive = FALSE, include.dirs = FALSE)

    # only keep files with the Prefix~_Name syntax
    file_note_list <- file_note_list[grepl("~_", file_note_list)]

    # AND that start with the old prefix
    file_note_list <- file_note_list[ startsWith(file_note_list, prefix)]

    for(fnl in file_note_list) {
      # rename each component
      # form the current path
      subNotePath <- paste0(newProjectNoteDir, .Platform$file.sep, fnl)
      # form the new prefix - TRIM the header suffix -00 from each
      newSubNotePrefix <- sub( substr(prefix,1, nchar(prefix)-3),
                               substr(newProjectNotePrefix, 1, nchar(newProjectNotePrefix)-3),
                               getProjectPrefixFromName(fnl) )
      updateSubNotePrefix( subNotePath, newSubNotePrefix )
    }

  } # end if HEADER NOTE prefix

  # FINALLY - re-open the file if requested
  if(openRmd == TRUE) {
    rstudioapi::navigateToFile(newProjectNotePath)
  }

}



#' Update a Sub Note Prefix
#'
#' Renames a Project Sub Note Prefix at subNotePath with new prefix newSubNotePrefix consistent with
#' Project Header Note Prefix.
#'
#' This also modifies all LINKS to this file throughout the Organisation - updating them to use the new
#' sub Note name.
#'
#' @param subNotePath defines the path to the Sub Note - should point to the Sub Note Rmd file
#' @param newSubNotePrefix defines the new prefix - this must be consistent with the current sub note prefix
#' and also be available!
#'
updateSubNotePrefix <- function( subNotePath, newSubNotePrefix ) {


  cat( "\n  projectmanagr::updateSubNotePrefix(): ", newSubNotePrefix, " : ", basename(subNotePath), "\n" )

  ### EDITING THE PROJECT Comp: ###

  # Find Programme path
  progPath <- findProgDir(subNotePath)

  if( progPath == "" ) {
    # findProgDir returns a blank character vector if subNotePath is NOT in a PROGRAMME DIR
    stop( paste0("  subNotePath is not in a PROGRAMME Directory: ", subNotePath) )
  }

  # identify the orgPath:
  orgPath <- findOrgDir(subNotePath)

  # get old project Doc name:
  projectNoteName <- basename(subNotePath)


  ### FIRST - rename the project Comp file:

  # get Project Comp prefix:
  prefix <- getProjectPrefixFromName(projectNoteName)

  # CHECK this is a SUBNOTE prefix - do not allow renaming of subnote prefixes, these must remain
  # consistent with the HEADER NOTE
  if( regexpr("-", prefix) == -1 || # if prefix does not contain a -
      endsWith(prefix, "-00") ) { # OR it end with -00 (header note)
          #initialRename == TRUE ) { # AND this is the initialRename of files
    stop( paste0("  Project Note is not a SubNote - rename with renameProjectNotePrefix() function : ", projectNoteName) )
  }

  # CHECK the new prefix is consistent with the old one - check the CHARACTERS at start of prefix match
  if( (sub("^([[:alpha:]]*).*", "\\1", newSubNotePrefix) == sub("^([[:alpha:]]*).*", "\\1", prefix)) == FALSE ) {
    stop( paste0("  newSubNotePrefix not consistent with old prefix: ", newSubNotePrefix, ' old prefix: ', prefix) )
  }


  # CHECK the new prefix is AVAILABLE
  projectNotePir <- dirname(subNotePath) # in directory containing projectNote
  projectFiles <- Sys.glob( paste0(projectNotePir, .Platform$file.sep, "*~_*", "*.Rmd") )
  projectFilesPrefix <-sub("\\~_.*", "", basename(projectFiles))
  if(any(projectFilesPrefix == newSubNotePrefix) == TRUE ) {
    stop( paste0("  newSubNotePrefix already in use - select another: ", newSubNotePrefix) )
  }

  # get name WITHOUT PREFIX
  newProjectNoteName <- substr(projectNoteName, regexpr("~_", projectNoteName)+2, nchar(projectNoteName))

  # define new project Note path:
  newProjectNotePath <- paste0( dirname(subNotePath), .Platform$file.sep,
                                newSubNotePrefix, "~_", newProjectNoteName )

  # re-generate newProjectNoteName with prefix and .Rmd suffix
  newProjectNoteName <- basename(newProjectNotePath)

  # rename:
  done <- file.rename(subNotePath, newProjectNotePath )

  if(done == TRUE) {
    cat( "  Renamed Project Note file from: ", projectNoteName , " to: ", newProjectNoteName, "\n" )
  }
  else {
    stop( paste0("  Renamed Project Note file unsuccessful: ", basename(newProjectNotePath) ) )
  }

  ### SECOND - open, update prefix in TITLE
  fileConn <- file( newProjectNotePath )
  contents <- readLines( fileConn )
  close(fileConn)

  # find title line - FIRST line beginning with "title: '"
  line <- grep( paste0("title: '"), contents)

  # update prefix ONLY in the title
  contents[line] <- paste0("title: '",
                           newSubNotePrefix,
                           "~ ",
                           substr(contents[line],
                                  regexpr( "~ ", contents[line]) + 2,
                                  nchar(contents[line]) )  )

  # write the file
  projNoteFileConn <- file(newProjectNotePath)
  writeLines(contents, projNoteFileConn)
  close(projNoteFileConn)


  # NEXT - rename the Project Note DIR

  # define old & new project DIR path
  oldProjectNoteDir <- paste0( dirname(subNotePath), .Platform$file.sep, prefix )
  newProjectNoteDir <- paste0( dirname(subNotePath), .Platform$file.sep, newSubNotePrefix )

  done <- file.rename(oldProjectNoteDir, newProjectNoteDir)

  if(done == TRUE) {
    cat( "  Renamed Project Note DIR from: ", basename(oldProjectNoteDir) , " to: ", basename(newProjectNoteDir), "\n" )
  }
  else {
    # TODO undo the project note prefix rename changes..
    stop( paste0("  Renamed Project Note Dir unsuccessful: ", basename(newProjectNoteDir) ) )
  }


  ### NEXT - replace projectDocName/Title with newProjectDocName/Title THROUGHOUT the Organisation:
  updateAllLinks( orgPath, projectNoteName, newProjectNoteName )

}



#' Rename a Project Note Prefix
#'
#' Renames a Project Note Prefix at projectNotePath with new prefix newProjectNotePrefix.
#'
#' Project note prefix is either a Simple Project Note (eg. SJW01~001) or a HEADER NOTE (eg. SJW01~002-00).
#'
#' The renaming here pertains to the Project Note NUMBERING - after the ~ and before any -.
#'
#' Renaming of SubNote prefixes inside a HEADER NOTE Group is conducted with renameSubNotePrefix function.
#'
#' This also modifies all LINKS to this file throughout the Organisation - updating them to use the new
#' project Note name, and renames any subnotes to new Header Note Prefix if required.
#'
#' @param subNotePath defines the path to the Project Note - should point to the Project Note Rmd file
#' @param newSubNotePrefix defines the new prefix - this must be consistent with the current project note prefix
#' and also be available!
#' @param initialRename Set to FALSE if this method is called recursively.  Controls which file is opened when renaming
#' has finished - only the initial file that is renamed will be opened in rstudio. For internal recursive function calls
#' @param openRmd Whether to open the newlt renamed Rmd in rstudio - TRUE by default.
#'
#' @export
renameSubNotePrefix <- function( subNotePath, newSubNotePrefix, openRmd=TRUE ) {


  cat( "\nprojectmanagr::renameSubNotePrefix():\n" )

  ### EDITING THE PROJECT Comp: ###

  # Find Programme path
  progPath <- findProgDir(subNotePath)

  if( progPath == "" ) {
    # findProgDir returns a blank character vector if subNotePath is NOT in a PROGRAMME DIR
    stop( paste0("  subNotePath is not in a PROGRAMME Directory: ", subNotePath) )
  }

  # identify the orgPath:
  orgPath <- findOrgDir(subNotePath)

  # get old project Doc name:
  projectNoteName <- basename(subNotePath)


  ### FIRST - rename the project Comp file:

  # get Project Comp prefix:
  prefix <- getProjectPrefixFromName(projectNoteName)

  # CHECK this is a SUBNOTE prefix
  if( regexpr("-", prefix) == -1 || # if prefix doesnt contain a - (simple note)
      substring(prefix, regexpr("-", prefix)) == "-00" ) { # OR it does end with -00 (header note)
          #initialRename == TRUE ) { # AND this is the initialRename of files
    stop( paste0("  Project Note is not a SubNote - rename with renameProjectNotePrefix function: ", projectNoteName) )
  }

  # CHECK the new prefix is consistent with the old one - check the CHARACTERS at start of prefix match
  if( (sub("^([[:alpha:]]*).*", "\\1", newSubNotePrefix) == sub("^([[:alpha:]]*).*", "\\1", prefix)) == FALSE ) {
    stop( paste0("  newSubNotePrefix not consistent with old prefix: ", newSubNotePrefix, ' old prefix: ', prefix) )
  }


  # CHECK the new prefix is AVAILABLE
  projectNotePir <- dirname(subNotePath) # in directory containing projectNote
  projectFiles <- Sys.glob( paste0(projectNotePir, .Platform$file.sep, "*~_*", "*.Rmd") )
  projectFilesPrefix <-sub("\\~_.*", "", basename(projectFiles))

  # check newProjectPrefix is available
  if(any(projectFilesPrefix == newSubNotePrefix) == TRUE ) {
    stop( paste0("  newSubNotePrefix already in use: ", newSubNotePrefix) )
  }

  # get name WITHOUT PREFIX
  newProjectNoteName <- substr(projectNoteName, regexpr("~_", projectNoteName)+2, nchar(projectNoteName))

  # define new project Note path:
  newProjectNotePath <- paste0( dirname(subNotePath), .Platform$file.sep,
                                newSubNotePrefix, "~_", newProjectNoteName )

  # re-generate newProjectNoteName with prefix and .Rmd suffix
  newProjectNoteName <- basename(newProjectNotePath)

  # rename:
  done <- file.rename(subNotePath, newProjectNotePath )

  if(done == TRUE) {
    cat( "  Renamed Project Note file from: ", projectNoteName , " to: ", newProjectNoteName, "\n" )
  }
  else {
    stop( paste0("  Renamed Project Note file unsuccessful: ", basename(newProjectNotePath) ) )
  }

  ### SECOND - open, update prefix in TITLE
  fileConn <- file( newProjectNotePath )
  contents <- readLines( fileConn )
  close(fileConn)

  # find title line - FIRST line beginning with "title: '"
  line <- grep( paste0("title: '"), contents)

  # update prefix ONLY in the title
  contents[line] <- paste0("title: '",
                           newSubNotePrefix,
                           "~ ",
                           substr(contents[line],
                                  regexpr( "~ ", contents[line]) + 2,
                                  nchar(contents[line]) )  )

  # write the file
  projNoteFileConn <- file(newProjectNotePath)
  writeLines(contents, projNoteFileConn)
  close(projNoteFileConn)


  # NEXT - rename the Project Note DIR

  # define old & new project DIR path
  oldProjectNoteDir <- paste0( dirname(subNotePath), .Platform$file.sep, prefix )
  newProjectNoteDir <- paste0( dirname(subNotePath), .Platform$file.sep, newSubNotePrefix )

  done <- file.rename(oldProjectNoteDir, newProjectNoteDir)

  if(done == TRUE) {
    cat( "  Renamed Project Note DIR from: ", basename(oldProjectNoteDir) , " to: ", basename(newProjectNoteDir), "\n" )
  }
  else {
    # TODO undo the project note prefix rename changes..
    stop( paste0("  Renamed Project Note Dir unsuccessful: ", basename(newProjectNoteDir) ) )
  }


  ### NEXT - replace projectDocName/Title with newProjectDocName/Title THROUGHOUT the Organisation:
  updateAllLinks( orgPath, projectNoteName, newProjectNoteName )

  # FINALLY - re-open the file if requested
  if(openRmd == TRUE) {
    rstudioapi::navigateToFile(newProjectNotePath)
  }

}
