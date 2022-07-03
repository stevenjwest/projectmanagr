#' Add a New Project Note
#'
#' This Function adds a single Project Note - consisting of one Note and
#' its corresponding Directory.
#'
#' @param projectNotePrefix The whole projectNotePrefix, including identifier and Major
#' Numbering, separated by ~.
#'
#' @param projectNoteName The name of the Project Note, a Title with all SPACES replaced
#' with - or _.
#'
#' @param projectNotePath The directory where the Project Note will be stored.  This
#' may be a Project Directory, or another Directory specified by the User.  MUST be a sub-directory
#' or lower inside a PROGRAMME Directory.
#'
#' @param selection List containing the Goal, Del, Task selected from the Project Doc, as well as other useful
#' information - lines of Task/Del/Goal, projectDoc path content of selection line.  See cursorSelection()
#' or userSelection().
#'
#' @param projectNoteTitle OPTIONAL title for the Project Note.  Default is to use projectNoteName and replace
#' all _ and - with SPACES.
#'
#' @param projNoteTemplate Template to use, as found in the `config/templates/` directory.  Default is
#' "Project-Note-Template.Rmd"
#'
#' @export
addProjectNote <- function( projectNotePrefix, projectNoteName, projectNotePath, selection,
                                  projectNoteTitle="", projNoteTemplate="Project-Note-Template.Rmd"  ) {

  cat( "\nprojectmanagr::addProjectNote():\n" )

  # Check projectNoteName contains NO SPACES:
  if( grepl("\\s+", projectNoteName) ) {
    stop( paste0("  projectNoteName contains a SPACE: ", projectNoteName) )
  }

  # Check projectNotePrefix contains NO SPACES:
  if( grepl("\\s+", projectNotePrefix) ) {
    stop( paste0("  projectNotePrefix contains a SPACE: ", projectNotePrefix) )
  }

  # Check projectTitle, and if blank, fill with projectName, replacing all "_" and "-" with spaces
  if( nchar(projectNoteTitle)==0 ) {
    projectNoteTitle = gsub("-", " ", gsub("_", " ", projectNoteName) )
  }

  # set projectDocPath
  projectDocPath <- selection[["projectDocPath"]]

  # Find programme DIR from projectDocPath:
  progPath <- findProgDir(projectDocPath)

  # Check projectNotePath is a sub-dir in a Programme DIR, which is a sub-dir to the root of an ORGANISATION:
    # run dirname TWICE as want to ensure projectNotePath is a sub-dir in a Programme!
  orgPath <- dirname( dirname(projectNotePath) )

  orgPath <- findOrgDir(orgPath)

  if(orgPath == "" ) {
    # the search reached the root of the filesystem without finding the Organisation files,
    # therefore, projectNotePath is not inside a PROGRAMME sub-dir!
    stop( paste0("  projectNotePath is not in a sub-dir of a PROGRAMME Directory: ", projectNotePath) )
  }
  # now, orgPath should be the root dir of the organisation

  # set confPath + tempPath:
  confPath <- paste0( orgPath, .Platform$file.sep, "config" )
  tempPath <- paste0( confPath, .Platform$file.sep, "templates" )

  projectNotePath <- normalizePath(projectNotePath)


  # Create Project Note DIR:
  projDirPath <- paste( projectNotePath, .Platform$file.sep, projectNotePrefix, sep="")
  done <- dir.create( projDirPath )

  if(!done) {
    stop( paste0("  DIR for Project Note could not be created: ", projDirPath) )
  }

  cat( "  Made Project Note DIR: ", projDirPath, "\n" )


  # read Simple project note template:
  templateFileConn <- file( paste( tempPath, .Platform$file.sep, projNoteTemplate, sep="") )
  templateContents <- readLines( templateFileConn )
  close(templateFileConn)

  # Create the new RMD DOCUMENT:
  projNotePath <- paste( projectNotePath, .Platform$file.sep, projectNotePrefix, "~_", projectNoteName, ".Rmd", sep="")
  done <- file.create( projNotePath )

  if(!done) {
    file.remove(projDirPath) # remove the project note dir
    stop( paste0("  Project Note could not be created: ", projNotePath) )
  }

  cat( "  Made Project Note: ", projNotePath, "\n" )


  # get creation time - use to write to SUMMARY:
  summaryBullet <- paste0("* ", as.character(file.info(projNotePath)[,5]) )


  # extract the Author value from the settings.yml file:
  #settingsFile = paste( confPath, .Platform$file.sep, "settings.yml", sep="" )
  #settings <- yaml::yaml.load( yaml::read_yaml( settingsFile ) )
  #authorValue <- settings[["Author"]]
  authorValue <- Sys.info()["user"] # use username as author instead

  # modify templateContents to include PREFIX and projectTitle
  templateContents <- gsub("{{PREFIX}}", projectNotePrefix, templateContents, fixed=TRUE)
  templateContents <- gsub("{{TITLE}}", projectNoteTitle, templateContents, fixed=TRUE)
  templateContents <- gsub("{{AUTHOR}}", authorValue, templateContents, fixed=TRUE)

  # add the current data storage path:
  #templateContents <- gsub("{{D ATASTORAGE}}", projectNotePrefix, templateContents, fixed=TRUE)
   # do not add the project note DIR here - the DATA STORAGE section will be filled out by volume functions

  ### replace the {{OBJECTIVES}} part of the template with the Objectives Template:

  # read objectives template:
  objectivesFileConn <- file( paste( tempPath, .Platform$file.sep, "Objectives.Rmd", sep="") )
  objectives <- readLines( objectivesFileConn )
  close(objectivesFileConn)

  # replace:
  templateContents <- replaceAndInsertVector("{{OBJECTIVES}}", objectives, templateContents)



  ### compute Project Source Doc RELATIVE LINK Plus GOAL/DEL/TASK:

  DocLink <- R.utils::getRelativePath(projectDocPath, relativeTo=projNotePath)
  DocLink <- substring(DocLink, first=4, last=nchar(DocLink)) # remove first `../`
    # DocLink <- paste( substring(DocLink, first=1, last=nchar(DocLink)-4), "/", sep="")
  # for now have left links as ".Rmd", but this needs to be ".html", or "/" in a rendered website!
  # Its set as ".Rmd" as ".Rmd" links can be navigated in RStudio!  CONVERT LINKS in update() function

  DocName <- basename(projectDocPath)
  DocName <- substring(DocName, first=1, last=nchar(DocName)-4)

  DocTitleLink <- paste( "[", DocName, "](", DocLink, ")", sep="" )

  # GOAL:
  goal <- substring(selection[["goal"]], first=4)
  goalTitle <- substring(goal,  first=(regexpr(":", goal)+2 ) )
  goalNum <- as.integer(  substring(goal,  first=5, last=(regexpr(":", goal)-1) )  )

  goalTag <- paste("#", gsub("[ ]|[_]", "-", gsub("[:]", "", tolower(goal) ) ), ")", sep="" )

  GoalTitleLink <- paste("* [", goal, "](", DocLink, goalTag, sep="")


  # DEL:
  del <- substring(selection[["deliverable"]], first=5)
  delTitle <- substring(del,  first=(regexpr(":", del)+2 ) )
  delNum <- as.integer(  substring(del,  first=12, last=(regexpr(":", del)-1) )  )

  delTag <- paste("#", gsub("[ ]|[_]", "-", gsub("[:]", "", tolower(del) ) ), ")", sep="" )

  DelTitleLink <- paste("    + [", del, "](", DocLink, delTag, sep="")

  # TASK:
  task <- substring(selection[["task"]], first=6)
  taskTitle <- substring(task,  first=(regexpr(":", task)+2 ) )
  taskNum <- as.integer(  substring(task,  first=5, last=(regexpr(":", task)-1) )  )

  taskTag <- paste("#", gsub("[ ]|[_]", "-", gsub("[:]", "", tolower(task) ) ), ")", sep="" )

  TaskTitleLink <- paste("        - [", task, "](", DocLink, taskTag, sep="")


  # create DocTitle - DocName plus the Gnum Dnum Tnum
  DocTitle <- paste( "## ", DocName, " : G", goalNum, " D", delNum, " T", taskNum, sep="")


  templateContents <- gsub("{{PROJECT_DOC_TITLE}}", DocTitle, templateContents, fixed=TRUE)

  templateContents <- gsub("{{PROJECT_DOC_LINK}}", DocTitleLink, templateContents, fixed=TRUE)

  templateContents <- gsub("{{PROJECT_DOC_LINK_GOAL}}", GoalTitleLink, templateContents, fixed=TRUE)
  templateContents <- gsub("{{PROJECT_DOC_LINK_DEL}}", DelTitleLink, templateContents, fixed=TRUE)
  templateContents <- gsub("{{PROJECT_DOC_LINK_TASK}}", TaskTitleLink, templateContents, fixed=TRUE)

  # insert the summaryBullet into SUMMARY_INFO field:
  templateContents <- gsub("{{SUMMARY_INFO}}", summaryBullet, templateContents, fixed=TRUE)


  # write to projFile
  fileConn <- file(projNotePath)
  writeLines(templateContents, fileConn)
  close(fileConn)

  cat( "  Written Goal Del Task to Project Note file: ", basename(projNotePath), "\n" )


  ### INSERT LINK FROM PROJECT NOTE INTO PROJECT DOC:

  # read Project Doc:
  projDocFileConn <- file( projectDocPath )
  projDocContents <- readLines( projDocFileConn )
  close(projDocFileConn)

  # create the projectNoteLink:
  NoteLink <- R.utils::getRelativePath(projNotePath, relativeTo=projectDocPath)
  NoteLink <- substring(NoteLink, first=4, last=nchar(NoteLink)) # remove first `../`
  noteName <- substring( basename(projNotePath), first=1, last=nchar(basename(projNotePath))-4)
  projectNoteLink <- paste("**[", noteName, "](", NoteLink, ")**",  sep="")
  #[BMS~314~_AVIL_42SNI_EdU_16wks](../BMS/BMS~314~_AVIL_42SNI_EdU_16wks/)

  # create the Vector, including Whitespace and Summary information:
  projectNoteLinkVector <- c( "", "", "", projectNoteLink, "", "", summaryBullet, "" )

  # compute place to insert the project note link:
    # get the line selected in the projectDoc - [["originalLine"]]
  line <- computeNextLine(selection[["originalLineNumber"]], projDocContents)

  # Insert projectNoteLinkVector to projDocContents:
  projDocContents <- c(projDocContents[1:(line-1)], projectNoteLinkVector, projDocContents[(line+1):length(projDocContents)])


  # write to projFile
  projDocFileConn <- file( projectDocPath )
  writeLines(projDocContents, projDocFileConn)
  close(projDocFileConn)

  cat( "  Written Project Note Link to Project Doc: ", basename(projectDocPath), "\n" )


}
