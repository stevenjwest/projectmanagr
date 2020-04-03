#' Add a New Link from a Project Doc Goal/Del/Task to a Group of Project Notes
#'
#' This Function adds a new Link from an Existing Group of Project Notes (including the Header Note and
#' all existing Sub Notes) to a Project Doc.  The link is made to a Project Doc GOAL/DEL/TASK, and this
#' GOAL/DEL/TASK is added to the Project Notes' OBJECTIVES.
#'
#' @param headerNotePath The path to the Header Note.  This MUST be in a sub-directory or lower inside a
#'   PROGRAMME Directory.  This MUST be an ABSOLUTE PATH.
#'
#' @param selection List containing the Goal, Del, Task selected from the Project Doc, as well as other useful
#'   information - lines of Task/Del/Goal, projectDoc path, content of selection line.  See cursorSelection()
#'   or userSelection().
#'
#' @export
addLinkProjectGroup <- function( headerNotePath, selection ) {

  cat( "\nprojectmanagr::addLinkProjectGroup():\n" )

  # set projectDocPath
  projectDocPath <- selection[["projectDocPath"]]

  #  Determine headerNote PREFIX and TITLE:
  projectNotePrefix <- substring( basename(headerNotePath), first=1, last=regexpr("~_", basename(headerNotePath), fixed=TRUE)-1 )
  projectNoteTitle <- substring( basename(headerNotePath), first=regexpr("~_", basename(headerNotePath), fixed=TRUE)+2 ) # still contains .Rmd!

  projectNoteName <- substring( gsub("_", " ", projectNoteTitle), first=1, last=nchar(projectNoteTitle)-4)


  # Check headerNoteDir is a sub-dir in a Programme DIR, which itself is a sub-dir to the root of an ORGANISATION:
  # run dirname TWICE as want to ensure headerNoteDir is a sub-dir in a Programme!
  orgPath <- dirname( dirname(headerNotePath) )

  orgPath <- findOrgDir(orgPath)

  if(orgPath == "" ) {
    # the search reached the root of the filesystem without finding the Organisation files,
    # therefore, headerNoteDir is not inside a PROGRAMME sub-dir!  STOP:
    stop( cat("  headerNotePath is not in a sub-dir of a PROGRAMME Directory: ", projectNoteDir, "\n") )
  }
    # now, orgPath should be the root dir of the organisation

  # Check headerNotePrefix IS a HEADER NOTE (ending with "-00"):
  if( regexpr("-", projectNotePrefix) == -1 || substring(projectNotePrefix, regexpr("-", projectNotePrefix)+1) != "00" ) {
    # Single OR SUB NOTE:  This method is not designed to deal with these Notes - STOP:
    stop( cat("  headerNotePath is to a Single Note or Sub Note of a Group Project Note: ", headerNotePath, " Use addLinkProjectNote() Function.\n") )
  }


  # set confPath:
  confPath <- paste(orgPath, .Platform$file.sep, "config" , sep="")

  # read Project Note:
  projNoteFileConn <- file( headerNotePath )
  projNoteContents <- readLines( projNoteFileConn )
  close(projNoteFileConn)


  # modify the Project Note to add the new Project Doc GOAL/DEL/TASK:

  # compute Project Source Doc RELATIVE LINK:
  DocLink <- R.utils::getRelativePath(projectDocPath, relativeTo=headerNotePath)
  DocLink <- substring(DocLink, first=4, last=nchar(DocLink)) # remove first `../`
  # DocLink <- paste( substring(DocLink, first=1, last=nchar(DocLink)-4), "/", sep="")
  # for now have left links as ".Rmd", but this needs to be ".html", or "/" in a rendered website!
  # Its set as ".Rmd" as ".Rmd" links can be navigated in RStudio!

  # NB Need to convert the .Rmd links to .html when WRITING the Organisation to a html site!

  DocName <- basename(projectDocPath)

  DocName <- gsub("-", " ",  gsub("_", " ", substring(DocName, first=1, last=nchar(DocName)-4) )  )

  DocTitleLink <- paste( "## [", DocName, "](", DocLink, ")", sep="" )


  # GOAL:
  goal <- substring(selection[["goal"]], first=4)
  goalTitle <- substring(goal,  first=(regexpr(":", goal)+2 ) )
  goalNum <- as.integer(  substring(goal,  first=5, last=(regexpr(":", goal)-1) )  )

  goalTag <- paste("#", gsub("[ ]|[_]", "-", gsub("[:]", "", tolower(goal) ) ), ")", sep="" )

  GoalTitleLink <- paste("# [", goal, "](", DocLink, goalTag, sep="")


  # DEL:
  del <- substring(selection[["deliverable"]], first=5)
  delTitle <- substring(del,  first=(regexpr(":", del)+2 ) )
  delNum <- as.integer(  substring(del,  first=12, last=(regexpr(":", del)-1) )  )

  delTag <- paste("#", gsub("[ ]|[_]", "-", gsub("[:]", "", tolower(del) ) ), ")", sep="" )

  DelTitleLink <- paste("## [", del, "](", DocLink, delTag, sep="")


  # TASK:
  task <- substring(selection[["task"]], first=6)
  taskTitle <- substring(task,  first=(regexpr(":", task)+2 ) )
  taskNum <- as.integer(  substring(task,  first=5, last=(regexpr(":", task)-1) )  )

  taskTag <- paste("#", gsub("[ ]|[_]", "-", gsub("[:]", "", tolower(task) ) ), ")", sep="" )

  TaskTitleLink <- paste("### [", task, "](", DocLink, taskTag, sep="")


  # form the objectivesContents:
  objectivesContents <- c("----","","","",DocTitleLink,"","","",
                          GoalTitleLink,"","","",
                          DelTitleLink,"","","",
                          TaskTitleLink,"","",
                          "* overview","","")

  # insert objectivesContents into the first line that matches the string "------"
      # "------" (6 x '-') denotes the END of the objectives section

  # compute place to insert the project note link:
  # get the line selected in the projectDoc - [["originalLine"]]
  line <- computeLineIndex("------", projNoteContents)

  # Insert projectNoteLinkVector to projNoteContents:
  projNoteContents <- c(projNoteContents[1:(line-1)], objectivesContents, projNoteContents[(line):length(projNoteContents)])




  # write to projFile
  fileConn <- file(headerNotePath)
  writeLines(projNoteContents, fileConn)
  close(fileConn)

  cat( "  Written Goal Del Task to Header Note file: ", basename(headerNotePath), "\n" )



  # read Project Doc:
  projDocFileConn <- file( projectDocPath )
  projDocContents <- readLines( projDocFileConn )
  close(projDocFileConn)

  # create the projectNoteLink:
  NoteLink <- R.utils::getRelativePath(headerNotePath, relativeTo=projectDocPath)
  NoteLink <- substring(NoteLink, first=4, last=nchar(NoteLink)) # remove first `../`
  projectNoteLink <- paste("**[", projectNotePrefix, "~ ", projectNoteName, "](", NoteLink, ")**",  sep="")
  #[BMS~314~ AVIL 42SNI EdU 16wks](../BMS/BMS~314~_AVIL_42SNI_EdU_16wks/)

  # create the Vector, including Whitespace and Summary information:
  projectNoteLinkVector <- c( "", "", "", projectNoteLink, "" )

  # compute place to insert the project note link:
  # get the line selected in the projectDoc - [["originalLine"]]
  line <- computeNextLine(selection[["originalLineNumber"]], projDocContents)

  # Insert projectNoteLinkVector to projDocContents:
  projDocContents <- c(projDocContents[1:(line-1)], projectNoteLinkVector, projDocContents[(line+1):length(projDocContents)])


  # write to projFile
  projDocFileConn <- file( projectDocPath )
  writeLines(projDocContents, projDocFileConn)
  close(projDocFileConn)

  cat( "  Written Header Note Link to Project Doc: ", basename(projectDocPath), "\n" )



  # Write PROJECT NOTE to the status.yml file:
  # NO LONGER USING STATUS.YML TO HOLD DATA ON ALL DOCS AND NOTES

  # Read the status.yml file first into a LIST:
  #statusFile = paste( confPath, .Platform$file.sep, "status.yml", sep="" )
  #status <- yaml::yaml.load( yaml::read_yaml( statusFile ) )

  # Then, FIND the projectName [paste(projectNotePrefix, "~_", projectNoteName, sep="") ] under PROJECT_NOTES
  # And then ADD to the objectives list of the Project Note entry:

  # will need to do this differently for SIMPLE NOTES, HEADER NOTES, and SUBNOTES

  # the projectName denotes the path to the Project DOCUMENT ->
  # extract FULLY QUALIFIED PROJECT DOC NAME - from root of ORG - including the Programme, "PROJECTS/" and the ProjectDoc Name:
  #projectName <- paste(
  #  basename(dirname(dirname(projectDocPath))), .Platform$file.sep,
  #  basename(dirname(projectDocPath)), .Platform$file.sep,
  #  substring(basename(projectDocPath), first=1, last=(nchar(basename(projectDocPath))-4)  ), sep="")

  #projectNoteName <- substring(projectNoteTitle, 1, nchar(projectNoteTitle)-4)

  # HEADER NOTE:

  # form the Objectives List:
  # projectName is the name of the Project DOC!
  #objs <- list(projectName, goalNum, delNum, taskNum)
  #names(objs) <- c("projectName", "goalNum", "delNum", "taskNum")

  # assign this OBJECTIVE to the NEXT available index:
  #nextIndex <- as.character(length(status[["PROJECT_NOTES"]][[ paste(projectNotePrefix, "~_", projectNoteName, sep="") ]][["OBJECTIVES"]])+1)
  # insert into the status list:
  #status[["PROJECT_NOTES"]][[ paste(projectNotePrefix, "~_", projectNoteName, sep="") ]][["OBJECTIVES"]][[nextIndex]]<- objs

  # Write status list to the statusFile:
  #yaml::write_yaml( yaml::as.yaml(status), statusFile )

  #cat( "  Written PROJECT DOC OBJECTIVE to Status.yml file: ", statusFile, "\n" )


  # Now, need to loop through EVERY SubNote, and write to the file as above:

  # use a separate function for this:

  # RE-MAKE the selection object - select the line in ProjectDoc where the HEADER link is inserted!
  selection <- projectmanagr::userSelection(projectDocPath, (line+3) )

  addLinkToAllSubNotes(headerNotePath, selection)


}
