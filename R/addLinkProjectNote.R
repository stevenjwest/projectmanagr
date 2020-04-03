#' Add a New Link from a Project Doc to a Project Note
#'
#' This Function adds a new Link from an Existing (SINGLE or SUBNOTE) Project Note to a Project Doc.  The link
#' is made to a Project Doc GOAL/DEL/TASK, and this GOAL/DEL/TASK is added to the Project Notes' OBJECTIVES.
#'
#' @param projectNotePath The path to the Project Note.  This MUST be a sub-directory or lower inside a
#'   PROGRAMME Directory.
#'
#' @param selection List containing the Goal, Del, Task selected from the Project Doc, as well as other useful
#'   information - lines of Task/Del/Goal, projectDoc path, content of selection line.  See cursorSelection()
#'   or userSelection().
#'
#' @export
addLinkProjectNote <- function( projectNotePath, selection ) {

  cat( "\nprojectmanagr::addLinkProjectNote():\n" )

  # set projectDocPath
  projectDocPath <- selection[["projectDocPath"]]

  #  Determine projectNote PREFIX and TITLE:
  projectNotePrefix <- substring( basename(projectNotePath), first=1, last=regexpr("~_", basename(projectNotePath), fixed=TRUE)-1 )
  projectNoteTitle <- substring( basename(projectNotePath), first=regexpr("~_", basename(projectNotePath), fixed=TRUE)+2 )
  # Remove - and _ and remove file suffix:
  projectNoteTitle <- gsub("-", " ",  gsub("_", " ", substring(projectNoteTitle, first=1, last=nchar(projectNoteTitle)-4) )  )


  # Check projectNoteDir is a sub-dir in a Programme DIR, which itself is a sub-dir to the root of an ORGANISATION:
  # run dirname TWICE as want to ensure projectNoteDir is a sub-dir in a Programme!
  orgPath <- dirname( dirname(projectNotePath) )

  orgPath <- findOrgDir(orgPath)

  if(orgPath == "" ) {
    # the search reached the root of the filesystem without finding the Organisation files,
    # therefore, projectNoteDir is not inside a PROGRAMME sub-dir!  STOP:
    stop( cat("  projectNotePath is not in a sub-dir of a PROGRAMME Directory: ", projectNoteDir, "\n") )
  }
    # now, orgPath should be the root dir of the organisation

  # Check projectNotePrefix is NOT a HEADER NOTE (ending with "-00"):
  if( regexpr("-", projectNotePrefix) != -1 && substring(projectNotePrefix, regexpr("-", projectNotePrefix)+1) == "00" ) {
    # HEADER NOTE:  This method is not designed to deal with Header Notes - STOP:
    stop( cat("  projectNotePath is to a Header Note of a Group Project Note: ", projectNotePath, " Use addLinkProjectGroup() Function.\n") )
  }


  # set confPath:
  confPath <- paste(orgPath, .Platform$file.sep, "config" , sep="")

  # read Project Note:
  projNoteFileConn <- file( projectNotePath )
  projNoteContents <- readLines( projNoteFileConn )
  close(projNoteFileConn)


  # modify the Project Note to add the new Project Doc GOAL/DEL/TASK:

  # compute Project Source Doc RELATIVE LINK:
  DocLink <- R.utils::getRelativePath(projectDocPath, relativeTo=projectNotePath)
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
  fileConn <- file(projectNotePath)
  writeLines(projNoteContents, fileConn)
  close(fileConn)

  cat( "  Written Goal Del Task to Project Note file: ", basename(projectNotePath), "\n" )



  # read Project Doc:
  projDocFileConn <- file( projectDocPath )
  projDocContents <- readLines( projDocFileConn )
  close(projDocFileConn)

  # create the projectNoteLink:
  NoteLink <- R.utils::getRelativePath(projectNotePath, relativeTo=projectDocPath)
  NoteLink <- substring(NoteLink, first=4, last=nchar(NoteLink)) # remove first `../`
  projectNoteLink <- paste("**[", projectNotePrefix, "~ ", projectNoteTitle, "](", NoteLink, ")**",  sep="")
  #[BMS~314~ AVIL 42SNI EdU 16wks](../BMS/BMS~314~_AVIL_42SNI_EdU_16wks/)

  # create the Vector, including Whitespace and Summary information:
  projectNoteLinkVector <- c( "", "", "", projectNoteLink, "", "", "* Summary", "" )

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

  # First, determine if the note is SIMPLE, HEADER, or SUBNOTE from the Prefix:
  #if( regexpr("-", projectNotePrefix) == -1 ) {
    # SIMPLE NOTE:

    # form the Objectives List:
    # projectName is the name of the Project DOC!
   # objs <- list(projectName, goalNum, delNum, taskNum)
  #  names(objs) <- c("projectName", "goalNum", "delNum", "taskNum")

    # assign this OBJECTIVE to the NEXT available index:
   # nextIndex <- as.character(length(status[["PROJECT_NOTES"]][[ paste(projectNotePrefix, "~_", projectNoteName, sep="") ]][["OBJECTIVES"]])+1)
    # insert into the status list:
  #  status[["PROJECT_NOTES"]][[ paste(projectNotePrefix, "~_", projectNoteName, sep="") ]][["OBJECTIVES"]][[nextIndex]]<- objs

    # Write status list to the statusFile:
   # yaml::write_yaml( yaml::as.yaml(status), statusFile )

    #cat( "  Written PROJECT DOC OBJECTIVE to Status.yml file: ", statusFile, "\n" )

#  }
#  else {

 #   if( substring(projectNotePrefix, regexpr("-", projectNotePrefix)+1) == "00" ) {
      # HEADER NOTE:
      # here need to assign the Objectives to the Header note and EVERY SubNote
      # Will do this in another Function - addLinkProjectGroup()
      # so leave this are BLANK

#    }

#    else {
      # SUBNOTE:

      # here need to add the objectives from the ProjectDoc to the SubNote, inside its HeaderNote:

      # form the Objectives List:
      #objs <- list(projectName, goalNum, delNum, taskNum)
      #names(objs) <- c("projectName", "goalNum", "delNum", "taskNum")

      # to identify the correct path in `status.yml` will need to compute the HEADER NOTE Prefix and title:
      #line <- computeLineIndex("------", projNoteContents)

      #headerNoteLink <- computeNextLinkLine(line, projNoteContents)

      # headerNoteLink now has a line such as: ## [CP01~002 00~ New HEADER Note](../CP01~002-00~_New_HEADER_Note.Rmd)
      # Compute the Prefix and the Title - everything in the last (), after the last / and before the end ".Rmd"
      #headerNoteRelLink <- substring(headerNoteLink, regexpr("](", headerNoteLink, fixed=TRUE)+2, regexpr(".Rmd", headerNoteLink, fixed=TRUE)-1)

      #headerNoteFileName <- substring( headerNoteRelLink, regexpr("\\/[^\\/]*$", headerNoteRelLink)+1 )

      #cat("headerNoteFileName: ", headerNoteFileName)

      # assign this OBJECTIVE to the NEXT available index:
      #nextIndex <- as.character(length(status[["PROJECT_NOTES"]][[headerNoteFileName]][[ paste(projectNotePrefix, "~_", projectNoteName, sep="") ]][["OBJECTIVES"]])+1)
      # insert into the status list - headernote then subnote names:
     # status[["PROJECT_NOTES"]][[headerNoteFileName]][[ paste(projectNotePrefix, "~_", projectNoteName, sep="") ]][["OBJECTIVES"]][[nextIndex]]<- objs

      # Write status list to the statusFile:
      #yaml::write_yaml( yaml::as.yaml(status), statusFile )

      #cat( "  Written PROJECT DOC OBJECTIVE to Status.yml file: ", statusFile, "\n" )

    #}
  #}

}
