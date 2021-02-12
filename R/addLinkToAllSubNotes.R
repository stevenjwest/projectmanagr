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
#'   or userSelection().  The selection MUST be made on the line containing the link to the headerNote!
#'
#'  @param summaryBullet contains the bullet to insert as initial summary when adding a link.
#'
addLinkToAllSubNotes <- function( headerNotePath, selection, summaryBullet ) {

  #cat( "\nprojectmanagr::addLinkToAllSubNotes():\n" )

  # set projectDocPath
  projectDocPath <- selection[["projectDocPath"]]

  #cat( "    projectDocPath:", projectDocPath, "\n" )

  # Find programme DIR from projectDocPath:
  progPath <- findProgDir(projectDocPath)


  #  Determine headerNote PREFIX and TITLE:
  projectNotePrefix <- substring( basename(headerNotePath), first=1, last=regexpr("~_", basename(headerNotePath), fixed=TRUE)-1 )
  projectNoteTitle <- substring( basename(headerNotePath), first=regexpr("~_", basename(headerNotePath), fixed=TRUE)+2 ) # still contains .Rmd!


  # Check headerNoteDir is a sub-dir in a Programme DIR, which itself is a sub-dir to the root of an ORGANISATION:
  # run dirname TWICE as want to ensure headerNoteDir is a sub-dir in a Programme!
  orgPath <- dirname( dirname(headerNotePath) )

  orgPath <- findOrgDir(orgPath)

  if(orgPath == "" ) {
    # the search reached the root of the filesystem without finding the Organisation files,
    # therefore, headerNoteDir is not inside a PROGRAMME sub-dir!  STOP:
    stop( paste0("  headerNotePath is not in a sub-dir of a PROGRAMME Directory: ", projectNoteDir) )
  }
  # now, orgPath should be the root dir of the organisation

  # Check headerNotePrefix IS a HEADER NOTE (ending with "-00"):
  if( regexpr("-", projectNotePrefix) == -1 || substring(projectNotePrefix, regexpr("-", projectNotePrefix)+1) != "00" ) {
    # Single OR SUB NOTE:  This method is not designed to deal with these Notes - STOP:
    stop( paste0("  headerNotePath is to a Single Note or Sub Note of a Group Project Note: ", headerNotePath, " Use addLinkProjectNote() Function.") )
  }


  # set confPath:
  confPath <- paste(orgPath, .Platform$file.sep, "config" , sep="")


  # Use current time as initial summary bullet - use to write to SUMMARY:
  #summaryBullet <- paste0("* ", as.character( Sys.time() ) ) # now pass this in from header note - addLinkProjectGroup


  # Identify each Sub Note:

  subNotes <- listSubNotePaths(headerNotePath)

  for(i in 1:length(subNotes) ) {

    headerNotePath <- subNotes[i]

    cat("SubNote: ", i, " name: ", headerNotePath, "\n")

    subNotePrefix <- substring( basename(headerNotePath), first=1, last=regexpr("~_", basename(headerNotePath), fixed=TRUE)-1 )
    subNoteTitle <- substring( basename(headerNotePath), first=regexpr("~_", basename(headerNotePath), fixed=TRUE)+2 ) # still contains .Rmd!

    subNoteName <- substring( gsub("_", " ", subNoteTitle), first=1, last=nchar(subNoteTitle)-4)

    # read Sub Note:
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

    # form the objectivesContents:
    objectivesContents <- c("----","","","",DocTitle,"","",DocTitleLink,"","",
                            GoalTitleLink,"",
                            DelTitleLink,"",
                            TaskTitleLink,"","",
                            summaryBullet,"","")

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

    cat( "    Written Goal Del Task to Sub Note file: ", basename(headerNotePath), "\n" )


    ### INSERT LINK FROM PROJECT NOTE INTO PROJECT DOC:

    # read Project Doc:
    projDocFileConn <- file( projectDocPath )
    projDocContents <- readLines( projDocFileConn )
    close(projDocFileConn)

    # create the projectNoteLink:
    NoteLink <- R.utils::getRelativePath(headerNotePath, relativeTo=projectDocPath)
    NoteLink <- substring(NoteLink, first=4, last=nchar(NoteLink)) # remove first `../`
    projectNoteLink <- paste("*[", subNotePrefix, "~ ", subNoteName, "](", NoteLink, ")*",  sep="")
    #[BMS~314~ AVIL 42SNI EdU 16wks](../BMS/BMS~314~_AVIL_42SNI_EdU_16wks/)

    # create the Vector, including Whitespace and Summary information:
    projectNoteLinkVector <- c( "", "", "", projectNoteLink, "",
                                summaryBullet, "" )

    # compute place to insert the project note link:
    # get the line selected in the projectDoc - [["originalLine"]]
    line <- computeNextSubNoteLine(selection[["originalLineNumber"]], projDocContents)

    # Insert projectNoteLinkVector to projDocContents:
    projDocContents <- c(projDocContents[1:(line-1)], projectNoteLinkVector, projDocContents[(line+1):length(projDocContents)])


    # write to projFile
    projDocFileConn <- file( projectDocPath )
    writeLines(projDocContents, projDocFileConn)
    close(projDocFileConn)

    cat( "    Written Sub Note Link to Project Doc: ", basename(projectDocPath), "\n" )



    ### WRITE PROJECT NOTE TO PROGRAMME INDEX FILE:  NOT USED

    # read Programme Index File:
    #progIndexPath = paste(progPath, .Platform$file.sep, basename(progPath), "_index.Rmd", sep="")
    #progIndexFileConn <- file( progIndexPath )
    #progIndexContents <- readLines( progIndexFileConn )
    #close(progIndexFileConn)


    # create the projIndexLink:
    #NoteLink <- R.utils::getRelativePath(headerNotePath, relativeTo=progIndexPath)
    #NoteLink <- substring(NoteLink, first=4, last=nchar(NoteLink)) # remove first `../`
    #projIndexLink <- paste("* [", subNoteTitle, "](", NoteLink, ")",  sep="")

    # create the Vector, including Whitespace and Summary information:
    #projIndexLinkVector <- c( "", "", "", projIndexLink, "" )

    # compute place to insert the project doc link:
    # First get the line index containing containing the projectDoc Name
    #lineProg <- grepLineIndex(basename(projectDocPath), progIndexContents)

    # Then get the NEXT line that starts with ##
    #lineProg <- computeNextLine(lineProg, progIndexContents)

    # Insert projIndexLinkVector to progIndexContents:
    #progIndexContents <- c(progIndexContents[1:(lineProg-1)], projIndexLinkVector, progIndexContents[(lineProg+1):length(progIndexContents)])


    # write to progIndexPath
    #progIndexFileConn <- file( progIndexPath )
    #writeLines(progIndexContents, progIndexFileConn)
    #close(progIndexFileConn)

    #cat( "  Written Project Note to Programme File: ", basename(progIndexPath), "\n" )






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
    #subNoteName <- substring(subNoteTitle, 1, nchar(subNoteTitle)-4)

    # SUB NOTE:

    # form the Objectives List:
    # projectName is the name of the Project DOC!
    #objs <- list(projectName, goalNum, delNum, taskNum)
    #names(objs) <- c("projectName", "goalNum", "delNum", "taskNum")

    # assign this OBJECTIVE to the NEXT available index:
    #nextIndex <- as.character(length(status[["PROJECT_NOTES"]][[ paste(projectNotePrefix, "~_", projectNoteName, sep="") ]][[ paste(subNotePrefix, "~_", subNoteName, sep="") ]][["OBJECTIVES"]])+1)
    # insert into the status list:
    #status[["PROJECT_NOTES"]][[ paste(projectNotePrefix, "~_", projectNoteName, sep="") ]][[ paste(subNotePrefix, "~_", subNoteName, sep="") ]][["OBJECTIVES"]][[nextIndex]]<- objs

    # Write status list to the statusFile:
    #yaml::write_yaml( yaml::as.yaml(status), statusFile )

    #cat( "    Written PROJECT DOC OBJECTIVE to Status.yml file: ", statusFile, "\n" )

  }


}
