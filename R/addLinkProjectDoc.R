#' Add a New Link from a Project Doc Goal/Del/Task to another Project Doc
#'
#' This Function adds a new Link from one Project Doc (source) to another Project Doc (destination).  The link is made to a
#' SOURCE Project Doc GOAL/DEL/TASK, and this GOAL/DEL/TASK is added to the DESTINATIONS Project Docs' OBJECTIVES.  If the
#' Project Doc does not contain ANY objectives (by default they do not), an Objectives Section is added to the start of
#' the Document.
#'
#' @param projectDocPath The path to the DESTINATION Project Doc.  This MUST be in a sub-directory or lower inside a
#'   PROGRAMME Directory.  This MUST be an ABSOLUTE PATH.  This is the Project Doc that FULFILLS another Project Docs'
#'   Goal/Del/Task.
#'
#' @param selection List containing the Goal, Del, Task selected from the Project Doc, as well as other useful
#'   information - lines of Task/Del/Goal, SOURCE projectDoc path, content of selection line.  See cursorSelection()
#'   or userSelection().
#'
#' @export
addLinkProjectDoc <- function( projectDocPath, selection ) {

  cat( "\nprojectmanagr::addLinkProjectDoc():\n" )

  # set projectDocPath
  sourceProjectDocPath <- selection[["projectDocPath"]]

  destProjectDocPath <- projectDocPath # rename to make clear this is the DESINTATION Project Doc


  # Check destProjectDocPath is a sub-dir in a Programme DIR, which itself is a sub-dir to the root of an ORGANISATION:
  projPath <- dirname(destProjectDocPath)

  # first check the basename of projPath is "PROJECTS":

  if( basename(projPath) != "PROJECTS") {
    # destProjectDocPath is not in the PROJECTS DIR - so cannot be a ProjectDoc - STOP:
    stop( paste0("  projectDocPath is not in a sub-dir of a PROGRAMME Directory: ", destProjectDocPath) )
  }

  orgPath <- dirname( projPath )

  orgPath <- findOrgDir(orgPath)

  if(orgPath == "" ) {
    # the search reached the root of the filesystem without finding the Organisation files,
    # therefore, headerNoteDir is not inside a PROGRAMME sub-dir!  STOP:
    stop( paste0("  projectDocPath is not in a sub-dir of a PROGRAMME Directory: ", destProjectDocPath) )
  }
    # now, orgPath should be the root dir of the organisation


  # set confPath:
  confPath <- paste(orgPath, .Platform$file.sep, "config" , sep="")


  # Use current time as initial summary bullet - use to write to SUMMARY:
  summaryBullet <- paste0("* ", as.character(Sys.time()) )


  # read DESTINATION Project Doc:
  destProjDocFileConn <- file( destProjectDocPath )
  destProjDocContents <- readLines( destProjDocFileConn )
  close(destProjDocFileConn)

  # get TITLE from contents
  destProjectDocTitle <- getProjCompTitle(destProjDocContents)
  destProjectDocName <- substring( basename(destProjectDocPath), first=1, last=nchar(basename(destProjectDocPath))-4)
  # ACTUALLY better to link with doc file name!
   # avoids bug where old links made with old Titles would be missed if title is edited in doc in meantime..


  # modify the Project Note to add the new Project Doc GOAL/DEL/TASK:

  # compute Project Source Doc RELATIVE LINK:
  DocLink <- R.utils::getRelativePath(sourceProjectDocPath, relativeTo=destProjectDocPath)
  DocLink <- substring(DocLink, first=4, last=nchar(DocLink)) # remove first `../`
  # DocLink <- paste( substring(DocLink, first=1, last=nchar(DocLink)-4), "/", sep="")
  # for now have left links as ".Rmd", but this needs to be ".html", or "/" in a rendered website!
  # Its set as ".Rmd" as ".Rmd" links can be navigated in RStudio!

  DocName <- basename(sourceProjectDocPath)
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

  # insert objectivesContents into the first line that matches the string "------"
      # "------" (6 x '-') denotes the END of the objectives section
  line <- computeLineIndex("------", destProjDocContents)

  if(line == -1) {
    # if line is 01, then no "------" line found:
    #this is the FIRST SET of Objectives inserted into this Project Doc
    # so set line to 10:
    line <- 10 # this is where the end of the meta data is in a Project Doc

    # and, add an OBJECTIVES HEADER:
    # note need to use "---" at end of metadata for metadata to END!
    objectivesContents <- c("","","",
                            "# OBJECTIVES:","","","",
                            DocTitle,"","",DocTitleLink,"","",
                            GoalTitleLink,"",
                            DelTitleLink,"",
                            TaskTitleLink,"","",
                            summaryBullet,"","",
                            "------")

    # Insert projectNoteLinkVector to destProjDocContents:
    destProjDocContents <- c(destProjDocContents[1:(line)], objectivesContents, destProjDocContents[(line+1):length(destProjDocContents)])

  }
  else {
    # otherwise, insert the link where line indicates:
    objectivesContents <- c("----","","","",DocTitle,"","",DocTitleLink,"","",
                            GoalTitleLink,"",
                            DelTitleLink,"",
                            TaskTitleLink,"","",
                            summaryBullet,"","")

    # Insert projectNoteLinkVector to destProjDocContents:
    destProjDocContents <- c(destProjDocContents[1:(line-1)], objectivesContents, destProjDocContents[(line):length(destProjDocContents)])

  }


  # write to projFile
  destProjDocFileConn <- file(destProjectDocPath)
  writeLines(destProjDocContents, destProjDocFileConn)
  close(destProjDocFileConn)

  cat( "  Written Goal Del Task Link to Desintation Project Doc: ", basename(destProjectDocPath), "\n" )



  # read SOURCE Project Doc:
  projDocFileConn <- file( sourceProjectDocPath )
  projDocContents <- readLines( projDocFileConn )
  close(projDocFileConn)

  # create the projectNoteLink:
  NoteLink <- R.utils::getRelativePath(destProjectDocPath, relativeTo=sourceProjectDocPath)
  NoteLink <- substring(NoteLink, first=4, last=nchar(NoteLink)) # remove first `../`

  projectNoteLink <- paste("**[", destProjectDocName, "](", NoteLink, ")**",  sep="")

  # create the Vector, including Whitespace and Summary information:
  projectNoteLinkVector <- c( "", "", "", projectNoteLink, "", "",
                              summaryBullet, "")

  # compute place to insert the project note link:
  # get the line selected in the projectDoc - [["originalLine"]]
  line <- computeNextLine(selection[["originalLineNumber"]], projDocContents)

  # Insert projectNoteLinkVector to projDocContents:
  projDocContents <- c(projDocContents[1:(line-1)], projectNoteLinkVector, projDocContents[(line+1):length(projDocContents)])

  # write to projFile
  projDocFileConn <- file( sourceProjectDocPath )
  writeLines(projDocContents, projDocFileConn)
  close(projDocFileConn)

  cat( "  Written Destination Projec Doc Link to Source Project Doc: ", basename(sourceProjectDocPath), "\n" )



  # Write new Objectives of Destination Project Doc to the status.yml file:
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
  #  basename(dirname(dirname(sourceProjectDocPath))), .Platform$file.sep,
  #  basename(dirname(sourceProjectDocPath)), .Platform$file.sep,
  #  substring(basename(sourceProjectDocPath), first=1, last=(nchar(basename(sourceProjectDocPath))-4)  ), sep="")

   #destProjectDocName <- substring(destProjectDocTitle, 1, nchar(destProjectDocTitle)-4)

  # HEADER NOTE:

  # form the Objectives List:
  # projectName is the name of the Project DOC!
  #objs <- list(projectName, goalNum, delNum, taskNum)
  #names(objs) <- c("projectName", "goalNum", "delNum", "taskNum")

  # assign this OBJECTIVE to the NEXT available index:
  #nextIndex <- as.character(length(status[["PROJECTS"]][[ destProjectDocName ]][["OBJECTIVES"]])+1)
  # insert into the status list:
  #status[["PROJECTS"]][[ destProjectDocName ]][["OBJECTIVES"]][[nextIndex]]<- objs

  # Write status list to the statusFile:
  #yaml::write_yaml( yaml::as.yaml(status), statusFile )

  #cat( "  Written PROJECT DOC OBJECTIVE to Status.yml file: ", statusFile, "\n" )


}
