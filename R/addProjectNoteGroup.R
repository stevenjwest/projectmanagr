#' Add a New Group of Project Notes
#'
#' This Function adds a Project Note Group - consisting of one Header
#' Note, and one SubNote INSIDE the Header Note Dir.  This can be expanded
#' by adding further SubNotes - useful for Optimisation and Experimental Repeats.
#'
#' projectNoteName - the name of the Project HEADER Note, a Title with all SPACES replaced
#' with - or _.
#'
#' projectNotePrefix - the whole projectNotePrefix, including identifier and Major
#' Numbering, separated by ~.  NB Do NOT add the Minor Numbering - this is added automatically!
#'
#' projectNoteDir - the directory where the Project Note will be stored.  This
#' may be a Project Directory, or another Directory specified by the User.  MUST be a sub-directory
#' or lower inside a PROGRAMME Directory.
#'
#' selection - List containing the Goal, Del, Task selected from the Project Doc, as well as other useful
#' information - lines of Task/Del/Goal, projectDoc path content of selection line.  See cursorSelection()
#' or userSelection().
#'
#' subNoteName - the First SubNote name.
#'
#' projectNoteTitle - OPTIONAL title for the Project HEADER Note.  Default is to use projectNoteName and replace
#' all _ and - with SPACES.
#'
#' subNoteTitle - OPTIONAL title for the Project Sub Note.  Default is to use subNoteName and replace
#' all _ and - with SPACES.
#'
#' projNoteTemplate - template to use, as found in the `config/templates/` directory.  Default is
#' "Project-Header-Note-Template.Rmd"
#'
#' subNoteTemplate - template to use, as found in the `config/templates/` directory.  Default is
#' "Project-Sub-Note-Template.Rmd"
#'
#' @export
addProjectNoteGroup  <- function( projectNoteName, projectNotePrefix, projectNoteDir, selection,
                                  subNoteName, projectNoteTitle="", subNoteTitle="",
                                  projNoteTemplate="Project-Header-Note-Template.Rmd",
                                  subNoteTemplate="Project-Sub-Note-Template.Rmd" ) {

  # Check projectNoteName contains NO SPACES:
  if( grepl("\\s+", projectNoteName) ) {
    stop( cat("projectNoteName contains a SPACE: ", projectNoteName, "\n") )
  }

  # Check projectTitle, and if blank, fill with projectName, replacing all "_" and "-" with spaces
  if( nchar(projectNoteTitle)==0 ) {
    projectNoteTitle = gsub("-", " ", gsub("_", " ", projectNoteName) )
  }

  # set projectDocPath
  projectDocPath <- selection[["projectDocPath"]]

  # Check projectNoteDir is a sub-dir in a Programme DIR, which is a sub-dir to the root of an ORGANISATION:
  # run dirname TWICE as want to ensure projectNoteDir is a sub-dir in a Programme!
  orgPath <- dirname( dirname(projectNoteDir) )

  orgPath <- findOrgDir(orgPath)

  if(orgPath == "" ) {
    # the search reached the root of the filesystem without finding the Organisation files,
    # therefore, projectNoteDir is not inside a PROGRAMME sub-dir!
    stop( cat("projectNoteDir is not in a sub-dir of a PROGRAMME Directory: ", projectNoteDir, "\n") )
  }
  # now, orgPath should be the root dir of the organisation

  # set confPath + tempPath:
  confPath <- paste(orgPath, .Platform$file.sep, "config" , sep="")
  tempPath <- paste(confPath, .Platform$file.sep, "templates", sep="")


  ### HEADER NOTE ###

  # create the headerNotePrefix and headerNoteDir:
  headerNotePrefix <- paste(projectNotePrefix, "-00", sep="")
  headerNoteDir <- paste( projectNoteDir, .Platform$file.sep, headerNotePrefix, sep="")


  # Create DIR for the Project Note (using its PREFIX as its name):
  dir.create( headerNoteDir )

  # read Simple project note template:
  templateFileConn <- file( paste( tempPath, .Platform$file.sep, projNoteTemplate, sep="") )
  templateContents <- readLines( templateFileConn )
  close(templateFileConn)

  # Create the new RMD DOCUMENT:
  headerNotePath <- paste( projectNoteDir, .Platform$file.sep, headerNotePrefix, "~_", projectNoteName, ".Rmd", sep="")
  done <- file.create( headerNotePath )

  if(!done) {
    stop( cat("Project Header Note could not be created: ", headerNotePath, "\n") )
  }

  cat( "Made Project Header Note: ", headerNotePath, "\n" )


  # extract the Author value from the settings.yml file:
  settingsFile = paste( confPath, .Platform$file.sep, "settings.yml", sep="" )
  settings <- yaml::yaml.load( yaml::read_yaml( settingsFile ) )
  authorValue <- settings[["Author"]]

  # modify templateContents to include PREFIX and projectTitle
  templateContents <- gsub("{{PREFIX}}", projectNotePrefix, templateContents, fixed=TRUE)
  templateContents <- gsub("{{TITLE}}", projectNoteTitle, templateContents, fixed=TRUE)
  templateContents <- gsub("{{AUTHOR}}", authorValue, templateContents, fixed=TRUE)


  # replace the {{OBJECTIVES}} part of the template with the Objectives Template:

  # read objectives template:
  objectivesFileConn <- file( paste( tempPath, .Platform$file.sep, "Objectives.Rmd", sep="") )
  objectives <- readLines( objectivesFileConn )
  close(objectivesFileConn)

  # replace:
  templateContents <- replaceAndInsertVector("{{OBJECTIVES}}", objectives, templateContents)



  # compute Project Source Doc RELATIVE LINK:
  DocLink <- R.utils::getRelativePath(projectDocPath, relativeTo=headerNotePath)
  DocLink <- substring(DocLink, first=4, last=nchar(DocLink)) # remove first `../`
  # DocLink <- paste( substring(DocLink, first=1, last=nchar(DocLink)-4), "/", sep="")
  # for now have left links as ".Rmd", but this needs to be ".html", or "/" in a rendered website!
  # Its set as ".Rmd" as ".Rmd" links can be navigated in RStudio!

  # NB Need to convert the .Rmd links to .html when WRITING the Organisation to a html site!

  DocName <- basename(projectDocPath)
  DocName <- gsub("_", " ", substring(DocName, first=1, last=nchar(DocName)-4))

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


  templateContents <- gsub("{{PROJECT_DOC_LINK}}", DocTitleLink, templateContents, fixed=TRUE)

  templateContents <- gsub("{{PROJECT_DOC_LINK_GOAL}}", GoalTitleLink, templateContents, fixed=TRUE)
  templateContents <- gsub("{{PROJECT_DOC_LINK_DEL}}", DelTitleLink, templateContents, fixed=TRUE)
  templateContents <- gsub("{{PROJECT_DOC_LINK_TASK}}", TaskTitleLink, templateContents, fixed=TRUE)


  # write to projFile
  fileConn <- file(headerNotePath)
  writeLines(templateContents, fileConn)
  close(fileConn)



  ### INSERT LINK FROM PROJECT NOTE INTO PROJECT DOC:

  # read Project Doc:
  projDocFileConn <- file( projectDocPath )
  projDocContents <- readLines( projDocFileConn )
  close(projDocFileConn)

  # create the projectNoteLink:
  NoteLink <- R.utils::getRelativePath(headerNotePath, relativeTo=projectDocPath)
  NoteLink <- substring(NoteLink, first=4, last=nchar(NoteLink)) # remove first `../`
  projectNoteLink <- paste("**[", headerNotePrefix, "~ ", projectNoteTitle, "](", NoteLink, ")**",  sep="")
  #**[LAB~003-00~ Axonal Projections Hippocampal Entorhinal Pathway](../LAB/LAB~003-00~_axonal_projections_hippocampal_entorhinal_pathway.Rmd)**

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

  cat( "  Written Project Header Note Link to Project Doc: ", projectDocPath," at line: ", line, "\n" )



  # Write PROJECT NOTE to the status.yml file:

  # Read the status.yml file first into a LIST:
  statusFile = paste( confPath, .Platform$file.sep, "status.yml", sep="" )
  status <- yaml::yaml.load( yaml::read_yaml( statusFile ) )

  # add projectName, goalNum, delNum, taskNum under the FULL projectNoteName (including prefix, index and name)
  # in the "PROJECT_NOTES" section of the status.yml List:
  projectName <- substring(basename(projectDocPath), first=1, last=(nchar(basename(projectDocPath))-4)  )
  noteType <- c( "GROUP" ) # noteType is group - subNotes will be added
  # add the Project Doc GOAL/DEL/TASK in a list of objectives - so more can be added later
  objs <- list(projectName, goalNum, delNum, taskNum)
  names(objs) <- c("projectName", "goalNum", "delNum", "taskNum")
  obj <- list(objs)
  names(obj) <- c("1")
  attrs <- list(obj, as.character(file.info(headerNotePath)[,5]), noteType )
  names(attrs) <- c("OBJECTIVES", "creationTime", "noteType")
  status[["PROJECT_NOTES"]][[ paste(headerNotePrefix, "~_", projectNoteName, sep="") ]] <- attrs
  # can retrieve the programmePrefix with call to:
  # status[["PROJECT_NOTES"]][[projectNoteName]][["projectName"]]
  # status[["PROJECT_NOTES"]][[projectNoteName]][["goalNum"]]
  # etc.

  # Write status list to the statusFile:
  yaml::write_yaml( yaml::as.yaml(status), statusFile )

  cat( "  Written PROJECT to Status.yml file: ", statusFile, "\n" )



  ###  SUB NOTE  ###

  # create the subNotePrefix:
  subNotePrefix <- paste(projectNotePrefix, "-001", sep="")

  # RE-MAKE the selection object - select the line in ProjectDoc where the HEADER link is inserted!
  selection <- projectmanagr::userSelection(projectDocPath, (line+3) )

  # now run addSubNoteToGroup using subNotePrefix and new selection:
  addSubNoteToGroup( subNoteName, subNotePrefix, headerNoteDir, selection, subNoteTemp=subNoteTemplate )

}
