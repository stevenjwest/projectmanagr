#' Add a New Sub Note to a Project Group
#'
#' This Function adds a Sub Project Note to a Project Group:
#'
#' - The subNote is placed into the Project Group Directory
#'
#' - Links to the subNote are added to the project doc and the project group header
#' note.
#'
#'
#' subNoteName - the name of the Project Sub Note, a Title with all SPACES replaced
#' with - or _.
#'
#' subNotePrefix - the whole subNotePrefix, including identifier and Major
#' Numbering, separated by ~, and finally the Minor Numbering system (or subNote numbering),
#' separated by -.
#'
#' subNoteDir - the directory where the Sub Note will be stored.  This will be the Project Group
#' Note Directory.
#'
#' selection - List containing the Goal, Del, Task selected from the Project Doc, as well as other useful
#' information - lines of Task/Del/Goal, projectDoc path content of selection line.  See cursorSelection()
#' or userSelection().
#'
#' subNoteTitle - OPTIONAL title for the Project Note.  Default is to use subNoteName and replace
#' all _ and - with SPACES.
#'
#' subNoteTemp - OPTIONAL template to use, as found in the `templates/` directory.  Default is
#' "Project-Sub-Note-Template.Rmd"
#'
#' @export
addSubNoteToGroup <- function( subNoteName, subNotePrefix, subNoteDir, selection,
                            subNoteTitle="", subNoteTemp="Project-Sub-Note-Template.Rmd"  ) {

  cat( "\nprojectmanagr::addSubNoteToGroup():\n" )

  # Check subNoteName contains NO SPACES:
  if( grepl("\\s+", subNoteName) ) {
    stop( cat("  subNoteName contains a SPACE: ", subNoteName, "\n") )
  }

  # Check subNoteTitle, and if blank, fill with subNoteName, replacing all "_" and "-" with spaces
  if( nchar(subNoteTitle) == 0 ) {
    subNoteTitle = gsub("-", " ", gsub("_", " ", subNoteName) )
  }

  # set projectDocPath
  projectDocPath <- selection[["projectDocPath"]]

  # Check subNoteDir is a sub-dir in a Programme DIR, which is a sub-dir to the root of an ORGANISATION:
  # run dirname TWICE as want to ensure subNoteDir is a sub-dir in a Programme!
  orgPath <- dirname( dirname(subNoteDir) )

  orgPath <- findOrgDir(orgPath)

  if(orgPath == "" ) {
    # the search reached the root of the filesystem without finding the Organisation files,
    # therefore, subNoteDir is not inside a PROGRAMME sub-dir!
    stop( cat("  subNoteDir is not in a sub-dir of a PROGRAMME Directory: ", subNoteDir, "\n") )
  }
  # now, orgPath should be the root dir of the organisation

  # set confPath + tempPath:
  confPath <- paste(orgPath, .Platform$file.sep, "config" , sep="")
  tempPath <- paste(confPath, .Platform$file.sep, "templates", sep="")


  # Create DIR for the Sub Note (using its PREFIX as its name):
  done <- dir.create( paste( subNoteDir, .Platform$file.sep, subNotePrefix, sep="") )

  if(!done) {
    stop( cat("  Project Sub Note Dir could not be created: ", paste( subNoteDir, .Platform$file.sep, subNotePrefix, sep=""), "\n") )
  }

  cat( "  Made Project Sub Note Dir: ", paste( subNoteDir, .Platform$file.sep, subNotePrefix, sep=""), "\n" )

  # read Simple project note template:
  templateFileConn <- file( paste( tempPath, .Platform$file.sep, subNoteTemp, sep="") )
  templateContents <- readLines( templateFileConn )
  close(templateFileConn)

  # Create the new RMD DOCUMENT:
  subNotePath <- paste( subNoteDir, .Platform$file.sep, subNotePrefix, "~_", subNoteName, ".Rmd", sep="")
  done <- file.create( subNotePath )

  if(!done) {
    stop( cat("  Project Sub Note could not be created: ", subNotePath, "\n") )
  }

  cat( "  Made Project Sub Note: ", subNotePath, "\n" )


  # extract the Author value from the settings.yml file:
  settingsFile = paste( confPath, .Platform$file.sep, "settings.yml", sep="" )
  settings <- yaml::yaml.load( yaml::read_yaml( settingsFile ) )
  authorValue <- settings[["Author"]]

  # modify templateContents to include PREFIX and projectTitle
  templateContents <- gsub("{{PREFIX}}", subNotePrefix, templateContents, fixed=TRUE)
  templateContents <- gsub("{{TITLE}}", subNoteTitle, templateContents, fixed=TRUE)
  templateContents <- gsub("{{AUTHOR}}", authorValue, templateContents, fixed=TRUE)


  # replace the {{OBJECTIVES}} part of the template with the Objectives Template:

  # read objectives template:
  objectivesFileConn <- file( paste( tempPath, .Platform$file.sep, "Objectives.Rmd", sep="") )
  objectives <- readLines( objectivesFileConn )
  close(objectivesFileConn)

  # replace:
  templateContents <- replaceAndInsertVector("{{OBJECTIVES}}", objectives, templateContents)



  # compute Project Source Doc RELATIVE LINK:
  DocLink <- R.utils::getRelativePath(projectDocPath, relativeTo=subNotePath)
  DocLink <- substring(DocLink, first=4, last=nchar(DocLink)) # remove first `../`
  # DocLink <- paste( substring(DocLink, first=1, last=nchar(DocLink)-4), "/", sep="")
  # for now have left links as ".Rmd", but this needs to be ".html", or "/" in a rendered website!
  # Its set as ".Rmd" as ".Rmd" links can be navigated in RStudio!

  # NB Need to convert the .Rmd links to .html when WRITING the Organisation to a html site!

  DocName <- basename(projectDocPath)
  DocName <- gsub( "-", " ",  gsub("_", " ", substring(DocName, first=1, last=nchar(DocName)-4) )  )

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


  # HEADER

  # Want to add a RELATIVE LINK to the HEADER NOTE here

  # first, derive the Header RMD file and path:
  headerList <- list.files(dirname(subNoteDir), pattern = basename(subNoteDir) )
  headerName <- headerList[substring(headerList, first = nchar(headerList)-3) == ".Rmd"]
  headerName <- substring(headerName, first=1, last=nchar(headerName)-4)
  headerPath <- paste(dirname(subNoteDir), .Platform$file.sep, headerName, ".Rmd", sep="")


  # define the headerTitle:
  headerTitle <- gsub( "-", " ",  gsub("_", " ", headerName )  )

  # compute Project Source Doc RELATIVE LINK:
  headerLink <- R.utils::getRelativePath(headerPath, relativeTo=subNotePath)
  headerLink <- substring(headerLink, first=4, last=nchar(headerLink)) # remove first `../`

  headerTitleLink <- paste( "## [", headerTitle, "](", headerLink, ")", sep="" )


  templateContents <- gsub("{{PROJECT_DOC_LINK}}", DocTitleLink, templateContents, fixed=TRUE)

  templateContents <- gsub("{{PROJECT_DOC_LINK_GOAL}}", GoalTitleLink, templateContents, fixed=TRUE)
  templateContents <- gsub("{{PROJECT_DOC_LINK_DEL}}", DelTitleLink, templateContents, fixed=TRUE)
  templateContents <- gsub("{{PROJECT_DOC_LINK_TASK}}", TaskTitleLink, templateContents, fixed=TRUE)

  templateContents <- gsub("{{HEADER_NOTE_LINK}}", headerTitleLink, templateContents, fixed=TRUE)


  # write to projFile
  fileConn <- file(subNotePath)
  writeLines(templateContents, fileConn)
  close(fileConn)

  cat( "  Written Goal Del Task to Sub Note file: ", basename(subNotePath), "\n" )



  ### INSERT LINK FROM PROJECT SUB NOTE INTO PROJECT DOC:

  # read Project Doc:
  projDocFileConn <- file( projectDocPath )
  projDocContents <- readLines( projDocFileConn )
  close(projDocFileConn)

  # create the projectNoteLink:
  NoteLink <- R.utils::getRelativePath(subNotePath, relativeTo=projectDocPath)
  NoteLink <- substring(NoteLink, first=4, last=nchar(NoteLink)) # remove first `../`
  projectNoteLink <- paste("* [", subNotePrefix, "~ ", subNoteTitle, "](", NoteLink, ")",  sep="")
  #* [LAB~003-001~ THF MeOH/DCM Clearing Tau Labelling](../LAB/LAB~001-00~_thf_meoh_dcm_clearing_tau_labelling.Rmd)

  # create the Vector, including Whitespace and Summary information:
  projectNoteLinkVector <- c( "", "", "", projectNoteLink, "", "    + Summary", "" )

  # compute place to insert the project note link:
  # get the line selected in the projectDoc - [["originalLineNumber"]]
  line <- computeNextSubNoteLine(selection[["originalLineNumber"]], projDocContents)

  # Insert projectNoteLinkVector to projDocContents:
  projDocContents <- c(projDocContents[1:(line-1)], projectNoteLinkVector, projDocContents[(line+1):length(projDocContents)])


  # write to projFile
  projDocFileConn <- file( projectDocPath )
  writeLines(projDocContents, projDocFileConn)
  close(projDocFileConn)

  cat( "  Written Project Sub Note Link to Project Doc: ", basename(projectDocPath), "\n" )



  ### INSERT LINK FROM PROJECT SUB NOTE INTO PROJECT HEADER NOTE:

  # read header note:  headerNoteFileConn
  headerNoteFileConn <- file( headerPath )
  headerNoteContents <- readLines( headerNoteFileConn )
  close(headerNoteFileConn)

  # create the projectNoteLink:
  NoteLink <- R.utils::getRelativePath(subNotePath, relativeTo=headerPath)
  NoteLink <- substring(NoteLink, first=4, last=nchar(NoteLink)) # remove first `../`
  projectNoteLink <- paste("[", subNotePrefix, "~ ", subNoteTitle, "](", NoteLink, ")",  sep="")
  #[BMS~314~ AVIL 42SNI EdU 16wks](../BMS/BMS~314~_AVIL_42SNI_EdU_16wks/)

  # create the Vector, including Whitespace and Summary information:
  projectNoteLinkVector <- c( "", "", projectNoteLink, "" )

  # compute place to insert the sub note link in the header note:
  line <- computeNextHeaderLine( headerNoteContents )

  # Insert projectNoteLinkVector to headerNoteContents:
  headerNoteContents <- c(headerNoteContents[1:(line-1)], projectNoteLinkVector, headerNoteContents[(line+1):length(headerNoteContents)])


  # write to headerNote File:
  headerNoteFileConn <- file( headerPath )
  writeLines(headerNoteContents, headerNoteFileConn)
  close(headerNoteFileConn)

  cat( "  Written Project Sub Note Link to Header Note file: ", basename(headerPath), "\n" )



  # Write PROJECT SUB NOTE to the status.yml file - under the HEADER NOTE:

  # Read the status.yml file first into a LIST:
  statusFile = paste( confPath, .Platform$file.sep, "status.yml", sep="" )
  status <- yaml::yaml.load( yaml::read_yaml( statusFile ) )

  # add projectName, goalNum, delNum, taskNum under the FULL subNoteName (including prefix, index and name)
  # in the "PROJECT_NOTES" section of the status.yml List:
  # extract FULLY QUALIFIED PROJECT DOC NAME - including the Programme, "PROJECTS/" and the ProjectDoc Name:
  projectName <- paste(
    basename(dirname(dirname(projectDocPath))), .Platform$file.sep,
    basename(dirname(projectDocPath)), .Platform$file.sep,
    substring(basename(projectDocPath), first=1, last=(nchar(basename(projectDocPath))-4)  ), sep="")
  noteType <- c( "SUBNOTE" ) # noteType is single - no subNotes will be added
  # add the Project Doc GOAL/DEL/TASK in a list of objectives - so more can be added later
  objs <- list(projectName, goalNum, delNum, taskNum)
  names(objs) <- c("projectName", "goalNum", "delNum", "taskNum")
  obj <- list(objs)
  names(obj) <- c("1")
  attrs <- list(obj, as.character(file.info(subNotePath)[,5]), noteType )
  names(attrs) <- c("OBJECTIVES", "creationTime", "noteType")
  status[["PROJECT_NOTES"]][[ headerName ]][[paste(subNotePrefix, "~_", subNoteName, sep="")]] <- attrs
  # can retrieve data with call to:
  # status[["PROJECT_NOTES"]][[headerName]][["GROUP"]][["projectName"]]
  # status[["PROJECT_NOTES"]][[headerName]][["GROUP"]][["goalNum"]]
  # etc.

  # Write status list to the statusFile:
  yaml::write_yaml( yaml::as.yaml(status), statusFile )

  cat( "  Written PROJECT to Status.yml file: ", statusFile, "\n" )


}
