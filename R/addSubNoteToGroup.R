#' Add a New Sub Note to a Project Group
#'
#' This Function adds a Sub Project Note to a Project Group: The subNote is placed into the Project
#' Group Directory, links to the subNote are added to the project doc(s) and the project group header
#' note, and the subnote is written to any Programme Index files necessary, under the project doc(s).
#'
#' @param subNoteName The name of the Project Sub Note, a Title with all SPACES replaced
#' with - or _.
#' @param subNotePrefix The whole subNotePrefix, including identifier and Major
#' Numbering, separated by ~, and finally the Minor Numbering system (or subNote numbering),
#' separated by -.  User needs to define this.
#' @param subNoteDir The directory where the Sub Note will be stored.  This will be the Project Group
#' Note Directory.  Must be an ABSOLUTE path!
#' @param selection List containing the Goal, Del, Task selected from the Project Doc, as well as other useful
#' information - lines of Task/Del/Goal, projectDoc path content of selection line.  Mainly for validating the
#' input (its not actually used in this function!  But the selection is used in RStudio Addins to determine whether
#' to run addSubNoteToGroup, addProjectNote, or addProjectNoteGroup - may omit?).  See `cursorSelection()` or
#' `userSelection()`.  Selection MUST be on a HeaderNote!
#' @param volume The name of the volume where the sub-notes data will be stored.  Must be a directory inside the
#' `volumes/` directory in the Project Organisation.  "local" by default.
#' @param subNoteTitle OPTIONAL title for the Project Note.  Default is to use subNoteName and replace
#' all _ and - with SPACES.
#' @param subNoteTemp OPTIONAL template to use, as found in the `config/templates/` directory of the Organisation.
#' Default is `Project-Sub-Note-Template.Rmd`
#'
#' @export
addSubNoteToGroup <- function( subNoteName, subNotePrefix, subNoteDir, selection, volume = "local",
                            subNoteTitle="", subNoteTemp="Project-Sub-Note-Template.Rmd"  ) {


  cat( "\nprojectmanagr::addSubNoteToGroup():\n" )


  ####################
  ### CHECK INPUT
  ####################

  # Check subNoteName contains NO SPACES:
  if( grepl("\\s+", subNoteName) ) {
    stop( paste0("  subNoteName contains a SPACE: ", subNoteName) )
  }

  # Check subNotePrefix contains NO SPACES:
  if( grepl("\\s+", subNotePrefix) ) {
    stop( paste0("  subNotePrefix contains a SPACE: ", subNotePrefix) )
  }

  # Check subNoteTitle, and if blank, fill with subNoteName, replacing all "_" and "-" with spaces
  if( nchar(subNoteTitle) == 0 ) {
    subNoteTitle <- gsub("-", " ", gsub("_", " ", subNoteName) )
  }


  #################################
  ### FIND ORG PATH AND RESOURCES
  #################################


  # Check subNoteDir is a sub-dir in a Programme DIR, which is a sub-dir to the root of an ORGANISATION:
  # run dirname TWICE as want to ensure subNoteDir is a sub-dir in a Programme!
  orgPath <- dirname( dirname(subNoteDir) )

  orgPath <- findOrgDir(orgPath)

  if(orgPath == "" ) {
    # the search reached the root of the filesystem without finding the Organisation files,
    # therefore, subNoteDir is not inside a PROGRAMME sub-dir!
    stop( paste0("  subNoteDir is not in a sub-dir of a PROGRAMME Directory: ", subNoteDir) )
  }
  # now, orgPath should be the root dir of the organisation

  # set confPath + tempPath:
  confPath <- paste0( orgPath, .Platform$file.sep, "config" )
  tempPath <- paste0(confPath, .Platform$file.sep, "templates" )
  # set volume path:
  volPath <- paste0( orgPath, .Platform$file.sep, "volumes", .Platform$file.sep, volume )

  if( file.exists(volPath) == FALSE ) {
    stop( paste0("  volume does not exist: ", volPath) )
  }



  ##########################
  ### CREATE SYMLINK TO DIR for Project Sub Note (using its PREFIX as its name):
  ##########################

  # first make the Note DIR on the volume - traversing the SAME path as from the root of the projectmanagr org:

  subNoteDir <- normalizePath(subNoteDir)

  projDirOrgPath <- paste0( substr(subNoteDir,
                                   nchar(findOrgDir(subNoteDir))+2,
                                   nchar(subNoteDir) ),
                            .Platform$file.sep,
                            subNotePrefix )

  noteDirPath = paste( volPath, .Platform$file.sep, projDirOrgPath, sep="")
  done <- dir.create( noteDirPath, recursive = TRUE )

  if(!done) {
    stop( paste0("  Project Note directory could not be created on volume: ", noteDirPath) )
  }

  cat( "  Made Project Note dir on volume: ", noteDirPath, "\n" )


  # THEN make the symlink to this, putting the symlink in the projectNotePath:
  noteDirSymPath = paste0( subNoteDir, .Platform$file.sep, subNotePrefix )
  symLink <- R.utils::getRelativePath(noteDirPath, relativeTo=noteDirSymPath)
  symLink <- substring(symLink, first=4, last=nchar(symLink)) # remove first `../`

  done <- file.symlink( symLink, noteDirSymPath )

  if(!done) {
    file.remove(noteDirPath) # remove the file
    stop( paste0("  Project Note symlink could not be made to volume: ", noteDirSymPath, " ", noteDirPath) )
  }

  cat( "  Made Project Note symlink: ", noteDirSymPath, "\n" )


  #####################################
  ### WRITE SubNote Template to File:
  #####################################

  # read SubNote template:
  templateFileConn <- file( paste( tempPath, .Platform$file.sep, subNoteTemp, sep="") )
  templateContents <- readLines( templateFileConn )
  close(templateFileConn)


  # Create the new RMD DOCUMENT:
  subNotePath <- paste( subNoteDir, .Platform$file.sep, subNotePrefix, "~_", subNoteName, ".Rmd", sep="")
  done <- file.create( subNotePath ) # just check the path is legal..

  if(!done) {
    file.remove(noteDirPath) # remove the file
    file.remove(noteDirSymPath) # remove the symlink
    stop( paste0("  Project Sub Note could not be created: ", subNotePath) )
  }

  cat( "  Made Project Sub Note: ", subNotePath, "\n" )


  # get creation time - use to write to SUMMARY:
  summaryBullet <- paste0("* ", as.character(file.info(subNotePath)[,5]) )


  # extract the Author value from the settings.yml file:
  settingsFile = paste( confPath, .Platform$file.sep, "settings.yml", sep="" )
  settings <- yaml::yaml.load( yaml::read_yaml( settingsFile ) )
  authorValue <- settings[["Author"]]

  # modify templateContents to include PREFIX and projectTitle
  templateContents <- gsub("{{PREFIX}}", subNotePrefix, templateContents, fixed=TRUE)
  templateContents <- gsub("{{TITLE}}", subNoteTitle, templateContents, fixed=TRUE)
  templateContents <- gsub("{{AUTHOR}}", authorValue, templateContents, fixed=TRUE)

  # add DATA STORAGE location
  templateContents <- gsub("{{DATA_STORAGE}}", symLink, templateContents, fixed=TRUE)


  # replace the {{OBJECTIVES}} part of the template with the Objectives Template:

  # read objectives template:
  objectivesFileConn <- file( paste( tempPath, .Platform$file.sep, "Objectives.Rmd", sep="") )
  objectives <- readLines( objectivesFileConn )
  close(objectivesFileConn)

  # replace:
  templateContents <- replaceAndInsertVector("{{OBJECTIVES}}", objectives, templateContents)


  ### INSERT Project Doc Links:

  # There may be more than one, so first retrieve all ProjectDoc links from headerNote:
    # any new SubNote should inherit all Project Doc links from its parent -> header note

  # first, derive the Header RMD file and path:
  headerList <- list.files(dirname(subNoteDir), pattern = basename(subNoteDir) )
  headerName <- headerList[substring(headerList, first = nchar(headerList)-3) == ".Rmd"]
  headerName <- substring(headerName, first=1, last=nchar(headerName)-4)
  headerPath <- paste(dirname(subNoteDir), .Platform$file.sep, headerName, ".Rmd", sep="")

  # read header note:
  headerNoteFileConn <- file( headerPath )
  headerNoteContents <- readLines( headerNoteFileConn )
  close(headerNoteFileConn)

  # get list of project docs from the header note:
  projDocList <- getProjectNoteDocLinkList(headerNoteContents, headerPath)

  # add Project Doc links to the current subnote contents (templateContents):
  templateContents <- addLinks( templateContents, subNotePath, projDocList )



  ### WRITE HEADER LINK
  # Want to add a RELATIVE LINK to the HEADER NOTE here

  # define the headerTitle:
  headerTitle <- gsub( "-", " ",  gsub("_", " ", headerName )  )

  # compute Project Source Doc RELATIVE LINK:
  headerLink <- R.utils::getRelativePath(headerPath, relativeTo=subNotePath)
  headerLink <- substring(headerLink, first=4, last=nchar(headerLink)) # remove first `../`

  headerTitleLink <- paste( "## [", headerTitle, "](", headerLink, ")", sep="" )

  templateContents <- gsub("{{HEADER_NOTE_LINK}}", headerTitleLink, templateContents, fixed=TRUE)

  cat( "    Written Header Link to Sub Note file: ", basename(subNotePath), "\n" )



  # write subNote file to disk:
  fileConn <- file(subNotePath)
  writeLines(templateContents, fileConn)
  close(fileConn)

  cat( "  Written Sub Note to disk: ", basename(subNotePath), "\n" )



  ### INSERT LINK FROM PROJECT SUB NOTE INTO PROJECT DOC(S) ONLY

    # No Longer writing to Programmes - Programmes just contains a list of Projects
    # Should use the Pomodoro TODO Sheet to keep track of Tasks!

  addSubNoteLinkToDocs(projDocList, subNotePath, headerPath)



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
  # NO LONGER USING STATUS.YML TO HOLD DATA ON ALL DOCS AND NOTES

  # Read the status.yml file first into a LIST:
  #statusFile = paste( confPath, .Platform$file.sep, "status.yml", sep="" )
  #status <- yaml::yaml.load( yaml::read_yaml( statusFile ) )

  # add projectName, goalNum, delNum, taskNum under the FULL subNoteName (including prefix, index and name)
  # in the "PROJECT_NOTES" section of the status.yml List:
  # extract FULLY QUALIFIED PROJECT DOC NAME - including the Programme, "PROJECTS/" and the ProjectDoc Name:
  #projectName <- paste(
  #  basename(dirname(dirname(projectDocPath))), .Platform$file.sep,
  #  basename(dirname(projectDocPath)), .Platform$file.sep,
  #  substring(basename(projectDocPath), first=1, last=(nchar(basename(projectDocPath))-4)  ), sep="")
  #noteType <- c( "SUBNOTE" ) # noteType is single - no subNotes will be added
  # add the Project Doc GOAL/DEL/TASK in a list of objectives - so more can be added later
  #objs <- list(projectName, goalNum, delNum, taskNum)
  #names(objs) <- c("projectName", "goalNum", "delNum", "taskNum")
  #obj <- list(objs)
  #names(obj) <- c("1")
  #attrs <- list(obj, as.character(file.info(subNotePath)[,5]), noteType )
  #names(attrs) <- c("OBJECTIVES", "creationTime", "noteType")
  #status[["PROJECT_NOTES"]][[ headerName ]][[paste(subNotePrefix, "~_", subNoteName, sep="")]] <- attrs
  # can retrieve data with call to:
  # status[["PROJECT_NOTES"]][[headerName]][["GROUP"]][["projectName"]]
  # status[["PROJECT_NOTES"]][[headerName]][["GROUP"]][["goalNum"]]
  # etc.

  # Write status list to the statusFile:
  #yaml::write_yaml( yaml::as.yaml(status), statusFile )

  #cat( "  Written PROJECT to Status.yml file: ", statusFile, "\n" )


}
