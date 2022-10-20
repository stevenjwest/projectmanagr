#' Add a New Sub Note to a Project Group
#'
#' This Function adds a Sub Project Note to a Project Group: The subNote is placed into the Project
#' Group Directory, links to the subNote are added to the project doc(s) and the project group header
#' note, and the subnote is written to any Programme Index files necessary, under the project doc(s).
#'
#' @param subNotePrefix The whole subNotePrefix, including identifier and Major
#' Numbering, separated by ~, and finally the Minor Numbering system (or subNote numbering),
#' separated by -.  User needs to define this.
#'
#' @param subNoteName The name of the Project Sub Note, a Title with all SPACES replaced
#' with - or _.
#'
#' @param subNoteDir The directory where the Sub Note will be stored.  This will be the Project Group
#' Note Directory.  Must be an ABSOLUTE path!
#'
#' @param selection List containing the Goal, Del, Task selected from the Project Doc, as well as other useful
#' information - lines of Task/Del/Goal, projectDoc path content of selection line.  Mainly for validating the
#' input (its not actually used in this function!  But the selection is used in RStudio Addins to determine whether
#' to run addSubNoteToGroup, addProjectNote, or addProjectNoteGroup - may omit?).  See `cursorSelection()` or
#' `userSelection()`.  Selection MUST be on a HeaderNote!
#'
#' @param subNoteTitle OPTIONAL title for the Project Note.  Default is to use subNoteName and replace
#' all _ and - with SPACES.
#'
#' @param subNoteTemp OPTIONAL template to use, as found in the `config/templates/` directory of the Organisation.
#' Default is `Project-Sub-Note-Template.Rmd`
#'
#' @export
addSubNoteToGroup <- function( subNotePrefix, subNoteName, subNoteDir, selection,
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

  # load settings file for user defined settings
  settingsFile <- paste( confPath, .Platform$file.sep, "settings.yml", sep="" )
  settings <- yaml::yaml.load( yaml::read_yaml( settingsFile ) )

  subNoteDir <- normalizePath(subNoteDir)


  ##############################
  ### CREATE Sub Note DIR:
  ##############################
  noteDirPath <- paste( subNoteDir, .Platform$file.sep, subNotePrefix, sep="")
  done <- dir.create( noteDirPath )

  if(!done) {
    stop( paste0("  DIR for Project Note could not be created: ", noteDirPath) )
  }

  cat( "  Made Project Note DIR: ", noteDirPath, "\n" )


  #####################################
  ### WRITE Sub Note Template to File:
  #####################################

  # read SubNote template:
  templateFileConn <- file( paste( tempPath, .Platform$file.sep, subNoteTemp, sep="") )
  templateContents <- readLines( templateFileConn )
  close(templateFileConn)


  # Create the new RMD DOCUMENT:
  subNotePath <- paste( subNoteDir, .Platform$file.sep, subNotePrefix, "~_", subNoteName, ".Rmd", sep="")
  done <- file.create( subNotePath ) # just check the path is legal..

  if(!done) {
    file.remove(noteDirPath) # remove the dir
    stop( paste0("  Project Sub Note could not be created: ", subNotePath) )
  }

  cat( "  Made Project Sub Note: ", subNotePath, "\n" )

  # Use task header from template
  #summaryBullet < - paste0("* ", as.character(Sys.time()) )
  #summaryBullet <- getTaskSectionHeader(orgPath)
   # MAY NOT BE USED - links written in addLinks() function

  # extract the Author value from the settings.yml file:
  #settingsFile = paste( confPath, .Platform$file.sep, "settings.yml", sep="" )
  #settings <- yaml::yaml.load( yaml::read_yaml( settingsFile ) )
  #authorValue <- settings[["Author"]]
  authorValue <- Sys.info()["user"] # use username as author instead

  # modify templateContents to include PREFIX and projectTitle
  templateContents <- gsub("{{PREFIX}}", subNotePrefix, templateContents, fixed=TRUE)
  templateContents <- gsub("{{TITLE}}", subNoteTitle, templateContents, fixed=TRUE)
  templateContents <- gsub("{{AUTHOR}}", authorValue, templateContents, fixed=TRUE)
  templateContents <- gsub("{{DATA_STORAGE_SECTION}}", settings[["dataStorageSectionValue"]],
                           templateContents, fixed=TRUE)
  templateContents <- gsub("{{SUBNOTE_CONTENTS_SECTION}}", settings[["subNoteContentsSectionValue"]],
                           templateContents, fixed=TRUE)

  # add the current data storage path:
  #templateContents <- gsub("{{D ATASTORAGE}}", subNotePrefix, templateContents, fixed=TRUE)
   # do not add the project note DIR here - the DATA STORAGE section will be filled out by volume functions


  # replace the {{OBJECTIVES}} part of the template with the Objectives Template:

  # read objectives template:
  objectivesFileConn <- file( paste( tempPath, .Platform$file.sep, "Objectives.Rmd", sep="") )
  objectives <- readLines( objectivesFileConn )
  close(objectivesFileConn)

  # replace:
  templateContents <- replaceAndInsertVector("{{OBJECTIVES}}", objectives, templateContents)


  ##############################
  ### INSERT Project Doc Links:
  ##############################

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
  projDocList <- getHeaderNoteDocLinkList(headerNoteContents, headerPath)

  headerHasObjs <- (length(projDocList) > 0) # store boolean as headerHasObjs
  cat( "  headerHasObjs: ", headerHasObjs, "\n" )

  # add Project Doc links to the current subnote contents (templateContents):
  # ONLY IF projDocList has ANY G/D/Ts!
  if( headerHasObjs == TRUE ) {
    # this checks whether the HEADER has ANY objectives to add
    templateContents <- addLinks( templateContents, subNotePath, projDocList )

  } else { # want to add the Proj Doc G/D/T in selection to subnote

    selectionMissing <- (selection[[1]][1] == "FALSE") # boolean to represent whether selection exists or is missing

    # This only applies if a selection object EXISTS
     # it may not exist if subnote is added from a HEADER NOTE (no selection object generated as no Project Doc G/D/T selected!)
     # in this case the projDocList may be 0 if the HEADER NOTE has no project doc links in it
     # so should just set the subnote to have NO PROJECT DOC LINKS
    if( selectionMissing == TRUE ) {

      cat( "    Missing Selection \n" )
      # crop the templateContents to remove the OBJECTIVES
      templateContents <- c(templateContents[1:15], templateContents[32:length(templateContents)])

    } else {

      # convert the selection object to a projDocList object
      projDocList <- list()
      projDocList[[1]] <- selection$projectDocPath
      projDocList[[1]][2] <- selection$goal
      projDocList[[1]][3] <- selection$deliverable
      projDocList[[1]][4] <- selection$task

      #cat( "  projDocList length: ", length(projDocList), "\n" )

      # now add this selection "projDocList" to subNote
      templateContents <- addLinks( templateContents, subNotePath, projDocList )

    }

  }


  ##############################
  ### WRITE HEADER LINK
  ##############################

  # Want to add a RELATIVE LINK to the HEADER NOTE here

  # compute Project Source Doc RELATIVE LINK:
  headerLink <- R.utils::getRelativePath(headerPath, relativeTo=subNotePath)
  headerLink <- substring(headerLink, first=4, last=nchar(headerLink)) # remove first `../`

  headerTitleLink <- paste( "[", headerName, "](", headerLink, ")", sep="" )

  templateContents <- gsub("{{HEADER_NOTE_LINK}}", headerTitleLink, templateContents, fixed=TRUE)

  cat( "    Written Header Link to Sub Note file: ", basename(subNotePath), "\n" )


  # write subNote file to disk:
  fileConn <- file(subNotePath)
  writeLines(templateContents, fileConn)
  close(fileConn)

  cat( "  Written Sub Note to disk: ", basename(subNotePath), "\n" )


  #################################################################
  ### INSERT LINK FROM PROJECT SUB NOTE INTO PROJECT DOC(S) ONLY
  #################################################################

    # No Longer writing to Programmes - Programmes just contains a list of Projects

  if( headerHasObjs == TRUE ) { # if header has objectives the projDocList is filled with these common objectives

    # so add these with this method, placing the new subNote under the headerNote links in proj docs
    addSubNoteLinkToDocs(projDocList, subNotePath, headerPath)

  } else {

    if( selectionMissing == TRUE  ) {
      # do nothing - there is no project doc to write to!

    } else {
      # Otherwise add the subNote as a simple link to the projDocList/selection
       # so WRITE the link via selection object - this only writes the link in ProjDoc and DOES NOT write the ProjDoc link in the note!
      writeLinkProjectNote(subNotePath, selection)

      if(FALSE) {
        # read Project Doc:
        projectDocPath <- selection$projectDocPath
        projDocFileConn <- file( projectDocPath )
        projDocContents <- readLines( projDocFileConn )
        close(projDocFileConn)

        # create the projectNoteLink:
        NoteLink <- R.utils::getRelativePath(subNotePath, relativeTo=projectDocPath)
        NoteLink <- substring(NoteLink, first=4, last=nchar(NoteLink)) # remove first `../`
        noteName <- substring( basename(subNotePath), first=1, last=nchar(basename(subNotePath))-4)
        projectNoteLink <- paste("**[", noteName, "](", NoteLink, ")**",  sep="")
        #[BMS~314~_AVIL_42SNI_EdU_16wks](../BMS/BMS~314~_AVIL_42SNI_EdU_16wks/)

        # edit the summary information - if TaskTodoSectionHeader is in summaryBullet, remove everything FROM THAT LINE
        # get TaskTodoSectionHeader value
        todoValue <- settings[["TaskTodoSectionHeader"]]
        # get new summary info - WITHOUT the TaskTodoSectionHeader plus excess whitespace
        summaryBullet <- getTaskSectionHeader(orgPath)
        summaryInfo <- summaryBullet[1: computePreviousLineIndex(grep(todoValue, summaryBullet, fixed=TRUE)-1, summaryBullet)+1 ]

        # create the Vector, including Whitespace and Summary information ONLY - without Task TODO Section :
        projectNoteLinkVector <- c( "", "", "", projectNoteLink, "", "", summaryInfo, "" )

        # compute place to insert the project note link:
        # get the line selected in the projectDoc - [["originalLine"]]
        line <- computeNextLine(selection[["originalLineNumber"]], projDocContents)

        # Insert projectNoteLinkVector to projDocContents:
        projDocContents <- c(projDocContents[1:(line-1)], projectNoteLinkVector, projDocContents[(line+1):length(projDocContents)])


        # write to projFile
        projDocFileConn <- file( projectDocPath )
        writeLines(projDocContents, projDocFileConn)
        close(projDocFileConn)

        cat( "  Written Sub Note Link to Project Doc: ", basename(projectDocPath), "\n" )
      }

    }
  }



  ### INSERT LINK FROM PROJECT SUB NOTE INTO PROJECT HEADER NOTE:

  # read header note:  headerNoteFileConn
  headerNoteFileConn <- file( headerPath )
  headerNoteContents <- readLines( headerNoteFileConn )
  close(headerNoteFileConn)

  # create the projectNoteLink:
  NoteLink <- R.utils::getRelativePath(subNotePath, relativeTo=headerPath)
  NoteLink <- substring(NoteLink, first=4, last=nchar(NoteLink)) # remove first `../`
  projectNoteLink <- paste("[", subNotePrefix, "~_", subNoteName, "](", NoteLink, ")",  sep="")
  #[BMS~314~_AVIL_42SNI_EdU_16wks](../BMS/BMS~314~_AVIL_42SNI_EdU_16wks/)

  # create the Vector, including Whitespace and Summary information:
  projectNoteLinkVector <- c( "", "", projectNoteLink, "" )

  # compute place to insert the sub note link in the header note:
  headerContentsSectionValue <- settings[["headerNoteContentsSectionValue"]]
  line <- matchLineIndex(headerContentsSectionValue, headerNoteContents)
  line <- grepLineIndexFrom("----", headerNoteContents, line)
  line <- computeNextHeaderLine( line, headerNoteContents )

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
