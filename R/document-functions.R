#' Compute Line Index
#'
#' Returns the index of the first EXACT MATCH of line in contents, if not found returns -1.
#'
#' line - a String to be found
#'
#' contents - a vector of strings, the index of the first instance of line in this vector is returned.
#'
#'
computeLineIndex <- function(line, contents) {

  returnVal <- -1
  # look through contents to find an index that matches line:
  for( l in 1:length(contents) ) {

    if( contents[l] == line ) {
      returnVal <- l
      break
    }

  }

  returnVal

}


#' Grep Line Index
#'
#' Returns the index of the first PARTIAL MATCH (using grep) of line in contents,
#' if not found returns -1.
#'
#' @param line a String to be found
#' @param contents a vector of strings, the index of the first instance of line in
#' this vector is returned.
#'
#'
grepLineIndex <- function(line, contents) {

  returnVal <- -1
  # look through contents to find an index that matches line:
  for( l in 1:length(contents) ) {

    if( grepl(line, contents[l]) ) {
      returnVal <- l
      break
    }

  }

  returnVal

}


#' Grep Line Index from initialIndex
#'
#' Returns the index of the first PARTIAL MATCH (using grep) of line in contents,
#' from initialIndex, if not found returns -1.
#'
#' @param line a String to be found
#' @param contents a vector of strings, the index of the first instance of line in
#' this vector is returned.
#'
#'
grepLineIndexFrom <- function(line, contents, initialIndex) {

  returnVal <- -1
  # look through contents to find an index that matches line:
  for( l in initialIndex:length(contents) ) {

    if( grepl(line, contents[l]) ) {
      returnVal <- l
      break
    }

  }

  returnVal

}


#' Compute Last Line Index
#'
#' Returns the index of the LAST Line in contents that contains any content ("[A-z,0-9]")
#'
#' contents - a vector of strings, the index of the first instance of line in this vector is returned.
#'
#'
computeLastLineIndex <- function(contents) {

  returnVal <- 1

  # start at the END of contents:
  for( l in length(contents):1 ) {

    if( grepl("[A-z,0-9]", contents[l]) ) { # returns TRUE if line CONTAINS any letter or number

      returnVal <- (l+1) # return the NEXT LINE after the line which contains content
      break

    }

  }

  returnVal

}


#' Compute Previous Line Index
#'
#' Returns the index of the Previous Line in contents that contains any content ("[A-z,0-9]"), looking back
#' from lineIndex to 1.
#'
#' @param lineIndex the index to start looking back from.
#' @param contents a vector of strings, the index of the first instance of line in this vector is returned.
#'
#'
computePreviousLineIndex <- function(lineIndex, contents) {

  returnVal <- 1

  # start at the END of contents:
  for( l in lineIndex:1 ) {

    if( grepl("[A-z,0-9]", contents[l]) ) { # returns TRUE if line CONTAINS any letter or number

      returnVal <- (l+1) # return the NEXT LINE after the line which contains content
      break

    }

  }

  returnVal

}


#' Compute next Header line
#'
#' Returns the first blank line after the last content in Header Note contents.
#'
#'
#'
computeNextHeaderLine <- function(headerContents) {

  returnVal <- 1

  # start at the END of headerContents:
  for( l in length(headerContents):1 ) {

    if( grepl("[A-z,0-9]", headerContents[l]) ) { # returns TRUE if line CONTAINS any letter or number

      returnVal <- (l+1) # return the NEXT LINE after the line which contains content
      break

    }

  }

  returnVal

}

#' Get Project Note Doc Link List
#'
#' Returns a list of VECTORS:
#'
#'  list[[i]] : A VECTOR that includes the Project Doc ABSOLUTE Link, GOAL Num, DEL Num, TASK Num.
#'
#'  list[[i]][1] : Project Doc ABSOLUTE Link
#'
#'  list[[i]][2] : Project Doc GOAL - number plus title
#'
#'  list[[i]][3] : Project Doc DELIVERABLE - number plus title
#'
#'  list[[i]][4] : Project Doc TASK - number plus title
#'
#'  length( list ) returns the number of Project Doc links in the returned list.
#'
#'  @param projectNoteContents Character vector containing the contents of a Project Note, which includes links to
#'  at least one ProjectDoc, separated by "----", and ended with "------".
#'
#'  @param projectNotePath the FULL PATH to the project note, from which the contents is derived.
#'
#'
getProjectNoteDocLinkList <- function(projectNoteContents, projectNotePath) {

  # instantiate a list to store links
  linkList <- list()

  # start indices at 1:
  linkIndex <- 1
  goaldeltaskIndex <- 1

  # look through projectNoteContents up to line "------" - which marks the end of the Project Doc
    # links
  for( l in 1:grep("------", projectNoteContents) ) {

    # check for Project Doc link, plus GOAL, DEL, TASK
    # First will encounter Project Doc Link, then GOAL, DEL, TASK

    # however, check for GOAL DEL TASK first, as DEL has same signature as Project Doc Link
      # NOTE the ProjectDoc Link will ALWAYS come first though!
    if(substring(projectNoteContents[l], first=1, last=7) == "# [GOAL") {

          linkList[[linkIndex]][goaldeltaskIndex] <- paste0( "## ", substring(projectNoteContents[l],
                                                               first=4,
                                                               last=regexpr("]", projectNoteContents[l])-1 ) )

          goaldeltaskIndex <- goaldeltaskIndex +1 # increment index

          }

    else if(substring(projectNoteContents[l], first=1, last=15) == "## [DELIVERABLE") {

          linkList[[linkIndex]][goaldeltaskIndex] <- paste0( "### ", substring(projectNoteContents[l],
                                                               first=5,
                                                               last=regexpr("]", projectNoteContents[l])-1 ) )

          goaldeltaskIndex <- goaldeltaskIndex +1 # increment index

          }

    else if(substring(projectNoteContents[l], first=1, last=9) == "### [TASK") {

          linkList[[linkIndex]][goaldeltaskIndex] <- paste0( "#### ", substring(projectNoteContents[l],
                                                               first=6,
                                                               last=regexpr("]", projectNoteContents[l])-1 ) )

          goaldeltaskIndex <- goaldeltaskIndex +1 # increment index

          }

    else if( substring(projectNoteContents[l], first=1, last=4) == "## [" ) { # this must be ProjectDocLink

          # projectNoteContents[l] is a line that contains Project Doc link in ()
          # extract this Relative Link, and add to list to return:
            # AUTOMATICALLY uses [1] as the vector reference (and creates an error if referencing it!)
            # SO do not use goaldeltaskIndex:
          projDocRelLink <- substring(projectNoteContents[l],
                                             first=regexpr("\\(", projectNoteContents[l])+1,
                                             last=regexpr("\\)", projectNoteContents[l])-1 )

          linkList[[linkIndex]] <- computePath(projectNotePath, projDocRelLink)

          goaldeltaskIndex <- goaldeltaskIndex +1 # STILL increment this index

    }

    else if( substring(projectNoteContents[l], first=1, last=4) == "----" ) {
        #increment linkIndex when divider BETWEEN ProjectDoc links is seen:
        linkIndex <- linkIndex + 1
        # and reset the goaldeltaskIndex
        goaldeltaskIndex <- 1
    }

  }

  #return the linkList
  linkList

}


#' Find Goal/Del/Task Line Index in Project Doc
#'
#' Identify the line index of a goal/del/task, given the goalNum, delNum, taskNum.
#'
findProjectDocGoalDelTaskLineIndex <- function(projectDocContents, goalNum, delNum, taskNum) {

}


#' Compute Next Line
#'
#' Returns the first blank line after the last content under a Task
#' in a Project Document,
#'
#'
computeNextLine <- function(line, projDocContents) {

  # look through projDocContents FROM line, and identify the line that begins ### or ---
  for( l in (line+1):length(projDocContents) ) {

    if( substring(projDocContents[l], first=1, last=2) =="##" || substring(projDocContents[l], first=1, last=3) =="###" || substring(projDocContents[l], first=1, last=3) =="---" ) {

      val <- l
      break
    }
    else {
      val <- l
    }
  }

  # next, look from val-1 BACK to line, and identify the first line
  for( lb in (val-1):(line) ) {

    if( grepl("[A-z,0-9]", projDocContents[lb]) ) { # returns TRUE if line CONTAINS any letter or number

      returnVal <- (lb+1) # return the NEXT LINE after the line which contains content
      break

    }

  }

  returnVal

}



#' Compute Next Line Header
#'
#' Returns the first blank line after the last Project SubNote in a Project Group, under a Task
#' in a Project Document.
#'
#'
computeNextLineHeader <- function(line, projDocContents) {

  # look through projDocContents FROM line, and identify the line that begins ### or ---
  for( l in (line+1):length(projDocContents) ) {

    if( substring(projDocContents[l], first=1, last=2) =="##" || substring(projDocContents[l], first=1, last=3) =="###" || substring(projDocContents[l], first=1, last=3) =="---" || substring(projDocContents[l], first=1, last=3) =="**[" ) {

      val <- l
      break
    }
    else {
      val <- l
    }
  }

  # next, look from val-1 BACK to line, and identify the first line
  for( lb in (val-1):(line) ) {

    if( grepl("[A-z,0-9]", projDocContents[lb]) ) { # returns TRUE if line CONTAINS any letter or number

      returnVal <- (lb+1) # return the NEXT LINE after the line which contains content
      break

    }

  }

  returnVal

}



#' Compute Next Link Line
#'
#' Returns the next line in contents, from line, that contains a Rmd link (the next
#' line that contains the string "](" ), else if no link is found, returns a BLANK
#' STRING.
#'
#'
computeNextLinkLine <- function(line, contents) {

  returnVal <- ""

  # look through projDocContents FROM line, and identify the line that begins ### or ---
  for( l in (line+1):length(contents) ) {

    if( regexpr("](", contents[l], fixed=TRUE) > 0 ) {
      returnVal <- contents[l]
      break
    }

  }

  returnVal

}


#' Compute Next SubNote Line
#'
#' Returns the first blank line after the last content under a Header Note Link
#' in a Project Document,
#'
#'
computeNextSubNoteLine <- function(line, projDocContents) {

  # look through projDocContents FROM line, and identify the line that begins ### or ---
  # or **[ (start of next HEADER or SIMPLE NOTE)
  for( l in (line+1):length(projDocContents) ) {

    if( substring(projDocContents[l], first=1, last=3) == "###" ||
        substring(projDocContents[l], first=1, last=3) == "---" ||
        substring(projDocContents[l], first=1, last=3) == "**[" ) {

      val <- l
      break
    }
    else {
      val <- l
    }
  }

  # next, look from val-1 BACK to line, and identify the first line
  for( lb in (val-1):(line) ) {

    if( grepl("[A-z,0-9]", projDocContents[lb]) ) { # returns TRUE if line CONTAINS any letter or number

      returnVal <- (lb+1) # return the NEXT LINE after the line which contains content
      break

    }

  }

  returnVal

}


#' Document Cursor Selection
#'
#' Identify the selection from the Active Cursor position in current R Studio file.
#'
#' First identifies if the current Cursor position is on a GROUP (HEADER NOTE or SUBNOTE).
#' It then searches back through the Active Document to find the first line starting with
#' "##" - if this is a "#### TASK" line, then the first previous "### DELIVERABLE" line and
#' the first previous "### GOAL" line is also identified.
#'
#' If TASK, DELIVERABLE and GOAL lines are all successfully found, this method returns
#' a LIST:
#'
#' [[1]] or [["task"]] - contains the line that starts "#### TASK" in the Active Document.
#'
#' [[2]] or [["taskLine"]] - the task line number (index) in the Active Document.
#'
#' [[3]] or [["deliverable"]] - contains the line that starts "### DELIVERABLE" in the Active Document.
#'
#' [[4]] or [["deliverableLine"]] - the deliverable line number (index) in the Active Document.
#'
#' [[5]] or [["goal"]] - contains the line that starts "## GOAL" in the Active Document.
#'
#' [[6]] or [["goalLine"]] - the goal line number (index) in the Active Document.
#'
#' [[7]] or [["originalLine"]] - the original line where the Active Cursor was on in the Active Document.
#'
#' [[8]] or [["originalLineNumber"]] - the original line number where the Active Cursor was on in the Active Document.
#'
#' [[9]] or [["addingSubNote"]] - a Boolean value indicating the Cursor was on a GROUP (HEADER NOTE or SUBNOTE).
#'
#' [[10]] or [["headerNoteLink"]] - if addingSubNote is TRUE, contains the link to the headerSubNote, AS IS GIVEN
#' IN THE PROJECT DO (i.e. the link TITLE and then the link ADDRESS - for example
#' **[LAB~001-00~ GARS SC Clearing Labelling](../LAB/LAB~001-00~_GARS_SC_Clearing_Labelling.Rmd)**), else is BLANK
#'
#' [[11]] or [["headerNoteLineNumber"]] - if addingSubNote is TRUE, contains the line that contains the headerSubNoteLink, else is BLANK.
#'
#' [[12]] or [["projectDocPath"]] - the path of the Active Doc (a Project Doc, as successfully retrieved Task/Del/Goal)
#'
#'
#' If this method is unsuccessful (there is an error), it returns a LIST:
#'
#' [[1]] - contains the String "FALSE"
#' [[2]] - contains the errorMessage - a String indicating why the method failed.
#'
#'
cursorSelection <- function() {

  taskRetrieved <- TRUE

  originalLine <- ""
  task <- ""
  deliverable <- ""
  goal <- ""

  taskLine <- 1
  delLine <- 1
  goalLine <- 1

  headerRetrieved <- TRUE

  errorMessage <- ""

  addingSubNote <- FALSE
  headerNoteLink <- ""
  headerNoteLineNumber <- 0

  context <- rstudioapi::getActiveDocumentContext()

  # first ENSURE the current file is saved:
  rstudioapi::documentSave(context$id)

  original <- context$contents

  cursor <- rstudioapi::primary_selection(context)
  line <- (cursor$range[[1]])[1] # get the line number of cursor

  # store the originalLineNumber:
  originalLineNumber <- line

  lineContent <- original[ line ] # retrieve the CONTENT of the line with the cursor on
  originalLine <- lineContent

  # identify if the selected line is selecting a HEADER NOTE
  # if a HEADER NOTE is selected, the line will contain the string "-00~"
  # if a SUBNOTE is selected, the line will contain the string "* ["
  if( grepl("-00~", lineContent, fixed = TRUE) || grepl("* [", lineContent, fixed = TRUE) ) {

    addingSubNote <- TRUE

    # find the headerNote:
    if( grepl("-00~", lineContent, fixed = TRUE) ) {
      headerNoteLink <- lineContent
      headerNoteLineNumber <- line
    }
    else {
      for(l in line:1) {
        lineContent <- original[ l ]
        if( grepl("-00~", lineContent, fixed = TRUE) ) {
          headerNoteLink <- lineContent
          headerNoteLineNumber <- l
          break
        }
      }
    }
    if(headerNoteLink == "" ) { # if headerNoteLink not found, set errorMessage:
      headerRetrieved <- FALSE
      errorMessage <- "Could Not Identify HEADER NOTE"
    }
  }

  # Search BACK through lineContent to find the first line that starts with "##"
  for(l in line:1) {
    lineContent <- original[ l ]
    if(substring(lineContent, 1, 2)  == "##") {
      line <- l
      break
    }
  }

  # Now check if lineContent is a Task
  taskString <- substring(lineContent, 1, 9)

  if(taskString=="#### TASK") { # if so, extract the first DELIVERABLE and GOAL:

    task <- lineContent
    taskLine <- line

    # Extract "### DELIVERABLE" line
    for(l in line:1) {
      lineContent <- original[ l ]
      if(substring(lineContent, 1, 15)  == "### DELIVERABLE") {
        deliverable <- lineContent
        line <- l
        break
      }
    }

    if( line == taskLine ) {
      # didnt find ### DELIVERABLE - set error message
      taskRetrieved <- FALSE
      errorMessage <- "Could Not Locate TASK/DEL/GOAL"
    }

    delLine <- line

    # Extract "## GOAL" line
    for(l in line:1) {
      lineContent <- original[ l ]
      if(substring(lineContent, 1, 7)  =="## GOAL") {
        goal <- lineContent
        line <- l
        break
      }
    }

    if( line == delLine ) {
      # didnt find ## GOAL - set error message
      taskRetrieved <- FALSE
      errorMessage <- "Could Not Locate TASK/DEL/GOAL"
    }

    goalLine <- line

  }
  else { # if task not found, set errorMessage:
    taskRetrieved <- FALSE
    errorMessage <- "Could Not Locate TASK/DEL/GOAL"
  }



  # form the output for this Function:

  if( taskRetrieved == TRUE && headerRetrieved == TRUE ) {

    #taskNum <- as.integer(  substring(task,  first=11, last=(regexpr(":", task)-1) )  )
    #delNum <- as.integer(  substring(deliverable,  first=17, last=(regexpr(":", deliverable)-1) )  )
    #goalNum <- as.integer(  substring(goal,  first=9, last=(regexpr(":", goal)-1) )  )

    #taskTitle <- substring(task,  first=(regexpr(":", task)+2 ) )
    #delTitle <- substring(deliverable,  first=(regexpr(":", deliverable)+2 ) )
    #goalTitle <- substring(goal,  first=(regexpr(":", goal)+2 ) )

    output <- list( task, taskLine, deliverable, delLine, goal, goalLine,
                    originalLine, originalLineNumber,
                    addingSubNote, headerNoteLink, headerNoteLineNumber,
                    context$path )

    names(output) <- c( "task", "taskLine", "deliverable", "delLine", "goal", "goalLine",
                        "originalLine", "originalLineNumber",
                        "addingSubNote", "headerNoteLink", "headerNoteLineNumber",
                        "projectDocPath")

    output

  }
  else {

    list( "FALSE", errorMessage )

  }

}


#' Document User Selection
#'
#' Create a selection list from a projectDocPath and line number.
#'
#' projectDocPath - must be an ABSOLUTE PATH if using the output for addProjectNote() or similar
#' methods.
#'
#' line - the line number to generate the selection list from.
#'
#'
#' This function first identifies if the line in projectDocPath is on a GROUP (HEADER NOTE or SUBNOTE).
#' It then searches back through the Active Document to find the first line starting with
#' "##" - if this is a "#### TASK" line, then the first previous "### DELIVERABLE" line and
#' the first previous "### GOAL" line is also identified.
#'
#' If TASK, DELIVERABLE and GOAL lines are all successfully found, this method returns
#' a LIST:
#'
#' [[1]] or [["task"]] - contains the line that starts "#### TASK" in the Active Document.
#'
#' [[2]] or [["taskLine"]] - the task line number (index) in the Active Document.
#'
#' [[3]] or [["deliverable"]] - contains the line that starts "### DELIVERABLE" in the Active Document.
#'
#' [[4]] or [["deliverableLine"]] - the deliverable line number (index) in the Active Document.
#'
#' [[5]] or [["goal"]] - contains the line that starts "## GOAL" in the Active Document.
#'
#' [[6]] or [["goalLine"]] - the goal line number (index) in the Active Document.
#'
#' [[7]] or [["originalLine"]] - the original line where the Active Cursor was on in the Active Document.
#'
#' [[8]] or [["originalLineNumber"]] - the original line number where the Active Cursor was on in the Active Document.
#'
#' [[9]] or [["addingSubNote"]] - a Boolean value indicating the Cursor was on a GROUP (HEADER NOTE or SUBNOTE).
#'
#' [[10]] or [["headerNoteLink"]] - if addingSubNote is TRUE, contains the link to the headerSubNote, AS IS GIVEN
#' IN THE PROJECT DO (i.e. the link TITLE and then the link ADDRESS - for example
#' **[LAB~001-00~ GARS SC Clearing Labelling](../LAB/LAB~001-00~_GARS_SC_Clearing_Labelling.Rmd)**), else is BLANK
#'
#' [[11]] or [["headerNoteLineNumber"]] - if addingSubNote is TRUE, contains the line that contains the headerSubNoteLink, else is BLANK.
#'
#' [[12]] or [["projectDocPath"]] - the projectDocPath
#'
#'
#' If this method is unsuccessful (there is an error), it returns a LIST that contains:
#'
#' [[1]] - contains the String "FALSE"
#' [[2]] - contains the errorMessage - a String indicating why the method failed.
#'
#' @export
userSelection <- function(projectDocPath, line) {

  taskRetrieved <- TRUE

  originalLine <- ""
  task <- ""
  deliverable <- ""
  goal <- ""

  taskLine <- 1
  delLine <- 1
  goalLine <- 1

  headerRetrieved <- TRUE

  errorMessage <- ""

  addingSubNote <- FALSE
  headerNoteLink <- ""
  headerNoteLineNumber <- 0


  # store the originalLineNumber:
  originalLineNumber <- line

  # read Project Doc:
  projectDocConn <- file(projectDocPath)
  projectDocContents <- readLines( projectDocConn )
  close(projectDocConn)


  lineContent <- projectDocContents[ line ] # retrieve the CONTENT at line

  originalLine <- lineContent # store original line content


  # identify if the selected line is selecting a HEADER NOTE
  # if a HEADER NOTE is selected, the line will contain the string "-00~"
  # if a SUBNOTE is selected, the line will contain the string "* **["
  if( grepl("-00~", lineContent, fixed=TRUE) || grepl("* [", lineContent, fixed=TRUE) ) {

    addingSubNote <- TRUE

    # find the headerNote:
    if( grepl("-00~", lineContent, fixed=TRUE) ) {
      headerNoteLink <- lineContent
      headerNoteLineNumber <- line
    }
    else {
      for(l in line:1) {
        lineContent <- projectDocContents[ l ]
        if( grepl("-00~", lineContent, fixed=TRUE) ) {
          headerNoteLink <- lineContent
          headerNoteLineNumber <- l
          break
        }
      }
    }
    if(headerNoteLink == "" ) { # if headerNoteLink not found, set errorMessage:
      headerRetrieved <- FALSE
      errorMessage <- "Could Not Identify HEADER NOTE"
    }
  }

  # Search BACK through lineContent to find the first line that starts with "##"
  for(l in line:1) {
    lineContent <- projectDocContents[ l ]
    if(substring(lineContent, 1, 2)  == "##") {
      line <- l
      break
    }
  }

  # Now check if lineContent is a Task
  taskString <- substring(lineContent, 1, 9)

  if(taskString=="#### TASK") { # if so, extract the first DELIVERABLE and GOAL:

    task <- lineContent
    taskLine <- line

    # Extract "### DELIVERABLE" line
    for(l in line:1) {
      lineContent <- projectDocContents[ l ]
      if(substring(lineContent, 1, 15)  == "### DELIVERABLE") {
        deliverable <- lineContent
        line <- l
        break
      }
    }

    if( line == taskLine ) {
      # didnt find ### DELIVERABLE - set error message
      taskRetrieved <- FALSE
      errorMessage <- "Could Not Locate TASK/DEL/GOAL"
    }

    delLine <- line

    # Extract "## GOAL" line
    for(l in line:1) {
      lineContent <- projectDocContents[ l ]
      if(substring(lineContent, 1, 7)  =="## GOAL") {
        goal <- lineContent
        line <- l
        break
      }
    }

    if( line == delLine ) {
      # didnt find ## GOAL - set error message
      taskRetrieved <- FALSE
      errorMessage <- "Could Not Locate TASK/DEL/GOAL"
    }

    goalLine <- line

  }
  else { # if task not found, set errorMessage:
    taskRetrieved <- FALSE
    errorMessage <- "Could Not Locate TASK/DEL/GOAL"
  }



  # form the output for this Function:

  if( taskRetrieved == TRUE && headerRetrieved == TRUE ) {

    #taskNum <- as.integer(  substring(task,  first=11, last=(regexpr(":", task)-1) )  )
    #delNum <- as.integer(  substring(deliverable,  first=17, last=(regexpr(":", deliverable)-1) )  )
    #goalNum <- as.integer(  substring(goal,  first=9, last=(regexpr(":", goal)-1) )  )

    #taskTitle <- substring(task,  first=(regexpr(":", task)+2 ) )
    #delTitle <- substring(deliverable,  first=(regexpr(":", deliverable)+2 ) )
    #goalTitle <- substring(goal,  first=(regexpr(":", goal)+2 ) )

    output <- list( task, taskLine, deliverable, delLine, goal, goalLine,
                    originalLine, originalLineNumber,
                    addingSubNote, headerNoteLink, headerNoteLineNumber,
                    projectDocPath )

    names(output) <- c( "task", "taskLine", "deliverable", "delLine", "goal", "goalLine",
                        "originalLine", "originalLineNumber",
                        "addingSubNote", "headerNoteLink", "headerNoteLineNumber",
                        "projectDocPath")

    output

  }
  else {

    list( "FALSE", errorMessage )

  }

}


#' Replace and insert a vector
#'
#' This function will find the FIRST INSTANCE of the exact match pattern in the passed vector, x,
#' and replace this element with all elements in the vector, values.
#'
replaceAndInsertVector <- function( pattern, values, x ) {

  index <- match(pattern, x)

  c(x[1:(index-1)], values, x[(index+1):length(x)])

}



#' Get RStudio Open Document IDs
#'
#' Returns a list of IDs for all open RStudio Documents, and their paths.
#'
#' fileList[[i]][1] - RStudio Document ID
#'
#' fileList[[i]][2] - RStudio Document Absolute Path.
#'
#'
getRStudioOpenDocIDs <- function() {

  #cat( "\nprojectmanagr::getRstudioDocIDs():\n" )

  # first, check if ~/.rstudio-desktop/sources exists
  if( file.exists("~/.rstudio-desktop/sources") ) {

    # check if a DIR exists which STARTS WITH "s-" (remainder is unique RStudio session ID)
    if ( file.exists(  Sys.glob("~/.rstudio-desktop/sources/s-*")  )  ) {

      # get file list
      fileIDs <- list.files(  Sys.glob("~/.rstudio-desktop/sources/s-*")  )
      # look in the "s-*" DIR for every file that does NOT end in "-contents"
      fileIDs <- fileIDs[lapply(fileIDs,function(x) length(grep("-contents",x,value=FALSE))) == 0]
      # nor have name "lock_file"
      fileIDs <- fileIDs[lapply(fileIDs,function(x) length(grep("lock_file",x,value=FALSE))) == 0]

      # convert to full path:
      filePath <- paste0(Sys.glob("~/.rstudio-desktop/sources/s-*"), .Platform$file.sep, fileIDs)

      # inside remaining files, if saved to disk, will be a line starting 'path" : "[path_to_file]",'
      # if not saved to disk, will contain the line '"path" : null,'
      # Extract the value of path for each entry in filePath:
      fileList <- list()
      for (i in 1:length(filePath) ) {

        # put path into fileList
        fileList[[i]] <- fileIDs[i]

        # open file
        FileConn <- file( filePath[i] )
        lines <- readLines( FileConn, warn = FALSE )
        close(FileConn)

        # extract path string:
        pathLine <- lines[grepl("    \"path\"*", lines)]

        if( grepl("\"path\" : null,", pathLine) ) {
          # path is null - file has not been saved - store "null" in fileList:
          fileList[[i]][2] <- "null"
        }
        else {

          # copy path into fileList - second index
          fileList[[i]][2] <- substring(pathLine, 15, nchar(pathLine)-2)
        }

      }

    }
    else {

      #cat("  ~/.rstudio-desktop does not exist\n")
      stop( cat("No Active RStudio Session\n") )

    }
  }
  else {

    #cat("  ~/.rstudio-desktop does not exist\n")
    stop( cat("No Active RStudio Session\n") )

  }

  # return fileList:
  fileList


}

