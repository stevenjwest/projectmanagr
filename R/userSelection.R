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
