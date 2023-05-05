
#' Compute Project Doc Goal Del Task Link
#'
#' Compute the components that form the Project Doc Goal Del Task Link, for
#' insertion into a linked Project Note.
#'
compute_doc_GDT_link <- function(projectDocPath, projNoteRmdPath, settings,
                                      goal, deliverable, task) {


  #### instance variables ####

  DocLink <- R.utils::getRelativePath(projectDocPath, relativeTo=projNoteRmdPath)
  DocLink <- substring(DocLink, first=4, last=nchar(DocLink)) # remove first `../`
  # DocLink <- paste( substring(DocLink, first=1, last=nchar(DocLink)-4), "/", sep="")
  # for now have left links as ".Rmd", but this needs to be ".html", or "/" in a rendered website!
  # Its set as ".Rmd" as ".Rmd" links can be navigated in RStudio!  CONVERT LINKS in update() function

  DocName <- basename(projectDocPath)
  DocName <- substring(DocName, first=1, last=nchar(DocName)-4)


  #### link ####

  DocTitleLink <- paste0( settings[["ProjectLinkFormat"]],
                          "[", DocName, "](", DocLink, ")", settings[["ProjectLinkFormat"]])


  #### GOAL ####

  GoalTitleLink <- compute_goal_link(goal, DocLink, settings)


  #### DEL ####

  DelTitleLink <- compute_deliverable_link(deliverable, DocLink, settings)


  #### TASK ####

  TaskTitleLink <- compute_task_link(task, DocLink, settings)


  #### TITLE ####

  # create DocTitle - DocName plus the Gnum Dnum Tnum
  #DocTitle <- paste( "## ", DocName, " : G", goalNum, " D", delNum, " T", taskNum, sep="")
  # create DocTitle - DocName plus the TaskTitle
  DocTitle <- paste0( settings[["NoteSummaryTitle"]] ,
                      DocName, " : ", get_task_title(task, settings) )

  # return
  l <- list(DocTitle, DocTitleLink, GoalTitleLink, DelTitleLink, TaskTitleLink)
  names(l) <- c("title", "link", "goal", "del", "task")
  l

}


#' compute goal link
#'
#' generate new link from project doc with html # tag for goal section.
compute_goal_link <- function(goal, DocLink, settings) {


  #### goal link ####

  # get start of goal link line WITHOUT [+GoalTitle - will use goal_trim to fill link title
  goalLinkLineStart <- unlist(strsplit(settings[["NoteGoalLinkLine"]],
                                       split=paste0("[",settings[["ProjectGoalTitle"]]), fixed=TRUE))

  # identify the length of goal header - default '# GOAL' minus GoalTitle - default 'GOAL'
  # +1 for substring
  glen <- nchar(unlist(strsplit(settings[["ProjectGoalHeader"]],
                                split=settings[["ProjectGoalTitle"]], fixed=TRUE)))+1

  # get goal without the header portion
  goal_trim <- substring(goal, first=glen)

  # create html tag to section - START #, replace space & _ with -, remove :, all lower case
  goalTag <- paste0("#", gsub("[ ]|[_]", "-", gsub("[:]", "", tolower(goal_trim) ) ), ")" )

  # return formatted link with title goal_trim & link to doc + goalTag
  paste0(goalLinkLineStart, "[", goal_trim, "](", DocLink, goalTag)

}

#' get goal title
#'
#' `goal` as collected from selection[["goal"]] - fitting the format specified
#' in settings[["ProjectGoalHeader"]].
#'
get_goal_title <- function(goal, settings) {

  # return the goal's Title : everything after ProjectGoalDivider - with whitespace removed
  trimws(substring(goal,  first=(regexpr(settings[["ProjectGoalDivider"]], goal)+1 ) ))

}

#' get goal number
#'
#' `goal` as collected from selection[["goal"]] - fitting the format specified
#' in settings[["ProjectGoalHeader"]].
#'
get_goal_number <- function(goal, settings) {

  # return the goal's Number : everything after ProjectGoalHeader & before ProjectGoalDivider - with whitespace removed
  as.integer( trimws(  substring(goal,
               first=(regexpr(settings[["ProjectGoalHeader"]],goal) + nchar(settings[["ProjectGoalHeader"]])),
               last=(regexpr(settings[["ProjectGoalDivider"]],goal)-1) )  )   )

}


#' compute deliverable link
#'
#' generate new link from project doc with html # tag for deliverable section.
compute_deliverable_link <- function(del, DocLink, settings) {


  #### deliverable link ####

  # get start of del link line WITHOUT [+DelTitle - will use del_trim to fill link title
  delLinkLineStart <- unlist(strsplit(settings[["NoteDeliverableLinkLine"]],
                                      split=paste0("[",settings[["ProjectDeliverableTitle"]]), fixed=TRUE))

  # identify the length of del header - default '# DELIVERABLE' minus DelTitle - default 'DELIVERABLE'
  # +1 for substring
  dlen <- nchar(unlist(strsplit(settings[["ProjectDeliverableHeader"]],
                                split=settings[["ProjectDeliverableTitle"]], fixed=TRUE)))+1

  # get del without the header portion
  del_trim <- substring(del, first=dlen)

  # create html tag to section
  delTag <- paste0("#", gsub("[ ]|[_]", "-", gsub("[:]", "", tolower(del_trim) ) ), ")" )

  # return formatted link with title del_trim & link to doc + delTag
  paste0(delLinkLineStart, "[", del_trim, "](", DocLink, delTag)

}

#' get deliverable title
#'
#' `deliverable` as collected from selection[["deliverable"]] - fitting the format specified
#' in settings[["ProjectDeliverableHeader"]].
#'
get_deliverable_title <- function(deliverable, settings) {

  # return the del's Title : everything after ProjectGoalDivider - with whitespace removed
  trimws(substring(deliverable,  first=(regexpr(settings[["ProjectDeliverableDivider"]], deliverable)+1 ) ))

}

#' get deliverable number
#'
#' `deliverable` as collected from selection[["deliverable"]] - fitting the format specified
#' in settings[["ProjectDeliverableHeader"]].
#'
get_deliverable_number <- function(deliverable, settings) {

  # return the del's Number : everything after ProjectDelHeader & before ProjectDelDivider - with whitespace removed
  as.integer( trimws(  substring(deliverable,
                                 first=(regexpr(settings[["ProjectDeliverableHeader"]],deliverable) + nchar(settings[["ProjectDeliverableHeader"]])),
                                 last=(regexpr(settings[["ProjectDeliverableDivider"]],deliverable)-1) )  )   )

}



#' compute deliverable link
#'
#' generate new link from project doc with html # tag for deliverable section.
compute_task_link <- function(task, DocLink, settings) {


  #### task link ####

  # get start of goal link line WITHOUT [+GoalTitle - will use goal_trim to fill link title
  taskLinkLineStart <- unlist(strsplit(settings[["NoteTaskLinkLine"]],
                                       split=paste0("[",settings[["ProjectTaskTitle"]]), fixed=TRUE))

  # identify the length of task header - default '### TASK' minus TaskTitle - default 'TASK'
  # +1 for substring
  tlen <- nchar(unlist(strsplit(settings[["ProjectTaskHeader"]],
                                split=settings[["ProjectTaskTitle"]], fixed=TRUE)))+1

  # get task without the header portion
  task_trim <- substring(task, first=tlen)

  # create html tag to section - START #, replace space & _ with -, remove :, all lower case
  taskTag <- paste0("#", gsub("[ ]|[_]", "-", gsub("[:]", "", tolower(task_trim) ) ), ")" )

  # return formatted link with title task_trim & link to doc + taskTag
  paste0(taskLinkLineStart, "[", task_trim, "](", DocLink, taskTag)

}

#' get task title
#'
#' `task` as collected from selection[["task"]] - fitting the format specified
#' in settings[["ProjectTaskHeader"]].
#'
get_task_title <- function(task, settings) {

  # return the task's Title : everything after ProjectTaskDivider - with whitespace removed
  trimws(substring(task,  first=(regexpr(settings[["ProjectTaskDivider"]], task)+1 ) ))

}

#' get task number
#'
#' `task` as collected from selection[["task"]] - fitting the format specified
#' in settings[["ProjectTaskHeader"]].
#'
get_task_number <- function(task, settings) {

  # return the task's Number : everything after ProjectTaskHeader & before ProjectTaskDivider - with whitespace removed
  as.integer( trimws(  substring(task,
                                 first=(regexpr(settings[["ProjectTaskHeader"]],task) + nchar(settings[["ProjectTaskHeader"]])),
                                 last=(regexpr(settings[["ProjectTaskDivider"]],task)-1) )  )   )

}



#' Get Project Note Paths from Doc GDT links
#'
#' Get project note paths from doc GDT links  Assumes `projDocContents` is a
#' character vector from a project doc.
#'
get_doc_gdt_project_note_paths <- function(projectDocPath, projDocContents, settings) {

  projectDocParent <- dirname( normalizePath(projectDocPath))
  orgPath <- find_org_directory(projectDocParent)


  #### split projDocContents into each TASK ####

  # indices of each task HEADER
  taskHeaderIndices <- grep( trimws(settings[["ProjectTaskHeader"]]), trimws(projDocContents), fixed=TRUE)

  # index of each task LOG HEADER from each task HEADER
  taskLogIndices <- c()
  fi <- 1
  for(head in taskHeaderIndices) {
    taskLogIndices[fi] <- grep_line_index_from(load_param_vector(settings[["ProjectTaskLogHeader"]], orgPath),
                                                  projDocContents, head)
    fi <- fi+1
  }

  # index of each task FOOTER from each task HEADER
  taskFooterIndices <- c()
  fi <- 1
  for(head in taskHeaderIndices) {
    taskFooterIndices[fi] <- grep_line_index_from(load_param_vector(settings[["ProjectTaskFooter"]], orgPath),
                                                  projDocContents, head)
    fi <- fi+1
  }


  #### get each project note absolute path linked to GDTs ####

  # loop through index of taskLogIndices
  for(i in 1:length(taskLogIndices) ) {

    taskContents <- projDocContents[ (taskLogIndices[i]):(taskFooterIndices[i]) ]

    # find lines that start with note link format && contain a link
    pnLines <- taskContents[startsWith(taskContents, paste0(settings[["NoteLinkFormat"]],"[") ) & grepl("](", taskContents, fixed=TRUE)]
    snLines <- taskContents[startsWith(taskContents, paste0(settings[["SubNoteLinkFormat"]],"[") ) & grepl("](", taskContents, fixed=TRUE)]
    lines <- c(pnLines, snLines)

    # extract the absolute path from each link
    relLinks <- substr(lines, regexpr("](", lines, fixed=TRUE)+2, regexpr(")", lines, fixed=TRUE)-1 )
    absPaths <- R.utils::getAbsolutePath( paste0(projectDocParent, .Platform$file.sep, relLinks))

  }

  # return
  absPaths
}


#' Extract Project Note Doc Links GDT & Summary
#'
#' Returns a list containing string vectors with projectDocFilePath, goal,
#' deliverable, task for all objectives, and summary & todo for single &
#' subnotes (not header notes).
#'
#' If no objectives are identified, a BLANK LIST `list()` is returned.
#'
extract_note_obj_doc_link_GDT_summ <- function(linkNoteRmdContents, linkNoteRmdPath,
                                               settings, orgPath) {

  # linkNoteRmdContents contains all ProjDoc GDT links to add to new subnote

  type <- get_file_type(linkNoteRmdPath, settings)

  # isolate the objectives segment of contents
  linkObjHeadIndex <- match_line_index( load_param_vector(settings[["NoteObjectivesHeader"]], orgPath),
                                      linkNoteRmdContents)+1
  linkObjFootIndex <- grep_line_index_from( load_param_vector(settings[["NoteObjectivesFooter"]], orgPath),
                                            linkNoteRmdContents, linkObjHeadIndex)-1

  linkObjectives <- linkNoteRmdContents[ linkObjHeadIndex:linkObjFootIndex ]
  # this contains all the links plus summary & todo information inserted in
  # current linkNoteRmd so cannot just extract as-is - return projDoc ABSOLUTE PATH

  #### split objectives list ####

  # split linkObjectives into each objective : ProjDoc Link plus GDT
  objList <- list()
  objSplit <- match_vector(load_param_vector(settings[["NoteObjectivesSep"]], orgPath), linkObjectives)

  if( identical(objSplit, integer(0)) ) {
    # no objectives are identified!
    # this may be the case for header notes with no objective
    # return a blank list
    return(list())
  }

  objSplitMax <- c(objSplit, length(linkObjectives)+1)
  for( o in 1:length(objSplit) ) {
    objList[[o]] <- linkObjectives[(objSplitMax[o]):(objSplitMax[o+1]-1)]
  }

  #### For each Objective ####

  # extract ProjDoc absolute path plus GDT from each objective
  DocGDTsList <- list()
  dGDTi <- 1

  for( o in objList ) {


    ##### Extract ProjDoc Path #####

    # projdoc - string to start with ProjectLinkFormat plus '['
    pdli <- grep_line_index(paste0(settings[["ProjectLinkFormat"]], "["), o) # projDoc link index
    gli <- grep_line_index_from(settings[["NoteGoalLinkLine"]], o, pdli) # goal index
    dli <- grep_line_index_from(settings[["NoteDeliverableLinkLine"]], o, gli) # del index
    tli <- grep_line_index_from(settings[["NoteTaskLinkLine"]], o, dli) # task index
    if(type != "HEAD") { # summary and todo sections only exist for NON HEAD notes
      summi <- grep_line_index_from(settings[["NoteObjectivesSummarySectionHeader"]], o, tli) # summary index
      todoi <- grep_line_index_from(settings[["NoteObjectivesTodoSectionHeader"]], o, summi) # todo index
    }

    # get ProjDoc RelativePath
    pdrp <- substr(o[pdli], regexpr("](", o[pdli], fixed=TRUE)+2, regexpr(")", o[pdli], fixed=TRUE)-1)

    # compute ProjDoc AbsolutePath
    pdap <- R.utils::getAbsolutePath( paste0(dirname(linkNoteRmdPath), .Platform$file.sep, pdrp))


    #### Extract Goal Del Task Headers ####

    #compute goal, del, task titles from links
    goal <- paste0(
      unlist(strsplit(settings[["ProjectGoalHeader"]],
                      split=paste0(settings[["ProjectGoalTitle"]]), fixed=TRUE)),
      substr(o[gli], regexpr(settings[["ProjectGoalTitle"]], o[gli], fixed=TRUE),
                   regexpr("](", o[gli], fixed=TRUE)-1)
    )

    del <- paste0(
      unlist(strsplit(settings[["ProjectDeliverableHeader"]],
                      split=paste0(settings[["ProjectDeliverableTitle"]]), fixed=TRUE)),
      substr(o[dli], regexpr(settings[["ProjectDeliverableTitle"]], o[dli], fixed=TRUE),
             regexpr("](", o[dli], fixed=TRUE)-1)
    )

    task <- paste0(
      unlist(strsplit(settings[["ProjectTaskHeader"]],
                      split=paste0(settings[["ProjectTaskTitle"]]), fixed=TRUE)),
      substr(o[tli], regexpr(settings[["ProjectTaskTitle"]], o[tli], fixed=TRUE),
             regexpr("](", o[tli], fixed=TRUE)-1)
    )


    #### Extract Goal Del Task Summary & TODO Sections ####

    if(type != "HEAD") { # summary and todo sections only exist for NON HEAD notes
      summLen <- length(settings[["NoteObjectivesSummarySectionHeader"]])
      todoLen <- length(settings[["NoteObjectivesTodoSectionHeader"]])
      summary <- o[(summi+summLen):(todoi-1)]
      todo <- o[(todoi+todoLen):length(o)]
    } else {
      summary <- ""
      todo <- "" # if HEAD note set to BLANK
    }


    #### Add to list ####

    dgdt <- list(pdap, goal, del, task, summary, todo)
    names(dgdt) <- c("projectDocFilePath",
                     "goal", "deliverable", "task",
                     "summary", "todo")
    DocGDTsList[[dGDTi]] <- dgdt
    dGDTi <- dGDTi+1
  }

  DocGDTsList

}


#' Extract GDT & summary & TODO from Project Note Objectives vector
#'
#'
#'
extract_objectives_note_GDT <- function(linkObjectives, linkRmdPath, subNoteRmdPath,
                                       settings, orgPath) {

  #### split objectives list ####

  # split linkObjectives into each objective : ProjDoc Link plus GDT
  objList <- list()
  objSplit <- match_vector(load_param_vector(settings[["NoteObjectivesSep"]], orgPath), linkObjectives)
  objSplitMax <- c(objSplit, length(linkObjectives))
  for( o in 1:length(objSplit) ) {
    objList[[o]] <- linkObjectives[(objSplitMax[o]):(objSplitMax[o+1])]
  }

  #### For each Objective ####

  # extract ProjDoc absolute path plus GDT from each objective
  DocGDTsList <- list()
  dGDTi <- 1

  for( o in objList ) {


    ##### Extract ProjDoc Path #####

    # projdoc - string to start with ProjectLinkFormat plus '['
    pdli <- grep_line_index(paste0(settings[["ProjectLinkFormat"]], "["), o) # projDoc link index
    gli <- grep_line_index_from(settings[["NoteGoalLinkLine"]], o, pdli) # goal index
    dli <- grep_line_index_from(settings[["NoteDeliverableLinkLine"]], o, gli) # del index
    tli <- grep_line_index_from(settings[["NoteTaskLinkLine"]], o, dli) # task index
    summi <- grep_line_index_from(settings[["NoteObjectivesSummarySectionHeader"]], o, tli) # summary index
    todoi <- grep_line_index_from(settings[["NoteObjectivesTodoSectionHeader"]], o, summi) # todo index

    # get ProjDoc RelativePath
    pdrp <- substr(o[pdli], regexpr("](", o[pdli], fixed=TRUE)+2, regexpr(")", o[pdli], fixed=TRUE)-1)

    # compute ProjDoc AbsolutePath
    pdap <- R.utils::getAbsolutePath( paste0(dirname(linkRmdPath), .Platform$file.sep, pdrp))


    #### Extract Goal Del Task Headers ####

    #compute goal, del, task titles from links
    goal <- paste0(
      unlist(strsplit(settings[["ProjectGoalHeader"]],
                      split=paste0(settings[["ProjectGoalTitle"]]), fixed=TRUE)),
      substr(o[gli], regexpr(settings[["ProjectGoalTitle"]], o[gli], fixed=TRUE),
             regexpr("](", o[gli], fixed=TRUE)-1)
    )

    del <- paste0(
      unlist(strsplit(settings[["ProjectDeliverableHeader"]],
                      split=paste0(settings[["ProjectDeliverableTitle"]]), fixed=TRUE)),
      substr(o[dli], regexpr(settings[["ProjectDeliverableTitle"]], o[dli], fixed=TRUE),
             regexpr("](", o[dli], fixed=TRUE)-1)
    )

    task <- paste0(
      unlist(strsplit(settings[["ProjectTaskHeader"]],
                      split=paste0(settings[["ProjectTaskTitle"]]), fixed=TRUE)),
      substr(o[tli], regexpr(settings[["ProjectTaskTitle"]], o[tli], fixed=TRUE),
             regexpr("](", o[tli], fixed=TRUE)-1)
    )


    #### Extract Goal Del Task Summary & TODO Sections ####
    summary <- o[summi:todoi]
    todo <- o[todoi:length(o)]


    #### Add to list ####

    dgdt <- c(pdap, goal, del, task)
    names(dgdt) <- c("projectDocFilePath", "goal", "deliverable", "task")
    DocGDTsList[[dGDTi]] <- dgdt
    dGDTi <- dGDTi+1
  }

  DocGDTsList

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
grep_line_index <- function(line, contents) {

  #### grep line in contents FIXED ####
  returnVal <- -1
  # look through contents to find an index that matches line:
  for( l in 1:length(contents) ) {

    if( grepl(line[1], contents[l], , fixed=TRUE) ) {
      returnVal <- l
      break
    }

  }

  returnVal

}


#' Grep Line Index from initialIndex to END of contents
#'
#' Returns the index of the first PARTIAL MATCH (using grep) of line in contents,
#' from initialIndex, if not found returns -1.
#'
#' @param line a String to be found
#' @param contents a vector of strings, the index of the first instance of line in
#' this vector is returned.
#'
#'
grep_line_index_from <- function(line, contents, initialIndex) {

  #### grep line in contents from initialIndex FIXED ####

  returnVal <- -1
  # look through contents to find an index that matches line:
  for( l in initialIndex:length(contents) ) {

    if( grepl(line[1], contents[l], fixed=TRUE) ) {
      returnVal <- l
      break
    }

  }

  returnVal

}


#' Grep Line Index from initialIndex to START of contents
#'
#' Returns the index of the first PARTIAL MATCH (using grep) of line in contents,
#' from initialIndex, if not found returns -1.
#'
#' @param line a String to be found
#' @param contents a vector of strings, the index of the first instance of line in
#' this vector is returned.
#'
#'
grep_line_index_from_rev <- function(line, contents, initialIndex) {

  #### grep line in contents from initialIndex FIXED ####

  returnVal <- -1
  # look through contents to find an index that matches line:
  for( l in initialIndex:1 ) {

    if( grepl(line[1], contents[l], fixed=TRUE) ) {
      returnVal <- l
      break
    }

  }

  returnVal

}

match_line_index <- function(line, contents) {

  indices <- grep( trimws(line), trimws(contents), fixed=TRUE)

  if( length(indices) == 0 ) {
    returnVal <- -1
  } else {
    returnVal <- indices[1]
  }

  returnVal

}




#' Match vector in parent vector
#'
#' Identifies each initial index of `vector` in `parent` where all elements in
#' `vector` match elements in `parent` in order.
#'
match_vector <- function(vector, parent, nomatch = 0L) {

  #### match vector in parent ####
  sieved <- which(parent == vector[1L])
  for (i in seq.int(1L, length(vector) - 1L)) {
    sieved <- sieved[parent[sieved + i] == vector[i + 1L]]
  }
  sieved
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
#' @export
cursor_selection <- function() {


  #### get source editor selection ####

  context <- rstudioapi::getSourceEditorContext()

  # first ENSURE the current file is saved:
  rstudioapi::documentSave(context$id)

  # recapture, to ensure the path is retrieved!
  context <- rstudioapi::getSourceEditorContext()


  #### get path contents & selected line ####

  filePath <- normalizePath(context$path)
  contents <- context$contents

  cursor <- rstudioapi::primary_selection(context)
  line <- (cursor$range[[1]])[1] # get the line number of cursor

  # get orgPath confPath and tempPath
  orgPath <- find_org_directory(filePath)
  if(orgPath == "" ) { # only if orgPath not identified
    stop( paste0("  filePath is not in a projectmanagr ORGANISATION: ", filePath) )
  }
  # set confPath + tempPath:
  confPath <- paste(orgPath, .Platform$file.sep, "config" , sep="")
  tempPath <- paste(confPath, .Platform$file.sep, "templates", sep="")

  # load settings file for user defined settings
  settingsFile <- paste( confPath, .Platform$file.sep, "settings.yml", sep="" )
  settings <- yaml::yaml.load( yaml::read_yaml( settingsFile ) )

  create_selection(filePath, contents, line, settings) # return selection

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
user_selection <- function(filePath, line) {

  # get orgPath confPath and tempPath
  orgPath <- find_org_directory(filePath)
  if(orgPath == "" ) { # only if orgPath not identified
    stop( paste0("  filePath is not in a projectmanagr ORGANISATION: ", filePath) )
  }
  # set confPath + tempPath:
  confPath <- paste(orgPath, .Platform$file.sep, "config" , sep="")
  tempPath <- paste(confPath, .Platform$file.sep, "templates", sep="")

  # load settings file for user defined settings
  settingsFile <- paste( confPath, .Platform$file.sep, "settings.yml", sep="" )
  settings <- yaml::yaml.load( yaml::read_yaml( settingsFile ) )

  #### read file ####

  contents <- read_file(filePath)

  create_selection(filePath, contents, line, settings) # return selection

}


#' Create Selection
#'
#' Creates a selection list generated from a filePath, its contents, and an
#' existing line.  The selection list consists of a `rmdType` named vector that
#' defines what kind of file is in the current selection: `DOC` `HEAD` `SUB`
#' `NOTE` or `FALSE`.
#'
#' If a Project Document (`DOC`) is selected, the returned list contains several
#' other useful parameters:
#'
#' From project doc filepath contents and selection line.
#'
create_selection <- function(filePath, contents, line, settings) {


  #### get Rmd file type ####

  rmdType <- get_file_type(filePath, settings)

  # store the originalLineNumber:
  originalLineNumber <- line

  lineContent <- contents[ line ] # retrieve the CONTENT of the line with the cursor on
  originalLine <- lineContent


  #### Deal with DOC Selection ####

  if( rmdType == "DOC" ) {

    taskRetrieved <- TRUE

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
    headerNoteRmdPath <- ""
    headerNoteName <- ""
    headerNoteLineNumber <- 0


    ##### Identify HEADER or SUBNOTE ####

    # identify if the selected line is selecting a HEADER OR SUB NOTE
    # if a HEADER NOTE is selected, the line will contain the string "-00~" by default
    headerString <- paste0(settings[["HeaderNotePrefix"]], settings[["ProjectPrefixSep"]])
    # if a SUBNOTE is selected, the line will contain the string "*[" by default (subnotelinkformat plus start of link)
    subNoteString <- paste0(settings[["SubNoteLinkFormat"]], "[")
    if( grepl(headerString, lineContent, fixed = TRUE) || grepl(subNoteString, lineContent, fixed = TRUE) ) {

      addingSubNote <- TRUE

      # find the headerNote:
      if( grepl(headerString, lineContent, fixed = TRUE) ) {
        headerNoteLink <- lineContent
        headerNoteLineNumber <- line
      } else {
        for(l in line:1) {
          lineContent <- contents[ l ]
          if( grepl(headerString, lineContent, fixed = TRUE) ) {
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

      # get headerNoteName
      hnrp <- substr(headerNoteLink,
                     regexpr("](", headerNoteLink, fixed=TRUE)+2,
                     nchar(headerNoteLink) )
      hnrp <- substr(hnrp, 1, regexpr(")", hnrp, fixed=TRUE)-1)
      # combine with filePath to get the full path
      headerNoteRmdPath <- R.utils::getAbsolutePath(
        paste0(dirname(filePath), .Platform$file.sep, hnrp) )
      headerNoteFileName <- basename(headerNoteRmdPath)
      # now extract the headerNoteName from filename
      headerNoteName <- substring(headerNoteFileName,
                                  first=regexpr(settings[["ProjectPrefixSep"]], headerNoteFileName, fixed=TRUE) + nchar(settings[["ProjectPrefixSep"]]),
                                  last=regexpr( paste0(".", settings[["FileTypeSuffix"]]), headerNoteFileName, fixed=TRUE)-1  )

    }


    #### Find TASK DEL GOAL ####

    # crop project doc content to include content up to selection line
    contentsCrop <- contents[1:line]

    # identify line indices that starts with TASK DEL GOAL Strings
    taskIndices <- which( startsWith(contentsCrop, settings[["ProjectTaskHeader"]]) )
    delIndices <- which( startsWith(contentsCrop, settings[["ProjectDeliverableHeader"]]) )
    goalIndices <- which( startsWith(contentsCrop, settings[["ProjectGoalHeader"]]) )

    if(length(taskIndices) == 0 | length(delIndices) == 0 | length(goalIndices) == 0) {

      # if task/del/goal not found, set errorMessage:
      taskRetrieved <- FALSE
      errorMessage <- "Could Not Locate TASK/DEL/GOAL"

    } else {

      # task del goal to retrieve is the LAST in indices
      taskLine <- taskIndices[length(taskIndices)]
      delLine <- delIndices[length(delIndices)]
      goalLine <- goalIndices[length(goalIndices)]

      # get string for task del goal
      task <- contentsCrop[taskLine]
      deliverable <- contentsCrop[delLine]
      goal <- contentsCrop[goalLine]

    }

    #### return goal del task plus header note info ####

    if( taskRetrieved == TRUE && headerRetrieved == TRUE ) {

      output <- list( rmdType, task, taskLine, deliverable, delLine,
                      goal, goalLine,
                      originalLine, originalLineNumber,
                      addingSubNote, headerNoteLink,
                      headerNoteRmdPath, headerNoteName,
                      headerNoteLineNumber,
                      filePath )

      names(output) <- c( "rmdType", "task", "taskLine", "deliverable", "delLine",
                          "goal", "goalLine",
                          "originalLine", "originalLineNumber",
                          "addingSubNote", "headerNoteLink",
                          "headerNoteRmdPath", "headerNoteName",
                          "headerNoteLineNumber",
                          "filePath")

    }
    else {

      output <- create_no_selection(rmdType, errorMessage, filePath)

    }

  } else if( rmdType == "HEAD" ) {

    #### Deal with HEAD Selection ####

    addingSubNote <- TRUE

    # get headerNoteName from filePath
    headerNoteRmdPath <- filePath
    headerNoteFileName <- basename(filePath)
    # now extract the headerNoteName from filename
    headerNoteName <- substring(headerNoteFileName,
                                first=regexpr(settings[["ProjectPrefixSep"]], headerNoteFileName, fixed=TRUE) + nchar(settings[["ProjectPrefixSep"]]),
                                last=regexpr( paste0(".", settings[["FileTypeSuffix"]]), headerNoteFileName, fixed=TRUE)-1  )


    output <- list( rmdType,
                    originalLine, originalLineNumber,
                    addingSubNote,
                    headerNoteRmdPath, headerNoteName,
                    filePath )

    names(output) <- c( "rmdType",
                        "originalLine", "originalLineNumber",
                        "addingSubNote",
                        "headerNoteRmdPath", "headerNoteName",
                        "filePath")

  } else if( rmdType == "SUB" ) {

    #### Deal with SUBNOTE Selection ####

    addingSubNote <- TRUE

    headerNoteRmdPath <- find_header_Rmd_path(filePath, settings)
    headerNoteFileName <- basename(headerNoteRmdPath)
    # now extract the headerNoteName from filename
    headerNoteName <- substring(headerNoteFileName,
                                first=regexpr(settings[["ProjectPrefixSep"]], headerNoteFileName, fixed=TRUE) + nchar(settings[["ProjectPrefixSep"]]),
                                last=regexpr( paste0(".", settings[["FileTypeSuffix"]]), headerNoteFileName, fixed=TRUE)-1  )


    output <- list( rmdType,
                    originalLine, originalLineNumber,
                    addingSubNote,
                    headerNoteRmdPath, headerNoteName,
                    filePath )

    names(output) <- c( "rmdType",
                        "originalLine", "originalLineNumber",
                        "addingSubNote",
                        "headerNoteRmdPath", "headerNoteName",
                        "filePath")

  } else if( rmdType == "NOTE" ) {

    #### Deal with PROJECT NOTE Selection ####

    addingSubNote <- FALSE

    output <- list( rmdType,
                    originalLine, originalLineNumber,
                    addingSubNote,
                    filePath )

    names(output) <- c( "rmdType",
                        "originalLine", "originalLineNumber",
                        "addingSubNote",
                        "filePath")

  } else {

    #### Deal with unidentified Rmd file ####

    errorMessage <- "Unidentified file type selected."
    output <- create_no_selection("UNKNOWN", errorMessage, filePath)

  }

  output # return output

}



#' Create No Selection
#'
#' Selection object that signifies No Selection, with error message.
#'
#' Consists of a list with first item the string "FALSE", and second item
#' an `errorMessage`.
#'
#'
create_no_selection <- function(rmdType, errorMessage, filePath) {
  output <- list( rmdType, errorMessage, filePath )
  names(output) <- c( "rmdType", "errorMessage", "filePath")
  output # return output

}



#' Substitute Sub Note params in contents
#'
sub_subnote_params <- function(subNoteContents, subNotePrefix,
                               subNoteTitle, authorValue,
                               settings, orgPath) {

  #### sub subnote params in contents ####
  subNoteContents <- sub_template_param(subNoteContents, "{{PREFIX}}",
                                        subNotePrefix, orgPath)
  subNoteContents <- sub_template_param(subNoteContents, "{{TITLE}}",
                                        subNoteTitle, orgPath)
  subNoteContents <- sub_template_param(subNoteContents, "{{AUTHOR}}",
                                        authorValue, orgPath)

  subNoteContents <- sub_template_param(subNoteContents, "{{OBJECTIVES_HEADER}}",
                                        settings[["NoteObjectivesHeader"]], orgPath)
  subNoteContents <- sub_template_param(subNoteContents, "{{OBJECTIVES_FOOTER}}",
                                        settings[["NoteObjectivesFooter"]], orgPath)

  subNoteContents <- sub_template_param(subNoteContents, "{{DATA_STORAGE_HEADER}}",
                                        settings[["NoteStorageHeader"]], orgPath)
  subNoteContents <- sub_template_param(subNoteContents, "{{DATA_STORAGE_FOOTER}}",
                                        settings[["NoteStorageFooter"]], orgPath)

  subNoteContents <- sub_template_param(subNoteContents, "{{GROUP_NOTE_CONTENTS_HEADER}}",
                                        settings[["SubNoteContentsHeader"]], orgPath)
  subNoteContents <- sub_template_param(subNoteContents, "{{GROUP_NOTE_CONTENTS_FOOTER}}",
                                        settings[["SubNoteContentsFooter"]], orgPath)

  # modify subNoteContents with rmarkdown-html-header content
  subNoteContents <- replace_markdown_header(subNoteContents, orgPath)

  # modify subNoteContents with SEP values
  subNoteContents <- replace_sep_values(subNoteContents, orgPath)

  subNoteContents # return

}

#' Insert header link content into subnote
#'
insert_subnote_header_link <- function(subNoteContents, headerNoteFileName,
                                       headerNoteRmdPath, subNoteRmdPath,
                                       headerNoteContentLinkContents,
                                       settings, orgPath) {


  #### Insert header link content into subnote ####

  headerNoteContentLink <- create_hyperlink( headerNoteFileName, headerNoteRmdPath, subNoteRmdPath)
  headerNoteContentLinkContents <- sub_template_param(headerNoteContentLinkContents,
                                                      "{{SUB_NOTE_CONTENT_LINK}}",
                                                      headerNoteContentLink, orgPath)

  noteContentsHeadIndex <- match_line_index( load_param_vector(settings[["SubNoteContentsHeader"]], orgPath),
                                           subNoteContents)
  noteContentsFootIndex <- grep_line_index_from( load_param_vector(settings[["SubNoteContentsFooter"]], orgPath),
                                              subNoteContents, noteContentsHeadIndex)

  subNoteContents <- insert_at_indices(subNoteContents, noteContentsFootIndex, headerNoteContentLinkContents)

  subNoteContents # return

}


#' Substitute Note Link Summary in Contents
#'
note_link_summ_params <- function(projNoteLinkSummaryContents, todoContents, settings, orgPath) {

  #### sub headers in contents ####
  noteLinkSummContents <- sub_template_param(projNoteLinkSummaryContents,
                                             "{{OBJECTIVES_SUMMARY_HEADER}}",
                                             settings[["NoteObjectivesSummarySectionHeader"]],
                                             orgPath)

  noteLinkSummContents <- sub_template_param(noteLinkSummContents,
                                             "{{OBJECTIVES_TODO_HEADER}}",
                                             settings[["NoteObjectivesTodoSectionHeader"]],
                                             orgPath)

  # replace todoContents
  todo <- sub_template_param(todoContents, "{{TODO_ITEM_HEADER_TEMPLATE}}",
                                     settings[["TodoItemHeaderTemplate"]], orgPath)
  noteLinkSummContents <- sub_template_param(noteLinkSummContents, "{{TODO_TEMPLATE}}",
                                         todo, orgPath)

  noteLinkSummContents # return

}

#' Substitute Note Link in Contents
#'
sub_note_link_params <- function(noteLinkContents, settings, DocGDTList,
                                 projNoteLinkSummaryContents, orgPath) {

  #### sub link in contents ####
  noteLinkContents <- sub_template_param(noteLinkContents, "{{OBJECTIVES_SEP}}",
                                         settings[["NoteObjectivesSep"]], orgPath)
  noteLinkContents <- sub_template_param(noteLinkContents, "{{PROJECT_DOC_TITLE}}",
                                         DocGDTList$title, orgPath)
  noteLinkContents <- sub_template_param(noteLinkContents, "{{PROJECT_DOC_LINK}}",
                                         DocGDTList$link, orgPath)
  noteLinkContents <- sub_template_param(noteLinkContents, "{{PROJECT_DOC_LINK_GOAL}}",
                                         DocGDTList$goal, orgPath)
  noteLinkContents <- sub_template_param(noteLinkContents, "{{PROJECT_DOC_LINK_DEL}}",
                                         DocGDTList$del, orgPath)
  noteLinkContents <- sub_template_param(noteLinkContents, "{{PROJECT_DOC_LINK_TASK}}",
                                         DocGDTList$task, orgPath)

  # insert the summaryBullet into SUMMARY_INFO field:
  noteLinkContents <- sub_template_param(noteLinkContents, "{{SUMMARY_INFO}}",
                                         projNoteLinkSummaryContents, orgPath)

  # return
  noteLinkContents

}



#' Substitute a templates' parameter for content
#'
#' This function deals with the cases where the paramContents points a multi-lined
#' vector, or where paramContents points to to a template file in templates/ that
#' specifies multi-lined content to insert into the templateContents vector.
#'
sub_template_param <- function(templateContents, templateParam, paramContents, orgPath) {

  #### sub param in contents ####

  if(is.character(paramContents) &&
     length(paramContents) == 1 &&
     startsWith(paramContents, "template:")) {

    # check orgPath
    orgPath <- find_org_directory(orgPath)
    # set confPath + tempPath:
    confPath <- paste(orgPath, .Platform$file.sep, "config" , sep="")
    tempPath <- paste(confPath, .Platform$file.sep, "templates", sep="")

    # paramContents is a POINTER to a multi-line content file in templates dir
    # so open this and insert the multi-lined vector into templateContents
    paramPointer <- substr(paramContents, nchar("template:")+1, nchar(paramContents))
    paramContents <- read_file( paste0( tempPath, .Platform$file.sep, paramPointer) )

    templateContents <- replace_params_with_vector(templateContents, templateParam, paramContents)

  } else if( length(paramContents) > 1 ) {

    # paramContents is a multi-lined vector
    # so insert the multi-lined vector appropriately
    templateContents <- replace_params_with_vector(templateContents, templateParam, paramContents)

  } else {

    # paramContents is a vector of length 1, insert appropriately with gsub()
    templateContents <- gsub(templateParam, paramContents, templateContents, fixed=TRUE)

  }

  templateContents # return

}

#' Replace every instance of `templateParam` in `templateContents` with `paramContents`.
replace_params_with_vector <- function(templateContents, templateParam, paramContents) {

  tempParamIndices <- grep(templateParam, templateContents, fixed=TRUE)
  templateContents <- replace_at_indices(templateContents, tempParamIndices, paramContents)
  templateContents

}



#' Load `paramContents` (a param from settings.yml) correctly
#'
#' Some params start with keyword `template:` which indicates the relative path
#' from the templatesDir to the template file to be loaded.
#'
#' Else `paramContents` is returned unchanged.
load_param_vector <- function(paramContents, orgPath) {

  #### load value if param points to template ####

  # load the param if it points to a file
  if(is.character(paramContents) &&
     length(paramContents) == 1 &&
     startsWith(paramContents, "template:")) {

    # check orgPath
    orgPath <- find_org_directory(orgPath)
    # set confPath + tempPath:
    confPath <- paste(orgPath, .Platform$file.sep, "config" , sep="")
    tempPath <- paste(confPath, .Platform$file.sep, "templates", sep="")

    # paramContents is a POINTER to a multi-line content file in templates dir
    # so open this and insert the multi-lined vector into templateContents
    paramPointer <- substr(paramContents, nchar("template:")+1, nchar(paramContents))
    paramContents <- read_file( paste0( tempPath, .Platform$file.sep, paramPointer) )

  }

  # return
  paramContents

}



#' Replace SEP Values
#'
#' In a string vector, replace every instance of SEP values - syntax used to denote
#' separators between document sections in projectmanagr templates.
#'
#' * Six separators strings are defined in config/templates in projectmanagr org
#' in the files: `SEP01.txt` to `SEP06.txt`
#'
#' * Syntax in projectmanagr templates for inserting the separators in projectmanagr
#' templates: `{{SEP01}}` to `{{SEP06}}`
#'
#'
replace_sep_values <- function(templateContents, orgPath) {


  # set confPath + tempPath:
  confPath <- paste0(orgPath, .Platform$file.sep, "config")
  tempPath <- paste0(confPath, .Platform$file.sep, "templates")

  # get all SEP files form tempPath
  sepFiles <- list.files(tempPath)[ startsWith(list.files(tempPath), "SEP") ]

  # read all SEP values
  SEPS <- list()
  for( s in 1:length(sepFiles) ) {
    templateFileConn <- file( paste( tempPath, .Platform$file.sep, sepFiles[s], sep="") )
    SEPS[[s]] <- readLines( templateFileConn )
    close(templateFileConn)
  }

  #### replace SEP values ####

  sep01indices <- grep("{{SEP01}}", templateContents, fixed=TRUE)
  templateContents <- replace_at_indices(templateContents, sep01indices, SEPS[[1]])

  sep02indices <- grep("{{SEP02}}", templateContents, fixed=TRUE)
  templateContents <- replace_at_indices(templateContents, sep02indices, SEPS[[2]])

  sep03indices <- grep("{{SEP03}}", templateContents, fixed=TRUE)
  templateContents <- replace_at_indices(templateContents, sep03indices, SEPS[[3]])

  sep04indices <- grep("{{SEP04}}", templateContents, fixed=TRUE)
  templateContents <- replace_at_indices(templateContents, sep04indices, SEPS[[4]])

  sep05indices <- grep("{{SEP05}}", templateContents, fixed=TRUE)
  templateContents <- replace_at_indices(templateContents, sep05indices, SEPS[[5]])

  sep06indices <- grep("{{SEP06}}", templateContents, fixed=TRUE)
  templateContents <- replace_at_indices(templateContents, sep06indices, SEPS[[6]])

  templateContents
}


#' Replace with VECTOR at indices in contents
replace_at_indices <- function(templateContents, indices, replacementVector) {

  #### sort indices in reverse ####
  indices <- sort(indices, decreasing=TRUE) # work from END to START so indices remain VALID

  #### replace each index with replacementVector
  for( i in indices) {
    max <- length(templateContents) # compute with each loop
    # deal with edge cases - where index is 1 or max
    if( i == 1 ) {
      templateContents <- c(replacementVector, templateContents[(i+1):max])
    } else if( i == max ) {
      templateContents <- c(templateContents[1:(i-1)], replacementVector)
    } else { # deal with standard case - replace index i with the vector
      templateContents <- c(templateContents[1:(i-1)], replacementVector, templateContents[(i+1):max])
    }
  }

  templateContents
}



replace_markdown_header  <- function(templateContents, orgPath,
                                     htmlMarkdown="{{HTML_HEADER}}",
                                     htmlHeaderFilename="rmarkdown-html-header.txt") {

  # set confPath + tempPath:
  confPath <- paste0(orgPath, .Platform$file.sep, "config")
  tempPath <- paste0(confPath, .Platform$file.sep, "templates")

  # get all SEP files form tempPath
  htmlHeaderFile <- file( paste0(tempPath, .Platform$file.sep, htmlHeaderFilename) )
  htmlHeader <- readLines( htmlHeaderFile )
  close(htmlHeaderFile)

  #### replace HTML_HEADER ####
  htmlHeaderIndex <- grep(htmlMarkdown, templateContents, fixed=TRUE)
  templateContents <- replace_at_indices(templateContents, htmlHeaderIndex, htmlHeader)

  templateContents
}


#' Create Hyperlink
#'
#' creates string for hyperlink in Rmd, using `toFileName` as the hyperlink text,
#' and generating a RELATIVE LINK to `toFilePath` from `fromFilePath`.
#'
create_hyperlink <- function(toFileName, toFilePath, fromFilePath) {
  NoteLink <- R.utils::getRelativePath(toFilePath, relativeTo=fromFilePath)
  NoteLink <- substring(NoteLink, first=4, last=nchar(NoteLink)) # remove first `../`
  HyperLink <- paste("[", toFileName, "](", NoteLink, ")",  sep="")
  HyperLink # return
}



#' Update links
#'
#' Updates every hyperlink in all files within `dirTree`, replacing
#' `oldFileName` (the basename of the file, including prefix and extension) with
#' `newName` (the new file name, without prefix or extension).
#'
#' @param oldFileName defines the OLD name that will be in Hyperlinks.  Should be
#' the FILE NAME - with no spaces, including PREFIX and .Rmd EXTENSION
#' @param newName defines the NEW name to be written into Hyperlinks.  Should
#' be the FILE NAME - with no spaces.
#' @param dirTree Directory tree to search for files for replacing links in.
#' @param oldTitle defines the OLD TITLE that will be in Hyperlinks.  By default
#' replaces - & _ with spaces from name.
#' @param newTitle defines the NEW TITLE that will be in Hyperlinks.  By default
#' replaces - & _ with spaces from name.
#' @param fileExtensions List of file extensions indicating what types of files
#' should be searched for links and have links replaced.  Must be plaintext file
#' type.  Default is Rmd files only.
#'
#' @export
update_links <- function( oldFileName, newName, dirTree, settings,
                          fileExtensions = list("Rmd") ) {

  # check oldFileName contains NO SPACES:
  if( grepl("\\s+", oldFileName) ) {
    stop( paste0("  oldFileName contains a SPACE: ",oldFileName) )
  }

  # check newName contains NO SPACES:
  if( grepl("\\s+", newName) ) {
    stop( paste0("  newName contains a SPACE: ",newName) )
  }

  # make dirTree full path
  dirTree <- normalizePath(dirTree)

  # split oldFileName into PREFIX NAME and EXTENSION
  oldPrefix <- substr(oldFileName, 1,
                   regexpr(settings[["ProjectPrefixSep"]], oldFileName, fixed=TRUE)-1 )
  oldFileNameExt <- tools::file_ext(oldFileName)
  oldName <- substr(oldFileName,
                    regexpr(settings[["ProjectPrefixSep"]], oldFileName, fixed=TRUE)+(nchar(settings[["ProjectPrefixSep"]])),
                    regexpr(oldFileNameExt, oldFileName, fixed=TRUE)-2) # first letter AND extension .

  # construct oldLink: prefix plus oldName (no extension) for replacing
  oldLink <- paste0(oldPrefix, settings[["ProjectPrefixSep"]], oldName)

  # construct newLink: prefix plus newName (no extension) for replacing
  newLink <- paste0(oldPrefix, settings[["ProjectPrefixSep"]], newName)

  # get orgPath from dirTree
  orgPath <- find_org_directory(dirTree)
  if(orgPath == "" ) { # only if orgPath not identified
    stop( paste0("  projectNotePath is not in a sub-dir of a PROGRAMME Directory: ", projectNotePath) )
  }

  # get config path to exclude files from (in case dirTree is orgPath!)
  confPath <- paste0( orgPath, .Platform$file.sep, "config" )


  #### get file list ####

  # traverse EVERY Rmd file in orgPath dir tree:
  fileList <- c()
  for(fe in fileExtensions) {
    fileList <- c(fileList,
                   paste0( dirTree, .Platform$file.sep,
                           list.files(path = dirTree, pattern = paste0("*.",fe),
                                all.files = TRUE, recursive = TRUE, include.dirs = TRUE) ) )
  }

  # remove all files in config directory
  fileList <- fileList[ !startsWith(fileList, confPath)]


  #### replace oldLink with newLink ####

  for(fl in fileList) {

    # read file:
    fileConn <- file(fl)
    contents <- readLines(fileConn)
    close(fileConn)

    if( any( grepl(oldLink, contents, fixed=TRUE )) ) {
      # replace oldLink with newLink in contents
      contents <- gsub(oldLink, newLink, contents, fixed=TRUE)
      # and save file
      cat( "    replaced link(s) in file:", fl ,"\n" )
      fileConn <- file(fl)
      writeLines(contents, fileConn)
      close(fileConn)
    }
  }
}



#' Insert vector at indices in contents
#'
#' Inserts `replacementVector` into `templateContents` at `indices` WITHOUT
#' replacing the content at each index.
#'
insert_at_indices <- function(templateContents, indices, replacementVector) {

  #### insert at indices without replacing each index ####

  max <- length(templateContents)

  indices <- sort(indices, decreasing=TRUE) # work from END to START so indices remain VALID
  for( i in indices) {
    # deal with edge cases - where index is 1 or max
    if( i == 1 ) {
      templateContents <- c(replacementVector, templateContents[(i):max])
    } else if( i == max ) {
      templateContents <- c(templateContents[1:(i-1)], replacementVector)
    } else { # deal with standard case - replace index i with the vector
      templateContents <- c(templateContents[1:(i-1)], replacementVector, templateContents[(i):max])
    }
  }

  templateContents
}



#' Update ProjectManagr Files
#'
#' Updates all summary information in ORG index, PROG index, & Project Docs
#' with linked PROG index, Project Docs, & Project Notes, respectively.
#'
#' @param dirTree OPTIONAL argument to set the directory tree in which an update
#' will apply.  Will only update the summary information for ORG index, PROG
#' index, Project Doc files that are contained within dirTree if set.  Default is
#' a BLANK STRING, which means the current working directory is used, or the
#' Source Editor context file path is used, to identify the orgPath to update
#' the whole organisation.
#'
update <- function(dirTree="") {

  #### update all projectmanagr documents in dirTree ####

  if( dirTree == "" ) {

    orgPath <- find_org_directory( getwd() )
    if(orgPath == "" & Sys.getenv("RSTUDIO") != "" ) {
      orgPath <- find_org_directory( rstudioapi::getSourceEditorContext()$path)
    } else {
      stop( paste0("  Path is not in an Organisation: ",  getwd()) )
    }
    dirTree <- orgPath # simplifies code below - will udpate files in dirTree only which IS orgPath!

  } else {

    orgPath <- find_org_directory( dirTree )
    if(orgPath == "") {
      stop( paste0("  dirTree is not in an Organisation: ",  dirTree) )
    }
  }

  # set confPath + tempPath - these names are FIXED:
  confPath <- paste0( orgPath, .Platform$file.sep, "config" )
  tempPath <- paste0( confPath, .Platform$file.sep, "templates" )

  # load settings file for user defined settings
  settingsFile <- paste( confPath, .Platform$file.sep, "settings.yml", sep="" )
  settings <- yaml::yaml.load( yaml::read_yaml( settingsFile ) )
  statusFile <- paste0(confPath, .Platform$file.sep, settings[["ConfigStatusYamlFile"]])
  status <- yaml::yaml.load( yaml::read_yaml( statusFile ) )


  #### find all projectManagr files in dirTree for updating ####

  # ORG index
  orgFiles <- list.files(orgPath)
  orgIndex <- orgFiles[ startsWith(orgFiles, settings[["OrgIndexFileNamePrefix"]])]

  # PROG index
  progDirs <- names(status$PROGRAMMES)
  progIndexes <- c()
  for(pd in progDirs) {
    progFiles <- list.files( paste0(orgPath, .Platform$file.sep, pd) )
    progIndexes <- c(progIndexes, paste0(orgPath, .Platform$file.sep, pd, .Platform$file.sep,
                                         progFiles[ startsWith(progFiles, settings[["ProgIndexFileNamePrefix"]])]) )
  }

  # ProjDocs
  progProjects <- paste0(progDirs, .Platform$file.sep, settings[["ProgrammeProjectsDir"]])
  projDocs <- c()
  for(pp in progProjects) {
    projFiles <- list.files( paste0(orgPath, .Platform$file.sep, pp) )
    if(length(projFiles) > 0 ) {
      projDocs <- c(projDocs,
                    paste0(orgPath, .Platform$file.sep, pp, .Platform$file.sep,
                           projFiles[ grepl(settings[["ProjectPrefixSep"]], projFiles, fixed=TRUE)  ] ) )
    }
  }


  #### update summaries in files ####


  # ORG index

  # only if dirTree == orgPath - otherwise dirTree has been set by function call to be sub-dir in org..
  if(dirTree == orgPath ) {

    update_org_index(orgIndex)
  }


  # PROG index

  for(pi in progIndexes) {
    # only if PROG INDEX startsWith dirTree - otherwise dirTree has been set by function call to be sub-dir in org that doesnt include PROG INDEX
    if( startsWith(pi, dirTree) == TRUE ) {

      update_prog_index(pi)
    }
  }


  # proj docs

  update_project_docs( projDocs[ startsWith(projDocs, dirTree) ], settings, orgPath )

}



update_org_index <- function(orgIndexPath) {

  orgIndexContents <- read_file(orgIndexPath)

  # identify each programme in this orgIndex

  # read each programme file

  # extract summary section

  # insert into orgIndexContents


}

update_prog_index <- function(progIndexPath) {

  progIndexContents <- read_file(progIndexPath)

  # identify each projDoc in this progIndex

  # read each projDoc

  # extract summary section

  # insert into progIndexContents

}


#' updating all project docs in one function for efficiency
#'
#' As one project note can contain MULTIPLE GDTs to Project Notes, and want to
#' efficiently update the projDoc GDTs, need to keep track of which project
#' notes have been updated.
#'
#' Every time a project note is opened, ALL OF ITS GDT SUMMARY SECTIONS are updated.
#' Once updated do not want to update again, so add its full path to a character
#' vector to check against before opening and updating any further project notes.
update_project_docs <- function(projectDocPaths, settings, orgPath) {

  projectNotesUpdated <- c() # vector to append project notes to as they are updated

  for(projectDocPath in projectDocPaths) {

    projectDocContents <- read_file(projectDocPath)

    # identify project note paths linked to each GDT
    projNotePaths <- get_doc_gdt_project_note_paths(projectDocPath, projectDocContents, settings)

    for(pn in projNotePaths) {

      if( any(projectNotesUpdates == pn) == FALSE ) { # only parse project note if not parsed already

        projectDocContents <- update_project_note(pn, projectDocPath, projectDocContents)

        # add pn to projectNotesUpdates
        projectNotesUpdates <- c(projectNotesUpdates, pn)
      }

    }

  }

}


#' update project note, putting GDT summary updates for projectDoc that exists at
#' projectDocPath into projectDocContents.
update_project_note <- function(projectNotePath, projectDocPath, projectDocContents) {

  projNoteContents <- read_file(projectNotePath)

  # extract each objective
  DocGDTsList <- extract_note_obj_doc_link_GDT_summ(projNoteContents, pn, settings, orgPath)

  if(is.list(DocGDTsList) & length(DocGDTsList) == 0) {
    # if a blank list - this note has no objective - skip this note
    next
  }

  # update each GDT summary section to the relevant project doc
  for(dGDT in DocGDTsList) {

  }

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
compute_previous_line_index <- function(lineIndex, contents) {

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





