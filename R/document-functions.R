
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

  DocPrefix <- get_prefix(projectDocPath, settings)


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

  # create DocTitle - DocPrefix plus the Gnum Dnum Tnum
  #DocTitle <- paste( "## ", DocName, " : G", goalNum, " D", delNum, " T", taskNum, sep="")
  # create DocTitle - DocName plus the TaskTitle
  DocTitle <- paste0( settings[["NoteSummaryTitle"]] ,
                      DocPrefix, " : ", get_task_title(task, settings) )

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

  # get goal without the header portion
  goal_trim <- trim_goal_hash(goal, settings)

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



get_goal_title_from_link <- function(goalLink, settings) {

  paste0(

    # goal title header '#' in proj doc - [1]:get FIRST VALUE for the PREFIX - omit any suffix
    unlist(
      strsplit(settings[["ProjectGoalHeader"]],
               split=paste0(settings[["ProjectGoalTitle"]]),
               fixed=TRUE))[1],

    # get goals title from link
    substr(goalLink,
           regexpr(settings[["ProjectGoalTitle"]], goalLink, fixed=TRUE),
           regexpr("](", goalLink, fixed=TRUE)-1)
    )

}




#' Trim Goal Hash
#'
#' Trim default hash from GOAL, as specified in settings yml:
#' `ProjectGoalHeader` & `ProjectGoalTitle`
#'
#' @param goal Goal Header String - including markdown header hashes.
#'
#' @param settings projectmanagr settings list.
#'
#' @return Task String - without header hashes
#'
trim_goal_hash <- function(goal, settings) {

  # identify the length of goal header - default '# GOAL' minus GoalTitle - default 'GOAL'
  # +1 for substring - get FIRST VALUE for the PREFIX - omit and suffix
  glen <- nchar(unlist(strsplit(settings[["ProjectGoalHeader"]],
                                split=settings[["ProjectGoalTitle"]], fixed=TRUE))[1])+1

  # get goal without the header portion
  goal_trim <- substring(goal, first=glen)

  goal_trim

}

#' compute deliverable link
#'
#' generate new link from project doc with html # tag for deliverable section.
#'
#' @param del Deliverable Header String
#'
#' @param DocLink Link to Project Doc.
#'
#' @param settings projectmanagr settings list.
#'
compute_deliverable_link <- function(del, DocLink, settings) {


  #### deliverable link ####

  # get start of Del LINK line WITHOUT [+DelTitle - will use del_trim to fill link title
  delLinkLineStart <- unlist(strsplit(settings[["NoteDeliverableLinkLine"]],
                                      split=paste0("[",settings[["ProjectDeliverableTitle"]]), fixed=TRUE))

  # get del without the header hash
  del_trim <- trim_deliverable_hash(del, settings)

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


get_deliverable_title_from_link <- function(delLink, settings) {

  paste0(

    # del title header '##' in proj doc - [1]:get FIRST VALUE for the PREFIX - omit any suffix
    unlist(
      strsplit(settings[["ProjectDeliverableHeader"]],
               split=paste0(settings[["ProjectDeliverableTitle"]]),
               fixed=TRUE))[1],

    # get dels title from link
    substr(delLink,
           regexpr(settings[["ProjectDeliverableTitle"]], delLink, fixed=TRUE),
           regexpr("](", delLink, fixed=TRUE)-1)
  )

}


#' Trim Deliverable Hash
#'
#' Trim default hash from DELIVERABLE, as specified in settings yml:
#' `ProjectDeliverableHeader` & `ProjectDeliverableTitle`
#'
#' @param del Deliverable Header String - including markdown header hashes.
#'
#' @param settings projectmanagr settings list.
#'
#' @return Deliverable String - without header hashes
#'
trim_deliverable_hash <- function(del, settings) {

  # identify the length of del header - default '## DELIVERABLE' minus DelTitle - default 'DELIVERABLE'
  # +1 for substring
  dlen <- nchar(unlist(strsplit(settings[["ProjectDeliverableHeader"]],
                                split=settings[["ProjectDeliverableTitle"]], fixed=TRUE))[1])+1

  # get del without the header portion
  del_trim <- substring(del, first=dlen)

  del_trim

}



#' compute deliverable link
#'
#' generate new link from project doc with html # tag for deliverable section.
compute_task_link <- function(task, DocLink, settings) {


  #### task link ####

  # get start of goal link line WITHOUT [+GoalTitle - will use goal_trim to fill link title
  taskLinkLineStart <- unlist(strsplit(settings[["NoteTaskLinkLine"]],
                                       split=paste0("[",settings[["ProjectTaskTitle"]]), fixed=TRUE))

  # get task without the header portion
  task_trim <- trim_task_hash(task, settings)

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


get_task_title_from_link <- function(taskLink, settings) {

  paste0(

    # task title header '###' in proj doc - [1]:get FIRST VALUE for the PREFIX - omit any suffix
    unlist(
      strsplit(settings[["ProjectTaskHeader"]],
               split=paste0(settings[["ProjectTaskTitle"]]),
               fixed=TRUE))[1],

    # get tasks title from link
    substr(taskLink, regexpr(settings[["ProjectTaskTitle"]], taskLink, fixed=TRUE),
           regexpr("](", taskLink, fixed=TRUE)-1)
  )

}

#' Trim Task Hash
#'
#' Trim default hash from TASK, as specified in settings yml:
#' `ProjectTaskHeader` & `ProjectTaskTitle`
#'
#' @param task Task Header String - including markdown header hashes.
#'
#' @param settings projectmanagr settings list.
#'
#' @return Task String - without header hashes
#'
trim_task_hash <- function(task, settings) {

  # identify the length of task header - default '### TASK' minus TaskTitle - default 'TASK'
  # +1 for substring - get FIRST VALUE for the PREFIX - omit and suffix
  tlen <- nchar(unlist(strsplit(settings[["ProjectTaskHeader"]],
                                split=settings[["ProjectTaskTitle"]], fixed=TRUE))[1])+1

  # get task without the header portion
  task_trim <- substring(task, first=tlen)

  task_trim

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

  # separate linkObjectives into separate GDTs
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

    # compute goal, del, task titles from links
    goal <- get_goal_title_from_link(o[gli], settings)
    del <- get_deliverable_title_from_link(o[dli], settings)
    task <- get_task_title_from_link(o[tli], settings)


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



#' Get Header Title
#'
#' Removes leading #s in a markdown header.  Ensures any subsequent #s in the title
#' are RETAINED.
#'
#' eg. '### Header : Title # after hash ## after double-hash'
#'
#' is returned as " Header : string # after hash ## after double-hash"
#'
#' @param header The header string
#'
get_header_title <- function(header) {

  # split by '#'
  hV <- strsplit(header, '#', fixed=TRUE)[[1]]

  # count leading #s
  leadHashNum <- 0
  for( h in 1:length(hV) ) {
    if(hV[h] == "") {
      leadHashNum <- leadHashNum+1
    } else {
      break
    }
  }

  # increment one further
  leadHashNum <- leadHashNum+1

  # remove the leading #s
  hVr <- hV[leadHashNum:length(hV)]

  # re-add subsequent #s
  hVfinal <- paste0(hVr, collapse ='',sep='#')

  hVfinal

}

#' Get Header Hash
#'
#' Returns leading #s in a markdown header.
#'
#' eg. '### Header : Title # after hash ## after double-hash'
#'
#' is returned as "###"
#'
#' @param header The header string
#'
get_header_hash <- function(header) {

  # split by '#'
  hV <- strsplit(header, '#', fixed=TRUE)[[1]]

  # count leading #s
  leadHashNum <- 0
  for( h in 1:length(hV) ) {
    if(hV[h] == "") {
      leadHashNum <- leadHashNum + 1
    } else {
      break
    }
  }

  # create new # string with leadHashNum
  hh <- paste0(rep('#', leadHashNum), collapse='')

  hh # return

}

#' return index of line that matches line in contents
#'
match_line_index <- function(line, contents) {

  #### get line index in contents ####
  indices <- grep( trimws(line), trimws(contents), fixed=TRUE)

  if( length(indices) == 0 ) {
    returnVal <- -1
  } else {
    returnVal <- indices[1]
  }

  returnVal

}


#' Match vector in parent
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
#' "##" - if this is a `TASK` line, then the first previous `DELIVERABLE` line and
#' the first previous `GOAL` line are also identified.
#'
#' If TASK, DELIVERABLE and GOAL lines are all successfully found, this method returns
#' a LIST:
#'
#' [[1]] or [["rmdType"]] - type of file selected: DOC, HEAD, SUB, NOTE, or UNKNOWN
#'
#' [[2]] or [["task"]]
#'
#' [[3]] or [["taskLine"]]
#'
#' [[4]] or [["deliverable"]]
#'
#' [[5]] or [["delLine"]]
#'
#' [[6]] or [["goal"]]
#'
#' [[7]] or [["goalLine"]]
#'
#' [[8]] or [["originalLine"]]
#'
#' [[9]] or [["originalLineNumber"]]
#'
#' [[10]] or [["addingSubNote"]]
#'
#' [[11]] or [["headerNoteLink"]]
#'
#' [[12]] or [["headerNoteRmdPath"]]
#'
#' [[13]] or [["headerNoteName"]]
#'
#' [[14]] or [["headerNoteLineNumber"]]
#'
#' [[15]] or [["filePath"]]
#'
#' [[16]] or [["defaultPath"]]
#'
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

  # get config templates settings yml
  confPath <- get_config_dir(orgPath)
  tempPath <- get_template_dir(orgPath)
  settings <- get_settings_yml(orgPath)

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

  # get config templates settings yml
  confPath <- get_config_dir(orgPath)
  tempPath <- get_template_dir(orgPath)
  settings <- get_settings_yml(orgPath)


  #### read file ####

  contents <- read_file(filePath)


  #### create selection at line ####

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

    defaultPath <- get_project_doc_dir_path(filePath, settings) # set default path to DOC dir


    ##### Identify Selected Line: NOTE Link ####

    # check if selected line is a link - links contain string `](`
    if( grepl("](", lineContent, fixed=TRUE) ) {

      # extract path & make absolute (combining with filePath if relative)
      linkPath <- get_path_from_link(lineContent)

      if( R.utils::isAbsolutePath(linkPath) == FALSE ) {
        # combine with the parent dir of DOC filePath
        linkPath <- get_absolute_path( dirname(filePath), linkPath)
      }

      # check if the link points to a NOTE HEAD or SUB
      linkType <- get_file_type(linkPath, settings)

      if( linkType == "NOTE" ) {

        # selected link is a simple note - want to use its parent as default location for new note
        defaultPath <- dirname(linkPath)


      } else if( linkType == "HEAD" ) {

        # selected link is a group note - set further params

        # selected link is a group note - so set further params
        addingSubNote <- TRUE

        # headerNote line is original selected line!
        headerNoteLink <- lineContent
        headerNoteLineNumber <- line

        headerNoteRmdPath <- linkPath
        headerNoteFileName <- basename(headerNoteRmdPath)
        headerNoteName <- get_name_from_file_name(headerNoteFileName, settings)


      } else if( linkType == "SUB" ) {

        # selected link is a group note - so set further params
        addingSubNote <- TRUE

        # find headerNote line
        for(l in line:1) {
          lineContent <- contents[ l ]
          if( grepl(headerString, lineContent, fixed = TRUE) ) {
            headerNoteLink <- lineContent
            headerNoteLineNumber <- l
            break
          }
        }

        # extract path & make absolute (combining with filePath if relative)
        headerNoteRmdPath <- get_path_from_link(lineContent)

        if( R.utils::isAbsolutePath(headerNoteRmdPath) == FALSE ) {
          # combine with the parent dir of DOC filePath
          headerNoteRmdPath <- get_absolute_path( dirname(filePath), headerNoteRmdPath)
        }

        headerNoteFileName <- basename(headerNoteRmdPath)
        headerNoteName <- get_name_from_file_name(headerNoteFileName, settings)

      }
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
                      filePath, defaultPath )

      names(output) <- c( "rmdType", "task", "taskLine", "deliverable", "delLine",
                          "goal", "goalLine",
                          "originalLine", "originalLineNumber",
                          "addingSubNote", "headerNoteLink",
                          "headerNoteRmdPath", "headerNoteName",
                          "headerNoteLineNumber", "filePath", "defaultPath")

    }
    else {

      output <- create_no_selection(rmdType, errorMessage, filePath, originalLine, originalLineNumber)

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
    output <- create_no_selection("UNKNOWN", errorMessage, filePath,
                                  originalLine, originalLineNumber)

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
create_no_selection <- function(rmdType, errorMessage, filePath,
                                originalLine, originalLineNumber) {
  output <- list( rmdType, errorMessage, filePath,
                  originalLine, originalLineNumber )
  names(output) <- c( "rmdType", "errorMessage", "filePath",
                      "originalLine", "originalLineNumber")
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

    # get config templates settings yml
    confPath <- get_config_dir(orgPath)
    tempPath <- get_template_dir(orgPath)
    settings <- get_settings_yml(orgPath)

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
    # get config templates settings yml
    confPath <- get_config_dir(orgPath)
    tempPath <- get_template_dir(orgPath)

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
  # get config templates settings yml
  confPath <- get_config_dir(orgPath)
  tempPath <- get_template_dir(orgPath)

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

  # get config templates settings yml
  confPath <- get_config_dir(orgPath)
  tempPath <- get_template_dir(orgPath)

  # get all SEP files form tempPath
  htmlHeaderFile <- file( paste0(tempPath, .Platform$file.sep, htmlHeaderFilename) )
  htmlHeader <- readLines( htmlHeaderFile )
  close(htmlHeaderFile)

  #### replace HTML_HEADER ####
  htmlHeaderIndex <- grep(htmlMarkdown, templateContents, fixed=TRUE)
  templateContents <- replace_at_indices(templateContents, htmlHeaderIndex, htmlHeader)

  templateContents
}


#' Replace Knitr include_graphics link
#'
#' Replace the path inside knitr::include_graphics() functions with a relative
#' path to sourceFilePath from destinationFilePath.  Deals with different quotes
#' and whitespace in function call.
#'
#' @param contentContents String vector of contents that may contain calls to
#' Knitr::include_graphics()
#'
#' @param sourceFilePath The path to the source file - where contentContents was
#' read from
#'
#' @param destinationFilePath The path where the content will be moved to, used
#' to compute the relative path of all files.
#'
#' @param knitrGraphics The function call to knitr grpahics to search for, set
#' to `knitr::include_graphics(` by default.
#'
#' @param knitrGraphicsEnd The end of the function call, set to `)` by default.
#'
replace_knitr_include_graphics_link <- function(contentContents, sourceFilePath,
                                                destinationFilePath,
                                                knitrGraphics='knitr::include_graphics(',
                                                knitrGraphicsEnd=')') {

  knitrGraphicsIndex <- grep(knitrGraphics, contentContents, fixed=TRUE)

  for( kgi in knitrGraphicsIndex) {

    # get knitr line
    kLine <- contentContents[kgi]

    # compute indices based on knitrGraphics content
    kPathStartIndex <- (regexpr(knitrGraphics, kLine, fixed=TRUE) + nchar(knitrGraphics))
    sqS <- regexpr("'", substring(kLine, kPathStartIndex), fixed=TRUE) # get start quote index
    dqS <- regexpr('"', substring(kLine, kPathStartIndex), fixed=TRUE)
    qS <- max(sqS, dqS)

    kPathEndIndex <- (regexpr(knitrGraphicsEnd, kLine, fixed=TRUE) - nchar(knitrGraphicsEnd) - 1)
    sqE <- regexpr("'", substring(kLine, (kPathStartIndex+qS)), fixed=TRUE) -1 # get end quote index
    dqE <- regexpr('"', substring(kLine, (kPathStartIndex+qS)), fixed=TRUE) -1 # get end quote index
    qE <- max(sqE, dqE)

    # extract path from `knitr::include_graphics()` string
     # from WITHIN the quotes && trim all whitespace - will remove
    kPath <- trimws( substring(kLine, (kPathStartIndex+qS), (kPathStartIndex+qS+qE-1)) )

    # kPath is relative to sourceFilePath - collapse from dirname of sourceFilePath
    kPathFull <- fs::path_abs(fs::path( dirname(sourceFilePath), kPath))

    # get relative link to dirname of destinationFilePath
    relPathFull <- fs::path_rel(kPathFull, dirname(destinationFilePath) )

    # replace path in kLine with relPathFull
    contentContents[kgi] <- paste0(
      substring(kLine, 1, (kPathStartIndex+qS-1) ),
      relPathFull,
      substring(kLine, (kPathStartIndex+qS+qE), nchar(kLine)) )

  }

  # return
  contentContents

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



#' Create Hyperlink Section
#'
#' creates string for hyperlink in Rmd, using `toFileName : toFileSection` as
#' the hyperlink text, and generating a RELATIVE LINK to
#' `toFilePath#toFileSection` from `fromFilePath`.
#'
create_hyperlink_section <- function(toFileName, toFileSection, toFilePath, fromFilePath) {

  NoteLink <- R.utils::getRelativePath(toFilePath, relativeTo=fromFilePath)
  NoteLink <- substring(NoteLink, first=4, last=nchar(NoteLink)) # remove first `../`
  NoteLink <- paste0(NoteLink, '#', gsub("[ ]|[_]", "-", trimws( tolower( gsub("[^[:alnum:] ]", "", toFileSection) ) ) ) )
  HyperLink <- paste("[", toFileName, " : ", gsub('#', '', toFileSection, fixed=TRUE), "](", NoteLink, ")",  sep="")
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
#'
#' @param newName defines the NEW name to be written into Hyperlinks.  Should
#' be the FILE NAME - with no spaces.
#'
#' @param dirTree Directory tree to search for files for replacing links in.
#'
#' @param oldTitle defines the OLD TITLE that will be in Hyperlinks.  By default
#' replaces - & _ with spaces from name.
#'
#' @param newTitle defines the NEW TITLE that will be in Hyperlinks.  By default
#' replaces - & _ with spaces from name.
#'
#' @param fileExtensions List of file extensions indicating what types of files
#' should be searched for links and have links replaced.  Must be plaintext file
#' type.  Default is Rmd files only.
#'
#' @export
update_links_filenames <- function( oldFileName, newName, dirTree, settings,
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

  # split oldFileName into PREFIX, NAME, EXTENSION
  oldPrefix <- get_prefix(oldFileName, settings)
  oldFileNameExt <- tools::file_ext(oldFileName)
  oldName <- substr(oldFileName,
                    regexpr(settings[["ProjectPrefixSep"]],
                            oldFileName, fixed=TRUE)+(nchar(settings[["ProjectPrefixSep"]])),
                    regexpr(oldFileNameExt, oldFileName, fixed=TRUE)-2) # first letter AND extension .

  # construct oldLink: prefix plus oldName (no extension) for replacing
  oldLink <- paste0(oldPrefix, settings[["ProjectPrefixSep"]], oldName)

  # construct newLink: prefix plus newName (no extension) for replacing
  newLink <- paste0(oldPrefix, settings[["ProjectPrefixSep"]], newName)

  # get orgPath from dirTree
  orgPath <- find_org_directory(dirTree)
  if(orgPath == "" ) { # only if orgPath not identified
    stop( paste0("  projectNotePath is not in a sub-dir of a PROGRAMME Directory: ",
                    projectNotePath) )
  }

  # get config path to exclude files from (in case dirTree is orgPath!)
  confPath <- get_config_dir(orgPath)
  tempPath <- get_template_dir(orgPath)
  volPath <- paste0(orgPath, .Platform$file.sep, settings[["VolumesDir"]])


  #### get file list ####

  # first grab all files WITHOUT RECURSION - all files in dirTree directory
  fileList <- c()
  for(fe in fileExtensions) {
    fileList <- c(fileList,
                  paste0( dirTree, .Platform$file.sep,
                          list.files(path = dirTree, pattern = paste0("*.",fe),
                                     all.files = TRUE, include.dirs = TRUE) ) )
  }

  # now grab all dirs WITHOUT RECURSION
  dirsList <- list.dirs(path = dirTree, recursive=FALSE)

  # and exclude confPath and volPath if present
  dirsList <- dirsList[ dirsList != confPath]
  dirsList <- dirsList[ dirsList != volPath]

  # next traverse each directory - but only down to project note level
  for(dl in dirsList) {
    # get fileList recursively but only down to project note parent dir level
    fileList <- get_file_list_to_project_notes(fileList, dl, settings, fileExtensions)
  }

  # now RECURSIVELY traverse EVERY Rmd file in dirTree
  #fileList <- c()
  #for(fe in fileExtensions) {
  #  for(dl in dirsList) {
  #    fileList <- c(fileList,
  #                  paste0( dl, .Platform$file.sep,
  #                          list.files(path = dl, pattern = paste0("*.",fe),
  #                          all.files = TRUE, recursive = TRUE, include.dirs = TRUE) ) )
  #  }
  #}

  # remove all files in config directory
  #fileList <- fileList[ !startsWith(fileList, confPath)]


  #### replace oldLink with newLink ####

  for(fl in fileList) {

    # read file:
    contents <- read_file(fl)

    if( any( grepl(oldLink, contents, fixed=TRUE )) ) {
      # replace oldLink with newLink in contents
      contents <- gsub(oldLink, newLink, contents, fixed=TRUE)
      # and save file
      cat( "    replaced link(s) in file:", fl ,"\n" )
      write_file(contents, fl)
    }
  }
}


#' Update links
#'
#' Updates every hyperlink in all Project Notes within `dirTree`, replacing
#' `oldLinkSuffix` (the path of the file) with `newLinkSuffix` (the new path).
#'
#' @param oldLinkSuffix Old link string to be updated.
#'
#' @param newLinkSuffix New link string to replace `oldLinkSuffix`.
#'
#' @param dirTree Directory tree to search for files for replacing links in.
#'
#' @param settings projectmanagr settings list.
#'
#' @param fileExtensions List of file extensions indicating what types of files
#' should be searched for links and have links replaced.  Must be plaintext file
#' type.  Default is Rmd files only.
#'
#' @export
update_links <- function( oldLinkSuffix, newLinkSuffix, dirTree, settings,
                          oldLinkString="", newLinkString="", fileExtensions = list("Rmd") ) {

  # make dirTree full path
  dirTree <- normalizePath(dirTree)

  # get orgPath from dirTree
  orgPath <- find_org_directory(dirTree)
  if(orgPath == "" ) { # only if orgPath not identified
    stop( paste0("  projectNotePath is not in a sub-dir of a PROGRAMME Directory: ",
                   projectNotePath) )
  }

  # get config path to exclude files from (in case dirTree is orgPath!)
  confPath <- get_config_dir(orgPath)
  tempPath <- get_template_dir(orgPath)
  volPath <- paste0(orgPath, .Platform$file.sep, settings[["VolumesDir"]])


  #### get file list ####

  # first grab all files WITHOUT RECURSION - all files in dirTree directory
  fileList <- c()
  for(fe in fileExtensions) {
    fileList <- c(fileList,
                  paste0( dirTree, .Platform$file.sep,
                          list.files(path = dirTree, pattern = paste0("*.",fe),
                                     all.files = TRUE, include.dirs = TRUE) ) )
  }

  # now grab all dirs WITHOUT RECURSION
  dirsList <- list.dirs(path = dirTree, recursive=FALSE)

  # and exclude confPath and volPath if present
  dirsList <- dirsList[ dirsList != confPath]
  dirsList <- dirsList[ dirsList != volPath]

  # next traverse each directory - but only down to project note level
  for(dl in dirsList) {
    # get fileList recursively but only down to project note parent dir level
    fileList <- get_file_list_to_project_notes(fileList, dl, settings,
                                               fileExtensions)
  }

  # now RECURSIVELY traverse EVERY Rmd file in dirTree
  #fileList <- c()
  #for(fe in fileExtensions) {
  #  for(dl in dirsList) {
  #    fileList <- c(fileList,
  #                  paste0( dl, .Platform$file.sep,
  #                          list.files(path = dl, pattern = paste0("*.",fe),
  #                          all.files = TRUE, recursive = TRUE, include.dirs = TRUE) ) )
  #  }
  #}

  # remove all files in config directory
  #fileList <- fileList[ !startsWith(fileList, confPath)]


  #### replace oldLinkSuffix with newLinkSuffix ####

  for(fl in fileList) {

    contents <- read_file(fl)
    cL <- grepl(oldLinkSuffix, contents, fixed=TRUE )
    if( any(cL) ) {
      # replace oldLinkSuffix with newLinkSuffix in contents
      contents[cL] <- gsub(oldLinkSuffix, newLinkSuffix, contents[cL], fixed=TRUE)
      if(oldLinkString!="" && newLinkString!="") {
        contents[cL] <- gsub(oldLinkString, newLinkString, contents[cL], fixed=TRUE)
      }
      # and save file
      cat( "    replaced link(s) in file:", fl ,"\n" )
      write_file(contents, fl)
    }
  }
}




#' get file list down to project notes
#'
#' Traverses all directory tree in `dl` but only get fileList recursively but
#' only down to project note parent dir level.
#'
#' This method makes parsing all plaintext Project Notes across an Organisation
#' much more efficient!
#'
#' @param fileList List of files recursively retrieved by this function.
#' @param dl DirsList - a list of directory paths.
#' @param settings projectmanagr settings list.
#' @param fileExtensions File extensions of files to list.
#' @param pathExclusions Directory Paths which should be EXCLUDED from file
#' search. Typically want to exclude the config and volumes directories in the
#' root of an Organisation.
#' @param retrievalDateTimeCutoff Any project notes last modified BEFORE the
#' cutoff time will not be returned. If NULL ignored. This is a lubridate datetime
#' object, made by parsing a datetime string through `lubridate::ymd_hm()`
#'
get_file_list_to_project_notes <- function(fileList, dl, settings,
                                           fileExtensions = list("Rmd"),
                                           pathExclusions = c(),
                                           retrievalDateTimeCutoff = NULL ) {


  #### Set Instance Variables ####

  # get important delimiters from settings
  #projIdentifierSep <- load_param_vector(settings[["ProjectIdentifierSep"]]) # "_"
  #projPrefixSep <- load_param_vector(settings[["ProjectPrefixSep"]]) #  "~_"
  #projIndexSep <- load_param_vector(settings[["ProjectIndexSep"]]) # "~"
  #groupIndexSep <- load_param_vector(settings[["GroupNotePrefixSep"]]) # "-"


  #### get all files from dl ####

  if( is.null(retrievalDateTimeCutoff) ) { # no cutoff filter

    # get each file with extension from all dirs in dl
    for(fe in fileExtensions) {
      # get local copy of fl to check for project notes
      fl <- paste0( dl, .Platform$file.sep,
                    list.files(path = dl, pattern = paste0("*.",fe),
                               all.files = TRUE) )
      fl <- fl[fl!=paste0(dl, .Platform$file.sep)] # remove any instances of just dl/ - if no files found!
      fileList <- c(fileList, fl )
    }

  } else { # filter for cutoff datetime

    # get each file with extension from all dirs in dl
    for(fe in fileExtensions) {
      # get local copy of fl to check for project notes
      fl <- paste0( dl, .Platform$file.sep,
                    list.files(path = dl, pattern = paste0("*.",fe),
                               all.files = TRUE) )
      fl <- fl[fl!=paste0(dl, .Platform$file.sep)] # remove any instances of just dl/ - if no files found!

      # filter for datetime - only keep files
      flf <- fl[lubridate::as_datetime(file.info(fl)$ctime, tz='UTC') > retrievalDateTimeCutoff]

      fileList <- c(fileList, flf )
    }

  }


  #### get next level of sub-dirs from dl ####

  dls <- list.dirs(path = dl, recursive=FALSE)

  # remove any directories that match pathExclusions
  dls <- dls[ !dls %in% pathExclusions ]


  #### filter next level of sub-dirs to remove SIMPLE & SUB NOTE DIRS ####

  types <- get_file_types(fl, settings)
  fl_ex <- fl[types=="NOTE" | types=="SUB"] # excluding dirs belonging to simple notes and subnotes
  fl_ex_dir <- get_project_note_dir_path(fl_ex, settings) # get the note's dirs

  # remove the project note dirs from dls
  for(fled in fl_ex_dir) {
    dls <- dls[dls != fled]
  }


  #### recursively get more files from next level of sub-dirs ####

  # recurses with each set of dls retrieved!
  for(d in dls) {
    fileList <- get_file_list_to_project_notes(
                    fileList, d, settings,
                    fileExtensions, pathExclusions, retrievalDateTimeCutoff)
  }

  #### return list of files down to project note dirs ####

  # return fileList
  fileList


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


get_project_prefix_from_path <- function(projectPath, settings) {
  get_project_prefix_from_name(basename(projectPath), settings )
}


#' Get Project Prefix From Name
#'
#' Extracts the Project Prefix from the projectName - all characters before "~_"
#'
#'
get_project_prefix_from_name <- function(projectFileName, settings) {
  substr(projectFileName, 1, regexpr(settings[["ProjectPrefixSep"]], projectFileName)-(nchar(settings[["ProjectPrefixSep"]])-1 ) )
}


#' Get Name from File Name
#'
#' fileName is PREFIX~_COMP_TITLE.Rmd - returned is COMP_TITLE
#'
#'
get_name_from_file_name <- function(projectFileName, settings) {

  substring(projectFileName,
            first=regexpr(settings[["ProjectPrefixSep"]], projectFileName, fixed=TRUE) + nchar(settings[["ProjectPrefixSep"]]),
            last=regexpr( paste0(".", settings[["FileTypeSuffix"]]), projectFileName, fixed=TRUE)-1  )
}



#' Get File Contents Headers
#'
#' Get every Markdown header line from text file - every line that begins with
#' `#`.  Pre-filter contents to remove all code blocks, that may have lines
#' beginning with `#` but are not markdown headers.
#'
#' @param contents Character vector containing the contents of the file.
#'
#' @return Character vector of every header line.
get_file_contents_headers <- function(contents) {

  # trim contents to remove all code sections - lines starting with ```
  contentCodeIndices <- which(startsWith(contents, '```'))
  if( length(contentCodeIndices) > 1 ) {
    for(i in (floor((length(contentCodeIndices)-1)/2)):0 ) {
      i1 <- (i*2+1)
      i2 <- (i*2+2)
      contents <- c(contents[1:contentCodeIndices[i1]], contents[contentCodeIndices[i2]:length(contents)])
    }
  }

  # trim contents to all header lines - that start with #
  contentHeaders <- contents[startsWith(contents, '#')]

  contentHeaders

}


#' Get Content Declaration from Project Note
#'
#' Declarations of content identified as existing between `settings[["ContentSep"]]`
#' delimiters.  This function gets all declared content from `sourceNoteRmdPath`,
#' and then filters these to identify the one contentDeclaration that is within
#' the selection made in `selectionSource`.
#'
#' @param sourceNoteRmdPath Path to source project note Rmd
#'
#' @param selectionSource The selection made within source project note, should
#' be made on a content declaration.
#'
#' @param settings ProjectManagr organisation settings.yml file
#'
#' @param orgPath The path to the root of the organisation
#'
#' @return List of content declaration containing named parameters: "contentTitle",
#' "contentDescription", "contentSource", "contentStartLine", "contentEndLine",
#' "projectNotePath", "projectNoteContentHeader"
#'
get_content_declaration <- function(sourceNoteRmdPath, selectionSource, settings, orgPath) {

  content <- list()
  contents <- get_content_declarations(sourceNoteRmdPath, settings, orgPath)

  # filter contents to desired content declaration based on selectionSource originalLineNumber
  for( i in 1:length(contents)) {

    if( dplyr::between(selectionSource$originalLineNumber,
                       contents[[i]][['contentStartLine']],
                       contents[[i]][['contentEndLine']]) ) {
      content <- contents[[i]]
      break
    }
  }

  content
}


#' Extract metadata from each declaration of content in source note
#'
#' Declarations of content identified as existing between `settings[["ContentSep"]]`
#' delimiters.  This function gets all declared content from `sourceNoteRmdPath`,
#' and returns a named list of all content identified.
#'
#' @param sourceNoteRmdPath Path to source project note Rmd
#'
#' @param settings ProjectManagr organisation settings.yml file
#'
#' @param orgPath The path to the root of the organisation
#'
#' @return List of content declaration containing named parameters: "contentTitle",
#' "contentDescription", "contentSource", "contentStartLine", "contentEndLine",
#' "projectNotePath", "projectNoteContentHeader"
#'
get_content_declarations <- function(sourceNoteRmdPath, settings, orgPath) {

  # get sourceNote contents from path
  sourceNoteRmdContents <- read_file(sourceNoteRmdPath)

  # blank list - filled with metadata of each identified declaration of contents
  contents <- list()

  # open content sep delimiter
  contentSepContents <- load_param_vector(settings[["ContentSep"]], orgPath)

  ps <- match_vector(contentSepContents, sourceNoteRmdContents)

  # content seps should be in PAIRS
  if( length(ps) %% 2 != 0 ) {
    # DEAL WITH ERROR
    stop( cat("  file contains uneven number of content separators: ", sourceNoteRmdPath))
  }

  # if the length is 0 just return blank list
  if(length(ps) == 0 ) {
    return( contents )
  }

  # extract metadata from each content:
   # CONTENT_TITLE, CONTENT_DESCRIPTION, COUNTENT_SOURCE
  # START_LINE_NUM, END_LINE_NUM, PROJECT_NOTE_PATH
  for( i in 1:(length(ps)/2) ) {
    j <- (i*2)-1
    startSep <- ps[j]
    endSep <- ps[(j+1)] + length(contentSepContents) - 1
    contentDeclContents <- sourceNoteRmdContents[startSep:endSep]
    #contentDeclContents <- get_content_declaration_contents(sourceNoteRmdContents, startSep, settings, orgPath)

    contentTitle <- get_content_title(contentDeclContents, settings, orgPath)
    contentDescription <- get_content_description(contentDeclContents, settings, orgPath)
    contentSource <- get_absolute_path( dirname(sourceNoteRmdPath), # project note dir included in content path
                      get_path_from_link(get_content_source(contentDeclContents, settings, orgPath)))

    contentHeaders <- get_file_contents_headers(sourceNoteRmdContents[1:startSep])
    contentHeaderCurrent <- contentHeaders[length(contentHeaders)] # get last header
    #contentHeaderCurrent <- tolower(contentHeaderCurrent) # format: make lower case
    #contentHeaderCurrent <- trimws(gsub('#', '', contentHeaderCurrent, fixed=TRUE)) # remove leading #s & trim whitespace
    #contentHeaderCurrent <- paste0('#', gsub(' ', '-', contentHeaderCurrent, fixed=TRUE)) # replace spaces with '-' and add leading #

    attrs <- list(contentTitle, contentDescription, contentSource, startSep, endSep,
                  sourceNoteRmdPath, contentHeaderCurrent )
    names(attrs) <- c("contentTitle", "contentDescription", "contentSource",
                      "contentStartLine", "contentEndLine", "projectNotePath",
                      "projectNoteContentHeader")
    id <- paste0(sourceNoteRmdPath, ':::', startSep)
    contents[[id]] <- attrs

  }

  contents

}



get_content_declaration_contents <- function(sourceNoteRmdContents, startSep,
                                             settings, orgPath) {

  # open content sep delimiter
  contentSepContents <- load_param_vector(settings[["ContentSep"]], orgPath)

  ps <- match_vector(contentSepContents, sourceNoteRmdContents)

  for(p in 1:length(ps) ) {
    if( startSep <= ps[p] ) {
      endSep <- ps[(p+1)] + length(contentSepContents) - 1
      break
    }
  }

  contentDeclContents <- sourceNoteRmdContents[startSep:endSep]

  contentDeclContents

}


#' Get Content Title
#'
#' Extract content title from the content Contents, using the ContentTitleField
#' parameter declared in settings.
#'
#' @param contentDeclContents The Content Declaration Contents - character vector that
#' includes all lines between content separators.
#'
#' @param settings ProjectManagr organisation settings.yml file
#'
#' @param orgPath The path to the root of the organisation
#'
#' @return the Content Title value as string
get_content_title <- function(contentDeclContents, settings, orgPath) {

  # get titleLine in contentDeclContents: will contain ContentTitleField
  titleLine <- grep_line_index(load_param_vector(settings[["ContentTitleField"]], orgPath),
                               contentDeclContents)

  # get the content title from titleLine & ContentTitleField param
  contentTitle <- substring(contentDeclContents[titleLine],
                            regexpr(settings[["ContentTitleField"]], contentDeclContents[titleLine], fixed=TRUE) +
                              nchar(load_param_vector(settings[["ContentTitleField"]], orgPath)),
                            nchar(contentDeclContents[titleLine]))

  # return with leading & trailing whitespace removed
  trimws(contentTitle)

}


#' Get Content Description
#'
#' Extract content description from the content Contents, using the ContentDescriptionField
#' parameter declared in settings.
#'
#' @param contentDeclContents The Content Declaration Contents - character vector that
#' includes all lines between content separators.
#'
#' @param settings ProjectManagr organisation settings.yml file
#'
#' @param orgPath The path to the root of the organisation
#'
#' @return the Content Description value as string
get_content_description <- function(contentDeclContents, settings, orgPath) {

  # get descriptionLine in contentDeclContents: will contain ContentDescriptionField
  descriptionLine <- grep_line_index(load_param_vector(settings[["ContentDescriptionField"]], orgPath),
                               contentDeclContents)

  # get sourceLine in contentDeclContents: will contain ContentSourceField
  sourceLine <- grep_line_index(load_param_vector(settings[["ContentSourceField"]], orgPath),
                                contentDeclContents)

  # get the content Description : content between descriptionLine && sourceLine
  contentDescription <- contentDeclContents[(descriptionLine+1):(sourceLine-1)]

  # return with leading & trailing whitespace removed
  trimws(contentDescription)

}


#' Get Content Source
#'
#' Extract content Source from the content Contents, using the ContentSourceField
#' parameter declared in settings.
#'
#' @param contentDeclContents The Content Declaration Contents - character vector that
#' includes all lines between content separators.
#'
#' @param settings ProjectManagr organisation settings.yml file
#'
#' @param orgPath The path to the root of the organisation
#'
#' @return the Content Source value as string
get_content_source <- function(contentDeclContents, settings, orgPath) {

  # get SourceLine in contentDeclContents: will contain ContentSourceField
  SourceLine <- grep_line_index(load_param_vector(settings[["ContentSourceField"]], orgPath),
                               contentDeclContents)

  # get the content Source from SourceLine & ContentSourceField param
  contentSource <- substring(contentDeclContents[SourceLine],
                            regexpr(settings[["ContentSourceField"]], contentDeclContents[SourceLine], fixed=TRUE) +
                              nchar(load_param_vector(settings[["ContentSourceField"]], orgPath)),
                            nchar(contentDeclContents[SourceLine]))

  # return with leading & trailing whitespace removed
  trimws(contentSource)

}


#' Find Insertable Contents in Project Notes in ORG Tree
#'
#' Searches through Organisation recursively for Project Notes, and then looks
#' for Insertable Contents in these based on the content sep delimiter - defined
#' in `CONTENT_SEP.txt`.
#'
#' Eliminates directories in ORG ROOT which should note be searched: volumes/ &
#' .config/ & site/ & weekly-journal
#'
#' Returns structured lists that describe each contents in the project notes:
#'
#' Each Structured List contains: is a character vector comprised
#' name() : ProjectNotePath && Project Note Line Number where Insertable
#' Contents BEGINS (start of first delimiter), separated with `:::`
#'
#' [["contentTitle"]] : Title of Insertable Contents in the Project Note
#'
#' [["contentDescription"]] : Description of Insertable Contents in the Project Note
#'
#' [["contentSource"]] : Absolute Path to Contents declared in the Project Note
#'
#' [["contentStartLine"]] : Start line of the contents declaration in Project Note
#'
#' [["contentEndLine"]] : End line of the contents declaration in Project Note
#'
#' [["projectNotePath"]] : Path to Project Note
#'
#' [["projectNoteContentHeader"]] : Identified markdwon header section that the
#' Contents Declaration is in.
#'
#' @param orgPath Organisation path - also used to search for content declatations
#' within project notes.
#'
#' @param settings settings.yml in org config.
#'
#' @return List of content declaration containing named parameters: "contentTitle",
#' "contentDescription", "contentSource", "contentStartLine", "contentEndLine",
#' "projectNotePath", "projectNoteContentHeader"
#'
find_contents_org_tree <- function(orgPath, settings) {

  #### instance variables ####

  contents <- list() # to store all retrieved contents metadata in

  # get config templates settings yml
  confPath <- get_config_dir(orgPath)
  tempPath <- get_template_dir(orgPath)
  settings <- get_settings_yml(orgPath)

  # get dirs in root to EXCLUDE from search
  volPath <- get_volumes_dir(orgPath, settings)
  sitePath <- get_site_dir(orgPath, settings)
  weeklyjournalPath <- get_weekly_journal_dir(orgPath, settings)

  # get status yml
  #status <- get_status_yml(orgPath, settings)

  # read status information for CONTENTS if it exists
  #contentsStatus <- status[['CONTENTS']]

  contentSepContents <- load_param_vector(settings[["ContentSep"]], orgPath)
  # as this is called in a for loop, better to open this file external to that loop and pass as param


  #### loop through filePaths ####

  # get all project notes in orgPath RECURSIVELY
  fileList <- list()
  filePaths <- get_file_list_to_project_notes(
                  fileList, orgPath, settings,
                  pathExclusions = c(confPath, volPath, sitePath, weeklyjournalPath) )
   # EXCLUDING confPath volPath and sitePath from search!!
  # get all project notes in orgPath
  #filePaths <- get_project_note_paths(orgPath, settings)

  # for all identified project notes identify all insertable contents
  for( f in filePaths) {
    pr <- get_content_declarations(f, settings, orgPath)
    #pr <- get_contents(f, contentSepContents, settings, orgPath)
    contents[names(pr)] <- pr
  }

  # return structured list
  contents

}



#' Update Insertable Contents in Project Notes in ORG Tree
#'
#' Updates existing cache of insertable contents from Project Notes in ORG.
#'
#' This is faster than running `find_contents_org_tree` as will only read project
#' notes that have been modified since the last `contentRetrievalDateTime`.
#'
#' Looks through Organisation recursively for Project Notes, and then looks
#' for Insertable Contents in these based on the content sep delimiter - defined
#' in `CONTENT_SEP.txt`.
#'
#' Eliminates directories in ORG ROOT which should note be searched: volumes/ &
#' .config/ & site/ & weekly-journal
#'
#' Returns structured lists that describe each contents in the project notes:
#'
#' Each Structured List contains: is a character vector comprised
#' name() : ProjectNotePath && Project Note Line Number where Insertable
#' Contents BEGINS (start of first delimiter), separated with `:::`
#'
#' [["contentTitle"]] : Title of Insertable Contents in the Project Note
#'
#' [["contentDescription"]] : Description of Insertable Contents in the Project Note
#'
#' [["contentSource"]] : Absolute Path to Contents declared in the Project Note
#'
#' [["contentStartLine"]] : Start line of the contents declaration in Project Note
#'
#' [["contentEndLine"]] : End line of the contents declaration in Project Note
#'
#' [["projectNotePath"]] : Path to Project Note
#'
#' [["projectNoteContentHeader"]] : Identified markdwon header section that the
#' Contents Declaration is in.
#'
#' @param contentsCache Named List of contents previous cached - comprising the
#' contentRetrievalDateTime, and then contents - list of paths to insertable
#' contents files.  First level in list is the SOURCE DIR - Org Path - shown with
#' function call `names(contentsCache)`.  Second level is
#' "contentRetrievalDateTime" & "contents" in Org Path - shown with call to
#' `names(contentsCahce[[1]])`, and the actual contents declaration titles are accessed
#' with a call to `names(contentsCache[[1]][['contents']])`
#'
#' @param orgPath Organisation path - also used to search for content declatations
#' within project notes.
#'
#' @param settings settings.yml in org config.
#'
#' @return List of content declaration containing named parameters: "contentTitle",
#' "contentDescription", "contentSource", "contentStartLine", "contentEndLine",
#' "projectNotePath", "projectNoteContentHeader"
#'
update_contents_org_tree <- function(contentsCache, orgPath, settings) {

  #### instance variables ####

  # get the retrieval datetime as string
  dtStr <- lapply(X = contentsCache, FUN = `[[`, "contentRetrievalDateTime")
  dt <- lubridate::ymd_hm(dtStr) # convert to datetime

  #contents <- list() # to store all retrieved contents metadata in

  # get config templates settings yml
  confPath <- get_config_dir(orgPath)
  tempPath <- get_template_dir(orgPath)
  settings <- get_settings_yml(orgPath)

  # get dirs in root to EXCLUDE from search
  volPath <- get_volumes_dir(orgPath, settings)
  sitePath <- get_site_dir(orgPath, settings)
  weeklyjournalPath <- get_weekly_journal_dir(orgPath, settings)

  # get status yml
  #status <- get_status_yml(orgPath, settings)

  # read status information for CONTENTS if it exists
  #contentsStatus <- status[['CONTENTS']]

  contentSepContents <- load_param_vector(settings[["ContentSep"]], orgPath)
  # as this is called in a for loop, better to open this file external to that loop and pass as param


  #### loop through filePaths ####

  # get all project notes in orgPath RECURSIVELY
  fileList <- list()
  filePaths <- get_file_list_to_project_notes(
    fileList, orgPath, settings,
    pathExclusions = c(confPath, volPath, sitePath, weeklyjournalPath),
    retrievalDateTimeCutoff = dt )
  # EXCLUDING confPath volPath and sitePath from search!!
  # AND applying the cutoff datetime from cache!

  # get all project notes in orgPath
  #filePaths <- get_project_note_paths(orgPath, settings)

  # for all identified project notes identify all insertable contents
  for( f in filePaths) {
    pr <- get_content_declarations(f, settings, orgPath)
    #pr <- get_contents(f, contentSepContents, settings, orgPath)

    # first REMOVE all references in contentsCache to content declarations of filePath f
    contentsCache[[1]][['contents']] <-
          contentsCache[[1]][['contents']][ ! startsWith(names(contentsCache[[1]][['contents']]), f) ]

    if( length(pr) > 0 ) {
      # if contents have been found - add these to contentsCache
      contentsCache[[1]][['contents']]  <- c(contentsCache[[1]][['contents']] , pr)
    }
    # over-write the contents declarations in contentsCache directly
    #contents[names(pr)] <- pr
  }

  # return structured lists : contentsCache 'contents'
  contentsCache[[1]][['contents']]

}


#' Write Insertable Contents Cache
#'
#' Once retrieved write to status yml file.
#'
#' @param contentRetrievalDateTime Datetime of retrieval
#'
#' @param contents A list indicating the source project note, location, description,
#' start and end lines of each insertable content file.
#'
#' @param orgPath Organisation path.
#'
#' @param status The current status file content as list.
#'
#' @param statusFile Path to current status file.
#'
write_insertable_contents_cache <- function(contentRetrievalDateTime, contents,
                                            orgPath, status, statusFile) {
  attrs <- list(contentRetrievalDateTime, contents )
  names(attrs) <- c("contentRetrievalDateTime", "contents")
  status[["CONTENTS"]][[orgPath]] <- attrs
  yaml::write_yaml( yaml::as.yaml(status), statusFile )
}


#' Find Insertable Contents in Project Notes from Dir Tree
#'
#' Searches through dirTree recursively for Project Notes, and then looks
#' for Insertable Contents in these based on the content sep delimiter - defined
#' in `CONTENT_SEP.txt`.
#'
#' Returns structured lists that describe each contents in the project notes:
#'
#' Each Structured List contains: is a character vector comprised
#' name() : ProjectNotePath && Project Note Line Number where Insertable
#' Contents BEGINS (start of first delimiter), separated with `:::`
#'
#' [["contentTitle"]] : Title of Insertable Contents in the Project Note
#'
#' [["contentDescription"]] : Description of Insertable Contents in the Project Note
#'
#' [["contentSource"]] : Absolute Path to Contents declared in the Project Note
#'
#' [["contentStartLine"]] : Start line of the contents declaration in Project Note
#'
#' [["contentEndLine"]] : End line of the contents declaration in Project Note
#'
#' [["projectNotePath"]] : Path to Project Note
#'
#' [["projectNoteContentHeader"]] : Identified markdwon header section that the
#' Contents Declaration is in.
#'
#' @param dirTree Directory tree to search for content declarations in within
#' project notes.
#'
#' @param orgPath Organisation path.
#'
#' @param settings settings.yml in org config.
#'
#' @return List of content declaration containing named parameters: "contentTitle",
#' "contentDescription", "contentSource", "contentStartLine", "contentEndLine",
#' "projectNotePath", "projectNoteContentHeader"
#'
find_contents_in_dir_tree <- function(dirTree, orgPath, settings) {

  #### instance variables ####

  contents <- list() # to store all retrieved contents metadata in

  # get config templates settings yml
  confPath <- get_config_dir(orgPath)
  tempPath <- get_template_dir(orgPath)
  settings <- get_settings_yml(orgPath)

  # get status yml
  status <- get_status_yml(orgPath, settings)

  # read status information for CONTENTS if it exists
  contentsStatus <- status[['CONTENTS']]

  contentSepContents <- load_param_vector(settings[["ContentSep"]], orgPath)
  # as this is called in a for loop, better to open this file external to that loop and pass as param


  #### loop through filePaths ####

  # get all project notes in dirTree RECURSIVELY
  fileList <- list()
  filePaths <- get_file_list_to_project_notes(fileList, dirTree, settings)
  # get all project notes in dirTree
  #filePaths <- get_project_note_paths(dirTree, settings)

  # for all identified project notes identify all insertable contents
  for( f in filePaths) {
    pr <- get_content_declarations(f, settings, orgPath)
    #pr <- get_contents(f, contentSepContents, settings, orgPath)
    contents[names(pr)] <- pr
  }

  # return structured list
  contents

}



#' Update Insertable Contents in List
#'
#' Updates the contents found
#'
#' Searches through dirTree (non-recursively) for Project Notes, and then looks
#' for Contents in these based on the content sep delimiter (identified in
#' `config/templates/` & `settings[["ContentSep"]]`).
#'
#' Returns a structured lists that describe each content in the project notes:
#'
#' Each Structured List contains: is a character vector comprised
#' name() : ProjectNotePath && Project Note Line Number where Insertable
#' Contents BEGINS (start of first delimiter), separated with `:::`
#' [["contentTitle"]] : Title of Insertable Contents in the Project Note
#'
update_contents_in_list <- function(contentsStatus, dirPath, orgPath, settings) {

  #### instance variables ####

  contentRetrievalDateTime <- contentsStatus[[dirPath]]$contentRetrievalDateTime
  prdt <- lubridate::ymd_hm(contentRetrievalDateTime) # get as datetime
  contents <- contentsStatus[[dirPath]]$contents

  # get config templates settings yml
  confPath <- get_config_dir(orgPath)
  tempPath <- get_template_dir(orgPath)
  settings <- get_settings_yml(orgPath)

  contentSepContents <- load_param_vector(settings[["ContentSep"]], orgPath)
  # in case needed to check new files

  #### loop through filePaths ####

  # first screen filePaths for project notes
  filePaths <- get_project_note_paths(dirPath, settings)

  for( f in filePaths) {

    # get the last modified datetime mtime
    updateTime <- lubridate::force_tz(file.info(f)[,5], "UTC") # retrieve mtime for file in UTC

    if( prdt < updateTime ) {

      # if TRUE : retrieval datetime BEFORE updateTime of file
      # potentially new contents may have been added to this content

      # so retrieve contents information from the project note - this handles blank lists!
      pr <- get_content_declarations(f, settings, orgPath)
      contents[names(pr)] <- pr # update the contents of contents

    }
  }

  # update contents
  contentsStatus[[dirPath]]$contents <- contents

  # update retrieval time
  contentsStatus[[dirPath]]$contentRetrievalDateTime <- get_datetime()

  # return contentsStatus for dirPath
  contentsStatus[[dirPath]]

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

  # get config templates settings yml
  confPath <- get_config_dir(orgPath)
  tempPath <- get_template_dir(orgPath)
  settings <- get_settings_yml(orgPath)

  # get status yml
  statusFile <- get_status_yml_file(orgPath, settings)
  status <- get_status_yml(orgPath, settings)


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


  ### todo ######

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





