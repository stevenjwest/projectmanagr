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


  #### DOC TASK TITLE ####

  # create DocTaskTitle - DocName plus the TaskTitle
  DocTaskTitle <- get_doc_task_title(DocPrefix, task, settings)

  # return
  l <- list(DocTaskTitle, DocTitleLink, GoalTitleLink, DelTitleLink, TaskTitleLink)
  names(l) <- c("title", "link", "goal", "del", "task")
  l

}


#' update GDT links in all project notes with new Task Title that are linked
#' to a given GDT in Project Doc.
#'
#' Updating the GDT Link TITLE - which contains the Task String.
#'
#' Updating the GDT TASK LINK - which is formed from the Task String.
#'
update_GDT_link_task <- function(projectDocPath, projectDocContents, taskSelection,
                                 newTaskName, settings) {

  # get note absolute paths from project doc GDT
  notePaths <- get_doc_GDT_link_note_paths(projectDocPath, projectDocContents,
                                           taskSelection[['taskLine']], settings)

  # if paths to notes have been found:
  if(length(notePaths) > 0 ) {

    # loop through all notes
    for(pn in notePaths) {
      # generate old GDT and new GDT structures
      oldGDT <- compute_doc_GDT_link(projectDocPath, pn, settings,
                                     taskSelection[["goal"]],
                                     taskSelection[["deliverable"]],
                                     taskSelection[["task"]])
      newGDT <- compute_doc_GDT_link(projectDocPath, pn, settings,
                                     taskSelection[["goal"]],
                                     taskSelection[["deliverable"]],
                                     paste0(settings[['ProjectTaskHeader']], ' ',
                                            newTaskName) )

      # replace lines in pn contents:
      pnc <- read_file(pn)
      #oldGDT$title -> newGDT$title
      titlei <- grep(oldGDT$title, pnc, fixed=TRUE)[1] # get first - should only be one GDT link title!
      pnc[titlei] <- newGDT$title

      #oldGDT$task -> newGDT$task
      taski <- grep(oldGDT$task, pnc, fixed=TRUE)[1] # get first - should only be one GDT link task!
      pnc[taski] <- newGDT$task

      write_file(pnc, pn)
    } # end loop pn
  } # end if pn > 0
  # finished!
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


#' get doc task title
#'
#' `DocPrefix` is prefix to project document that `task` is from.
#'
#' `task` as collected from selection[["task"]] - fitting the format specified
#' in settings[["ProjectTaskHeader"]].
#'
#'  Returns DocTaskTitle Header - DocName plus the TaskTitle, spearated with ` : `.
#'
get_doc_task_title <- function(DocPrefix, task, settings) {
  # create DocTitle - DocPrefix plus the Gnum Dnum Tnum
  #paste( "## ", DocName, " : G", goalNum, " D", delNum, " T", taskNum, sep="")
  paste0( settings[["NoteSummaryTitle"]] ,
          DocPrefix, " : ", get_task_title(task, settings) )
}

#' get doc task title
#'
#' Used for forming the GDT link - see `copmute_doc_GDT_link()`
#'
#' `DocPrefix` is prefix to project document that `task` is from.
#'
#' `taskName` as collected from `get_task_title(selection[["task"]])`, is the
#' task name string of a task.
#'
#'  Returns DocTaskTitle Header - DocName plus the TaskTitle, spearated with ` : `.
#'
get_doc_task_title_name <- function(DocPrefix, taskName, settings) {
  # create DocTitle - DocPrefix plus the Gnum Dnum Tnum
  #paste( "## ", DocName, " : G", goalNum, " D", delNum, " T", taskNum, sep="")
  paste0( settings[["NoteSummaryTitle"]] ,
          DocPrefix, " : ", taskName )
}

