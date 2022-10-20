#' Update Project Organisation
#'
#' Compares all Project Notes to the Last Update date-time (from config/status.yml).  If a
#' Project Note has been edited since, reads it summary information for each Project Doc Link.
#' If there is content, navigates to the Project Doc and compares its content with the Project
#' Note summary content.  If different, the Project Doc content is updated with the new Project
#' Note summary content.
#'
#' The summary information contains an executive summary of progress on the current project
#' notes work on the Project Doc TASK it is linked to.  It may ALSO contain a TODO section
#' (by default denoted by the delimiter **TODO**), which this method will cache in `config/todo-cache`
#' in the orgPath.  This is then retrieved when calling the summariseToDo() method.
#'
#' @param fileSystemPath an absolute path in the filesystem - should be within an Organisation.
#'
#' @export
updateProjectOrg <- function( fileSystemPath=getwd()  ) {

  cat( "\nprojectmanagr::update():\n" )

  orgPath <- findOrgDir(fileSystemPath)

  if(orgPath == "" ) {
    # the search reached the root of the filesystem without finding the Organisation files,
    # therefore, projectNotePath is not inside a PROGRAMME sub-dir!
    stop( paste0("  fileSystemPath is not in an ORGANISATION Directory: ", fileSystemPath) )
  }
  # now, orgPath should be the root dir of the organisation

  confPath <- paste0( orgPath, .Platform$file.sep, "config" )
  todoPath <- paste0( confPath, .Platform$file.sep, "todo-cache" )

  # get TaskTodoSectionHeader value && goal del task Headers values
  settingsFile <- paste( confPath, .Platform$file.sep, "settings.yml", sep="" )
  settings <- yaml::yaml.load( yaml::read_yaml( settingsFile ) )
  todoHeader <- settings[["TaskTodoSectionHeader"]]
  goalHeader <- settings[["GoalHeader"]]
  delHeader <- settings[["DeliverableHeader"]]
  taskHeader <- settings[["TaskHeader"]]

  # get config/status.yml and read updateTime:
  statusFile <- paste( confPath, .Platform$file.sep, "status.yml", sep="" )
  status <- yaml::yaml.load( yaml::read_yaml( statusFile ) )

  updateTime <- as.POSIXct(status$updateTime)


  # read through whole Organisation > all Project Notes:
  # Project Note defined as a file found in a sub DIR of PROGRAMME/, or PROGRAMME/PROJECTS DIR
  # Project Note has the string "~_" in its file name

  # list of all DIRS in the orgPath - will include PROGRAMMES and config DIRS:
  progList <- list.dirs(orgPath, recursive=FALSE)

  # remove standard DIRS:
    # config, docs, volumes:
  progList <- progList[ !endsWith(progList, "config" ) &
                        !endsWith(progList, "docs" ) &
                        !endsWith(progList, "volumes" ) ]

  # for each PROGRAMME DIR:
    # list all sub-Dirs in PROGRAMME, and all sub-dirs in PROGRAMME/PROJECTS
    # for each which is NOT the PROJECTS Dir -> look inside

  dirsList <- c()

  for(i in 1:length(progList) ) {

    dirList <- list.dirs(progList[i], recursive=FALSE)
    dirList <- dirList[ !endsWith(dirList, "PROJECTS" ) ]
    #dirList <- paste0(progList[i], .Platform$file.sep, dirList)

    projList <- list.dirs( paste0(progList[i], .Platform$file.sep, "PROJECTS" ), recursive=FALSE )
    #projList <- paste0(progList[i], .Platform$file.sep, "PROJECTS", .Platform$file.sep, projList)

    dirsList <- c(dirsList, dirList, projList)

  }

  # remove standard DIRS:
  # REF SOP DB
  dirsList <- dirsList[ !endsWith(dirsList, "REF" ) &
                          !endsWith(dirsList, "SOP" ) &
                          !endsWith(dirsList, "DB" ) ]



  for(i in 1:length(dirsList) ) {

    cat( "\n  Checking Directory:", dirsList[i],"\n" )

    # get full paths:
    fileList <- paste0( dirsList[i], .Platform$file.sep, list.files(dirsList[i], recursive=TRUE) )

    # filter for Project Notes, NOT Header Notes, Rmd files:
    fileList <- fileList[ grepl( "~_", fileList, fixed = TRUE ) &
                             !grepl( "-00~_", fileList, fixed = TRUE ) &
                             endsWith(fileList, ".Rmd") ]


    if( length(fileList > 0) ) {

      fileList <- fileList[ sapply(fileList, function(x) file.info(x)[,5] > updateTime ) ] # filter against updateTime

      if( length(fileList) != 0 ) {

        # check the summary information for each Project Doc Link in each Project Note:
        for(j in 1:length(fileList) ) {

          cat( "    checking Project Note -", j, ":", basename(fileList[j]),"\n" )
          checkProjectNote(fileList[j], todoHeader, goalHeader, delHeader, taskHeader)

        }

      }
    }
  }

  # finally, add new update time to yaml:
  status$updateTime <- as.character( Sys.time() )
  yaml::write_yaml( yaml::as.yaml(status), statusFile )

  cat( "\n  Written UPDATE Time to Status.yml file: ", statusFile, "\n\n" )

}



#' Check Project Note
#'
#' Opens Project Note, collects its Project Doc Links, and then compares
#' the Summary information between the Note and its Doc Links.
#'
#'
checkProjectNote <- function(path, todoHeader, goalHeader, delHeader, taskHeader) {

  # open project note:
  fileConn <- file(path)
  contents <- readLines(fileConn)
  close(fileConn)

  #projDocs <- getProjectNoteDocLinkList(contents, path, todoHeader, goalHeader, delHeader, taskHeader)
  projDocs <- getProjectNoteSummaryTodoList(contents, path, todoHeader, goalHeader, delHeader, taskHeader)

  # check summary contents (projDocs[[i]][[5]] list) against the summary in each Project Doc:

  for(i in 1:length(projDocs) ) {

    compareGoalDelTaskSummary(projDocs[[i]], path)

  }

}



#' Get Project Note Summary and Todo Link List
#'
#' Returns a list of VECTORS:
#'
#'  list[[i]] : A VECTOR that includes the Project Doc ABSOLUTE Link, GOAL Num, DEL Num, TASK Num.
#'
#'  list[[i]][[1]] : Project Doc ABSOLUTE Link
#'
#'  list[[i]][[2]] : Project Doc GOAL - number plus title
#'
#'  list[[i]][[3]] : Project Doc DELIVERABLE - number plus title
#'
#'  list[[i]][[4]] : Project Doc TASK - number plus title
#'
#'  list[[i]][[5]] : Vector containing all summary information in Project Note for this
#'                   Project Doc GOAL/DEL/TASK
#'  list[[i]][[6]] : Vector containing all TODO information in Project Note for this
#'                   Project Doc GOAL/DEL/TASK
#'
#'  length( list ) returns the number of Project Doc links in the returned list.
#'
#'  @param projectNoteContents Character vector containing the contents of a Project Note, which includes links to
#'  at least one ProjectDoc, separated by "----", and ended with "------".
#'
#'  @param projectNotePath the FULL PATH to the project note, from which the contents is derived.
#'
#'  @param todoHeader The header of any TODO section that may be defined under the project note summary.  This Header
#'  and remaining content of a summary should be saved SEPARATELY to the summaryVector.
#'
getProjectNoteSummaryTodoList <- function(contents, projectNotePath, todoHeader, goalHeader, delHeader, taskHeader) {


  # instantiate a list to store links
  linkList <- list()

  summaryVector <- c()
  todoVector <- c()

  # start indices at 1:
  linkIndex <- 1
  goaldeltaskIndex <- 1

  orgPath <- findOrgDir(fileSystemPath)

  if(orgPath == "" ) {
    # the search reached the root of the filesystem without finding the Organisation files,
    # therefore, projectNotePath is not inside a PROGRAMME sub-dir!
    stop( paste0("  fileSystemPath is not in an ORGANISATION Directory: ", fileSystemPath) )
  }
  # now, orgPath should be the root dir of the organisation

  confPath <- paste0( orgPath, .Platform$file.sep, "config" )
  todoPath <- paste0( confPath, .Platform$file.sep, "todo-cache" )

  # get all objectives as vector
  objectives <- contents[ ( grep("# OBJECTIVES", contents, fixed=TRUE) +1 ):( grep("------", contents)[1] -1 ) ]

  objs <- splitAt(objectives, grep("----", objectives))

  for( o in objs) {

    # get FIRST INSTANCE of a link - this will be ProjectDocLink
    pnc <- o[which(startsWith(o, "["))[1]]
    projDocRelLink <- substring(pnc, first=regexpr("\\(", pnc)+1, last=regexpr("\\)", pnc)-1 )
    linkList[[linkIndex]] <- computePath(projectNotePath, projDocRelLink)
    goaldeltaskIndex <- goaldeltaskIndex +1 # increment goalDelTask index to 2

    # Get GOAL Title
    pnc <- o[which(startsWith(o, goalHeader))[1]]
    linkList[[linkIndex]][goaldeltaskIndex] <- paste0( "## ", substring(pnc, first=4, last=regexpr("]", pnc)-1 ) )
    goaldeltaskIndex <- goaldeltaskIndex +1 # increment index

    # Get DELIVERABLE Title
    pnc <- o[which(startsWith(o, delHeader))[1]]
    linkList[[linkIndex]][goaldeltaskIndex] <- paste0( "## ", substring(pnc, first=8, last=regexpr("]", pnc)-1 ) )
    goaldeltaskIndex <- goaldeltaskIndex +1 # increment index

    # Get TASK Title
    pnc <- o[which(startsWith(o, taskHeader))[1]]
    linkList[[linkIndex]][goaldeltaskIndex] <- paste0( "## ", substring(pnc, first=12, last=regexpr("]", pnc)-1 ) )
    goaldeltaskIndex <- goaldeltaskIndex +1 # increment index

    # from Task Title index to end of o extract summary and TODO
    taskIndex <- which(startsWith(o, taskHeader))[1]
    todoIndex <-which(startsWith(o, todoHeader))[1]

    if( is.na(todoIndex) == TRUE ) {
      # no todo header in summary, so extract ALL REMAINING vector to summaryVector
      summaryVector <- o[ (taskIndex+1) : length(o) ]
      linkList[[linkIndex]][goaldeltaskIndex] <- list(summaryVector)
      goaldeltaskIndex <- goaldeltaskIndex +1 # increment index
      linkList[[linkIndex]][goaldeltaskIndex] <- list(c("")) # insert a blank vector in place of TODO Vector

    } else {
      # todo was found! split at todo
      summaryVector <- o[ (taskIndex+1) : (todoIndex-1) ] # remove TODO HEADER
      linkList[[linkIndex]][goaldeltaskIndex] <- list(summaryVector)
      goaldeltaskIndex <- goaldeltaskIndex +1 # increment index
      todoVector <- o[ (todoIndex) : length(o) ] # keep TODO HEADER
      linkList[[linkIndex]][goaldeltaskIndex] <- list(todoVector)
    }

    # adjust indices
    linkIndex <- linkIndex + 1
    goaldeltaskIndex <- 1

  }

  #return the linkList
  linkList

}


#' Split Vector at positions
#'
#'
splitAt <- function(x, pos) {
  unname(split(x, cumsum(seq_along(x) %in% pos)))
}


#' Compare Project Goal/Del/Task Summary
#'
#' Open Project Doc and extract the Goal/Del/Task from it.  Compare its summary information
#' to the Project Note, and if they do not match, write the summary information in the
#' Project Note to the Project Doc.
#'
#'
compareGoalDelTaskSummary <- function(projDocLinkList, projNotePath) {

  # open project doc:
  fileConn <- file(projDocLinkList[[1]])
  projDocContents <- readLines(fileConn)
  close(fileConn)

  # find the ProjDoc GOAL, DEL, TASK > projNote LINK line in contents:

  min <- 1
  max <- length(projDocContents)


  min <- min + grep(projDocLinkList[[2]], projDocContents, fixed=TRUE)[1] -1

  if( is.na(min) ) {
    # finding GOAL failed!
    stop( paste0("  failed to find GOAL in ", projDocLinkList[[1]], " - GOAL : ", projDocLinkList[[2]]) )
  }


  # to GOAL:
  projDocContents2 <- projDocContents[
                        grep(projDocLinkList[[2]], projDocContents, fixed=TRUE)[1] :
                          length(projDocContents) ]



  min <- min + grep(projDocLinkList[[3]], projDocContents2, fixed=TRUE)[1] -1

  if( is.na(min) ) {
    # finding DEL failed!
    stop( paste0("  failed to find DELIVERABLE in ", projDocLinkList[[1]], " - DELIVERABLE : ", projDocLinkList[[3]]) )
  }

  # to DEL:
  projDocContents2 <- projDocContents2[
                        grep(projDocLinkList[[3]], projDocContents2, fixed=TRUE)[1] :
                          length(projDocContents2) ]


  min <- min + grep(projDocLinkList[[4]], projDocContents2, fixed=TRUE)[1] -1

  if( is.na(min) ) {
    # finding TASK failed!
    stop( paste0("  failed to find TASK in ", projDocLinkList[[1]], " - TASK : ", projDocLinkList[[4]]) )
  }

  # to TASK:
  projDocContents2 <- projDocContents2[
                        grep(projDocLinkList[[4]], projDocContents2, fixed=TRUE)[1] :
                          length(projDocContents2) ]


  max <- (grep("---", projDocContents2, fixed=TRUE)[1] + min -2)

  # up to END of TASK:  "---"
  projDocContents2 <- projDocContents2[
                        1 :
                          grep("---", projDocContents2, fixed=TRUE)[1] -1 ]


  min <- min + grep( paste0(basename(projNotePath), ")" ), projDocContents2, fixed=TRUE)[1]

  # to projNote Link:
  projDocContents2 <- projDocContents2[
                        (grep( paste0(basename(projNotePath), ")" ), projDocContents2, fixed=TRUE)[1] +1 ) :
                          length(projDocContents2) ]



  if( !is.na(grep("**[", projDocContents2, fixed=TRUE)[1]) ) {
    max <- (grep("**[", projDocContents2, fixed=TRUE)[1] + min -2)
  # up to next Project Note link:
  projDocContents2 <- projDocContents2[
                        1 :
                          ( grep("**[", projDocContents2, fixed=TRUE)[1] -1 ) ]
  }


  if( !is.na(grep("*[", projDocContents2, fixed=TRUE)[1]) ) {
    max <- (grep("*[", projDocContents2, fixed=TRUE)[1] + min -2)
  # up to next Sub Note link:
  projDocContents2 <- projDocContents2[
                        1 :
                          ( grep("*[", projDocContents2, fixed=TRUE)[1] -1 ) ]

  }

  # extract only the lines which contain any text/numbers:
  projDocContents2 <- projDocContents2[ grepl("[A-Za-z0-9]", projDocContents2) ]

  # test if the Summary text in the Project Doc matches the Summary text in the Project Note
  summaryMatch <- all(length(projDocContents2)==length(projDocLinkList[[5]])) &&
                  all(projDocContents2==projDocLinkList[[5]])

  # if these do not match, INSERT text from Project Note into Project Doc - use original projDocContents:
  if( summaryMatch == FALSE ) {

    # log this in output:
    cat( "    updating Project Note summary -", basename(projNotePath), ":", basename(projDocLinkList[[1]]),"\n" )

    # get appropriately spaced Character Vector for Summary Bullet Points:
    summaryBullets <- spaceSummaryBulletPoints(projDocLinkList[[5]])


    # remove projDocContents[min:max] and replace with projDocLinkList[[5]]
    projDocContents <- c( projDocContents[1:(min-1)],
                          summaryBullets,
                          projDocContents[(max+1):length(projDocContents)] )

    # write this new projDocContents to the Project Doc:
    fileConn <- file(projDocLinkList[[1]])
    writeLines(projDocContents, fileConn)
    close(fileConn)

  }


}
