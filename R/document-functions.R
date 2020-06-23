#' Add Project Doc Links to Project Note.
#'
#' Assumes the Project Note MAY not have ANY Project Doc links written to it, and
#' therefore still contain the original Objective.Rmd template Variables.
#'
#' @param projNoteContents character vector containing the initial contents of a project note.
#' @param projNotePath ABSOLUTE path to the location on the filesystem where project note will be saved.
#' @param projDocList List containing all Project Docs - Absolute path, Goal/Del/Task.  Like the list
#' returned by getProjectNoteDocLinkList().
#' @param summaryBullet The Contents of the Summary Bullet - initial default is "* [POSIXct date-time]"
#'
#'
addLinks <- function(projNoteContents, projNotePath, projDocList) {

  for( i in 1:length(projDocList) ) {

    projectDocPath <- projDocList[[i]][[1]]

    # generate the summaryBullets from the projDocList:
    summaryBullets <- spaceSummaryBulletPoints(projDocList[[i]][[5]])

    cat("summaryBullets:", summaryBullets, "\n")
    cat("summaryBullets length:", length(summaryBullets), "\n")

    # compute Project Source Doc RELATIVE LINK:
    DocLink <- R.utils::getRelativePath(projectDocPath, relativeTo=projNotePath)
    DocLink <- substring(DocLink, first=4, last=nchar(DocLink)) # remove first `../`
    # DocLink <- paste( substring(DocLink, first=1, last=nchar(DocLink)-4), "/", sep="")
    # for now have left links as ".Rmd", but this needs to be ".html", or "/" in a rendered website!
    # Its set as ".Rmd" as ".Rmd" links can be navigated in RStudio!

    # NB Need to convert the .Rmd links to .html when WRITING the Organisation to a html site!

    DocName <- basename(projectDocPath)
    DocName <- gsub( "-", " ",  gsub("_", " ", substring(DocName, first=1, last=nchar(DocName)-4) )  )

    DocTitleLink <- paste( "## [", DocName, "](", DocLink, ")", sep="" )


    # GOAL:

    goal <- substring(projDocList[[i]][[2]], first=4)
    goalTitle <- substring(goal,  first=(regexpr(":", goal)+2 ) )
    goalNum <- as.integer(  substring(goal,  first=5, last=(regexpr(":", goal)-1) )  )

    goalTag <- paste("#", gsub("[ ]|[_]", "-", gsub("[:]", "", tolower(goal) ) ), ")", sep="" )

    GoalTitleLink <- paste("# [", goal, "](", DocLink, goalTag, sep="")


    # DEL:

    del <- substring(projDocList[[i]][[3]], first=5)
    delTitle <- substring(del,  first=(regexpr(":", del)+2 ) )
    delNum <- as.integer(  substring(del,  first=12, last=(regexpr(":", del)-1) )  )

    delTag <- paste("#", gsub("[ ]|[_]", "-", gsub("[:]", "", tolower(del) ) ), ")", sep="" )

    DelTitleLink <- paste("## [", del, "](", DocLink, delTag, sep="")


    # TASK:

    task <- substring(projDocList[[i]][[4]], first=6)
    taskTitle <- substring(task,  first=(regexpr(":", task)+2 ) )
    taskNum <- as.integer(  substring(task,  first=5, last=(regexpr(":", task)-1) )  )

    taskTag <- paste("#", gsub("[ ]|[_]", "-", gsub("[:]", "", tolower(task) ) ), ")", sep="" )

    TaskTitleLink <- paste("### [", task, "](", DocLink, taskTag, sep="")

    if( is.element("{{PROJECT_DOC_LINK}}", projNoteContents) ) { # if the VARIABLES are in contents:

      projNoteContents <- gsub("{{PROJECT_DOC_LINK}}", DocTitleLink, projNoteContents, fixed=TRUE)

      projNoteContents <- gsub("{{PROJECT_DOC_LINK_GOAL}}", GoalTitleLink, projNoteContents, fixed=TRUE)
      projNoteContents <- gsub("{{PROJECT_DOC_LINK_DEL}}", DelTitleLink, projNoteContents, fixed=TRUE)
      projNoteContents <- gsub("{{PROJECT_DOC_LINK_TASK}}", TaskTitleLink, projNoteContents, fixed=TRUE)

      # insert the projDocList[[i]][[5]] vector - it should be a single String, and inserted correctly:
      #projNoteContents <- gsub("{{SUMMARY_INFO}}",
      #                         projDocList[[i]][[5]],
      #                         projNoteContents, fixed=TRUE )

      # use replaceAndInsertVector ??
      sb <- summaryBullets[3:(length(summaryBullets)-3)]

      cat("sb:", sb, "\n")
      cat("sb length:", length(sb), "\n")

      projNoteContents <- replaceAndInsertVector("{{SUMMARY_INFO}}", sb, projNoteContents)

    }
    else { # insert the links at END of OBJECTIVES Section - marked by "------"

      # form the objectivesContents:
      objectivesContents <- c("----","","","",DocTitleLink,"","","",
                              GoalTitleLink,"","","",
                              DelTitleLink,"","","",
                              TaskTitleLink,
                              summaryBullets)

      # insert objectivesContents into the first line that matches the string "------"
      # "------" (6 x '-') denotes the END of the objectives section

      # compute place to insert the project note link:
      # get the line selected in the projectDoc - [["originalLine"]]
      line <- computeLineIndex("------", projNoteContents)

      # Insert projectNoteLinkVector to projNoteContents:
      projNoteContents <- c(projNoteContents[1:(line-1)], objectivesContents, projNoteContents[(line):length(projNoteContents)])

    }

    cat( "    Written Goal Del Task ", DocName, " to Sub Note file: ", basename(projNotePath),  "\n" )

  }

  projNoteContents

}




#' Update all links
#'
#' Updates every hyperlink in all Rmd files within orgPath directory tree, from oldName
#' to newName.
#'
#' @param orgPath defines the path to the Organisation - should point to the root directory of the Organisation.
#' @param oldName defines the OLD name that will be in Hyperlinks.  Should be the FILE NAME - with no spaces.
#' @param oldName defines the NEW name to be written into Hyperlinks.  Should be the FILE NAME - with no spaces.
#'
#' @export
updateAllLinks <- function( orgPath, oldName, oldTitle, newName, newTitle ) {


  #cat( "\nprojectmanagr::editProjectOrg():\n" )

  # first, confirm the current orgPath is an organisation:


  # Check orgPath is at the root of an ORGANISATION:

  # look for the config/ and templates/ dirs:
  confPath <- paste(orgPath, .Platform$file.sep, "config" , sep="")
  tempPath <- paste(confPath, .Platform$file.sep, "templates" , sep="")


  if(  !( file.exists(confPath) && file.exists(tempPath) )  ) {
    stop( paste0("  orgPath is not an ORGANISATION directory: ",orgPath) )
  }


  # check oldName contains NO SPACES:
  if( grepl("\\s+", oldName) ) {
    stop( paste0("  oldName contains a SPACE: ",oldName) )
  }


  # check newName contains NO SPACES:
  if( grepl("\\s+", newName) ) {
    stop( paste0("  newName contains a SPACE: ",newName) )
  }


  # define the old and new titles:
  if(nchar(oldTitle) == 0) {
    oldTitle  <- gsub("-", " ", gsub("_", " ", oldName) )
  }
  if( nchar(newTitle) == 0) {
    newTitle  <- gsub("-", " ", gsub("_", " ", newName) )
  }


  # traverse EVERY Rmd file in orgPath dir tree:
  file_list <- list.files(path = orgPath, pattern = "*.Rmd", all.files = TRUE, recursive = TRUE, include.dirs = TRUE)

  for(x in file_list) {
    # read file:
    fileConn <- file( paste0( orgPath, .Platform$file.sep, x) )
    contents <- readLines( fileConn )
    close(fileConn)

    # replace every instance of oldName with newName
    grepName <- any( grepl(oldName, contents, fixed=TRUE ))
    if(grepName) {
      contents <- gsub(oldName, newName, contents, fixed=TRUE)
    }

    # replace every instance of oldtitle with newTitle
    grepTitle <- any( grepl(oldTitle, contents, fixed=TRUE ))
    if(grepTitle) {
      contents <- gsub(oldTitle, newTitle, contents, fixed=TRUE)
    }

    # write file ONLY IF a replacement has been made
    if(grepTitle || grepName) {
      cat( "    replaced in file:", x ,"\n" )
      fileConn <- file( paste0( orgPath, .Platform$file.sep, x) )
      writeLines(contents, fileConn)
      close(fileConn)
    }
  }

  #lapply(file_list, function(x) {

    # read file:
   # fileConn <- file( paste0( orgPath, .Platform$file.sep, x) )
  #  contents <- readLines( fileConn )
   # close(fileConn)

    # replace every instance of oldName with newName
  #  grepName <- any( grepl(oldName, contents, fixed=TRUE ))
   # if(grepName) {
    #  contents <- gsub(oldName, newName, contents, fixed=TRUE)
    #}

    # replace every instance of oldtitle with newTitle
    #grepTitle <- any( grepl(oldTitle, contents, fixed=TRUE ))
  #  if(grepTitle) {
   #   contents <- gsub(oldTitle, newTitle, contents, fixed=TRUE)
  #  }

    # write file ONLY IF a replacement has been made
   # if(grepTitle || grepName) {
  #    cat( "    replaced in file:", x ,"\n" )
   #   fileConn <- file( paste0( orgPath, .Platform$file.sep, x) )
    #  writeLines(contents, fileConn)
     # close(fileConn)
    #}

  #})


}




#' Add Project Doc Links to Project Note.
#'
#' Assumes the Project Note MAY not have ANY Project Doc links written to it, and
#' therefore still contain the original Objective.Rmd template Variables.
#'
#' @param projDocList List containing all Project Docs - Absolute path, Goal/Del/Task.  Like the list
#' returned by getProjectNoteDocLinkList().
#' @param subNotePath ABSOLUTE path to the location on the filesystem where sub note is saved.
#' @param headerPath the Absolute path to the header note, which the subnote is stored under.
#'
addSubNoteLinkToDocs <- function(projDocList, subNotePath, headerPath) {

  # first, compute subnote prefix and title:
  subNotePrefix <- getProjectPrefixFromPath(subNotePath)
  subNoteName <- substr(basename(subNotePath), 1, nchar(basename(subNotePath))-4 )
  subNoteTitle <- gsub("-", " ", gsub("_", " ", subNoteName) )

  # compute headerName
  headerName <- basename(headerPath)

  for( i in 1:length(projDocList) ) {


    ### WRITE PROJECT NOTE TO PROJECT DOC:

    projectDocPath <- projDocList[[i]][[1]]

    # generate the summaryBullets from the projDocList:
    summaryBullets <- spaceSummaryBulletPoints(projDocList[[i]][[5]])

    # read Project Doc:
    projDocFileConn <- file( projectDocPath )
    projDocContents <- readLines( projDocFileConn )
    close(projDocFileConn)

    # create the projectNoteLink:
    NoteLink <- R.utils::getRelativePath(subNotePath, relativeTo=projectDocPath)
    NoteLink <- substring(NoteLink, first=4, last=nchar(NoteLink)) # remove first `../`
    projectNoteLink <- paste("*[", subNotePrefix, "~ ", subNoteTitle, "](", NoteLink, ")*",  sep="")
    # output ex.:
    # *[LAB~003-001~ THF MeOH/DCM Clearing Tau Labelling](../LAB/LAB~001-00~_thf_meoh_dcm_clearing_tau_labelling.Rmd)*

    sb <- summaryBullets[1:(length(summaryBullets)-2)]

    cat("sb:", sb, "\n")
    cat("sb length:", length(sb), "\n")

    # create the Vector, including summaryBullets (these have appropriate whitespace):
    projectNoteLinkVector <- c( "", "", "", projectNoteLink, sb )

    # compute place to insert the project note link:
    line <- computeHeaderLineUnderProjectDocGoalDelTask(headerName,
                                                        projDocContents,
                                                        projDocList[[i]][[2]],
                                                        projDocList[[i]][[3]],
                                                        projDocList[[i]][[4]] )


    # Insert projectNoteLinkVector to projDocContents:
    projDocContents <- c(projDocContents[1:(line-1)], projectNoteLinkVector, projDocContents[(line+1):length(projDocContents)])


    # write to projFile
    projDocFileConn <- file( projectDocPath )
    writeLines(projDocContents, projDocFileConn)
    close(projDocFileConn)

    cat( "  Written Project Sub Note Link ", basename(subNotePath), " to Project Doc: ", basename(projectDocPath), "\n" )



    ### WRITE PROJECT NOTE TO PROGRAMME INDEX FILE:  NOT USED

    # identify programme path:
    #progPath <- findProgDir(projectDocPath)

    # read Programme Index File:
    #progIndexPath = paste(progPath, .Platform$file.sep, basename(progPath), "_index.Rmd", sep="")
    #progIndexFileConn <- file( progIndexPath )
    #progIndexContents <- readLines( progIndexFileConn )
    #close(progIndexFileConn)


    # create the projIndexLink:
    #NoteLink <- R.utils::getRelativePath(subNotePath, relativeTo=progIndexPath)
    #NoteLink <- substring(NoteLink, first=4, last=nchar(NoteLink)) # remove first `../`
    #projIndexLink <- paste("* [", subNoteTitle, "](", NoteLink, ")",  sep="")

    # create the Vector, including Whitespace and Summary information:
    #projIndexLinkVector <- c( "", "", "", projIndexLink, "" )

    # compute place to insert the project doc link:
    # First get the line index containing containing the header Name
      # -1 as the projectDocPath line is counted TWICE in lineHead!
      # projectDocLine INCLUDES projectDocPath line, and the filtered projIndexContents includes it too!
    #projectDocLine <- grep(basename(projectDocPath), progIndexContents)
    #lineHead <- projectDocLine + grepLineIndex(
     #     basename(headerPath),
      #    progIndexContents[projectDocLine:grepLineIndexFrom("---", progIndexContents,projectDocLine)])-1

    # Then get the NEXT line that is before ##, ---, or **[, inside the headerNote Group:
    #lineProg <- computeNextLineHeader(lineHead, progIndexContents)

    # Insert projIndexLinkVector to progIndexContents:
      # only if projIndexLink doesnt already exist in the programme contents UNDER THE CURRENT HEADER NOTE GROUP
      # this ensures the first place the subnote is written is the only place it is written
      # BUT this should still allow the subnote to be written to the PROGRAMME file under other PROJECTS
    #if( is.element(projIndexLink, progIndexContents[lineHead:lineProg] ) == FALSE ) {

            #progIndexContents <- c(progIndexContents[1:(lineProg-1)], projIndexLinkVector, progIndexContents[(lineProg+1):length(progIndexContents)])

            #}


    # write to progIndexPath
    #progIndexFileConn <- file( progIndexPath )
    #writeLines(progIndexContents, progIndexFileConn)
    #close(progIndexFileConn)

    #cat( "  Written Project Note ", basename(subNotePath), " to Programme File: ", basename(progIndexPath), "\n" )

  }

}



#' Compute Next Line under Header Links, in Project Doc Goal Del Task
#'
#' In projDocContents, sequentially identifies lines containing GOAL, DELIVERABLE,
#' TASK, headerName.  Then identifies the next line AFTER all subnote links under the headerName
#' link.  This returned index is where the NEXT subnote should be inserted.
#'
#' @param headerName name of header note to search for
#' @param projDocContents character vector containing the Project Doc
#' @param GOAL the GOAL character string to find in projDocContents
#' @param DELIVERABLE the DELIVERABLE character string to find in projDocContents
#' @param TASK the TASK character string to find in projDocContents
#'
#'
computeHeaderLineUnderProjectDocGoalDelTask <- function(headerName, projDocContents,
                                                        GOAL, DELIVERABLE, TASK ) {

  #update headerName to include ")**" suffix:
  headerName <- paste0(headerName, ")**")

  # find first line that contains GOAL String
  for( j in 1:length(projDocContents)) {

    if( grepl(GOAL, projDocContents[j], fixed=TRUE) ) {

      goalIndex <- j

      break

    }

  }

  #cat( "  goalIndex: ", goalIndex, "\n" )

  # find first line that contains DELIVERABLE String FROM GOAL
  for( j in goalIndex:length(projDocContents)) {

    if( grepl(DELIVERABLE, projDocContents[j], fixed=TRUE) ) {

      delIndex <- j

      break

    }

  }

  #cat( "  delIndex: ", delIndex, "\n" )

  # find first line that contains TASK String FROM DELIVERABLE
  for( j in delIndex:length(projDocContents)) {

    if( grepl(TASK, projDocContents[j], fixed=TRUE) ) {

      taskIndex <- j

      break

    }

  }

  #cat( "  taskIndex: ", taskIndex, "\n" )

  # find first line that contains headerName String FROM TASK
  for( j in taskIndex:length(projDocContents)) {

    if( grepl(headerName, projDocContents[j], fixed=TRUE) ) {

      headIndex <- j

      break

    }

  }

  #cat( "  headIndex: ", headIndex, "\n" )

  # compute the NEXT LINE After the Header set of Links from headIndex:
  line <- computeNextLineHeader(headIndex, projDocContents)

  # return this index:
  line

}


#' Get Project Prefix From Path
#'
#' Extracts the Project Prefix from the projectPath - all characters before "~_"
#'
#'
getProjectPrefixFromPath <- function(projectPath) {
    getProjectPrefixFromName(basename(projectPath) )
}



#' Get Project Prefix From Name
#'
#' Extracts the Project Prefix from the projectName - all characters before "~_"
#'
#'
getProjectPrefixFromName <- function(projectName) {
  substr(projectName, 1, regexpr("~_", projectName)-1)
}



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
#'  list[[i]][[5]] : LIST containing all summary information in Project Note for this
#'                   Project Doc GOAL/DEL/TASK
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

  summaryVector <- c()

  # start indices at 1:
  linkIndex <- 1
  goaldeltaskIndex <- 1

  # look through projectNoteContents:
    # From OBJECTIVES up to line "------" - which marks the end of the Project Doclinks

  for( l in ( grep("# OBJECTIVES", projectNoteContents, fixed=TRUE) +1 ):
            ( grep("------", projectNoteContents) )  ) {

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

        # save summaryVector to linkList, and reset the summaryVector
        linkList[[linkIndex]][goaldeltaskIndex] <- list(summaryVector)
        summaryVector <- c()

        #increment linkIndex when divider BETWEEN ProjectDoc links is seen:
        linkIndex <- linkIndex + 1

        # and reset the goaldeltaskIndex
        goaldeltaskIndex <- 1
    }
    else if(grepl("[A-Za-z0-9]", projectNoteContents[l]) ) {
      # if the line contains letters and is not the Doc Link, or Goal/Del/Task, it is part of the
      # overview - so concat this into a character vector:
      summaryVector <- c(summaryVector, projectNoteContents[l])
    }

  }

  #return the linkList
  linkList

}



#' Space Summary Bullets
#'
#' Summary Bullets - acquired from getProjectNoteDocLinkList() (in returned linkList[[5]]) -
#' can be re-spaced for insertion into Rmd file.  2 blank lines between primary bullet ("*"), 1
#' blank line between all other lines, and ends with three blank lines.
#'
#'
spaceSummaryBulletPoints <- function(summaryBullets) {

  summaryBullets2 <- c()

  for( i in 1:length(summaryBullets) ) {

    if( startsWith(summaryBullets[i], "*") ) {
      summaryBullets2 <- c(summaryBullets2, "")
      summaryBullets2 <- c(summaryBullets2, "")
      summaryBullets2 <- c(summaryBullets2, summaryBullets[i])
    }
    else {
      summaryBullets2 <- c(summaryBullets2, "")
      summaryBullets2 <- c(summaryBullets2, summaryBullets[i])
    }

  }

  # add three blank lines at end:
  summaryBullets2 <- c(summaryBullets2, "")
  summaryBullets2 <- c(summaryBullets2, "")
  summaryBullets2 <- c(summaryBullets2, "")

  summaryBullets2

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

    if( substring(projDocContents[l], first=1, last=2) =="##" ||
        substring(projDocContents[l], first=1, last=3) =="###" ||
        substring(projDocContents[l], first=1, last=3) =="---" ||
        substring(projDocContents[l], first=1, last=3) =="**[" ) {

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
  # if a SUBNOTE is selected, the line will contain the string "*["
  if( grepl("-00~", lineContent, fixed = TRUE) || grepl("*[", lineContent, fixed = TRUE) ) {

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
  # if a SUBNOTE is selected, the line will contain the string "*["
  if( grepl("-00~", lineContent, fixed=TRUE) || grepl("*[", lineContent, fixed=TRUE) ) {

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


