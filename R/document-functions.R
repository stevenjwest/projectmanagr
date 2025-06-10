

#' Get Project Note Paths from Doc GDT links
#'
#' Get project note paths from doc GDT links  Assumes `projectDocContents` is a
#' character vector from a project doc.
#'
get_doc_gdt_project_note_paths <- function(projectDocPath, projectDocContents, settings) {

  projectDocParent <- dirname( fs::path_expand(projectDocPath))
  orgPath <- find_org_directory(projectDocParent)


  #### split projectDocContents into each TASK ####

  # indices of each task HEADER
  taskHeaderIndices <- grep( trimws(settings[["ProjectTaskHeader"]]), trimws(projectDocContents), fixed=TRUE)

  # index of each task LOG HEADER from each task HEADER
  taskLogIndices <- c()
  fi <- 1
  for(head in taskHeaderIndices) {
    taskLogIndices[fi] <- grep_line_index_from(load_param_vector(settings[["ProjectTaskLogHeader"]], orgPath),
                                                  projectDocContents, head, orgPath)
    fi <- fi+1
  }

  # index of each task FOOTER from each task HEADER
  taskFooterIndices <- c()
  fi <- 1
  for(head in taskHeaderIndices) {
    taskFooterIndices[fi] <- grep_line_index_from(load_param_vector(settings[["ProjectTaskFooter"]], orgPath),
                                                  projectDocContents, head, orgPath)
    fi <- fi+1
  }


  #### get each project note absolute path linked to GDTs ####

  # loop through index of taskLogIndices
  for(i in 1:length(taskLogIndices) ) {

    taskContents <- projectDocContents[ (taskLogIndices[i]):(taskFooterIndices[i]) ]

    # find lines that start with note link format && contain a link
    pnLines <- taskContents[startsWith(taskContents, paste0(settings[["NoteLinkFormat"]],"[") ) & grepl("](", taskContents, fixed=TRUE)]
    snLines <- taskContents[startsWith(taskContents, paste0(settings[["SubNoteLinkFormat"]],"[") ) & grepl("](", taskContents, fixed=TRUE)]
    lines <- c(pnLines, snLines)

    # extract the absolute path from each link
    relLinks <- substr(lines, regexpr("](", lines, fixed=TRUE)+2, regexpr(")", lines, fixed=TRUE)-1 )
    absPaths <- fs::path_abs( fs::path(projectDocParent, relLinks))

  }

  # return
  absPaths
}


#' Get Project Note Paths from specified Doc GDT link
#'
#' Get project note paths from the doc GDT link at `taskLine`. Assumes
#' `projectDocContents` is a character vector from `projectDocPath`.
#'
get_doc_GDT_link_note_paths <- function(projectDocPath, projectDocContents,
                                        taskLine, settings) {

  projectDocParent <- dirname( fs::path_expand(projectDocPath))
  orgPath <- find_org_directory(projectDocParent)

  # indices of each task HEADER
  taskHeaderIndex <- taskLine

  # index of each task LOG HEADER from each task HEADER
  taskLogIndex <- grep_line_index_from(load_param_vector(
                                          settings[["ProjectTaskLogHeader"]],
                                          orgPath),
                                       projectDocContents, taskHeaderIndex, orgPath)
  # index of each task FOOTER from each task HEADER
  taskFooterIndex <- grep_line_index_from(load_param_vector(
                                            settings[["ProjectTaskFooter"]], orgPath),
                                          projectDocContents, taskLogIndex, orgPath)


  #### get each project note absolute path linked to GDT ####
  taskContents <- projectDocContents[ taskLogIndex:taskFooterIndex ]

  # find lines that start with note link format && contain a link
  pnLines <- taskContents[startsWith(taskContents, paste0(settings[["NoteLinkFormat"]],"[") ) & grepl("](", taskContents, fixed=TRUE)]
  snLines <- taskContents[startsWith(taskContents, paste0(settings[["SubNoteLinkFormat"]],"[") ) & grepl("](", taskContents, fixed=TRUE)]
  lines <- c(pnLines, snLines)

  # extract the absolute path from each link
  # get relative path from the hyperlink
  relLinks <- substr(lines, regexpr("](", lines, fixed=TRUE)+2, regexpr(")", lines, fixed=TRUE)-1 )
  # get absolute paths by combining relLinks with projectDocParent path
  absPaths <- fs::path_abs( fs::path(projectDocParent, relLinks))

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
extract_note_obj_doc_link_GDT_summ <- function(linkNoteRmdContents,
                                               linkNoteRmdPath,
                                               settings, orgPath) {

  # linkNoteRmdContents contains all ProjDoc GDT links to add to new subnote

  type <- get_file_type(linkNoteRmdPath, settings)

  # isolate the objectives segment of contents
  linkObjHeadIndex <- match_line_index( load_param_vector(settings[["NoteObjectivesHeader"]], orgPath),
                                      linkNoteRmdContents)+1
  linkObjFootIndex <- grep_line_index_from( load_param_vector(settings[["NoteObjectivesFooter"]], orgPath),
                                            linkNoteRmdContents, linkObjHeadIndex, orgPath)-1

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
    pdli <- grep_line_index(paste0(settings[["ProjectLinkFormat"]], "["), o, orgPath) # projDoc link index
    gli <- grep_line_index_from(settings[["NoteGoalLinkLine"]], o, pdli, orgPath) # goal index
    dli <- grep_line_index_from(settings[["NoteDeliverableLinkLine"]], o, gli, orgPath) # del index
    tli <- grep_line_index_from(settings[["NoteTaskLinkLine"]], o, dli, orgPath) # task index
    if(type != "HEAD") { # summary and todo sections only exist for NON HEAD notes
      # removed NoteObjectivesSummarySectionHeader - using tli as start of summary
      #summi <- grep_line_index_from(settings[["NoteObjectivesSummarySectionHeader"]], o, tli) # summary index
      summi <- tli
      todoi <- grep_line_index_from(load_param_vector(settings[["NoteObjectivesTodoSectionHeader"]], orgPath),
                                    o, summi, orgPath) # todo index
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
      # no longer have a summary head - plus 1 from summi as Task Line is length 1
      #summLen <- length(settings[["NoteObjectivesSummarySectionHeader"]])
      todoLen <- length(settings[["NoteObjectivesTodoSectionHeader"]])
      summary <- o[(summi+1):(todoi-1)]
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
    pdli <- grep_line_index(paste0(settings[["ProjectLinkFormat"]], "["), o, orgPath) # projDoc link index
    gli <- grep_line_index_from(settings[["NoteGoalLinkLine"]], o, pdli, orgPath) # goal index
    dli <- grep_line_index_from(settings[["NoteDeliverableLinkLine"]], o, gli, orgPath) # del index
    tli <- grep_line_index_from(settings[["NoteTaskLinkLine"]], o, dli, orgPath) # task index
    # removed NoteObjectivesSummarySectionHeader - using tli as start of summary
    #summi <- grep_line_index_from(settings[["NoteObjectivesSummarySectionHeader"]], o, tli) # summary index
    summi <- tli
    todoi <- grep_line_index_from(load_param_vector(settings[["NoteObjectivesTodoSectionHeader"]], orgPath),
                                  o, summi, orgPath) # todo index

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

} #### ________________________________ ####


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
grep_line_index <- function(line, contents, orgPath, initialIndex=1) {

  if(is.character(line) &&
     length(line) == 1 &&
     startsWith(line, "::template::")) {

    # # check orgPath
    # orgPath <- find_org_directory(orgPath)

    # get templates
    tempPath <- get_template_dir(orgPath)
    # confPath <- get_config_dir(orgPath)
    # settings <- get_settings_yml(orgPath)

    # line is a POINTER to a multi-line content file in templates dir
    # so open this and insert the multi-lined vector into templateContents
    paramPointer <- substr(line, nchar("::template::")+1, nchar(line))
    line <- read_file( paste0( tempPath, .Platform$file.sep, paramPointer) )
    matches <- match_vector(line, contents)
    returnVal <- matches[1] + (initialIndex - 1)

    #templateContents <- replace_params_with_vector(templateContents, templateParam, paramContents)

  } else if( length(line) > 1 ) { # line is a multi-lined vector

    # so want to check for matches of these lines in contents
    # using match_vector_sub - to ensure the vector found in subsets of content elements are also returned!
    matches <- match_vector_sub(line, contents)
    returnVal <- matches[1] + (initialIndex - 1)

  } else {

    # line is a vector of length 1
    # so grepl and return first value
    matches <- grep(line, contents, fixed=TRUE)
    returnVal <- matches[1] + (initialIndex - 1)
    #insert appropriately with gsub()
    #templateContents <- gsub(templateParam, paramContents, templateContents, fixed=TRUE)

  }

  # #### grep line in contents FIXED ####
  # returnVal <- -1
  # # look through contents to find an index that matches line:
  # for( l in 1:length(contents) ) {
  #
  #   if( grepl(line[1], contents[l], , fixed=TRUE) ) {
  #     returnVal <- l
  #     break
  #   }
  #
  # }

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
grep_line_index_from <- function(line, contents, initialIndex, orgPath) {

  # line <- load_param_vector(settings[["NoteObjectivesTodoSectionHeader"]], orgPath)
  # contents <- o
  # initialIndex <- summi
  grep_line_index(line, contents[initialIndex:length(contents)], orgPath, initialIndex=initialIndex)
  # #### grep line in contents from initialIndex FIXED ####
  #
  # returnVal <- -1
  # # look through contents to find an index that matches line:
  # for( l in initialIndex:length(contents) ) {
  #
  #   if( grepl(line[1], contents[l], fixed=TRUE) ) {
  #     returnVal <- l
  #     break
  #   }
  #
  # }
  #
  # returnVal

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

  if( startsWith(header, '#' ) ) {

    # identify when the # header stops
    hspl <- strsplit(header, split='')[[1]]
    endH <- 1
    for( lt in hspl ) {
      if( lt == '#' ) {
        endH <- endH + 1
      } else {
        break
      }
    }

    # now reform the string from hspl
    hV <- paste0(hspl[endH:length(hspl)], collapse='')
  } else {
    hV <- header
  }

  hV # return

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

  # get each index in parent that contains the first element in vector
  sieved <- which(parent == vector[1L])

  # for each additional element in vector, identify matches in parent at subsequent indices
  for (i in seq.int(1L, length(vector) - 1L)) {
    # filter the original indices with boolean
    # whether parent elements continue to equal vector elements in subsequent indices
    sieved <- sieved[parent[sieved + i] == vector[i + 1L]]
  }

  # remove all NA values - these appear when indices from vector exceed indices in parent
   # e. when the last element in parent matches first element in vector, and further retireval
   # of elements in parent beyond its length result in NA being returned
  sieved[!is.na(sieved)]
}


#' Match vector is subset in parent
#'
#' Identifies each initial index where each consecutive element of `vector` is
#' present in a consecutive set of elements in `parent` - where all elements in
#' `vector` match parts of elements in `parent` in order.
#'
match_vector_sub <- function(vector, parent, nomatch = 0L) {

  #### match vector in parent ####

  # get each index in parent that contains the first element in vector
  #sieved <- which(parent == vector[1L])
  sieved <- which( grepl(vector[1L], parent, fixed=TRUE))

  # for each additional element in vector, identify matches in parent at subsequent indices
  for (i in seq.int(1L, length(vector) - 1L)) {
    #print(i)
    # filter the original indices with boolean
    # whether parent elements continue to equal vector elements in subsequent indices
    #sieved <- sieved[ parent[sieved + i] == vector[i + 1L]]
    sieved <- sieved[ grepl(vector[i + 1L], parent[sieved + i], fixed = TRUE) ]
  }

  # remove all NA values - these appear when indices from vector exceed indices in parent
  # e. when the last element in parent matches first element in vector, and further retireval
  # of elements in parent beyond its length result in NA being returned
  sieved[!is.na(sieved)]

  }


#' returns true is string is identified anywhere in vector.  String must be a
#' character vecgor of length 1.
str_in_vec <- function(string, vector) {
  length( grep(string, vector, fixed=TRUE) ) > 0
} #### ________________________________ ####


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
cursor_selection <- function(
    save_context_fn =   .save_context_doc,
    get_context_fn = .get_source_editor_context
  ) {


  #### get source editor selection ####

  save_context_fn() # save current file first

  context <- get_context_fn()


  #### get path contents & selected line ####

  filePath <- fs::path_expand(context$path)
  contents <- context$contents

  cursor <- context$selection[[1]]
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
#' projectDocPath - must be an ABSOLUTE PATH if using the output for
#' addProjectNote() or similar methods.
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
#' `[[1]]` or `[["task"]]` - contains the line that starts "#### TASK" in the Active Document.
#'
#' `[[2]]` or `[["taskLine"]]` - the task line number (index) in the Active Document.
#'
#' `[[3]]` or `[["deliverable"]]` - contains the line that starts "### DELIVERABLE" in the Active Document.
#'
#' `[[4]]` or `[["deliverableLine"]]` - the deliverable line number (index) in the Active Document.
#'
#' `[[5]]` or `[["goal"]]` - contains the line that starts "## GOAL" in the Active Document.
#'
#' `[[6]]` or `[["goalLine"]]` - the goal line number (index) in the Active Document.
#'
#' `[[7]]` or `[["originalLine"]]` - the original line where the Active Cursor was on in the Active Document.
#'
#' `[[8]]` or `[["originalLineNumber"]]` - the original line number where the Active Cursor was on in the Active Document.
#'
#' `[[9]]` or `[["addingSubNote"]]` - a Boolean value indicating the Cursor was on a GROUP (HEADER NOTE or SUBNOTE).
#'
#' `[[10]]` or `[["headerNoteLink"]]` - if addingSubNote is TRUE, contains the link to the headerSubNote, AS IS GIVEN
#' IN THE PROJECT DO (i.e. the link TITLE and then the link ADDRESS - for example
#' `**[LAB~001-00~ GARS SC Clearing Labelling](../LAB/LAB~001-00~_GARS_SC_Clearing_Labelling.Rmd)**`), else is BLANK
#'
#' `[[11]]` or `[["headerNoteLineNumber"]]` - if addingSubNote is TRUE, contains the line that contains the headerSubNoteLink, else is BLANK.
#'
#' `[[12]]` or `[["projectDocPath"]]` - the projectDocPath
#'
#'
#' If this method is unsuccessful (there is an error), it returns a LIST that contains:
#'
#' `[[1]]` - contains the String "FALSE"
#' `[[2]]` - contains the errorMessage - a String indicating why the method failed.
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

    # get indices of all GOAL DEL TASK strings
    goalIndices <- which( startsWith(contents, settings[["ProjectGoalHeader"]]) )
    delIndices <- which( startsWith(contents, settings[["ProjectDeliverableHeader"]]) )
    taskIndices <- which( startsWith(contents, settings[["ProjectTaskHeader"]]) )

    # crop project doc content to include content up to selection line
    #contentsCrop <- contents[1:line]

    # identify line indices that starts with TASK DEL GOAL Strings
    #taskIndices <- which( startsWith(contentsCrop, settings[["ProjectTaskHeader"]]) )
    #delIndices <- which( startsWith(contentsCrop, settings[["ProjectDeliverableHeader"]]) )
    #goalIndices <- which( startsWith(contentsCrop, settings[["ProjectGoalHeader"]]) )

    if(length(taskIndices) == 0 | length(delIndices) == 0 | length(goalIndices) == 0) {

      # if task/del/goal not found, set errorMessage:
      taskRetrieved <- FALSE
      errorMessage <- "Could Not Locate TASK/DEL/GOAL"

    } else {

      # compute the GDT selected

      # first determine the GDT indices that is LESS THAN OR EQUAL to line
      goalIndicesCrop <- goalIndices[goalIndices <= line]
      delIndicesCrop <- delIndices[delIndices <= line]
      taskIndicesCrop <- taskIndices[taskIndices <= line]

      # if no goal is found before line, return an error
       # dealing with del and task below
      if(length(goalIndicesCrop) == 0) {

        # if task/del/goal not found, set errorMessage:
        taskRetrieved <- FALSE
        errorMessage <- "Could Not Locate TASK/DEL/GOAL"

      } else {

        # need to check the values of the last indices to determine what lines to return

        # get initial GDT lines - LAST line numbers in the cropped indices
        goalLine <- goalIndicesCrop[length(goalIndicesCrop)]

        # deal with edge case where FIRST GOAL selected - so no indices for del or task
        if( length(delIndicesCrop) == 0 ) {
          delLine <- 0 # set to int 0
        } else {
          delLine <- delIndicesCrop[length(delIndicesCrop)]
        }

        if( length(taskIndicesCrop) == 0 ) {
          taskIndicesCropAdj <- 0 # set to int 0
        } else {
          taskLine <- taskIndicesCrop[length(taskIndicesCrop)]
        }

        # if goal < del < task - selected on or below a full GDT
        if( (goalLine < delLine) & (delLine < taskLine) ) {
          # so can return these lines plus contents
          # DO NOTHING HERE
        } else if( (goalLine < delLine) ) { # then line is above the correct Del
          # need to correct taskLine only
          # easiest way to do this is to select the next task after line
          # can retrieve this by selecting the length(taskIndicesCrop) + 1 value
          # from taskIndices
          taskLine <- taskIndices[length(taskIndicesCrop) + 1]
        } else { # line is above BOTH correct Del and Task
          # retrieve correct delLine & taskLine
          delLine <- delIndices[length(delIndicesCrop) + 1]
          taskLine <- taskIndices[length(taskIndicesCrop) + 1]
        } # also handles cases where selection made on FIRST GOAL and FIRST GOAL/DEL

        # get string for task del goal
        task <- contents[taskLine]
        deliverable <- contents[delLine]
        goal <- contents[goalLine]

      }
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

} #### ________________________________ ####



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
note_link_todo_params <- function(projNoteLinkSummaryContents, settings, orgPath) {

  # add TODO header
  noteLinkSummContents <- sub_template_param(projNoteLinkSummaryContents,
                                             "{{OBJECTIVES_TODO_HEADER}}",
                                             settings[["NoteObjectivesTodoSectionHeader"]],
                                             orgPath)

  # replace todoContents
  todo <- sub_template_param(todoContents, "{{TODO_HEADER_TEMPLATE}}",
                                     settings[["TodoHeaderTemplate"]], orgPath)

  todo <- sub_template_param(todo, "{{TODO_ITEM_TEMPLATE}}",
                             settings[["TodoItemTemplate"]], orgPath)

  noteLinkSummContents <- sub_template_param(noteLinkSummContents, "{{TODO_TEMPLATE}}",
                                         todo, orgPath)

  noteLinkSummContents # return

}

#' Substitute Note Link in Contents
#'
sub_note_link_params <- function(noteLinkContents, settings, DocGDTList,
                                 projNoteLinkSummaryContents, orgPath) {

  #### sub link in contents ####
  noteLinkContents <- sub_template_param(
                          noteLinkContents, "{{OBJECTIVES_SEP}}",
                          settings[["NoteObjectivesSep"]], orgPath)
  noteLinkContents <- sub_template_param(
                          noteLinkContents, "{{PROJECT_DOC_TITLE}}",
                          DocGDTList$title, orgPath)
  noteLinkContents <- sub_template_param(
                          noteLinkContents, "{{PROJECT_DOC_LINK}}",
                          DocGDTList$link, orgPath)
  noteLinkContents <- sub_template_param(
                          noteLinkContents, "{{PROJECT_DOC_LINK_GOAL}}",
                          DocGDTList$goal, orgPath)
  noteLinkContents <- sub_template_param(
                          noteLinkContents, "{{PROJECT_DOC_LINK_DEL}}",
                          DocGDTList$del, orgPath)
  noteLinkContents <- sub_template_param(
                          noteLinkContents, "{{PROJECT_DOC_LINK_TASK}}",
                          DocGDTList$task, orgPath)

  # insert the summaryBullet into SUMMARY_INFO field:
  noteLinkContents <- sub_template_param(
                          noteLinkContents, "{{SUMMARY_INFO}}",
                          projNoteLinkSummaryContents, orgPath)

  # return
  noteLinkContents

}





get_project_prefix_from_path <- function(projectPath, settings) {
  get_project_prefix_from_name(basename(projectPath), settings )
}


#' Get Project Prefix From Name
#'
#' Extracts the Project Prefix from the projectName - all characters before
#' `settings[["ProjectPrefixSep"]]`
#'
get_project_prefix_from_name <- function(projectFileName, settings) {
  substr(projectFileName, 1, regexpr(settings[["ProjectPrefixSep"]], projectFileName)-(1) )
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


#' Get Name from File Name
#'
#' fileName is PREFIX~_COMP_TITLE.Rmd - returned is COMP_TITLE
#'
#'
get_name_from_file_path <- function(projectFilePath, settings) {

  get_name_from_file_name( fs::path_file(projectFilePath), settings)
  #substring( basename(projectFilePath),
  #          first=regexpr(settings[["ProjectPrefixSep"]], projectFilePath, fixed=TRUE) + nchar(settings[["ProjectPrefixSep"]]),
  #          last=regexpr( paste0(".", settings[["FileTypeSuffix"]]), projectFilePath, fixed=TRUE)-1  )
}


get_prefix_name_from_file_name <- function(projectFileName, settings) {
  substring(projectFileName,
            first=1,
            last=regexpr( paste0(".", settings[["FileTypeSuffix"]]), projectFileName, fixed=TRUE)-1  )

}


get_prefix_name_from_file_path <- function(projectFilePath, settings) {
  get_prefix_name_from_file_name(fs::path_file(projectFilePath), settings)
}


#' Get File Contents Headers
#'
#' Returns every Markdown header line from the text file (all lines that begin
#' with \code{#}), while skipping over code blocks (anything between lines that
#' start with triple backticks).
#'
#' If it detects an odd number of triple-backtick lines, it warns you that
#' some fences may not be at the start of the line. This can happen if a code
#' fence is preceded by spaces.
#'
#' @param contents Character vector containing the contents of the file.
#'
#' @return Character vector of every header line (lines starting with \code{#})
#'   after code blocks have been removed.
#'
#' @examples
#' \dontrun{
#'   txt <- readLines("somefile.Rmd")
#'   get_file_contents_headers(txt)
#' }
get_file_contents_headers <- function(contents) {

  # Find all lines that start exactly with triple backticks
  contentCodeIndices <- which(grepl("^```", contents))

  # If an odd number of them is found, warn the user
  if (length(contentCodeIndices) %% 2 != 0) {
    warning(
      "Odd number of code-fence lines detected. Possibly a code fence does not ",
      "start in column 1. Search your Rmd for ' ```' (a space followed by three ",
      "backticks) to find any mismatched fences."
    )
  }

  # remove code blocks for every *complete pair* of backticks
  if (length(contentCodeIndices) >= 2) {
    # number of complete pairs
    num_pairs <- floor(length(contentCodeIndices) / 2)
    # remove from last pair down to first
    for (i in seq(num_pairs, 1, by = -1)) {
      start_line <- contentCodeIndices[2 * i - 1]
      end_line   <- contentCodeIndices[2 * i]
      # slice out the code-block portion
      contents <- c(
        contents[1:(start_line - 1)],
        contents[(end_line + 1):length(contents)]
      )
    }
  }

  # keep only lines that begin with '#'
  contentHeaders <- contents[grepl('^#{1,}', contents)]
  contentHeaders
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
Oget_file_contents_headers <- function(contents) {

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
  journalPath <- get_journal_dir(orgPath, settings)

  # get status yml
  #status <- get_status_yml(orgPath, settings)

  # read status information for CONTENTS if it exists
  #contentsStatus <- status[['CONTENTS']]

  contentSepContents <- load_param_vector(settings[["ContentSep"]], orgPath)
  # as this is called in a for loop, better to open this file external to that loop and pass as param


  #### loop through filePaths ####

  # get all project notes in orgPath RECURSIVELY
  filePaths <- get_file_list_to_project_notes(
                  orgPath, settings,
                  pathExclusions = c(confPath, volPath, sitePath, journalPath) )
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
  dt <- lubridate::ymd_hm(dtStr) # convert to datetime in UTC timezone

  #contents <- list() # to store all retrieved contents metadata in

  # get config templates settings yml
  confPath <- get_config_dir(orgPath)
  tempPath <- get_template_dir(orgPath)
  settings <- get_settings_yml(orgPath)

  # get dirs in root to EXCLUDE from search
  volPath <- get_volumes_dir(orgPath, settings)
  sitePath <- get_site_dir(orgPath, settings)
  journalPath <- get_journal_dir(orgPath, settings)

  # get status yml
  #status <- get_status_yml(orgPath, settings)

  # read status information for CONTENTS if it exists
  #contentsStatus <- status[['CONTENTS']]

  contentSepContents <- load_param_vector(settings[["ContentSep"]], orgPath)
  # as this is called in a for loop, better to open this file external to that loop and pass as param


  #### loop through filePaths ####

  # get all project notes in orgPath RECURSIVELY
  paths <- get_file_list_to_project_notes(
    dirs = orgPath,
    settings = settings,
    pathExclusions = c(confPath, volPath, sitePath, journalPath),
    retrievalDateTimeCutoff = dt )
  # EXCLUDING confPath volPath and sitePath from search!!
  # AND applying the cutoff datetime from cache!

  # remove any dirs from retrieved paths
  filePaths <- paths[fs::is_file(paths)]

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
  status[['CONTENTS']] <- list() # reset contents - remove any old cache!
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
  filePaths <- get_file_list_to_project_notes(dirTree, settings)
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
  #get_index_prog()
  orgIndex <- get_index_org(orgPath, settings)
  orgFiles <- list.files(orgPath)
  orgIndex <- orgFiles[ startsWith(orgFiles, settings[["OrgIndexFileNamePrefix"]])]

  # PROG index
  progIndexes <- get_prog_index_files(orgPath, settings)

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

    if( grepl("[A-z,0-9,=,+,-,_]", contents[l]) ) { # returns TRUE if line CONTAINS any letter or number

      returnVal <- (l+1) # return the NEXT LINE after the line which contains content
      break

    }

  }

  returnVal
}





