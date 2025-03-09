

#' Extract All TODO lists from a Collection of Project Notes
#'
#' Extracted to Todo-Extraction-Template.Rmd that provides a template to
#' extract an absolute filesystem link to the project note, summarise each
#' Doc GDT with link, and copy all incompleted TODOs under this Doc GDT.
#'
#' Todo-Collection-Template.Rmd that provides a template for organising multiple
#' Project Notes, listing notes by PROGRAMME in organisation directory tree.
#'
#' incompleted TODOs can then be reviewed, and prioritised by re-ordering &
#' deleting the project note / doc GDT / TODOs.
#'
#' The filled Todo-Extraction-Template.Rmd is returned as character vector.
#'
#' @param location A path to a Directory or Project Note file inside an
#' Organisation where TODO lists should be extracted from each GDT.  Can be
#' Organisation root, a programme, any sub-directory that contains project
#' notes, or a single project note.
#'
#' @param date The date of extraction, default is today's date. Should be in
#' char format as 'YYYY-MM-DD' - added to header of Todo Collection.
#'
#' @param todoGDTExtractionTemplate Rmd template for todo GDT extraction
#'
#' @param todoExtractionTemplate Rmd template for todo Extraction
#'
#' @param todoProgCollectionTemplate Rmd template for todo Prog Collection
#'
#' @param todoCollectionTemplate Rmd template for todo Collection
#'
#' @return Character Vector of Todo Collection.
#'
#' @export
extract_todos <- function(location, date=get_date(split='-'),
                          todoGDTExtractionTemplate="Todo-GDT-Extraction-Template.Rmd",
                          todoExtractionTemplate="Todo-Extraction-Template.Rmd",
                          todoProgCollectionTemplate="Todo-Programme-Collection-Template.Rmd",
                          todoCollectionTemplate="Todo-Collection-Template.Rmd" ) {

  cat( "\nprojectmanagr::extract_todos():\n" )


  #### Set Instance Variables & Error Checking ####

  # get orgPath
  orgPath <- find_org_directory(location)

  if(orgPath == "" ) { # only if orgPath not identified
    stop( paste0("  Cannot identify organisation directory in location: ", location) )
  }
  # now, orgPath should be the root dir of the organisation

  # get config templates settings yml
  confPath <- get_config_dir(orgPath)
  tempPath <- get_template_dir(orgPath)
  settings <- get_settings_yml(orgPath)

  # define todoCollectionFileName with fileNamingConvention
  #fileNamingConvention <- sub("YYYYMMDD", get_date(split=""), fileNamingConvention )

  # define scope - location directory name or project-note title
  scope <- basename(location)
  if( substr(scope, nchar(scope)-3, nchar(scope)-3) == '.' ) {
    scope <- substr(scope, 1, nchar(scope)-4) # trim file extension
  }

  #fileNamingConvention <- sub("SCOPE", scope, fileNamingConvention )

  # define todoCollectionPath - where to save todo collection
  #todoCollectionDir <- paste0(orgPath, .Platform$file.sep, settings[["TodoCollectionDir"]])
  #if(dir.exists(todoCollectionDir) == FALSE) {
  #  dir.create(todoCollectionDir)
  #}
  #todoCollectionPath <- paste0(todoCollectionDir, .Platform$file.sep, fileNamingConvention,
  #                             ".", settings[["FileTypeSuffix"]])

  # get all project notes that exist within location
  projectNotePaths <- check_location_project_notes(location, settings)

  # separate project notes vector by programme
  projectNoteProgPaths <- split_project_notes_by_programme(projectNotePaths, settings)


  #### Read todo Template Rmds ####

  todoGDTExtractionContents <- read_file( fs::path( tempPath, todoGDTExtractionTemplate) )
  todoExtractionContents <- read_file( fs::path( tempPath, todoExtractionTemplate) )
  todoProgCollectionContents <- read_file( fs::path( tempPath, todoProgCollectionTemplate) )
  todoCollectionContents <- read_file( fs::path( tempPath, todoCollectionTemplate) )

  # replace programme sep
  todoProgCollectionContents <- sub_template_param(todoProgCollectionContents, "{{TODO_PROGRAMME_SEP}}",
                                                   settings[["TodoProgrammeSep"]], orgPath)


  #### Extract each project note TODO section  ####

  cat( "\n\n  Extracting Project Note TODOs:\n" )

  for(p in projectNoteProgPaths) {
    # for each programme

    # put programme basename as header - basename in first index of char vector
    progCollection <- sub_template_param(todoProgCollectionContents, "{{PROGRAMME_HEADER}}",
                                          paste0("# ", basename(p[1])), orgPath)

    cat( "\n    Programme:", basename(p[1]),"\n" )

    # extract each project note todos to extraction template
    for(pn in p[2:length(p)]) {

      cat( "\n      Project Note:", basename(pn) )

      # set flag to determine whether to add this note to collection
      incompleteTodoFound <- FALSE

      # open project note
      projectNoteContents <- read_file(pn)

      # extract each project Doc + GDT + summary & todo from each Note objective
      DocGDTsList <- extract_note_obj_doc_link_GDT_summ(projectNoteContents, pn,
                                                        settings, orgPath)

      if(is.list(DocGDTsList) & length(DocGDTsList) == 0) {
        # if a blank list - this note has no objective - skip this note
        next
      }

      # add projectNotePath Title (basename) into todoExtract
      todoExtract <- sub_template_param(todoExtractionContents, "{{PROJECT_NOTE_TITLE}}",
                                        paste0("## ", basename(pn)), orgPath)

      # form absolute link to project note
      pnl <- paste0("[", basename(pn), "](", pn, ")")

      # add projectNotePath absolute link into todoExtract
      todoExtract <- sub_template_param(todoExtract, "{{PROJECT_NOTE_LINK}}",
                                        pnl, orgPath)

      for(dGDT in DocGDTsList) {

        # filter TODO list for incompleted TODOs that are NOT the template TODO
        todoList <- filter_todos_incomplete(dGDT[["todo"]], settings)

        if( all( trimws(todoList) == "") ) { # if no TODOs exist do NOT add this to TODO extraction

        } else { # if TODOs do exist, ADD THEM!

          incompleteTodoFound <- TRUE # mark the fact Todos were found!

          # form GDT summary & incompleted TODOs vector
          gdt_summ <- paste0(#basename(dGDT[["projectDocFilePath"]]),
                             #" :  ",
                             "G: ", get_goal_title(dGDT[["goal"]], settings),
                             " -  ",
                             "D: ", get_deliverable_title(dGDT[["deliverable"]], settings),
                             " -  ",
                             "T: ", get_task_title(dGDT[["task"]], settings))

          todoGDTExtract <- sub_template_param(todoGDTExtractionContents, "{{GDT_SUMMARY}}",
                                               gdt_summ, orgPath)

          todoGDTExtract <- sub_template_param(todoGDTExtract, "{{GDT_TODO_LIST}}",
                                               todoList, orgPath)

          # APPEND to todoExtract
          gdtTodoSummLine <- grep_line_index("{{GDT_TODO_SUMMARY}}", todoExtract)
          todoExtract <- insert_at_indices(todoExtract, gdtTodoSummLine, todoGDTExtract)
        }


      } # dGDT

      # append only if incomplete TODOs were found
      if( incompleteTodoFound == TRUE ) {
        # REMOVE GDT_TODO_SUMMARY marker - set to blank
        todoExtract <- sub_template_param(todoExtract, "{{GDT_TODO_SUMMARY}}",
                                             "", orgPath)
        # APPEND to progCollection
        pnTodoSummLine <- grep_line_index("{{PROJECT_NOTE_TODO_SUMMARY}}", progCollection)
        progCollection <- insert_at_indices(progCollection, pnTodoSummLine, todoExtract)
      }

    } # pn

    # REMOVE PROJECT_NOTE_TODO_SUMMARY marker - set to blank
    progCollection <- sub_template_param(progCollection, "{{PROJECT_NOTE_TODO_SUMMARY}}",
                                      "", orgPath)

    # APPEND to todoCollectionContents
    todoSummLine <- grep_line_index("{{PROGRAMME_TODO_SUMMARY}}", todoCollectionContents)
    todoCollectionContents <- insert_at_indices(todoCollectionContents, todoSummLine, progCollection)

  }

  # REMOVE PROGRAMME_TODO_SUMMARY marker - set to blank
  todoCollectionContents <- sub_template_param(todoCollectionContents, "{{PROGRAMME_TODO_SUMMARY}}",
                                       "", orgPath)

  # ADD DATE
  todoCollectionContents <- sub_template_param(todoCollectionContents, "{{DATE}}",
                                               date, orgPath)

  # ADD SCOPE
  todoCollectionContents <- sub_template_param(todoCollectionContents, "{{SCOPE}}",
                                               scope, orgPath)


  #### return todo collection char vector ####

  cat("\n  return todo collection\n\n")
  return(todoCollectionContents)

  #write_file(todoCollectionContents, todoCollectionPath)

  #cat( "  written todo collection to disk: ", todoCollectionPath, "\n" )

}



check_location_project_notes <- function(location, settings) {

  if( dir.exists(location) ) {

    # location is a dir - get all project notes
    projectNotePaths <- get_project_note_paths(location, settings)

    # Check projectNotePaths is not blank
    if( length(projectNotePaths) == 0 ) {
      stop( paste0("  No Project Notes identified in location: ", location) )
    }

  } else if( file.exists(location) ) {

    # location is a file - check its a project note
    if( get_file_type(location, settings) == "HEAD" |
        get_file_type(location, settings) == "SUB" |
        get_file_type(location, settings) == "NOTE" ) {
      projectNotePaths <- fs::path_expand(location)

    } else if( get_file_type(location, settings) == "DOC" ) {
      # get all project note paths that link to this Docs GDTs
      projectNotePaths <- get_doc_gdt_project_note_paths( location, read_file(location), settings)

    } else {
      stop( paste0("  File is not a project note or doc: ", location) )
    }

  } else {

    # location doesnt exist
    stop( paste0("  location is not an existing file or directory: ", location) )

  }
  # return
  projectNotePaths
}



#' Split project notes vector by programme
#'
#' Returns a list where first element of each vector is the programme path, and
#' subsequent elements are project notes within that programme.
#'
#' Number of list elements is number of programmes, and number of elements in
#' vector is number of project notes in that programme -1.
split_project_notes_by_programme <- function(projectNotePaths, settings) {

  progs <- unique(find_prog_dir(projectNotePaths))

  # extract each projectNote into vector starting with prog path
  prog_proj_notes <- list()
  pi <- 1
  for(p in progs) {
    progsVector <- p
    pni <- 2
    for(pn in projectNotePaths) {
      if(startsWith(pn, p) ) {
        progsVector[pni] <- pn
        pni <- pni+1
      }
    }
    prog_proj_notes[[pi]] <- progsVector
    pi <- pi+1
  }

  prog_proj_notes

}


filter_todos_incomplete <- function(todoList, settings) {

  # filter out just Headers & Items
  todoListFiltered <- todoList[ startsWith(todoList, settings[["TodoHeader"]]) | startsWith(todoList, settings[["TodoItem"]]) ]

  # remove any items that are the TEMPLATE Todos
  todoListFiltered <- todoListFiltered[ !startsWith(todoListFiltered, settings[["TodoHeaderTemplate"]]) ]
  todoListFiltered <- todoListFiltered[ !startsWith(todoListFiltered, settings[["TodoItemTemplate"]]) ]

  # remove any items that are COMPLETE
  todoListFiltered <- todoListFiltered[ !startsWith(todoListFiltered, settings[["TodoItemComplete"]]) ]

  # interlace with blank strings for whitespace
  todoListFiltered <- as.vector(rbind(todoListFiltered, rep("", length(todoListFiltered))))

  # return todo list with template and completed todos removed
  todoListFiltered

}

OLDfilter_todos_incomplete <- function(todoList, settings) {

  todoListFiltered <- c()
  skip <- FALSE
  for(l in todoList) {

    # each line that starts with TODO
    if( startsWith(l, settings[["TodoItemHeaderTemplate"]]) ) {
      skip <- TRUE

    } else if( startsWith(l, settings[["TodoItemHeaderComplete"]]) ){
      skip <- TRUE

    } else if( startsWith(l, settings[["TodoItemHeader"]]) ) {
      skip <- FALSE
      if( startsWith(l, settings[["TodoItemHeaderTemplate"]]) ) {
        skip <- TRUE

      } else if( startsWith(l, settings[["TodoItemHeaderComplete"]]) ){
        skip <- TRUE
      }

      if( skip == FALSE ) {
        todoListFiltered <- c(todoListFiltered, l)
      }

    } else if( skip == FALSE ) {
      todoListFiltered <- c(todoListFiltered, l)
    }

  }

  # return todo list with template and completed todos removed
  todoListFiltered

}


