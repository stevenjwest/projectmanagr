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
#' The filled Todo-Extraction-Template.Rmd is saved to the todo directory,
#' defined in settings.  The TODO extraction is saved using a naming convention
#' of the date & scope (location directory name or project-note title) :
#' `YYYMMDD_SCOPE.Rmd`
#'
#' @param location A Project Note file or Directory inside an Organisation
#' where TODO lists should be extracted from each GDT.  Can be Organisation
#' root, a programme, any sub-directory that contains project notes, or a
#' single project note.
#'
#' @export
extract_todos <- function(location, fileNamingConvention="YYYYMMDD_SCOPE",
                          todoGDTExtractionTemplate="Todo-GDT-Extraction-Template.Rmd",
                          todoExtractionTemplate="Todo-Extraction-Template.Rmd",
                          todoProgCollectionTemplate="Todo-Programme-Collection-Template.Rmd",
                          todoCollectionTemplate="Todo-Collection-Template.Rmd") {

  cat( "\nprojectmanagr::extract_todos():\n" )


  #### Set Instance Variables & Error Checking ####

  # get orgPath
  orgPath <- find_org_directory(location)

  if(orgPath == "" ) { # only if orgPath not identified
    stop( paste0("  Cannot identify organisation directory in location: ", location) )
  }
  # now, orgPath should be the root dir of the organisation

  # set confPath + tempPath - these names are FIXED:
  confPath <- paste0( orgPath, .Platform$file.sep, "config" )
  tempPath <- paste0( confPath, .Platform$file.sep, "templates" )

  # load settings file for user defined settings
  settingsFile <- paste( confPath, .Platform$file.sep, "settings.yml", sep="" )
  settings <- yaml::yaml.load( yaml::read_yaml( settingsFile ) )

  # define todoCollectionFileName with fileNamingConvention
  fileNamingConvention <- sub("YYYYMMDD", get_date(split=""), fileNamingConvention )
  scope <- basename(location)
  if( substr(scope, nchar(scope)-3, nchar(scope)-3) == '.' ) {
    scope <- substr(scope, 1, nchar(scope)-4) # trim file extension
  } else if(scope == settings[["ProgrammeProjectsDir"]]) {
    # if location is ProgrammeProjectsDir give the programme name too!
    scope <- paste0( basename(dirname(location)), "-", scope)
  }
  fileNamingConvention <- sub("SCOPE", scope, fileNamingConvention )

  # define todoCollectionPath - where to save todo collection
  todoCollectionDir <- paste0(orgPath, .Platform$file.sep, settings[["TodoCollectionDir"]])
  if(dir.exists(todoCollectionDir) == FALSE) {
    dir.create(todoCollectionDir)
  }
  todoCollectionPath <- paste0(todoCollectionDir, .Platform$file.sep, fileNamingConvention,
                               ".", settings[["FileTypeSuffix"]])

  # get all project notes that exist within location
  projectNotePaths <- check_location_project_notes(location, settings)

  # separate project notes vector by programme
  projectNoteProgPaths <- split_project_notes_by_programme(projectNotePaths, settings)


  #### Read todo Template Rmds ####

  todoGDTExtractionContents <- read_file( paste0( tempPath, .Platform$file.sep, todoGDTExtractionTemplate) )
  todoExtractionContents <- read_file( paste0( tempPath, .Platform$file.sep, todoExtractionTemplate) )
  todoProgCollectionContents <- read_file( paste0( tempPath, .Platform$file.sep, todoProgCollectionTemplate) )
  todoCollectionContents <- read_file( paste0( tempPath, .Platform$file.sep, todoCollectionTemplate) )

  # replace programme sep
  todoProgCollectionContents <- sub_template_param(todoProgCollectionContents, "{{TODO_PROGRAMME_SEP}}",
                                                   settings[["TodoProgrammeSep"]], orgPath)


  #### Extract each project note TODO section  ####

  cat( "\n\n  Extracting Project Note TODOs:\n" )

  for(p in projectNoteProgPaths) {
    # for each programme

    # put programme basename as header
    progCollection <- sub_template_param(todoProgCollectionContents, "{{PROGRAMME_HEADER}}",
                                          paste0("# ", basename(p[1])), orgPath)

    # extract each project note todos to extraction template
    for(pn in p[2:length(p)]) {

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
                             "G", get_goal_number(dGDT[["goal"]], settings),
                             " ", get_goal_title(dGDT[["goal"]], settings),
                             " :  ",
                             "D", get_deliverable_number(dGDT[["deliverable"]], settings),
                             " ", get_deliverable_title(dGDT[["deliverable"]], settings),
                             " :  ",
                             "T", get_task_number(dGDT[["task"]], settings),
                             " ", get_task_title(dGDT[["task"]], settings))

          todoGDTExtract <- sub_template_param(todoGDTExtractionContents, "{{GDT_SUMMARY}}",
                                               gdt_summ, orgPath)

          todoGDTExtract <- sub_template_param(todoGDTExtract, "{{GDT_TODO_LIST}}",
                                               todoList, orgPath)

          # APPEND to todoExtract
          gdtTodoSummLine <- grep_line_index("{{GDT_TODO_SUMMARY}}", todoExtract)
          todoExtract <- insert_at_indices(todoExtract,
                                                      gdtTodoSummLine, todoGDTExtract)
        }


      }

      # append only if incomplete TODOs were found
      if( incompleteTodoFound == TRUE) {
        # REMOVE GDT_TODO_SUMMARY marker - set to blank
        todoExtract <- sub_template_param(todoExtract, "{{GDT_TODO_SUMMARY}}",
                                             "", orgPath)

        # APPEND to progCollection
        pnTodoSummLine <- grep_line_index("{{PROJECT_NOTE_TODO_SUMMARY}}", progCollection)
        progCollection <- insert_at_indices(progCollection, pnTodoSummLine, todoExtract)
      }

    }

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


  #### write todo collection ####

  write_file(todoCollectionContents, todoCollectionPath)

  cat( "  written todo collection to disk: ", todoCollectionPath, "\n" )

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
      projectNotePaths <- normalizePath(location)

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

  progs <- unique(find_prog_dir(projectNotePaths, settings))

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


