
#' Rename a ProjectManagr Component
#'
#' This Addin provides access to renaming any projectmanagr component,
#' including:
#'
#' * ProjectManagr Project Note filenames
#'
#' * ProjectManagr Project Doc filenames
#'
#' * Directories inside a ProjectManagr Organisation
#'
#' This Addin ensures any renaming will automatically update all ProjectManagr
#' components to this change, including hyperlinks, GDT links, and paths, as
#' needed:
#'
#' * ProjectManagr Project Note renaming will update all hyperlinks to the note
#' to use the new name.
#'
#' * ProjectManagr Project Doc renaming will update all hyperlinks & GDT links
#' to the Doc to use the new name.
#'
#' * Directory renaming will depend on the directory type:
#'     + If a user-defined directory containing project notes, all note prefixes
#'       (inherited from the directory name) will update, as well as any strings
#'       that contain /{{DIRNAME}}/?? To update all links that contain the dir?
#'
#'
addin_rename_projectmanagr_component <- function() {

}



#' Rename a Project File
#'
#' This Addin renames the currently active project file - Note or Doc - with a
#' new filename, and a new title.
#'
#' All links are updated throughout the organisation to the project file.
#'
#'@export
addin_rename <- function() {

  cat( "\nprojectmanagr::addin_rename():\n" )


  #### instance variables ####

  # Retrieve cursor_selection from current Active Doc in rstudio:
  selection <- cursor_selection() # Project Doc, Project Note - Simple, Header, SubNote

  if(selection[["rmdType"]]=="UNKNOWN") { # return error message from selection
    addin_error("Rename Project File", selection[["errorMessage"]])
    stop( paste0("  ", selection[["errorMessage"]]))
  }

  # get the orgPath:
  orgPath <- find_org_directory(selection$filePath)

  # if orgPath not identified present error interface and then stop this function
  if(orgPath=="") {
    addin_error_path("Rename Project File",
                     "No Organisation identified - ensure active document is in an Organisation.",
                     selection$filePath)
    stop( paste0("  No Organisation identified - ensure active document is in an Organisation: \n    ", selection$filePath))
  }

  # get config templates settings yml
  confPath <- get_config_dir(orgPath)
  tempPath <- get_template_dir(orgPath)
  settings <- get_settings_yml(orgPath)

  # get status yml
  status <- get_status_yml(orgPath, settings)


  #### Rename Project File Addins ####

  # check rmdType and generate correct addin interface
  if( selection$rmdType == "DOC" ) { # project doc active doc


    ##### Rename Project DOC GDT #####

    # if selection is on a GOAL DLIVERABLE or TASK line: rename the GOAL DLIVERABLE or TASK
    if( selection[['originalLine']] == selection[['goal']] ) {
      addin_rename_project_doc_goal(selection, settings, orgPath)

    } else if( selection[['originalLine']] == selection[['deliverable']] ) {
      addin_rename_project_doc_deliverable(selection, settings, orgPath)

    } else if( selection[['originalLine']] == selection[['task']] ) {
      addin_rename_project_doc_task(selection, settings, orgPath)

    } else if( startsWith(selection[['originalLine']], '#') ) {


      ##### Rename Project Doc HEADER #####

      addin_rename_project_file_header(selection, settings, orgPath)
    } else {


      ##### Rename Project DOC #####

      addin_rename_project_doc(selection, settings, orgPath)

    }


  } else if( selection$rmdType == "HEAD" ||
             selection$rmdType == "SUB" ||
             selection$rmdType == "NOTE") { # project note active doc


    if( startsWith(selection[['originalLine']], '#') ) {


      ##### Rename Project Note HEADER #####

      addin_rename_project_file_header(selection, settings, orgPath)

    } else {


      ##### Rename Project NOTE #####

      addin_rename_project_note(selection, settings, orgPath)

    }


  } else if( selection$rmdType == "UNKNOWN" ) {

    if( startsWith(selection[['originalLine']], '#') ) {


      ##### Rename Project File HEADER #####

      addin_rename_project_file_header(selection, settings, orgPath)

    } else {


    addin_error_path("Rename Project File",
                     "No Valid Rmd file selected - ensure a Project Doc or Project Note is the active document.",
                     selection$filePath, settings)
    stop( paste0("  No Valid Rmd file selected - ensure a Project Doc or Project Note is the active document: \n    ", selection$filePath))

    }

  }

}


#' Rename a Project Doc
#'
addin_rename_project_doc <- function(selection, settings, orgPath) {


  #### instance variables ####

  oldProjectDocPath <- selection[["filePath"]]

  oldProjectDocFileName <- basename(oldProjectDocPath)

  # split oldProjectDocFileName into PREFIX NAME and EXTENSION
  oldPrefix <- substr(oldProjectDocFileName, 1,
                      regexpr(settings[["ProjectPrefixSep"]], oldProjectDocFileName, fixed=TRUE)-1 )
  oldProjectDocFileNameExt <- tools::file_ext(oldProjectDocFileName)
  oldName <- substr(oldProjectDocFileName,
                    regexpr(settings[["ProjectPrefixSep"]], oldProjectDocFileName, fixed=TRUE)+(nchar(settings[["ProjectPrefixSep"]])),
                    regexpr(oldProjectDocFileNameExt, oldProjectDocFileName, fixed=TRUE)-2) # first letter AND extension .


  # retrieve id to close document prior to renaming
  id <- rstudioapi::getSourceEditorContext()$id


  #### user interface ####

  ui <- miniPage(

    gadgetTitleBar("Rename Project Doc"),

    miniContentPanel(

      fillCol( #flex=NA, # use the natural size of the elements in col

        fillRow( h5("Rename Project Doc:") ),
        fillRow( code( paste0("  ", selection[["filePath"]]) ) ),

        fillRow(  textInput("projectDocName", "New Project Doc Name:", width='100%')  ),
        fillRow(  span( textOutput("warningName"), style="color:red")  ),

        fillRow(  textInput("projectDocTitle", "Sub Note Title:", width='100%')  ),
        fillRow(   span( textOutput("warningDirectory"), style="color:red")  ),

        fillRow(   textOutput("projectDocPath")  )

      )
    )
  )


  #### server function ####

  server <- function(input, output, session) {

    # update projectDocTitle when projectDocName is changed:
    observe({

      updateTextInput(session, "projectDocTitle",
                      value = gsub("-", " ", gsub("_", " ", input$projectDocName) )  )
    })

    observe({

      if( input$projectDocName != "" ) {

        newProjectDocPath <- paste0(dirname(oldProjectDocPath), .Platform$file.sep,
                                    oldPrefix, settings[["ProjectPrefixSep"]],
                                    input$projectDocName, ".", oldProjectDocFileNameExt)
        output$projectDocPath <- renderText({ newProjectDocPath })
      } else {

        output$projectDocPath <- renderText({ "" })
      }
    })

    observe({

      if( grepl("\\s", input$projectDocName)  ) {
        # set the warningName TextOutput:
        output$warningName <- renderText({
          "PROJECT NAME CANNOT CONTAIN SPACES"
        })
      }
      else {
        output$warningName <- renderText({
          ""
        })
      }
    })

    # perform computations to create new Programme:
    observeEvent(input$done, {

      if(input$projectDocName == "") {
        # set the warningName TextOutput:
        output$warningName <- renderText({
          "*** PROVIDE PROJECT NAME ***"
        })
      } else if( grepl("\\s", input$projectDocName)  ) {
        # set the warningName TextOutput:
        output$warningName <- renderText({
          "*** PROJECT NAME CANNOT CONTAIN SPACES ***"
        })
      } else {


        #### Rename Project Doc ####

        # close document
        rstudioapi::documentClose(id)

        # compute new project doc path
        newProjectDocPath <- paste0(dirname(oldProjectDocPath), .Platform$file.sep,
                                    oldPrefix, settings[["ProjectPrefixSep"]],
                                    input$projectDocName, ".", oldProjectDocFileNameExt)

        # rename project doc
        rename_project_doc(
          projectDocPath = oldProjectDocPath,
          newProjectDocName = input$projectDocName,
          newProjectDocTitle = input$projectDocTitle,
          replaceLinksFileExtensions = list(oldProjectDocFileNameExt)
        )

        # open renamed project doc
        rstudioapi::navigateToFile(newProjectDocPath)

        # navigate to containing dir
        rstudioapi::filesPaneNavigate( dirname(newProjectDocPath) )
        # and set working directory
        setwd( dirname(newProjectDocPath) )

        # Close Gadget after computations are complete:
        stopApp()

      }
    })
  }


  #### view gadget ####

  viewer <- dialogViewer("Rename Project Doc",
                         width = settings[["GadgetWidth"]],
                         height = settings[["GadgetHeight"]])

  runGadget(ui, server, viewer = viewer)

}


#' Rename a Project Note
#'
addin_rename_project_note <- function(selection, settings, orgPath) {


  #### instance variables ####

  oldProjectNotePath <- selection[["filePath"]]

  oldProjectNoteFileName <- basename(oldProjectNotePath)

  # split oldProjectNoteFileName into PREFIX NAME and EXTENSION
  oldPrefix <- substr(oldProjectNoteFileName, 1,
                      regexpr(settings[["ProjectPrefixSep"]], oldProjectNoteFileName, fixed=TRUE)-1 )
  oldProjectNoteFileNameExt <- tools::file_ext(oldProjectNoteFileName)
  oldName <- substr(oldProjectNoteFileName,
                    regexpr(settings[["ProjectPrefixSep"]], oldProjectNoteFileName, fixed=TRUE)+(nchar(settings[["ProjectPrefixSep"]])),
                    regexpr(oldProjectNoteFileNameExt, oldProjectNoteFileName, fixed=TRUE)-2) # first letter AND extension .


  # retrieve id to close document prior to renaming
  id <- rstudioapi::getSourceEditorContext()$id


  #### user interface ####

  ui <- miniPage(

    gadgetTitleBar("Rename Project Note"),

    miniContentPanel(

      fillCol( #flex=NA, # use the natural size of the elements in col

        fillRow( h5("Rename Project Note:") ),
        fillRow( code( paste0("  ", selection[["filePath"]]) ) ),

        fillRow(  textInput("projectNoteName", "New Project Note Name:", width='100%')  ),
        fillRow(  span( textOutput("warningName"), style="color:red")  ),

        fillRow(  textInput("projectNoteTitle", "Sub Note Title:", width='100%')  ),
        fillRow(   span( textOutput("warningDirectory"), style="color:red")  ),

        fillRow(   textOutput("projectNotePath")  )

      )
    )
  )


  #### server function ####

  server <- function(input, output, session) {

    # update projectNoteTitle when projectNoteName is changed:
    observe({

      updateTextInput(session, "projectNoteTitle",
                      value = gsub("-", " ", gsub("_", " ", input$projectNoteName) )  )
    })

    observe({

      if( input$projectNoteName != "" ) {

        newProjectNotePath <- paste0(dirname(oldProjectNotePath), .Platform$file.sep,
                                     oldPrefix, settings[["ProjectPrefixSep"]],
                                     input$projectNoteName, ".", oldProjectNoteFileNameExt)
        output$projectNotePath <- renderText({ newProjectNotePath })
      } else {

        output$projectNotePath <- renderText({ "" })
      }
    })

    observe({

      if( grepl("\\s", input$projectNoteName)  ) {
        # set the warningName TextOutput:
        output$warningName <- renderText({
          "PROJECT NAME CANNOT CONTAIN SPACES"
        })
      }
      else {
        output$warningName <- renderText({
          ""
        })
      }
    })

    # perform computations to create new Programme:
    observeEvent(input$done, {

      if(input$projectNoteName == "") {
        # set the warningName TextOutput:
        output$warningName <- renderText({
          "*** PROVIDE PROJECT NAME ***"
        })
      } else if( grepl("\\s", input$projectNoteName)  ) {
        # set the warningName TextOutput:
        output$warningName <- renderText({
          "*** PROJECT NAME CANNOT CONTAIN SPACES ***"
        })
      } else {


        #### Rename Project Note ####

        # close document
        rstudioapi::documentClose(id)

        # compute new project note path
        newProjectNotePath <- paste0(dirname(oldProjectNotePath), .Platform$file.sep,
                                     oldPrefix, settings[["ProjectPrefixSep"]],
                                     input$projectNoteName, ".", oldProjectNoteFileNameExt)

        # rename project note
        rename_project_note(
          projectNotePath = oldProjectNotePath,
          newProjectNoteName = input$projectNoteName,
          newProjectNoteTitle = input$projectNoteTitle,
          replaceLinksFileExtensions = list(oldProjectNoteFileNameExt)
        )

        # open renamed project note
        rstudioapi::navigateToFile(newProjectNotePath)

        # navigate to containing dir
        rstudioapi::filesPaneNavigate( dirname(newProjectNotePath) )
        # and set working directory
        setwd( dirname(newProjectNotePath) )

        # Close Gadget after computations are complete:
        stopApp()

      }
    })
  }


  #### view gadget ####

  viewer <- dialogViewer("Rename Project Note",
                         width = settings[["GadgetWidth"]],
                         height = settings[["GadgetHeight"]])

  runGadget(ui, server, viewer = viewer)

}


#' ADDIN to rename Project Doc GOAL
#'
#' @param selection Selection made on Project Doc Goal.
#'
#' @param settings ProjectManagr organisation settings.yml file
#'
#' @param orgPath The path to the root of the organisation
#'
addin_rename_project_doc_goal <- function(selection, settings, orgPath) {

  #### user interface ####

  ui <- miniPage(

    gadgetTitleBar("Rename Project Doc Goal"),

    miniContentPanel(

      fillCol( #flex=NA, # use the natural size of the elements in col

        fillRow( h5("Rename Project Doc Goal:") ),
        fillRow( code( paste0("  ", selection[["filePath"]]) ) ),
        fillRow( code( paste0("  ", selection[["goal"]]) ) ),

        fillRow(  textInput("projectDocGoalName", "New Project Doc Goal Name:", width='100%')  ),
        fillRow(  span( textOutput("warningName"), style="color:red")  )

      )
    )
  )


  #### server function ####

  server <- function(input, output, session) {

    # Rename goal:
    observeEvent(input$done, {

      if(input$projectDocGoalName == "") {
        # set the warningName TextOutput:
        output$warningName <- renderText({
          "*** PROVIDE GOAL NAME ***"
        })
      } else {


        #### Rename Project Doc GOAL ####

        # rename project doc
        rename_project_doc_goal(
          goalSelection = selection,
          newGoalName = input$projectDocGoalName  )

        # open renamed project doc
        rstudioapi::navigateToFile(selection$filePath)

        # navigate to containing dir
        rstudioapi::filesPaneNavigate( dirname(selection$filePath) )
        # and set working directory
        setwd( dirname(selection$filePath) )

        # Close Gadget after computations are complete:
        stopApp()

      }
    })
  }


  #### view gadget ####

  viewer <- dialogViewer("Rename Project Doc GOAL",
                         width = settings[["GadgetWidth"]],
                         height = settings[["GadgetHeight"]])

  runGadget(ui, server, viewer = viewer)

}



#' ADDIN to rename Project Doc Deliverable
#'
#' @param selection Selection made on Project Doc Deliverable.
#'
#' @param settings ProjectManagr organisation settings.yml file
#'
#' @param orgPath The path to the root of the organisation
#'
addin_rename_project_doc_deliverable <- function(selection, settings, orgPath) {

  #### user interface ####

  ui <- miniPage(

    gadgetTitleBar("Rename Project Doc Deliverable"),

    miniContentPanel(

      fillCol( #flex=NA, # use the natural size of the elements in col

        fillRow( h5("Rename Project Doc Deliverable:") ),
        fillRow( code( paste0("  ", selection[["filePath"]]) ) ),
        fillRow( code( paste0("  ", selection[["deliverable"]]) ) ),

        fillRow(  textInput("projectDocDeliverableName", "New Project Doc Deliverable Name:", width='100%')  ),
        fillRow(  span( textOutput("warningName"), style="color:red")  )

      )
    )
  )


  #### server function ####

  server <- function(input, output, session) {

    # Rename deliverable:
    observeEvent(input$done, {

      if(input$projectDocDeliverableName == "") {
        # set the warningName TextOutput:
        output$warningName <- renderText({
          "*** PROVIDE DELIVERABLE NAME ***"
        })
      } else {


        #### Rename Project Doc DELIVERABLE ####

        # rename project doc
        rename_project_doc_deliverable(
          deliverableSelection = selection,
          newDeliverableName = input$projectDocDeliverableName  )

        # open renamed project doc
        rstudioapi::navigateToFile(selection$filePath)

        # navigate to containing dir
        rstudioapi::filesPaneNavigate( dirname(selection$filePath) )
        # and set working directory
        setwd( dirname(selection$filePath) )

        # Close Gadget after computations are complete:
        stopApp()

      }
    })
  }


  #### view gadget ####

  viewer <- dialogViewer("Rename Project Doc DELIVERABLE",
                         width = settings[["GadgetWidth"]],
                         height = settings[["GadgetHeight"]])

  runGadget(ui, server, viewer = viewer)

}



#' ADDIN to rename Project Doc Task
#'
#' @param selection Selection made on Project Doc Task.
#'
#' @param settings ProjectManagr organisation settings.yml file
#'
#' @param orgPath The path to the root of the organisation
#'
addin_rename_project_doc_task <- function(selection, settings, orgPath) {

  #### user interface ####

  ui <- miniPage(

    gadgetTitleBar("Rename Project Doc Task"),

    miniContentPanel(

      fillCol( #flex=NA, # use the natural size of the elements in col

        fillRow( h5("Rename Project Doc Task:") ),
        fillRow( code( paste0("  ", selection[["filePath"]]) ) ),
        fillRow( code( paste0("  ", selection[["task"]]) ) ),

        fillRow(  textInput("projectDocTaskName", "New Project Doc Task Name:", width='100%')  ),
        fillRow(  span( textOutput("warningName"), style="color:red")  )

      )
    )
  )


  #### server function ####

  server <- function(input, output, session) {

    # Rename Task:
    observeEvent(input$done, {

      if(input$projectDocTaskName == "") {
        # set the warningName TextOutput:
        output$warningName <- renderText({
          "*** PROVIDE TASK NAME ***"
        })
      } else {


        #### Rename Project Doc TASK ####

        # rename project doc
        rename_project_doc_task(
          taskSelection = selection,
          newTaskName = input$projectDocTaskName  )

        # open renamed project doc
        rstudioapi::navigateToFile(selection$filePath)

        # navigate to containing dir
        rstudioapi::filesPaneNavigate( dirname(selection$filePath) )
        # and set working directory
        setwd( dirname(selection$filePath) )

        # Close Gadget after computations are complete:
        stopApp()

      }
    })
  }


  #### view gadget ####

  viewer <- dialogViewer("Rename Project Doc TASK",
                         width = settings[["GadgetWidth"]],
                         height = settings[["GadgetHeight"]])

  runGadget(ui, server, viewer = viewer)

}



#' ADDIN to rename Project File Section Header
#'
#' Section Headers defined in Markdown plasintext files, beginning wiht `#`.
#'
#' @param selection Selection made on Project Doc Task.
#'
#' @param settings ProjectManagr organisation settings.yml file
#'
#' @param orgPath The path to the root of the organisation
#'
addin_rename_project_file_header <- function(selection, settings, orgPath) {

  #### user interface ####

  ui <- miniPage(

    gadgetTitleBar("Rename Project File Header"),

    miniContentPanel(

      fillCol( #flex=NA, # use the natural size of the elements in col

        fillRow( h5("Rename Project File Header:") ),
        fillRow( code( paste0("  ", selection[["filePath"]]) ) ),
        fillRow( code( paste0("  ", selection[["task"]]) ) ),

        fillRow(  textInput("projectFileHeaderName", "New Project File Header Name:", width='100%')  ),
        fillRow(  span( textOutput("warningName"), style="color:red")  )

      )
    )
  )


  #### server function ####

  server <- function(input, output, session) {

    # Rename Task:
    observeEvent(input$done, {

      if(input$projectFileHeaderName == "") {
        # set the warningName TextOutput:
        output$warningName <- renderText({
          "*** PROVIDE TASK NAME ***"
        })
      } else {


        #### Rename Project File Header ####

        # rename project doc
        rename_project_file_header(
          selection = selection,
          headerName = input$projectFileHeaderName  )

        # open renamed project doc
        rstudioapi::navigateToFile(selection$filePath)

        # navigate to containing dir
        rstudioapi::filesPaneNavigate( dirname(selection$filePath) )
        # and set working directory
        setwd( dirname(selection$filePath) )

        # Close Gadget after computations are complete:
        stopApp()

      }
    })
  }


  #### view gadget ####

  viewer <- dialogViewer("Rename Project File Header",
                         width = settings[["GadgetWidth"]],
                         height = settings[["GadgetHeight"]])

  runGadget(ui, server, viewer = viewer)

}


