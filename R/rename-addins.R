
#' Rename a Project File
#'
#' This Addin renames the currently active project file - Note or Doc - with a
#' new filename, and a new title.
#'
#' All links are updated throughout the organisation to the project file.
#'
#'@export
addin_rename_project_file <- function() {

  cat( "\nprojectmanagr::addin_rename_project_file():\n" )


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

  # set confPath + tempPath - these names are FIXED:
  confPath <- paste0( orgPath, .Platform$file.sep, "config" )
  tempPath <- paste0( confPath, .Platform$file.sep, "templates" )

  # load settings + status - settings.yml is FIXED:
  settingsFile <- paste0( confPath, .Platform$file.sep, "settings.yml" )
  settings <- yaml::yaml.load( yaml::read_yaml( settingsFile ) )
  statusFile <- paste0( confPath, .Platform$file.sep, settings[["ConfigStatusYamlFile"]])
  status <- yaml::yaml.load( yaml::read_yaml( statusFile ) )


  #### Rename Project File Addins ####

  # check rmdType and generate correct addin interface
  if( selection$rmdType == "DOC" ) { # project doc active doc


    ##### Rename Project DOC #####

    addin_rename_project_doc(selection, settings, orgPath)

  } else if( selection$rmdType == "HEAD" ||
             selection$rmdType == "SUB" ||
             selection$rmdType == "NOTE") { # project note active doc


    ##### Rename Project NOTE #####

    addin_rename_project_note(selection, settings, orgPath)

  } else if( selection$rmdType == "UNKNOWN" ) {

    addin_error_path("Rename Project File",
                     "No Valid Rmd file selected - ensure a Project Doc or Project Note is the active document.",
                     selection$filePath)
    stop( paste0("  No Valid Rmd file selected - ensure a Project Doc or Project Note is the active document: \n    ", selection$filePath))

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


#'
#' TODO
#'
addin_rename_project_doc_gdt <- function() {

}


