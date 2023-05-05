#' Create a New Project Org Addin
#'
#' Generates a Shiny Gadget for initialising
#' a new Project Organisation.  User selects a destination in
#' the file system, Organisation name (for the directory) and
#' Organisation title (for the html page).
#'
#' @export
addin_create_project_org <- function() {

  cat( "\nprojectmanagr::addin_create_project_org():\n" )


  #### user interface ####

    ui <- miniPage(

      gadgetTitleBar("Create New Project Org"),

      miniContentPanel(

        fillCol(

          fillRow( h5("Initialise a new Project Organisation in Selected Directory.") ),

          fillRow(  textInput("organisationName", "Organisation Directory Name:", value = "00_ORG", width="100%")  ),

          fillRow(  span( textOutput("warning"), style="color:red")  ),

          fillRow(  textInput("organisationTitle", "Organisation Title:", value = "ORGANISATION", width="100%")  ),

          fillRow(   span( textOutput("warning2"), style="color:red")  ),

          #fillRow(  textInput("authorName", "Author Name:", value = "", width="100%")  ),

          #fillRow(   span( textOutput("warning3"), style="color:red")  ),

          fillRow( flex = c(7, 1),  verbatimTextOutput("dir", placeholder = TRUE), shinyDirButton("dir", "Select Directory", "Organisation Parent Directory")  )

        )
      )
    )


    #### server code ####

    server <- function(input, output, session) {

      global <- reactiveValues(datapath = normalizePath("~") )

      # encode functionality for shinyDirButton - select from HOME directory
      shinyDirChoose(
        input,
        'dir',
        roots = c(home = '~'),
        filetypes = c('', 'txt', 'bigWig', "tsv", "csv", "bw")
      )

      dir <- reactive(input$dir)

      output$dir <- renderText({
        global$datapath
      })

      # alter global$datapath to dir selection if dir is changed
      observeEvent(ignoreNULL = TRUE,
         eventExpr = {
           input$dir
         },
         handlerExpr = {
           if (!"path" %in% names(dir())) return()
           home <- normalizePath("~")
           global$datapath <-
             file.path(home, paste(unlist(dir()$path[-1]), collapse = .Platform$file.sep))
         })


      observeEvent(input$done, {

        if(input$organisationName == "") {
          # set the warning TextOutput:
          output$warning <- renderText({
            "PROVIDE ORGANISATION NAME"
          })
        }
        else if(input$organisationTitle == "") {
          # set the warning TextOutput:
          output$warning2 <- renderText({
            "PROVIDE ORGANISATION TITLE"
          })
        }
        #else if(input$authorName == "") {
        #  # set the warning TextOutput:
        #  output$warning3 <- renderText({
        #    "PROVIDE AUTHOR NAME"
        #  })
        #}
        else {


          #### create project org ####

          cat(paste0("organisationName: ", input$organisationName, "\n") )
          cat(paste0("organisationTitle: ", input$organisationTitle, "\n") )
          cat(paste0("global$datapath: ", global$datapath, "\n") )

          # call projectmanagr::create_project_org:
          projectmanagr::create_project_org(
                    orgName = input$organisationName,
                    orgTitle = input$organisationTitle,
                    organisationParentDirectory = global$datapath,
                    settingsYamlPath = ""
                          )

          # navigate to org index file:
          rstudioapi::navigateToFile( paste( global$datapath, .Platform$file.sep, input$organisationName, .Platform$file.sep,
                                             "index_", input$organisationName, ".Rmd", sep=""))

          # navigate to containing dir
          rstudioapi::filesPaneNavigate( paste0(global$datapath, .Platform$file.sep, input$organisationName) )
          # and set working directory
          setwd( paste0(global$datapath, .Platform$file.sep, input$organisationName) )

          # Close Gadget after 'done' is clicked.
          stopApp()
        }
      })
    }


    #### view gadget ####

    viewer <- dialogViewer("Create New Project Organisation", width = 1000, height = 800)
    runGadget(ui, server, viewer = viewer)

}


#' Create a New Programme Addin
#'
#' Generates a Shiny Gadget for creating a new Programme inside
#' a Project Organisation.  User selects a destination in
#' the file system, Programme name (for the directory),
#' Programme prefix (used to identify Projects with this
#' Programme), and Programme title (for the html page).
#'
#' @export
addin_create_programme <- function() {

  cat( "\nprojectmanagr::addin_create_programme():\n" )


  #### instance variables ####

  # identify the orgPath from current working directory - to retrieve the settings yaml file
  orgPath <- find_org_directory( getwd() )

  # if orgPath not identified present error interface and then stop this function
  if(orgPath=="") {
    addin_error_path("Create Programme",
                     "No Organisation identified - ensure working directory is in an Organisation.",
                     orgPath)
    stop( paste0("  No Organisation identified - ensure working directory is in an Organisation: \n    ", getwd()))
  }

  # set confPath + tempPath - these names are FIXED:
  confPath <- paste0( orgPath, .Platform$file.sep, "config" )
  tempPath <- paste0( confPath, .Platform$file.sep, "templates" )

  # load settings file for user defined settings
  settingsFile <- paste( confPath, .Platform$file.sep, "settings.yml", sep="" )
  settings <- yaml::yaml.load( yaml::read_yaml( settingsFile ) )

  # navigate to orgPath
  rstudioapi::filesPaneNavigate(orgPath)
  # and set working directory
  setwd(orgPath)


  #### user interface ####

  ui <- miniPage(

    gadgetTitleBar("Create a Programme"),

    miniContentPanel(

      fillCol(

        fillRow( h3("Create a new Programme inside the Project Organisation") ),

        fillRow( code( orgPath ) ),

        fillRow(  textInput("programmeName", "Programme Directory Name:", value = "01-PROGRAMME", width="100%")  ),

        fillRow(  span( textOutput("warningName"), style="color:red")  ),

        fillRow(  textInput("programmePrefix", "Programme Prefix:", value = "", width="100%")  ),

        fillRow(   span( textOutput("warningPrefix"), style="color:red")  ),

        fillRow(  textInput("programmeTitle", "Programme Title:", value = "01 PROGRAMME", width="100%")  )

        #fillRow( flex = c(7, 1),  verbatimTextOutput("dir", placeholder = TRUE), shinyDirButton("dir", "Select Directory", "Programme Parent Directory")  ),

        #fillRow(   span( textOutput("warningDirectory"), style="color:red")  )

      )
    )
  )


  #### server code ####

  server <- function(input, output, session) {

    # update programmeTitle when programmeName is changed:
    observe({

      updateTextInput(session, "programmeTitle", value = gsub("-", " ", gsub("_", " ", input$programmeName) )  )

    })

    # compute Dir selection:
    #global <- reactiveValues(datapath = find_org_directory( normalizePath(getwd()) )  ) # this sets initial val to organisation DIR

    # allows selection of Dir, with Volume set to HOME Dir
    #shinyDirChoose(
    #  input,
    #  'dir',
    #  roots = c(home = '~'),
    #  filetypes = c('', 'txt', 'bigWig', "tsv", "csv", "bw")
    #)

    #dir <- reactive(input$dir)

    #output$dir <- renderText({
    #  global$datapath
    #})

    #observeEvent(ignoreNULL = TRUE,
    #             eventExpr = {
    #               input$dir
    #             },
    #             handlerExpr = {
    #               if (!"path" %in% names(dir())) return()
    #               home <- normalizePath("~")
    #               global$datapath <-
    #                 file.path(home, paste(unlist(dir()$path[-1]), collapse = .Platform$file.sep))
    #             })

    #observe({
    #  if(global$datapath == "") {
    #    output$warningDirectory <- renderText({
    #      "DIR PATH NOT VALID ORGANISATION"
    #    })
    #  }
    #  else {
    #    output$warningDirectory <- renderText({
    #      ""
    #    })
    #  }
    #})

    # perform computations to create new Programme:
    observeEvent(input$done, {

      if(input$programmeName == "") {
        # set the warningName TextOutput:
        output$warningName <- renderText({
          "*** PROVIDE PROGRAMME NAME ***"
        })
      } else if(input$programmePrefix == "") {
        # set the warningPrefix TextOutput:
        output$warningPrefix <- renderText({
          "*** PROVIDE PROGRAMME PREFIX ***"
        })
      #}
      #else if(global$datapath == "") {
      #  output$warningDirectory <- renderText({
      #    "*** DIR PATH NOT VALID ORGANISATION ***"
      #  })
      } else {

        #### create programme ####

        # call projectmanagr::create_programme:
        projectmanagr::create_programme(
          programmeName = input$programmeName,
          programmePrefix = input$programmePrefix,
          organisationPath = orgPath,
          programmeTitle = input$programmeTitle
        )

        # navigate to programme index file:
        rstudioapi::navigateToFile( paste0( orgPath, .Platform$file.sep,
                                           input$programmeName, .Platform$file.sep,
                                           "index_", input$programmeName, ".Rmd"))

        # navigate to containing dir
        rstudioapi::filesPaneNavigate( paste0(orgPath, .Platform$file.sep, input$programmeName) )
        # and set working directory
        setwd( paste0(orgPath, .Platform$file.sep, input$programmeName) )

        # Close Gadget after 'done' is clicked.
        stopApp()
      }
    })
  }


  #### view gadget ####

  viewer <- dialogViewer("Create a Programme",
                         width = settings[["GadgetWidth"]],
                         height = settings[["GadgetHeight"]])
  runGadget(ui, server, viewer = viewer)

}


#' Create a New Project Document Addin
#'
#' Generates a Shiny Gadget for creating a new Project Doc inside
#' a Programme.
#'
#' User selects a destination in
#' the file system (MUST be a Programme Dir), Project name, and
#' Project title (for the html page).
#'
#' Stipulates any errors in the input, and can only be completed
#' when these errors have been resolved.
#'
#'
#' @export
addin_create_project_doc <- function() {

  cat( "\nprojectmanagr::addin_create_project_doc():\n" )


  #### instance variables ####

  # store current working directory
  WD <- getwd()

  # identify the orgPath from current working directory - to retrieve the settings yaml file
  orgPath <- find_org_directory( WD )

  # if orgPath not identified present error interface and then stop this function
  if(orgPath=="") {
    addin_error_path("Create Project Document",
                     "No Organisation identified - ensure working directory is in an Organisation.",
                     orgPath)
    stop( paste0("  No Organisation identified - ensure working directory is in an Organisation: \n    ", WD))
  }

  # set confPath + tempPath - these names are FIXED:
  confPath <- paste0( orgPath, .Platform$file.sep, "config" )
  tempPath <- paste0( confPath, .Platform$file.sep, "templates" )

  # load settings file for user defined settings
  settingsFile <- paste0( confPath, .Platform$file.sep, "settings.yml")
  settings <- yaml::yaml.load( yaml::read_yaml( settingsFile ) )

  # load status file for organisation status
  statusFile <- paste0( confPath, .Platform$file.sep, settings[["ConfigStatusYamlFile"]])
  status <- yaml::yaml.load( yaml::read_yaml( statusFile ) )

  # compute all programme directories
  programmeDirNames <- names(status$PROGRAMMES)
  programmeDirPaths <-  paste0(orgPath, .Platform$file.sep, programmeDirNames)

  # check these exist
  if( any( check_prog_dir(programmeDirPaths, settings) == "" ) ) {
    addin_error_path("Create Project Document",
                     "Programme directories not identified - check the status.yaml file is correct.",
                     programmeDirPaths[check_prog_dir(programmeDirPaths, settings) == "" ])
    stop( paste0("  Programme directories not identified - check the status.yaml file is correct: \n    ", statusFile) )
  }

  # create named list of programmeDirNames
  programmeDirNamesChoices <- as.list(seq(length(programmeDirNames)))
  names(programmeDirNamesChoices) <- programmeDirNames

  # check whether the current working directory is inside a programme
  # if so determine the INDEX of this in programmeDirNames
  currentProgDir <- find_prog_dir(WD, settings) # returns blank string if no prog dirs identified
  if( currentProgDir == "" ) {
    progSelected <- 1
  } else {
    progSelected <- grep(currentProgDir, programmeDirPaths)
  }



  #### user interface ####

  ui <- miniPage(

    gadgetTitleBar("Create a Project Document"),

    miniContentPanel(

      fillCol(

        fillRow( h4("Create a Project Document inside a Programme.") ),

        fillRow( code( orgPath ) ),

        fillRow( selectInput("selectProg", h3("Select Programme"),
                             choices = programmeDirNamesChoices,
                             selected = progSelected) ),

        fillRow(  textInput("projectName", "Project Name:", value = "PROJECT", width="100%")  ),

        fillRow(  span( textOutput("warningName"), style="color:red")  ),

        fillRow(  textInput("projectTitle", "Project Title:", value = "01 PROGRAMME", width="100%")  ),

        #fillRow( flex = c(7, 1),  verbatimTextOutput("dir", placeholder = TRUE), shinyDirButton("dir", "Select Directory", "Programme Parent Directory")  ),

        #fillRow(   span( textOutput("warningDirectory"), style="color:red")  ),

        fillRow(   h3(textOutput("projectPathOutput"))  )

      )
    )
  )


  #### server code ####

  server <- function(input, output, session) {

    # update programmeTitle when programmeName is changed:
    observe({

      updateTextInput(session, "projectTitle", value = gsub("-", " ", gsub("_", " ", input$projectName) )  )

    })


    # compute Dir selection:
    #global <- reactiveValues(datapath = normalizePath(orgPath)) # this sets initial val to wd - current WD or the PROGRAMME if in the PROEJCTS DIR

    # allows selection of Dir, with Volume set to HOME Dir
    #shinyDirChoose(
    #  input,
    #  'dir',
    #  roots = c(home = '~'),
    #  filetypes = c('', 'txt', 'bigWig', "tsv", "csv", "bw")
    #)

    #dir <- reactive(input$dir)

    #observeEvent(ignoreNULL = TRUE,
    #             eventExpr = {
    #               input$dir
    #             },
    #             handlerExpr = {
    #               if (!"path" %in% names(dir())) return()
    #               home <- normalizePath("~")
    #               global$datapath <-
    #                 file.path(home, paste(unlist(dir()$path[-1]), collapse = .Platform$file.sep))
    #             })

    #observe({
    #  if(global$datapath != "") {
    #    global$datapath <- check_prog_dir(global$datapath)
    #  }
    #})

    #output$dir <- renderText({
    #  global$datapath
    #})

    #observe({
    #  if(global$datapath == "") {
    #    output$warningDirectory <- renderText({
    #      "DIR PATH NOT VALID PROGRAMME"
    #    })
    #  }
    #  else {
    #    output$warningDirectory <- renderText({
    #      ""
    #    })
    #  }
    #})


    observe({

      if( input$projectName != "" ) {
        progPath <- programmeDirPaths[ as.integer(input$selectProg[1]) ]
        projsPath <- paste0(progPath, .Platform$file.sep, settings[["ProgrammeProjectsDir"]])
        progName <- basename(progPath)
        programmePrefix <- status[["PROGRAMMES"]][[progName]][["programmePrefix"]]
        projectIndex <- compute_project_index(projsPath, programmePrefix)

        projPath <- paste0(progPath,
                           .Platform$file.sep, settings[["ProgrammeProjectsDir"]],
                           .Platform$file.sep, programmePrefix, projectIndex,
                           settings[["ProjectPrefixSep"]], input$projectName, ".Rmd")

        output$projectPathOutput <- renderText({
          projPath
        })
      } else {
        output$projectPathOutput <- renderText({
          ""
        })
      }

    })


    observe({

      if( grepl("\\s", input$projectName)  ) {
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

      if(input$projectName == "") {
        # set the warningName TextOutput:
        output$warningName <- renderText({
          "*** PROVIDE PROJECT NAME ***"
        })
      }
      else if( grepl("\\s", input$projectName)  ) {
        # set the warningName TextOutput:
        output$warningName <- renderText({
          "*** PROJECT NAME CANNOT CONTAIN SPACES ***"
        })
      }
      #else if(global$datapath == "") {
      #  output$warningDirectory <- renderText({
      #    "*** DIR PATH NOT VALID PROGRAMME ***"
      #  })
      #}
      #else if(input$select == "") {
      #  output$warningTemplate <- renderText({
      #    "*** SELECT A TEMPLATE ***"
      #  })
      #}
      else {

        # FIRST - save all open documents in RStudio:
        rstudioapi::documentSaveAll()

        #### create project doc ####

        progPath <- programmeDirPaths[ as.integer(input$selectProg[1]) ]

        # call projectmanagr::createProjectDoc:
        projectmanagr::create_project_doc(
          projectName = input$projectName,
          programmePath = progPath,
          projectTitle = input$projectTitle
        )


        # navigate to project doc file:
        projsPath <- paste0(progPath, .Platform$file.sep, settings[["ProgrammeProjectsDir"]])
        rstudioapi::navigateToFile( paste0( projsPath, .Platform$file.sep,
                                    list.files(projsPath)[grepl(input$projectName, list.files(projsPath))]  )  )

        # navigate to containing dir
        rstudioapi::filesPaneNavigate( projsPath )
        # and set working directory
        setwd( projsPath )

        # Close Gadget after 'done' is clicked.
        stopApp()
      }
    })
  }


  #### view gadget ####

  viewer <- dialogViewer("Create Project Document",
                         width = settings[["GadgetWidth"]],
                         height = settings[["GadgetHeight"]])
  runGadget(ui, server, viewer = viewer)

}



#' Add a New Project Note Addin
#'
#' Generates a Shiny Gadget for adding a new Project Note inside
#' a Project Doc.
#'
#' The User must select a Task or Project Header Note Link in a
#' Project Document.  If a Project Task is selected, the User
#' can build a new Project Note - either a Simple Note or a Group
#' Note (Header plus one SubNote).  If a Project Header Note Link
#' is selected, the User can ADD a new SubNote to this Group Note.
#'
#' User selects a destination in
#' the file system (MUST be a Programme Dir), Project name, and
#' Project title (for the html page).
#'
#' Stipulates any errors in the input, and can only be completed
#' when these errors have been resolved.
#'
#' @export
addin_create_project_note <- function() {

  cat( "\nprojectmanagr::addin_create_project_note():\n" )


  #### instance variables ####

  # Retrieve cursor_selection from current Active Doc in rstudio:
  selection <- cursor_selection() # Project Doc, Project Note - Simple, Header, SubNote

  if(selection[["rmdType"]]=="UNKNOWN") { # return error message from selection
    addin_error("Add Project Note", selection[["errorMessage"]])
    stop( paste0("  ", selection[["errorMessage"]]))
  }

  # get the orgPath:
  orgPath <- find_org_directory(selection$filePath)

  # if orgPath not identified present error interface and then stop this function
  if(orgPath=="") {
    addin_error_path("Add Project Note",
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

  # get progPath
  progPath <- find_prog_dir(selection$filePath, settings)


  #### Add Project Note Addins ####

  # check rmdType and generate correct addin interface
  if( selection$rmdType == "DOC" ) { # project doc selected

    if( selection$addingSubNote == FALSE ) { # selection on Project TASK

      ##### Add Project Note to DOC #####
      # add a single or group note to doc at selected GDT
      addin_create_project_note_from_doc_gdt(selection, settings, orgPath)

    } else { # selection on group project note

      ##### Add Sub Note to group #####
      # add new subnote under group note - inheriting all header note GDTs
      addin_create_subnote_from_group(selection, settings, orgPath)

    }

  } else if( selection$rmdType == "HEAD" ) {

    ##### Add Sub Note to GROUP : inherit header #####
    # add new subnote to group note - inheriting all header note GDTs
    addin_create_subnote_from_group(selection, settings)

  } else if( selection$rmdType == "SUB" ) {

    ##### Add Sub Note to GROUP : inherit subnote #####
    # add new subnote to group note - inheriting all subnote GDTs
    addin_create_subnote_from_subnote(selection, settings)

  } else if( selection$rmdType == "NOTE" ) {

    ##### Add Project Note : inherit single note - NOT SUPPORTED #####
    # currently NOT SUPPORTED - so generating an error message
    addin_error_path("Add Project Note",
                     "  Simple Project Note selected - unsupported selection. Select a project doc GDT to add new Simple Project Note.",
                     selection$filePath)
    stop( paste0("  Simple Project Note selected - unsupported selection. Select a project doc GDT to add new Simple Project Note: \n    ", selection$filePath))

    #addin_create_project_note_to_single(selection, settings)

  } else if( selection$rmdType == "UNKNOWN" ) {

    addin_error_path("Add Project Note",
                     "No Valid Rmd file selected - ensure a Project Doc or Project Note is the active document.",
                     selection$filePath)
    stop( paste0("  No Valid Rmd file selected - ensure a Project Doc or Project Note is the active document: \n    ", selection$filePath))

  }

}


#' Add a single or group note to doc at selected GDT
#'
addin_create_project_note_from_doc_gdt <- function(selection, settings, orgPath) {


  #### instance variables ####

  # get projectDir & progDir
  projectDirPath <- get_project_doc_dir_path(selection[["filePath"]], settings)
  progPath <- find_prog_dir(selection[["filePath"]], settings)

  # create vector of possible roots for dir selection in server()
  roots <- c(projectDirPath, progPath, orgPath) # can use projectDirPath, progPath, or orgPath as roots
  names(roots) <- c('project_dir', basename(progPath), basename(orgPath))


  # compute the goal/del/task NUM and TITLE:
  glen <- nchar(unlist(strsplit(settings[["ProjectGoalHeader"]],
                                split=settings[["ProjectGoalTitle"]], fixed=TRUE)))+1
  goal <- substring(selection[["goal"]], first=glen)
  goalTitle <- substring(goal,  first=(regexpr(":", goal)+2 ) )
  goalNum <- as.integer(  substring(goal,  first=5, last=(regexpr(":", goal)-1) )  )

  dlen <- nchar(unlist(strsplit(settings[["ProjectDeliverableHeader"]],
                                split=settings[["ProjectDeliverableTitle"]], fixed=TRUE)))+1
  del <- substring(selection[["deliverable"]], first=dlen)
  delTitle <- substring(del,  first=(regexpr(":", del)+2 ) )
  delNum <- as.integer(  substring(del,  first=12, last=(regexpr(":", del)-1) )  )

  tlen <- nchar(unlist(strsplit(settings[["ProjectTaskHeader"]],
                                split=settings[["ProjectTaskTitle"]], fixed=TRUE)))+1
  task <- substring(selection[["task"]], first=tlen)
  taskTitle <- substring(task,  first=(regexpr(":", task)+2 ) )
  taskNum <- as.integer(  substring(task,  first=5, last=(regexpr(":", task)-1) )  )



  #### user interface ####

  ui <- miniPage(

    shinyjs::useShinyjs(),

    gadgetTitleBar("Add New Project Note"),

    miniContentPanel(

      fillCol( #flex=NA, # use the natural size of the elements in col

        fillRow( h5("Add a new Project Note to a Project Document.") ),

        fillRow(
          helpText(  h3(  paste("GOAL", goalNum), align="center" )   ),
          helpText(  h3(  paste("DELIVERABLE", delNum), align="center" )   ),
          helpText(  h3(  paste("TASK", taskNum) ), align="center"   )
        ),

        fillRow(
          helpText( p(goalTitle, align="center") ),
          helpText( p(delTitle, align="center") ),
          helpText( p(taskTitle, align="center") )
        ),

        fillRow(  selectInput("prefixType", "Select Project Note Type:",
                              choices = list("Single" = 1, "Group" = 2),
                              selected = 1, width = '50%')  ),

        fillRow( br() ),

        fillRow(  span( textOutput("warningName"), style="color:red")  ),

        fillRow(  textInput("projectNoteName", "Project Note Name:", value = "Note_Name", width='95%'),
                  textInput("projectNoteTitle", "Project Note Title:", value = "Note Name", width='95%')   ),

        fillRow(   span( textOutput("warningDirectory"), style="color:red")  ),

        fillRow( flex = c(5, 1),  verbatimTextOutput("dir", placeholder = TRUE), shinyDirButton("dir", "Select Directory", "Note Parent Directory")  ),

        fillRow(   textOutput("projectNotePath")  ),

        fillRow( br() ),

        fillRow(  textInput("subNoteName", "Project SubNote Name:", width='95%'),
                  textInput("subNoteTitle", "Project SubNote Title:", width='95%')  ),

        fillRow( checkboxInput("addObjToHeader", 'Add Objective to Header', width='95%') ),

        fillRow(   textOutput("subNotePath")  ),

      ),

      padding = 10

    )

  )


  #### server function ####

  server <- function(input, output, session) {

    # update projectNoteTitle when projectNoteName is changed:
    observe({

      updateTextInput(session, "projectNoteTitle", value = gsub("-", " ", gsub("_", " ", input$projectNoteName) )  )

    })

    # update subNoteTitle when subNoteName is changed:
    observe({

      updateTextInput(session, "subNoteTitle", value = gsub("-", " ", gsub("_", " ", input$subNoteName) )  )

    })


    # compute Dir selection:
    global <- reactiveValues(datapath = projectDirPath )
    # this sets initial value of global$datapath to projectDirPath

    # allows selection of Dir, with roots set to project doc DIR or ORG Dir
    shinyDirChoose(
      input, 'dir', defaultRoot = 'project_dir',
      roots=roots, # can use projectDirPath, progPath, or orgPath as roots
      filetypes = c('', 'txt', 'Rmd', "tsv", "csv", "bw")
    )

    dir <- reactive(input$dir)

    # observe({ cat('\n  input$dir: _', input$dir[[1]], '_\n') }) this causes an error when input$dir becomes a list
     # so check in the observeEvent() function below

    # update global$datapath
    observeEvent(ignoreNULL = TRUE,
                 eventExpr = { # if input$dir is changed
                   input$dir
                 },
                 handlerExpr = { # update datapath with dir() list
                   if (!"path" %in% names(dir())) return() # check the path element exists in dir
                   #cat("\n dir() names: ", names(dir())) # contains : root, path
                   #cat("\n  dir$root: ", dir()$root) # name of the root selected in shinyDirChoose
                   #cat("\n  dir$path: _", unlist( dir()$path ), "_" ) # list of each dir in dirTree, separated by space?
                   #cat("\n  dir$path pasted with fileSep: _", paste( unlist( dir()$path ), collapse = .Platform$file.sep ), "_" )
                    # list of each dir in dirTree created into a path
                   #cat("\n  dir$path[-1]: _", unlist( dir()$path[-1] ), "_" ) # list of each dir in dirTree, separated by space?
                   #cat("\n  dir$path[-1] pasted with fileSep: _", paste( unlist( dir()$path[-1] ), collapse = .Platform$file.sep ), "_" )
                    # list of each dir in dirTree created into a path
                   global$datapath <- file.path( # form path with
                     roots[[dir()$root]], # shinyDirChoose selected ROOT (selected by its NAME found in dir()$root)
                     paste( unlist( dir()$path[-1] ), collapse = .Platform$file.sep )  ) # shinyDirChoose selected PATH with file.sep added
                 })

    observe({ cat('\n  global$datapath: _', global$datapath, '_\n') })



    observe({
      if(global$datapath != "") { # check computed dir is in a Programme
        global$datapath <- check_prog_sub_dir(global$datapath)
      }
    })

    # show the global$datapath computed from input$dir in output$dir (next to shinyDirButton!)
    output$dir <- renderText({
      global$datapath
    })

    observe({
      if(global$datapath == "") {
        output$warningDirectory <- renderText({
          "DIR PATH NOT VALID DIRECTORY - Must be INSIDE a PROGRAMME DIR"
        })
      }
      else {
        output$warningDirectory <- renderText({
          ""
        })
      }
    })


    observe({

      if(input$prefixType == "1") { # SINGLE NOTE selected

        # render projectNoteName as SINGLE note then DISABLE subnote names & title inputs
        if( global$datapath != "" && input$projectNoteName != "" ) {

          projNotePath <- get_project_note_path(global$datapath, input$projectNoteName, settings)
          output$projectNotePath <- renderText({ projNotePath })

        } else { # do not render a projectNotePath

          output$projectNotePath <- renderText({ "" })

        }

        shinyjs::disable("subNoteName")
        shinyjs::disable("subNoteTitle")
        shinyjs::disable("addObjToHeader")

      } else if(input$prefixType == "2") { # GROUP NOTE selected

        # render projectNoteName as GROUP HEADER note, THEN enable subnote name/title and render subNoteName as SUB NOTE
        if( global$datapath != "" && input$projectNoteName != "" ) {

          headerNotePath <- get_header_note_path(global$datapath, input$projectNoteName, settings )
          output$projectNotePath <- renderText({ headerNotePath })

        }else {

          output$projectNotePath <- renderText({ "" })

        }

        shinyjs::enable("subNoteName")
        shinyjs::enable("subNoteTitle")
        shinyjs::enable("addObjToHeader")

        if( global$datapath != "" && input$subNoteName != "" ) {

          headerNoteDir <- get_next_header_note_dir(global$datapath, input$projectNoteName, settings)
          subNotePath <- get_sub_note_path(headerNoteDir, input$subNoteName, input$projectNoteName, settings)
          output$subNotePath <- renderText({ subNotePath })

        } else {

          output$subNotePath <- renderText({ "" })

        }
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


    observe({

      if( grepl("\\s", input$subNoteName)  ) {
        # set the warningSubName TextOutput:
        output$warningName <- renderText({
          "SUBNOTE NAME CANNOT CONTAIN SPACES"
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
      } else if(input$prefixType == "2" && input$subNoteName == "") {
        # set the warningName TextOutput:
        output$warningName <- renderText({
          "*** PROVIDE SUBNOTE NAME ***"
        })
      } else if( input$prefixType == "2" && grepl("\\s", input$subNoteName)  ) {
        # set the warningName TextOutput:
        output$warningName <- renderText({
          "*** SUBNOTE NAME CANNOT CONTAIN SPACES ***"
        })
      } else if(global$datapath == "") {
        output$warningDirectory <- renderText({
          "*** DIR PATH NOT VALID PROGRAMME ***"
        })
      } else { # add project note

        if(input$prefixType == "1") {

          #### Add SINGLE note ####

          # compute new project note path
          projNotePath <- get_project_note_path(global$datapath, input$projectNoteName, settings)

          # add project note
          create_project_note(projectNoteName = input$projectNoteName,
                              projectNotePath = global$datapath,
                              selection = selection,
                              projectNoteTitle = input$projectNoteTitle)

          # open new project note
          rstudioapi::navigateToFile(projNotePath)

          # navigate to containing dir
          rstudioapi::filesPaneNavigate( global$datapath )
          # and set working directory
          setwd( global$datapath )

        } else {

          #### Add GROUP note ####

          # compute new header note dir && subnote path
          headerNoteDir <- get_next_header_note_dir(global$datapath, input$projectNoteName, settings)
          subNotePath <- get_sub_note_path(headerNoteDir, input$subNoteName,
                                           input$projectNoteName, settings)

          # add group note
          create_group_note(groupNoteName = input$projectNoteName,
                            groupNotePath = global$datapath,
                            selection = selection,
                            subNoteName = input$subNoteName,
                            addObjToHeader = input$addObjToHeader,
                            groupNoteTitle = input$projectNoteTitle,
                            subNoteTitle = input$subNoteTitle)

          # open new subnote
          rstudioapi::navigateToFile(subNotePath)

          # navigate to containing dir
          rstudioapi::filesPaneNavigate(headerNoteDir)
          # and set working directory
          setwd(headerNoteDir)

        }

        # Close Gadget after computations are complete:
        stopApp()

      }
    })
  }


  #### view gadget ####

  viewer <- dialogViewer("Add New Project Note",
                         width = settings[["GadgetWidth"]],
                         height = settings[["GadgetHeight"]])

  runGadget(ui, server, viewer = viewer)

}


#' Add new subnote under group note - inheriting all group note GDTs
#'
addin_create_subnote_from_group <- function(selection, settings, orgPath) {


  #### user interface ####

  ui <- miniPage(

    gadgetTitleBar("Add New Sub Note"),

    miniContentPanel(

      fillCol( #flex=NA, # use the natural size of the elements in col

        fillRow( h5("Add a new Sub Note to a Project Note Group:") ),

        fillRow( h5("Inherit Goal/Del/Tasks from Group HEADER NOTE:") ),
        fillRow( code( paste0("  ", selection[["headerNoteRmdPath"]]) ) ),

        fillRow(  textInput("projectNoteName", "Sub Note Name:", width='100%')  ),
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

        subNotePath <- get_sub_note_path(
          get_header_note_dir_path(selection[["headerNoteRmdPath"]], settings),
          input$projectNoteName, selection[["headerNoteName"]], settings)
        output$projectNotePath <- renderText({ subNotePath })

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


        #### Add SUB note ####

        # compute new subnote path
        subNotePath <- get_sub_note_path(
          get_header_note_dir_path(selection[["headerNoteRmdPath"]], settings),
          input$projectNoteName, selection[["headerNoteName"]], settings)

        # add subnote
        create_sub_note(
          subNoteName = input$projectNoteName,
          subNotePath = dirname(subNotePath),
          selection = selection,
          subNoteTitle = input$projectNoteTitle
        )

        # open new subnote
        rstudioapi::navigateToFile(subNotePath)

        # navigate to containing dir
        rstudioapi::filesPaneNavigate( dirname(subNotePath) )
        # and set working directory
        setwd( dirname(subNotePath) )

        # Close Gadget after computations are complete:
        stopApp()

      }
    })
  }


  #### view gadget ####

  viewer <- dialogViewer("Add New Sub Note",
                         width = settings[["GadgetWidth"]],
                         height = settings[["GadgetHeight"]])

  runGadget(ui, server, viewer = viewer)

}


#' Add new subnote under group note - inheriting all group note GDTs
#'
addin_create_subnote_from_subnote <- function(selection, settings, orgPath) {


  #### user interface ####

  ui <- miniPage(

    gadgetTitleBar("Add New Sub Note"),

    miniContentPanel(

      fillCol( #flex=NA, # use the natural size of the elements in col

        fillRow( h5("Add a new Sub Note to a Project Note Group.") ),

        fillRow( h5("Inherit Goal/Del/Tasks from SUBNOTE:") ),
        fillRow( code( paste0("  ", selection[["filePath"]]) ) ),

        fillRow(  textInput("projectNoteName", "Sub Note Name:", width='100%')  ),
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

        subNotePath <- get_sub_note_path(
          get_header_note_dir_path(selection[["headerNoteRmdPath"]], settings),
          input$projectNoteName, selection[["headerNoteName"]], settings)
        output$projectNotePath <- renderText({ subNotePath })

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


        #### Add SUB note ####

        # compute new subnote path
        subNotePath <- get_sub_note_path(
          get_header_note_dir_path(selection[["headerNoteRmdPath"]], settings),
          input$projectNoteName, selection[["headerNoteName"]], settings)

        # add subnote
        create_sub_note(
          subNoteName = input$projectNoteName,
          subNotePath = dirname(subNotePath),
          selection = selection,
          subNoteTitle = input$projectNoteTitle
        )

        # open new subnote
        rstudioapi::navigateToFile(subNotePath)

        # navigate to containing dir
        rstudioapi::filesPaneNavigate( dirname(subNotePath) )
        # and set working directory
        setwd( dirname(subNotePath) )

        # Close Gadget after computations are complete:
        stopApp()

      }
    })
  }


  #### view gadget ####

  viewer <- dialogViewer("Add New Sub Note",
                         width = settings[["GadgetWidth"]],
                         height = settings[["GadgetHeight"]])

  runGadget(ui, server, viewer = viewer)

}



#' Add a New Protocol Addin
#'
#' Generates a Shiny Gadget for adding a new Protocol inside the `protocol`
#' directory of the containing Programme, and inserting a link to this
#' inside the active Project Note.
#'
#' The User must be in an active Project Note, and the selected
#' line and column is where the Protocol Link will be inserted.
#' This command works for all Project Notes.  A reciprocal link is formed
#' from the Protocol to the Project Note.
#'
#' User selects a Protocol name, and (Optionally) Protocol
#'  title (filled by default with SPACES replacing "_" and "-" from name).
#'
#' Stipulates any errors in the input, and can only be completed
#' when these errors have been resolved.
#'
#'
#' @export
addin_create_protocol <- function() {

  cat( "\nprojectmanagr::addin_create_protocol():\n" )


  #### instance variables ####

  # Retrieve cursor_selection from current Active Doc in rstudio:
  selection <- cursor_selection() # Project Doc, Project Note - Simple, Header, SubNote

  if(selection[["rmdType"]]=="UNKNOWN") { # return error message from selection
    addin_error("Add New Protocol", selection[["errorMessage"]])
    stop( paste0("  ", selection[["errorMessage"]]))
  }

  # get the orgPath:
  orgPath <- find_org_directory(selection$filePath)

  # if orgPath not identified present error interface and then stop this function
  if(orgPath=="") {
    addin_error_path("Add New Protocol",
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

  context <- rstudioapi::getSourceEditorContext()
  cursor <- rstudioapi::primary_selection(context)


  #### user interface ####

  ui <- miniPage(

    gadgetTitleBar("Add New Protocol"),

    miniContentPanel(

      fillCol(

        fillRow( h5("Add a new Protocol to a Project File") ),

        #fillRow( p("Protocol Name MUST end with _Protocol:") ),

        fillRow(  textInput("protocolName", "Protocol Name", value=settings[["ProtocolNameSuffix"]], width="100%")  ),

        fillRow(  span( textOutput("warningName"), style="color:red")  ),

        fillRow(  textInput("protocolTitle", "Protocol Title:", width="100%")  ),

        fillRow(   span( textOutput("warningDirectory"), style="color:red")  ),

        fillRow(   textOutput("protocolPath")  )

      )
    )
  )


  #### server function ####

  server <- function(input, output, session) {

    # update protocolTitle when protocolName is changed:
    observe({

      updateTextInput(session, "protocolTitle", value = gsub("-", " ", gsub("_", " ", input$protocolName) )  )

    })

    output$protocolPath <- renderText({
      #selection[["filePath"]]
      projectNotePath <- normalizePath(selection[["filePath"]])
      progPath <- find_prog_dir(projectNotePath, settings)
      protocolsPath <- paste0(progPath, .Platform$file.sep, settings[["ProgrammeProtocolsDir"]])
      protocolDirPath <- paste( protocolsPath, .Platform$file.sep, input$protocolName, sep="")
      paste0( protocolDirPath, .Platform$file.sep, input$protocolName, ".Rmd")
    })

    observe({

      if( grepl("\\s", input$protocolName)  ) {
        # set the warningName TextOutput:
        output$warningName <- renderText({
          "PROTOCOL NAME CANNOT CONTAIN SPACES"
        })
      } else {
        output$warningName <- renderText({""})
      }
    })

    # perform computations to create new Programme:
    observeEvent(input$done, {

      if(input$protocolName == "") {
        # set the warningName TextOutput:
        output$warningName <- renderText({
          "*** PROVIDE PROTOCOL NAME ***"
        })
      }
      else if(! endsWith( input$protocolName, settings[["ProtocolNameSuffix"]] )  ) {
        # if protocolName does not end with protocol or reference, stop and inform user:
        # set the warningName TextOutput:
        output$warningName <- renderText({
          paste0("*** NAME MUST END WITH ProtocolNameSuffix*** : ", settings[["ProtocolNameSuffix"]])
        })
      }
      else if( grepl("\\s", input$protocolName)  ) {
        # set the warningName TextOutput:
        output$warningName <- renderText({
          "*** PROTOCOL NAME CANNOT CONTAIN SPACES ***"
        })
      }
      else {

        # define protocol Rmd path
        projectNotePath <- normalizePath(selection[["filePath"]])
        progPath <- find_prog_dir(projectNotePath, settings)
        protocolsPath <- paste0(progPath, .Platform$file.sep, settings[["ProgrammeProtocolsDir"]])
        protocolDirPath <- paste0( protocolsPath, .Platform$file.sep, input$protocolName)
        protocolPath <- paste0( protocolDirPath, .Platform$file.sep, input$protocolName, ".Rmd")

        # create protocol
        projectmanagr::create_protocol(
          protocolName = input$protocolName,
          selection = selection,
          protocolTitle = input$protocolTitle,
          protocolTemplate="Protocol-Template.Rmd" )

        # open new protocol
        rstudioapi::navigateToFile(protocolPath)

        # navigate to containing dir
        rstudioapi::filesPaneNavigate( dirname(protocolPath) )
        # and set working directory
        setwd( dirname(protocolPath) )

        # Close Gadget after computations are complete:
        stopApp()

      }
    })
  }


  #### view gadget ####

  viewer <- dialogViewer("Add New Protocol",
                         width = settings[["GadgetWidth"]],
                         height = settings[["GadgetHeight"]])

  runGadget(ui, server, viewer = viewer)

}


