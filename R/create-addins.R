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

  runGadget(addin_create_project_org_ui(), addin_create_project_org_server, viewer=dialogViewer("Create New Project Organisation", width = 1000, height = 800))

}


#' Create Project Org UI
#'
addin_create_project_org_ui <- function() {

  miniPage(

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

        fillRow( flex = c(7, 1),  verbatimTextOutput("dirO", placeholder = TRUE), shinyDirButton("dir", "Select Directory", "Organisation Parent Directory")  )

        #fillRow( textInput("dirInput", "Directory (Manual Entry):", value="", width="100%") )

      )
    )
  )
}


#' Create Project Org: Shiny Server
#'
#' Creates a Proect Org using `create_project_org()` function, with reactive
#' inputs: input$dir, input$organisationName, input$organisationTitle.
#'
addin_create_project_org_server <- function(input, output, session) {

  ### SETUP

  # define the root as root of current working directory : first element in list from path_wd split
  rootDir <- fs::path_split(fs::path_wd())[[1]][1]

  # define global$datapath as rootDir
  global <- reactiveValues(datapath = rootDir )
  #global <- reactiveValues(datapath = normalizePath("~") )

  # encode functionality for shinyDirButton - select from rootDir
  shinyDirChoose(
    input,
    'dir',
    roots = c(home = rootDir),
    filetypes = c('', 'txt', 'bigWig', "tsv", "csv", "bw")
  )

  dir <- reactive(input$dir)

  # alter global$datapath to dir selection if dir is changed
  observeEvent(ignoreNULL = TRUE, eventExpr = {input$dir},
               handlerExpr = { if (!"path" %in% names(dir())) return()
                 #home <- normalizePath("~")
                 #global$datapath <- file.path(rootDir, paste(unlist(dir()$path[-1]), collapse = .Platform$file.sep) ) })
                 global$datapath <- fs::path(rootDir, paste(unlist(dir()$path[-1]), collapse = .Platform$file.sep) ) })

  observe({ cat('\n  global$datapath: _', global$datapath, '_\n') })

  # update dirO text output to global datapath
  output$dirO <- renderText({ global$datapath })

  observeEvent(input$done, {

    if(input$organisationName == "") {output$warning <- renderText({"PROVIDE ORGANISATION NAME" })

    } else if(input$organisationTitle == "") {output$warning2 <- renderText({"PROVIDE ORGANISATION TITLE"})

    } else {

      #### create project org ####

      #cat(paste0("organisationName: ", input$organisationName, "\n") )
      #cat(paste0("organisationTitle: ", input$organisationTitle, "\n") )
      #cat(paste0("global$datapath: ", global$datapath, "\n") )

      # call projectmanagr::create_project_org:
      orgIndexPath <- create_project_org(orgParentPath = global$datapath,
                        orgName = input$organisationName, orgTitle = input$organisationTitle,
                        settingsYamlPath = "", orgTemplate="Org-Template.Rmd" )

      # navigate to org index file & close addin:
      addin_rstudio_nav(orgIndexPath)
    }
  })
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
  if(orgPath=="") { addin_error_org("Create Programme") }

  settings <- get_settings_yml(orgPath)

  #### RUN GADGET ####
  runGadget(addin_create_programme_ui(orgPath), addin_create_programme_server, viewer = addin_create_dialog_viewer("Create a Programme",settings))

}


addin_create_programme_ui <- function(orgPath) {

  miniPage(

    shinyjs::useShinyjs(), # to enable & disable subNote input

    gadgetTitleBar("Create a Programme"),

    miniContentPanel(

      fillCol(

        fillRow( h3("Create a new Programme inside the Project Organisation") ),

        #fillRow( code( orgPath ) ),

        fillRow( textInput("orgPath", "Organisation Path:", value=orgPath, width="100%")),

        fillRow(  textInput("programmeName", "Programme Directory Name:", value = "01-PROGRAMME", width="100%")  ),

        fillRow(  span( textOutput("warningName"), style="color:red")  ),

        fillRow(  textInput("programmeTitle", "Programme Title:", value = "01 PROGRAMME", width="100%")  )

        #fillRow( flex = c(7, 1),  verbatimTextOutput("dir", placeholder = TRUE), shinyDirButton("dir", "Select Directory", "Programme Parent Directory")  ),

        #fillRow(   span( textOutput("warningDirectory"), style="color:red")  )

      )
    )
  )
}


addin_create_programme_server <- function(input, output, session) {


    observe({
      shinyjs::disable("orgPath") # never allow orgPath be edited
    })

    # update programmeTitle when programmeName is changed:
    observe({

      updateTextInput(session, "programmeTitle", value = gsub("-", " ", gsub("_", " ", input$programmeName) )  )

    })

    # perform computations to create new Programme:
    observeEvent(input$done, {

      if(input$programmeName == "") {
        # set the warningName TextOutput:
        output$warningName <- renderText({"*** PROVIDE PROGRAMME NAME ***"})
      } else if( grepl("\\s", input$programmeName)  ) {
        # set the warningName TextOutput:
        output$warningName <- renderText({"*** PROGRAMME NAME CANNOT CONTAIN SPACES ***"})
      } else {

        #### create programme ####

        # call projectmanagr::create_programme:
        progIndexPath <- projectmanagr::create_programme(
          programmeName = input$programmeName,
          organisationPath = input$orgPath,
          programmeTitle = input$programmeTitle
        )

        # navigate to org index file & close addin:
        addin_rstudio_nav(progIndexPath)
      }
    })
}



#' Create a New Project Document Addin
#'
#' Generates a Shiny Gadget for creating a new Project Doc inside
#' a Programme.
#'
#' User selects a Programme, then destination in the file system (MUST be within
#' a Programme Dir), Project name, and Project title (for the html page).
#'
#' Stipulates any errors in the input, and can only be completed
#' when these errors have been resolved.
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
  if(orgPath=="") { addin_error_org("Create Project Document") }

  # get config templates settings yml
  confPath <- get_config_dir(orgPath)
  tempPath <- get_template_dir(orgPath)
  settings <- get_settings_yml(orgPath)

  # get status yml
  status <- get_status_yml(orgPath, settings)

  # get programe names
  programmeDirPaths <- find_prog_dirs(orgPath, settings)
  programmeDirNames <- basename(programmeDirPaths)

  # create named list of programmeDirNames
  programmeDirNamesChoices <- as.list(seq(length(programmeDirNames)))
  names(programmeDirNamesChoices) <- programmeDirNames

  # check whether the current working directory is inside a programme
  # if so determine the INDEX of this in programmeDirNames
  currentProgDir <- find_prog_dir(WD) # returns blank string if no prog dirs identified
  if( currentProgDir == "" ) { progSelected <- 1
  } else { progSelected <- grep(currentProgDir, programmeDirPaths)
  }

  # create vector of possible roots for dir selection in server()
  roots <- programmeDirPaths # can use any of the programmePaths as root
  names(roots) <- programmeDirNames


  #### user interface ####

  ui <- miniPage(

    gadgetTitleBar("Create a Project Document"),

    miniContentPanel(

      fillCol(

        fillRow( h4("Create a Project Document inside a Programme.") ),

        fillRow( code( orgPath ) ),

        fillRow( flex = c(5, 1),  verbatimTextOutput("dir", placeholder = TRUE), shinyDirButton("dir", "Select Directory", "Doc Parent Directory")  ),

        fillRow( span( textOutput("warningDirectory"), style="color:red")  ),

        fillRow(  textInput("projectPrefix", "Project Prefix:", value = "", width="25%"), textInput("projectName", "Project Name:", value = "", width="75%")  ),

        fillRow(  span( textOutput("warningName"), style="color:red")  ),

        fillRow(  textInput("projectTitle", "Project Title:", value = "", width="100%")  ),

        fillRow(   h3(textOutput("projectPathOutput"))  )

      )
    )
  )


  #### server code ####

  server <- function(input, output, session) {

    # update projectTitle when projectName is changed:
    observe({
      updateTextInput(session, "projectTitle", value = gsub("-", " ", gsub("_", " ", input$projectName) )  )
    })

    # compute Dir selection:
    global <- reactiveValues(datapath = roots[progSelected] )
    # this sets initial value of global$datapath to the currently selected programme

    # allows selection of Dir, with roots set to all progDirs
    shinyDirChoose(
      input, 'dir',
      defaultRoot = names(roots)[progSelected], # set default to identified progPath root
      roots=roots, # can use any of the identified progPaths as roots
      filetypes = c('', 'txt', 'Rmd', "tsv", "csv", "bw") # show text files
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
      if( endsWith(global$datapath, .Platform$file.sep) ) {
        global$datapath <- substr(global$datapath, 1, nchar(global$datapath)-1) # remove final file.sep!
      }
    })

    # show the global$datapath computed from input$dir in output$dir (next to shinyDirButton!)
    output$dir <- renderText({
      global$datapath
    })



    # projectPrefix error checking
    observe({

      if( grepl('[[:punct:]]', input$projectPrefix) ) {
        output$warningName <- renderText({
          "PROJECT PREFIX ONLY SUPPORTS ALPHANUMERICS"
        })
      }
      else {
        output$warningName <- renderText({
          ""
        })
      }
    })


    # projectName error checking
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


    # render path
    observe({

      if( input$projectPrefix != "" && input$projectName != "" ) {

        projPath <- paste0(global$datapath, .Platform$file.sep, input$projectPrefix,
                           settings[["ProjectPrefixSep"]], input$projectName, ".Rmd")

        output$projectPathOutput <- renderText({projPath})

      } else {

        output$projectPathOutput <- renderText({""})
      }

    })


    # create new project doc
    observeEvent(input$done, {

      if(input$projectName == "") {

        # set the warningName TextOutput:
        output$warningName <- renderText({
          "*** PROVIDE PROJECT NAME ***"
        })
      } else if( grepl("\\s", input$projectName)  ) {

        # set the warningName TextOutput:
        output$warningName <- renderText({
          "*** PROJECT NAME CANNOT CONTAIN SPACES ***"
        })
      } else if(input$projectPrefix == "") {

        # set the warningName TextOutput:
        output$warningName <- renderText({
          "*** PROVIDE PROJECT PREFIX ***"
        })
      } else if( grepl('[[:punct:]]', input$projectPrefix)  ) {

        # set the warningName TextOutput:
        output$warningName <- renderText({
          "*** PROJECT PREFIX ONLY SUPPORTS ALPHANUMERICS ***"
        })
      } else {

        # FIRST - save all open documents in RStudio:
        rstudioapi::documentSaveAll()

        #### create project doc ####

        #progPath <- programmeDirPaths[ as.integer(input$selectProg[1]) ]

        # call projectmanagr::createProjectDoc:
        projectDocRmdPath <- projectmanagr::create_project_doc(
          projectPrefix = input$projectPrefix,
          projectName = input$projectName,
          projectParentPath = global$datapath,
          projectTitle = input$projectTitle
        )

        # get project doc dir path
        projectDocDirPath <- get_project_doc_dir_path(projectDocRmdPath, settings)

        # navigate to project doc file:
        rstudioapi::navigateToFile(projectDocRmdPath)

        # navigate to containing dir
        rstudioapi::filesPaneNavigate(projectDocDirPath)

        # and set working directory
        setwd(projectDocDirPath)

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



#' Create a New Project Document Addin
#'
#' TRIED SPLITTING UI AND SERVER - but cannot get this to work with shinyDirChoose
#' So abandoning this for now...
#'
#' Generates a Shiny Gadget for creating a new Project Doc inside
#' a Programme.
#'
#' User selects a Programme, then destination in the file system (MUST be within
#' a Programme Dir), Project name, and Project title (for the html page).
#'
#' Stipulates any errors in the input, and can only be completed
#' when these errors have been resolved.
#'
addin_create_project_doc_2 <- function() {

  cat( "\nprojectmanagr::addin_create_project_doc():\n" )


  #### instance variables ####

  # identify the orgPath from current working directory - to retrieve the settings yaml file
  orgPath <- find_org_directory( getwd() )

  # if orgPath not identified present error interface and then stop this function
  if(orgPath=="") { addin_error_org("Create Project Document") }

  settings <- get_settings_yml(orgPath)
  #programmeDirPaths <- fund_prog_dirs(orgPath, settings)

  runGadget(addin_create_project_doc_ui(orgPath, settings, find_prog_dirs(orgPath, settings), get_prog_selected(find_prog_dirs(orgPath, settings))),
            addin_create_project_doc_server, viewer = addin_create_dialog_viewer("Create Project Document", settings))
}


get_prog_selected <- function(programmeDirPaths) {
  currentProgDir <- find_prog_dir(getwd()) # returns blank string if no prog dirs identified
  if( currentProgDir == "" ) { progSelected <- 1
  } else { progSelected <- grep(currentProgDir, programmeDirPaths)
  }
  progSelected
}


addin_create_project_doc_ui <- function(orgPath, settings, programmeDirPaths, progSelected) {

  miniPage(

    shinyjs::useShinyjs(), # to enable & disable subNote input

    gadgetTitleBar("Create a Project Document"),

    miniContentPanel(

      fillCol(

        fillRow( h4("Create a Project Document inside a Programme.") ),

        #fillRow( code( orgPath ) ),

        fillRow( textInput("orgPath", "Organisation Path:", value=orgPath, width="100%")),

        fillRow( flex = c(5, 1),  verbatimTextOutput("dir", placeholder = TRUE), shinyDirButton("dir", "Select Directory", "Doc Parent Directory")  ),

        fillRow( span( textOutput("warningDirectory"), style="color:red")  ),

        fillRow(  textInput("projectPrefix", "Project Prefix:", value = "", width="25%"), textInput("projectName", "Project Name:", value = "", width="75%")  ),

        fillRow(  span( textOutput("warningName"), style="color:red")  ),

        fillRow(  textInput("projectTitle", "Project Title:", value = "", width="100%")  ),

        fillRow(   h3(textOutput("projectPathOutput"))  ),

        fillRow( shinyjs::hidden( textInput("progSelected", "", value=progSelected, width="10%") ),
                 shinyjs::hidden( selectInput("programmeDirPaths", "", choices=programmeDirPaths, selected=programmeDirPaths, multiple=TRUE, width="10%") ) )

      )
    )
  )
}


addin_create_project_doc_server <- function(input, output, session) {


  ### OBSERVERS

  observe({
    shinyjs::disable("orgPath") # never allow orgPath be edited
  })


  # update projectTitle when projectName is changed:
  observe({
    updateTextInput(session, "projectTitle", value = gsub("-", " ", gsub("_", " ", input$projectName) )  )
  })

  # create named programme vector as reactive
  namedProgVec <- reactive({
    roots <- input$programmeDirPaths
    if(is.null(roots) == FALSE) { # deal with error in basename : a character vector argument expected - if NULL
      names(roots) <- basename(input$programmeDirPaths)
    }
    roots
  })

  # create prog selected integer as reactive
  progSelectedInt <- reactive({
    as.integer(input$progSelected)
  })

  # compute Dir selection:
  #global <- reactiveValues(datapath = "" )
  global <- reactive({reactiveValues(datapath = namedProgVec()[progSelectedInt()] ) })
  # this sets initial value of global$datapath to the currently selected programme

  # allows selection of Dir, with roots set to all progDirs
  dirChoose <- reactive({
    shinyDirChoose(input, 'dir',
    defaultRoot = names(namedProgVec())[progSelectedInt()], # set default name to identified progPath root
    roots=namedProgVec(), # can use any of the identified progPaths as roots - named vector
    filetypes = c('', 'txt', 'Rmd', "tsv", "csv", "bw") # show text files
      )
    })

  dir <- reactive(input$dir)
  # observe({ cat('\n  input$dir: _', input$dir[[1]], '_\n') }) this causes an error when input$dir becomes a list
  # so check in the observeEvent() function below


  # update global$datapath
  observeEvent(ignoreNULL = TRUE,
               eventExpr = { # if input$dir is changed
                 input$dir },
               handlerExpr = { # update datapath with dir() list
                 if (!"path" %in% names(dir())) return() # check the path element exists in dir
                 cat("\n dir() names: ", names(dir())) # contains : root, path
                 cat("\n  dir$root: ", dir()$root) # name of the root selected in shinyDirChoose
                 cat("\n  dir$path: _", unlist( dir()$path ), "_" ) # list of each dir in dirTree, separated by space?
                 cat("\n  dir$path pasted with fileSep: _", paste( unlist( dir()$path ), collapse = .Platform$file.sep ), "_" )
                 # list of each dir in dirTree created into a path
                 cat("\n  dir$path[-1]: _", unlist( dir()$path[-1] ), "_" ) # list of each dir in dirTree, separated by space?
                 cat("\n  dir$path[-1] pasted with fileSep: _", paste( unlist( dir()$path[-1] ), collapse = .Platform$file.sep ), "_" )
                 cat("\n  namedProgVec: ", namedProgVec())
                 cat("\n  namedProgVec()[[dir()$root]]")
                 # list of each dir in dirTree created into a path
                 global$datapath <- file.path( # form path with
                   namedProgVec()[[dir()$root]], # shinyDirChoose selected ROOT (selected by its NAME found in dir()$root)
                   paste( unlist( dir()$path[-1] ), collapse = .Platform$file.sep )  ) # shinyDirChoose selected PATH with file.sep added
               })

  observe({ cat('\n  global$datapath: _', global$datapath, '_\n') })


  observe({ # deal with Warning: Error in endsWith: non-character object(s) in global$datapath - if NULL
    if( is.null(global$datapath) == FALSE && endsWith(global$datapath, .Platform$file.sep) ) {
      global$datapath <- substr(global$datapath, 1, nchar(global$datapath)-1) # remove final file.sep!
    }
  })

  # show the global$datapath computed from input$dir in output$dir (next to shinyDirButton!)
  output$dir <- renderText({global$datapath})


  # projectPrefix error checking
  observe({

    if( grepl('[[:punct:]]', input$projectPrefix) ) {output$warningName <- renderText({"PROJECT PREFIX ONLY SUPPORTS ALPHANUMERICS"})
    } else {output$warningName <- renderText({""})
    }
  })

  # projectName error checking
  observe({

    if( grepl("\\s", input$projectName)  ) {output$warningName <- renderText({"PROJECT NAME CANNOT CONTAIN SPACES"})
    } else { output$warningName <- renderText({""})
    }
  })


  # render path
  observe({

    if( input$projectPrefix != "" && input$projectName != "" ) {

      projPath <- paste0(global$datapath, .Platform$file.sep, input$projectPrefix,
                         settings[["ProjectPrefixSep"]], input$projectName, ".Rmd")

      output$projectPathOutput <- renderText({projPath})

    } else {

      output$projectPathOutput <- renderText({""})
    }

  })


  # create new project doc
  observeEvent(input$done, {

    if(input$projectName == "") {
      # set the warningName TextOutput:
      output$warningName <- renderText({"*** PROVIDE PROJECT NAME ***"})
    } else if( grepl("\\s", input$projectName)  ) {
      # set the warningName TextOutput:
      output$warningName <- renderText({"*** PROJECT NAME CANNOT CONTAIN SPACES ***"})
    } else if(input$projectPrefix == "") {
      # set the warningName TextOutput:
      output$warningName <- renderText({"*** PROVIDE PROJECT PREFIX ***"})
    } else if( grepl('[[:punct:]]', input$projectPrefix)  ) {
      # set the warningName TextOutput:
      output$warningName <- renderText({"*** PROJECT PREFIX ONLY SUPPORTS ALPHANUMERICS ***"})
    } else {

      # FIRST - save all open documents in RStudio:
      if( rstudioapi::isAvailable() ) { rstudioapi::documentSaveAll() }

      #### create project doc ####

      #progPath <- programmeDirPaths[ as.integer(input$selectProg[1]) ]

      projectDocRmdPath <- create_project_doc(
        projectPrefix = input$projectPrefix, projectName = input$projectName,
        projectParentPath = global$datapath, projectTitle = input$projectTitle )

      # navigate to org index file & close addin:
      addin_rstudio_nav(projectDocRmdPath)

    }
  })
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

  # get config templates settings yml
  confPath <- get_config_dir(orgPath)
  tempPath <- get_template_dir(orgPath)
  settings <- get_settings_yml(orgPath)

  # get status yml
  status <- get_status_yml(orgPath, settings)

  # get progPath
  progPath <- find_prog_dir(selection$filePath)


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
    addin_error_path(
      "Add Project Note",
      "  Simple Project Note selected - unsupported selection. Select a project doc GDT to add new Simple Project Note.",
      selection$filePath, settings)
    stop( paste0("  Simple Project Note selected - unsupported selection. Select a project doc GDT to add new Simple Project Note: \n    ", selection$filePath))

    #addin_create_project_note_from_single(selection, settings)

  } else if( selection$rmdType == "UNKNOWN" ) {

    addin_error_path(
      "Add Project Note",
      "No Valid Rmd file selected - ensure a Project Doc or Project Note is the active document.",
      selection$filePath, settings)
    stop( paste0("  No Valid Rmd file selected - ensure a Project Doc or Project Note is the active document: \n    ", selection$filePath))

  }

}


#' Add a single or group note to doc at selected GDT
#'
addin_create_project_note_from_doc_gdt <- function(selection, settings, orgPath) {


  #### instance variables ####

  # get projectDir & progDir
  projectDirPath <- get_project_doc_dir_path(selection[["filePath"]], settings)
  progPath <- find_prog_dir(selection[["filePath"]])

  # create vector of possible roots for dir selection in server()
  roots <- c(projectDirPath, progPath, orgPath) # can use projectDirPath, progPath, or orgPath as roots
  names(roots) <- c(basename(projectDirPath), basename(progPath), basename(orgPath))

  # extract GDT titles from selection
  goalTitle <- get_goal_title(selection[["goal"]], settings)
  delTitle <- get_deliverable_title(selection[["deliverable"]], settings)
  taskTitle <- get_deliverable_title(selection[["task"]], settings)


  #### user interface ####

  ui <- miniPage(

    shinyjs::useShinyjs(), # to enable & disable subNote input

    gadgetTitleBar("Add New Project Note"),

    miniContentPanel(

      fillCol( #flex=NA, # use the natural size of the elements in col

        fillRow( h5("Add a new Project Note to a Project Document.") ),

        fillRow(
          helpText(  h3("GOAL", align="center")   ),
          helpText(  h3("DELIVERABLE", align="center")   ),
          helpText(  h3("TASK", align="center")   )
        ),

        fillRow(
          helpText( p(goalTitle, align="center") ),
          helpText( p(delTitle, align="center") ),
          helpText( p(taskTitle, align="center") )
        ),

        fillRow(   span( textOutput("warningDirectory"), style="color:red")  ),

        fillRow( flex = c(5, 1),  verbatimTextOutput("dir", placeholder = TRUE), shinyDirButton("dir", "Select Directory", "Note Parent Directory")  ),

        fillRow(  selectInput("prefixType", "Select Project Note Type:",
                              choices = list("Single" = 1, "Group" = 2),
                              selected = 1, width = '50%')  ),

        fillRow( br() ),

        fillRow(  span( textOutput("warningName"), style="color:red")  ),

        fillRow(  textInput("projectNoteName", "Project Note Name:", value = "Note_Name", width='95%'),
                  textInput("projectNoteTitle", "Project Note Title:", value = "Note Title", width='95%')   ),

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
    global <- reactiveValues(datapath = selection$defaultPath )
    # this sets initial value of global$datapath to selections defaultPath (either projDocDir or selected note pir)

    # allows selection of Dir, with roots set to project doc DIR or ORG Dir
    shinyDirChoose(
      input, 'dir',
      defaultRoot = names(roots)[1], # set default to first root
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



#' Add new note from existing single note - inheriting all note GDTs
#'
addin_create_project_note_from_single <- function(selection, settings, orgPath) {


  #### user interface ####

  ui <- miniPage(

    gadgetTitleBar("Add New Project Note"),

    miniContentPanel(

      fillCol( #flex=NA, # use the natural size of the elements in col

        fillRow( h5("Add a new Project Note for existing Note : Inherit all GDT Links.") ),

        fillRow( h5("Inherit Goal/Del/Tasks from NOTE:") ),
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

        projNotePath <- get_project_note_path(
          dirname(selection[["filePath"]]), # current project note parent dir
          input$projectNoteName,
          settings)
        output$projectNotePath <- renderText({ projNotePath })

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


        #### Add SINGLE note ####

        # compute new single note path
        projNotePath <- get_project_note_path(
          dirname(selection[["filePath"]]), # current project note parent dir
          input$projectNoteName,
          settings)

        # get all GDTs from selected project note

        # form FIRST selection

        # add single note
        create_project_note(
          projectNoteName = input$projectNoteName,
          projectNotePath = dirname(projNotePath),
          selection = selection,
          projectNoteTitle = input$projectNoteTitle
        )

        # add all links to remaining GDTs from selected project note

        # open new subnote
        rstudioapi::navigateToFile(projNotePath)

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



#' Create New Content Addin
#'
#' Shiny Gadget generated to create new Insertable Content in a Project Note
#' at the currently selected line.
#'
#' Content is any templated insertable text for boilerplate content.  Typically
#' in ProjectManage it is used to define a set of Standard Operating
#' Procedures (SOPs) & LOG Sections that record Protocol execution.  The
#' Content is defined between specific delimiters, defined in
#' `CONTENT_SEP.txt`: by default the delimiter is a series of `====`.
#'
#' User defines the following in the gadget interface:
#'
#' * `contentName` : A descriptive name for the block of insertable content.
#'
#' * `contentDescription` : A concise description of the block of insertable
#'   content - what will this content be used for?
#'
#' Stipulates any errors in the input, and can only be completed
#' when these errors have been resolved.
#'
#' This addin calls `create_content()` with appropriate ARGs.
#'
#' @export
addin_create_content <- function() {

  cat( "\nprojectmanagr::addin_create_content():\n" )


  #### instance variables ####

  # Retrieve cursor_selection from current Active Doc in rstudio:
  selection <- cursor_selection() # Project Doc, Project Note - Simple, Header, SubNote

  if(selection[["rmdType"]] == "UNKNOWN") { # return error message from selection
    addin_error("Add New Content", selection[["errorMessage"]])
    stop( paste0("  ", selection[["errorMessage"]]))
  }

  if(selection[["rmdType"]] == "DOC") { # return error message from selection
    addin_error("Add New Content", "Selection is a Project Doc - can only add content to Project Notes..")
    stop( paste0("  ", selection[["errorMessage"]]))
  }


  # get the orgPath:
  orgPath <- find_org_directory(selection$filePath)

  # if orgPath not identified present error interface and then stop this function
  if(orgPath=="") {
    addin_error_path("Add New Content",
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

  context <- rstudioapi::getSourceEditorContext()
  cursor <- rstudioapi::primary_selection(context)

  # create vector of possible roots for dir selection in server()
  roots <- c(get_project_note_dir_path(selection$filePath, settings), orgPath) # use selected project note dir + orgPath as roots
  names(roots) <- basename(roots)


  #### user interface ####

  ui <- miniPage(

    gadgetTitleBar("Add New Content"),

    miniContentPanel(

      fillCol(

        fillRow( h5("Add a new Content in a Project Note") ),

        #fillRow( p("Content Name MUST end with _Content:") ),

        fillRow( flex = c(5, 1),  verbatimTextOutput("dir", placeholder = TRUE), shinyDirButton("dir", "Select Directory for Content", "Note Parent Directory")  ),

        fillRow( span( textOutput("warningDirectory"), style="color:red")  ),

        fillRow(  textInput("contentName", "Content Name:", value = "", width="50%"),
                  textInput("contentTitle", "Content Title:", value = "", width="50%")  ),

        fillRow(  span( textOutput("warningName"), style="color:red")  ),

        fillRow(  textAreaInput("contentDescription", "Content Description:", value='This Content specifies how to ', rows=3, width="100%")  ),

        fillRow(   span( textOutput("warningDescription"), style="color:red")  ),

        fillRow(   textOutput("contentPath")  )

      )
    )
  )


  #### server function ####

  server <- function(input, output, session) {

    # update contentTitle when contentName is changed:
    observe({
      updateTextInput(session, "contentTitle", value = gsub("-", " ", gsub("_", " ", input$contentName) )  )
    })

    # compute Dir selection:
    global <- reactiveValues(datapath = roots[1] )
    # this sets initial value of global$datapath to the currently selected note dir

    # allows selection of Dir, with roots set to all progDirs
    shinyDirChoose(
      input, 'dir',
      defaultRoot = names(roots)[1], # set default to selected note dir
      roots=roots, # can use any roots in this vector - only selected note dir!
      filetypes = c('', 'txt', 'Rmd', "tsv", "csv", "bw") # show text files
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
      if( endsWith(global$datapath, .Platform$file.sep) ) {
        global$datapath <- substr(global$datapath, 1, nchar(global$datapath)-1) # remove final file.sep!
      }
    })

    # show the global$datapath computed from input$dir in output$dir (next to shinyDirButton!)
    output$dir <- renderText({
      global$datapath
    })


    # render contentPath
    observe({

      if( input$contentName != "" ) {

        contentRmdPath <- fs::path(global$datapath, input$contentName, paste0(input$contentName, ".Rmd"))
        output$contentPath <- renderText({contentRmdPath})

      } else {

        output$contentPath <- renderText({""})
      }

    })


    # create new content in note
    observeEvent(input$done, {

      if(input$contentName == "") {
        output$warningName <- renderText({"*** PROVIDE CONTENT NAME ***"})

      } else if( input$contentDescription == 'This Content specifies how to '  ) {
        output$warningDescription <- renderText({"*** PROVIDE CONTENT DESCRIPTION ***"})

      } else {

        # FIRST - save all open documents in RStudio:
        rstudioapi::documentSaveAll()

        #### create content ####
        contentRmdPath <- projectmanagr::create_content(
          selection = selection,
          contentName = input$contentName,
          contentDescription = input$contentDescription,
          contentSourcePath = global$datapath,
          contentTitle = input$contentTitle,
          contentDeclarationTemplate="Content-Declaration-Template.Rmd",
          contentSourceTemplate="Content-Source-Template.Rmd"
        )

        # get content dir path
        contentDirPath <- dirname(contentRmdPath)

        # navigate to content file:
        rstudioapi::navigateToFile(contentRmdPath)

        # navigate to containing dir
        rstudioapi::filesPaneNavigate(contentDirPath)

        # and set working directory
        setwd(contentDirPath)

        # close Gadget after computations are complete
        stopApp()

      }
    })
  }


  #### view gadget ####

  viewer <- dialogViewer("Add New Content",
                         width = settings[["GadgetWidth"]],
                         height = settings[["GadgetHeight"]])

  runGadget(ui, server, viewer = viewer)

}


