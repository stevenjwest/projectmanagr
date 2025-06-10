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

    tags$style(HTML( get_css_theme() )),  # Apply the theme-specific CSS: dark or light

    gadgetTitleBar("Create New Project Org"),

    miniContentPanel(

      fillCol(

        fillRow( h5("Initialise a new Project Organisation in Selected Directory.") ),

        fillRow(  textInput("organisationName",
                            "Organisation Directory Name:",
                            value = "00_ORG", width="100%")  ),

        fillRow(  span( textOutput("warning"), style="color:red")  ),

        fillRow(  textInput("organisationTitle",
                            "Organisation Title:",
                            value = "ORGANISATION", width="100%")  ),

        fillRow(   span( textOutput("warning2"), style="color:red")  ),

        #fillRow(  textInput("authorName", "Author Name:", value = "", width="100%")  ),

        #fillRow(   span( textOutput("warning3"), style="color:red")  ),

        fillRow( flex = c(6, 1),
                 verbatimTextOutput("dirO", placeholder = TRUE),
                 shinyDirButton("dir", "Select Directory",
                                "Organisation Parent Directory")  )

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
  global <- reactiveValues(datapath = rootDir, filepath = rootDir )
  #global <- reactiveValues(datapath = fs::path_expand("~") )

  # encode functionality for shinyDirButton - select from rootDir
  shinyDirChoose(
    input,
    'dir',
    roots = c(home = rootDir),
    filetypes = c('', 'txt', 'bigWig', "tsv", "csv", "bw")
  )

  dir <- reactive(input$dir)

  # alter global$datapath to dir selection if dir is changed
  observeEvent(ignoreNULL = TRUE,
               eventExpr = { input$dir },
               handlerExpr = { if (!"path" %in% names(dir())) return()
                 #home <- fs::path_expand("~")
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
      orgIndexPath <- create_project_org(
        orgParentPath = global$datapath,
        orgName = input$organisationName,
        orgTitle = input$organisationTitle,
        settingsYamlPath = "",
        orgTemplate="Org-Template.Rmd" )

      # navigate to org index file & close addin:
      addin_rstudio_nav(orgIndexPath)
    }
  })
}#### ________________________________ ####


#' Create a New Programme Addin
#'
#' Generates a Shiny Gadget for creating a new Programme inside
#' a Project Organisation.  User selects a destination in
#' the file system, Programme name (for the directory),
#' and Programme title (for the html page).
#'
#' @export
addin_create_programme <- function() {

  cat( "\nprojectmanagr::addin_create_programme():\n" )


  #### instance variables ####

  # identify the orgPath from current working directory - to retrieve the settings yaml file
  orgPath <- find_org_directory( getwd() )

  # if orgPath not identified present error interface and then stop this function
  if(orgPath=="") { addin_error_org("Create Programme") }

  settings <- get_settings_yml(orgPath) # for gadget width & height

  #### RUN GADGET ####

  runGadget(addin_create_programme_ui(orgPath), addin_create_programme_server, viewer = addin_create_dialog_viewer("Create a Programme", settings))

}


addin_create_programme_ui <- function(orgPath) {

  miniPage(

    tags$style(HTML( get_css_theme() )),  # Apply the theme-specific CSS: dark or light

    shinyjs::useShinyjs(), # to enable & disable subNote input

    gadgetTitleBar("Create a Programme"),

    miniContentPanel(

      fillCol(

        fillRow( h3("Create a new Programme inside the Project Organisation") ),

        #fillRow( code( orgPath ) ),

        fillRow( textInput("orgPath",
                           "Organisation Path:",
                           value=orgPath, width="100%")),

        fillRow(  textInput("programmeName",
                            "Programme Directory Name:",
                            value = "01-PROGRAMME", width="100%")  ),

        fillRow(  span( textOutput("warningName"), style="color:red")  ),

        fillRow(  textInput("programmeTitle",
                            "Programme Title:",
                            value = "01 PROGRAMME", width="100%")  )
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

      if(input$programmeName == "") { output$warningName <- renderText({"*** PROVIDE PROGRAMME NAME ***"})
      } else if( grepl("\\s", input$programmeName)  ) { output$warningName <- renderText({"*** PROGRAMME NAME CANNOT CONTAIN SPACES ***"})
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
}#### ________________________________ ####


#' Create a New Programme Section Addin
#'
#' Generates a Shiny Gadget for creating a new Programme Section inside
#' a Programme.  User selects a destination in
#' the file system, Programme Section name (for the directory),
#' and Programme Section title (for the html page).
#'
#' @export
addin_create_programme_section <- function() {

  cat( "\nprojectmanagr::addin_create_programme_section():\n" )


  #### instance variables ####

  # identify the orgPath from current working directory - to retrieve the settings yaml file
  orgPath <- find_org_directory( getwd() )

  # if orgPath not identified present error interface and then stop this function
  if(orgPath=="") { addin_error_org("Create Programme Section") }

  settings <- get_settings_yml(orgPath) # for gadget width & height

  #### RUN GADGET ####

  runGadget(addin_create_programme_section_ui(orgPath),
            addin_create_programme_section_server,
            viewer = addin_create_dialog_viewer("Create a Programme Section", settings))

}


addin_create_programme_section_ui <- function(orgPath) {

  miniPage(

    tags$style(HTML( get_css_theme() )),  # Apply the theme-specific CSS: dark or light

    shinyjs::useShinyjs(), # to enable & disable subNote input

    gadgetTitleBar("Create a Programme Section"),

    miniContentPanel(

      fillCol(

        fillRow( h3("Create a new Programme Section inside the Project Organisation") ),

        fillRow( textInput("orgPath",
                           "Organisation Path:",
                           value=orgPath, width="100%")),

        fillRow( flex = c(5, 1),
                 verbatimTextOutput("dir", placeholder = TRUE),
                 shinyDirButton("dir", "Select Directory", "Doc Parent Directory")  ),

        fillRow( span( textOutput("warningDirectory"), style="color:red")  ),

        fillRow(  textInput("sectionName",
                            "Programme Section Directory Name:",
                            value = "programme-section", width="100%")  ),

        fillRow(  span( textOutput("warningName"), style="color:red")  ),

        fillRow(  textInput("sectionTitle",
                            "Programme Section Title:",
                            value = "programme section", width="100%")  )
        #fillRow( flex = c(7, 1),  verbatimTextOutput("dir", placeholder = TRUE), shinyDirButton("dir", "Select Directory", "Programme Parent Directory")  ),
        #fillRow(   span( textOutput("warningDirectory"), style="color:red")  )
      )
    )
  )
}


addin_create_programme_section_server <- function(input, output, session) {

  orgPath <- find_org_directory( getwd() )
  settings <- get_settings_yml(orgPath)
  programmeDirPaths <- find_prog_dirs(orgPath, settings)
  programmeDirNames <- basename(programmeDirPaths)

  # create named list of programmeDirNames
  programmeDirNamesChoices <- as.list(seq(length(programmeDirNames)))
  names(programmeDirNamesChoices) <- programmeDirNames

  # check whether the current working directory is inside a programme
  # if so determine the INDEX of this in programmeDirNames
  currentProgDir <- find_prog_dir(getwd()) # returns blank string if no prog dirs identified
  if( currentProgDir == "" ) {
    progSelected <- 1
  } else {
    progSelected <- grep(currentProgDir, programmeDirPaths)
  }

  # create vector of possible roots for dir selection in server()
  roots <- programmeDirPaths # can use any of the programmePaths as root
  names(roots) <- programmeDirNames

  observe({
    shinyjs::disable("orgPath") # never allow orgPath be edited
  })

  # update sectionTitle when sectionName is changed:
  observe({

    updateTextInput(session, "sectionTitle",
                    value = gsub("-", " ", gsub("_", " ", input$sectionName) )  )

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
                 global$datapath <- fs::path(
                   roots[[basename(dir()$root)]],
                   paste(unlist(dir()$path[-1]), collapse = .Platform$file.sep) )
                 # basename of dir()$root can be used to selected the path in named list roots
                 # selected by its NAME found in dir()$root)
                 # shinyDirChoose selected PATH with file.sep added
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


  # perform computations to create new Programme Section:
  observeEvent(input$done, {

    if(input$sectionName == "") {
      output$warningName <- renderText({"*** PROVIDE PROGRAMME SECTION NAME ***"})
    } else if( grepl("\\s", input$sectionName)  ) {
      output$warningName <- renderText({"*** PROGRAMME SECTION NAME CANNOT CONTAIN SPACES ***"})
    } else {

      # FIRST - save all open documents in RStudio:
      .save_all_doc()

      #### create programme section ####

      cat('\n  sectionName: _', input$sectionName, '_\n')
      cat('\n  sectionParentPath: _', global$datapath, '_\n')
      cat('\n  sectionTitle: _', input$sectionTitle, '_\n')

      # call projectmanagr::create_programme_section:
      sectIndexPath <- projectmanagr::create_programme_section(
        sectionName = input$sectionName,
        sectionParentPath = global$datapath,
        sectionTitle = input$sectionTitle
      )

      # navigate to org index file & close addin:
      addin_rstudio_nav(sectIndexPath)
    }
  })
}#### ________________________________ ####

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

  set_wd_active_doc() # ensure the currently active file's path is used to find org & prog

  #### instance variables ####

  # identify the orgPath from current working directory - to retrieve the settings yaml file
  orgPath <- find_org_directory( getwd() )

  # if orgPath not identified present error interface and then stop this function
  if(orgPath=="") { addin_error_org("Create Project Document") }

  settings <- get_settings_yml(orgPath)

  #### RUN GADGET ####

  #addin_create_project_doc_ui(orgPath, settings, find_prog_dirs(orgPath, settings), get_prog_selected(find_prog_dirs(orgPath, settings)))
  runGadget(addin_create_project_doc_ui(orgPath, settings),
            addin_create_project_doc_server,
            viewer = addin_create_dialog_viewer("Create Project Document", settings))
}


get_prog_selected <- function(programmeDirPaths) {
  currentProgDir <- find_prog_dir(getwd()) # returns blank string if no prog dirs identified
  if( currentProgDir == "" ) { progSelected <- 1
  } else { progSelected <- grep(currentProgDir, programmeDirPaths)
  }
  progSelected
}

#' User Interface for Create Project Doc shiny gadget
#'
#' Passes some important instance variables:
#'
#' @param orgPath The Organisation Path - derived from the current working directory.
#'
#' @param settings
#'
addin_create_project_doc_ui <- function(orgPath, settings) {

  #addin_create_project_doc_ui <- function(orgPath, settings, programmeDirPaths, progSelected) {
  miniPage(

    tags$style(HTML( get_css_theme() )),  # Apply the theme-specific CSS: dark or light

    shinyjs::useShinyjs(), # to enable & disable programme selection

    gadgetTitleBar("Create a Project Document"),

    miniContentPanel(

      fillCol(

        fillRow( h4("Create a Project Document inside a Programme.") ),

        #fillRow( code( orgPath ) ),

        fillRow( textInput("orgPath",
                           "Organisation Path:",
                           value=orgPath, width="100%")),

        fillRow( flex = c(5, 1),
                 verbatimTextOutput("dir", placeholder = TRUE),
                 shinyDirButton("dir", "Select Directory", "Doc Parent Directory")  ),

        fillRow( span( textOutput("warningDirectory"), style="color:red")  ),

        fillRow(  textInput("projectPrefix",
                            "Project Prefix:",
                            value = "", width="25%"),
                  textInput("projectName",
                            "Project Name:",
                            value = "", width="75%")  ),

        fillRow(  span( textOutput("warningName"), style="color:red")  ),

        fillRow(  textInput("projectTitle",
                            "Project Title:",
                            value = "", width="100%")  ),

        fillRow(   h3(textOutput("projectPathOutput"))  )#,
      )
    )
  )
}



addin_create_project_doc_server <- function(input, output, session) {

  orgPath <- find_org_directory( getwd() )
  settings <- get_settings_yml(orgPath)
  programmeDirPaths <- find_prog_dirs(orgPath, settings)
  programmeDirNames <- basename(programmeDirPaths)

  # create named list of programmeDirNames
  programmeDirNamesChoices <- as.list(seq(length(programmeDirNames)))
  names(programmeDirNamesChoices) <- programmeDirNames

  # check whether the current working directory is inside a programme
  # if so determine the INDEX of this in programmeDirNames
  currentProgDir <- find_prog_dir(getwd()) # returns blank string if no prog dirs identified
  if( currentProgDir == "" ) {
    progSelected <- 1
  } else {
    progSelected <- grep(currentProgDir, programmeDirPaths)
  }

  # create vector of possible roots for dir selection in server()
  roots <- programmeDirPaths # can use any of the programmePaths as root
  names(roots) <- programmeDirNames

  # update projectTitle when projectName is changed:
  observe({
    updateTextInput(session, "projectTitle",
                    value = gsub("-", " ", gsub("_", " ", input$projectName) )  )
  })

  # compute Dir selection:
  #global <- reactiveValues(datapath = roots[progSelected] )
  # this sets initial value of global$datapath to the currently selected programme
  global <- reactiveValues(datapath = getwd() )
  # this sets initial value of global$datapath to the current wd - active doc wd

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
                 global$datapath <- fs::path(
                   roots[[basename(dir()$root)]],
                   paste(unlist(dir()$path[-1]), collapse = .Platform$file.sep) )
                  # basename of dir()$root can be used to selected the path in named list roots
                   # selected by its NAME found in dir()$root)
                  # shinyDirChoose selected PATH with file.sep added
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
      .save_all_doc()

      #### create project doc ####

      #progPath <- programmeDirPaths[ as.integer(input$selectProg[1]) ]

      # call projectmanagr::createProjectDoc:
      projectDocRmdPath <- projectmanagr::create_project_doc(
        projectPrefix = input$projectPrefix,
        projectName = input$projectName,
        projectParentPath = global$datapath,
        projectTitle = input$projectTitle
      )

      # navigate to org index file & close addin:
      addin_rstudio_nav(projectDocRmdPath)
    }
  })
}#### ________________________________ ####


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

  # extract GDT titles from selection
  goalTitle <- get_goal_title(selection[["goal"]], settings)
  delTitle <- get_deliverable_title(selection[["deliverable"]], settings)
  taskTitle <- get_deliverable_title(selection[["task"]], settings)

  #### RUN GADGET ####

  runGadget(addin_create_prn_doc_gdt_ui(goalTitle, delTitle, taskTitle),
            addin_create_prn_doc_gdt_server, viewer = addin_create_dialog_viewer("Add New Project Note", settings))

}


addin_create_prn_doc_gdt_ui <- function(goalTitle, delTitle, taskTitle) {

  ui <- miniPage(

    tags$style(HTML( get_css_theme() )),  # Apply the theme-specific CSS: dark or light

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
                              selected = 1, width = '50%'),
                  checkboxInput("addObjToHeader", 'Add Objective to Header', width='50%') ),

        fillRow( br() ),

        fillRow(  span( textOutput("warningName"), style="color:red")  ),

        fillRow(  textInput("projectNoteName", "Project Note Name:", value = "Note_Name", width='95%'),
                  textInput("projectNoteTitle", "Project Note Title:", value = "Note Title", width='95%')   ),

        fillRow( br() ),

        fillRow(   textOutput("projectNotePath")  ),

        fillRow(  textInput("subNoteName", "Project SubNote Name:", width='95%'),
                  textInput("subNoteTitle", "Project SubNote Title:", width='95%')  ),

        fillRow( br() ),

        fillRow(   textOutput("subNotePath")  ),

      ),
      padding = 10
    )
  )
  ui # return
}


addin_create_prn_doc_gdt_server <- function(input, output, session) {

  # initialise variables
  selection <- cursor_selection() # Project Doc, Project Note - Simple, Header, SubNote
  #cat("selection: ", paste(selection, collapse=' '))
  orgPath <- find_org_directory(selection$filePath)
  settings <- get_settings_yml(orgPath)
  projectDirPath <- get_project_doc_dir_path(selection[["filePath"]], settings)
  progPath <- find_prog_dir(selection[["filePath"]])
  # create vector of possible roots for dir selection in server()
  roots <- c(projectDirPath, progPath, orgPath) # can use projectDirPath, progPath, or orgPath as roots
  names(roots) <- c(basename(projectDirPath), basename(progPath), basename(orgPath))


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

        # navigate to org index file & close addin:
        addin_rstudio_nav(projNotePath)

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

#' Add new subnote under group note - inheriting all group note GDTs
#'
addin_create_subnote_from_group <- function(selection, settings, orgPath) {


  #### user interface ####

  ui <- miniPage(

    tags$style(HTML( get_css_theme() )),  # Apply the theme-specific CSS: dark or light

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

    tags$style(HTML( get_css_theme() )),  # Apply the theme-specific CSS: dark or light

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

    tags$style(HTML( get_css_theme() )),  # Apply the theme-specific CSS: dark or light

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

    tags$style(HTML( get_css_theme() )),  # Apply the theme-specific CSS: dark or light

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

}#### ________________________________ ####


#' Addin to Open Daily Journal
#'
#' A Shiny gadget that displays the current ORG directory path and a calendar
#' interface. The user can select a date, which determines the day for the
#' journal. If a journal for the selected day already exists, it will
#' be opened. Otherwise, a new journal is created using the
#' `create_daily_journal()` function.
#'
#' @details
#' The function uses the current working directory of the active document to
#' identify the ORG directory and settings. The user is presented with a
#' calendar interface to select a date. If the ORG directory is not found,
#' an error is displayed, and the function terminates.
#'
#' Internally, the function:
#' - Ensures the working directory is set to the active document's path.
#' - Finds the ORG directory.
#' - Retrieves settings from a YAML file in the ORG directory.
#' - Displays a Shiny gadget with the appropriate UI and server components.
#'
#' @seealso
#' - \code{\link{create_daily_journal}} for creating journal files.
#' - \code{\link{find_org_directory}} for locating the ORG directory.
#' - \code{\link{get_settings_yml}} for retrieving settings from a YAML file.
#' - \code{\link{addin_error_org}} for handling ORG-related errors.
#'
#' @examples
#' # Open the daily journal gadget:
#' projectmanagr::addin_open_daily_journal()
#'
#' @note
#' This function is intended to be used interactively as an RStudio Addin.
#'
#' @export
addin_open_daily_journal <- function() {

  cat("\nprojectmanagr::addin_open_daily_journal():\n")

  # Set working directory to the active document's path
  set_wd_active_doc()

  #### Instance Variables ####

  # Locate the ORG directory based on the current working directory
  orgPath <- find_org_directory(getwd())

  # Handle error if ORG directory is not found
  if (orgPath == "") {
    addin_error_org("Open Daily Journal")
  }

  # Retrieve settings from the YAML file in the ORG directory
  settings <- get_settings_yml(orgPath)

  #### Run Shiny Gadget ####

  runGadget(
    addin_open_daily_journal_ui(orgPath), addin_open_daily_journal_server, viewer = addin_create_dialog_viewer("Open Daily Journal", settings)
  )
}



#' UI for Addin: Open Daily Journal
#'
#' Constructs the UI for the "Open Daily Journal" Shiny gadget.
#' Displays the organisation path, a calendar for selecting a date, and
#' highlights the day corresponding to the selected date.
#'
#' @param orgPath Character string. The path to the ORG directory where journal
#'   files are stored.
#' @param calDate Date object. The default date to display in the calendar.
#'   Defaults to the current system date (\code{Sys.Date()}).
#' @param calendar_function Function. The calendar rendering function to use.
#'   Defaults to \code{shiny.fluent::Calendar.shinyInput}.
#'
#' @details
#' This function creates a user interface for selecting a date using a calendar
#' component. The selected date determines the journal to open or create.
#' The organisation path is displayed prominently for user context.
#'
#' Features:
#' - Automatically listens for the "Enter" key to trigger the Done button.
#' - Highlights the current and selected months in the calendar.
#' - Displays the selected day dynamically.
#'
#' The function uses \code{miniUI} to create a compact, user-friendly layout
#' suitable for Shiny gadgets.
#'
#' @return
#' A \code{miniUI::miniPage} object representing the UI for the gadget.
#'
#' @examples
#' # Example usage:
#' ui <- addin_open_daily_journal_ui(
#'   orgPath = "/path/to/org",
#'   calDate = as.Date("2024-01-01"),
#'   calendar_function = shiny.fluent::Calendar.shinyInput
#' )
#'
#' @seealso
#' - \code{\link[miniUI]{miniPage}} for details on constructing UI components
#'   for Shiny gadgets.
#' - \code{\link[shiny.fluent]{Calendar.shinyInput}} for the calendar component.
#'
#' @note
#' This UI is designed for use with the server logic defined in
#' \code{addin_open_daily_journal_server}.
#'
addin_open_daily_journal_ui <- function(
    orgPath,
    calDate = Sys.Date(),
    calendar_function = shiny.fluent::Calendar.shinyInput) {

    miniUI::miniPage(

      tags$style(HTML( get_css_theme() )),  # Apply the theme-specific CSS: dark or light

      # JavaScript to listen for Enter key and trigger Done button
      tags$script(HTML("
        $(document).on('keydown', function(e) {
          if (e.key === 'Enter') {
            $('#done').click(); // Trigger the Done button
          }
        });
      ")),
      miniUI::gadgetTitleBar("Open Daily Journal: Select a Day"),
      miniUI::miniContentPanel(
        h4(paste0("Organisation Path:  ", orgPath), align = "center"),
        div(
          style = "padding: 20px;",
          calendar_function(
            "calendar", value = calDate, showGoToToday = TRUE,
            highlightCurrentMonth = TRUE, highlightSelectedMonth = TRUE, firstDayOfWeek = 1
          ),
          align = "center"
        ),
        br(),
        verbatimTextOutput("selected_day_text")
    )
  )
}


#' Server Logic for Addin: Open Daily Journal
#'
#' Implements the server logic for the "Open Daily Journal" Shiny gadget.
#' Handles date selection from the calendar, determines the selected day,
#' and creates or navigates to the journal file as needed.
#'
#' @param input Reactive input object. Contains inputs from the Shiny gadget UI, including the selected date from the calendar (\code{input$calendar}) and the "Done" button action (\code{input$done}).
#' @param output Reactive output object. Used to render outputs in the UI, such as the text displaying the selected day (\code{output$selected_day_text}).
#' @param session Reactive session object. Used to manage the Shiny gadget's session.
#'
#' @details
#' This function processes user interactions with the "Open Daily Journal" gadget:
#' - Monitors the calendar input for date selection.
#' - Calculates the date selected.
#' - Displays the calculated date in the UI.
#' - Responds to the "Done" button click:
#'   - If a day is selected, it creates the journal for that day (if necessary) and navigates to the journal file in RStudio.
#'   - If no day is selected, the gadget is closed without any action.
#'
#' @return
#' This function does not return a value. It is invoked as part of the Shiny server logic for the gadget.
#'
#' @examples
#' # Example usage within a Shiny gadget:
#' server <- function(input, output, session) {
#'   addin_open_daily_journal_server(input, output, session)
#' }
#'
#' @seealso
#' - \code{\link{addin_open_daily_journal_ui}} for the corresponding UI function.
#' - \code{\link{create_daily_journal}} for creating daily journal files.
#' - \code{\link{addin_rstudio_nav}} for navigating to files in RStudio.
#'
#' @note
#' This function is designed to be used exclusively within a Shiny gadget as part of the "Open Daily Journal" add-in.
#'
addin_open_daily_journal_server <- function(input, output, session) {
  # Reactive value to store the selected day
  selected_date <- reactiveVal(NULL)

  # Observe date selection
  observeEvent(input$calendar, {
    if (!is.null(input$calendar)) {
      #print("click")
      # Convert the input to POSIXct using UTC to avoid local timezone conversion issues
      #print( paste0("input$calendar: ", input$calendar) )
      #print(  paste0("input$calendar typeof: ",typeof(input$calendar)) )
      date <- as.POSIXct(input$calendar, tz = "UTC")
      #print( paste0("date UTC: ", date) )
      #selected_date(date)
      # Try to parse the input as a date-time string in UTC.
      dt_utc <- suppressWarnings(lubridate::ymd_hms(input$calendar, tz = "UTC"))
      # If parsing fails (e.g. the string is simply "YYYY-MM-DD"), fall back to ymd()
      #print( paste0("1dt_utc: ", dt_utc))
      if (is.na(dt_utc)) {
        dt_utc <- lubridate::ymd(input$calendar, tz = "UTC")
      }
      #print( paste0("dt_utc: ", dt_utc))
      # Convert the UTC time to your local timezone
      dt_local <- lubridate::with_tz(dt_utc, tzone = Sys.timezone())
      #print(paste0("dt_local: ", dt_local))
      #print(paste0("lubridate::as_date() dt_local: ", lubridate::as_date((dt_local) ) ))

      # need to use lubridate as_date - as.Date() function returns incorrect date!
      selected_date(lubridate::as_date((dt_local)))

    }
  })

  # Display the Monday of the selected week
  output$selected_day_text <- renderText({
    if (!is.null(selected_date())) {
      paste("Selected day:", selected_date())
    } else {
      "No day selected yet."
    }
  })

  # Handle "Done" button click
  observeEvent(input$done, {
    if (!is.null(selected_date())) {
      # Create journal if necessary
      journalPath <- create_daily_journal(date = selected_date())

      # Navigate to journal file & close add-in
      addin_rstudio_nav(journalPath)
    } else {
      stopApp(NULL)
    }
  })
}#### ________________________________ ####




