#' Create a New Project Org Addin
#'
#' Generates a Shiny Gadget for searching a ProjectManagr Organisation.
#' User can select the organisations & the scope for searching within
#' it - default is the root of organisation of current working directory.
#'
#' Search terms cover only the ProjectManagr files by default - this scope
#' can be expanded to include other files as set by the user.
#'
#' @export
addin_search_project_org <- function() {

  cat( "\nprojectmanagr::addin_search_project_org():\n" )


  #### instance variables ####

  # Retrieve cursor_selection from current Active Doc in rstudio:
  selection <- cursor_selection() # Project Doc, Project Note - Simple, Header, SubNote
  # may be unknown - ALL have filePath

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

  # define roots for dir selection - to orgPath
  roots <- c(orgPath)
  names(roots) <- c(basename(orgPath))

  # generate a blank table to initialise addin with
  FILENAME <- ""
  CONTEXT <- ""
  LOCATION <- "" # storing location but not adding it to the table
  searchResults <- tibble::tibble(FILENAME, CONTEXT)


  #### user interface ####

  ui <- miniPage(

    gadgetTitleBar("Search Project Organisation"),

    miniContentPanel(

      fillCol( flex = c(1,1,1,1,1,20),

        fillRow( h5("Select organisation scope for search:") ),

        # search filesystem scope selected here
        fillRow( flex = c(7, 1),  verbatimTextOutput("dirtxt", placeholder = TRUE), shinyDirButton("dir", "Select Directory", "Organisation Search Scope")  ),

        # search terms entered here
        fillRow(  textInput("searchTerm", "Search:", value = "", width="100%")  ),

        fillRow( h5("") ),

        fillRow(   span( textOutput("warning"), style="color:red")  ),

        # results displayed in this table
        fillRow( DT::dataTableOutput("mytable1", height = "100%") )

      )
    )
  )


  #### server code ####

  server <- function(input, output, session) {

    # compute Dir selection:
    global <- reactiveValues(datapath = orgPath )
    # this sets initial value of global$datapath to orgPath

    # allows selection of Dir, with roots set to project doc DIR or ORG Dir
    shinyDirChoose(
      input, 'dir',
      roots=roots, # orgPath used as root
      filetypes = c('', settings[["FileTypeSuffix"]])
    )

    # reactive dir to extract data from it
    dir <- reactive(input$dir)

    # reactive mytable to extract data from it
    mytable <- reactive(input$mytable1)

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

    #observe({ cat('\n  global$datapath: _', global$datapath, '_\n') })

    observe({
      if(input$searchTerm != "") { # only run search once a search term has been entered

        # Create a Progress object
        progress <- shiny::Progress$new()
        progress$set(message = "Computing data", value = 0)
        # Close the progress when this reactive exits (even if there's an error)
        on.exit(progress$close())

        # Create a callback function to update progress.
        # Each time this is called:
        # - If `value` is NULL, it will move the progress bar 1/5 of the remaining
        #   distance. If non-NULL, it will set the progress to that value.
        # - It also accepts optional detail text.
        updateProgress <- function(value = NULL, detail = NULL) {
          #if (is.null(value)) {
          #  value <- progress$getValue()
          #  value <- value + (progress$getMax() - value) / 5
          #}
          progress$set(value = value, detail = detail)
        }
        outputList <- search_dir_tree(global$datapath, input$searchTerm, updateProgress = updateProgress, settings, orgPath)
        global$summary <- outputList[[1]]
        global$data <- outputList[[2]]
      }
    })

    # set the dir text to the selected path
    output$dirtxt <- renderText({ global$datapath })

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


    output$mytable1 <- DT::renderDT( global$summary,
                                     selection = 'single',
                                     class = 'cell-border',
                                     #editable = list(target = "cell", disable = list(columns = c(1:7))),
                                     editable = FALSE,
                                     filter = 'top',
                                     caption = 'Search Summary Table',
                                     fillContainer = TRUE,
                                     options = list(
                                       lengthMenu = c(20, 50, 100),
                                       pageLength = 20,
                                       columnDefs = list(list(className = 'dt-center', targets = c(1:2))) ),
                                     server = FALSE ) # processing on client-side means edits to IMPORT col are kept when searching the table


    observe({ cat('\n  input$mytable1_rows_selected: _', input$mytable1_rows_selected, '_\n') })

    observeEvent(input$done, {

      if( is.null( input$mytable1_rows_selected ) == TRUE) {
        # set the warning TextOutput:
        output$warning <- renderText({
          "SELECT ROW IN TABLE"
        })
      } else {


        #### open selected file from table ####

        cat("LOCATION : ", global$data$LOCATION, "\n\n")

        cat("LOCATION[index] : ", global$data$LOCATION[input$mytable1_rows_selected], "\n\n")

        cat("LINE : ", global$data$LINE, "\n\n")

        # navigate to org index file:
        rstudioapi::navigateToFile( global$data$LOCATION[input$mytable1_rows_selected] )

        # move to line where search term was identified


        workingDir <- dirname(global$data$LOCATION[input$mytable1_rows_selected])

        # navigate to containing dir
        rstudioapi::filesPaneNavigate( workingDir )
        # and set working directory
        setwd( workingDir )

        # Close Gadget after 'done' is clicked.
        stopApp()
      }
    })
  }


  #### view gadget ####

  viewer <- dialogViewer("Search Project Organisation",
                         width = settings[["GadgetWidth"]],
                         height = settings[["GadgetHeight"]])

  runGadget(ui, server, viewer = viewer)

}
