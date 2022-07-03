#' Create a New Project Document
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
addinCreateProjectDoc <- function() {

  # find the PROGRAMME DIR if the current path is inside one:
  wd <- findProgDir(getwd())


  # set templates:
  templates <- c("", "REVIEW", "DEV")

  ui <- miniPage(

    gadgetTitleBar("Create New Project Document"),

    miniContentPanel(

      fillCol(

        fillRow( h5("Create a new Project Document inside a Programme.") ),

        fillRow(  textInput("projectName", "Project Name:", value = "PROJECT", width="100%")  ),

        fillRow(  span( textOutput("warningName"), style="color:red")  ),

        fillRow(  textInput("projectTitle", "Project Title:", value = "01 PROGRAMME", width="100%")  ),

        fillRow( flex = c(7, 1),  verbatimTextOutput("dir", placeholder = TRUE), shinyDirButton("dir", "Select Directory", "Programme Parent Directory")  ),

        fillRow(   span( textOutput("warningDirectory"), style="color:red")  ),

        fillRow(   textOutput("projectPathOutput")  )

        #fillRow(   textOutput("projectPathOutput")  ),

        #fillRow( selectInput("select", "Select Template:",
        #                     choices = templates, selected = templates[1]) ),

        #fillRow(   span( textOutput("warningTemplate"), style="color:red")  )

      )
    )
  )

  server <- function(input, output, session) {

    # update programmeTitle when programmeName is changed:
    observe({

      updateTextInput(session, "projectTitle", value = gsub("-", " ", gsub("_", " ", input$projectName) )  )

    })


    # compute Dir selection:
    global <- reactiveValues(datapath = checkProgDir( normalizePath(wd) )  ) # this sets initial val to wd - current WD or the PROGRAMME if in the PROEJCTS DIR

    # allows selection of Dir, with Volume set to HOME Dir
    shinyDirChoose(
      input,
      'dir',
      roots = c(home = '~'),
      filetypes = c('', 'txt', 'bigWig', "tsv", "csv", "bw")
    )

    dir <- reactive(input$dir)

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

    observe({
      if(global$datapath != "") {
        global$datapath <- checkProgDir(global$datapath)
      }
    })

    output$dir <- renderText({
      global$datapath
    })

    observe({
      if(global$datapath == "") {
        output$warningDirectory <- renderText({
          "DIR PATH NOT VALID PROGRAMME"
        })
      }
      else {
        output$warningDirectory <- renderText({
          ""
        })
      }
    })


    observe({

      if( global$datapath != "" && input$projectName != "" ) {

        projPath <- getProjectPath(global$datapath, input$projectName )

        output$projectPathOutput <- renderText({
          projPath
        })

      }

      else {

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
      else if(global$datapath == "") {
        output$warningDirectory <- renderText({
          "*** DIR PATH NOT VALID PROGRAMME ***"
        })
      }
      #else if(input$select == "") {
      #  output$warningTemplate <- renderText({
      #    "*** SELECT A TEMPLATE ***"
      #  })
      #}
      else {

        # FIRST - save all open documents in RStudio:
        rstudioapi::documentSaveAll()

        # call projectmanagr::createProjectDoc:
        projectmanagr::createProjectDoc(
          projectName = input$projectName,
          projectTitle = input$projectTitle,
          fileSystemPath = global$datapath,
          projDocTemplate = "Project-Doc-Template.Rmd",
          projectIndex = 0
        )

        #if(input$select == "REVIEW") {
        #  # call projectmanagr::createProjectDoc:
        #  projectmanagr::createProjectDoc(
        #    projectName = input$projectName,
        #    projectTitle = input$projectTitle,
        #    fileSystemPath = global$datapath,
        #    projDocTemplate = "Project-Doc-Template-REVIEW.Rmd",
        #    projectIndex = 0
        #          )
        #
        #}
        #else if(input$select == "DEV") {
        #  # call projectmanagr::createProjectDoc:
        #  projectmanagr::createProjectDoc(
        #    projectName = input$projectName,
        #    projectTitle = input$projectTitle,
        #    fileSystemPath = global$datapath,
        #    projDocTemplate = "Project-Doc-Template-DEV.Rmd",
        #    projectIndex = 0
        #          )
        #}

        # navigate to project doc file:
        pro_path <- paste0(global$datapath, .Platform$file.sep, "PROJECTS")
        rstudioapi::navigateToFile( paste0( pro_path, .Platform$file.sep,
                                      list.files(pro_path)[grepl(input$projectName, list.files(pro_path))]  )  )

        # navigate to containing dir - this function currently doesnt work!
        #rstudioapi::filesPaneNavigate( pro_path )

        # Close Gadget after 'done' is clicked.
        stopApp()

      }

    })

  }


  viewer <- dialogViewer("Create New Project Document", width = 1000, height = 800)

  runGadget(ui, server, viewer = viewer)


}
