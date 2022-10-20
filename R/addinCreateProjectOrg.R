#' Create a New Project Org
#'
#' Generates a Shiny Gadget for initialising
#' a new Project Organisation.  User selects a destination in
#' the file system, Organisation name (for the directory) and
#' Organisation title (for the html page).
#'
#' @export
addinCreateProjectOrg <- function() {

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

    server <- function(input, output, session) {

      global <- reactiveValues(datapath = normalizePath("~") )

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

          # call projectmanagr::createProjectOrg:
          projectmanagr::createProjectOrg(
                    orgName = input$organisationName,
                    orgTitle = input$organisationTitle,
                    organisationParentDirectory = global$datapath,
                    settingsYamlPath = ""
                          )

          # navigate to org index file:
          rstudioapi::navigateToFile( paste( global$datapath, .Platform$file.sep, input$organisationName, .Platform$file.sep,
                                             "index_", input$organisationName, ".Rmd", sep=""))

          # navigate to containing dir - this function currently doesnt work!
          #rstudioapi::filesPaneNavigate( paste0(global$datapath, .Platform$file.sep, input$organisationName) )

          # Close Gadget after 'done' is clicked.
          stopApp()

        }

      })

    }


    viewer <- dialogViewer("Create New Project Organisation", width = 1000, height = 800)

    runGadget(ui, server, viewer = viewer)


}
