#' Create a New Programme
#'
#' Generates a Shiny Gadget for creating a new Programme inside
#' a Project Organisation.  User selects a destination in
#' the file system, Programme name (for the directory),
#' Programme prefix (used to identify Projects with this
#' Programme), and Programme title (for the html page).
#'
#'
#' @export
addinCreateProgramme <- function() {

  ui <- miniPage(

    gadgetTitleBar("Create New Programme"),

    miniContentPanel(

      fillCol(

        fillRow( h5("Create a new Programme inside a Project Organisation.") ),

        fillRow(  textInput("programmeName", "Programme Directory Name:", value = "01-PROGRAMME", width="100%")  ),

        fillRow(  span( textOutput("warningName"), style="color:red")  ),

        fillRow(  textInput("programmePrefix", "Programme Prefix:", value = "", width="100%")  ),

        fillRow(   span( textOutput("warningPrefix"), style="color:red")  ),

        fillRow(  textInput("programmeTitle", "Programme Title:", value = "01 PROGRAMME", width="100%")  ),

        fillRow( flex = c(7, 1),  verbatimTextOutput("dir", placeholder = TRUE), shinyDirButton("dir", "Select Directory", "Programme Parent Directory")  ),

        fillRow(   span( textOutput("warningDirectory"), style="color:red")  )

      )
    )
  )

  server <- function(input, output, session) {

    # update programmeTitle when programmeName is changed:
    observe({

      updateTextInput(session, "programmeTitle", value = gsub("-", " ", gsub("_", " ", input$programmeName) )  )

    })


    # compute Dir selection:
    global <- reactiveValues(datapath = findOrgDir( normalizePath(getwd()) )  ) # this sets initial val to CURRENT WD

    # allows selection of Dir, with Volume set to HOME Dir
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


    observe({
      if(global$datapath == "") {
        output$warningDirectory <- renderText({
          "DIR PATH NOT VALID ORGANISATION"
        })
      }
      else {
        output$warningDirectory <- renderText({
          ""
        })
      }
    })



    # perform computations to create new Programme:
    observeEvent(input$done, {

      if(input$programmeName == "") {
        # set the warningName TextOutput:
        output$warningName <- renderText({
          "*** PROVIDE PROGRAMME NAME ***"
        })
      }
      else if(input$programmePrefix == "") {
        # set the warningPrefix TextOutput:
        output$warningPrefix <- renderText({
          "*** PROVIDE PROGRAMME PREFIX ***"
        })
      }
      else if(global$datapath == "") {
        output$warningDirectory <- renderText({
          "*** DIR PATH NOT VALID ORGANISATION ***"
        })
      }
      else {

        # call projectmanagr::createProgramme:
        projectmanagr::createProgramme(programmeName = input$programmeName,
                                       programmePrefix = input$programmePrefix,
                                       programmeTitle = input$programmeTitle,
                                       fileSystemPath = global$datapath  )

        # Close Gadget after 'done' is clicked.
        stopApp()

      }

    })

  }


  viewer <- dialogViewer("Create New Programme", width = 1000, height = 800)

  runGadget(ui, server, viewer = viewer)


}
