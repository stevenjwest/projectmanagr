#' Create a New Volume
#'
#' Generates a Shiny Gadget for creating a new Volume inside
#' a Project Organisation.  User selects a destination in
#' the file system, Volume name (for the symlink), and
#' optionally the current working directory (set to
#' current WD).  The addin will call createVolume.
#'
#'
#'
#'
#' @export
addinCreateVolume <- function() {

  ui <- miniPage(

    gadgetTitleBar("Create New Volume"),

    miniContentPanel(

      fillCol(

        fillRow( h5("Create a new Volume inside a Project Organisation.") ),

        fillRow(  textInput("volumeName", "Volume Directory Name:", value = "", width="100%")  ),

        fillRow(  span( textOutput("warningName"), style="color:red")  ),

        fillRow( flex = c(6, 1),  verbatimTextOutput("linkdir", placeholder = TRUE), shinyDirButton("linkdir", "Select Link Dir.", "Link Directory")  ),

        fillRow(   span( textOutput("warningLinkDir"), style="color:red")  ),

        fillRow( flex = c(6, 1),  verbatimTextOutput("dir", placeholder = TRUE), shinyDirButton("dir", "Select Org Dir.", "Org Directory")  ),

        fillRow(   span( textOutput("warningDirectory"), style="color:red")  )

      )
    )
  )

  server <- function(input, output, session) {


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


    # compute Dir selection:
    global2 <- reactiveValues(datapath = '/'  ) # this sets initial val to CURRENT WD

    # allows selection of Dir, with Volume set to HOME Dir
    shinyDirChoose(
      input,
      'linkdir',
      roots = c(home = '/'),
      filetypes = c('', 'txt', 'bigWig', "tsv", "csv", "bw")
    )

    linkdir <- reactive(input$linkdir)

    output$linkdir <- renderText({
      global2$datapath
    })

    observeEvent(ignoreNULL = TRUE,
                 eventExpr = {
                   input$linkdir
                 },
                 handlerExpr = {
                   if (!"path" %in% names(linkdir())) return()
                   home <- ""
                   global2$datapath <-
                     file.path(home, paste(unlist(linkdir()$path[-1]), collapse = .Platform$file.sep))
                 })


    observe({
      if(global2$datapath == "") {
        output$warningLinkDir <- renderText({
          "DIR PATH NOT VALID ORGANISATION"
        })
      }
      else {
        output$warningLinkDir <- renderText({
          ""
        })
      }
    })



    # perform computations to create new Programme:
    observeEvent(input$done, {

      if(input$volumeName == "") {
        # set the warningName TextOutput:
        output$warningName <- renderText({
          "*** PROVIDE PROGRAMME NAME ***"
        })
      }
      else if(global$datapath == "") {
        output$warningDirectory <- renderText({
          "*** DIR PATH NOT VALID ORGANISATION ***"
        })
      }
      else if(global2$datapath == "") {
        output$warningLinkDir <- renderText({
          "*** DIR PATH NOT VALID ORGANISATION ***"
        })
      }
      else {

        # call projectmanagr::createVolume:
        projectmanagr::createVolume(sourcePath = global2$datapath,
                                    volName = input$volumeName,
                                    orgPath = global$datapath )

        # Close Gadget after 'done' is clicked.
        stopApp()

      }

    })

  }


  viewer <- dialogViewer("Create New Volume", width = 1000, height = 600)

  runGadget(ui, server, viewer = viewer)


}
