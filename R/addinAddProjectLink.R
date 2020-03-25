#' Add a New Project Link
#'
#' Generates a Shiny Gadget for adding a new Project Link inside
#' a Project Doc.  The Link must be FROM a (Source) Project Docs'
#' Goal/Del/Task TO a Project Note (Single, SubNote, or a whole
#' Group of Notes), or a Destination Project Doc.
#'
#' The User must select a Task in the Source Project Document.
#' A link will then be made to a User selected Project Note or
#' Project Doc in the file system.
#'
#' The ADDIN stipulates any errors in the input, and can only be completed
#' when these errors have been resolved.
#'
#'
#' @export
addinAddProjectLink <- function() {


  # Retrieve cursorSelection from current Active Doc in rstudio:
    # MUST select a TASK or within bounds of a TASK - addProjectNote() OR addProjectNoteGroup()
  selection <- cursorSelection()

  # If no project Task is selected, present ERROR MESSAGE:
  if( selection[[1]] == "FALSE" ) {

    ui <- miniPage(
      gadgetTitleBar("Add New Project Link"),
      h2("Select a Task in a Project Document.", align="center", style="color:red")
    )

    server <- function(input, output, session) {

      observeEvent(input$done, {
        stopApp()
      })

    }

    viewer <- dialogViewer("Add New Project Link", width = 500, height = 300)

    runGadget(ui, server, viewer = viewer)

  }
  else {

    # if selection[["addingSubNote"]] is TRUE, then a Header Note Link has been SELECTED
    # AGAIN, present an ERROR MESSAGE:

    if( selection[["addingSubNote"]] == TRUE ) {


      ui <- miniPage(
        gadgetTitleBar("Add New Project Link"),
        h2("Select a TASK in a Project Document.", align="center", style="color:red")
      )

      server <- function(input, output, session) {

        observeEvent(input$done, {
          stopApp()
        })

      }

      viewer <- dialogViewer("Add New Project Link", width = 500, height = 300)

      runGadget(ui, server, viewer = viewer)

    }



    else {

      # else if selection[["addingSubNote"]] is FALSE, then a PRoject Doc TASK has been selected.
      # want to add a new Link from this task to a user-selected Project Note or Doc.
      # SINGLE or SUBNOTE: addLinkProjectNote()
      # GROUP HEADER: addLinkProjectGroup()
      # PROJECT DOC: addLinkProjectDoc()

      ui <- miniPage(

        gadgetTitleBar("Add New Project Link"),

        miniContentPanel(

          fillCol(

            fillRow( h5("Add a new Project Link to a Project Document.") ),

            fillRow( flex = c(7, 1),  verbatimTextOutput("file", placeholder = TRUE), shinyFilesButton("file", "Select File", "Note Project File to Link to", FALSE)  ),

            fillRow(   span( textOutput("warningFile"), style="color:red")  )

          )
        )
      )



      server <- function(input, output, session) {

        # initialises global$datapath:
        #global <- reactiveValues(datapath = checkProgSubDir( normalizePath("~") )  ) # this sets initial val to current working DIR

        shinyFileChoose(
          input,
          'file',
          root = c(home = '~'),
          filetypes = c('', "Rmd", "txt")
        )

        #file <- reactive( input$file )

        #filepath <- reactive({

         # if(!"datapath" %in% parseFilePaths(c(home = normalizePath("~")),input$file) ) {
          #  paste0("")
          #}
          #else {
            #checkProgSubDir( as.character( parseFilePaths(c(home = normalizePath("~")),input$file)$datapath ) )
          #  as.character( parseFilePaths(c(home = normalizePath("~")),input$file)$datapath )
          #}

          #})

        #observeEvent(ignoreNULL = TRUE,
        #             eventExpr = {
        #               input$file
        #             },
        #             handlerExpr = {
        #               if (!"path" %in% names(file())) return()
        #               home <- normalizePath("~")
        #               global$datapath <- file.path(home, paste(unlist(file()$path[-1]), collapse = .Platform$file.sep))
        #                 #checkProgSubDir( file.path(home, paste(unlist(file()$path[-1]), collapse = .Platform$file.sep)) )
        #                 #checkProgSubDir( as.character(parseFilePaths(c(home = normalizePath("~")),file())$datapath) )
        #
        #             })

        output$file <- renderText({

          #checkProgSubDir( as.character(parseFilePaths(c(home = normalizePath("~")),file())$datapath) )

          checkProgSubDir( as.character(parseFilePaths(c(home = normalizePath("~")),input$file)$datapath) )

          #checkProgSubDir(global$datapath)

          #filepath()

        })

        #observe({

         # updateTextInput(session, "filepath", value = checkProgSubDir( as.character(parseFilePaths(c(home = normalizePath("~")),file())$datapath) )  )

        #})

        #observe({
        #    global <- checkProgSubDir( as.character(parseFilePaths(c(home = normalizePath("~")),input$file)$datapath) )
        #})

        #observe({
        #  if(output$file != "") { # file MUST be in a dir in a Programme
        #    output$file <- checkProgSubDir(file())
        #  }
        #})

        #output$file <- renderText({
        #  global
        #})

        #observe({

         #  if(!"datapath" %in% parseFilePaths(c(home = normalizePath("~")),input$file) ) {
          #  filepath <- paste0("")
        #  }
         # else {
        #    filepath <- reactive( checkProgSubDir( as.character( parseFilePaths(c(home = normalizePath("~")),input$file)$datapath ) ) )
        #    #as.character( parseFilePaths(c(home = normalizePath("~")),input$file)$datapath )
        #  }

        #  if( filepath == "") {
        #    output$warningFile <- renderText({
        #      "FILE PATH NOT VALID"
        #    })
        #  }
        #  else {
        #    output$warningFile <- renderText({
        #      ""
        #    })
        #  }
        #})


        # perform computations to create new Programme:
        observeEvent(input$done, {

          if( checkProgSubDir( as.character(parseFilePaths(c(home = normalizePath("~")),input$file)$datapath) ) == "") {
            output$warningFile <- renderText({
              "*** FILE PATH NOT IN VALID PROGRAMME ***"
            })
          }
          else {

            # construct the link between the project doc goal/del/task in selection
            # and the output$file

            # determine if file is SIMPLE/SUBNOTE, HEADER NOTE or DOC:
            type <- getFileType( checkProgSubDir( as.character(parseFilePaths(c(home = normalizePath("~")),input$file)$datapath) ) )

            cat( "\nTYPE: ", type, "\n" )


            if( type == "NOTE" ) {

                # SIMPLE or SUB NOTE:

                projectmanagr::addLinkProjectNote( checkProgSubDir( as.character(parseFilePaths(c(home = normalizePath("~")),input$file)$datapath) ), selection)

            }
            else if( type == "HEAD" ) {

                # GROUP (HEADER) NOTE:

                projectmanagr::addLinkProjectGroup( checkProgSubDir( as.character(parseFilePaths(c(home = normalizePath("~")),input$file)$datapath) ), selection)

            }
            else if( type == "DOC" ) {

                # PROJECT DOC:

                projectmanagr::addLinkProjectDoc( checkProgSubDir( as.character(parseFilePaths(c(home = normalizePath("~")),input$file)$datapath) ), selection)

            }


            # Close Gadget after computations are complete:
            stopApp()

          }

        })

      }


      viewer <- dialogViewer("Create New Project Note", width = 1000, height = 800)

      runGadget(ui, server, viewer = viewer)

    }


  }

}
