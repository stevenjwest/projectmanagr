
#' Addin to Rename a Project Component - Doc or Note
#'
#'
#'@export
addinRenameComp <- function( ) {

  # get the currently active document:
  context <- rstudioapi::getSourceEditorContext()

  # get contents:
  openDocContents <- context$contents

  cursor <- rstudioapi::primary_selection(context)

  line <- (cursor$range[[1]])[1]
  column <- (cursor$range[[1]])[2]
  contextName <- basename(context$path)

  progPath <- findProgDir(context$path)

  if( progPath == "" || grepl("~_", basename(context$path) ) == FALSE ) {

    ui <- miniPage(
      gadgetTitleBar("Rename Component"),
      h2("No Project Doc or Note is Selected.", align="center", style="color:red")
    )

    server <- function(input, output, session) {

      observeEvent(input$done, {
        stopApp()
      })

    }

    viewer <- dialogViewer("Add New Project Note", width = 500, height = 300)

    runGadget(ui, server, viewer = viewer)

  }

  else {


  # UI:

  ui <- miniPage(

    gadgetTitleBar("Rename Component"),

    miniContentPanel(

      fillCol(

        fillRow( h5("Add a new Hyperlink:") ),

        fillRow(
          h3(  paste("TO:", contextName ), align="center"  )
        ),

        fillRow(
          h5(  paste("Line:", line ), align="center"  ),
          h5(  paste("Column:", column ), align="center"  )
        ),

        fillRow(  textInput("newName", "New Name:", width="100%")  ),

        fillRow(  span( textOutput("warningName"), style="color:red")  ),

        fillRow(  textInput("newTitle", "New Title:", width="100%")  )

      )
    )
  )


  ### ENCODE BEHAVIOUR ###

  server <- function(input, output, session) {


    # update projectNoteTitle when projectNoteName is changed:
    observe({

      updateTextInput(session, "newTitle", value = gsub("-", " ", gsub("_", " ", input$newName) )  )

    })


    observe({

      if( grepl("\\s", input$newName)  ) {
        # set the warningName TextOutput:
        output$warningName <- renderText({
          "NEW NAME CANNOT CONTAIN SPACES"
        })
      }
      else {
        output$warningName <- renderText({
          ""
        })
      }
    })


    # perform computations to form new hyperlink
    observeEvent(input$done, {


      if(input$newName == "") {
        # set the warningName TextOutput:
        output$warningName <- renderText({
          "*** PROVIDE NEW NAME ***"
        })
      }
      else if( grepl("\\s", input$newName)  ) {
        # set the warningName TextOutput:
        output$warningName <- renderText({
          "*** NEW NAME CANNOT CONTAIN SPACES ***"
        })
      }
      else {


        renameProjectComp( context$path, input$newName, newProjectCompTitle=input$newTitle )

        # Close Gadget after computations are complete:
        stopApp()

      }


    })

  }


  ### VIEW GADGET ###

  viewer <- dialogViewer("Create New Project Document", width = 800, height = 600)

  runGadget(ui, server, viewer = viewer)



  }

}



