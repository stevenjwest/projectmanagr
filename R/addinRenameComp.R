
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

    viewer <- dialogViewer("Rename Component", width = 500, height = 300)

    runGadget(ui, server, viewer = viewer)

  }

  else {


    # variable to decide whether ui is name or prefix
    ui_type <- "name"

  # UI:

  ui <- miniPage(

    gadgetTitleBar("Rename Component"),

    miniTabstripPanel(

      miniTabPanel("name",

        miniContentPanel(

          fillCol(

            fillRow( h5("Rename component: Name") ),

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
        ),

        value = "name"
    ),

    miniTabPanel("prefix",

       miniContentPanel(

         fillCol(

           fillRow( h5("Rename component: Prefix") ),

           fillRow(
             h3(  paste("TO:", contextName ), align="center"  )
           ),

           fillRow(
             h5(  paste("Line:", line ), align="center"  ),
             h5(  paste("Column:", column ), align="center"  )
           ),

           fillRow(  textInput("newPrefix", "New Prefix:", width="100%")  ),

           fillRow(  span( textOutput("warningPrefix"), style="color:red")  )

         )
       ),

       value = "prefix"
    ),

    id = "strippanel"

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

    observe({

      if(  ( checkProjectPrefix(context$path, input$newPrefix) != "" ) && ( input$newPrefix != "" )  ) {
        # set the warningName TextOutput:
        output$warningPrefix <- renderText({
          paste0("*** ", checkProjectPrefix(context$path, input$newPrefix), " ***" )
        })
      } else {
        output$warningPrefix <- renderText({
          ""
        })
      }
    })

    # perform computations to form new hyperlink
    observeEvent(input$done, {

      cat( "  input strippanel : ", input$strippanel)

     if( input$strippanel == "name" ) {

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

     } else if( input$strippanel == "prefix" ) {

       if(input$newPrefix == "") {
         # set the warningName TextOutput:
         output$warningPrefix <- renderText({
           "*** PROVIDE NEW PREFIX ***"
         })
       } else if( (checkProjectPrefix(context$path, input$newPrefix) != "") && (input$newPrefix != "")  ) {
         # set the warningName TextOutput:
         output$warningPrefix <- renderText({
           paste0("*** ", checkProjectPrefix(context$path, input$newPrefix), " ***" )
         })

       } else {

         file_type <- getFileType( context$path )

         cat( "  file type : ", file_type )

         if( file_type == "DOC" ) {

           renameProjectDocPrefix( context$path, input$newPrefix )

         } else if( file_type == "HEAD" ) {

           renameProjectNotePrefix( context$path, input$newPrefix )

         } else if( file_type == "NOTE" ) {

           renameProjectNotePrefix( context$path, input$newPrefix )

         }

         # Close Gadget after computations are complete:
         stopApp()

       }

     }


    })

  }


  ### VIEW GADGET ###

  # get config/status.yml to read gadgetWidth/Height preferences
  orgPath <- findOrgDir(context$path)
  confPath <- paste0( orgPath, .Platform$file.sep, "config" )
  settingsFile = paste( confPath, .Platform$file.sep, "settings.yml", sep="" )
  settingsContents <- yaml::yaml.load( yaml::read_yaml( settingsFile ) )

  viewer <- dialogViewer("Rename Component", width = settingsContents$gadgetWidth,
                                             height = settingsContents$gadgetHeight )

  runGadget(ui, server, viewer = viewer)



  }

}



