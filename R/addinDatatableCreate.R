#' Create Datatable from File
#'
#' Generates a Shiny Gadget for initialising
#' a new datatable from file  User selects a csv file from
#' the file system, and this is converted to datatable format and inserted
#' in current Rmd at the current line.
#'
#' @export
addinDatatableCreate <- function() {

   # get data from current file
   context <- rstudioapi::getSourceEditorContext()
   row <- context$selection[[1]]$range$start[1]
   path <- normalizePath( context$path )

   # SAVE the document before processing!
   rstudioapi::documentSave(id = context$id)

   # identify the lines that begin with "+==="
   indices <- which( startsWith( context$contents, "+===") )
   indices_row <- which( startsWith( context$contents[1:row], "+===") )

   # from indices determine if the cursor is sitting IN or OUT of a datatable

   if((length(indices_row) %% 2) == 1) {
     # cursor is sitting IN a datatable - potentially expanding a CREATE TEMPLATE
      # check if it is a CREATE TEMPLATE

     if(length(indices) <= length(indices_row) ) {
       stop( paste0("  Cannot identify next datatable separator - check syntax?") )
     }

     # get the template_table:
    startrow <- indices_row[length(indices_row)]
    endrow <- indices[length(indices_row)+1]
    template_table <- context$contents[ startrow : endrow ]

    table_name <- trimws(strsplit( as.character(template_table[4]), ":")[[1]][1])
    table_function <- trimws(strsplit( as.character(template_table[4]), ":")[[1]][2])

    if(table_name != "TEMPLATE") {
      stop( paste0("  Cursor is inside a datatable that is not a TEMPLATE - rename table or move cursor!") )
    }

    # After these basic checks, now can generate shiny gadget for user to provide input

    ui <- miniPage(

      gadgetTitleBar("Create Datatable from Template"),

      miniContentPanel(

        fillCol(

          fillRow( p("Create a datatable in the active Rmd in the selected Template.") ),

          fillRow(
            helpText( p('Rmd: ', context$path, align="center") ) ),

          fillRow(
            helpText( p('Start line: ', startrow, align="center") ),
            helpText( p('End line: ', endrow, align="center") )),

          fillRow(  textInput("name", "Datatable name:", value = "samples", width="100%")  ),

          fillRow(  span( textOutput("warningName"), style="color:red")  ),

          fillRow(  textInput("ids", "IDs (space-separated):", width="100%")  ),

          fillRow(  span( textOutput("warningName2"), style="color:red")  )

        )
      )
    )

    server <- function(input, output, session) {

      observeEvent(input$done, {

        if(input$name == "") {

          # set the warningName TextOutput:
          output$warningName <- renderText({
            "*** PROVIDE DATATABLE NAME ***"
          })

        } else {

          # split ids and data_cols at spaces
          ids <- unlist( strsplit(as.character(input$ids), ' ') )

          # change any character(0) to blank string:
          if( identical(ids, character(0) ) ) {
            ids <- ""
          }

          # create datatable from params and put into path at row
          projectmanagr::datatable_create_template_rmd(path, startrow, endrow, ids, input$name)

          # navigate to file - to reload:
          #rstudioapi::navigateToFile( path )

          # Close Gadget after 'done' is clicked.
          stopApp()

        }
      })
    }


    viewer <- dialogViewer("Create Datatable", width = 1000, height = 800)

    runGadget(ui, server, viewer = viewer)


   } else { # cursor lays OUTSIDE of a datatable - here will just generate shiny gadget to CREATE a datatable:

    ui <- miniPage(

      gadgetTitleBar("Create Datatable"),

      miniContentPanel(

        fillCol(

          fillRow( p("Create a datatable in the active Rmd at the cursor from input params.") ),

          fillRow(
            helpText( p('Rmd: ', context$path, align="center") ) ),

          fillRow(
            helpText( p('line: ', row, align="center") ) ),

          fillRow(  textInput("name", "Datatable name:", value = "samples", width="100%")  ),

          fillRow(  span( textOutput("warningName"), style="color:red")  ),

          fillRow(  textInput("ids", "IDs (space-separated):", width="100%")  ),

          fillRow(  span( textOutput("warningName2"), style="color:red")  ),

          fillRow(  textInput("data_cols", "Extra Data Cols (space-separated):", width="100%")  )

        )
      )
    )

    server <- function(input, output, session) {

        observeEvent(input$done, {

          if(input$name == "") {

            # set the warningName TextOutput:
            output$warningName <- renderText({
              "*** PROVIDE DATATABLE NAME ***"
            })

          } else {

            # split ids and data_cols at spaces
            ids <- unlist( strsplit(as.character(input$ids), ' ') )
            data_cols <- unlist( strsplit(as.character(input$data_cols), ' ') )

            # change any character(0) to blank string:
            if( identical(ids, character(0) ) ) {
              ids <- ""
            }
            if( identical(data_cols, character(0) ) ) {
              data_cols <- ""
            }

            # create datatable from params and put into path at row
            projectmanagr::datatable_create_rmd(path, row, ids, data_cols, input$name)

            # navigate to file - to reload:
            #rstudioapi::navigateToFile( path )

            # Close Gadget after 'done' is clicked.
            stopApp()

          }
        })
    }


    viewer <- dialogViewer("Create Datatable", width = 1000, height = 800)

    runGadget(ui, server, viewer = viewer)

   }

}
