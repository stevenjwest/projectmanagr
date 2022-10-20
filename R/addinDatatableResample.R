#' Add Resample Datatable to existing samples
#'
#' Generates a Shiny Gadget for adding data to existing samples in a new
#' datatable.
#'
#' User selects a datatable name : list of existing datatables
#' in the current Rmd.
#'
#' User selects datatable layout : samples, variables, timetable.
#'
#' User selects the ID format to add : ALL, all-IDs, or a group set.
#'
#' The datatable with function ADD_DATA is added with appropriate layout.
#'
#' If the cursor is already INSIDE a GENERIC ADD_DATA, which has a valid
#' ADD_DATA layout (and is already sample, variable or timetable), then
#' user just needs to select the IDs to add : ALL, all-IDs, or a group set.
#'
#' The generic ADD_DATA table will be updated to contain the correct IDs
#' from existing samples for adding data.
#'
#' @export
addinDatatableResample <- function() {

   # get data from current file
   context <- rstudioapi::getSourceEditorContext()
   row <- context$selection[[1]]$range$start[1]
   path <- normalizePath( context$path )

   # SAVE the document before processing!
   rstudioapi::documentSave(id = context$id)

   # collect all sample data from the current Rmd:
   dts <- projectmanagr::datatable_read_rmd(path)
   dt_names <- names(dts)

   lt <- as.list( 1:length(dt_names) )
   names(lt) <- dt_names

    ui <- miniPage(

      gadgetTitleBar("Resample Datatable"),

      miniContentPanel(

        fillCol(

          fillRow( p("Resample a datatable in the active Rmd at the cursor from input params.") ),

          fillRow(
            helpText( p('Rmd: ', context$path, align="center") ) ),

          fillRow(
            helpText( p('line: ', row, align="center") ) ),

          fillRow( p("Choose which datatable to resample.") ),

          fillRow( selectInput("dt", "Select Datatable",
                               choices = lt, selected = 1, width="100%")  ),

          fillRow( p("Define the resample vector - these MUST be unique to this new datatable.") ),

          fillRow(  textInput("data_cols", "Resample vector (space-separated):", width="100%")  ),

          fillRow(  span( textOutput("warningName"), style="color:red")  )

        )
      )
    )

    server <- function(input, output, session) {

      observeEvent(input$done, {

          col_name_warning <- ""
          # check the selected dt does not contain the defined data cols:
          dt_col_names <- names(dts[[input$dt]])

          #for(i in 1:length(input$data_cols) ) { # SKIP index 1 - this IS ID COL - which already exists in dt!

            # check dt doesnt already contain col of same name?
           # if( any( dt_col_names == input$data_cols[i] ) ) {
         #     col_name_warning <- paste0("  Data col already exists in datatable: ", input$data_cols[i] )
          #  }
          #}

          if(col_name_warning != "") {

            # set the warningName TextOutput:
            output$warningName <- renderText({
              col_name_warning
            })

          } else {

             #dts <- current_dt()

            # split data_cols at spaces
            data_cols <- unlist( strsplit(as.character(input$data_cols), ' ') )

            if( identical(data_cols, character(0) ) ) {
              data_cols <- ""
            }

            projectmanagr::datatable_resample_rmd(
               rmd_path = path,
               rmd_line = row,
               datatable_name = names(dts)[ as.numeric(input$dt) ],
               resample_vector = data_cols,
               dt_length = 120 )


            # navigate to file - to reload:
            rstudioapi::navigateToFile( path )

            # Close Gadget after 'done' is clicked.
            stopApp()

          }
        })
    }


    viewer <- dialogViewer("Create Datatable", width = 1000, height = 800)

    runGadget(ui, server, viewer = viewer)

}
