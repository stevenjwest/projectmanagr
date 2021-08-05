#' Create Datatable from File
#'
#' Generates a Shiny Gadget for initialising
#' a new datatable from file.  User selects a csv file from
#' the file system, and this is converted to datatable format and inserted
#' in current Rmd at the current line.
#'
#' @export
addinDatatableCreateFromFile <- function() {

   # get data from current file
   context <- rstudioapi::getSourceEditorContext()
   row <- context$selection[[1]]$range$start[1]
   path <- normalizePath( context$path )

   # SAVE the document before processing!
   rstudioapi::documentSave(id = context$id)

    ui <- miniPage(

      gadgetTitleBar("Create Datatable from File"),

      miniContentPanel(

        fillCol(

          fillRow( p("Create a datatable in the active Rmd at the cursor from a CSV (or other data) file.") ),

          fillRow(
            helpText( p('Rmd: ', context$path, align="center") ) ),

          fillRow(
            helpText( p('line: ', row, align="center") ) ),

          fillRow(  textInput("dt_name", "Datatable name:", value="samples", width="100%")  ),

          # using shiny's fileInput function
           # see: https://shiny.rstudio.com/articles/upload.html

          fillRow( fileInput("file1", "Choose CSV File",
                    multiple = FALSE,
                    accept = c("text/csv",
                               "text/comma-separated-values,text/plain",
                               ".csv")) )
        )
      )
    )

    server <- function(input, output, session) {

      observeEvent(input$done, {

        # open csv
        tb <- readr::read_csv( input$file1$datapath, col_types = readr::cols(.default = "c") )

        # create datatable from it
        dt <- projectmanagr::parse_tibble(tb, input$dt_name )

        # insert into current document
        fileConn <- file(path)
        writeLines(
          c(context$contents[1:row], dt, context$contents[(row+1):length(context$contents)]),
          fileConn)
        close(fileConn)

        # navigate to file:
        rstudioapi::navigateToFile( path )

        # Close Gadget after 'done' is clicked.
        stopApp()

      })

    }


    viewer <- dialogViewer("Create Datatable from File", width = 1000, height = 800)

    runGadget(ui, server, viewer = viewer)

}
