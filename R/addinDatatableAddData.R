#' Add Datatable to existing samples
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
#' User defines the data table column names : using the suffix `_dt` denotes
#' that the column is a DATETIME and it appropriately spaced to hold a datetime.
#' Otherwise columns are spaced according to the width of the column title.
#'
#' The datatable with function ADD_DATA is added with appropriate layout.
#'
#' If the cursor is already INSIDE a TEMPLATE ADD_DATA, which has a valid
#' ADD_DATA layout (and is already sample, variable or timetable), then
#' user just needs to select the existing samples datatable to insert data from,
#' and the IDs to add : ALL, all-IDs, or a group set.
#'
#' The generic ADD_DATA table will be updated to contain the correct IDs
#' from the selected existing samples.
#'
#' @export
addinDatatableAddData <- function() {

   # get data from current file
   context <- rstudioapi::getSourceEditorContext()
   row <- context$selection[[1]]$range$start[1]
   path <- normalizePath( context$path )

   # SAVE the document before processing!
   rstudioapi::documentSave(id = context$id)

   # create named list of types
   type <- list("sample-first"=1, "variable-first"=2, "timetable"=3)

   # collect all sample data from the current Rmd
   #dts <- projectmanagr::datatable_read_rmd(path)
   # collect all sample data from the current Rmd up to SELECTED LINE
   dts <- projectmanagr::datatable_read_vector(context$contents[1:row]) # TEST THIS!
   dt_names <- names(dts)

   lt <- as.list( 1:length(dt_names) )
   names(lt) <- dt_names

   # select group cols from first datatable INITIALLY
   gdt <- dplyr::select( dts[[1]], dplyr::starts_with("group-") )
   ltid <- as.list(1:(length(gdt)+2) )
   names(ltid) <- c("ALL", "all-IDs", names(gdt))

    ui <- miniPage(

      gadgetTitleBar("Add Data Datatable"),

      miniContentPanel(

        fillCol(

          fillRow( p("Add Data to a datatable in the active Rmd at the cursor from input params.") ),

          fillRow(
            helpText( p('Rmd: ', context$path, align="center") ) ),

          fillRow(
            helpText( p('line: ', row, align="center") ) ),

          fillRow( p("sample-first: First col. is filled with all sample or group IDs, subsequent columns are data cols. specified below.") ),

          fillRow( p("variable-first: First col. is filled with all data cols. specified below, subsequent columns are sample or group IDs") ),

          fillRow( p("timetable: special table for planning and recording datetimes of a procedure's steps executed with different timing across sample or group IDs.") ),

          fillRow( selectInput("type", "Select Datatable Type",
                               choices = type, selected = 1, width="100%")  ),

          fillRow( p("Choose which datatable the new data columns will be added to.") ),

          fillRow( selectInput("dt", "Select Datatable",
                               choices = lt, selected = 1, width="100%")  ),

          fillRow( p("Choose which sample or group IDs the new data columns will be added to: ") ),

          fillRow( p("    ALL : A single ID, ALL, is used - will add all data to all sample IDs.") ),

          fillRow( p("    all-IDs : all sample IDs from selected datatable are used.") ),

          fillRow( p("    <group> : All IDs from the selected group are used.") ),

          fillRow( selectInput("id", "Select IDs or Groups",
                               choices = ltid, selected = 1, width="100%")  ),

          fillRow( p("Define the data column names - these MUST be unique to this new datatable.") ),

          fillRow(  textInput("data_cols", "Data Cols. to add (space-separated):", width="100%")  ),

          fillRow(  span( textOutput("warningName"), style="color:red")  )

        )
      )
    )

    server <- function(input, output, session) {

      observe({

        x <- input$dt # returns CHARACTER - cast to numeric to use as index!!

        #print( paste0("x: ", x) )

        if( is.null(x) ) {
          # do nothing
        } else {
          #print( paste0("x: ", x) )
           # must convert x to numeric as its returned as a character!
          gdt <- dplyr::select( dts[[ as.numeric(x) ]], dplyr::starts_with("group-") )
          ltid <- as.list(1:(length(gdt)+2) )
          names(ltid) <- c("ALL", "all-IDs", names(gdt))
          # update select input with new ltid:
          updateSelectInput(session, "id",
                          label = "Select IDs or Groups",
                          choices = ltid,
                          selected = 1
              )
        }
      })

      observeEvent(input$done, {

          col_name_warning <- ""

          # check the selected dt does not contain the newly defined data cols:
          dt_col_names <- names(dts[[input$dt]])

          # split data_cols at spaces
          data_cols <- unlist( strsplit(as.character(input$data_cols), ' ') )
          if( identical(data_cols, character(0) ) ) {
             data_cols <- ""
          }

          for(i in 1:length(data_cols) ) {
            # check dt doesnt already contain col of same name?
            if( any( dt_col_names == data_cols[i] ) ) {
              col_name_warning <- paste0("  Data col already exists in datatable: ", data_cols[i] )
            }
          }

          if(col_name_warning != "") {
            # set the warningName TextOutput:
            output$warningName <- renderText({
              col_name_warning
            })

          } else {

            if( identical(data_cols, character(0) ) ) {
              data_cols <- ""
            }

            # FIRST PARSE input$id :
             # generate a vector containing sample IDs, group IDs or ALL as selected
            if( as.numeric(input$id) == 1 ) { # ALL
               cat( "\n  group_vector : ALL" )
              ids_vector <- "ALL"
              group_vector <- ids_vector

            } else if( as.numeric(input$id) == 2 ) { # sample IDs
               cat( "\n  group_vector : ID" )
              ids_vector <- dts[[ as.numeric(input$dt) ]]$ID
              group_vector <- ids_vector
              cat( "\n  group_vector : ", group_vector )

            } else if( as.numeric(input$id) > 2 ) { # a group col has been selected

               cat( "\n  group_vector : COL" )
               # retrieve the group names AGAIN - as its not updated globally from previous observation function!
               gdt <- dplyr::select( dts[[ as.numeric(input$dt) ]], dplyr::starts_with("group-") )
               ltid <- as.list(1:(length(gdt)+2) )
               names(ltid) <- c("ALL", "all-IDs", names(gdt))
               # now get ids vector:
               ids_vector <- unique( dts[[ as.numeric(input$dt) ]][[ names(ltid)[ as.numeric(input$id) ] ]] )
               group_vector <- ids_vector
               cat( "\n  group_vector : ", group_vector )
               cat( "\n  ids_vector - COL : ", names(ltid)[ as.numeric(input$id) ] )
               cat( "\n  ids_vector : ", ids_vector, "  \n\n" )

            }


            if( as.numeric(input$type) == 1 ) {

              # sample-first
               # currently automatically uses all sample IDs
               # cannot choose any group IDs or special group ALL

               cat( "\npSAMPLE-FIRST LAYOUT")
               cat( "\npath: ", path)
               cat( "\nline: ", row)
               cat( "\ndata_cols: ", data_cols)
               cat( "\ndatatable_name: ", names(dts)[ as.numeric(input$dt) ])

              projectmanagr::datatable_add_data_samples_rmd(
                rmd_path = path,
                rmd_line = row,
                data_cols = data_cols,
                ids_vector = ids_vector,
                datatable_name =  names(dts)[ as.numeric(input$dt) ]
                )

            } else if( as.numeric(input$type) == 2 ) {

              # variable-first
              projectmanagr::datatable_add_data_variables_rmd(
                rmd_path = path,
                rmd_line = row,
                var_names = data_cols,
                datatable_name =  names(dts)[ as.numeric(input$dt) ],
                group_names = group_vector
              )

            } else if( as.numeric(input$type) == 3 ) {

              # timetable
              projectmanagr::datatable_add_data_timetable_rmd(
                rmd_path = path,
                rmd_line = row,
                step_names = data_cols,
                datatable_name =  names(dts)[ as.numeric(input$dt) ],
                group_names = group_vector
              )

            }

            # navigate to file - to reload:
            rstudioapi::navigateToFile( path )

            # Close Gadget after 'done' is clicked.
            stopApp()

          }
        })
    }


    viewer <- dialogViewer("Add Data to Datatable", width = 1000, height = 800)

    runGadget(ui, server, viewer = viewer)

}
