#' Add Datatable containing groups to existing samples
#'
#' Generates a Shiny Gadget for adding group assignment data to existing samples
#' in a new datatable.
#'
#' User selects a datatable name : list of existing datatables
#' in the current Rmd.
#'
#' User defines the data table column names and values : these MUST begin with
#' the prefix `group-` to define each group set.  Then the group codes are
#' entered, separated by spaces after the group column name.
#'
#' Multiple groups can be entered by repeating this layout.  The groups are
#' automatically distributed across the samples, which may need re-ordering.
#' The columns are spaced according to the max width of the group column names
#' and the group codes.
#'
#' A datatable with the function GROUP is added with sample-first layout.
#'
#' @export
addinDatatableAddGroups <- function() {

   # get data from current file
   context <- rstudioapi::getSourceEditorContext()
   row <- context$selection[[1]]$range$start[1]
   path <- normalizePath( context$path )

   # SAVE the document before processing!
   rstudioapi::documentSave(id = context$id)

   # create named list of types
   # type <- list("sample-first"=1, "variable-first"=2, "timetable"=3)
    # NOT NEEDED - Group Tables are ALWAYS SAMPLE-FIRST

   # collect all sample data from the current Rmd:
   dts <- projectmanagr::datatable_read_rmd(path)
   dt_names <- names(dts)

   # use for selectInput object:
   lt <- as.list( 1:length(dt_names) )
   names(lt) <- dt_names

   # select group cols from first datatable
   #gdt <- dplyr::select(dts[[1]], dplyr::starts_with("group-"))
   #ltid <- as.list(1:(length(gdt)+2) )
   #names(ltid) <- c("ALL", "all-IDs", names(gdt))
    # NOT NEEDED - Group Tables will always use all-IDs

    ui <- miniPage(

      gadgetTitleBar("Add Groups to Datatable"),

      miniContentPanel(

        fillCol(

          fillRow( p("Add Group Data to a datatable in the active Rmd at the cursor from input params.") ),

          fillRow(
            helpText( p('Rmd: ', context$path, align="center") ) ),

          fillRow(
            helpText( p('line: ', row, align="center") ) ),

          fillRow( p("Choose which datatable the new group data columns will be added to.") ),

          fillRow( selectInput("dt", "Select Datatable",
                               choices = lt, selected = 1, width="100%")  ),

          fillRow( p("Define the group data column names and values - these MUST be unique to the selecred datatable.") ),

          fillRow( p("  Each group column name must begin with `group-`, then space-separated group names are given.") ),

          fillRow( p("  Repeat for all groups.  eg. group-solvent-inc MeOH-DCM 1P group-ab-inc-conc 1mg/mL 0.5mg/mL") ),

          fillRow(  textInput("data_cols", "Group Data Cols. to add (space-separated):", width="100%")  ),

          fillRow(  span( textOutput("warningName"), style="color:red")  )

        )
      )
    )

    server <- function(input, output, session) {

      observeEvent(input$done, {

          col_name_warning <- ""
          # check the selected dt does not contain the defined data cols:
          dt_col_names <- names(dts[[input$dt]])

          for(i in 1:length(input$data_cols) ) {

            # check dt doesnt already contain col of same name?
            if( any( dt_col_names == input$data_cols[i] ) ) {
              col_name_warning <- paste0("  Data col already exists in datatable: ", input$data_cols[i] )
            }
          }

          if(col_name_warning != "") {

            # set the warningName TextOutput:
            output$warningName <- renderText({
              col_name_warning
            })

          } else if( regexpr("group-", input$data_cols) == -1 ) {

             # set the warningName TextOutput:
             output$warningName <- renderText({
                "Group Col definition does not contain a `group-` col header!"
             })

          } else {

            # split data_cols at the `group-` value
            group_cols <- paste0("group-", unlist( strsplit(as.character(input$data_cols), 'group-') ) )
            # remove FIRST index
            group_cols <- group_cols[2:length(group_cols)]
            # NOW split group_cols at spaces
            group_cols <- strsplit(as.character(group_cols), ' ')

            # create group_names and group_values
            group_names <- unlist(lapply(group_cols, function(l) l[[1]]))
            group_values <- lapply(group_cols, function(l) l[2:length(l)])

             projectmanagr::datatable_add_group_rmd(
                rmd_path = path,
                rmd_line = row,
                group_names = group_names,
                datatable_name =  names(dts)[ as.numeric(input$dt) ],
                groups = group_values
                )

            # navigate to file - to reload:
            rstudioapi::navigateToFile( path )

            # Close Gadget after 'done' is clicked.
            stopApp()

          }
        })
    }


    viewer <- dialogViewer("Add Groups to Datatable", width = 1000, height = 800)

    runGadget(ui, server, viewer = viewer)

}
