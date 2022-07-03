#' Import EXISTING samples and reps from Datatable in source Project Note
#'
#' Generates a Shiny Gadget for importing existing samples and reps derived from
#' datatables that exist in a source Project Note into the currently active
#' desintation Project Note.
#'
#' * User selects the line in the current Project Note where the IMPORT will
#' be written.
#'
#' * User selects a source Project Note, from the current ORGANISATION: This
#' note is read for all EXISTING samples/reps in all datatables, which are
#' presented in SUMMARY FORMAT
#'
#' * User filters the datatable for desired characteristics of samples/reps for
#' import.
#'
#' * User selects the samples by clicking on the rows in the datatable.
#'
#' * User selects reps by filling in the rep number in the left hand column of
#' selected sample rows.
#'
#' * User selects DONE : samples/reps are first
#'
#'   + EXPORTED from the source Project Note : Export datatable is updated/written
#'     as needed in the source note, pointing to the desintation Project Note
#'
#'   + IMPORTED to the destination Project Note : Import datatable is written
#'     which includes the samples/reps selected, pointing to the source Project
#'     Note.
#'
#'
#' @export
addinDatatableImport <- function() {

   # get data from current file
   context <- rstudioapi::getSourceEditorContext()
   row <- context$selection[[1]]$range$start[1]
   path <- normalizePath( context$path )

   # SAVE the document before processing!
   rstudioapi::documentSave(id = context$id)

   # collect the organisation path from current path
   orgPath <- findOrgDir(path)


    ui <- miniPage(

      gadgetTitleBar("Resample Datatable"),

      miniContentPanel(

        fillCol(

          fillRow( p("Import samples/reps from datatables in a Project Note") ),

          fillRow( flex = c(7, 1),  verbatimTextOutput("rmd", placeholder = TRUE), shinyDirButton("rmd", "Select Project Note", "Organisation Parent Directory")  ),

          fillRow(   span( textOutput("warningDirectory"), style="color:red")  ),


          fillRow(
            helpText( p('Rmd: ', context$path, align="center") ) ),

          fillRow(
            helpText( p('line: ', row, align="center") ) ),

          fillRow( p("Choose which datatable to import samples from") ),

          fillRow( selectInput("dt", "Select Datatable",
                               choices = lt, selected = 1, width="100%")  ),

          DT::dataTableOutput("mytable1", height = "100%")

        )
      )
    )

    server <- function(input, output, session) {


       # compute Dir selection:
       global <- reactiveValues(datapath = findOrgDir( normalizePath(getwd()) )  ) # this sets initial val to organisation DIR

       # allows selection of Dir, with Volume set to HOME Dir
       shinyFileChoose(
          input,
          'rmd',
          roots = c(home = '~'),
          filetypes = c('', 'Rmd')
       )

       rmd <- reactive(input$rmd)

       output$rmd <- renderText({
          global$datapath
       })

       observeEvent(ignoreNULL = TRUE,
                    eventExpr = {
                       input$rmfd
                    },
                    handlerExpr = {
                       if (!"path" %in% names(rmd())) return()
                       home <- normalizePath("~")
                       global$datapath <-
                          file.path(home, paste(unlist(rmd()$path[-1]), collapse = .Platform$file.sep))
                    })


       observe({
          if(global$datapath == "") {
             output$warningDirectory <- renderText({
                "RMD PATH NOT VALID ORGANISATION"
             })
          }
          else {
             output$warningDirectory <- renderText({
                ""
             })
          }
       })


       # collect all sample data from the current Rmd:
       dts <- projectmanagr::datatable_read_rmd(rmd)
       dt_names <- names(dts)

       lt <- as.list( 1:length(dt_names) )
       names(lt) <- dt_names


       output$mytable1 <- DT::renderDataTable({

          table <- DT::datatable(data,
                                 extensions = c('Buttons','Scroller'),
                                 options = list(dom = 'BrtS',
                                                buttons = I('colvis'),
                                                scrollY = 200,
                                                deferRender = TRUE,
                                                paging = TRUE
                                 )#,
                                 #fillContainer = TRUE
          )
       })

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
