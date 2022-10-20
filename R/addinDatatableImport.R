#' Import EXISTING samples and reps from Datatable in source Project Note
#'
#' Generates a Shiny Gadget for importing existing samples and reps derived from
#' datatables that exist in a source Project Note into the currently active
#' destination Project Note.
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
#'     as needed in the source note, pointing to the destination Project Note
#'
#'   + IMPORTED to the destination Project Note : Import datatable is written
#'     which includes the samples/reps selected, pointing to the source Project
#'     Note.
#'
#'
#' @export
addinDatatableImport <- function() {

  # get data from current file - DESTINATION FILES
  context <- rstudioapi::getSourceEditorContext()
  row <- context$selection[[1]]$range$start[1]
  path <- normalizePath( context$path )

  # SAVE the document before processing!
  rstudioapi::documentSave(id = context$id)

  # collect the organisation path from current path
  orgPath <- findOrgDir(path)

  # generate a blank table to initialise addin with
  ID <- ""      # ID: Each Sample ID
  SAMPLE <- ""  # SAMPLE: COMPOSITE of all subsampling columns: CNS-RT-MB etc.
  COUNT <- integer()  # COUNT: How many REPS are there of this sample?
  PREFIX <- ""     # EXP: Fill with the Experiment Prefix ID
  TITLE <- ""   # TITLE: Fill with Experiment Title - will contain the LAB_TREATMENT
  #PATH <- ""    # PATH: Put the absolute PATH to the Project Note Rmd to Navigate to
  LOCATION <- "" # LOCATION of the sample, if in storage
  CONDITION <- "" # CONDITIONS of sample - what is it in?
  IMPORT <- integer()  # IMPORT: How many REPS to import from this sample?

  #samples_summary <- tibble::tibble(ID, SAMPLE, COUNT, PREFIX, TITLE, LOCATION, CONDITION)
  samples_summary <- tibble::tibble(PREFIX, TITLE, ID, SAMPLE, COUNT, CONDITION, LOCATION, IMPORT)

  ui <- miniPage(

    gadgetTitleBar("Import Samples"),

    miniContentPanel(

      fillCol( flex = c(1,1,1,20),
             # , verbatimTextOutput("rows", placeholder = TRUE)
        fillRow( p("Import samples/reps from datatables in a Source Project Note"), actionButton("click_action", "Click") ),

        #fillRow(
        #  helpText( p('Rmd: ', context$path, align="center") ) ),

        #fillRow(
        #  helpText( p('line: ', row, align="center") ) ),

        fillRow( flex = c(7, 1),  verbatimTextOutput("dirtxt", placeholder = TRUE), shinyDirButton("dir", "Select Directory", "Note Parent Directory")  ),

        fillRow(   span( textOutput("warningDirectory"), style="color:red")  ),

        #fillRow( p("Choose which datatable to import samples from") ),

        #fillRow( selectInput("dt", "Select Datatable",
        #                     choices = lt, selected = 1, width="100%")  ),

        fillRow ( DT::dataTableOutput("mytable1", height = "100%") )
      )
    )
  )

  server <- function(input, output, session) {

    # compute Dir selection:
    global <- reactiveValues(datapath = dirname(path),
                             summary = samples_summary) # this sets initial val to current working DIR

    # allows selection of Dir, with Volume set to HOME Dir
    shinyDirChoose(
      input,
      'dir',
      roots = c(home = orgPath), # set to orgPath - so user can select any DIR inside the ORG!
      filetypes = c('', 'Rmd')
    )

    dir <- reactive(input$dir)

    observeEvent(ignoreNULL = TRUE,
                 eventExpr = {
                   input$dir
                 },
                 handlerExpr = {
                   if (!"path" %in% names(dir())) return() # check the path element exists in dir
                   #home <- normalizePath("~")
                   global$datapath <-
                     file.path(orgPath, paste(unlist(dir()$path[-1]), collapse = .Platform$file.sep))
                 })




    observe({
      if(global$datapath != "") { # dir MUST be in a dir in a Programme

        #global$datapath <- checkProgSubDir(global$datapath)
        global$datapath <- global$datapath

        # Create a Progress object
        progress <- shiny::Progress$new()
        progress$set(message = "Computing data", value = 0)
        # Close the progress when this reactive exits (even if there's an error)
        on.exit(progress$close())

        # Create a callback function to update progress.
        # Each time this is called:
        # - If `value` is NULL, it will move the progress bar 1/5 of the remaining
        #   distance. If non-NULL, it will set the progress to that value.
        # - It also accepts optional detail text.
        updateProgress <- function(value = NULL, detail = NULL) {
          #if (is.null(value)) {
          #  value <- progress$getValue()
          #  value <- value + (progress$getMax() - value) / 5
          #}
          progress$set(value = value, detail = detail)
        }
        global$summary <- projectmanagr::datatable_find(global$datapath, updateProgress = updateProgress)
      }
    })

    output$dirtxt <- renderText({
      global$datapath
    })


    observe({
      if(global$datapath == "") {
        output$warningDirectory <- renderText({
          "DIR PATH NOT VALID DIRECTORY - Must be INSIDE a PROGRAMME DIR"
        })
      }
      else {
        output$warningDirectory <- renderText({
          ""
        })
      }
    })


    output$mytable1 <- DT::renderDT( global$summary,
                                     selection = 'none',
                                     class = 'cell-border',
                                     #editable = list(target = "cell", disable = list(columns = c(1:7))),
                                     editable = TRUE,
                                     filter = 'top',
                                     caption = 'Sample Summary Table',
                                     fillContainer = TRUE,
                                     options = list(
                                       lengthMenu = c(20, 50, 100),
                                        pageLength = 20,
                                        columnDefs = list(list(className = 'dt-center', targets = c(1:8))) ),
                                     server = FALSE ) # processing on client-side means edits to IMPORT col are kept when searching the table

    # edit a column - this does not work without creating a JSON error in the gadget
    #observeEvent(input$mytable1_cell_edit, {
    #  global$summary <<- DT::editData(global$summary, input$mytable1_cell_edit, 'mytable1')
    #})

    # this edits the original table correctly - but only when editing by CELL - editing by row or column does not complete
     # therefore does not set input$mytable1_cell_edit
    observeEvent(input$mytable1_cell_edit, {
      global$summary[input$mytable1_cell_edit$row,input$mytable1_cell_edit$col] <<- input$mytable1_cell_edit$value
    })



    # perform computations to export & import samples from selected samples/reps
    observeEvent(input$done, {

      # maek sure IMPORT col is written to summary
      #global$summary <<- DT::editData(global$summary, input$mytable1_cell_edit, 'mytable1')

      # filter to remove all rows where IMPORT is 0
      samp_summ <- dplyr::filter(global$summary, IMPORT > 0 )

      cur_datetime <- projectmanagr::get_current_datetime_string()

      # export samples from SOURCE Rmds
      export_dts <- projectmanagr::datatable_export(samples_summary = samp_summ,
                                        destination_note_path = path,
                                        datetime=cur_datetime,
                                        summarise_reps = TRUE, dt_length = 120)

      # AND import samples from SOURCE Rmds to DESTINATION Rmd
      projectmanagr::datatable_import(export_datatables = export_dts,
                                      destination_note_path = path,
                                      destination_note_line = row,
                                      datetime=cur_datetime,
                                      summarise_reps = FALSE, dt_length = 120)

      # Close Gadget after computations are complete:
      stopApp()

      print(global$summary)
      print(samp_summ)


    })

  }

  runGadget(ui, server, viewer = dialogViewer("Import Samples", width = 2000, height = 2000))

}





viewDT <- function() {


  # get data from current file - DESTINATION FILES
  context <- rstudioapi::getSourceEditorContext()
  row <- context$selection[[1]]$range$start[1]
  path <- normalizePath( context$path )

  # SAVE the document before processing!
  rstudioapi::documentSave(id = context$id)

  # collect the organisation path from current path
  orgPath <- findOrgDir(path)

  # generate a blank table to initialise addin with
  ID <- ""      # ID: Each Sample ID
  SAMPLE <- ""  # SAMPLE: COMPOSITE of all subsampling columns: CNS-RT-MB etc.
  COUNT <- integer()  # COUNT: How many REPS are there of this sample?
  PREFIX <- ""     # EXP: Fill with the Experiment Prefix ID
  TITLE <- ""   # TITLE: Fill with Experiment Title - will contain the LAB_TREATMENT
  #PATH <- ""    # PATH: Put the absolute PATH to the Project Note Rmd to Navigate to
  LOCATION <- "" # LOCATION of the sample, if in storage
  CONDITION <- "" # CONDITIONS of sample - what is it in?

  #samples_summary <- tibble::tibble(ID, SAMPLE, COUNT, PREFIX, TITLE, LOCATION, CONDITION)
  samples_summary <- tibble::tibble(PREFIX, TITLE, ID, SAMPLE, COUNT, CONDITION, LOCATION)


  ui <- miniPage(

    gadgetTitleBar("Import Samples"),

    miniContentPanel(

      fillCol(

        fillRow( p("Import samples/reps from datatables in a Source Project Note") ),

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
    global <- reactiveValues(datapath = dirname(path) ) # this sets initial val to organisation DIR

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
