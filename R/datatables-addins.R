#' Create Datatable from File
#'
#' Generates a Shiny Gadget for initialising
#' a new datatable from ID list, plus optional cols with vals.  A datatable
#' is formed and inserted in current Rmd at the current line.
#'
#' @export
addin_datatable_create <- function() {


  #### instance variables ####

  # get data from current file
  context <- rstudioapi::getSourceEditorContext()
  row <- context$selection[[1]]$range$start[1]
  path <- normalizePath( context$path )

  # SAVE the document before processing!
  rstudioapi::documentSave(id = context$id)

  WD <- path

  # identify the orgPath from current working directory - to retrieve the settings yaml file
  orgPath <- find_org_directory( WD )

  # if orgPath not identified present error interface and then stop this function
  if(orgPath=="") {
    addin_error_path("Create Datatable",
                     "No Organisation identified - ensure working directory is in an Organisation.",
                     orgPath)
    stop( paste0("  No Organisation identified - ensure working directory is in an Organisation: \n    ", WD))
  }

  # get config templates settings yml
  confPath <- get_config_dir(orgPath)
  tempPath <- get_template_dir(orgPath)
  settings <- get_settings_yml(orgPath)


  # identify the lines that begin with "+==="
  indices <- which( startsWith( context$contents, "+===") )
  indices_row <- which( startsWith( context$contents[1:row], "+===") )

  # from indices determine if the cursor is sitting IN or OUT of a datatable


  #### IF CURSOR IN DATATABLE : TEMPLATE ####

  if( (length(indices_row) %% 2) == 1 ) {


    #### Check DT is TEMPLATE CREATE ####

    # cursor is sitting IN a datatable - potentially expanding a CREATE TEMPLATE

    # check the datatable is complete - it has an end
    if(length(indices) <= length(indices_row) ) {
      stop( paste0("  Cannot identify next datatable separator - check syntax?") )
    }

    # get the template_table:
    startrow <- indices_row[length(indices_row)]
    endrow <- indices[length(indices_row)+1]
    template_table <- context$contents[ startrow : endrow ]

    # get current table name function & possible output name
    table_name_function_vector <- trimws(strsplit( as.character(template_table[4]), ":")[[1]])

    table_name <- table_name_function_vector[1]
    table_function <- table_name_function_vector[2]

    # confirm table is named/of type TEMPLATE
    if(table_name != "TEMPLATE") {
      stop( paste0("  Cursor is inside a datatable that is not a TEMPLATE - rename table or move cursor!") )
    }

    # confirm table function is CREATE
    if(table_function != "CREATE") {
      stop( paste0("  Cursor is inside a datatable that is not of function CREATE - use appropriate `addin_datatable` function: ", table_function) )
    }

    # After these basic checks, now can generate shiny gadget for user to provide input

    # generate separtate gadgets depending on whether the output name is given
    table_output_name_provided <- FALSE
    if( length(table_name_function_vector) > 2 ) {
      # if a third index exists this is the output_name - given after `TEMPLATE : CREATE : {{template_name}}`
      table_output_name_provided <- TRUE
    }


    if(table_output_name_provided == TRUE) {


      ##### datatable output name provided ####

      # so provide user interface for user to select EXISTING datatable to re-use IDs from in this new datatable

      # note the current name CAN exist, but any new IDs that are generated should NOT already exist in that datatable

      # create instance variables : existing datatable list for selection

      # output_table_name is the third index - passed to create dt function
      output_table_name <- table_name_function_vector[3]

      # collect all sample data from the current Rmd UP TO TEMPLATE
      #dts <- projectmanagr::datatable_read_rmd(path)
      # collect all sample data from the current Rmd up to SELECTED LINE
      dts <- projectmanagr::datatable_read_vector(context$contents[1:(startrow-1)]) # -1 to select line just BEFORE TEMPLATE dt
      dt_names <- names(dts)

      lt <- as.list( 1:length(dt_names) )
      names(lt) <- dt_names

      ##### user interface #####

      ui <- miniPage(

        gadgetTitleBar("Create Datatable from Template using <<IDS>> from existing datatable"),

        miniContentPanel(

          fillCol(

            fillRow( p("Create a datatable in the active Rmd in the selected Template using <<IDS>> from existing datatable.") ),

            fillRow(
              helpText( p('Rmd: ', context$path, align="center") ) ),

            fillRow(
              helpText( p('Start line: ', startrow, align="center") ),
              helpText( p('End line: ', endrow, align="center") )),

            fillRow( h4("Choose datatable:") ),

            fillRow( selectInput("dt", "Select Datatable",
                                 choices = lt, selected = 1, width="100%")  ),

            fillRow(  textInput("name", "New Datatable name:", value = output_table_name, width="100%")  ),

            fillRow(  span( textOutput("warningName2"), style="color:red")  )

          )
        )
      )


      ##### server code #####

      server <- function(input, output, session) {

        observeEvent(input$done, {

          # get the INDEX of selection of existing dt
          x <- input$dt # returns CHARACTER - cast to numeric to use as index!!

          # get selected dt
          dt_selected <- dts[[ as.numeric(x) ]]

          # get ids from dt_selected
          ids <- dt_selected$ID


          #### datatable create template rmd ####

          # create datatable from params and put into path at row
          projectmanagr::datatable_create_template_rmd(
                            rmd_path = path,
                            rmd_startline = startrow,
                            rmd_endline = endrow,
                            settings = settings,
                            IDs = ids,
                            datatable_name = input$name,
                            dt_length = 100
                          )

          # Close Gadget after 'done' is clicked.
          stopApp()

        })
      }


      ##### view gadget #####

      viewer <- dialogViewer("Create Datatable",
                             width = settings[["GadgetWidth"]],
                             height = settings[["GadgetHeight"]])
      runGadget(ui, server, viewer = viewer)


    } else { # else no datatable output name provided


      ###### no datatable output name provided ######

      # so provide user interface for user to give datatable name, and set of space-separated IDs


      ##### user interface #####

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


      ##### server code #####

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


            #### datatable create template rmd ####

            # create datatable from params and put into path at row
            projectmanagr::datatable_create_template_rmd(
                                      rmd_path = path,
                                      rmd_startline = startrow,
                                      rmd_endline = endrow,
                                      settings = settings,
                                      IDs = ids,
                                      datatable_name = input$name,
                                      dt_length = 100
                                  )

            # Close Gadget after 'done' is clicked.
            stopApp()

          }
        })
      }


      ##### view gadget #####

      viewer <- dialogViewer("Create Datatable",
                             width = settings[["GadgetWidth"]],
                             height = settings[["GadgetHeight"]])
      runGadget(ui, server, viewer = viewer)

    } # end else no datatable output name provided

  } else { # cursor lays OUTSIDE of a datatable - here will just generate shiny gadget to CREATE a datatable:


    #### ELSE CURSOR OUT DATATABLE : Create Datatable ####


    ##### user interface #####

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


    ##### server code ####

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


          #### datatable create rmd ####

          # create datatable from params and put into path at row
          projectmanagr::datatable_create_rmd(
                                      rmd_path = path,
                                      rmd_line = row,
                                      settings = settings,
                                      IDs = ids,
                                      data_cols = data_cols,
                                      datatable_name = input$name,
                                      dt_length = 100
                                    )

          # Close Gadget after 'done' is clicked.
          stopApp()

        }
      })
    }


    ##### view gadget ####

    viewer <- dialogViewer("Create Datatable",
                           width = settings[["GadgetWidth"]],
                           height = settings[["GadgetHeight"]])
    runGadget(ui, server, viewer = viewer)

  }

} #### _______________________________________________ ####



#' Create Datatable from File
#'
#' Generates a Shiny Gadget for initialising
#' a new datatable from file.  User selects a csv file from
#' the file system, and this is converted to datatable format and inserted
#' in current Rmd at the current line.
#'
#' @export
addin_datatable_create_from_file <- function() {

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

  #### view gadget ####
  viewer <- dialogViewer("Create Datatable from File", width = 1000, height = 800)
  runGadget(ui, server, viewer = viewer)

} #### _______________________________________________ ####



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
addin_datatable_add_data <- function() {


  #### instance variables ####

  # get data from current file
  context <- rstudioapi::getSourceEditorContext()
  row <- context$selection[[1]]$range$start[1]
  path <- normalizePath( context$path )

  # SAVE the document before processing!
  rstudioapi::documentSave(id = context$id)

  WD <- path

  # identify the orgPath from current working directory - to retrieve the settings yaml file
  orgPath <- find_org_directory( WD )

  # if orgPath not identified present error interface and then stop this function
  if(orgPath=="") {
    addin_error_path("Add Data Datatable",
                     "No Organisation identified - ensure working directory is in an Organisation.",
                     orgPath)
    stop( paste0("  No Organisation identified - ensure working directory is in an Organisation: \n    ", WD))
  }

  # get config templates settings yml
  confPath <- get_config_dir(orgPath)
  tempPath <- get_template_dir(orgPath)
  settings <- get_settings_yml(orgPath)

  # identify the lines that begin with "+==="
  indices <- which( startsWith( context$contents, "+===") )
  indices_row <- which( startsWith( context$contents[1:row], "+===") )

  # from indices determine if the cursor is sitting IN or OUT of a datatable


  #### IF CURSOR IN DATATABLE : TEMPLATE ####

  if( (length(indices_row) %% 2) == 1 ) {


    ##### Check DT is TEMPLATE ADD_DATA ####

    # cursor is sitting IN a datatable - potentially expanding a TEMPLATE datatable ADD_DATA

    # check the datatable is complete - it has an end
    if(length(indices) <= length(indices_row) ) {
      stop( paste0("  Cannot identify next datatable separator - check syntax?") )
    }

    # get the template_table:
    startrow <- indices_row[length(indices_row)]
    endrow <- indices[length(indices_row)+1]
    template_table <- context$contents[ startrow : endrow ]

    table_name <- trimws(strsplit( as.character(template_table[4]), ":")[[1]][1])
    table_function <- trimws(strsplit( as.character(template_table[4]), ":")[[1]][2])

    # confirm table is named/of type TEMPLATE
    if(table_name != "TEMPLATE") {
      stop( paste0("  Cursor is inside a datatable that is not a TEMPLATE - rename table or move cursor!") )
    }

    # confirm table function is ADD_DATA
    if(table_function != "ADD_DATA") {
      stop( paste0("  Cursor is inside a datatable that is not of function ADD_DATA - use appropriate `addin_datatable` function: ", table_function) )
    }

    # After these basic checks, now can generate shiny gadget for user to provide input

    # create instance variables : existing datatable list for selection

    # collect all sample data from the current Rmd UP TO TEMPLATE
    #dts <- projectmanagr::datatable_read_rmd(path)
    # collect all sample data from the current Rmd up to SELECTED LINE
    dts <- projectmanagr::datatable_read_vector(context$contents[1:(startrow-1)]) # -1 to select line just BEFORE TEMPLATE dt
    dt_names <- names(dts)

    lt <- as.list( 1:length(dt_names) )
    names(lt) <- dt_names

    # select group cols from first datatable INITIALLY
    gdt <- dplyr::select( dts[[1]], dplyr::starts_with("group-") )
    ltid <- as.list(1:(length(gdt)+2) )
    names(ltid) <- c("ALL", "all-IDs", names(gdt))


    #### user interface #####

    ui <- miniPage(

      gadgetTitleBar("Add Data to Datatable Template from Existing Table"),

      miniContentPanel(

        fillCol(

          fillRow( p("Add IDs from existing an datatable to the datatable template in the active Rmd.") ),

          fillRow(
            helpText( p('Rmd: ', context$path, align="center") ) ),

          fillRow(
            helpText( p('Start line: ', startrow, align="center") ),
            helpText( p('End line: ', endrow, align="center") )),

          fillRow( h4("Choose datatable:") ),

          fillRow( selectInput("dt", "Select Datatable",
                               choices = lt, selected = 1, width="100%")  ),

          # for template only support selection of IDs
          #fillRow( h4("Choose sample/group IDs: ") ),

          #fillRow( p("    ALL : A single ID, ALL, is used - will add all data to all sample IDs.") ),

          #fillRow( p("    all-IDs : all sample IDs from selected datatable are used.") ),

          #fillRow( p("    <group> : All IDs from the selected group are used.") ),

          #fillRow( selectInput("id", "Select IDs or Groups",
           #                    choices = ltid, selected = 1, width="100%")  ),

          fillRow( checkboxInput("summarise_reps", "Summarise Sample Reps"))
        )
      )
    )


    #### server code ####

    server <- function(input, output, session) {

      observeEvent(input$done, {

        # get the INDEX of selection
        x <- input$dt # returns CHARACTER - cast to numeric to use as index!!

        # get selected dt
        dt_selected <- dts[[ as.numeric(x) ]]

        # get ids from dt_selected - only support selection of all IDs
        ids <- dt_selected$ID

        # get name of dt - from index
        dt_name <- dt_names[[ as.numeric(x) ]]

        #### datatable add data samples template rmd ####

        # create datatable from params and put into path at row
        projectmanagr::datatable_add_data_samples_template_rmd(
                                rmd_path = path,
                                rmd_startline = startrow,
                                rmd_endline = endrow,
                                IDs = ids,
                                datatable_name = dt_name,
                                settings = settings,
                                dt_length = 100,
                                summarise_reps = input$summarise_reps
                              )

        # Close Gadget after 'done' is clicked.
        stopApp()

      })
    }


    #### view gadget ####

    viewer <- dialogViewer("Add Data to Datatable",
                           width = settings[["GadgetWidth"]],
                           height = settings[["GadgetHeight"]])
    runGadget(ui, server, viewer = viewer)


  } else { # cursor lays OUTSIDE of a datatable - here will just generate shiny gadget to CREATE a datatable:


    #### ELSE CURSOR OUT DATATABLE : Create Datatable ####

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


    ##### user interface #####

    ui <- miniPage(

      gadgetTitleBar("Add Data Datatable"),

      miniContentPanel(

        fillCol(

          fillRow( p("Add Data to a datatable in the active Rmd at the cursor from input params.") ),

          fillRow(
            helpText( p('Rmd: ', context$path, align="center") ) ),

          fillRow(
            helpText( p('line: ', row, align="center") ) ),

          fillRow( h4("Choose datatable:") ),

          fillRow( selectInput("dt", "Select Datatable",
                               choices = lt, selected = 1, width="100%")  ),

          fillRow( h4("Choose datatable type: ") ),

          fillRow( p("sample-first: First col. is filled with all sample or group IDs, subsequent columns are data cols. specified below.") ),

          fillRow( p("variable-first: First col. is filled with all data cols. specified below, subsequent columns are sample or group IDs") ),

          fillRow( p("timetable: special table for planning and recording datetimes of a procedure's steps executed with different timing across sample or group IDs.") ),

          fillRow( selectInput("type", "Select Datatable Type",
                               choices = type, selected = 1, width="100%")  ),

          fillRow( h4("Choose sample/group IDs: ") ),

          fillRow( p("    ALL : A single ID, ALL, is used - will add all data to all sample IDs.") ),

          fillRow( p("    all-IDs : all sample IDs from selected datatable are used.") ),

          fillRow( p("    <group> : All IDs from the selected group are used.") ),

          fillRow( selectInput("id", "Select IDs or Groups",
                               choices = ltid, selected = 1, width="100%")  ),

          fillRow( p("Define data column names - these MUST be unique to this new datatable.") ),

          fillRow(  textInput("data_cols", "Data Cols. to add (space-separated):", width="100%")  ),

          fillRow(  span( textOutput("warningName"), style="color:red")  ),

          fillRow( checkboxInput("summarise_reps", "Summarise Sample Reps"))

        )
      )
    )


    #### server code ####

    server <- function(input, output, session) {

      # modify group IDs if new dt is selected
      observe({

        x <- input$dt # returns CHARACTER - cast to numeric to use as index!!

        print( paste0("x: ", x) )

        if( is.null(x) ) {
          # do nothing
        } else {
          print( paste0("x: ", x) )
          print( paste0( "x type: ", typeof(x)))
          print(dts)
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

      # modify group IDs if different dt type is selected
      # variable-first does NOT support all-IDs anymore!
      observe({

        x <- input$type # returns CHARACTER - cast to numeric to use as index!!

        #print( paste0("x: ", x) )

        if( is.null(x) ) {
          # do nothing
        } else {
          xn <- as.numeric(x)
          if( xn == 2 || xn == 3 ) {
            # must convert input$dt to numeric as its returned as a character!
            gdt <- dplyr::select( dts[[ as.numeric(input$dt) ]], dplyr::starts_with("group-") )
            ltid <- as.list(1:(length(gdt)+1) )
            names(ltid) <- c("ALL", names(gdt))
            # update select input with new ltid:
            updateSelectInput(session, "id",
                              label = "Select IDs or Groups",
                              choices = ltid,
                              selected = 1
            )
          } else {
            # must convert input$dt to numeric as its returned as a character!
            gdt <- dplyr::select( dts[[ as.numeric(input$dt) ]], dplyr::starts_with("group-") )
            ltid <- as.list(1:(length(gdt)+2) )
            names(ltid) <- c("ALL", "all-IDs", names(gdt))
            # update select input with new ltid:
            updateSelectInput(session, "id",
                              label = "Select IDs or Groups",
                              choices = ltid,
                              selected = 1
            )
          }
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

          } else if( as.numeric(input$id) == 2 ) { # names(ltid)[as.numeric(input$id)] == "all-IDs" ) { # sample IDs

            if( as.numeric(input$type) == 1 ) { # all IDs can only be selected for sample-first

              cat( "\n  group_vector : ID" )
              ids_vector <- dts[[ as.numeric(input$dt) ]]$ID
              group_vector <- ids_vector
              cat( "\n  group_vector : ", group_vector )

            } else { # grab the group col for variable-first or timetable

              # retrieve the group names AGAIN - as its not updated globally from previous observation function!
              gdt <- dplyr::select( dts[[ as.numeric(input$dt) ]], dplyr::starts_with("group-") )
              ltid <- as.list(1:(length(gdt)+1) )
              names(ltid) <- c("ALL", names(gdt))
              # now get ids vector:
              ids_vector <- unique( dts[[ as.numeric(input$dt) ]][[ names(ltid)[ as.numeric(input$id) ] ]] )
              group_vector <- ids_vector
              cat( "\n  group_vector : ", group_vector )
              cat( "\n  ids_vector - COL : ", names(ltid)[ as.numeric(input$id) ] )
              cat( "\n  ids_vector : ", ids_vector, "  \n\n" )

            }

          } else { # a group col has been selected

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


            #### datatable add data samples rmd ####
            # sample-first
            # currently automatically uses all sample IDs
            # cannot choose any group IDs or special group ALL

            cat( "\nSAMPLE-FIRST LAYOUT")
            cat( "\npath: ", path)
            cat( "\nline: ", row)
            cat( "\ndata_cols: ", data_cols)
            cat( "\ndatatable_name: ", names(dts)[ as.numeric(input$dt) ])

            projectmanagr::datatable_add_data_samples_rmd(
              rmd_path = path,
              rmd_line = row,
              data_cols = data_cols,
              datatable_name =  names(dts)[ as.numeric(input$dt) ],
              settings = settings,
              ids_vector = ids_vector,
              dt_length = 100,
              summarise_reps = input$summarise_reps
            )

          } else if( as.numeric(input$type) == 2 ) {

            cat( "\nVARIABLE-FIRST LAYOUT")
            cat( "\npath: ", path)
            cat( "\nline: ", row)
            cat( "\ndata_cols: ", data_cols)
            cat( "\ndatatable_name: ", names(dts)[ as.numeric(input$dt) ])
            cat( "\ngroup_names: ", group_vector)

            #### datatable add data variables rmd ####
            # variable-first
            projectmanagr::datatable_add_data_variables_rmd(
              rmd_path = path,
              rmd_line = row,
              var_names = data_cols,
              datatable_name =  names(dts)[ as.numeric(input$dt) ],
              group_names = group_vector,
              settings = settings,
              dt_length = 100
            )

          } else if( as.numeric(input$type) == 3 ) {

            #### datatable add data timetable rmd ####
            projectmanagr::datatable_add_data_timetable_rmd(
              rmd_path = path,
              rmd_line = row,
              step_names = data_cols,
              datatable_name =  names(dts)[ as.numeric(input$dt) ],
              group_names = group_vector,
              settings = settings,
              dt_length = 100
            )

          }

          # navigate to file - to reload:
          rstudioapi::navigateToFile( path )

          # Close Gadget after 'done' is clicked.
          stopApp()

        }
      })
    }


    #### view gadget ####

    viewer <- dialogViewer("Add Data to Datatable",
                           width = settings[["GadgetWidth"]],
                           height = settings[["GadgetHeight"]])
    runGadget(ui, server, viewer = viewer)

  }
} #### _______________________________________________ ####




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
addin_datatable_add_groups <- function() {


  #### instance variables ####

  # get data from current file
  context <- rstudioapi::getSourceEditorContext()
  row <- context$selection[[1]]$range$start[1]
  rowStart <- context$selection[[1]]$range$start[1]
  rowEnd <- context$selection[[1]]$range$end[1]
  path <- normalizePath( context$path )

  # SAVE the document before processing!
  rstudioapi::documentSave(id = context$id)

  WD <- path

  # identify the orgPath from current working directory - to retrieve the settings yaml file
  orgPath <- find_org_directory( WD )

  # if orgPath not identified present error interface and then stop this function
  if(orgPath=="") {
    addin_error_path("Group Datatable",
                     "No Organisation identified - ensure working directory is in an Organisation.",
                     orgPath)
    stop( paste0("  No Organisation identified - ensure working directory is in an Organisation: \n    ", WD))
  }

  # get config templates settings yml
  confPath <- get_config_dir(orgPath)
  tempPath <- get_template_dir(orgPath)
  settings <- get_settings_yml(orgPath)

  # identify the lines that begin with "+==="
  indices <- which( startsWith( context$contents, "+===") )
  indices_row <- which( startsWith( context$contents[1:row], "+===") )

  # get the selection in contents
  content_selection <- context$contents[rowStart:rowEnd]

  # from indices determine if the cursor is sitting IN or OUT of a datatable


  #### IF CURSOR IN DATATABLE : TEMPLATE ####

  if( (length(indices_row) %% 2) == 1) {

    ##### for now FAIL - not supporting TEMPLATE GROUPS #####

    addin_error("Add Groups to Datatable", "No support for TEMPLATE : GROUP datatables.")
    stop( paste0("  No support for TEMPLATE : GROUP datatablesn: \n    "))

  } else {# cursor lays OUTSIDE of a datatable - here will just generate shiny gadget to CREATE a datatable:


    #### ELSE CURSOR OUT DATATABLE : Create Datatable ####

    # check selected text for group declaration
    group_declaration <- format_group_declaration_bullets(content_selection)

    # collect all sample data from the current Rmd UP TO TEMPLATE
    dts <- projectmanagr::datatable_read_vector(context$contents[1:(row)]) # -1 to select line just UPTO selected row
    dt_names <- names(dts)

    # use for selectInput object:
    lt <- as.list( 1:length(dt_names) )
    names(lt) <- dt_names


    ##### user interface #####

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

          fillRow(  textInput("data_cols", "Group Data Cols. to add (space-separated):", value = group_declaration, width="100%")  ),

          fillRow(  span( textOutput("warningName"), style="color:red")  )

        )
      )
    )


    #### server code ####

    server <- function(input, output, session) {

      observeEvent(input$done, {

        col_name_warning <- ""
        # check the selected dt does not contain the defined data cols:
        dt_col_names <- names(dts[[input$dt]])

        #for(i in 1:length(input$data_cols) ) {

          # check dt doesnt already contain col of same name?
         # if( any( dt_col_names == input$data_cols[i] ) ) {
          #  col_name_warning <- paste0("  Data col already exists in datatable: ", input$data_cols[i] )
          #}
        #} # NO LONGER WARNING ABOUT data_cols - as want to handle case where group are added to REMAINING IDs in EXISTING group cols

        #if(col_name_warning != "") {

          # set the warningName TextOutput:
        #  output$warningName <- renderText({
        #    col_name_warning
         # })

        #} else
        if( regexpr("group-", input$data_cols) == -1 ) {

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
            rmd_startline = rowStart,
            rmd_endline = rowEnd,
            group_names = group_names,
            datatable_name =  names(dts)[ as.numeric(input$dt) ],
            groups = group_values,
            settings = settings,
            dt_length = 100,
            summarise_reps = FALSE
          )

          # navigate to file - to reload:
          rstudioapi::navigateToFile( path )

          # Close Gadget after 'done' is clicked.
          stopApp()

        }
      })
    }


    #### view gadget ####

    viewer <- dialogViewer("Add Groups to Datatable",
                           width = settings[["GadgetWidth"]],
                           height = settings[["GadgetHeight"]])
    runGadget(ui, server, viewer = viewer)

  }

} #### _______________________________________________ ####


#' Format group declaration
#'
#' Convert the bulleted list of GROUP TITLEs and GROUP IDs to space-separated
#' string, for use in the addin_datatable_add_groups ADDIN.
#'
#' @param content_selection Selection of GROUP TITLEs and GOUP IDs in bullets
#'
format_group_declaration_bullets <- function(content_selection) {

  # define initial blank string to append group titles & IDs
  group_string <- ""

  # remove all blank lines and lines beginning with '>>>>' - these are comments
  # AND only keep lines that contain the '`' char - used to state group titles & IDs
  cs <- content_selection[ ( (!(trimws(content_selection) =="")) &
                             (!startsWith(content_selection, ">>>>")) &
                             (grepl('`', content_selection, fixed=TRUE))   ) ]

  if(  length(grep("`group-", cs, fixed=TRUE)) == 0 ) {
    # no group- titles identified in content_selection
    # just return blank string
    return(group_string)
  } else {

    # loop through all lines & append to group_string the titles and IDs
    for(line in cs) {
      csws <- trimws(line)
      seps_ind <- gregexpr('`', csws, fixed=TRUE)[[1]]
      title <- substring(csws, seps_ind[1]+1, seps_ind[2]-1)
      # append to group_string
      group_string <- paste0(group_string, title, " ")
    }
    # return formed string
    return( group_string )
  }
}


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
addin_datatable_resample <- function() {

  #### instance variables ####

  # get data from current file
  context <- rstudioapi::getSourceEditorContext()
  row <- context$selection[[1]]$range$start[1]
  path <- normalizePath( context$path )

  # SAVE the document before processing!
  rstudioapi::documentSave(id = context$id)

  WD <- path

  # identify the orgPath from current working directory - to retrieve the settings yaml file
  orgPath <- find_org_directory( WD )

  # if orgPath not identified present error interface and then stop this function
  if(orgPath=="") {
    addin_error_path("Resample Datatable",
                     "No Organisation identified - ensure working directory is in an Organisation.",
                     orgPath)
    stop( paste0("  No Organisation identified - ensure working directory is in an Organisation: \n    ", WD))
  }

  # get config templates settings yml
  confPath <- get_config_dir(orgPath)
  tempPath <- get_template_dir(orgPath)
  settings <- get_settings_yml(orgPath)

  # identify the lines that begin with "+==="
  indices <- which( startsWith( context$contents, "+===") )
  indices_row <- which( startsWith( context$contents[1:row], "+===") )

  # from indices determine if the cursor is sitting IN or OUT of a datatable


  #### IF CURSOR IN DATATABLE : TEMPLATE ####

  if( (length(indices_row) %% 2) == 1 ) {

    ##### Check DT is TEMPLATE RESAMPLE ####

    # cursor is sitting IN a datatable - potentially expanding a TEMPLATE datatable ADD_DATA

    # check the datatable is complete - it has an end
    if(length(indices) <= length(indices_row) ) {
      stop( paste0("  Cannot identify next datatable separator - check syntax?") )
    }

    # get the template_table:
    startrow <- indices_row[length(indices_row)]
    endrow <- indices[length(indices_row)+1]
    template_table <- context$contents[ startrow : endrow ]

    table_name <- trimws(strsplit( as.character(template_table[4]), ":")[[1]][1])
    table_function <- trimws(strsplit( as.character(template_table[4]), ":")[[1]][2])

    # confirm table is named/of type TEMPLATE
    if(table_name != "TEMPLATE") {
      stop( paste0("  Cursor is inside a datatable that is not a TEMPLATE - rename table or move cursor!") )
    }

    # confirm table function is RESAMPLE
    if(table_function != "RESAMPLE") {
      stop( paste0("  Cursor is inside a datatable that is not of function RESAMPLE - use appropriate `addin_datatable` function: ", table_function) )
    }

    # extract as list
    template_list <- datatable_extract(template_table)

    # remove IDs col - first index
    template_list <- template_list[2:length(template_list)]

    # confirm table has resample as first col
    if(template_list[[1]][1] != "resample") {
      stop( paste0("  RESAMPLE datatable not correctly formed - first col must be `resample`: ", template_list[[1]][1]) )
    }
    # confirm table has reps as second col
    if(template_list[[2]][1] != "reps") {
      stop( paste0("  RESAMPLE datatable not correctly formed - second col must be `reps`: ", template_list[[2]][1]) )
    }

    # After these basic checks, now can generate shiny gadget for user to provide input

    # create instance variables : existing datatable list for selection

    # collect all sample data from the current Rmd UP TO TEMPLATE -> SELECTED LINE
    dts <- projectmanagr::datatable_read_vector(context$contents[1:(startrow-1)]) # -1 to select line just BEFORE TEMPLATE dt
    dt_names <- names(dts)

    lt <- as.list( 1:length(dt_names) )
    names(lt) <- dt_names


    #### user interface ####

    ui <- miniPage(

      gadgetTitleBar("Resample Datatable Template from Existing Table"),

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

          fillRow(  span( textOutput("warningName"), style="color:red")  )

        )
      )
    )


    #### server code ####

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


          #### datatable resample template rmd ####

          projectmanagr::datatable_resample_template_rmd(
                            rmd_path = path,
                            rmd_startline = startrow,
                            rmd_endline = endrow,
                            datatable_name = names(dts)[ as.numeric(input$dt) ],
                            settings = settings,
                            dt_length = 100
                          )

          # navigate to file - to reload:
          rstudioapi::navigateToFile( path )

          # Close Gadget after 'done' is clicked.
          stopApp()

        }
      })
    }


    #### view gadget ####

    viewer <- dialogViewer("Resample Datatable",
                           width = settings[["GadgetWidth"]],
                           height = settings[["GadgetHeight"]])
    runGadget(ui, server, viewer = viewer)


  } else { # cursor lays OUTSIDE of a datatable - here will just generate shiny gadget to CREATE a datatable:


    #### ELSE CURSOR OUT DATATABLE : Create Datatable ####

    # collect all sample data from the current Rmd UP TO TEMPLATE -> SELECTED LINE
    dts <- projectmanagr::datatable_read_vector(context$contents[1:(row)]) # -1 to select line UPTO selected line
    dt_names <- names(dts)

    lt <- as.list( 1:length(dt_names) )
    names(lt) <- dt_names

    #### user interface ####

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


    #### server code ####

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
                             settings = settings,
                             resample_vector = data_cols,
                             dt_length = 100
                           )
          #cat("rmd_path: ", path, "\n")
          #cat("rmd_startline: ", startrow, "\n")
          #cat("rmd_endline: ", endrow, "\n")
          #cat("datatable_name: ", names(dts)[ as.numeric(input$dt) ], "\n")
          #cat("resample_vector: ", data_cols, "\n")
          #cat("length resample_vector: ", length(data_cols), "\n")
          #cat("dt_length: ", 120, "\n")


          # navigate to file - to reload:
          rstudioapi::navigateToFile( path )

          # Close Gadget after 'done' is clicked.
          stopApp()

        }
      })
    }


    #### view gadget ####

    viewer <- dialogViewer("Resample Datatable",
                           width = settings[["GadgetWidth"]],
                           height = settings[["GadgetHeight"]])
    runGadget(ui, server, viewer = viewer)

  }

} #### _______________________________________________ ####



#' Import EXISTING samples and reps from Datatables in source Project Note(s)
#'
#' Generates a Shiny Gadget for importing existing samples and reps derived from
#' datatables that exist in source Project Note(s), into the currently active
#' destination Project Note.
#'
#' * User selects the line in the current Project Note where the IMPORT will
#' be written.
#'
#' * User selects a dirTree, from the current ORGANISATION: All project notes
#' identified recursively are is read for all EXISTING samples/reps in datatables,
#' which are presented in SUMMARY FORMAT
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
addin_datatable_import <- function() {

  #### instance variables ####

  # get data from current file - DESTINATION FILES
  context <- rstudioapi::getSourceEditorContext()
  row <- context$selection[[1]]$range$start[1]
  path <- normalizePath( context$path )

  # SAVE the document before processing!
  rstudioapi::documentSave(id = context$id)

  # collect the organisation path from current path
  orgPath <- find_org_directory(path)

  # if orgPath not identified present error interface and then stop this function
  if(orgPath=="") {
    addin_error_path("Import Datatable",
                     "No Organisation identified - ensure working directory is in an Organisation.",
                     orgPath)
    stop( paste0("  No Organisation identified - ensure working directory is in an Organisation: \n    ", WD))
  }

  # get config templates settings yml
  confPath <- get_config_dir(orgPath)
  tempPath <- get_template_dir(orgPath)
  settings <- get_settings_yml(orgPath)

  # generate a blank table to initialise addin with
  ID <- ""      # ID: Each Sample ID
  SAMPLE <- ""  # SAMPLE: COMPOSITE of all subsampling columns: CNS-RT-MB etc.
  PREFIX <- ""     # EXP: Fill with the Experiment Prefix ID
  TITLE <- ""   # TITLE: Fill with Experiment Title - will contain the LAB_TREATMENT
  #PATH <- ""    # PATH: Put the absolute PATH to the Project Note Rmd to Navigate to
  LOCATION <- "" # LOCATION of the sample - where is it?
  CONDITION <- "" # CONDITION of sample - what is it in?
  DATETIME <- "" # DATETIME sample was moved to current CONDITION/LOCATION
  COUNT <- integer()  # COUNT: How many REPS are there of this sample?
  IMPORT <- integer()  # IMPORT: How many REPS to import from this sample?

  #samples_summary <- tibble::tibble(ID, SAMPLE, COUNT, PREFIX, TITLE, LOCATION, CONDITION)
  samples_summary <- tibble::tibble(PREFIX, TITLE, ID, SAMPLE,
                                    CONDITION, LOCATION, DATETIME,
                                    COUNT, IMPORT)

  ui <- miniPage(

    gadgetTitleBar("Import Samples"),

    miniContentPanel(

      fillCol( flex = c(1,1,1,20),
               # , verbatimTextOutput("rows", placeholder = TRUE)
               fillRow( p("Import samples/reps from datatables in Source Project Notes"), actionButton("click_action", "Click") ),

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

        #global$datapath <- check_prog_sub_dir(global$datapath)
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

      cur_datetime <- projectmanagr::get_datetime()

      # export samples from SOURCE Rmds
      export_dts <- projectmanagr::datatable_export(samples_summary = samp_summ,
                                                    destination_note_path = path,
                                                    datetime=cur_datetime,
                                                    summarise_reps = TRUE, dt_length = 100)

      # AND import samples from SOURCE Rmds to DESTINATION Rmd
      projectmanagr::datatable_import(export_datatables = export_dts,
                                      destination_note_path = path,
                                      destination_note_line = row,
                                      datetime=cur_datetime,
                                      summarise_reps = FALSE, dt_length = 100)

      # Close Gadget after computations are complete:
      stopApp()

      print(global$summary)
      print(samp_summ)


    })

  }

  #### view gadget ####

  viewer <- dialogViewer("Import Samples",
                         width = settings[["GadgetWidth"]],
                         height = settings[["GadgetHeight"]])
  runGadget(ui, server, viewer = viewer)

}



#' Insert a datatable from a dataframe
#'
#' Will insert the passed dataframe, using the variable's NAME as the datatable
#' NAME.  Will CREATE the datatable first, then subsequent cols that dont fit in
#' the first datatable will be in subsequent ADD_DATA datatables.
#'
#' Inserts the datatable where the CURSOR IS in the Active Rmd.
#'
#' @param df a dataframe/tibble with data to insert into Rmd
#'
#' @param dt_function String to declare type of table: MUST be CREATE ADD_DATA
#' RESAMPLE GROUP EXPORT or IMPORT. Default is CREATE.
#'
#' @param dt_length Int that dictates the maximum length of any one inserted datatable.
#'
#' @export
datatable_insert_from_dataframe <- function( df,
                                             dt_function = "CREATE", dt_length = 100 ) {

  DATATABLE_SPACER_CHAR <- "="

  if(dt_function == "CREATE" || dt_function == "ADD_DATA" || dt_function == "RESAMPLE") {
    dt_function_next <- "ADD_DATA"
  } else if( dt_function == "GROUP" ) {
    dt_function_next <- "GROUP" # create further group tables
  } else if( dt_function == "EXPORT" ) {
    dt_function_next <- "EXPORT" # create further export tables
  } else if( dt_function == "IMPORT" ) {
    dt_function_next <- "IMPORT" # create further import tables
  } else {
    stop( paste0("  dt_function MUST be CREATE ADD_DATA RESAMPLE GROUP EXPORT or IMPORT : ", dt_function))
  }

  # returns name of ORIGINAL VARIABLE!
  name <- rlang::enexpr(df)

  # get the current active doc and metadata
  context <- rstudioapi::getSourceEditorContext()
  rmd_line <- context$selection[[1]]$range$start[1]
  rmd_path <- normalizePath( context$path )
  rmd_contents <- context$contents

  # SAVE the document before processing!
  rstudioapi::documentSave(id = context$id)


  cat( "\nprojectmanagr::datatable_insert_from_dataframe():\n" )

  # if not an absolute path:
  if( R.utils::isAbsolutePath(rmd_path) == FALSE ) {
    rmd_path <- R.utils::getAbsolutePath(rmd_path )
  }

  # CONFIRM rmd_path is a project doc or note:
  # Check rmd_path is a sub-dir in a Programme DIR, which is a sub-dir to the root of an ORGANISATION:
  # run dirname TWICE as want to ensure rmd_path is a sub-dir in a Programme!
  orgPath <- dirname( dirname(rmd_path) )

  orgPath <- find_org_directory(orgPath)

  if(orgPath == "" ) {
    # the search reached the root of the filesystem without finding the Organisation files,
    # therefore, rmd_path is not inside a PROGRAMME sub-dir!
    stop( paste0("  rmd_path is not a Project Doc or Note - not in a sub-dir of a PROGRAMME Directory: ", rmd_path) )
  }
  # now, orgPath should be the root dir of the organisation

  # build the datatable text vector
  col_names <- names(df)[2:length(names(df))]

  # data - create blank list
  data <- list()
  for(i in 2:length(df) ) {
    data[[i-1]] <- df[[i]]  # concat each data vector to list
  }
  #data <- lapply(df, function(x) x[x != ""])
  #data <- data[2:length(data)]

  ID_col <- "ID"
  IDs <- df$ID
  data_cols <- col_names
  data <- data
  dt_function <- dt_function
  datatable_name <- name
  MAX_DATATABLE_LENGTH <- dt_length
  DATATABLE_SPACER_CHAR <- "="

  data_tables <- build_datatable_from_dataframe(ID_col, IDs, data_cols, data, dt_function,
                                                datatable_name, MAX_DATATABLE_LENGTH,
                                                DATATABLE_SPACER_CHAR)

  #ID_col <- "ID"
  #IDs <- df$ID
  #data_cols <- col_names
  #data <- data
  #dt_function <- "CREATE"
  #datatable_name <- name
  #MAX_DATATABLE_LENGTH <- dt_length
  #DATATABLE_SPACER_CHAR

  # write these to the file:
  cat( "\n  write data table(s) to Rmd at line: ", rmd_line )
  rmd_contents <- c( rmd_contents[1:(rmd_line-1)], data_tables, rmd_contents[(rmd_line+1):length(rmd_contents)] )
  # rmd_line-1 to REMOVE CURRENT LINE

  rmd_file_conn <- file( rmd_path )
  writeLines(rmd_contents, rmd_file_conn)
  close(rmd_file_conn)


}

#' Read datatables from ACTIVE RMD in RStudio
#'
#' Function to read all projectmanagr datatables declared in the active plain
#' text document (typically an R Markdown doc), and return them as tibbles.
#' Extracts all datatables between `+===` markers, and checks the validity of
#' each datatable declared.
#'
#' Each datatable is named, and each contains samples which must all possess a
#' unique ID.  Datatables are created in CREATE, and have data added to them
#' in ADD_DATA tables.  IDs from a given datatable can be put into new groups
#' using a GROUP table.
#'
#' Adding data can use various layouts:
#'
#' * Sample-first layout:  Typically used for measurements made on each sample
#' individually - sample IDs are in first col, subsequent cols contain the
#' measurement data.
#'
#' * Variable-first layout: Typically used to add datetimes of procedures -
#' procedure titles are laid out in first col, and subsequent cols are GORUPS
#' which are processed together.  This allows timing data to be added cleanly
#' and easily across all samples in a group (including the special group ALL).
#'
#' * TIMETABLE: Used for measuring the actual timings used during optimisation
#' of a protocol.  When time is varied in a protocol it is very difficult to
#' plan and track this.  The timetable provides a convenient layout for planning
#' the groups and the timings of changes over the procedure where timing is being
#' optimised.  It then allows the ACTUAL TIMINGS to be inserted into the table
#' as the procedures are followed - and these are linked to the original samples,
#' making this data available in further analyses.
#'
#' Samples in datatables can be subsampled - destroying the original sample,
#' and creating one or more sub-samples.  The parent samples can no longer
#' have data added to them, only the existing sub-samples.
#'
#' Samples in datatables are be exported to destination notes, and imported
#' from source notes.  If exported from this source note, the samples no longer
#' exist in the current note - they cannot be manipulated in this note.
#'
#' If imported from a previous source note, the samples now exist in this note.
#' They can have data added to them, grouped, subsampled, and exported.
#'
#' @export
datatable_read <- function() {

  datatable_read_rmd( rstudioapi::getSourceEditorContext()$path )

}



#' Read datatables upto SELECTION from ACTIVE RMD in RStudio
#'
#' Function to read all projectmanagr datatables up to cursor selection declared
#' in the active plain text document (typically an R Markdown doc), and return
#' them as tibbles. Extracts all datatables between `+===` markers, and checks
#' the validity of each datatable declared.
#'
#' Each datatable is named, and each contains samples which must all possess a
#' unique ID.  Datatables are created in CREATE, and have data added to them
#' in ADD_DATA tables.  IDs from a given datatable can be put into new groups
#' using a GROUP table.
#'
#' Adding data can use various layouts:
#'
#' * Sample-first layout:  Typically used for measurements made on each sample
#' individually - sample IDs are in first col, subsequent cols contain the
#' measurement data.
#'
#' * Variable-first layout: Typically used to add datetimes of procedures -
#' procedure titles are laid out in first col, and subsequent cols are GORUPS
#' which are processed together.  This allows timing data to be added cleanly
#' and easily across all samples in a group (including the special group ALL).
#'
#' * TIMETABLE: Used for measuring the actual timings used during optimisation
#' of a protocol.  When time is varied in a protocol it is very difficult to
#' plan and track this.  The timetable provides a convenient layout for planning
#' the groups and the timings of changes over the procedure where timing is being
#' optimised.  It then allows the ACTUAL TIMINGS to be inserted into the table
#' as the procedures are followed - and these are linked to the original samples,
#' making this data available in further analyses.
#'
#' Samples in datatables can be subsampled - destroying the original sample,
#' and creating one or more sub-samples.  The parent samples can no longer
#' have data added to them, only the existing sub-samples.
#'
#' Samples in datatables are be exported to destination notes, and imported
#' from source notes.  If exported from this source note, the samples no longer
#' exist in the current note - they cannot be manipulated in this note.
#'
#' If imported from a previous source note, the samples now exist in this note.
#' They can have data added to them, grouped, subsampled, and exported.
#'
#' @export
datatable_read_to_selection <- function() {

  row <- rstudioapi::getSourceEditorContext()$selection[[1]]$range$start[1]
  rmd_path <- normalizePath( rstudioapi::getSourceEditorContext()$path )

  # read rmd_path file:
  rmd_file_conn <- file( rmd_path )
  rmd_contents <- readLines( rmd_file_conn )
  close(rmd_file_conn)

  # read datatables upto selection
  datatables <- datatable_read_vector(rmd_contents[1:row] )

  # return
  datatables
}



#' Read datatables upto LINE in ACTIVE RMD in RStudio
#'
#' Function to read all projectmanagr datatables up to line number
#' in the active plain text document (typically an R Markdown doc), and return
#' them as tibbles. Extracts all datatables between `+===` markers, and checks
#' the validity of each datatable declared.
#'
#' Each datatable is named, and each contains samples which must all possess a
#' unique ID.  Datatables are created in CREATE, and have data added to them
#' in ADD_DATA tables.  IDs from a given datatable can be put into new groups
#' using a GROUP table.
#'
#' Adding data can use various layouts:
#'
#' * Sample-first layout:  Typically used for measurements made on each sample
#' individually - sample IDs are in first col, subsequent cols contain the
#' measurement data.
#'
#' * Variable-first layout: Typically used to add datetimes of procedures -
#' procedure titles are laid out in first col, and subsequent cols are GORUPS
#' which are processed together.  This allows timing data to be added cleanly
#' and easily across all samples in a group (including the special group ALL).
#'
#' * TIMETABLE: Used for measuring the actual timings used during optimisation
#' of a protocol.  When time is varied in a protocol it is very difficult to
#' plan and track this.  The timetable provides a convenient layout for planning
#' the groups and the timings of changes over the procedure where timing is being
#' optimised.  It then allows the ACTUAL TIMINGS to be inserted into the table
#' as the procedures are followed - and these are linked to the original samples,
#' making this data available in further analyses.
#'
#' Samples in datatables can be subsampled - destroying the original sample,
#' and creating one or more sub-samples.  The parent samples can no longer
#' have data added to them, only the existing sub-samples.
#'
#' Samples in datatables are be exported to destination notes, and imported
#' from source notes.  If exported from this source note, the samples no longer
#' exist in the current note - they cannot be manipulated in this note.
#'
#' If imported from a previous source note, the samples now exist in this note.
#' They can have data added to them, grouped, subsampled, and exported.
#'
#' @export
datatable_read_to_line <- function(line) {

  rmd_path <- normalizePath( rstudioapi::getSourceEditorContext()$path )

  # read rmd_path file:
  rmd_file_conn <- file( rmd_path )
  rmd_contents <- readLines( rmd_file_conn )
  close(rmd_file_conn)

  # read datatables upto selection
  datatables <- datatable_read_vector(rmd_contents[1:line] )

  # return
  datatables
}



