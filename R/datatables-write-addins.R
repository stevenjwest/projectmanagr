

#### _______________________________________________ ####

#' Create a New Datatable in R Markdown
#'
#' Generates a Shiny Gadget for initializing a new datatable based on an ID list and
#' optional columns with values. The created datatable is inserted at the current
#' cursor position in the active R Markdown document.
#'
#' @details
#' This function:
#' - Retrieves the current Rmd document content and the position of the cursor.
#' - Identifies the presence of existing datatables based on delimiter lines (starting with "+===").
#' - Saves the Rmd document before making changes.
#' - If the cursor is inside an existing datatable, it initializes a new template datatable.
#' - If the cursor is outside any existing datatable, it creates a new datatable and inserts it at the cursor's position.
#'
#' The user interacts with a Shiny Gadget to specify the ID list and optional column values
#' for the new datatable. Once the user confirms the operation, the datatable is inserted
#' directly into the current Rmd file at the cursor's position.
#'
#' @return
#' This function does not return any value. It modifies the Rmd document by creating
#' and inserting a new datatable at the cursor's current position.
#'
#' @examples
#' # Example usage:
#' # Run this function to create a new datatable in the R Markdown file.
#' addin_datatable_create()
#'
#' # During usage:
#' # - Specify an ID list and optional columns/values for the datatable.
#' # - The datatable will be inserted at the current cursor position in the Rmd.
#'
#' @export
addin_datatable_create <- function() {

  #### instance variables ####

  # get data from rstudio context
  path <- get_context_path()
  contents <- get_context_contents()
  row <- get_context_row()

  # indices of datatable delimiter lines - that begin with "+==="
  indices <- which( startsWith( contents, datatable_get_delimiter()) )
  indices_row <- which( startsWith( contents[1:row], datatable_get_delimiter()) )

  # SAVE the document before processing!
  save_context_doc()

  # from indices determine if the cursor is sitting IN or OUT of a datatable
  if( (length(indices_row) %% 2) == 1 ) { ##### If Cursor Inside Datatable : TEMPLATE ####
    ad_dt_create_template(path, contents, indices, indices_row)

  } else { ##### Else Cursor Outside Datatable : Create Datatable ####
    ad_dt_create(path, contents, row)
  }
}



ad_dt_create <- function(path, contents, row) {

  # identify the orgPath from current working directory - to retrieve the settings yaml file
  orgPath <- confirm_org_path_addin(normalizePath(path), "Create Datatable")

  # get settings yml for runGadget call
  settings <- get_settings_yml(orgPath)

  runGadget(dt_create_ui(path, row),
            dt_create_server,
            viewer=dialogViewer("Create Datatable", width=settings[["GadgetWidth"]], height=settings[["GadgetHeight"]]))
}


dt_create_ui <- function(path, row) {

  ui <- miniPage(

    gadgetTitleBar("Create Datatable"),

    miniContentPanel(

      fillCol(

        fillRow( p("Create a datatable in the active Rmd at the cursor from input params.") ),

        fillRow(
          helpText('Rmd: ', path, align='center'),
          helpText('cursor position (line): ', row, align='center' ) ),

        fillRow(  textInput("name", "Datatable name:",
                            value = "samples",
                            width="100%")  ),

        fillRow(  span( textOutput("warningName"),
                        style="color:red")  ),

        fillRow(  textInput("ids", "IDs (space-separated):",
                            width="100%")  ),

        fillRow(  span( textOutput("warningName2"),
                        style="color:red")  ),

        fillRow(  textInput("data_cols",
                            "Extra Data Cols (space-separated):",
                            width="100%")  ),

        fillRow( checkboxInput("expand",
                               'Expand data across IDs',
                               value = FALSE, width='95%') ),

      )
    )
  )
  ui
}

dt_create_server <- function(input, output, session) {

  path <- get_context_path()
  row <- get_context_row()
  settings <- get_settings_yml(find_org_directory(path))

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
        datatable_name = input$name,
        data_cols = data_cols,
        IDs = ids,
        default_data_vals=list(),
        dt_length = 100,
        expand = input$expand
      )

      # Close Gadget after 'done' is clicked.
      stopApp()

    }
  })
}

ad_dt_create_template <- function(path, contents, indices, indices_row) {

  # identify the orgPath from current working directory - to retrieve the settings yaml file
  orgPath <- confirm_org_path_addin(normalizePath(path), "Create Datatable")

  # get settings yml for runGadget call
  settings <- get_settings_yml(orgPath)

  # get the template_dt_vector:
  startrow <- indices_row[length(indices_row)]
  endrow <- indices[length(indices_row)+1]
  template_dt_vector <- contents[ startrow : endrow ]

  # get current table name function & possible output name
  table_name_function_vector <- get_dt_vector_name_function(template_dt_vector)
  table_name <- table_name_function_vector[1]
  table_function <- table_name_function_vector[2]

  # collect all sample data from the current Rmd up to SELECTED LINE
  lt <- get_dts_named_list(contents, startrow)

  check_dt_create_template(indices, indices_row, table_name, table_function)
  # After these basic checks, now can generate shiny gadget for user to provide input

  # generate separtate gadgets depending on whether the output name is given
  if( length(table_name_function_vector) > 2 ) { # if a third index exists this is the output_name - given after `TEMPLATE : CREATE : {{template_name}}`

    template_datatable_name <- extract_template_named_table(template_dt_vector) # get the name to audo fill in UI

    runGadget(dt_create_template_ui(path, startrow, endrow, lt, template_datatable_name),
              dt_create_template_server, viewer=dialogViewer("Create Datatable", width=settings[["GadgetWidth"]], height=settings[["GadgetHeight"]]))

  } else { # NOT a NAMED TEMPLATE DATATABLE - user must specify the name of th output table

    runGadget(dt_create_template_ui(path, startrow, endrow, lt, ""),
              dt_create_template_server, viewer=dialogViewer("Create Datatable", width=settings[["GadgetWidth"]], height=settings[["GadgetHeight"]]))
  }
}


check_dt_create_template <- function(indices, indices_row, table_name, table_function) {
  # check the datatable is complete - it has an end
  if(length(indices) <= length(indices_row) ) {
    stop( paste0("  Cannot identify next datatable separator - check syntax?") )
  }
  # confirm table is named/of type TEMPLATE
  if(table_name != "TEMPLATE") {
    stop( paste0("  Cursor is inside a datatable that is not a TEMPLATE - rename table or move cursor!") )
  }

  # confirm table function is CREATE
  if(table_function != "CREATE") {
    stop( paste0("  Cursor is inside a datatable that is not of function CREATE - use appropriate `addin_datatable` function: ", table_function) )
  }
}

dt_create_template_ui <- function(path, startrow, endrow, lt, template_datatable_name) {

  ui <- miniPage(

    shinyjs::useShinyjs(), # to enable & disable datatable or manual ID inputs

    gadgetTitleBar("Create Datatable from Template using <<IDS>> from existing datatable"),

    miniContentPanel(

      fillCol(

        fillRow( p("Create new datatable from the selected Template using <<IDS>> from existing datatable.") ),

        fillRow(
          helpText( p('Rmd: ', path, align="center") ),
          helpText( p('Template - Start line: ',
                      startrow, align="center") ),
          helpText( p('End line: ',
                      endrow, align="center") ) ),

        fillRow( h4("Select Existing Datatable:") ),

        fillRow(
                 checkboxInput("defIDs", 'Define IDs manually',
                               value = FALSE)  ),

        fillRow( flex = c(1,2),
                 selectInput("dt",
                             "Select existing datatable to source <<IDS>> from",
                             choices = lt, selected = 1, width="100%"),
                 textInput("dt_ids",
                           "Specify Datatable IDs manually (space separated):",
                           value = "", width="100%")  ),

        fillRow(  textInput("template_datatable_name",
                            "Specify new Datatable name:",
                            value = template_datatable_name, width="100%")  ),

        fillRow( checkboxInput("allIDs", 'Add All existing & non-existing IDs',
                               value = FALSE, width='95%') ),

        fillRow(  span( textOutput("warningName2"),
                        style="color:red")  )

      )
    )
  )

  ui # return
}


dt_create_template_server <- function(input, output, session) {

  observe({
    shinyjs::disable("dt_ids") # initially..
  })

  path <- get_context_path()
  row <- get_context_row()
  contents <- get_context_contents()
  settings <- get_settings_yml(find_org_directory(path))

  # # get the template_dt_vector - not needed as pass the name via UI?
  # indices <- which( startsWith( contents, "+===") )
  # indices_row <- which( startsWith( contents[1:row], "+===") )
  # startrow <- indices_row[length(indices_row)]
  # endrow <- indices[length(indices_row)+1]
  # template_dt_vector <- contents[ startrow : endrow ]
  # template_datatable_name <- extract_template_named_table(template_dt_vector)

  # get dts to get selected dt name
  dts <- datatable_read_vector(contents[1:row])

  observe({

    if(input$defIDs == TRUE) { # if checkbox ticked

      shinyjs::disable("dt")
      shinyjs::enable("dt_ids")

    } else {

      shinyjs::enable("dt")
      shinyjs::disable("dt_ids")

    }

  })


  observeEvent(input$done, {

    if(input$template_datatable_name == "") { # set the warningName TextOutput:
      output$warningName <- renderText({ "*** PROVIDE DATATABLE NAME ***" })
    } else {

      if( input$defIDs == TRUE ) {

        datatable_name <- NULL
        IDs <- unlist( strsplit(as.character(input$dt_ids), split= ' '))

      } else {
        # get the INDEX of selection of existing dt
        x <- input$dt # returns CHARACTER - must cast to numeric to use as index!!
        ids <- dts[[ as.numeric(x) ]]$ID # get ID vector from this datatable
        datatable_name <- names(dts[ as.numeric(x) ])
        IDs <- NULL

      }

      #### datatable create template rmd ####

      projectmanagr::datatable_create_template_rmd(
        rmd_path = path,
        rmd_line = row,
        datatable_name = datatable_name,
        template_datatable_name = input$template_datatable_name,
        IDs = IDs,
        all_ids=input$allIDs,
        dt_length=100 )

      # Close Gadget after 'done' is clicked.
      stopApp()
    }

  })
}


confirm_org_path_addin <- function(path, addinTitle) {

  # identify the orgPath from current working directory - to retrieve the settings yaml file
  orgPath <- find_org_directory(path)

  # if orgPath not identified present error interface and then stop this function
  if(orgPath=="") { addin_error_path("Create Datatable", "No Organisation identified - ensure working directory is in an Organisation.", orgPath)
    stop( paste0("  No Organisation identified - ensure working directory is in an Organisation: \n    ", path))
  }
  orgPath # returns
}


get_dts_named_list <- function(contents, row) {
  dts <- datatable_read_vector(contents[1:(row)])
  dt_names <- names(dts)
  lt <- as.list( 1:length(dt_names) )
  names(lt) <- dt_names
  lt
}


get_dt_vector_name_function <- function(template_dt_vector) {
  trimws(strsplit( as.character(template_dt_vector[4]), ":")[[1]])
}

#### _______________________________________________ ####



#' Create a New Datatable from a CSV File in R Markdown
#'
#' Generates a Shiny Gadget for initializing a new datatable from a CSV file selected
#' by the user. The content of the CSV file is converted to a datatable format and
#' inserted at the current cursor position in the active R Markdown document.
#'
#' @details
#' This function allows the user to:
#' - Select a CSV file from their file system through a Shiny Gadget.
#' - The selected file is then read, converted to a datatable format, and inserted
#'   at the current cursor position in the R Markdown document.
#' - The user can also select from different datatable types using a set of checkbox options.
#' - The function ensures the Rmd document is saved before any modifications are made.
#'
#' @return
#' This function does not return any value. It modifies the Rmd document by creating
#' and inserting a new datatable (derived from the selected CSV file) at the cursor's
#' current position.
#'
#' @examples
#' # Example usage:
#' # Run this function to create a new datatable from a selected CSV file and insert it in the Rmd.
#' addin_datatable_create_from_file()
#'
#' # During usage:
#' # - Select a CSV file using the Shiny Gadget.
#' # - The CSV file content will be converted to a datatable format and inserted at the cursor position.
#'
#' @export
addin_datatable_create_from_file <- function() {

  #### instance variables ####

  # get data from rstudio context
  path <- get_context_path()
  contents <- get_context_contents()
  row <- get_context_row()

  # checkbox values for datatable type
  dtT <- get_datatable_types_list()

  # SAVE the document before processing!
  save_context_doc()

  ad_dt_create_file(path, row, dtT)

}


get_datatable_types_list <- function() {
  dt_types <- c("CREATE", "ADD_DATA", "GROUP", "RSAMPLE", "DISPOSE", "EXPORT", "IMPORT")
  dtT <- as.list( 1:length(dt_types) )
  names(dtT) <- dt_types
  dtT
}


ad_dt_create_file <- function(path, row, dtT) {

  # identify the orgPath from current working directory - to retrieve the settings yaml file
  orgPath <- confirm_org_path_addin(normalizePath(path), "Create Datatable")

  # get settings yml for runGadget call
  settings <- get_settings_yml(orgPath)

  runGadget(dt_create_file_ui(path, row, dtT),
            dt_create_file_server,
            viewer=dialogViewer("Create Datatable from File", width=settings[["GadgetWidth"]], height=settings[["GadgetHeight"]]))

}

dt_create_file_ui <- function(path, row, dtT) {

  ui <- miniPage(

    gadgetTitleBar("Create Datatable from File"),

    miniContentPanel(

      fillCol(

        fillRow( p("Create a datatable in the active Rmd at the cursor from a CSV (or other data) file.") ),

        fillRow(
          helpText('Rmd: ', path, align='center'),
          helpText('cursor position (line): ', row, align='center' ) ),

        fillRow(  textInput("dt_name", "Datatable name:", value="samples", width="50%"),

                  selectInput("dtType",
                              "Select datatable type",
                              choices = dtT, selected = 1,
                              width="50%")  ),


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

  ui

}


dt_create_file_server <- function(input, output, session) {

  # get data from rstudio context
  path <- get_context_path()
  contents <- get_context_contents()
  row <- get_context_row()

  # checkbox values for datatable type
  dtT <- get_datatable_types_list()

  observeEvent(input$done, {

    x <- input$dtType # returns CHARACTER - must cast to numeric to use as index!!
    datatable_type <- names(dtT[ as.numeric(x) ])

    # open csv
    tb <- readr::read_delim( input$file1$datapath, col_types = readr::cols(.default = "c") )

    # create datatable from it
    dt <- build_datatable_from_tibble(tb, input$dt_name, datatable_type)

    # insert into current document
    write_file(insert_at_indices(contents, row, dt), path)

    # navigate to path & close addin:
    addin_rstudio_nav(path)
  })
}


#### _______________________________________________ ####



#' Add Data to Existing Samples in a New Datatable
#'
#' Generates a Shiny Gadget for adding data to existing samples in a new datatable,
#' with various layout and data selection options. The user can select a datatable name,
#' layout, and data to add, and then the new data is inserted at the cursor position
#' in the current R Markdown document.
#'
#' @details
#' This function allows the user to:
#' - Select an existing datatable from a list of available datatables in the current Rmd.
#' - Choose the datatable layout (samples, variables, or timetable).
#' - Select the ID format for data addition: "ALL", "all-IDs", or a group set.
#' - Define the column names, with special handling for datetime columns (denoted by the suffix `_dt`).
#' - The function will then insert the appropriate ADD_DATA table at the cursor location, based on the layout selected.
#'
#' If the cursor is already inside a valid ADD_DATA template (one that has a recognized layout such as sample, variable, or timetable),
#' the user can simply select the existing datatable and IDs to add, and the ADD_DATA table will be updated with the selected IDs.
#'
#' @param path A character string representing the file path of the current R Markdown document.
#' @param contents A character vector containing the content of the current R Markdown document.
#' @param row An integer indicating the row number where the cursor is positioned in the document.
#'
#' @return
#' This function does not return a value. It modifies the R Markdown document by adding data to an existing sample datatable
#' or updating the ADD_DATA template at the cursor position.
#'
#' @examples
#' # Example usage:
#' # Run this function to add data to an existing samples datatable at the cursor position.
#' addin_datatable_add_data()
#'
#' # During usage:
#' # - Select a datatable, layout, and ID format to add.
#' # - The new data will be inserted into the current Rmd at the cursor position.
#'
#' @export
addin_datatable_add_data <- function() {

  #### instance variables ####

  # get data from rstudio context
  path <- get_context_path()
  contents <- get_context_contents()
  row <- get_context_row()

  # indices of datatable delimiter lines - that begin with "+==="
  indices <- which( startsWith( contents, datatable_get_delimiter()) )
  indices_row <- which( startsWith( contents[1:row], datatable_get_delimiter()) )

  # SAVE the document before processing!
  save_context_doc()

  # from indices determine if the cursor is sitting IN or OUT of a datatable
  if( (length(indices_row) %% 2) == 1 ) { ##### If Cursor Inside Datatable : fill ADD_DATA TEMPLATE ####
    ad_dt_add_data_template(path, contents, row, indices, indices_row)

  } else { ##### Else Cursor Outside Datatable : generate ADD_DATA Datatable ####
    ad_dt_add_data(path, contents, row)
  }

}


ad_dt_add_data <- function(path, contents, row) {

  # identify the orgPath from current working directory - to retrieve the settings yaml file
  orgPath <- confirm_org_path_addin(normalizePath(path), "Add Data Datatable")

  # get settings yml for runGadget call
  settings <- get_settings_yml(orgPath)

  # collect all sample data from the current Rmd up to SELECTED LINE
  dts <- datatable_read_vector(contents[1:row])
  lt <- get_dts_named_list(contents, row)

  # get add _data named list of types
  type <- get_datatable_add_data_types_list()

  # select group cols from first datatable INITIALLY
  gdt <- get_group_cols(dts[[1]])
  ltid <- get_id_group_selection_list(gdt) # includes all-IDs
  #ltid <- as.list(1:(length(gdt)+2) )
  #names(ltid) <- c("ALL", "all-IDs", names(gdt))

  runGadget(dt_add_data_ui(path, row, lt, type, ltid),
            dt_add_data_server,
            viewer=dialogViewer("Add Data to Datatable", width=settings[["GadgetWidth"]], height=settings[["GadgetHeight"]]))
}


get_datatable_add_data_types_list <- function() {
  list("sample-first"=1, "variable-first"=2, "timetable"=3)
}


get_id_group_selection_list <- function(gdt) {
  ltid <- as.list(1:(length(gdt)+2) )
  names(ltid) <- c("ALL", "all-IDs", names(gdt))
  ltid
}

get_group_selection_list <- function(gdt) {
  ltid <- as.list(1:(length(gdt)+1) )
  names(ltid) <- c("ALL", names(gdt))
  ltid
}

dt_add_data_ui <- function(path, row, lt, type, ltid) {

  ui <- miniPage(

    shinyjs::useShinyjs(), # to enable & disable timetable col_name input

    gadgetTitleBar("Add Data Datatable"),

    miniContentPanel(

      fillCol(

        fillRow( p("Add Data to a datatable in the active Rmd at the cursor position.") ),

        fillRow(
          helpText('Rmd: ', path, align='center'),
          helpText('cursor position (line): ', row, align='center' ) ),

        fillRow( h4("Choose datatable:", align="center") ),

        fillRow( selectInput("dt", "Select Datatable",
                             choices = lt, selected = 1, width="100%")  ),

        fillRow( h4("Choose datatable type: ", align="center") ),

        fillRow( selectInput("type", "Select Datatable Type",
                             choices = type, selected = 1, width="100%")  ),

        fillRow( p("", strong("sample-first: "), "First col. is filled with all sample or group IDs, subsequent columns are data cols. specified below."),
                 p("", strong("variable-first: "), "First col. is filled with all data cols. specified below, subsequent columns are sample or group IDs"),
                 p("", strong("timetable: "), "Special table syntax for planning and recording datetimes of a procedure's steps executed with different timing across sample or group IDs.") ),

        fillRow( h4("Choose sample/group IDs: ", align="center") ),

        fillRow( selectInput("id", "Select IDs or Groups",
                             choices = ltid, selected = 1, width="100%")  ),

        fillRow( p("", strong("ALL: "), "A single ID, ALL, is used - will add all data to all sample IDs."),
                 p("", strong("all-IDs: "), "All sample IDs from selected datatable are used."),
                 p("", strong("<group-name>: "), "All IDs from the selected group are used.") ),

        fillRow( h4("Define new Data Columns/Step Names: ", align="center") ),

        fillRow(  textInput("data_cols", "Data Cols. to add (space-separated):", width="100%")  ),

        fillRow( p("Define data column names as a space-separated string - these MUST be unique to this new datatable. TIMETABLE: Supply space-separated step names") ),

        fillRow(  span( textOutput("warningName"), style="color:red")  ),

        fillRow( checkboxInput("summarise_reps", "Summarise Sample Reps", width="50%"),
                 checkboxInput("all_reps", "Select ALL Sample Reps", width="50%")),

        fillRow(  textInput("colName", "Timetable Column Name:", width='95%') ),

        fillRow( p("Define Timetable column name as a space-separated string - these MUST be unique to this new datatable.") )

      )
    )
  )

  ui
}



dt_add_data_server <- function(input, output, session) {

  # get initial instance variables
  path <- get_context_path()
  row <- get_context_row()
  contents <- get_context_contents()
  settings <- get_settings_yml(find_org_directory(path))

  # collect all sample data from the current Rmd up to SELECTED LINE
  dts <- datatable_read_vector(contents[1:row])
  dt_names <- names(dts)
  lt <- as.list( 1:length(dt_names) )
  names(lt) <- dt_names

  # get add _data named list of types
  type <- get_datatable_add_data_types_list()

  # select group cols from first datatable INITIALLY
  gdt <- get_group_cols(dts[[1]])
  ltid <- get_id_group_selection_list(gdt) # includes all-IDs
  #ltid <- as.list(1:(length(gdt)+2) )
  #names(ltid) <- c("ALL", "all-IDs", names(gdt))

  # modify group IDs if new dt is selected
  observe({

    x <- input$dt # returns CHARACTER - cast to numeric to use as index!!

    #print( paste0("x: ", x) )

    if( is.null(x) ) {
      # do nothing
    } else {
      #print( paste0("x: ", x) )
      #print( paste0( "x type: ", typeof(x)))
      #print(dts)
      # must convert x to numeric as its returned as a character!
      gdt <- get_group_cols(dts[[as.numeric(x)]])
      #gdt <- dplyr::select( dts[[ as.numeric(x) ]], dplyr::starts_with("group-") )
      #ltid <- as.list(1:(length(gdt)+2) )
      #names(ltid) <- c("ALL", "all-IDs", names(gdt))
      ltid <- get_id_group_selection_list(gdt) # includes all-IDs

      # update select input with new ltid:
      updateSelectInput(session, "id",
                        label = "Select IDs or Groups",
                        choices = ltid,
                        selected = 1 )
    }
  })

  # modify group IDs if different output dt TYPE is selected
  # variable-first does NOT support all-IDs anymore!
  observe({

    TYPE <- input$type # returns CHARACTER - cast to numeric to use as index!!
    #x <- input$type # returns CHARACTER - cast to numeric to use as index!!
    #print( paste0("value TYPE: ", TYPE) )
    #print( paste0("typeof TYPE: ", typeof(TYPE)) )

    if( is.null(TYPE) ) {
      # do nothing
    } else {
      TYPEn <- as.numeric(TYPE)
      #print( paste0("TYPEn: ", TYPEn))
      if( TYPEn == 2 ) { # if datatable type is VAR-FIRST

        # do not allow input on colName textInput
        shinyjs::disable("colName")

        # exclude all-IDs as an option from id
        gdt <- get_group_cols(dts[[as.numeric(input$dt)]]) # must convert input$dt to numeric as its returned as a character!
        ltid <- get_group_selection_list(gdt) # excludes all-IDs option
        # update select input with new ltid:
        updateSelectInput(session, "id", label = "Select IDs or Groups",
                          choices = ltid, selected = 1 )

      } else if(TYPEn == 3 ) { # if datatable type is TIMETABLE

        # allow input on colName textInput
        shinyjs::enable("colName")

        # exclude all-IDs as an option from id
        gdt <- get_group_cols(dts[[as.numeric(input$dt)]]) # must convert input$dt to numeric as its returned as a character!
        ltid <- get_group_selection_list(gdt) # excludes all-IDs option
        # update select input with new ltid:
        updateSelectInput(session, "id", label = "Select IDs or Groups",
                          choices = ltid, selected = 1 )

      } else { # datatable type is SAMPLE-FIRST

        # do not allow input on colName textInput
        shinyjs::disable("colName")

        # include all-IDs as an option from id
        gdt <- get_group_cols(dts[[as.numeric(input$dt)]]) # must convert input$dt to numeric as its returned as a character!
        ltid <- get_id_group_selection_list(gdt) # includes all-IDs option
        # update select input with new ltid:
        updateSelectInput(session, "id", label = "Select IDs or Groups",
                          choices = ltid, selected = 1 )
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
          gdt <- get_group_cols(dts[[as.numeric(input$dt)]])
          ltid <- get_group_selection_list(gdt) # excludes all-IDs
          #gdt <- dplyr::select( dts[[ as.numeric(input$dt) ]], dplyr::starts_with("group-") )
          #ltid <- as.list(1:(length(gdt)+1) )
          #names(ltid) <- c("ALL", names(gdt))
          # now get ids vector:
          ids_vector <- unique( dts[[ as.numeric(input$dt) ]][[ names(ltid)[ as.numeric(input$id) ] ]] )
          group_vector <- ids_vector
          cat( "\n  group_vector : ", group_vector )
          cat( "\n  ids_vector - COL : ", names(ltid)[ as.numeric(input$id) ] )
          cat( "\n  ids_vector : ", ids_vector, "  \n\n" )

        }

      } else { # a group col has been selected - parse the index depending on whether type is 1 (samples) or 2/3 (vars,timetable)

        if( as.numeric(input$type) == 1 ) { # all IDs can only be selected for sample-first
          cat( "\n  group_vector : COL" )
          # retrieve the group names AGAIN - as its not updated globally from previous observation function!
          dt <- dts[[as.numeric(input$dt)]]
          idI <- as.numeric(input$id)
          gdt <- get_group_cols(dts[[as.numeric(input$dt)]])
          ltid <- get_id_group_selection_list(gdt) # includes all-IDs
          #gdt <- dplyr::select( dts[[ as.numeric(input$dt) ]], dplyr::starts_with("group-") )
          #ltid <- as.list(1:(length(gdt)+2) )
          #names(ltid) <- c("ALL", "all-IDs", names(gdt))
          # now get ids vector:
          ids_vector <- unique( dts[[ as.numeric(input$dt) ]][[ names(ltid)[ as.numeric(input$id) ] ]] )
          group_vector <- ids_vector
          cat( "\n  group_vector : ", group_vector )
          cat( "\n  ids_vector - COL : ", names(ltid)[ as.numeric(input$id) ] )
          cat( "\n  ids_vector : ", ids_vector, "  \n\n" )

        } else { # grab the group col for variable-first or timetable

          cat( "\n  group_vector : COL" )
          # retrieve the group names AGAIN - as its not updated globally from previous observation function!
          gdt <- get_group_cols(dts[[as.numeric(input$dt)]])
          ltid <- get_group_selection_list(gdt) # includes all-IDs
          #gdt <- dplyr::select( dts[[ as.numeric(input$dt) ]], dplyr::starts_with("group-") )
          #ltid <- as.list(1:(length(gdt)+2) )
          #names(ltid) <- c("ALL", "all-IDs", names(gdt))
          # now get ids vector:
          ids_vector <- unique( dts[[ as.numeric(input$dt) ]][[ names(ltid)[ as.numeric(input$id) ] ]] )
          group_vector <- ids_vector
          cat( "\n  group_vector : ", group_vector )
          cat( "\n  ids_vector - COL : ", names(ltid)[ as.numeric(input$id) ] )
          cat( "\n  ids_vector : ", ids_vector, "  \n\n" )

        }

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
          datatable_name =  names(dts)[ as.numeric(input$dt) ],
          data_cols = data_cols,
          ids_vector = ids_vector,
          default_data_vals = list(),
          dt_length = 100,
          summarise_reps = input$summarise_reps,
          all_reps = input$all_reps
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
          datatable_name =  names(dts)[ as.numeric(input$dt) ],
          var_names = data_cols,
          group_names = group_vector,
          default_data_vals = list(),
          dt_length = 100
        )

      } else if( as.numeric(input$type) == 3 ) {

        #### datatable add data timetable rmd ####
        projectmanagr::datatable_add_data_timetable_rmd(
          rmd_path = path,
          rmd_line = row,
          datatable_name =  names(dts)[ as.numeric(input$dt) ],
          step_names = data_cols,
          group_names = group_vector,
          col_name = input$colName,
          dt_length = 100
        )
      }

      # navigate to path & close addin:
      addin_rstudio_nav(path)
    }
  })
}



ad_dt_add_data_template <- function(path, contents, row, indices, indices_row) {

  # identify the orgPath from current working directory - to retrieve the settings yaml file
  orgPath <- confirm_org_path_addin(normalizePath(path), "Add Data Datatable")

  # get settings yml for runGadget call
  settings <- get_settings_yml(orgPath)

  # get the template_dt_vector:
  startrow <- indices_row[length(indices_row)]
  endrow <- indices[length(indices_row)+1]
  template_dt_vector <- contents[ startrow : endrow ]

  # get current table name function & possible output name
  table_name_function_vector <- get_dt_vector_name_function(template_dt_vector)
  table_name <- table_name_function_vector[1] # TEMPLATE
  table_function <- table_name_function_vector[2] # ADD_DATA

  # collect all sample data from the current Rmd up to SELECTED LINE
  lt <- get_dts_named_list(contents, row)

  # # get add _data named list of types
  # type <- get_datatable_add_data_types_list()
  #
  # # select group cols from first datatable INITIALLY
  # gdt <- get_group_cols(dts[[1]])
  # ltid <- get_id_group_selection_list(gdt) # includes all-IDs

  check_dt_add_data_template(indices, indices_row, table_name, table_function)
  # After these basic checks, now can generate shiny gadget for user to provide input

  # generate separate gadgets depending on whether the output name is given
  if( length(table_name_function_vector) > 2 ) { # if a third index exists this is the datatable_name - given after `TEMPLATE : CREATE : {{template_name}}`

    ##### if NAMED TEMPLATE : use name to gen dt #####
    # no need to run a gadget - just execute the function : datatable_add_data_samples_template_rmd()
    # default to not summarise_reps - to do this, user must NOT name the datatable

    datatable_add_data_samples_template_rmd(
      rmd_path = path,
      rmd_line = row,
      datatable_name=table_name_function_vector[3],
      dt_length = 100,
      summarise_reps = FALSE,
      all_reps = FALSE
    )

  } else {

    ##### if UNNAMED TEMPLATE : gen ui for name #####
    runGadget(dt_add_data_template_ui(path, row, lt),
              dt_add_data_template_server,
              viewer=dialogViewer("Add Data to Datatable from Template", width=settings[["GadgetWidth"]], height=settings[["GadgetHeight"]]))
  }
}


check_dt_add_data_template <- function(indices, indices_row, table_name, table_function) {
  # check the datatable is complete - it has an end
  if(length(indices) <= length(indices_row) ) {
    stop( paste0("  Cannot identify next datatable separator - check syntax?") )
  }
  # confirm table is named/of type TEMPLATE
  if(table_name != "TEMPLATE") {
    stop( paste0("  Cursor is inside a datatable that is not a TEMPLATE - rename table or move cursor!") )
  }

  # confirm table function is CREATE
  if(table_function != "ADD_DATA") {
    stop( paste0("  Cursor is inside a datatable that is not of function ADD_DATA - use appropriate `addin_datatable` function: ", table_function) )
  }
}


dt_add_data_template_ui <- function(path, row, lt) {

  ui <- miniPage(

    gadgetTitleBar("Add Data to Datatable Template from Existing Table"),

    miniContentPanel(

      fillCol(

        fillRow( p("Add IDs from existing an datatable to the datatable template in the active Rmd.") ),

        fillRow(
          helpText('Rmd: ', path, align='center'),
          helpText('cursor position (line): ', row, align='center' ) ),

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

        fillRow( checkboxInput("summarise_reps", "Summarise Sample Reps", width="50%"),
                 checkboxInput("all_reps", "Select ALL Sample Reps", width="50%"))
        )
    )
  )

  ui
}


dt_add_data_template_server <- function(input, output, session) {

  # get initial instance variables
  path <- get_context_path()
  row <- get_context_row()
  contents <- get_context_contents()
  settings <- get_settings_yml(find_org_directory(path))

  # collect all sample data from the current Rmd up to SELECTED LINE
  dts <- datatable_read_vector(contents[1:row])
  dt_names <- names(dts)
  lt <- as.list( 1:length(dt_names) )
  names(lt) <- dt_names

  observeEvent(input$done, {

    #### datatable add data samples template rmd ####

    # create datatable from params and put into path at row
    projectmanagr::datatable_add_data_samples_template_rmd(
      rmd_path = path,
      rmd_line = row,
      datatable_name = names(dts)[ as.numeric(input$dt) ],
      dt_length = 100,
      summarise_reps = input$summarise_reps,
      all_reps = input$all_reps
    )

    # navigate to path & close addin:
    addin_rstudio_nav(path)
  })
}


#### _______________________________________________ ####



#' Add Datatable containing groups to existing samples
#'
#' Generates a Shiny Gadget for adding group assignment data to existing samples
#' in a new datatable.
#'
#' User selects a datatable name : list of existing datatables
#' in the current Rmd.
#'
#' User defines the data table column names and values : these MUST begin with
#' the prefix `group` to define each group set.  Then the group codes are
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

  # get data from rstudio context
  path <- get_context_path()
  contents <- get_context_contents()
  row <- get_context_row() # this is rowStart!
  rowEnd <- get_context_row_end()

  # indices of datatable delimiter lines - that begin with "+==="
  indices <- which( startsWith( contents, datatable_get_delimiter()) )
  indices_row <- which( startsWith( contents[1:row], datatable_get_delimiter()) )

  # SAVE the document before processing!
  save_context_doc()

  # from indices determine if the cursor is sitting IN or OUT of a datatable
  if( (length(indices_row) %% 2) == 1) {
    #### IF CURSOR IN DATATABLE : TEMPLATE ####

    # do nothing - do not support TEMPLATES for GROUP Datatables

    #ad_dt_add_group_template(path, contents, row, indices, indices_row)

  } else {
    ##### ELSE CURSOR OUT DATATABLE : Create Datatable ####
    # cursor lays OUTSIDE of a datatable - here will just generate shiny gadget to generate a GROUP datatable:

    ad_dt_add_group(path, contents, row, rowEnd)

  }

}


ad_dt_add_group <- function(path, contents, row, rowEnd) {

  # identify the orgPath from current working directory - to retrieve the settings yaml file
  orgPath <- confirm_org_path_addin(normalizePath(path), "Add Group Datatable")

  # get settings yml for runGadget call
  settings <- get_settings_yml(orgPath)

  # check selected text for group declaration
  group_declaration <- format_group_declaration_bullets(contents[row:rowEnd])

  # collect all sample data from the current Rmd UP TO TEMPLATE
  lt <- get_dts_named_list(contents, row)

  runGadget(dt_add_group_ui(path, row, lt, group_declaration),
            dt_add_group_server,
            viewer=dialogViewer("Add Group to Datatable", width=settings[["GadgetWidth"]], height=settings[["GadgetHeight"]]))
}

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


dt_add_group_ui <- function(path, row, lt, group_declaration) {
  ui <- miniPage(

    gadgetTitleBar("Add Groups to Datatable"),

    miniContentPanel(

      fillCol( #flex = c(1,1,1,1,20),

        fillRow( p("Add Group Data to a datatable in the active Rmd at the cursor from input params.") ),

        fillRow(
          helpText('Rmd: ', path, align='center'),
          helpText('cursor position (line): ', row, align='center' ) ),

        fillRow( p("Choose which datatable the new group data columns will be added to.") ),

        fillRow( selectInput("dt", "Select Datatable",
                             choices = lt, selected = 1, width="100%")  ),

        #fillRow ( DT::dataTableOutput("grouptable", height = "100%") )

        fillRow( p("Define the group data column names and values - these MUST be unique to the selecred datatable.") ),

        fillRow( p("  Each group column name must begin with `group-`, then space-separated group names are given.") ),

        fillRow( p("  Repeat for all groups.  eg. group-solvent-inc MeOH-DCM 1P group-ab-inc-conc 1mg/mL 0.5mg/mL") ),

        fillRow(  textInput("data_cols", "Group Data Cols. to add (space-separated):", value = group_declaration, width="100%")  ),

        fillRow(  span( textOutput("warningName"), style="color:red")  ),

        fillRow( checkboxInput("summarise_reps", "Summarise Sample Reps", width="50%"),
                 checkboxInput("all_reps", "Select ALL Sample Reps", width="50%"))

      )
    )
  )
  ui
}


dt_add_group_server <- function(input, output, session) {

  # get initial instance variables
  path <- get_context_path()
  contents <- get_context_contents()
  row <- get_context_row() # this is rowStart!
  rowEnd <- get_context_row_end()
  settings <- get_settings_yml(find_org_directory(path))

  # check selected text for group declaration
  group_declaration <- format_group_declaration_bullets(contents[row:rowEnd])

  # collect all sample data from the current Rmd UP TO TEMPLATE
  dts <- datatable_read_vector(contents[1:(row)])
  dt_names <- names(dts)
  lt <- as.list( 1:length(dt_names) )
  names(lt) <- dt_names

  # # generate a blank table to initialise addin with
  # ID <- dts[[1]]$ID # ID: Each Sample ID extracted from currently selected dt
  # GROUP1 <- rep("", length(ID))
  # GROUP2 <- rep("", length(ID))
  # GROUP3 <- rep("", length(ID))
  #
  # group_summary <- tibble::tibble(ID, GROUP1, GROUP2, GROUP3)
  #
  # # set global summary initially to the group_summary computed above
  # global <- reactiveValues(summary = group_summary)
  #
  # output$grouptable <- DT::renderDT( global$summary,
  #                                  selection = 'none',
  #                                  class = 'cell-border',
  #                                  #editable = list(target = "cell", disable = list(columns = c(1:7))),
  #                                  editable = TRUE,
  #                                  filter = 'top',
  #                                  caption = 'Group Summary Table',
  #                                  fillContainer = TRUE,
  #                                  options = list(
  #                                    lengthMenu = c(20, 50, 100),
  #                                    pageLength = 20,
  #                                    columnDefs = list(list(className = 'dt-center', targets = c(1:4))) ),
  #                                  server = FALSE ) # processing on client-side means edits to IMPORT col are kept when searching the table
  #
  # # edit a column - this does not work without creating a JSON error in the gadget
  # #observeEvent(input$grouptable_cell_edit, {
  # #  global$summary <<- DT::editData(global$summary, input$grouptable_cell_edit, 'grouptable')
  # #})
  #
  # # this edits the original table correctly - but only when editing by CELL - editing by row or column does not complete
  # # therefore does not set input$grouptable_cell_edit
  # observeEvent(input$grouptable_cell_edit, {
  #   global$summary[input$grouptable_cell_edit$row,input$grouptable_cell_edit$col] <<- input$grouptable_cell_edit$value
  # })


  observeEvent(input$done, {

    #col_name_warning <- ""
    # check the selected dt does not contain the defined data cols:
    #dt_col_names <- names(dts[[as.numeric(input$dt)]])

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
    if( regexpr("group", input$data_cols) == -1 ) {
    #if( any( startsWith( strsplit(input$data_cols, split=' '), "group") == FALSE ) == TRUE ) {

      # set the warningName TextOutput:
      output$warningName <- renderText({
        "Group Col definition does not contain a `group` col header!"
      })

    } else {

      # split data_cols at the `group` value
      group_cols <- paste0("group", unlist( strsplit(as.character(input$data_cols), 'group') ) )
      # remove FIRST index
      group_cols <- group_cols[2:length(group_cols)]
      # NOW split group_cols at spaces
      group_cols <- strsplit(as.character(group_cols), ' ')

      # create group_names and group_values
      group_names <- unlist(lapply(group_cols, function(l) l[[1]]))
      group_values <- lapply(group_cols, function(l) l[2:length(l)])

      projectmanagr::datatable_add_group_rmd(
        rmd_path = path,
        rmd_startline = row,
        rmd_endline = rowEnd,
        group_names = group_names,
        datatable_name =  names(dts)[ as.numeric(input$dt) ],
        groups = group_values,
        dt_length = 100,
        summarise_reps = input$summarise_reps,
        all_reps = input$all_reps
      )

      # navigate to path & close addin:
      addin_rstudio_nav(path)

    }
  })
}

#### _______________________________________________ ####



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

  # get data from rstudio context
  path <- get_context_path()
  contents <- get_context_contents()
  row <- get_context_row() # this is rowStart!

  # cat("path: ", path, "\n")
  # cat("row: ", row, "\n")

  # indices of datatable delimiter lines - that begin with "+==="
  indices <- which( startsWith( contents, datatable_get_delimiter()) )
  indices_row <- which( startsWith( contents[1:row], datatable_get_delimiter()) )

  # cat("indices: ", indices, "\n")
  # cat("indices_row: ", indices_row, "\n")

  # SAVE the document before processing!
  save_context_doc()

  # from indices determine if the cursor is sitting IN or OUT of a datatable

  #### IF CURSOR IN DATATABLE : TEMPLATE ####

  if( (length(indices_row) %% 2) == 1 ) {
    # cursor is sitting IN a datatable - potentially expanding a TEMPLATE datatable ADD_DATA
    ad_dt_resample_template(path, contents, row, indices, indices_row)

  } else { #### ELSE CURSOR OUT DATATABLE : Create Datatable ####
    # cursor lays OUTSIDE of a datatable - here will just generate shiny gadget to CREATE a datatable:
    ad_dt_resample(path, contents, row)

 }
}



ad_dt_resample <- function(path, contents, row) {

  # identify the orgPath from current working directory - to retrieve the settings yaml file
  orgPath <- confirm_org_path_addin(normalizePath(path), "Resample Datatable")

  # get settings yml for runGadget call
  settings <- get_settings_yml(orgPath)

  # datatable names from the current Rmd UP TO TEMPLATE -> SELECTED LINE
  lt <- get_dts_named_list(contents, row)

  runGadget(dt_resample_ui(path, row, lt),
            dt_resample_server,
            viewer=dialogViewer("Resample Datatable", width=settings[["GadgetWidth"]], height=settings[["GadgetHeight"]]))


}

dt_resample_ui <- function(path, row, lt) {

 ui <- miniPage(

    gadgetTitleBar("Resample Datatable"),

    miniContentPanel(

      fillCol(

        fillRow( p("Resample a datatable in the active Rmd at the cursor from input params.") ),

        fillRow(
          helpText('Rmd: ', path, align='center'),
          helpText('cursor position (line): ', row, align='center' ) ),

        fillRow( p("Choose which datatable to resample.") ),

        fillRow( selectInput("dt", "Select Datatable",
                             choices = lt, selected = 1, width="100%")  ),

        fillRow( p("Define the resample vector - these MUST be unique to this new datatable.") ),

        fillRow(  textInput("data_cols", "Resample vector (space-separated):", width="100%")  ),

        fillRow(  span( textOutput("warningName"), style="color:red")  ),

        fillRow( checkboxInput("summarise_reps", "Summarise Sample Reps", width="50%"),
                 checkboxInput("all_reps", "Select ALL Sample Reps", width="50%"))

      )
    )
  )
 ui
}


dt_resample_server <- function(input, output, session) {

  # get initial instance variables
  path <- get_context_path()
  contents <- get_context_contents()
  row <- get_context_row() # this is rowStart!
  settings <- get_settings_yml(find_org_directory(path))

  # collect all sample data from the current Rmd UP TO TEMPLATE
  dts <- datatable_read_vector(contents[1:(row)])

  observeEvent(input$done, {

      # split data_cols at spaces
      data_cols <- unlist( strsplit(as.character(input$data_cols), ' ') )

      if( identical(data_cols, character(0) ) ) {
        data_cols <- ""
      }

      #### datatable resample rmd ####
      projectmanagr::datatable_resample_rmd(

        rmd_path = path,
        rmd_line = row,
        datatable_name = names(dts)[ as.numeric(input$dt)],
        resample_vector = data_cols,
        rep_vector = rep(1, length(data_cols)),
        dt_length = 100,
        summarise_reps = input$summarise_reps,
        all_reps = input$all_reps
      )
      #cat("rmd_path: ", path, "\n")
      #cat("rmd_line: ", row, "\n")
      #cat("datatable_name: ", names(dts)[ as.numeric(input$dt) ], "\n")
      #cat("resample_vector: ", data_cols, "\n")
      #cat("length resample_vector: ", length(data_cols), "\n")
      #cat("dt_length: ", 100, "\n")

      # navigate to path & close addin:
      addin_rstudio_nav(path)
  })
}


ad_dt_resample_template <- function(path, contents, row, indices, indices_row) {

  # identify the orgPath from current working directory - to retrieve the settings yaml file
  orgPath <- confirm_org_path_addin(normalizePath(path), "Resample Datatable")

  # get settings yml for runGadget call
  settings <- get_settings_yml(orgPath)

  # get the template_dt_vector:
  startrow <- indices_row[length(indices_row)]
  endrow <- indices[length(indices_row)+1]
  template_dt_vector <- contents[ startrow : endrow ]

  # get current table name function & possible output name
  table_name_function_vector <- get_dt_vector_name_function(template_dt_vector)
  table_name <- table_name_function_vector[1] # TEMPLATE
  table_function <- table_name_function_vector[2] # ADD_DATA

  # collect all sample data from the current Rmd up to SELECTED LINE
  lt <- get_dts_named_list(contents, row)

  check_dt_resample_template(indices, indices_row, table_name, table_function, template_dt_vector)
  # After these basic checks, now can generate shiny gadget for user to provide input

  # generate separtate gadgets depending on whether the output name is given
  if( length(table_name_function_vector) > 2 ) { # if a third index exists this is the datatable_name - given after `TEMPLATE : CREATE : {{template_name}}`

    ##### if NAMED TEMPLATE : use name to gen dt #####
    # no need to run a gadget - just execute the function : datatable_add_data_samples_template_rmd()
    # default to not summarise_reps - to summarise_reps this, user must NOT name the datatable

    datatable_resample_template_rmd(
      rmd_path = path,
      rmd_line = row,
      datatable_name=table_name_function_vector[3],
      dt_length = 100,
      summarise_reps = FALSE,
      all_reps = FALSE
    )

  } else {

    ##### if UNNAMED TEMPLATE : gen ui for name #####

    runGadget(dt_resample_template_ui(path, row, lt),
              dt_resample_template_server,
              viewer=dialogViewer("Resample Datatable from Template", width=settings[["GadgetWidth"]], height=settings[["GadgetHeight"]]))
  }
}


check_dt_resample_template <- function(indices, indices_row, table_name,
                                       table_function, template_dt_vector) {
  # check the datatable is complete - it has an end
  if(length(indices) <= length(indices_row) ) {
    stop( paste0("  Cannot identify next datatable separator - check syntax?") )
  }
  # confirm table is named/of type TEMPLATE
  if(table_name != "TEMPLATE") {
    stop( paste0("  Cursor is inside a datatable that is not a TEMPLATE - rename table or move cursor!") )
  }

  # confirm table function is RESAMPLE
  if(table_function != "RESAMPLE") {
    stop( paste0("  Cursor is inside a datatable that is not of function RESAMPLE - use appropriate `addin_datatable` function: ", table_function) )
  }

  # extract as list
  template_list <- datatable_extract(template_dt_vector)

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

}


dt_resample_template_ui <- function(path, row, lt) {

  ui <- miniPage(

    gadgetTitleBar("Resample Datatable Template from Existing Table"),

    miniContentPanel(

      fillCol(

        fillRow( p("Resample a datatable in the active Rmd at the cursor from input params.") ),

        fillRow(
          helpText('Rmd: ', path, align='center'),
          helpText('cursor position (line): ', row, align='center' ) ),

        fillRow( p("Choose which datatable to resample.") ),

        fillRow( selectInput("dt", "Select Datatable",
                             choices = lt, selected = 1, width="100%")  ),

        fillRow(  span( textOutput("warningName"), style="color:red")  ),

        fillRow( checkboxInput("summarise_reps", "Summarise Sample Reps", width="50%"),
                 checkboxInput("all_reps", "Select ALL Sample Reps", width="50%"))
      )
    )
  )
  ui
}


dt_resample_template_server <- function(input, output, session) {

  # get initial instance variables
  path <- get_context_path()
  contents <- get_context_contents()
  row <- get_context_row() # this is rowStart!
  settings <- get_settings_yml(find_org_directory(path))

  # collect all sample data from the current Rmd UP TO TEMPLATE
  dts <- datatable_read_vector(contents[1:(row)])

  observe({
    output$warningName <- renderText(input$dt)
  })

  observeEvent(input$done, {

    #### datatable resample template rmd ####

    projectmanagr::datatable_resample_template_rmd(
      rmd_path = path,
      rmd_line = row,
      datatable_name = names(dts)[ as.numeric(input$dt) ],
      dt_length = 100,
      summarise_reps = input$summarise_reps,
      all_reps = input$all_reps
    )

    # navigate to path & close addin:
    addin_rstudio_nav(path)
  })
}

#### _______________________________________________ ####


#' Generate Dispose Datatable
#'
#' This function determines whether the cursor in the active RStudio document is
#' inside or outside a datatable template, and performs the appropriate action:
#' either filling a datatable DSPOSE template or generating a new DISPOSE datatable.
#'
#' @details
#' The function uses delimiter markers (e.g., "+===") in the document to identify
#' datatable boundaries. If the cursor is inside a datatable, the function invokes
#' template-specific processing. Otherwise, it generates a new DISPOSE datatable.
#'
#' The function also saves the document before processing to ensure changes
#' are preserved.
#'
#' @return
#' This function does not return a value but modifies the active RStudio document
#' by creating or processing datatables.
#'
#' @examples
#' # Run this function as an RStudio add-in with a document open:
#' addin_datatable_dispose()
#'
#' # Cursor inside a datatable:
#' # - The template-specific function `ad_dt_dispose_template` is called.
#'
#' # Cursor outside a datatable:
#' # - The function `ad_dt_dispose` creates a new datatable at the cursor's position.
#'
#' @export
addin_datatable_dispose <- function() {

  #### instance variables ####

  # get data from rstudio context
  path <- get_context_path()
  contents <- get_context_contents()
  row <- get_context_row()

  # indices of datatable delimiter lines - that begin with "+==="
  indices <- which( startsWith( contents, datatable_get_delimiter()) )
  indices_row <- which( startsWith( contents[1:row], datatable_get_delimiter()) )

  # SAVE the document before processing!
  save_context_doc()

  # from indices determine if the cursor is sitting IN or OUT of a datatable
  if( (length(indices_row) %% 2) == 1 ) { ##### If Cursor Inside Datatable : fill Dispose TEMPLATE Datatable ####
    ad_dt_dispose_template(path, contents, row, indices, indices_row)

  } else { ##### Else Cursor Outside Datatable : generate new Dispose Datatable ####
    ad_dt_dispose(path, contents, row)
  }

}


ad_dt_dispose <- function(path, contents, row) {

  # identify the orgPath from current working directory - to retrieve the settings yaml file
  orgPath <- confirm_org_path_addin(normalizePath(path), "Dispose Datatable")

  # get settings yml for runGadget call
  settings <- get_settings_yml(orgPath)

  # collect all sample data from the current Rmd up to SELECTED LINE
  dts <- datatable_read_vector(contents[1:row])
  lt <- get_dts_named_list(contents, row)

  cdt=get_datetime()

  runGadget(dt_dispose_ui(path, row, lt, cdt),
            dt_dispose_server,
            viewer=dialogViewer("Dispose Datatable", width=settings[["GadgetWidth"]], height=settings[["GadgetHeight"]]))
}


dt_dispose_ui <- function(path, row, lt, cdt) {

  ui <- miniPage(

    shinyjs::useShinyjs(), # to enable & disable timetable col_name input

    gadgetTitleBar("Dispose Datatable"),

    miniContentPanel(

      fillCol(

        fillRow( p("Dispose Samples from a datatable in the active Rmd at the cursor position.") ),

        fillRow(
          helpText('Rmd: ', path, align='center'),
          helpText('cursor position (line): ', row, align='center' ) ),

        fillRow( h4("Choose datatable:", align="center") ),

        fillRow( selectInput("dt", "Select Datatable",
                             choices = lt, selected = 1, width="100%")  ),

        fillRow( h4("Datetime of disposal: ", align="center") ),

        fillRow(  textInput("cdt", "Datetime:", width="100%", value = cdt)  ),

        fillRow(  span( textOutput("warningName"), style="color:red")  ),

        fillRow( checkboxInput("summarise_reps", "Summarise Sample Reps", width="50%"),
                 checkboxInput("all_reps", "Select ALL Sample Reps", width="50%"))
      )
    )
  )

  ui
}


dt_dispose_server <- function(input, output, session) {

  # get initial instance variables
  path <- get_context_path()
  contents <- get_context_contents()
  row <- get_context_row() # this is rowStart!
  settings <- get_settings_yml(find_org_directory(path))

  # collect all sample data from the current Rmd UP TO TEMPLATE
  dts <- datatable_read_vector(contents[1:(row)])

  observeEvent(input$done, {

    #### datatable dispose rmd ####
    projectmanagr::datatable_dispose_rmd(
      rmd_path = path,
      rmd_line = row,
      datatable_name = names(dts)[ as.numeric(input$dt)],
      dt_length = 100,
      summarise_reps = input$summarise_reps,
      all_reps = input$all_reps,
      cdt = input$cdt
    )

    # navigate to path & close addin:
    addin_rstudio_nav(path)
  })
}


ad_dt_dispose_template <- function(path, contents, row, indices, indices_row) {

  # identify the orgPath from current working directory - to retrieve the settings yaml file
  orgPath <- confirm_org_path_addin(normalizePath(path), "Dispose Datatable")

  # get settings yml for runGadget call
  settings <- get_settings_yml(orgPath)

  # get the template_dt_vector:
  startrow <- indices_row[length(indices_row)]
  endrow <- indices[length(indices_row)+1]
  template_dt_vector <- contents[ startrow : endrow ]

  # get current table name function & possible output name
  table_name_function_vector <- get_dt_vector_name_function(template_dt_vector)
  table_name <- table_name_function_vector[1] # TEMPLATE
  table_function <- table_name_function_vector[2] # ADD_DATA

  # collect all sample data from the current Rmd up to SELECTED LINE
  lt <- get_dts_named_list(contents, row)

  check_dt_dispose_template(indices, indices_row, table_name, table_function, template_dt_vector)
  # After these basic checks, now can generate shiny gadget for user to provide input

  # generate separtate gadgets depending on whether the output name is given
  if( length(table_name_function_vector) > 2 ) { # if a third index exists this is the datatable_name - given after `TEMPLATE : CREATE : {{template_name}}`

    # no need to run a gadget - just execute the function : datatable_add_data_samples_template_rmd()
    # default to not summarise_reps - to summarise_reps this, user must NOT name the datatable

    datatable_dispose_template_rmd(
      rmd_path = path,
      rmd_line = row,
      datatable_name=table_name_function_vector[3],
      dt_length = 100,
      summarise_reps = FALSE,
      all_reps = FALSE
    )

  } else {

    runGadget(dt_dispose_template_ui(path, row, lt),
              dt_dispose_template_server,
              viewer=dialogViewer("Resample Datatable from Template", width=settings[["GadgetWidth"]], height=settings[["GadgetHeight"]]))
  }
}


check_dt_dispose_template <- function(indices, indices_row, table_name,
                                       table_function, template_dt_vector) {
  # check the datatable is complete - it has an end
  if(length(indices) <= length(indices_row) ) {
    stop( paste0("  Cannot identify next datatable separator - check syntax?") )
  }
  # confirm table is named/of type TEMPLATE
  if(table_name != "TEMPLATE") {
    stop( paste0("  Cursor is inside a datatable that is not a TEMPLATE - rename table or move cursor!") )
  }

  # confirm table function is RESAMPLE
  if(table_function != "DISPOSE") {
    stop( paste0("  Cursor is inside a datatable that is not of function DISPOSE - use appropriate `addin_datatable` function: ", table_function) )
  }

  # extract as list
  template_list <- datatable_extract(template_dt_vector)

  # remove IDs col - first index
  template_list <- template_list[2:length(template_list)]

  # confirm table has resample as first col
  if(template_list[[1]][1] != "dispose") {
    stop( paste0("  DISPOSE datatable not correctly formed - first col must be `dispose`: ", template_list[[1]][1]) )
  }
}


dt_dispose_template_ui <- function(path, row, lt) {

  ui <- miniPage(

    gadgetTitleBar("Dispose Datatable Template from Existing Table"),

    miniContentPanel(

      fillCol(

        fillRow( p("Dispose Samples from a datatable in the active Rmd at the cursor position.") ),

        fillRow(
          helpText('Rmd: ', path, align='center'),
          helpText('cursor position (line): ', row, align='center' ) ),

        fillRow( p("Choose which datatable to dispose samples") ),

        fillRow( selectInput("dt", "Select Datatable",
                             choices = lt, selected = 1, width="100%")  ),

        fillRow(  span( textOutput("warningName"), style="color:red")  ),

        fillRow( checkboxInput("summarise_reps", "Summarise Sample Reps", width="50%"),
                 checkboxInput("all_reps", "Select ALL Sample Reps", width="50%"))
      )
    )
  )
  ui
}


dt_dispose_template_server <- function(input, output, session) {

  # get initial instance variables
  path <- get_context_path()
  contents <- get_context_contents()
  row <- get_context_row() # this is rowStart!
  settings <- get_settings_yml(find_org_directory(path))

  # collect all sample data from the current Rmd UP TO TEMPLATE
  dts <- datatable_read_vector(contents[1:(row)])

  observe({
    output$warningName <- renderText(input$dt)
  })

  observeEvent(input$done, {

    #### datatable dispose template rmd ####

    projectmanagr::datatable_dispose_template_rmd(
      rmd_path = path,
      rmd_line = row,
      datatable_name = names(dts)[ as.numeric(input$dt) ],
      dt_length = 100,
      summarise_reps = input$summarise_reps,
      all_reps = input$all_reps
    )

    # navigate to path & close addin:
    addin_rstudio_nav(path)
  })
}


#### _______________________________________________ ####


#' Import Existing Samples and Reps from Datatables in Source Project Notes
#'
#' Generates a Shiny Gadget for importing existing samples and reps from datatables
#' that exist in source Project Note(s) into the currently active destination Project Note.
#'
#' @details
#' This function allows the user to:
#' - Select the line in the current Project Note where the IMPORT will be written.
#' - Choose a directory tree (dirTree) from the current ORGANISATION: All project notes
#'   are read recursively for existing samples/reps in datatables.
#' - View and filter a summary of existing samples/reps in a datatable.
#' - Select samples by clicking on the rows in the datatable.
#' - Select reps by filling in the rep number in the left-hand column of selected sample rows.
#' - Finalize the operation by clicking "DONE", which:
#'   - EXPORTED the selected samples/reps from the source Project Note: The source datatable
#'     is updated, pointing to the destination Project Note.
#'   - IMPORTED the selected samples/reps into the destination Project Note: An import
#'     datatable is created with the selected samples/reps, pointing to the source Project Note.
#'
#' @return
#' This function does not return a value but modifies the current and source project notes
#' by creating or updating import/export datatables.
#'
#' @examples
#' # Example usage:
#' # Run this function as a Shiny Gadget to interactively import/export samples/reps.
#' addin_datatable_import_export()
#'
#' # During usage:
#' # - Select the desired rows in the datatable.
#' # - Choose the "DONE" option to export and import the samples/reps accordingly.
#'
#' @export
addin_datatable_import_export <- function() {


  #### instance variables ####

  # get data from rstudio context
  path <- get_context_path()
  contents <- get_context_contents()
  row <- get_context_row()

  # indices of datatable delimiter lines - that begin with "+==="
  indices <- which( startsWith( contents, datatable_get_delimiter()) )
  indices_row <- which( startsWith( contents[1:row], datatable_get_delimiter()) )

  # SAVE the document before processing!
  save_context_doc()

  # from indices determine if the cursor is sitting IN or OUT of a datatable
  if( (length(indices_row) %% 2) == 1 ) { ##### If Cursor Inside Datatable : TEMPLATE ####
    ad_dt_import_export_template(path, contents, row, indices, indices_row)

  } else { ##### Else Cursor Outside Datatable : Import Export Datatable ####
    ad_dt_import_export(path, contents, row)
  }
}


ad_dt_import_export <- function(path, contents, row) {

  # identify the orgPath from current working directory - to retrieve the settings yaml file
  orgPath <- confirm_org_path_addin(normalizePath(path), "Import/Export Datatable")

  # get settings yml for runGadget call
  settings <- get_settings_yml(orgPath)

  runGadget(dt_import_export_ui(path, row), dt_import_export_server,
            viewer=dialogViewer("Import/Export Datatable", width=settings[["GadgetWidth"]], height=settings[["GadgetHeight"]]))

}



dt_import_export_ui <- function(path, row) {

  ui <- miniPage(

    gadgetTitleBar("Import/Export Samples"),

    miniContentPanel(

      fillCol( flex = c(1,1,1,1,20),
               # , verbatimTextOutput("rows", placeholder = TRUE)
               fillRow(
                        p("Import/Export samples & reps from datatables in Source Project Notes") ),

               fillRow(
                 helpText('Rmd: ', path, align='center'),
                 helpText('cursor position (line): ', row, align='center' ) ),

               fillRow( flex = c(7, 1, 1),
                        div(verbatimTextOutput("dirtxt", placeholder = TRUE)),
                        div(shinyDirButton("dir", "Select Directory", "Note Parent Directory"), align='center'),
                        div( actionButton("decrement", "-", class = "btn btn-danger"),
                             actionButton("increment", "+", class = "btn btn-success"), align='center' ) ),

               fillRow(   span( textOutput("warningDirectory"), style="color:red")  ),

               #fillRow( p("Choose which datatable to import samples from") ),

               #fillRow( selectInput("dt", "Select Datatable",
               #                     choices = lt, selected = 1, width="100%")  ),

               fillRow ( DT::dataTableOutput("mytable1", height = "100%") )
      )
    )
  )
  ui
}

dt_import_export_server <- function(input, output, session) {

  # get data from rstudio context
  path <- get_context_path()
  contents <- get_context_contents()
  row <- get_context_row()
  orgPath <- find_org_directory(path)
  settings <- get_settings_yml(orgPath)

  samples_summary <- gen_summary_dt()

  # compute Dir selection:
  global <- reactiveValues(datapath = fs::path_dir(path), # sets initial datapath to dir of current file
                           summary = samples_summary) # sets summay dt to initial blank table with correct columns

  # allows selection of Dir, with Volume set to HOME Dir
  shinyDirChoose(
    input,
    'dir',
    roots = c(home = orgPath), # set to orgPath - so user can ONLY select any DIR inside the ORG!
    filetypes = c('', 'Rmd')
  )

  dir <- reactive(input$dir)

  observeEvent(ignoreNULL = TRUE,
               eventExpr = {
                 input$dir
               },
               handlerExpr = {
                 if (!"path" %in% names(dir())) return() # check the path element exists in dir
                 #cat("\n dir() names: ", names(dir())) # contains : path, root
                 #cat("\n  dir$root: ", dir()$root) # name of the root selected in shinyDirChoose: home
                 #cat("\n  dir$root: ", dir()$root[['home']]) # this is not a named vector, can only return the selected name, not value!
                 #cat("\n  dir$path: _", dir()$path, "_" ) # list of each dir in dirTree - cat() cannot handle list!  so split list:
                 #cat("\n  dir$path: _", unlist( dir()$path ), "_" ) # list of each dir in dirTree
                  # can repeat output with: path <- list("0-PR-DT", "PD", "tn-ex") ; cat("\n  dir$path: _", unlist( path ), "_" )
                 #cat("\n  dir$path pasted with fileSep: _", paste( unlist( dir()$path ), collapse = .Platform$file.sep ), "_" )
                 # list of each dir in dirTree created into a path
                 #cat("\n  dir$path[-1]: _", unlist( dir()$path[-1] ), "_" ) # -1 removes first position?
                 #cat("\n  dir$path[-1] pasted with fileSep: _", paste( unlist( dir()$path[-1] ), collapse = .Platform$file.sep ), "_" )
                  # this removes the first filesep?
                 #home <- normalizePath("~")
                 # so can form the datapath selected by concatenating 'home' path (set to orgPath above in roots) and the pasted path value
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
      ##### datatable find #####
      global$summary <- projectmanagr::datatable_find(global$datapath, settings, updateProgress = updateProgress)
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


  # render output DT table from summary
  output$mytable1 <- DT::renderDT( global$summary,
                                   selection = 'multiple', # allow selection of lines
                                   class = 'cell-border',
                                   #editable = list(target = "cell", disable = list(columns = c(1:7))),
                                   editable = TRUE,
                                   filter = 'top',
                                   caption = 'Sample Summary Table',
                                   fillContainer = TRUE,
                                   options = list(
                                     lengthMenu = c(20, 50, 100),
                                     pageLength = 20,
                                     columnDefs = list(
                                       list(className = 'dt-center', targets = c(1:8)), # centre align all visible colss
                                       list(visible = FALSE, targets = c(9)) # do not show the PATH col
                                       )
                                     ),
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


  # Update the IMPORT column when the decrement button is clicked
  observeEvent(input$decrement, {
    selected <- input$mytable1_rows_selected
    if (length(selected) > 0) {
      # Update the IMPORT column for selected rows
      global$summary$IMPORT[selected] <- pmax(
        as.integer(global$summary$IMPORT[selected]) - 1,
        0
      )

      # Ensure the entire IMPORT column remains numeric (no blank strings)
      global$summary$IMPORT <- as.integer(replace(global$summary$IMPORT, is.na(global$summary$IMPORT), 0))

      # Re-render the DataTable while retaining selections
      output$mytable1 <- DT::renderDT(
        global$summary,
        selection = list(mode = "multiple", selected = selected), # Retain selected rows
        class = 'cell-border',
        editable = TRUE,
        filter = 'top',
        caption = 'Sample Summary Table',
        fillContainer = TRUE,
        options = list(
          lengthMenu = c(20, 50, 100),
          pageLength = 20,
          columnDefs = list(
            list(className = 'dt-center', targets = c(1:8)),
            list(visible = FALSE, targets = c(9))
          )
        ),
        server = FALSE
      )
    }
  })

  # Update the IMPORT column when the increment button is clicked
  observeEvent(input$increment, {
    selected <- input$mytable1_rows_selected
    if (length(selected) > 0) {
      # Ensure IMPORT does not exceed COUNT
      global$summary$IMPORT[selected] <- pmin(
        as.integer(global$summary$IMPORT[selected]) + 1,
        global$summary$COUNT[selected]
      )

      # Ensure the entire IMPORT column remains numeric (no blank strings)
      global$summary$IMPORT <- as.integer(replace(global$summary$IMPORT, is.na(global$summary$IMPORT), 0))

      # Re-render the DataTable while retaining selections
      output$mytable1 <- DT::renderDT(
        global$summary,
        selection = list(mode = "multiple", selected = selected), # Retain selected rows
        class = 'cell-border',
        editable = TRUE,
        filter = 'top',
        caption = 'Sample Summary Table',
        fillContainer = TRUE,
        options = list(
          lengthMenu = c(20, 50, 100),
          pageLength = 20,
          columnDefs = list(
            list(className = 'dt-center', targets = c(1:8)),
            list(visible = FALSE, targets = c(9))
          )
        ),
        server = FALSE
      )
    }
  })

  # perform computations to export & import samples from selected samples/reps
  observeEvent(input$done, {

    # make sure IMPORT col is written to summary
    #global$summary <<- DT::editData(global$summary, input$mytable1_cell_edit, 'mytable1')

    # filter to remove all rows where IMPORT is 0
    samp_summ <- dplyr::filter(global$summary, IMPORT > 0 )

    # add col : concat path & datatable name
    samp_summ <- dplyr::mutate(samp_summ,
                               PS = paste0(global$datapath, .Platform$file.sep, PATH, ":::", SAMPLE))

    # get all unique path/dt strings - to perform import/export on each in a for loop
    ssPSU <- unique(samp_summ$PS)

    # cur_datetime <- projectmanagr::get_datetime()
    #
    # # export samples from SOURCE Rmds
    # export_dts <- projectmanagr::datatable_export(samples_summary = samp_summ,
    #                                               destination_note_path = path,
    #                                               datetime=cur_datetime,
    #                                               summarise_reps = TRUE, dt_length = 100)
    # # AND import samples from SOURCE Rmds to DESTINATION Rmd
    # projectmanagr::datatable_import(export_datatables = export_dts,
    #                                 destination_note_path = path,
    #                                 destination_note_line = row,
    #                                 datetime=cur_datetime,
    #                                 summarise_reps = FALSE, dt_length = 100)


    #### datatable import export rmd ####

    # TODO : currently this is a bit of a hack job
    # need to figure out how best to parse the source path, source line (length of Rmd),
     # datatable name, and ids_vector/reps_vector for each source to the import/export function
    # use a hidden column in datatable for full path!

    # set destination row line initially to row
    drl <- row

    # run import/export for each source note + datatable name
    for( sp in 1:length(ssPSU) ) {
      psu <- ssPSU[sp]
      # filter samp_summ
      ss <- dplyr::filter(samp_summ, PS==psu)
      srp <- paste0(global$datapath, .Platform$file.sep, ss$PATH[1]) # file path - same in each col
      srl <- length(read_file(srp))
      dtn <- ss$SAMPLE[1] # datatable name - same in each col
      iv <- ss$ID
      rv <- define_reps_vec_import(ss$IMPORT) # user selects number of reps to import by setting the integer in IMPORT col

      # perform import/export - and save the NEW destination rmd line into the drl variable for next loop iteration
      drl <- projectmanagr::datatable_import_export_rmd(
          source_rmd_path = srp,
          source_rmd_line = srl,
          destination_rmd_path = path,
          destination_rmd_line = drl,
          settings = settings,
          datatable_name = dtn,
          ids_vector=iv,
          reps_vector=rv,
          dt_length=100,
          summarise_reps=FALSE,
          exportTemplate="Datatables-Export-Template.Rmd"
        )

    }

    # for( si in 1:length(ssID) ) {
    #   dt_name <- substring(ssID[si], (regexpr(":::", ssID[si], fixed=TRUE)+3), nchar(ssID[si]))
    #   idv
    #
    #   projectmanagr::datatable_import_export_rmd(
    #     source_rmd_path = attr(samp_summ, "paths")[[ssID[si]]],
    #     source_rmd_line = length(read_file(attr(samp_summ, "paths")[[ssID[si]]])),
    #     destination_rmd_path = path,
    #     destination_rmd_line = row,
    #     settings = settings,
    #     datatable_name = dt_name,
    #     ids_vector="",
    #     reps_vector="",
    #     dt_length=100,
    #     summarise_reps=FALSE
    #   )

      # print(global$summary)
      # print(samp_summ)

    #}

    # navigate to path & close addin:
    addin_rstudio_nav(path)
  })

}


define_reps_vec_import <- function(import_vector) {
  if( all(import_vector == 1) ) {
    return("") # return a blank string, which signals all reps are 1
  } else {
    return(import_vector) # at least one IMPORT is above 1, so return the complete vector
  }
}



#### _______________________________________________ ####


#' Insert datatable from tibble in active RStudio file
#'
#' @export
addin_datatable_insert_tibble <- function(tb, dt_name, dt_function = "CREATE",
                                          dt_length = 100 ) {

  #### instance variables ####

  # get data from rstudio context
  path <- get_context_path()
  row <- get_context_row()

  # SAVE the document before processing!
  save_context_doc()

  datatable_insert_from_tibble(path, row, tb, dt_name, dt_function, dt_length)

}


#### _______________________________________________ ####



