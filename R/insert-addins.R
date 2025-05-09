#' Insert Date Addin
#'
#' Inserts the current date into the current selection in RStudio Source
#' Editor.  Uses `YYYY/MM/DD` format.
#'
#' The timezone used is specified in settings.yml under the `DateTimeZone`
#' parameter if settings.yml can be found, otherwise it uses the `UTC`
#' timezone.
#'
#' @export
addin_insert_date <- function() {

  cat( "\nprojectmanagr::addin_insert_date():\n" )

  # get currently active doc in rstudio
  context <- rstudioapi::getSourceEditorContext()

  # get orgPath
  orgPath <- find_org_directory(context$path)

  if(orgPath == "" ) { # only if orgPath not identified
    timezone <- get_locale()
    split <- "/"
    cat( paste0("\n  No projectmanagr Organisation identified\n
         - using locale timezone & / split by default.\n
         - locale: ", timezone ) )

  } else {

    # get config templates settings yml
    confPath <- get_config_dir(orgPath)
    tempPath <- get_template_dir(orgPath)
    settings <- get_settings_yml(orgPath)

    # get timezone/locale and split char
    #timezone <- settings[['DateTimeZone']]
    timezone <- get_locale() # using current system locale!
    split <- settings[['DateSplit']]

    cat( "\n  Date inserted based on timezone : ", timezone, "\n" )
  }

  original <- context$contents

  cursor <- rstudioapi::primary_selection(context)
  line <- (cursor$range[[1]])[1] # get the line number of cursor
  col <- (cursor$range[[1]])[2] # get the col number of cursor

  #### compute date with timezone param ####
  date <- get_date(timezone, split)

  #rstudioapi::insertText( paste0('"', datetime_colon, '"' ) )
  rstudioapi::insertText( date )

} #### ________________________________ ####


#' Insert Datetime Addin
#'
#' Inserts the current datetime into the current selection in RStudio Source
#' Editor. Uses `YYYY/MM/DD:hh:mm` format.
#'
#' The timezone used is specified in settings.yml under the `DateTimeZone`
#' parameter if settings.yml can be found, otherwise it uses the `UTC`
#' timezone.
#'
#' @export
addin_insert_datetime <- function() {

  cat( "\nprojectmanagr::addin_insert_datetime():\n" )

  # get currently active doc in rstudio
  context <- rstudioapi::getSourceEditorContext()

  # get orgPath
  orgPath <- find_org_directory(context$path)

  if(orgPath == "" ) { # only if orgPath not identified
    timezone <- get_locale()
    split <- "/"
    splitTime <- ":"
    cat( paste0("\n  No projectmanagr Organisation identified\n
         - using locale timezone & / : split by default.\n
         - locale: ", timezone ) )

  } else {

    # get config templates settings yml
    confPath <- get_config_dir(orgPath)
    tempPath <- get_template_dir(orgPath)
    settings <- get_settings_yml(orgPath)

    # get timezone and split chars
    #timezone <- settings[['DateTimeZone']]
    timezone <- get_locale() # using current system locale!
    split <- settings[['DateSplit']]
    splitTime <- settings[['DateTimeSplit']]

    cat( "\n  Date inserted based on timezone : ", timezone, "\n" )
  }

  original <- context$contents

  cursor <- rstudioapi::primary_selection(context)
  line <- (cursor$range[[1]])[1] # get the line number of cursor
  col <- (cursor$range[[1]])[2] # get the col number of cursor

  #### compute datetime with timezone param ####
  datetime <- get_datetime(timezone, split, splitTime)

  #rstudioapi::insertText( paste0('"', datetime_colon, '"' ) )
  rstudioapi::insertText( datetime )

} #### ________________________________ ####



#' Insert Todo Addin
#'
#' Inserts a todo block into the current selection in RStudio Source
#' Editor. Uses format specified in `config/templates/todo-block.txt`
#'
#' @export
addin_insert_todo <- function() {

  cat( "\nprojectmanagr::addin_insert_todo():\n" )

  # get currently active doc in rstudio
  context <- rstudioapi::getSourceEditorContext()

  # get orgPath
  orgPath <- confirm_find_org(context$path)

  #### insert todo block ####
  datetime <- get_todo_block(orgPath)

  rstudioapi::insertText( datetime )

} #### ________________________________ ####



#' Insert Goal Section to end of Project Doc GDT
#'
#' Collects selection object from active document and attempts to insert a
#' new goal section to the end of this document IF a Project Doc.
#'
#' @export
addin_insert_doc_goal_section <- function() {

  # get a selection object from current rstudio source editor
  selection <- cursor_selection()


  #### insert goal section ####

  insert_doc_goal_section(selection)

  # reload file
  id <- rstudioapi::navigateToFile(selection[["filePath"]])

} #### ________________________________ ####


#' Insert Deliverable Section to end of Project Doc GDT
#'
#' Collects selection object from active document and attempts to insert a
#' new deliverable section to the end of this Project Doc's selected
#' Goal Section.
#'
#' @export
addin_insert_doc_deliverable_section <- function() {

  # get a selection object from current rstudio source editor
  selection <- cursor_selection()


  #### insert deliverable section ####

  insert_doc_deliverable_section(selection)

  # reload file
  id <- rstudioapi::navigateToFile(selection[["filePath"]])

} #### ________________________________ ####


#' Insert Task Section to end of Project Doc GDT
#'
#' Collects selection object from active document and attempts to insert a
#' new task section to the end of this Project Doc's selected
#' Goal Section.
#'
#' @export
addin_insert_doc_task_section <- function() {

  rstudioapi::documentSave() # save current file first

  # get a selection object from current rstudio source editor
  selection <- cursor_selection()


  #### insert task section ####

  insert_position <- insert_doc_task_section(selection)

  # reload file & set cursor position
  #id <- rstudioapi::documentOpen(selection[["filePath"]])
  #rstudioapi::setCursorPosition(rstudioapi::document_position(insert_position, 1), id)

  # set files pane
  #rstudioapi::filesPaneNavigate( dirname(selection[["filePath"]]) )

} #### ________________________________ ####


#' Insert a Hyperlink
#'
#' Insert a RELATIVE Hyperlink from an Open RStudio File to the currently
#' Active RStudio file, at the current cursor position.
#'
#' @export
addin_insert_hyperlink <- function() {

  cat( "\nprojectmanagr::addin_insert_hyperlink():\n" )

  #### instance variables ####

  # get currently active doc in rstudio
  context <- rstudioapi::getSourceEditorContext()

  # get contents:
  openDocContents <- context$contents

  cursor <- rstudioapi::primary_selection(context)

  line <- (cursor$range[[1]])[1]
  column <- (cursor$range[[1]])[2]

  # identify the orgPath from current working directory - to retrieve the settings yaml file
  orgPath <- find_org_directory( context$path )

  # if orgPath not identified present error interface and then stop this function
  if( orgPath == "" ) { # do nothing

  } else { # get the settings file

    # get config templates settings yml
    confPath <- get_config_dir(orgPath)
    tempPath <- get_template_dir(orgPath)
    settings <- get_settings_yml(orgPath)

  }

  # get all open RStudio Doc PATHS:
  fileList <- get_rstudio_open_doc_IDs()

  foundActiveDoc = FALSE

  reorderedFileList = list() # all files to RIGHT of current active doc are stored here in order L>R
  firstFileList = list() # all files to LEFT of current active doc are stored here in order L>R
  numberedFileList <- list() # will fill the choices var for selectInput shiny widget
  # must be a list of numbers, with the names of each item the String that is presented in selectInput

  foundIndex <- 0

  for(i in 1:length(fileList) ) {

    if(foundActiveDoc == TRUE) {
      foundIndex <- foundIndex + 1
      reorderedFileList[[foundIndex]] <- fileList[[i]][2]
      numberedFileList[(i-1)] <- i-1 # AFTER skipped Active Doc, so set list[i-1] to i-1

    } else if (fileList[[i]][2] != context$path) {
      firstFileList[[i]] <- fileList[[i]][2]
      numberedFileList[i] <- i # BEFORE skipping Active Doc, so set list[i] to i

    } else if(fileList[[i]][2] == context$path) {

      foundActiveDoc = TRUE
      # This is the Active Doc - so DO NOT add an index to numberedFileList
    }
  }

  # concat the list of file paths, with files to the RIGHT of active doc FIRST:
  reorderedFileList <- c(reorderedFileList, firstFileList)

  # form the numberedFileList (list of indexes, with names constituting the image file NAMES)
  names(numberedFileList) <- lapply(reorderedFileList, basename)

  # OR just use a list of file NAMES (indexes are implicitly understood by selectInput):
  reorderedFileNames <- lapply(reorderedFileList, basename)

  contextName <- basename(context$path)

  # present these files in a combobox in shiny gadget


  #### user interface ####

  ui <- miniPage(

    gadgetTitleBar("Insert HyperLink"),

    miniContentPanel(

      fillCol(

        fillRow( h5("Insert a new Hyperlink:") ),

        fillRow(
          h5(  paste("TO:", contextName ), align="center"  )
        ),

        fillRow(
          h6(  paste("Line:", line ), align="center"  ),
          h6(  paste("Column:", column ), align="center"  )
        ),

        fillRow(
          selectInput("select", "Select Document:",
                      choices = numberedFileList,
                      selected = numberedFileList[1],
                      width="100%")
        ),

        fillRow( h5("") ),

        fillRow( h5("") ),

        fillRow( h5("") ),

        fillRow( h5("") )

      )
    )
  )


  #### server code ####

  server <- function(input, output, session) {


    # perform computations to form new hyperlink
    observeEvent(input$done, {

      print(input$select)

      print(reorderedFileList[[as.integer(input$select)]])


      #### done : insert hyperlink ####

      # form new hyperlink:
      DocTitleLink <- create_hyperlink_no_ext(
                              reorderedFileList[[as.integer(input$select)]],
                              context$path)

      # insert into contents
      rstudioapi::insertText(cursor$range, DocTitleLink, id = context$id)

      # Close Gadget after computations are complete:
      stopApp()

    })
  }


  #### view gadget ####

  if(orgPath == "") { # use default width/height
    viewer <- dialogViewer("Insert Hyperlink", width = 1000,
                           height = 800 )
  } else { # use width/height from settings
    viewer <- dialogViewer("Insert Hyperlink",
                           width = settings[["GadgetWidth"]],
                           height = settings[["GadgetHeight"]])
  }

  runGadget(ui, server, viewer = viewer)

} #### ________________________________ ####


#' Select & Insert Content
#'
#' A Directory Tree within the Organisation is used to screen all Project Notes
#' for all declared Contents, based on the Content Delimiter Syntax (defined
#' in `settings[["ContentSep"]]`).  This Directory Tree can be adjusted.
#'
#' All Insertable Content present in Project Notes in the Directory Tree are
#' shown in a DT datatable by content title.
#'
#' Individual Content can be selected to show the summary information.
#'
#' A Preview view of the Content can be opened to read through the entire
#' Content in the ADDIN.
#'
#' Selected content can be inserted into the current selection in a
#' Destination Project Note via the DONE button.
#'
#' Using DT to display a datatable in the shiny gadget here.  See the tutorial
#' at https://rstudio.github.io/DT/shiny.html
#'
#' @export
addin_insert_content <- function() {

  cat( "\nprojectmanagr::addin_insert_content():\n\n" )


  #### instance variables ####

  # Retrieve cursor_selection from current Active Doc in rstudio:
  selection <- cursor_selection() # Project Doc, Project Note - Simple, Header, SubNote

  projNoteRmdPath <- selection[["filePath"]] # presumed to be project note Rmd
  noteInsertionIndex <- selection[["originalLineNumber"]]

  orgPath <- confirm_find_org(projNoteRmdPath)

  confPath <- get_config_dir(orgPath)
  tempPath <- get_template_dir(orgPath)
  settings <- get_settings_yml(orgPath)

  # get status yml & file - to write to
  status <- get_status_yml(orgPath, settings)
  statusFile <- get_status_yml_file(orgPath, settings)

  # read status information for CONTENTS if it exists
  contentsCache <- status[['CONTENTS']]

  # get progPath
  progPath <- find_prog_dir(projNoteRmdPath)

  cat( paste0("  progPath: ", progPath, "\n\n") )

  # ensure contents cache is up to date
  if( is.null(contentsCache) ) {

    cat("  no insertable contents cache found - searching org directory of current file for contents..\n")
    contents <- find_contents_org_tree(orgPath, settings)
    contentRetrievalDateTime <- get_datetime() # set to current datetime

    cat("    writing org insertable contents cache..\n")
    write_insertable_contents_cache(contentRetrievalDateTime,
                                    contents, orgPath, status, statusFile)

  } else {

    cat("  insertable contents cache found - ensuring cache is up-to-date...\n\n")

    # first get only the contents that relates to the current ORG PATH
    # may have cache from other paths if ORG is ever moved
    contentsCache <- list(contentsCache[[orgPath]])
    names(contentsCache) <- orgPath

    # update contents list: Check if any project Notes are updated since the contentRetrievalDateTime
    contents <- update_contents_org_tree(contentsCache, orgPath, settings) # faster as only search since laste retrieval dt

    # latest retrieval datetime - using UTC for consistent retrieval datetime logging
    contentRetrievalDateTime <- get_datetime(timezone = "UTC")

    cat("    writing org insertable contents cache..\n")
    write_insertable_contents_cache(contentRetrievalDateTime, contents, orgPath, status, statusFile)

  }


  # if CONTENTS data exists use last dir tree where a content was inserted from?
   # otherwise set the dirtree to the ORG ROOT of the current document's path
  #if( is.null(contentsStatus) ) {
  #  cat("  no insertable contents cache found - searching org directory of current file for contents..\n")
    #dirPath <- dirname(projNoteRmdPath)
  #  dirPath <- orgPath
  #  contents <- find_contents_in_dir_tree(progPath, orgPath, settings)
  #  contentRetrievalDateTime <- get_datetime() # set to current datetime

  #} else {

  #  cat("  insertable contents cache found - updating cache...\n\n")
    # identify the latest used dirPath from metadata stored in contentsStatus
  #  contentDirPaths <- names(contentsStatus)
  #  dts <- lapply(X = contentsStatus, FUN = `[[`, "contentRetrievalDateTime")
  #  dts <- lubridate::ymd_hm(dts)
  #  dtsl <- max(dts) # get maximum value - or LATEST DATETIME
  #  dtsli <- which(dts == dtsl) # get the index
  #  dirPath <- contentDirPaths[[dtsli]]

    # validate the contents! Check if any project Notes are updated since the contentRetrievalDateTime
  #  contentsStatus[[dirPath]] <- update_contents_in_list(contentsStatus, dirPath, orgPath, settings)

    # latest retrieval datetime
  #  contentRetrievalDateTime <- contentsStatus[[dirPath]]$contentRetrievalDateTime

    # latest contents
  #  contents <- contentsStatus[[dirPath]]$contents

  #  cat( paste0("    displaying most recent contents from path: ", dirPath, "\n") )

  #}

  # open content sep delimiter
  contentSepContents <- load_param_vector(settings[["ContentSep"]], orgPath)

  # create vector of possible roots for dir selection in server()
  roots <- c(orgPath) # can use orgPath ONLY as roots
  names(roots) <- c(basename(orgPath))

  cat("  generate initial DT:")

  # generate a blank table to hold content names in
  CONTENT_NAME <- as.character(lapply(X = contents, FUN = `[[`, "contentTitle"))
  pathLine <- strsplit(names(contents), ':::', fixed=TRUE)
  CONTENT_LINE_NUM <- c()
  CONTENT_NOTE_PATH <- c()
  CONTENT_NOTE_NAME <- c()
  for(pl in 1:length(pathLine) ) {
    plc <- pathLine[[pl]]
    CONTENT_LINE_NUM[pl] <- as.character(plc[2]) #second index
    CONTENT_NOTE_PATH[pl] <- as.character(plc[1]) #first index
    CONTENT_NOTE_NAME[pl] <- basename( as.character(plc[1])) #first index
  }

  contentsTable <- tibble::tibble(CONTENT_NAME, CONTENT_LINE_NUM, CONTENT_NOTE_NAME)


  #### user interface ####

  ui <- miniPage(

    gadgetTitleBar("Insert Content"),

    miniContentPanel(

      fillCol( flex = c(1,1,20,1,1,1,4), #flex = c(1,1,1,20,1,1,1,4),

               fillRow( p("Insert Content into the current Project Note") ),

               #fillRow(   span( textOutput("warningDirectory"), style="color:red")  ),

               #fillRow( flex = c(5, 1),  verbatimTextOutput("dir", placeholder = TRUE), shinyDirButton("dir", "Select Directory", "Note Directory Tree")  ),

               #fillRow(   textOutput("projectNotePath")  ),

               fillRow( br() ),

               fillRow ( DT::DTOutput("mytable1", height = "100%") ),

               fillRow( br() ),

               fillRow( h3("Content Description") ),

               fillRow( br() ),

               fillRow( h4( htmlOutput("contentDescription") ) )

      )
    )
  )


  #### server code ####

  server <- function(input, output, session) {


    #### compute Dir selection ####

    #global <- reactiveValues(datapath = orgPath )
    # this sets initial value of global$datapath


    # allows selection of Dir, with roots set to project doc DIR or ORG Dir
    #shinyDirChoose(
    #  input, 'dir',
    #  defaultRoot = names(roots)[1], # set default to first root
      #defaultPath = global$datapath,
    #  roots=roots, # can use orgPath only as roots
    #  filetypes = c('', 'txt', 'Rmd', "tsv", "csv", "bw")
    #)

    #dir <- reactive(input$dir) # make input dir REACTIVE to enable update of global datapath when changed

    # show the global$datapath computed from input$dir in output$dir (next to shinyDirButton!)
    #output$dir <- renderText({ global$datapath })

    # update global$datapath
    #observeEvent(ignoreNULL = TRUE,
    #             eventExpr = { # if input$dir is changed/set
    #               input$dir
    #             },
    #             handlerExpr = { # update datapath with dir() list
    #               if (!"path" %in% names(dir())) return() # check the path element exists in dir
                   #cat("\n dir() names: ", names(dir())) # contains : root, path
                   #cat("\n  dir$root: ", dir()$root) # name of the root selected in shinyDirChoose
                   #cat("\n  dir$path: _", unlist( dir()$path ), "_" ) # list of each dir in dirTree, separated by space?
                   #cat("\n  dir$path pasted with fileSep: _", paste( unlist( dir()$path ), collapse = .Platform$file.sep ), "_" )
                   # list of each dir in dirTree created into a path
                   #cat("\n  dir$path[-1]: _", unlist( dir()$path[-1] ), "_" ) # list of each dir in dirTree, separated by space?
                   #cat("\n  dir$path[-1] pasted with fileSep: _", paste( unlist( dir()$path[-1] ), collapse = .Platform$file.sep ), "_" )
                   # list of each dir in dirTree created into a path
    #               global$datapath <- file.path( # form path with
    #                 roots[[dir()$root]], # shinyDirChoose selected ROOT (selected by its NAME found in dir()$root)
    #                 paste( unlist( dir()$path[-1] ), collapse = .Platform$file.sep )  ) # shinyDirChoose selected PATH with file.sep added
    #             })


    #### render the contentsTable ####

    #  make table reactive
    gt <- reactiveValues(table = contentsTable, paths = CONTENT_NOTE_PATH,
                         lines = CONTENT_LINE_NUM, contents = contents)

    #cat("\n  compute contents table\n")
    # compute contents table
    #observe({
    #  if( global$datapath != "" ) { # only if datapath has been assigned!

        #contents <- find_contents_in_dir_tree(global$datapath, orgPath, settings)
        # not needed now as already retrieve all possible content to insert at start!
        # TODO modify this code to allow FILTERING of results via datapath assignment

    #    CONTENT_NAME <- as.character(lapply(X = contents, FUN = `[[`, "contentTitle"))
    #    pathLine <- strsplit(names(contents), ':::', fixed=TRUE)
    #    CONTENT_LINE_NUM <- c()
    #    CONTENT_NOTE_PATH <- c()
    #    CONTENT_NOTE_NAME <- c()
    #    for(pl in 1:length(pathLine) ) {
    #      plc <- pathLine[[pl]]
    #      CONTENT_LINE_NUM[pl] <- as.character(plc[2]) #second index
    #      CONTENT_NOTE_PATH[pl] <- as.character(plc[1]) #first index
    #      CONTENT_NOTE_NAME[pl] <- basename( as.character(plc[1])) #first index
    #    }
    #    contentsTable <- tibble::tibble(CONTENT_NAME, CONTENT_LINE_NUM, CONTENT_NOTE_NAME)

        #gt$table <- tibble::tibble(CONTENT_NAME, CONTENT_LINE_NUM, CONTENT_NOTE_NAME)
    #    gt$table <- contentsTable
    #    gt$paths = CONTENT_NOTE_PATH
    #    gt$lines = CONTENT_LINE_NUM
    #    gt$contents <- contents
    #  }
    #})


    cat("\n  render the contentsTable\n")

    output$mytable1 <- DT::renderDT( gt$table,
                                     selection = 'single',
                                     class = 'cell-border',
                                     #editable = list(target = "cell", disable = list(columns = c(1:7))),
                                     #editable = TRUE,
                                     filter = 'top',
                                     caption = 'Contents Table',
                                     fillContainer = TRUE,
                                     #options = list(
                                    #   lengthMenu = c(20, 50, 100),
                                    #   pageLength = 20,
                                    #   columnDefs = list(list(className = 'dt-center', targets = c(1:8))) ),
                                     server = FALSE ) # processing on client-side means edits to IMPORT col are kept when searching the table


    #### set Content description ####

    # set the contentDescription text based on content selection
    output$contentDescription <- renderUI({
      if( is.null(input$mytable1_rows_selected) ) {
        paste0("  no content selected")
      } else {
        #paste0("row: ", input$mytable1_rows_selected, " line: ",
        #       CONTENT_LINE_NUM[[input$mytable1_rows_selected]],
        #       " path: ", CONTENT_NOTE_PATH[[input$mytable1_rows_selected]])
        cnts <- get_content_declaration_contents(read_file(gt$paths[[input$mytable1_rows_selected]]),
                                                 as.numeric(gt$lines[[input$mytable1_rows_selected]]),
                                                 settings, orgPath)
        desc <- get_content_description(cnts, settings, orgPath)

        HTML(paste0(desc[desc!=""], sep='<br/>') )

      }
    })


    #### done : insert content ####

    observeEvent(input$done, {

      cat( paste0("  inserting content into: ", basename(projNoteRmdPath), " at line: ", noteInsertionIndex, "\n") )

      projectmanagr::insert_content(

        selectionSource = user_selection(gt$paths[[input$mytable1_rows_selected]],
                       as.numeric(gt$lines[[input$mytable1_rows_selected]]) ),

        selectionDestination = user_selection(projNoteRmdPath, noteInsertionIndex)
      )

      # Close Gadget after computations are complete:
      stopApp()

    })


  }

  #### view gadget ####

  if(orgPath == "") { # use default width/height
    viewer <- dialogViewer("Insert Hyperlink", width = 1000,
                           height = 800 )
  } else { # use width/height from settings
    viewer <- dialogViewer("Insert Hyperlink",
                           width = settings[["GadgetWidth"]],
                           height = settings[["GadgetHeight"]])
  }

  runGadget(ui, server, viewer = viewer)

} #### ________________________________ ####


#' Insert Section Sep 1 Addin
#'
#' @export
addin_insert_section_sep_1 <- function() {

  addin_insert_section_sep(1)
}

#' Insert Section Sep 2 Addin
#'
#' @export
addin_insert_section_sep_2 <- function() {

  addin_insert_section_sep(2)
}

#' Insert Section Sep 3 Addin
#'
#' @export
addin_insert_section_sep_3 <- function() {

  addin_insert_section_sep(3)
}

#' Insert Section Sep 4 Addin
#'
#' @export
addin_insert_section_sep_4 <- function() {

  addin_insert_section_sep(4)
}

#' Insert Section Sep 5 Addin
#'
#' @export
addin_insert_section_sep_5 <- function() {

  addin_insert_section_sep(5)
}

#' Insert Section Sep 6 Addin
#'
#' @export
addin_insert_section_sep_6 <- function() {

  addin_insert_section_sep(6)
}

#' Insert Section Sep Addin
#'
#' Load section `sep<index>`` from templates DIR and insert into current
#' cursor position.
#'
addin_insert_section_sep <- function(index) {

  #cat( "\nprojectmanagr::addin_insert_section_sep_1():\n" )

  # get currently active doc in rstudio
  context <- rstudioapi::getSourceEditorContext()

  # get orgPath
  orgPath <- find_org_directory(context$path)

  if(orgPath == "" ) { # only if orgPath not identified
    stop( paste0("  Cannot identify organisation directory: ", context$path) )

  }

  # get config templates settings yml
  confPath <- get_config_dir(orgPath)
  tempPath <- get_template_dir(orgPath)
  settings <- get_settings_yml(orgPath)

  # get SEP file from tempPath
  sepFile <- list.files(tempPath)[ startsWith(list.files(tempPath), "SEP") &
                                   endsWith(list.files(tempPath), paste0(index, ".txt")) ]

  # read SEP value
  SEP <- read_file(paste0( tempPath, .Platform$file.sep, sepFile))


  #### insert SEP value in active doc ####

  rstudioapi::insertText(SEP)

} #### ________________________________ ####


