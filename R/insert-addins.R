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
    timezone <- "UTC"
    split <- "/"
    cat( "\n  No projectmanagr Organisation identified - using UTC timezone & / split by default.\n" )

  } else {

    # set confPath + tempPath - these names are FIXED:
    confPath <- paste0( orgPath, .Platform$file.sep, "config" )
    tempPath <- paste0( confPath, .Platform$file.sep, "templates" )

    # load settings file for user defined settings
    settingsFile <- paste( confPath, .Platform$file.sep, "settings.yml", sep="" )
    settings <- yaml::yaml.load( yaml::read_yaml( settingsFile ) )

    timezone <- settings[['DateTimeZone']]
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

}


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
    timezone <- "UTC"
    split <- "/"
    splitTime <- ":"
    cat( "\n  No projectmanagr Organisation identified - using UTC timezone by default.\n" )

  } else {

    # set confPath + tempPath - these names are FIXED:
    confPath <- paste0( orgPath, .Platform$file.sep, "config" )
    tempPath <- paste0( confPath, .Platform$file.sep, "templates" )

    # load settings file for user defined settings
    settingsFile <- paste( confPath, .Platform$file.sep, "settings.yml", sep="" )
    settings <- yaml::yaml.load( yaml::read_yaml( settingsFile ) )

    timezone <- settings[['DateTimeZone']]
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

}



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
}


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
}


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
}


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
  if(orgPath=="") { # do nothing

  } else { # get the settings file
    # set confPath + tempPath - these names are FIXED:
    confPath <- paste0( orgPath, .Platform$file.sep, "config" )
    tempPath <- paste0( confPath, .Platform$file.sep, "templates" )

    # load settings file for user defined settings
    settingsFile <- paste0( confPath, .Platform$file.sep, "settings.yml")
    settings <- yaml::yaml.load( yaml::read_yaml( settingsFile ) )
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


      #### insert hyperlink ####

      # form new hyperlink:
      DocLink <- R.utils::getRelativePath(reorderedFileList[[as.integer(input$select)]], relativeTo=context$path)
      DocLink <- substring(DocLink, first=4, last=nchar(DocLink))

      DocName <- basename(reorderedFileList[[as.integer(input$select)]])
      DocName <- substring(DocName, first=1, last=regexpr("\\.[^\\.]*$", DocName)-1 )

      DocTitleLink <- paste( "[", DocName, "](", DocLink, ")", sep="" )

      # insert into contents and save:
      #openDocContents[line] <- paste0(
      #  substr(openDocContents[line], 1, column-1),
      #  DocTitleLink,
      #  substr(openDocContents[line], column, nchar(openDocContents[line]) ) )

      #fileConn <- file(context$path)
      #writeLines(openDocContents, fileConn)
      #close(fileConn)

      #rstudioapi::navigateToFile(context$path, line=line, column=column)

      #rstudioapi::setDocumentContents(openDocContents, id = context$id)
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

}


#' Select & Insert Protocol
#'
#' Using DT to display a datatable in the shiny gadget here.  See the tutorial
#' at https://rstudio.github.io/DT/shiny.html
#'
addin_insert_protocol <- function() {

  cat( "\nprojectmanagr::addin_insert_protocol():\n" )


  #### instance variables ####

  # Retrieve cursor_selection from current Active Doc in rstudio:
  selection <- cursor_selection() # Project Doc, Project Note - Simple, Header, SubNote

  projNoteRmdPath <- selection[["filePath"]] # presumed to be project note Rmd
  noteInsertionIndex <- selection[["originalLineNumber"]]

  # get orgPath
  orgPath <- find_org_directory(context$path)

  if(orgPath == "" ) { # only if orgPath not identified
    stop( paste0("  Cannot identify organisation directory: ", context$path) )

  }

  # set confPath + tempPath - these names are FIXED:
  confPath <- paste0( orgPath, .Platform$file.sep, "config" )
  tempPath <- paste0( confPath, .Platform$file.sep, "templates" )

  # load settings file for user defined settings
  settingsFile <- paste0(confPath, .Platform$file.sep, "settings.yml")
  settings <- yaml::yaml.load( yaml::read_yaml( settingsFile ) )

  # get the progPath:
  progPath <- find_prog_dir(projNoteRmdPath, settings)

  # get protocol directory path:
  protocolsPath <- paste0(progPath, .Platform$file.sep, settings[["ProgrammeProtocolsDir"]])

  # get list of protocols
  sops <- list.dirs(protocolsPath, recursive=FALSE)
  protocols <- sops[endsWith(sops, settings[["ProtocolNameSuffix"]])] # extract only Protocol directories

  # get data from current file - DESTINATION FILE
  context <- rstudioapi::getSourceEditorContext()
  row <- context$selection[[1]]$range$start[1]
  path <- normalizePath( context$path )

  # generate a blank table to hold protocol names in
  PROTOCOL_NAME <- basename(protocols)
  protocolsTable <- tibble::tibble(PROTOCOL_NAME)


  #### user interface ####

  ui <- miniPage(

    gadgetTitleBar("Insert Protocol"),

    miniContentPanel(

      fillCol( flex = c(1,20),

               fillRow( p("Insert a protocol into the current Project Note") ),

               fillRow ( DT::DTOutput("mytable1") )
      )
    )
  )


  #### server code ####

  server <- function(input, output, session) {

    #  make table reactive
    global <- reactiveValues(table = protocolsTable)

    output$mytable1 <- DT::renderDT( global$table,
                                     selection = 'single',
                                     class = 'cell-border',
                                     #editable = list(target = "cell", disable = list(columns = c(1:7))),
                                     editable = TRUE,
                                     filter = 'top',
                                     caption = 'Protocols Table',
                                     fillContainer = TRUE,
                                     #options = list(
                                    #   lengthMenu = c(20, 50, 100),
                                    #   pageLength = 20,
                                    #   columnDefs = list(list(className = 'dt-center', targets = c(1:8))) ),
                                     server = FALSE ) # processing on client-side means edits to IMPORT col are kept when searching the table


    # Insert the Protocol from selected SOP
    observeEvent(input$done, {

      # insertProtocol:
      if(input$includeNotes == "1" && input$includeEquip == "1") {
        # NO - includeNotes is FALSE & includeNotes is FALSE
        cat( "  projectmanagr::insertProtocol():  includeNotes=FALSE, includeEquip=FALSE \n" )
        projectmanagr::insertProtocol(projNotePath, row, input$select, includeNotes=FALSE, includeEquip=FALSE)

      } else if(input$includeNotes == "2" && input$includeEquip == "1") {
        # YES - includeNotes is TRUE BUT includeNotes is FALSE
        cat( "  projectmanagr::insertProtocol():  includeNotes=TRUE, includeEquip=FALSE \n" )
        projectmanagr::insertProtocol(projNotePath, row, input$select, includeNotes=TRUE, includeEquip=FALSE)

      } else if(input$includeNotes == "1" && input$includeEquip == "2") {
        # NO - includeNotes is FALSE BUT includeNotes is TRUE
        cat( "  projectmanagr::insertProtocol():  includeNotes=FALSE, includeEquip=TRUE \n" )
        projectmanagr::insertProtocol(projNotePath, row, input$select, includeNotes=FALSE, includeEquip=TRUE)

      } else if(input$includeNotes == "2" && input$includeEquip == "2") {
        # YES - includeNotes is TRUE & includeNotes is TRUE
        cat( "  projectmanagr::insertProtocol():  includeNotes=TRUE, includeEquip=TRUE \n" )
        projectmanagr::insertProtocol(projNotePath, row, input$select, includeNotes=TRUE, includeEquip=TRUE)

      }

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

}


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

  # set confPath + tempPath - these names are FIXED:
  confPath <- paste0( orgPath, .Platform$file.sep, "config" )
  tempPath <- paste0( confPath, .Platform$file.sep, "templates" )

  # load settings file for user defined settings
  settingsFile <- paste( confPath, .Platform$file.sep, "settings.yml", sep="" )
  settings <- yaml::yaml.load( yaml::read_yaml( settingsFile ) )

  # get SEP file from tempPath
  sepFile <- list.files(tempPath)[ startsWith(list.files(tempPath), "SEP") &
                                   endsWith(list.files(tempPath), paste0(index, ".txt")) ]

  # read SEP value
  SEP <- read_file(paste0( tempPath, .Platform$file.sep, sepFile))


  #### insert SEP value in active doc ####

  rstudioapi::insertText(SEP)

}


