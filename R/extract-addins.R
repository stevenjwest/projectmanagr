

addin_insert_protocol <- function() {

}


addin_insert_section_sep <- function() {

}


#' Insert a Hyperlink
#'
#' Insert a RELATIVE Hyperlink from an Open RStudio File to the currently
#' Active RStudio file, at the current cursor position.
#'
#'@export
addin_insert_hyperlink <- function( ) {

  cat( "\nprojectmanagr::addin_insert_hyperlink():\n" )


  # FIRST, get the currently active document:
  context <- rstudioapi::getSourceEditorContext()

  # get all open RStudio Doc PATHS:
  fileList <- get_rstudio_open_doc_IDs()

  # get contents:
  openDocContents <- context$contents

  cursor <- rstudioapi::primary_selection(context)

  line <- (cursor$range[[1]])[1]
  column <- (cursor$range[[1]])[2]

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

    }
    else if (fileList[[i]][2] != context$path) {
      firstFileList[[i]] <- fileList[[i]][2]

      numberedFileList[i] <- i # BEFORE skipping Active Doc, so set list[i] to i
    }
    else if(fileList[[i]][2] == context$path) {

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


  # present these files in a combobox in shiny gadget:


  # UI:

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


  ### ENCODE BEHAVIOUR ###

  server <- function(input, output, session) {


    # perform computations to form new hyperlink
    observeEvent(input$done, {

      print(input$select)

      print(reorderedFileList[[as.integer(input$select)]])

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


  ### VIEW GADGET ###
  # get config/status.yml to read gadgetWidth/Height preferences
  orgPath <- find_org_directory(context$path)
  if(orgPath == "") {
    viewer <- dialogViewer("Insert Hyperlink", width = 1000,
                           height = 800 )
  } else {
    confPath <- paste0( orgPath, .Platform$file.sep, "config" )
    settingsFile = paste( confPath, .Platform$file.sep, "settings.yml", sep="" )
    settingsContents <- yaml::yaml.load( yaml::read_yaml( settingsFile ) )

    viewer <- dialogViewer("Insert Hyperlink", width = settingsContents$gadgetWidth,
                           height = settingsContents$gadgetHeight )
  }

  runGadget(ui, server, viewer = viewer)

}


addin_insert_datetime <- function() {

}

