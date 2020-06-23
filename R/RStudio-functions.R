
#' Add a RELATIVE Hyperlink from an Open RStudio File to the currently Active RStudio
#' file, at the current cursor position.
#'
#'
#'
#'
#'@export
addinAddHyperLink <- function( ) {


   # first, get all open RStudio Doc PATHS:
  fileList <- getRStudioOpenDocIDs()

  # next, get the currently active document:
  context <- rstudioapi::getSourceEditorContext()

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

    gadgetTitleBar("Add HyperLink"),

    miniContentPanel(

      fillCol(

        fillRow( h5("Add a new Hyperlink:") ),

        fillRow(
          h3(  paste("TO:", contextName ), align="center"  )
        ),

        fillRow(
          h5(  paste("Line:", line ), align="center"  ),
          h5(  paste("Column:", column ), align="center"  )
        ),

        fillRow(
          selectInput("select", "Select Document:",
                      choices = numberedFileList,
                      selected = numberedFileList[1],
                      width="100%")
        )

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
      DocName <- gsub("-", " ",  gsub("_", " ", substring(DocName, first=1, last=regexpr("\\.[^\\.]*$", DocName)-1 ) )  )

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

  viewer <- dialogViewer("Create New Project Document", width = 800, height = 600)

  runGadget(ui, server, viewer = viewer)





}



#' Get RStudio Open Document IDs
#'
#' Returns an ordered list of all open RStudio Documents.  The list contains a set
#' of VECTORS that contain the RStudio Document ID [1] and the RStudio Document absolute
#' path [2].
#'
#' The order of the open RStudio documents in the list matches the order they are open
#' in RStudio.
#'
#' fileList[[i]][1] - RStudio i'th Document ID.
#'
#' fileList[[i]][2] - RStudio i'th Document Absolute Path, or null if the file is not saved to disk.
#'
#'@export
getRStudioOpenDocIDs <- function() {

  #cat( "\nprojectmanagr::getRstudioDocIDs():\n" )

  # first, check if ~/.rstudio-desktop/sources exists
  if( file.exists("~/.rstudio-desktop/sources") ) {

    # check if a DIR exists which STARTS WITH "s-" (remainder is unique RStudio session ID)
    if ( file.exists(  Sys.glob("~/.rstudio-desktop/sources/s-*")  )  ) {

      # get file list
      fileIDs <- list.files(  Sys.glob("~/.rstudio-desktop/sources/s-*")  )
      # look in the "s-*" DIR for every file that does NOT end in "-contents"
      fileIDs <- fileIDs[lapply(fileIDs,function(x) length(grep("-contents",x,value=FALSE))) == 0]
      # nor have name "lock_file"
      fileIDs <- fileIDs[lapply(fileIDs,function(x) length(grep("lock_file",x,value=FALSE))) == 0]

      # convert to full path:
      filePath <- paste0(Sys.glob("~/.rstudio-desktop/sources/s-*"), .Platform$file.sep, fileIDs)

      # inside remaining files, if saved to disk, will be a line starting 'path" : "[path_to_file]",'
      # if not saved to disk, will contain the line '"path" : null,'
      # Extract the value of path for each entry in filePath:
      fileList <- list()
      relVector <- vector()
      for (i in 1:length(filePath) ) {

        # put ID into fileList
        fileList[[i]] <- fileIDs[i]

        # open file
        FileConn <- file( filePath[i] )
        lines <- readLines( FileConn, warn = FALSE )
        close(FileConn)

        # extract path string:
        pathLine <- lines[grepl("    \"path\"*", lines)]

        if( grepl("\"path\" : null,", pathLine) ) {
          # path is null - file has not been saved - store "null" in fileList:
          fileList[[i]][2] <- "null"
        }
        else {

          # copy path into fileList - second index
          fileList[[i]][2] <- substring(pathLine, 15, nchar(pathLine)-2)
        }

        # extract relative order string:
        relOrderLine <- lines[grepl("    \"relative_order\" :*", lines)]
        # copy order into relVector:
        relVector[i] <- as.integer(substring(relOrderLine, 24, nchar(relOrderLine)-1))


      }

    }
    else {

      #cat("  ~/.rstudio-desktop does not exist\n")
      stop( paste0("No Active RStudio Session") )

    }
  }
  else {

    #cat("  ~/.rstudio-desktop does not exist\n")
    stop( paste0("No Active RStudio Session") )

  }

  # finally, re-order list based on relVector:
  fileList <- fileList[ order( relVector ) ]

  # return fileList:
  fileList


}



#' Set WD to RStudio Active Doc parent Directory
#'
#'
#'@export
setwdActiveDoc <- function( ) {

  context <- rstudioapi::getSourceEditorContext() # use this to always get the active document in the source editor

  cat( "\nprojectmanagr::setwdActiveDoc():\n" )

  setwd( dirname(context$path) )

  cat( "  Set work dir: ", dirname(context$path), "\n" )

}





