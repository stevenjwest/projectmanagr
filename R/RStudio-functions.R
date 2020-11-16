
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





