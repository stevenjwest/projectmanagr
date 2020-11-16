#' Insert a Protocol into a Project Note
#'
#' This Function adds a new Protocol to a Project Note - saved in the PROGRAMMES' SOP/ Dir.
#' The Protocol is formed using a Rmd template, and uses the tufte::tufte_handout format to
#' generate a PDF of the protocol.
#'
#' Protocols are stored in the SOP/ directory inside the PROGRAMME Directory.  Each Protocol
#' exists in its own directory, to keep its compiled files together.
#'
#' The Protocol source Rmd will link to its creating Project Note, and the Project Note will link to the
#' compiled PDF of the Protocol.
#'
#' @param projectNotePath The ABSOLUTE path of the Project Note.
#' @param projectNoteRow Index to insert the protocol into project notes
#' @param protocolName The name of the Protocol Rmd file
#' @param includeNotes Boolean to indicate whether all Notes bullets are included or not.
#' @param includeEquip Boolean to indicate whether the initial equipment and material section should be included.
#'
#' @export
insertProtocol <- function( projectNotePath, projectNoteRow, protocolName,
                            includeNotes=FALSE, includeEquip=TRUE  ) {

  cat( "\nprojectmanagr::insertProtocol():\n" )


  protocolTitle <- gsub("-", " ", gsub("_", " ", protocolName) )


  # Check projectNotePath is a sub-dir in a Programme DIR, which is a sub-dir to the root of an ORGANISATION:
  # run dirname TWICE as want to ensure projectNotePath is a sub-dir in a Programme!
  orgPath <- dirname( dirname(projectNotePath) )

  orgPath <- findOrgDir(orgPath)

  if(orgPath == "" ) {
    # the search reached the root of the filesystem without finding the Organisation files,
    # therefore, projectNotePath is not inside a PROGRAMME sub-dir!
    stop( paste0("  projectNotePath is not in a sub-dir of a PROGRAMME Directory: ", projectNotePath) )
  }
  # now, orgPath should be the root dir of the organisation

  # set confPath + tempPath:
  confPath <- paste0( orgPath, .Platform$file.sep, "config" )
  tempPath <- paste0( confPath, .Platform$file.sep, "templates" )

  # normalize path - remove HOME REF ~
  projectNotePath <- normalizePath(projectNotePath)


  # get the progPath:
  progPath <- findProgDir(projectNotePath)

  # get SOP path:
  protocolsPath <- paste0(progPath, .Platform$file.sep, "SOP")



  # read projNote file:
  projNoteFileConn <- file( projectNotePath )
  projNoteContents <- readLines( projNoteFileConn )
  close(projNoteFileConn)


  protocolPath <- paste0( protocolsPath, .Platform$file.sep, protocolName,
                          .Platform$file.sep, paste0(protocolName, ".Rmd") )
  # read Protocol file:
  protocolFileConn <- file( protocolPath )
  protocolContents <- readLines( protocolFileConn )
  close(protocolFileConn)


  # Insert the PROTOCOL section from Protocol File, into ProjNote File

  # create link to protocol from projectNotePath
  DocLink <- R.utils::getRelativePath(protocolPath, relativeTo=projectNotePath)
  DocLink <- substring(DocLink, first=4, last=nchar(DocLink))

  DocTitleLink <- paste( "[", protocolTitle, "](", DocLink, ")", sep="" )

  extractedProtocol <- c(DocTitleLink, "", "", "") # begin with link to Protocol

  if( includeEquip == TRUE ) {
    # first insert equipment and material if needed:
    insertLine = TRUE
    protocolStartIndex <- matchLineIndex("# EQUIPMENT & MATERIAL", protocolContents)

    # get first line with characters AFTER HEADER
    protocolStartIndex <- computeNextLineIndex((protocolStartIndex+1), protocolContents )-1

    # End at the PROTOCOL HEADER
    protocolEndIndex <- matchLineIndex("# PROTOCOL", protocolContents)

    extractedProtocol <- c(extractedProtocol, "## Equipment & Material", "", "", "")

    for(i in protocolStartIndex:(protocolEndIndex-1) ) {

      if( startsWith( tolower( protocolContents[i] ), "* notes" )  ) { # if its a Notes header, set insertLine to FALSE
        insertLine = FALSE
      }

      if( insertLine == FALSE &&
          !startsWith( tolower(protocolContents[i]), "* notes" ) &&
          startsWith( protocolContents[i], "*" ) ||
          startsWith( protocolContents[i], "#") ||
          startsWith( protocolContents[i], "-" )  ) { # if a line that BREAKS a notes section
        insertLine = TRUE # set insertLine to TRUE
      }


      if(insertLine == FALSE && includeNotes == FALSE ) {
        # traversing NOTES lines, do not insert lines if not including notes!

      } else {
        # insert lines:
        extractedProtocol <- c(extractedProtocol, protocolContents[i])

      }
    }
  }

  # now insert the PROTOCOL
  protocolStartIndex <- matchLineIndex("# PROTOCOL", protocolContents)

  # get the NEXT LINE that starts with # - the FIRST HEADER of the actual Protocol
  protocolStartIndex <- grepLineIndexFrom("#", protocolContents, (protocolStartIndex+1) )

  # defined as the line starting: # NEXT STEPS
  protocolEndIndex <- matchLineIndex("# NEXT STEPS", protocolContents)

  insertLine = TRUE

  for(i in protocolStartIndex:(protocolEndIndex-1) ) {

    if( startsWith( tolower( protocolContents[i] ), "* notes" )  ) { # if its a Notes header, set insertLine to FALSE
      insertLine = FALSE
    }

    if( insertLine == FALSE &&
        !startsWith( tolower(protocolContents[i]), "* notes" ) &&
        startsWith( protocolContents[i], "*" ) ||
        startsWith( protocolContents[i], "#") ||
        startsWith( protocolContents[i], "-" )  ) { # if a line that BREAKS a notes section
      insertLine = TRUE # set insertLine to TRUE
    }


    if(insertLine == FALSE && includeNotes == FALSE ) {
      # traversing NOTES lines, do not insert lines if not including notes!

    } else {
      # insert lines:
      extractedProtocol <- c(extractedProtocol, protocolContents[i])

    }

  }

  # insert extracted protocol into projNoteContents
  projNoteContents <- c( projNoteContents[1:(projectNoteRow-1)],
                         extractedProtocol,
                         projNoteContents[projectNoteRow:length(projNoteContents)] )



  # write to projNotePath
  projNoteFileConn <- file(projectNotePath)
  writeLines(projNoteContents, projNoteFileConn)
  close(projNoteFileConn)


  cat( "  Written Protocol to Project Note file: ", protocolName, "\n" )

}
