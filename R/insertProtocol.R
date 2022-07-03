#' Insert a Protocol into a Project Note
#'
#' This Function adds a Protocol to a Project Note - protocols are found in the
#'  PROGRAMMES' SOP/ Dir.
#'
#' Inserted protocols add each Procedure defined under the '# PROTOCOL' section
#'  of the document. Notes sections (FALSE by default) and the Equipment section
#' (TRUE by default) can be optionally included in the inserted protocol.
#'
#' All links in the Protocol are UPDATED to work from the destination Project
#'  Note.
#'
#' All graphics included in a knitr::include_graphics() r code chunk are
#'  transferred to the new Project Note DIR and linked appropriately.
#'
#' @param projectNotePath The ABSOLUTE path of the Project Note.
#'
#' @param projectNoteRow Index to insert the protocol into project notes
#'
#' @param protocolName The name of the Protocol Rmd file
#'
#' @param includeNotes Boolean to indicate whether all Notes bullets are included or not.
#'
#' @param includeEquip Boolean to indicate whether the initial equipment and material section should be included.
#'
#' @export
insertProtocol <- function( projectNotePath, projectNoteRow, protocolName,
                            includeNotes=FALSE, includeEquip=TRUE  ) {

  cat( "\nprojectmanagr::insertProtocol():\n" )

  # get protocol title from Name - replace _ with ' '
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

  # get projectNotePrefix form path
  projectNotePrefix <- substring( basename(projectNotePath), first=1, last=regexpr("~_", basename(projectNotePath), fixed=TRUE)-1 )

  # read projectNote file:
  projNoteFileConn <- file( projectNotePath )
  projNoteContents <- readLines( projNoteFileConn )
  close(projNoteFileConn)

  # get the progPath:
  progPath <- findProgDir(projectNotePath)

  # get SOP path:
  protocolsPath <- paste0(progPath, .Platform$file.sep, "SOP")

  # get protocol path:
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

  extractedProtocol <- c(DocTitleLink, "", "", "---", "", "", "") # begin with link to Protocol


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
        line <- protocolContents[i]

        # if it contains a link, need to UPDATE this to be a link from the new Rmd file to link destination:
        if( grepl("\\]\\(", line) == TRUE ) {

          # line contains at least one link
          num <- length(gregexpr("\\]\\(", line)[[1]]) # gives the NUMBER of links (number of '](' strings in line))

          completeLink <- substr(line, 1, gregexpr("\\]\\(", line)[[1]][1]+2)

          for( a in 1:num) {

            # extract the relative link address from line
            start <- gregexpr("\\]\\(", line)[[1]][a]
            lineLink <- substr(line, start+2, nchar(line))
            end <- regexpr(")", lineLink)
            lineLink <- substr(lineLink, 1, end-1)

            # modify relative link to point from new source:
            lineLink <- R.utils::getAbsolutePath(lineLink, workDirectory=dirname(protocolPath) )
            DocLink <- R.utils::getRelativePath( dirname(protocolPath), relativeTo=dirname(projectNotePath) )
            #DocLink <- substring(DocLink, first=4, last=nchar(DocLink)) # PATH IS CORRECT FOR DIRS!
            DocSplit <-strsplit(DocLink, .Platform$file.sep) # get the containing DIR to link lineLink and DocLink
            pir <- DocSplit[[1]][ length(DocSplit[[1]]) ]
            # DocLink is now
            DocLink <- paste0(DocLink, .Platform$file.sep, substr(lineLink, ( regexpr(pir, lineLink) + nchar(pir) )+2, nchar(lineLink) ) )

            if(a < num) {
            completeLink <- paste0(completeLink, DocLink, substr(line, start+end+1, gregexpr("\\]\\(", line)[[1]][a+1]+1 ) )
            }
            else {
              completeLink <- paste0(completeLink, DocLink, substr(line, start+end+1, nchar(line) ) )
            }

          }

          protocolContents[i] <- completeLink

        }

        # ALSO if line includes a graphic should UPDATE this to point to the graphic from the new Rmd file
        if( grepl("knitr::include_graphics\\(", line) == TRUE ) {

            # extract the relative link address from line
            start <- gregexpr("knitr::include_graphics\\(", line)[[1]][1]
            lineLink <- substr(line, start+25, nchar(line))
            end <- regexpr(")", lineLink)
            lineLink <- substr(lineLink, 1, end-2)

            # modify relative link to point from new source:
            lineLink <-  R.utils::getAbsolutePath(lineLink, workDirectory=dirname(protocolPath) )
            DocLink <- R.utils::getRelativePath(dirname(protocolPath), relativeTo=dirname(projectNotePath) )
            #DocLink <- substring(DocLink, first=4, last=nchar(DocLink)) # PATH IS CORRECT FOR DIRS!
            DocSplit <-strsplit(DocLink, .Platform$file.sep) # get the containing DIR to link lineLink and DocLink
            pir <- DocSplit[[1]][ length(DocSplit[[1]]) ]
            # DocLink is now
            DocLink <- paste0(DocLink, substr(lineLink, ( regexpr(pir, lineLink) + nchar(pir) ), nchar(lineLink) ) )

            # put new link into line
            protocolContents[i] <- paste0( substr(line, 1, start+24), DocLink, substr(line, end-1, nchar(line) ) )

        }

        if( grepl("_PREFIX_", line) == TRUE ) {
          protocolContents[i] <- gsub("_PREFIX_", projectNotePrefix, line)
        }

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
      line <- protocolContents[i]

      # if it contains a link, need to UPDATE this to be a link from the new Rmd file to link destination:
      if( grepl("\\]\\(", line) == TRUE ) {

        # line contains at least one link
        num <- length(gregexpr("\\]\\(", line)[[1]]) # gives the NUMBER of links (number of '](' strings in line))

        completeLink <- substr(line, 1, gregexpr("\\]\\(", line)[[1]][1]+2)

        for( a in 1:num) {

          # extract the relative link address from line
          start <- gregexpr("\\]\\(", line)[[1]][a]
          lineLink <- substr(line, start+2, nchar(line))
          end <- regexpr(")", lineLink)
          lineLink <- substr(lineLink, 1, end-1)

          # modify relative link to point from new source:
          lineLink <- R.utils::getAbsolutePath(lineLink, workDirectory=dirname(protocolPath) )
          DocLink <- R.utils::getRelativePath( dirname(protocolPath), relativeTo=dirname(projectNotePath) )
          #DocLink <- substring(DocLink, first=4, last=nchar(DocLink)) # PATH IS CORRECT FOR DIRS!
          DocSplit <-strsplit(DocLink, .Platform$file.sep) # get the containing DIR to link lineLink and DocLink
          pir <- DocSplit[[1]][ length(DocSplit[[1]]) ]
          # DocLink is now
          DocLink <- paste0(DocLink, .Platform$file.sep, substr(lineLink, ( regexpr(pir, lineLink) + nchar(pir) )+2, nchar(lineLink) ) )

          if(a < num) {
            completeLink <- paste0(completeLink, DocLink, substr(line, start+end+1, gregexpr("\\]\\(", line)[[1]][a+1]+1 ) )
          }
          else {
            completeLink <- paste0(completeLink, DocLink, substr(line, start+end+1, nchar(line) ) )
          }

        }

        protocolContents[i] <- completeLink

      }

      # ALSO if line includes a graphic should UPDATE this to point to the graphic from the new Rmd file
      if( grepl("knitr::include_graphics\\(", line) == TRUE ) {

        # extract the relative link address from line
        start <- gregexpr("knitr::include_graphics\\(", line)[[1]][1]
        lineLink <- substr(line, start+25, nchar(line))
        end <- regexpr(")", lineLink)
        lineLink <- substr(lineLink, 1, end-2)

        # modify relative link to point from new source:
        lineLink <-  R.utils::getAbsolutePath(lineLink, workDirectory=dirname(protocolPath) )
        DocLink <- R.utils::getRelativePath(dirname(protocolPath), relativeTo=dirname(projectNotePath) )
        #DocLink <- substring(DocLink, first=4, last=nchar(DocLink)) # PATH IS CORRECT FOR DIRS!
        DocSplit <-strsplit(DocLink, .Platform$file.sep) # get the containing DIR to link lineLink and DocLink
        pir <- DocSplit[[1]][ length(DocSplit[[1]]) ]
        # DocLink is now
        DocLink <- paste0(DocLink, substr(lineLink, ( regexpr(pir, lineLink) + nchar(pir) ), nchar(lineLink) ) )

        # put new link into line
        protocolContents[i] <- paste0( substr(line, 1, start+24), DocLink, substr(line, start+end+23, nchar(line) ) )

      }


      if( grepl("PREFIX", line) == TRUE ) {
        protocolContents[i] <- gsub("PREFIX", projectNotePrefix, line)
      }


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
