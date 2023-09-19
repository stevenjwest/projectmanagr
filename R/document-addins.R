

#' Set WD to RStudio Active Doc parent Directory
#'
#'
#'@export
set_wd_active_doc <- function( ) {

  rstudioapi::documentSave() # save current file first
  context <- rstudioapi::getSourceEditorContext() # use this to always get the active document in the source editor
  dirPath <- dirname(context$path)

  cat( "\nprojectmanagr::set_wd_active_doc():\n" )

  # navigate to containing dir
  rstudioapi::filesPaneNavigate( dirPath )
  # and set working directory
  setwd( dirPath )


  cat( "  Set work dir: ", dirPath, "\n" )

}



#' Set Selection to Next Horizontal Rule
#'
#' Set Selection from current line to next line with same number of dashes as
#' current line.
#'
#'
#'@export
set_selection_next_horizontal_rule <- function( ) {

  cat( "\nprojectmanagr::setSelectionNextHorizontalRule():\n" )

  context <- rstudioapi::getSourceEditorContext() # use this to always get the active document in the source editor

  cursor <- rstudioapi::primary_selection(context)
  line <- (cursor$range[[1]])[1] # get the line number of cursor

  if( startsWith(context$contents[line], "---") == TRUE ) {
    # can only make selection if current line is horizontal rule!

    for(l in (line+2):length(context$contents) ) {
      # find the next line that starts with the same number or more dashes as current line ---
      # SKIP line itself AND the next line - as use two lines of dashes for them to show up in RStudio Outline for whitespace
      if( startsWith(context$contents[l], context$contents[line]) == TRUE ) {
        endLine <- l
        break # out of for loop
      }
    }
    # set selection
    ranges <- lapply(seq(line, by = 1, length.out = (endLine-line) ), function(start) {
      rstudioapi::document_range(
        c(start, 0),
        c(start, Inf)
      )
    })
    ranges <- rstudioapi::document_range( c(line, 0), c( (endLine), 0) )

    cat( "  Set selection from: ", line, " to: ", endLine, "\n" )
    rstudioapi::setSelectionRanges(ranges, id=context$id)

  } else {
    cat( "  No selection can be made - current line does not contain a horizontal rule: ---\n" )
  }

}


#' Navigate link to markdown file and header
#'
#' @export
navigate_markdown_link <- function() {

  WD <- getwd()
  # get currently active doc in rstudio
  context <- rstudioapi::getSourceEditorContext()
  path <- normalizePath(context$path)

  cursor <- rstudioapi::primary_selection(context)
  line <- (cursor$range[[1]])[1] # get the line number of cursor
  col <- (cursor$range[[1]])[2] # get the col number of cursor position of cursor on line

  lineContent <- context$contents[line]

  # check if lineContent contains a link
  linkStart <- regexpr("[", lineContent, fixed=TRUE)
  linkMiddle <- regexpr("](", lineContent, fixed=TRUE)
  linkEnd <- regexpr(")", lineContent, fixed=TRUE)

  if( linkStart>linkMiddle | linkMiddle>linkEnd) {
    stop( paste0("  Selected line does not contain a link: ", lineContent))
  }


  # extract relative path
  relPath <-  substr(lineContent, (linkMiddle+2), (linkEnd-1))

  # check if link includes pointer to header in markdown doc
  headerPointer <- regexpr("#", relPath, fixed=TRUE)

  if( headerPointer > 0 ) { # header pointer exists

    relPathTrim <- substring(relPath, 1, (headerPointer-1))
    # compute absolute path
    setwd(dirname(path)) # set working dir to dir of current file
    absPath <- R.utils::getAbsolutePath(relPathTrim)

    # open doc as vector and identify line where header exists
    contents_orig <- read_file(absPath)
    id <- rstudioapi::documentOpen(absPath)
    rstudioapi::setCursorPosition(rstudioapi::document_position( length(contents_orig), 1), id)
    contents <- contents_orig

    header <- substring(relPath, (headerPointer+1))
    headerWords <- strsplit(header, '-')[[1]]

    # trim contents to remove all code sections - lines starting with ```
    contentCodeIndices <- which(startsWith(contents, '```'))
    if( length(contentCodeIndices) > 1 ) {
      for(i in (floor((length(contentCodeIndices)-1)/2)):0 ) {
        i1 <- (i*2+1)
        i2 <- (i*2+2)
        contents <- c(contents[1:contentCodeIndices[i1]], contents[contentCodeIndices[i2]:length(contents)])
      }
    }

    # trim contents to all header lines - that start with #
    contentHeaders <- contents[startsWith(contents, '#')]

    # grep for each word
    wordIndices <- list()
    i <- 1
    for( w in headerWords) {
      wordIndices[[i]] <- grep(w, contentHeaders, ignore.case=TRUE)
      i <- i +1
    }

    headerTitle <- contentHeaders[Reduce(intersect, wordIndices)[1]]

    # finally grep the known header in contents_orig
    navLine <- grep(headerTitle, contents_orig)[1]

    # navigate to line in file
    Sys.sleep(0.1) # ensure first position is set
    # go 4 above - to ensure the navLine appears 4 lines BELOW top of doc
    rstudioapi::setCursorPosition(rstudioapi::document_position(max(navLine-4, 0), 1), id)

    Sys.sleep(0.1) # ensure first position is set
    # then go to navLine - so this line is selected!
    rstudioapi::setCursorPosition(rstudioapi::document_position(max(navLine, 0), 1), id)

    # navigate to containing dir
    rstudioapi::filesPaneNavigate( dirname(absPath) )

    # and set working directory
    setwd( dirname(absPath) )


  } else { # no header pointer - just navigate to the file

    # compute absolute path
    setwd(dirname(path)) # set working dir to dir of current file
    absPath <- R.utils::getAbsolutePath(relPath)

    if( file.exists(absPath) ) {
      # navigate to the file
      id <- rstudioapi::navigateToFile(absPath)

      # navigate to containing dir
      rstudioapi::filesPaneNavigate( dirname(absPath) )

      # and set working directory
      setwd( dirname(absPath) )

    } else {
      # the link may point to an INTERNAL section header in current doc!

      # open doc as vector and identify line where header exists
      contents_orig <- context$contents
      id <- context$id
      contents <- contents_orig

      header <- relPath
      headerWords <- strsplit(header, '-')[[1]]

      # trim contents to remove all code sections - lines starting with ```
      contentCodeIndices <- which(startsWith(contents, '```'))
      if( length(contentCodeIndices) > 1 ) {
        for(i in (floor((length(contentCodeIndices)-1)/2)):0 ) {
          i1 <- (i*2+1)
          i2 <- (i*2+2)
          contents <- c(contents[1:contentCodeIndices[i1]], contents[contentCodeIndices[i2]:length(contents)])
        }
      }

      # trim contents to all header lines - that start with #
      contentHeaders <- contents[startsWith(contents, '#')]

      # grep for each word
      wordIndices <- list()
      i <- 1
      for( w in headerWords) {
        wordIndices[[i]] <- grep(w, contentHeaders, ignore.case=TRUE)
        i <- i +1
      }

      headerTitle <- contentHeaders[Reduce(intersect, wordIndices)[1]]

      # finally grep the known header in contents_orig
      navLine <- grep(headerTitle, contents_orig)[1]


      if( is.na(navLine) ) { # no header exists that contains headerWords

        stop( paste0("  File linked to does not exist: ", lineContent))

      } else {

        # navigate to line in file
        # go to END of file first
        rstudioapi::setCursorPosition(rstudioapi::document_position( length(contents_orig), 1), id)

        Sys.sleep(0.1) # ensure first position is set
        # go 4 above - to ensure the navLine appears 4 lines BELOW top of doc
        rstudioapi::setCursorPosition(rstudioapi::document_position(max(navLine-4, 0), 1), id)

        Sys.sleep(0.1) # ensure first position is set
        # then go to navLine - so this line is selected!
        rstudioapi::setCursorPosition(rstudioapi::document_position(max(navLine, 0), 1), id)

        # navigate to containing dir of current doc
        rstudioapi::filesPaneNavigate( dirname(context$path) )

        # and set working directory
        setwd( dirname(context$path) )
      }

    }
  }
}




