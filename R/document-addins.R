

#' Set WD to RStudio Active Doc parent Directory
#'
#'
#'@export
set_wd_active_doc <- function( ) {

  if( rstudioapi::isAvailable() ) {

    save_context_doc()
    path <- get_context_path()
    dirPath <- dirname(path)

    cat( "\nprojectmanagr::set_wd_active_doc():\n" )

    # navigate to containing dir
    rstudioapi::filesPaneNavigate( dirPath )
    # and set working directory
    setwd( dirPath )

    cat( "  Set work dir: \n    ", dirPath, "\n" )

  } else {
    cat("\nRStudio context is unavailable")
  }
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
#' This function looks for a Markdown link on the currently selected line in RStudio
#' (or subsequent lines if the link is split) of the form `[...](...)`. Once the path
#' is identified, if there's a `#header` portion, the function opens the file, searches
#' for the matching header, and navigates there in RStudio. If it doesn't find such a
#' header, it raises an informative error.
#'
#' To keep this function short and readable, much of the logic is moved to internal
#' helper functions:
#'
#' \itemize{
#'   \item \code{parse_link_in_selected_line()} extracts the path (and anchor, if any)
#'         from the user’s selection, handling multi-line links.
#'   \item \code{navigate_file_with_anchor()} opens a file and navigates to the
#'         specified anchor.
#'   \item \code{navigate_file_no_anchor()} opens a file if one exists, or checks
#'         for an internal heading in the current file.
#' }
#'
#' @export
navigate_markdown_link <- function() {

  WD <- getwd()
  path <- get_context_path()
  id <- get_context_id()
  line <- get_context_row()
  contents <- get_context_contents()

  parsed <- parse_link_in_selected_line(contents, line)

  # check for anchor pointer in parsed$relPath
  headerPointer <- regexpr("#", parsed$relPath, fixed = TRUE)
  if (headerPointer > 0) {
    # there's an anchor portion
    relPathTrim <- substring(parsed$relPath, 1, (headerPointer - 1))
    anchorRaw   <- substring(parsed$relPath, (headerPointer + 1))
    navigate_file_with_anchor(relPathTrim, anchorRaw, path)
  } else {
    # no anchor portion
    navigate_file_no_anchor(parsed$relPath, path, contents)
  }
}


#' @keywords internal
parse_link_in_selected_line <- function(contents, line) {
  # Extracts the link path portion from the currently selected line or lines
  # if the link is split across multiple lines. Returns a list with 'relPath'.

  lineContent <- contents[line]
  linkMiddle <- regexpr("](", lineContent, fixed = TRUE)

  if (linkMiddle > -1) {
    linkStart <- regexpr("\\[[^\\[]*$", substr(lineContent, 1, linkMiddle))
    linkEnd <- (regexpr("\\)", substr(lineContent, linkMiddle, nchar(lineContent))) +
                  linkMiddle - 1)
  } else {
    stop(paste0("Selected line does not contain a link: ", lineContent))
  }

  if (linkStart > linkMiddle) {
    stop(paste0("Selected line does not contain a valid '[...](' link: ", lineContent))
  }

  # if we didn't find the closing parenthesis, see if link extends to subsequent lines
  if (linkEnd == -1) {
    line2 <- line
    rP <- ""
    while (TRUE) {
      line2 <- line2 + 1
      if (line2 > length(contents)) {
        stop("Could not find closing parenthesis ')' for the link.")
      }
      lC <- contents[line2]
      lE <- regexpr(")", lC, fixed = TRUE)
      if (lE > -1) {
        # once we find the ')', we tack on everything up to (lE - 1)
        rP <- paste0(rP, trimws(substr(lC, 1, lE - 1)))
        break
      } else {
        # everything in this line is part of the path
        rP <- paste0(rP, trimws(lC))
      }
    }
    relPath <- fs::path(
      paste0(substr(lineContent, (linkMiddle + 2), nchar(lineContent)), rP)
    )
  } else {
    relPath <- fs::path(substr(lineContent, (linkMiddle + 2), (linkEnd - 1)))
  }

  list(relPath = relPath)
}


#' @keywords internal
navigate_file_with_anchor <- function(relPath, anchorRaw, currentDocPath) {
  # Attempt to open the file at relPath, then navigate to the anchor anchorRaw
  # If file not found, we error. If anchor not found, we error.

  anchor <- sanitize_header_for_anchor(anchorRaw)
  absPath <- fs::path_abs(relPath, start = fs::path_dir(currentDocPath))

  if (!fs::file_exists(absPath)) {
    stop(paste0("Link points to non-existent file: ", absPath))
  }

  # open the file in RStudio
  contents_orig <- read_file(absPath)
  id <- rstudioapi::documentOpen(absPath)

  # place cursor at end first (some RStudio quirks)
  rstudioapi::setCursorPosition(rstudioapi::document_position(length(contents_orig), 1), id)
  contents <- contents_orig

  # find all header lines, then see if any matches this anchor
  contentHeaders <- get_file_contents_headers(contents)
  matchLine <- find_anchor_in_headers(anchor, anchorRaw, contentHeaders, contents_orig)

  # navigate
  Sys.sleep(0.1)
  rstudioapi::setCursorPosition(
    rstudioapi::document_position(max(matchLine - 4, 0), 1),
    id
  )
  Sys.sleep(0.1)
  rstudioapi::setCursorPosition(
    rstudioapi::document_position(matchLine, 1),
    id
  )

  # navigate to containing dir
  rstudioapi::filesPaneNavigate(dirname(absPath))
  setwd(dirname(absPath))
}


#' @keywords internal
navigate_file_no_anchor <- function(relPath, currentDocPath, contents_current_doc) {
  # If there's no anchor, either open the file or attempt to navigate to an
  # internal heading in the current doc.

  absPath <- fs::path_abs(relPath, start = fs::path_dir(currentDocPath))
  if (file.exists(absPath)) {
    # navigate to the file
    id <- rstudioapi::navigateToFile(absPath)
    rstudioapi::filesPaneNavigate(dirname(absPath))
    setwd(dirname(absPath))
  } else {
    # possibly an internal link within the current doc
    anchorRaw <- relPath
    anchor <- sanitize_header_for_anchor(anchorRaw)

    contentHeaders <- get_file_contents_headers(contents_current_doc)
    matchLine <- find_anchor_in_headers(
      anchor,
      anchorRaw,
      contentHeaders,
      contents_current_doc
    )

    # navigate within current doc
    id <- get_context_id()
    # go to end of file first
    rstudioapi::setCursorPosition(
      rstudioapi::document_position(length(contents_current_doc), 1),
      id
    )
    Sys.sleep(0.1)
    rstudioapi::setCursorPosition(
      rstudioapi::document_position(max(matchLine - 4, 0), 1),
      id
    )
    Sys.sleep(0.1)
    rstudioapi::setCursorPosition(
      rstudioapi::document_position(matchLine, 1),
      id
    )

    # navigate to containing dir of current doc
    rstudioapi::filesPaneNavigate(dirname(currentDocPath))
    setwd(dirname(currentDocPath))
  }
}


#' @keywords internal
find_anchor_in_headers <- function(anchor, anchorRaw, contentHeaders, contents_orig) {
  # Given a 'clean' anchor, loops over all found headers in 'contentHeaders',
  # tries to match them by creating a 'clean' anchor from the header line,
  # and returns the line index in 'contents_orig'. If not found, throws an error.

  for (hdr in contentHeaders) {
    # e.g. hdr might be "### Test-Hyphen Hyperlink!"
    # remove leading #, spaces
    hdrText <- sub("^#+\\s*", "", hdr)
    hdrAnchor <- sanitize_header_for_anchor(hdrText)
    if (hdrAnchor == anchor) {
      navLine <- grep(hdr, contents_orig, fixed = TRUE)
      if (length(navLine) && !is.na(navLine[1])) {
        return(navLine[1])
      }
    }
  }

  stop(
    "No matching header found for anchor: #", anchorRaw
  )
}

