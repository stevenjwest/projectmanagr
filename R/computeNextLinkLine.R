#'
#' Returns the next line in contents, from line, that contains a Rmd link (the next
#' line that contains the string "](" ), else if no link is found, returns a BLANK
#' STRING.
#'
#'
computeNextLinkLine <- function(line, contents) {

  returnVal <- ""

  # look through projDocContents FROM line, and identify the line that begins ### or ---
  for( l in (line+1):length(contents) ) {

    if( regexpr("](", contents[l], fixed=TRUE) > 0 ) {
      returnVal <- contents[l]
      break
    }

  }

  returnVal

}
