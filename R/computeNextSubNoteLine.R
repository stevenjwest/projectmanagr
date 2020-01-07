#'
#' Returns the first blank line after the last content under a Header Note Link
#' in a Project Document,
#'
#'
computeNextSubNoteLine <- function(line, projDocContents) {

  # look through projDocContents FROM line, and identify the line that begins ### or ---
    # or **[ (start of next HEADER or SIMPLE NOTE)
  for( l in (line+1):length(projDocContents) ) {

    if( substring(projDocContents[l], first=1, last=3) == "###" ||
        substring(projDocContents[l], first=1, last=3) == "---" ||
        substring(projDocContents[l], first=1, last=3) == "**[" ) {

      val <- l
      break
    }
    else {
      val <- l
    }
  }

  # next, look from val-1 BACK to line, and identify the first line
  for( lb in (val-1):(line) ) {

    if( grepl("[A-z,0-9]", projDocContents[lb]) ) { # returns TRUE if line CONTAINS any letter or number

      returnVal <- (lb+1) # return the NEXT LINE after the line which contains content
      break

    }

  }

  returnVal

}
