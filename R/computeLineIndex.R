#' Compute Line
#'
#' Returns the index of the first EXACT MATCH of line in contents, if not found returns -1.
#'
#' line - a String to be found
#'
#' contents - a vector of strings, the index of the first instance of line in this vector is returned.
#'
#'
computeLineIndex <- function(line, contents) {

  returnVal <- -1
  # look through contents to find an index that matches line:
  for( l in 1:length(contents) ) {

    if( contents[l] == line ) {
      returnVal <- l
      break
    }

  }

  returnVal

}
