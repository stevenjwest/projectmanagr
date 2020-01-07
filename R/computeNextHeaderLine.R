#' Compute next Header line
#'
#' Returns the first blank line after the last content in Header Note contents.
#'
#'
#'
computeNextHeaderLine <- function(headerContents) {

  returnVal <- 1

  # start at the END of headerContents:
  for( l in length(headerContents):1 ) {

    if( grepl("[A-z,0-9]", headerContents[l]) ) { # returns TRUE if line CONTAINS any letter or number

      returnVal <- (l+1) # return the NEXT LINE after the line which contains content
      break

    }

  }

  returnVal

}
