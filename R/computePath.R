#' Compute Path from the fullPath combined with the relPath.
#'
#' Relative Path is used to adjust the fullPath, and returns a fullPath
#' that is directed to the relativePath location.
#'
#' Examples
#'
#' 01:
#'
#' fullPath <- "/Users/user/00_ORG/01-PROGRAMME/PROJECTS/P01/P01~001~_Project_Doc.Rmd"
#'
#' relPath <- "../../LAB/LAB01-00~_Exp_01.Rmd"
#'
#' Output: /Users/user/00_ORG/01-PROGRAMME/LAB/LAB01-00~_Exp_01.Rmd
#'
#' 02:
#'
#' fullPath <- "/Users/user/00_ORG/01-PROGRAMME/PROJECTS/P01/P01~001~_Project_Doc.Rmd"
#'
#' relPath <- "../../LAB"
#'
#' Output: /Users/user/00_ORG/01-PROGRAMME/LAB
#'
#' 03:
#'
#' fullPath <- "/Users/user/00_ORG/01-PROGRAMME/PROJECTS/P01/P01~001~_Project_Doc.Rmd"
#'
#' relPath <- "../../LAB/"
#'
#' Output: /Users/user/00_ORG/01-PROGRAMME/LAB/
#'
#'
computePath <- function( fullPath, relPath ) {

  # first TRIM relPath to path with no pir jumps ("../"), and count number of "../" in the string
  parentDir <- 0

    while(TRUE) {
      if( substring(relPath, 1, 3) == "../"  ) {
        relPath <- substring(relPath, 4)
        parentDir <- parentDir + 1 # count the number of 'jumps'
      }
      else {
        break
      }
    }


  # next, trim fullPath to dir, then each dir by number of jumps (parentDir)
  fullPath <- substring(fullPath, 1, regexpr("\\/[^\\/]*$", fullPath)-1 ) # trim to DIR (if this is a DIR it is NOT moved up to parent!)
  if(parentDir > 0) {
    for(l in 1:parentDir) {
      fullPath <- dirname(fullPath)
    }
  }

  newPath <- paste(fullPath, .Platform$file.sep, relPath, sep="")

  newPath

}
