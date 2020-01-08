#' Get Next Group Prefix
#'
#' This method extracts the Prefix String from headerNotePath, then checks the
#' DIRs in headerNotePath that begin with prefixString to compute the next Group
#' Prefix - +1 from the highest prefix value.
#'
#' headerNotePath - must be the path to the headerNote - DIR or RMD file.
#'
#'
getNextGroupPrefix <- function( headerNotePath ) {

  # check if headerNotePath is a DIR or RMD file:
  if(regexpr("~_", headerNotePath, fixed=TRUE) == -1) {
    headerNoteDir <- headerNotePath
    # get prefix - extract after last file.sep:
    headerNotePrefix <- substring(headerNotePath, regexpr("\\/[^\\/]*$", headerNotePath)+1 )
  }
  else {
    # extract headerNoteDir from path - from the ~_ separator, replace fileName with file.sep
    headerNoteDir <- paste( substring(headerNotePath, 1, regexpr("~_", headerNotePath, fixed=TRUE)-1 ), sep="")
    # get prefix - extract between last file.sep and "~_" string:
    headerNotePrefix <- substring(headerNotePath, regexpr("\\/[^\\/]*$", headerNotePath)+1, regexpr("~_", headerNotePath, fixed=TRUE)-1 )
  }

  # get the ROOT of the Prefix:
  majorPrefix <- substring(headerNotePrefix, first=1, last=nchar(headerNotePrefix)-2)

  # define the Regular Exp for this prefix:
  majPrefixRegExp <- paste("^", majorPrefix, ".*", sep="")

  # look in headerNoteDir for any files - if so, compute Prefix from these, otherwise generate new Prefix for file:
  subNoteDirs <- list.dirs( headerNoteDir, full.names=FALSE, recursive=FALSE )

  # filter subNoteDirs to only contain content that starts with the majorPrefix:
  subNoteDirs <- subNoteDirs[grepl(majPrefixRegExp, subNoteDirs)]

  if(length(subNoteDirs) > 0 ) {
    # compute NEXT subNote Prefix from the DIRs in subNoteDirs:
    subNoteVals <- as.integer(substring(subNoteDirs, first=regexpr("-", subNoteDirs, fixed=TRUE)+1  ) )
    nextVal <- max(subNoteVals) +1
    if(nextVal>99) {
      nextValChar <- paste("", nextVal, sep="")
    }
    else if(nextVal>9) {
      nextValChar <- paste("0", nextVal, sep="")
    }
    else {
      nextValChar <- paste("00", nextVal, sep="")
    }
    subNotePrefix <- paste(majorPrefix, nextValChar, sep="")
  }
  else {
    # else, its the first subnote! create subNotePrefix by adding a "1" to end of headerNotePrefix:
    subNotePrefix <- paste(headerNotePrefix, "1", sep="")
  }

  subNotePrefix

}
