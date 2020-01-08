#' Get Next Simple Prefix
#'
#' This method extracts the Prefix String from fileSystemPath (isolated by extracting
#' all letters and numbers up to the first "_").  It then checks the
#' DIRs in fileSystemPath that begin with prefixString to compute the next SIMPLE
#' Prefix - +1 from the highest prefix value.
#'
#' fileSystemPath - must be a path to a VALID DIR for Project Notes - a SubDir in a PROGRAMME DIR.
#'
#'
getNextSimplePrefix <- function( fileSystemPath ) {

  # get the selected directory's BASENAME - to extract the prefix:
  pir <- basename(fileSystemPath)

  # Extract PREFIX - up to first "_"
  if(regexpr("_", pir, fixed=TRUE) > 0) {
    prefix <- substring(pir, first=1, last=regexpr("_", pir, fixed=TRUE)-1 )
  }
  else {
    prefix <- pir
  }

  # define the Regular Exp for this prefix:
  prefixRegExp <- paste("^", prefix, ".*", sep="")

  # look in headerNoteDir for any files - if so, compute Prefix from these, otherwise generate new Prefix for file:
  subDirs <- list.dirs( fileSystemPath, full.names=FALSE, recursive=FALSE )

  # filter subNoteDirs to only contain content that starts with the majorPrefix:
  subDirs <- subDirs[grepl(prefixRegExp, subDirs)]

  if(length(subDirs) > 0 ) {

    # extract all MAJOR NUMBERING of subDirs (numbers BETWEEN ~ and - for GROUP HEADERS, and AFTER ~ for SINGLE NOTES)
    subVals <- integer( length(subDirs) )
    for(l in 1:length(subDirs) ) {
      if( regexpr("-", subDirs[l], fixed=TRUE) > 0 ) {
        # this is a GROUP HEADER DIR - extract the integer between ~ and -
        subVals[l] <- as.integer( substring(
                                              subDirs[l],
                                              first=regexpr("~", subDirs[l], fixed=TRUE)+1,
                                              last=regexpr("-", subDirs[l], fixed=TRUE)-1
                                           )
                                  )
      }
      else {
        # this is a SINGLE DIR - extract the integer AFTER ~
        subVals[l] <- as.integer( substring(
                                              subDirs[l],
                                              first=regexpr("~", subDirs[l], fixed=TRUE)+1
                                            )
                                 )
      }
    }
    # subVals now contains all the index values - copmute the MAX, and then add 1 for NEXT index:
    nextIndex <- max(subVals) +1
    if(nextIndex>99) {
      nextIndexChar <- paste("", nextIndex, sep="")
    }
    else if(nextIndex>9) {
      nextIndexChar <- paste("0", nextIndex, sep="")
    }
    else {
      nextIndexChar <- paste("00", nextIndex, sep="")
    }
    simplePrefix <- paste(prefix, "~", nextIndexChar, sep="")

  }
  else {
    # else this is the first note in this DIR - set index to 001
    simplePrefix <- paste(prefix, "~001", sep="")
  }

  # return simplePrefix:
  simplePrefix

}
