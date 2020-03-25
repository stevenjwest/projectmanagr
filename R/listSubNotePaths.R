#' List all SubNotes that are below the Header Note.
#'
#' This Function returns a CHARACTER VECTOR of absolute paths to each subnote under the header note at headerNotePath.
#'
#' @param headerNotePath The path to the Header Note.  This MUST be in a sub-directory or lower inside a
#'   PROGRAMME Directory.  This MUST be an ABSOLUTE PATH.
#'
listSubNotePaths <- function( headerNotePath ) {

  #  Determine headerNote PREFIX
  projectNotePrefix <- substring( basename(headerNotePath), first=1, last=regexpr("~_", basename(headerNotePath), fixed=TRUE)-1 )

  # Check headerNotePrefix IS a HEADER NOTE (ending with "-00"):
  if( regexpr("-", projectNotePrefix) == -1 || substring(projectNotePrefix, regexpr("-", projectNotePrefix)+1) != "00" ) {
    # Single OR SUB NOTE:  This method is not designed to deal with these Notes - STOP:
    stop( cat("  headerNotePath is to a Single Note or Sub Note of a Group Project Note: ", headerNotePath, " Use addLinkProjectNote() Function.\n") )
  }

  # get headerNoteDir
  headerNoteDir <- paste(dirname(headerNotePath), .Platform$file.sep, projectNotePrefix, sep="" )

  # extract each file from dir - file defined by name containing string "~_"
  file_list <- list.files(headerNoteDir)
  file_list <- file_list[ regexpr("~_", file_list) > 0  ]
  file_list <- paste(headerNoteDir, .Platform$file.sep, file_list, sep="" )

  file_list

}
