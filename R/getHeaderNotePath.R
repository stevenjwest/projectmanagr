#' Get Project Header Note Path
#'
#' Compute a Project Header Note Path from a fileSystem path and a projectNote Name.
#'
#' The fileSystemPath must be a DIRECTORY and sub-dir in a PROGRAMME DIRECTORY
#' - either a project Document DIR, or another Directory in a PROGRAMME for
#' storing Project Notes.
#'
#' The NAME of the selected Directory is used to derive the PREFIX for files within this
#' Directory - it uses all LETTERS and NUMBERS UP TO the first "_" in the directory name.
#' If no Underscore exists, will use the whole Directory name as the Prefix for the projectNote.
#'
getHeaderNotePath <- function( fileSystemPath, projectHeaderNoteName ) {

  # search inside fileSystemPath to identify any DIRECTORIES that start with prefix, and if so, compute the NEXT INDEX:
  # NB take care of both SIMPLE and GROUP directories (Prefix~NUMBER for simple, Prefix~NUMBER-00 for GROUP)
  prefix <- paste( getNextSimplePrefix(fileSystemPath), "-00", sep="")

  paste(fileSystemPath, .Platform$file.sep, prefix, "~_", projectHeaderNoteName, ".Rmd", sep="")

}
