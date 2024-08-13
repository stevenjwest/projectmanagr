
#' Move a File to new Directory
#'
#' This Function moves a File (Project Note [simple, group] or Project Doc) to
#' a new directory in the same organisation.
#'
#' * Moves the Dir and all contents to new location
#'
#' * Updates all GDT links and hyperlinks
#'
#'     + The moved project note/doc
#'     + Any project notes contained in the Dir
#'
#' @param filePath Path to file to be moved.  Must be a Project Note
#' [simple, group] or Project Doc.
#'
#' @param destinationDir Path to destination directory.  Must be in same
#' Organisation.  If the directory does not exist, it will be created.
#'
#' @export
move_file <- function(filePath, destinationDir) {

  cat( "\nprojectmanagr::move_file():\n" )

}



#' Move a Directory to new location
#'
#' This Function moves a directory containing Project Note(s) [simple, group]
#' or Project Doc(s) to a new location.
#'
#' Updates all GDT links and hyperlinks across organisation.
#'
#' @param dirPath Path to directory to be moved.  Must not be the ORG root or
#' a PROGRAMME directory.
#'
#' @param destinationDir Path to destination directory.  Must be in same
#' Organisation.  If the directory does not exist, it will be created.
#'
#' @export
move_dir <- function(dirPath, destinationDir) {

  cat( "\nprojectmanagr::move_dir():\n" )

}

