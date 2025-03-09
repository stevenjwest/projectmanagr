
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

  #### get org and settings ####

  projectNotePath <- fs::path_expand(filePath)

  orgPath <- find_org_directory(filePath)
  if(orgPath == "" ) { # only if orgPath not identified
    stop( paste0("  filePath is not in a ProjectManagr Organsiation: ", filePath) )
  }

  # get config templates settings yml
  settings <- get_settings_yml(orgPath)

  # step required to mvoe file depend on its type
  type <- get_file_type(filePath, settings)

  if( type == "UNKNOWN" ) { # do not support moving files that are not Project Docs or Notes

  } else if( type == "DOC" ) {

  } else if( type == "NOTE") {

  } else if( type == "HEAD") {

  } else if( type == "SUB") {

  } else { # type returned does not match any expected value??

  }
}


move_doc <- function() {

  # create new project doc

  # replicate the content & modify GDT links to notes

  # update links to project docs in linked notes


}


move_note <- function() {

  # create new project note

  # link all GDTs to new note && update links in project docs


}


move_head <- function() {

  # create new project note

  # link all GDTs to new note && update links in project docs


}


move_sub <- function() {

  # create new project note

  # link all GDTs to new note && update links in project docs


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

