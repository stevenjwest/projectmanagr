
#' Create a Database
#'
#' A Database is a tibble (stored on disk as a CSV file) with a predefined
#' set of columns for storing items.
#'
#' To create a database, a filename String and a columns character vector
#' must be provided.  An empty tibble is formed and returned from this
#' function: it contains the specified columns, and this tibble contains
#' a 'path' attribute, that indicates where it is stored RELATIVE TO THE
#' CURRENT RMD FILE, and this can be retrieved using `attr(tibble, "path")`.
#'
#' The database is stored alongside the Rmd that calls it in the file
#' system - therefore this function can only be called from an Rmd, and
#' the Rmd must be saved to disk.
#'
#' @param filename the filename of the database to load.  Do NOT add .csv extension!
#'
#' @param columns A character vector containing a list of columns to add to this database.
#'
#' @return The loaded TIBBLE with the "path" attribute set to the database filename.
#'
#'@export
database_create <- function(filename, columns) {

  cat( "\nprojectmanagr::database_create():\n" )

  # check columns is a character vector, and contains unique values
  if(is.character(columns) == FALSE) {
    columns <- as.character(columns)
    if(is.character(columns) == FALSE) {
      stop( paste0("  columns is not a character vector: ", columns) )
    }
  }

  if(length(columns) == 0) {
    stop( paste0("  columns is of length 0: ", columns) )
  }

  if( any( duplicated(columns) ) ) {
    stop( paste0("  columns contains duplicate values: ", columns[duplicated(columns)]) )
  }

  # get the currently active source editor context - hoepfully a projectmanagr Project Note!
  context <- rstudioapi::getSourceEditorContext()
    # this will fail if rstudio is not running!
    # if no items are open in the source editro, context will be NULL:

  if(is.null(context) ) {
    stop( paste0("  No Rmd open in Source Editor.") )
  }

  # getRmd's DIR:
  DATA_DIR <- normalizePath(dirname(context$path) )

  # if filename already exists at relative path to current Rmd, STOP!
  path <- paste0(DATA_DIR, .Platform$file.sep, filename, ".csv")

  if( file.exists(path) ) {
    stop( paste0("  the database already exists: ", path) )
  }

  # Create a blank tibble with columns:
  vect <- setNames(rep("", length(columns)), columns)
  database <- dplyr::bind_rows(vect)[0,]

  # Write to CSV:
  readr::write_csv(database, path )
  cat( "\n  Written database CSV:", path, "\n\n" )

  # set path attr to RELATIVE PATH - i.e. just the filename
  attr(database, "path") <- filename

  # return the new tibble:
  database


}


#' Load Database
#'
#' Load a database located at the relativepath from the current Rmd.
#'
#' @param relativepath the relative path from the Rmd to the dataset CSV to load.  Do NOT
#' add .csv extension!
#'
#' @return The loaded TIBBLE with the "path" attribute set to the sample filename.
#'
#' @export
database_load <- function(relativepath) {

  cat( "\nprojectmanagr::database_load():\n" )

  # get the currently active source editor context - hoepfully a projectmanagr Project Note!
  context <- rstudioapi::getSourceEditorContext()
  # this will fail if rstudio is not running!
  # if no items are open in the source editro, context will be NULL:

  if(is.null(context) ) {
    stop( paste0("  No Rmd open in Source Editor.") )
  }

  # getRmd's DIR:
  DATA_DIR <- normalizePath(dirname(context$path) )
  path <- normalizePath( paste0(DATA_DIR, .Platform$file.sep, relativepath, ".csv") )
   # normalisePath will remove any ../ references from relativepath

  if( file.exists(path) == FALSE ) {
    stop( paste0("  the database doesn't exists at this path: ", path) )
  }

  # Read CSV:
  database <- readr::read_csv(path)
  cat( "\n  Read database CSV:", path, "\n\n" )

  # set path attr to RELATIVE PATH:
  attr(database, "path") <- relativepath

  # return the new tibble:
  database

}



#' Save Database
#'
#' @param a database - tibble with 'path' attribute
#'
#' @export
database_save <- function(database) {

  cat( "\nprojectmanagr::database_save():\n" )

  # get the currently active source editor context - hoepfully a projectmanagr Project Note!
  context <- rstudioapi::getSourceEditorContext()
  # this will fail if rstudio is not running!
  # if no items are open in the source editro, context will be NULL:

  if(is.null(context) ) {
    stop( paste0("  No Rmd open in Source Editor.") )
  }

  # getRmd's DIR:
  DATA_DIR <- normalizePath(dirname(context$path) )

  # get the relative path:
  relativepath <- attr(database, "path")

  if(is.null(relativepath) ) {
    stop( paste0("  database does not contain a `path`` attribute: ", database) )
  }

  path <- normalizePath( paste0(DATA_DIR, .Platform$file.sep, relativepath, ".csv") )

  # Write to CSV:
  readr::write_csv(database, path )
  cat( "\n  Written database CSV:", path, "\n\n" )

}



#' Add row to database
#'
#'
#'@export
database_add_row <- function(database, data) {

  # check the FIRST ELEMENT in data - check it doesnt already exist in database:
    # first element should ALWAYS be an ID and be UNIQUE for the item being entered
  check_database_for_ID(database, data[1]) # ID should be the FIRST ITEM

  cols <- colnames(database)

  # check data and cols are same length:
  if(length(data) != length(cols) ) {
    stop( paste0("  data not of correct length: ", data) )
  }

  vect <- setNames(data, cols)

  relativepath <- attr(database, "path")

  database <- dplyr::bind_rows(database, vect)

  # reassign the relativepath:
  attr(database, "path") <- relativepath

  database

}


check_database_for_ID <- function(database, ID) {

  if( is.element(ID, database[[1]]) ) {
    stop( paste0("  database already contains ID: ", ID) )
  }

}


#' Edit row in database
#'
#' Overwrite the line in database with parsed ID with content of data.
#'
#'
database_edit_row <- function(database, ID, data) {

}



#' Delete row in database
#'
#' Delete line in database with parsed ID.
#'
#'
database_delete_row <- function(database, ID) {



}
