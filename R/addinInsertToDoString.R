#' Insert ToDo String
#'
#' Inserts TODO, bullet with header and sub-bullet,
#' at the current cursor location.
#'
#' @export
addinInsertToDoString <- function() {

  # recapture, to ensure the path is retrieved!
  context <- rstudioapi::getSourceEditorContext()

  original <- context$contents

  cursor <- rstudioapi::primary_selection(context)
  line <- (cursor$range[[1]])[1] # get the line number of cursor
  col <- (cursor$range[[1]])[2] # get the col number of cursor

  # define TODO string
  todo_string <- c("* [] **TODO_DESCRIPTION**:", "", "    + details_of_todo")

  rstudioapi::insertText( todo_string )

}
