#' Insert DateTime
#'
#' Inserts DateTime, rounded to nearest 5 min interval,
#' at the current cursor location.
#'
#' @export
addinInsertDateTimeString <- function() {

  # recapture, to ensure the path is retrieved!
  context <- rstudioapi::getSourceEditorContext()

  original <- context$contents

  cursor <- rstudioapi::primary_selection(context)
  line <- (cursor$range[[1]])[1] # get the line number of cursor
  col <- (cursor$range[[1]])[2] # get the col number of cursor

  # get datetime
    # using UTC for consistency - this may mean the time is DIFFERENT to the current time!
  datetime <- lubridate::now("UTC") #Sys.time()

  # round to nearest minute:
  datetime <- round(datetime,"mins")

  # convert to POSIXlt:
  datetime <- as.POSIXlt(datetime)

  # round to nearest 5 min - not using now keep round to nearest 1min!
  #datetime$min <- (datetime$min + 5/2) %/% 5 * 5

  # format datetime to use "/" and have a ":" between date and time
  datetime_split <- strsplit(as.character(datetime), " ")
  datetime_split[[1]][1] <- gsub("-", "/", datetime_split[[1]][1] )

  datetime_colon <- paste0( datetime_split[[1]][1], ":", datetime_split[[1]][2] )

  # remove seconds:
  datetime_colon <- substr(datetime_colon, 1, nchar(datetime_colon)-3)

  #rstudioapi::insertText( paste0('"', datetime_colon, '"' ) )
  rstudioapi::insertText( datetime_colon )

}
