
#' Replace a Multiline String in R Markdown Files Recursively
#'
#' This function searches recursively for `.Rmd` files in the given directory
#' and replaces all occurrences of a specific multiline string (`oldCharVector`)
#' with a new multiline string (`newCharVector`).
#'
#' Line breaks and whitespace are treated literally, ensuring that block-level
#' text (like checklists or markdown sections) can be precisely updated.
#'
#' @param directory Character string. Path to the directory to search for `.Rmd` files.
#' @param oldCharVector Character string. The exact multiline string to be replaced.
#' @param newCharVector Character string. The new multiline string to replace the old one.
#'
#' @return This function does not return a value. It updates `.Rmd` files in place and prints messages indicating which files were modified.
#'
#' @examples
#' \dontrun{
#' replace_in_rmd_files(
#'   directory = "notes/",
#'   oldCharVector = "* Old checklist item\n\n+ subtask",
#'   newCharVector = "* Updated checklist item\n\n+ subtask"
#' )
#' }
#'
#' @export
replace_in_rmd_files <- function(directory, oldCharVector, newCharVector) {
  stopifnot(length(oldCharVector) == 1, length(newCharVector) == 1)

  # Escape special characters in the pattern
  escaped_old <- gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", oldCharVector)
  pattern <- gsub("\n", "\\\\n", escaped_old)  # Convert literal newlines to regex

  files <- list.files(directory, pattern = "\\.Rmd$", recursive = TRUE, full.names = TRUE)

  for (file in files) {
    text <- readLines(file, warn = FALSE)
    joined <- paste(text, collapse = "\n")

    updated <- gsub(pattern, newCharVector, joined, perl = TRUE)

    if (!identical(joined, updated)) {
      writeLines(strsplit(updated, "\n")[[1]], file)
      message("✅ Updated: ", file)
    } else {
      message("⚠️ No change made to: ", file)
    }
  }
}


