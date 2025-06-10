
#' Replace a multi-line string block in an Rmd file
#'
#' Replaces `oldCharVector` with `newCharVector` in a file, with optional link
#' adjustment.
#'
#' @param file Path to the target `.Rmd` file
#' @param oldCharVector String block to replace
#' @param newCharVector Replacement block (may contain a Markdown link)
#' @param source_file Path to source file for computing relative links
#' @return A list with elements `changed` (logical) and `new_text` (character vector)
#' @noRd
replace_rmd_text_block <- function(file, oldCharVector, newCharVector, source_file) {
  text <- readLines(file, warn = FALSE)
  joined <- paste(text, collapse = "\n")

  adjusted_newCharVector <- newCharVector

  # If Markdown link exists, adjust it
  md_link_match <- regexpr("\\]\\(([^)]+)\\)", newCharVector)
  if (md_link_match != -1) {
    orig_link <- regmatches(newCharVector, md_link_match)
    link_path <- sub("^\\]\\(([^#)]+).*$", "\\1", orig_link)
    anchor <- if (grepl("#", orig_link)) sub("^.*#(.*?)\\)$", "#\\1", orig_link) else ""

    abs_link_target <- fs::path_abs(fs::path(dirname(source_file), link_path))
    rel_link <- fs::path_rel(abs_link_target, start = fs::path_dir(file))

    if (!grepl("^\\.\\./", rel_link)) {
      rel_link <- file.path("..", rel_link)
    }

    new_link <- paste0("](", rel_link, anchor, ")")
    adjusted_newCharVector <- sub("\\]\\([^)]+\\)", new_link, newCharVector)
  }

  updated <- gsub(oldCharVector, adjusted_newCharVector, joined, fixed = TRUE)

  list(
    changed = !identical(joined, updated),
    new_text = strsplit(updated, "\n", fixed = TRUE)[[1]]
  )
}



#' Replace Multi-line Text Blocks in .Rmd Files with Dynamic Link Adjustment
#'
#' Replaces a specified multi-line string block in `.Rmd` files with a new one,
#' optionally adjusting any Markdown links within the new block so they are relative
#' to each target file. Supports dry-run previews and selective messaging.
#'
#' @param directory Path to the directory containing target `.Rmd` files
#' @param oldCharVector String block to replace
#' @param newCharVector Replacement block (may contain a Markdown link)
#' @param source_file Path to source file for computing relative links
#' @param directory Directory containing `.Rmd` files.
#' @param dry_run Logical. If `TRUE`, shows a preview instead of writing. Default is `FALSE`.
#' @param show_no_match Logical. Show message for unchanged files. Default is `TRUE`.
#'
#' @return No return value. Files are updated in place unless `dry_run = TRUE`.
#' @export
replace_in_rmd_files <- function(directory,
                                 oldCharVector,
                                 newCharVector,
                                 dry_run = FALSE,
                                 show_no_match = TRUE,
                                 source_file = NULL) {
  stopifnot(length(oldCharVector) == 1, length(newCharVector) == 1)

  if (is.null(source_file)) {
    if (.is_rstudio_available()) {
      source_file <- .get_context_path()
    } else {
      stop("source_file not provided and RStudio API is not available.")
    }
  }

  files <- list.files(directory, pattern = "\\.Rmd$", recursive = TRUE, full.names = TRUE)

  for (file in files) {
    result <- replace_rmd_text_block(file, oldCharVector, newCharVector, source_file)

    if (result$changed) {
      if (dry_run) {
        message("🔍 [Dry Run] Would update: ", file)
        preview <- result$new_text
        preview_lines <- preview[!preview %in% readLines(file, warn = FALSE)]
        preview_lines <- preview_lines[preview_lines != ""]
        cat("Preview of replaced lines:\n", paste(head(preview_lines, 10), collapse = "\n"), "\n\n")
      } else {
        writeLines(result$new_text, file)
        message("✅ Updated: ", file)
      }
    } else if (show_no_match) {
      message("⚠️ No change made to: ", file)
    }
  }
}




#' Wrap R Markdown lines to a fixed width
#'
#' This function wraps a character vector of R Markdown lines so that no line exceeds
#' a given character width (default: 80), while preserving indentation and R Markdown
#' bullet/list formatting. Bullet points and numbered lists are wrapped with a hanging
#' indent. Lines that appear to contain preformatted content (e.g., tables, ASCII
#' separators, or Markdown links with no spaces) are left untouched to avoid corrupting
#' their structure.
#'
#' @param lines A character vector of R Markdown lines to be wrapped.
#' @param width Maximum number of characters allowed per line (default is 80).
#'
#' @return A character vector of the same content, with long lines wrapped to the specified width.
#'
#' @examples
#' lines <- c(
#'   "* Here is a very long bullet point that needs to be wrapped because it goes beyond 80 characters.",
#'   "    + This is a sub-bullet with additional text that also exceeds the line length limit.",
#'   "",
#'   "        - Markdown links like [example](https://example.com/very/long/url) are left as-is."
#' )
#' cat(paste(wrap_rmd_lines(lines), collapse = "\n"))
#'
#' @export
wrap_rmd_lines <- function(lines, width = 80) {
  wrapped_lines <- lapply(lines, function(line) {
    # Preserve blank lines as-is
    if (nchar(trimws(line)) == 0) return(line)

    # Check if it's a bullet or numbered list line
    is_bullet <- grepl("^\\s*([*+-]|[0-9]+[.)])\\s+", line)

    # Skip wrapping if not a bullet and line has tab, lots of spaces, or is a separator
    if (!is_bullet &&
        (grepl("\t", line) || grepl(" {2,}", line) || grepl("^[+=\\-_*]{5,}$", trimws(line)))) {
      return(line)
    }

    # Match leading whitespace and bullet/number
    match <- regexpr("^([[:space:]]*)([*+-]|[0-9]+[.)])?\\s*", line, perl = TRUE)
    indent <- regmatches(line, match)
    if (length(indent) == 0) indent <- ""

    # Split indent into whitespace and bullet
    leading_ws <- sub("([[:space:]]*).*", "\\1", indent)
    bullet_part <- sub("^[[:space:]]*([*+-]|[0-9]+[.)])\\s*", "\\1 ", indent)
    bullet_part[is.na(bullet_part)] <- ""

    first_indent <- paste0(leading_ws, bullet_part)
    hanging_indent <- strrep(" ", nchar(first_indent))

    content <- substr(line, attr(match, "match.length") + 1, nchar(line))

    wrapped <- strwrap(content,
                       width = width,
                       initial = first_indent,
                       exdent = nchar(hanging_indent))

    wrapped
  })

  unlist(wrapped_lines)
}





#' Wrap Active R Markdown Document in RStudio
#'
#' Applies `wrap_rmd_lines()` to the currently active RStudio source document,
#' replacing the contents in the editor but not saving the file (so it remains undoable).
#'
#' Only works inside an RStudio session.
#'
#' @return Invisibly returns TRUE if wrapping was applied successfully, FALSE otherwise.
#' @export
wrap_active_rmd_document <- function() {
  if (!.is_rstudio_available()) {
    message("RStudio API is not available.")
    return(invisible(FALSE))
  }

  lines <- .get_context_contents()
  if (is.null(lines) || length(lines) == 0) {
    message("No active document or empty document.")
    return(invisible(FALSE))
  }

  wrapped_lines <- wrap_rmd_lines(lines)
  .set_document_contents(text = paste(wrapped_lines, collapse = "\n"),
                         id = .get_context_id())

  invisible(TRUE)
}





