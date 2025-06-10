

#' Insert vector at indices in contents
#'
#' Inserts `replacementVector` into `templateContents` *before* each line number
#' in `indices`.
#' Line numbers must be between 1 and `length(templateContents) + 1`.
#'
#' @param templateContents Character vector of lines
#' @param indices Integer vector of line numbers at which to insert
#' @param replacementVector Character vector to insert
#'
#' @return Modified character vector with insertions
#' @export
insert_at_indices <- function(templateContents, indices, replacementVector) {
  n <- length(templateContents)

  # Check for invalid indices (strict 1-based insertion)
  if (any(indices < 1 | indices > (n + 1))) {
    stop("All indices must be between 1 and length(templateContents) + 1")
  }

  indices <- sort(indices, decreasing = TRUE)

  for (i in indices) {
    before <- if (i == 1) character(0) else templateContents[1:(i - 1)]
    after  <- if (i > n) character(0) else templateContents[i:n]
    templateContents <- c(before, replacementVector, after)
    n <- length(templateContents)
  }

  templateContents
}



#' Replace lines at indices with replacement vector
#'
#' Replaces each index in `indices` with the contents of `replacementVector`.
#' The line at each index is completely replaced. All indices must be between
#' 1 and `length(templateContents)` (inclusive).
#'
#' @param templateContents Character vector of lines
#' @param indices Integer vector of line numbers to replace
#' @param replacementVector Character vector to insert in place of each index
#'
#' @return Modified character vector
#' @export
replace_at_indices <- function(templateContents, indices, replacementVector) {
  n <- length(templateContents)

  # Enforce strict 1-based bounds
  if (any(indices < 1 | indices > n)) {
    stop("All indices must be between 1 and length(templateContents)")
  }

  indices <- sort(indices, decreasing = TRUE)

  for (i in indices) {
    before <- if (i == 1) character(0) else templateContents[1:(i - 1)]
    after  <- if (i == n) character(0) else templateContents[(i + 1):n]

    templateContents <- c(before, replacementVector, after)
    n <- length(templateContents)
  }

  templateContents
}



#' Substitute a templates' parameter for content
#'
#' This function deals with the cases where the paramContents points a multi-lined
#' vector, or where paramContents points to to a template file in templates/ that
#' specifies multi-lined content to insert into the templateContents vector.
#'
sub_template_param <- function(templateContents, templateParam, paramContents,
                               orgPath) {

  #### sub param in contents ####

  if(is.character(paramContents) &&
     length(paramContents) == 1 &&
     startsWith(paramContents, "::template::")) {

    # get templates
    tempPath <- get_template_dir(orgPath)

    # paramContents is a POINTER to a multi-line content file in templates dir
    # so open this and insert the multi-lined vector into templateContents
    paramPointer <- substr(paramContents, nchar("::template::")+1, nchar(paramContents))
    paramContents <- read_file( paste0( tempPath, .Platform$file.sep, paramPointer) )

    templateContents <- replace_params_with_vector(templateContents,
                                                   templateParam, paramContents)
  } else if( length(paramContents) > 1 ) {

    # paramContents is a multi-lined vector
    # so insert the multi-lined vector appropriately
    templateContents <- replace_params_with_vector(templateContents,
                                                   templateParam, paramContents)
  } else {

    # paramContents is a vector of length 1, insert appropriately with gsub()
    templateContents <- gsub(templateParam, paramContents, templateContents, fixed=TRUE)
  }

  templateContents # return
}

#' Replace every instance of `templateParam` in `templateContents` with `paramContents`.
replace_params_with_vector <- function(templateContents, templateParam, paramContents) {

  tempParamIndices <- grep(templateParam, templateContents, fixed=TRUE)
  templateContents <- replace_at_indices(templateContents, tempParamIndices, paramContents)
  templateContents

}



#' Load `paramContents` (a param from settings.yml) correctly
#'
#' Some params start with keyword `template:` which indicates the relative path
#' from the templatesDir to the template file to be loaded.
#'
#' Else `paramContents` is returned unchanged.
load_param_vector <- function(paramContents, orgPath) {

  #### load value if param points to template ####

  # load the param if it points to a file
  if(is.character(paramContents) &&
     length(paramContents) == 1 &&
     startsWith(paramContents, "::template::")) {

    # check orgPath
    orgPath <- find_org_directory(orgPath)

    # set confPath + tempPath:
    # get config templates settings yml
    confPath <- get_config_dir(orgPath)
    tempPath <- get_template_dir(orgPath)

    # paramContents is a POINTER to a multi-line content file in templates dir
    # so open this and insert the multi-lined vector into templateContents
    paramPointer <- substr(paramContents, nchar("::template::")+1, nchar(paramContents))
    paramContents <- read_file( paste0( tempPath, .Platform$file.sep, paramPointer) )

  }

  # return
  paramContents

}



#' Replace SEP Values
#'
#' In a string vector, replace every instance of SEP values - syntax used to denote
#' separators between document sections in projectmanagr templates.
#'
#' * Six separators strings are defined in config/templates in projectmanagr org
#' in the files: `SEP01.txt` to `SEP06.txt`
#'
#' * Syntax in projectmanagr templates for inserting the separators in projectmanagr
#' templates: `{{SEP01}}` to `{{SEP06}}`
#'
#'
replace_sep_values <- function(templateContents, orgPath) {


  # set confPath + tempPath:
  # get config templates settings yml
  confPath <- get_config_dir(orgPath)
  tempPath <- get_template_dir(orgPath)

  # get all SEP files form tempPath
  sepFiles <- list.files(tempPath)[ startsWith(list.files(tempPath), "SEP") ]

  # read all SEP values
  SEPS <- list()
  for( s in 1:length(sepFiles) ) {
    templateFileConn <- file( paste( tempPath, .Platform$file.sep, sepFiles[s], sep="") )
    SEPS[[s]] <- readLines( templateFileConn )
    close(templateFileConn)
  }

  #### replace SEP values ####

  sep01indices <- grep("{{SEP01}}", templateContents, fixed=TRUE)
  templateContents <- replace_at_indices(templateContents, sep01indices, SEPS[[1]])

  sep02indices <- grep("{{SEP02}}", templateContents, fixed=TRUE)
  templateContents <- replace_at_indices(templateContents, sep02indices, SEPS[[2]])

  sep03indices <- grep("{{SEP03}}", templateContents, fixed=TRUE)
  templateContents <- replace_at_indices(templateContents, sep03indices, SEPS[[3]])

  sep04indices <- grep("{{SEP04}}", templateContents, fixed=TRUE)
  templateContents <- replace_at_indices(templateContents, sep04indices, SEPS[[4]])

  sep05indices <- grep("{{SEP05}}", templateContents, fixed=TRUE)
  templateContents <- replace_at_indices(templateContents, sep05indices, SEPS[[5]])

  sep06indices <- grep("{{SEP06}}", templateContents, fixed=TRUE)
  templateContents <- replace_at_indices(templateContents, sep06indices, SEPS[[6]])

  templateContents
}




replace_markdown_header  <- function(templateContents, orgPath,
                                     htmlMarkdown="{{HTML_HEADER}}",
                                     htmlHeaderFilename="rmarkdown-html-header.txt") {

  # get config templates settings yml
  confPath <- get_config_dir(orgPath)
  tempPath <- get_template_dir(orgPath)

  # get all SEP files form tempPath
  htmlHeaderFile <- file( paste0(tempPath, .Platform$file.sep, htmlHeaderFilename) )
  htmlHeader <- readLines( htmlHeaderFile )
  close(htmlHeaderFile)

  #### replace HTML_HEADER ####
  htmlHeaderIndex <- grep(htmlMarkdown, templateContents, fixed=TRUE)
  templateContents <- replace_at_indices(templateContents, htmlHeaderIndex, htmlHeader)

  templateContents
}


#' Replace Knitr include_graphics link
#'
#' Replace the path inside knitr::include_graphics() with a relative
#' path based on source and destination file paths.
#'
#' @param contentContents Character vector of lines
#' @param sourceFilePath Path of source file (where content was read from)
#' @param destinationFilePath Path of destination file
#' @param knitrGraphics Function call to search for (default: 'knitr::include_graphics(')
#' @param knitrGraphicsEnd End of function call (default: ')')
#'
#' @return Modified character vector with updated relative links
#'
replace_knitr_include_graphics_link <- function(contentContents, sourceFilePath,
                                                destinationFilePath,
                                                knitrGraphics = "knitr::include_graphics(",
                                                knitrGraphicsEnd = ")") {

  src_dir <- dirname(sourceFilePath)
  dst_dir <- dirname(destinationFilePath)

  knitr_lines <- grep(knitrGraphics, contentContents, fixed = TRUE)

  for (i in knitr_lines) {
    line <- contentContents[[i]]

    start_idx <- regexpr(knitrGraphics, line, fixed = TRUE)[[1]] + nchar(knitrGraphics)

    # Detect opening quote
    remaining <- substring(line, start_idx)
    sq <- regexpr("'", remaining, fixed = TRUE)[[1]]
    dq <- regexpr('"', remaining, fixed = TRUE)[[1]]

    if (sq == -1 && dq == -1) next  # no quotes found; skip this line

    quote_char <- if (sq != -1 && (dq == -1 || sq < dq)) "'" else '"'
    q_start <- regexpr(quote_char, remaining, fixed = TRUE)[[1]]
    if (q_start == -1) next

    rest <- substring(remaining, q_start + 1)
    q_end <- regexpr(quote_char, rest, fixed = TRUE)[[1]]
    if (q_end == -1) next

    original_path <- trimws(substring(rest, 1, q_end - 1))

    # Resolve path relative to sourceFilePath
    abs_path <- fs::path_abs(fs::path(src_dir, original_path))

    # Compute new relative path
    rel_path <- fs::path_rel(abs_path, start = dst_dir)

    # Replace in original line — include the closing quote
    prefix <- substring(line, 1, start_idx + q_start - 1)
    suffix <- substring(rest, q_end + 1)
    contentContents[[i]] <- paste0(prefix, rel_path, quote_char, suffix)
  }

  contentContents
}



#' Replace Hyper Links
#'
#' Rewrites markdown hyperlinks that point to files, converting them from paths relative to
#' `sourceFilePath` to paths relative to `destinationFilePath`. Internal header-only links
#' (e.g., `#section`) and non-existent file links are skipped. All valid links on each line
#' are processed.
#'
#' @param contentContents Character vector of Rmd lines to scan and replace
#' @param sourceFilePath Path to original file (links are valid relative to this file)
#' @param destinationFilePath Path to destination file (we rewrite links relative to this)
#' @param hyperLinkID Not used internally; kept for legacy compatibility
#' @param hyperLinkEnd Not used internally; kept for legacy compatibility
#'
#' @return Modified character vector with updated relative hyperlink paths
#'
replace_hyper_links <- function(contentContents, sourceFilePath, destinationFilePath,
                                hyperLinkID = "](", hyperLinkEnd = ")") {

  src_dir <- dirname(sourceFilePath)
  dst_dir <- dirname(destinationFilePath)

  # Only process lines containing markdown link patterns
  hyperlink_lines <- grep(hyperLinkID, contentContents, fixed = TRUE)

  for (i in hyperlink_lines) {
    line <- contentContents[[i]]

    # Match all markdown links in the form [label](target)
    match_positions <- gregexpr("\\[([^\\]]+)\\]\\(([^)]+)\\)", line, perl = TRUE)[[1]]

    # Regex breakdown:
    #   \\[        → match literal '['
    #   ([^\\]]+)  → capture group 1: one or more characters that are not ']'
    #   \\]        → match literal ']'
    #   \\(        → match literal '('
    #   ([^)]+)    → capture group 2: one or more characters that are not ')'
    #   \\)        → match literal ')'
    #
    # This captures links like [label](target), returning:
    #   capture group 1 = label
    #   capture group 2 = target path or path#header

    if (match_positions[1] == -1) next

    match_lengths <- attr(match_positions, "match.length")

    # Process matches in reverse order to preserve character offsets
    for (j in rev(seq_along(match_positions))) {
      start <- match_positions[j]
      end <- start + match_lengths[j] - 1
      full_match <- substring(line, start, end)

      # Extract label and target from [label](target)
      link_parts <- regexec("\\[([^\\]]+)\\]\\(([^)]+)\\)", full_match, perl = TRUE)[[1]]
      captures <- regmatches(full_match, list(link_parts))[[1]]
      if (length(captures) != 3) next  # malformed

      label <- captures[2]
      link_target <- captures[3]

      # Skip header-only links (e.g., #section)
      if (startsWith(link_target, "#")) next

      # Split into path and optional header (e.g., file.Rmd#methods)
      sPH <- split_path_header(link_target)

      # Compute absolute path to target file (relative to source file)
      abs_path <- fs::path_abs(fs::path(src_dir, sPH$path))

      # Only rewrite if the file exists
      if (!fs::file_exists(abs_path)) next

      # Compute new relative path from destination file to target
      rel_path <- fs::path_rel(abs_path, start = dst_dir)

      # Construct updated markdown link
      new_link <- paste0("[", label, "](", rel_path, sPH$header, ")")

      # Replace the original link in line
      line <- paste0(
        substring(line, 1, start - 1),
        new_link,
        substring(line, end + 1)
      )
    }

    contentContents[[i]] <- line
  }

  contentContents
}

