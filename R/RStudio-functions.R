
#' find organisation root directory from RStudio Editor active doc path
#'
#' Used by `find_org_directory()` as second pass attempt to identify an
#' organisation root directory.
#'
#' @param isAvailable pointer to rstudioapi function - override for testing
#' @param getSourceEditorContext pointer to rstudioapi function - override for
#'   testing
find_org_rstudio_editor <- function(
    isAvailable = .is_rstudio_available(),
    getSourceEditorContext = .get_source_editor_context()
    ) {
    if( isAvailable() ) {
      # try to find orgPath from active document path
      path <- getSourceEditorContext()$path
      confPath <- get_config_dir(path)
      tempPath <- get_template_dir(path)
      while(  !( file.exists(confPath) && file.exists(tempPath) )  ) {
        path2 <- path # save in placeholder
        path <- dirname(path)
        if( path2 == path ) {
          path <- ""
          break
        }
        confPath <- get_config_dir(path)
        tempPath <- get_template_dir(path)
      }
    } else {
      path <- "" # assign path to blank string
    }
  return(fs::path(path))
}


#' Get RStudio Internal State Directory
#'
#' Returns the path to RStudio Desktop's internal session state directory,
#' taking into account RStudio version and operating system.
#'
#' - RStudio ≥ 1.4:
#'   - macOS/Linux: `~/.local/share/rstudio`
#'   - Windows: `%LOCALAPPDATA%/RStudio`
#' - RStudio ≤ 1.3:
#'   - macOS/Linux: `~/.rstudio-desktop`
#'   - Windows: `%LOCALAPPDATA%/RStudio-Desktop`
#'
#' RStudio Server is not supported.
#'
#' see: https://support.posit.co/hc/en-us/articles/200534577-Resetting-RStudio-Desktop-s-State
#'
#' @return A filesystem path to the RStudio session state directory (as an
#'   `fs::path`), or `NULL` if unavailable.
#' @keywords internal
#' @export
get_rstudio_internal_state_dir <- function() {
  # Detect if running on RStudio Server
  is_server <- Sys.getenv("RSTUDIO", unset = "") == "1" &&
    !nzchar(Sys.getenv("DISPLAY")) &&
    nzchar(Sys.getenv("USER"))

  if (is_server) {
    warning("RStudio Server detected: internal state directory is not accessible.")
    return(NULL)
  }

  # Try to get RStudio version
  rstudio_version <- tryCatch({
    vi <- rstudioapi::versionInfo()
    vi$version
  }, error = function(e) NULL)

  is_new_version <- TRUE
  if (is.null(rstudio_version)) {
    message("Could not detect RStudio version; assuming RStudio ≥ 1.4 internal state layout.")
  } else {
    is_new_version <- utils::compareVersion(as.character(rstudio_version), "1.3.999") > 0
  }

  # Determine correct path
  if (.Platform$OS.type == "windows") {
    if (is_new_version) {
      state_dir <- rappdirs::user_data_dir("RStudio", appauthor = NULL)
    } else {
      # Older RStudio (≤1.3) used RStudio-Desktop
      state_dir <- fs::path(Sys.getenv("LOCALAPPDATA"), "RStudio-Desktop")
    }
  } else {
    if (is_new_version) {
      state_dir <- "~/.local/share/rstudio"
    } else {
      state_dir <- "~/.rstudio-desktop"
    }
  }

  fs::path_expand(fs::path(state_dir))
} #### ____ ####


#' List the open documents as a tibble
#'
#' *Assumes* you already have (or can write) `get_rstudio_open_doc_IDs()`
#' returning `list(list(id, path), …)`.
#' @keywords internal
list_open_documents <- function(get_open = get_rstudio_open_doc_IDs) {
  raw <- get_open()
  tibble::tibble(
    id   = vapply(raw, `[[`, "", 1),
    path = vapply(raw, `[[`, "", 2),
    name = basename(path)
  )
}


#' Extract IDs and paths of currently open RStudio documents
#'
#' Uses internal RStudio session state files to extract a list of open documents.
#' This function is useful for inspecting session metadata for all open files.
#'
#' @return A list of character vectors where each entry contains the file ID and
#'   its associated path (or "null" if unsaved).
#' @export
get_rstudio_open_doc_IDs <- function() {
  # Identify the active session directory
  session_dir <- get_active_rstudio_session_dir()

  if (is.null(session_dir) || !fs::dir_exists(session_dir)) {
    stop("No valid active RStudio session directory found.")
  }

  # List all metadata files (excluding -contents and lock_file)
  all_files <- fs::dir_ls(session_dir, type = "file")
  id_files <- all_files[
    !grepl("-contents$", fs::path_file(all_files)) &
      !grepl("lock_file$", fs::path_file(all_files))
  ]

  # Extract file metadata from each file
  fileList <- list()
  relVector <- integer()

  for (i in seq_along(id_files)) {
    file <- id_files[i]
    lines <- readLines(file, warn = FALSE)

    # Extract file ID
    file_id <- fs::path_file(file)

    # Extract path
    path_line <- grep('"path"', lines, value = TRUE)
    file_path <- if (any(grepl("null", path_line))) {
      "null"
    } else {
      sub('.*"path"\\s*:\\s*"(.*?)".*', "\\1", path_line)
    }

    # Extract relative order
    rel_line <- grep('"relative_order"', lines, value = TRUE)
    rel_order <- if (length(rel_line) > 0) {
      as.integer(sub('.*"relative_order"\\s*:\\s*([0-9]+).*', "\\1", rel_line))
    } else {
      NA_integer_
    }

    fileList[[i]] <- c(file_id, file_path)
    relVector[i] <- rel_order
  }

  # Order by relative_order if available
  fileList <- fileList[order(relVector, na.last = TRUE)]

  fileList

}



#' Identify the active RStudio session directory
#'
#' Main function to detect the currently active RStudio session directory.
#' Internally calls several helper functions to match document paths and session metadata.
#'
#' @param verbose Logical. If `TRUE`, prints messages about stale session cleanup. Default is `TRUE`.
#'
#' @return A string path to the active `session-*` directory, or `NULL` if none could be identified.
#' @keywords internal
#' @export
get_active_rstudio_session_dir <- function(verbose = TRUE) {

  doc_paths <- .get_open_doc_paths()

  sources_dirs <- .get_candidate_sources_dirs()

  session_dirs <- unlist(lapply(sources_dirs, function(sdir) {
    fs::dir_ls(sdir, type = "directory", regexp = "/session-[^/]+$")
  }), use.names = FALSE)

  all_matches <- .find_matching_sessions(session_dirs, doc_paths)

  if (length(all_matches) == 0) {
    warning("Could not determine active RStudio session directory (no matches found).")
    return(NULL)
  }

  matching_session <- .select_most_recent_session(all_matches)

  .cleanup_stale_sessions(session_dirs, matching_session, verbose = verbose)

  return(matching_session)
}


#' Get open document paths from RStudio source editor
#'
#' Attempts to safely retrieve the full paths of all open documents in the RStudio source editor.
#'
#' @return A character vector of absolute file paths.
#' @keywords internal
.get_open_doc_paths <- function() {
  contexts <- .get_source_editor_context()
  if (!is.list(contexts) || !is.null(contexts$path)) {
    contexts <- list(contexts)
  }
  paths <- unique(vapply(contexts, function(x) x$path, character(1)))
  fs::path_abs(fs::path_expand(paths))
}


#' Find candidate RStudio sources directories
#'
#' Determines whether the current session is project-based or global,
#' and returns the appropriate "sources" directories to inspect.
#'
#' @return A character vector of "sources" directory paths.
#' @keywords internal
.get_candidate_sources_dirs <- function() {
  project_path <- .get_active_project_path()
  sources_dirs <- character()

  if (is.null(project_path)) {
    global_sources <- fs::path_expand(
      fs::path(projectmanagr::get_rstudio_internal_state_dir(), "sources")
    )
    if (fs::dir_exists(global_sources)) {
      sources_dirs <- global_sources
    }
  } else {
    rproj_user_dir <- fs::path(project_path, ".Rproj.user")
    if (fs::dir_exists(rproj_user_dir)) {
      candidate_sources <- fs::dir_ls(
        path = rproj_user_dir,
        recurse = TRUE,
        type = "directory",
        regexp = "/sources$"
      )
      sources_dirs <- candidate_sources
    }
  }

  sources_dirs
}


#' Find sessions matching open document paths
#'
#' Searches through session metadata files to find sessions that have open documents matching the current session.
#'
#' @param session_dirs A character vector of session directory paths.
#' @param doc_paths A character vector of open document paths.
#'
#' @return A list of matches, each containing `session_dir` and `file` elements.
#' @keywords internal
.find_matching_sessions <- function(session_dirs, doc_paths) {
  matches <- list()

  for (session_dir in session_dirs) {
    files <- fs::dir_ls(session_dir, type = "file")
    id_files <- files[!grepl("-contents$", fs::path_file(files))]

    for (file in id_files) {
      json <- tryCatch(
        jsonlite::fromJSON(readLines(file, warn = FALSE), simplifyVector = TRUE),
        error = function(e) NULL
      )
      if (!is.null(json) && "path" %in% names(json)) {
        json_path <- fs::path_abs(fs::path_expand(json$path))
        if (json_path %in% doc_paths) {
          matches[[length(matches) + 1]] <- list(
            session_dir = session_dir,
            file = file
          )
        }
      }
    }
  }

  matches
}

#' Select the most recent matching session
#'
#' If multiple sessions match, selects the session with the most recently modified metadata file.
#'
#' @param matches A list of matched sessions.
#'
#' @return A string path to the selected session directory.
#' @keywords internal
.select_most_recent_session <- function(matches) {
  if (length(matches) == 1) {
    return(matches[[1]]$session_dir)
  }

  mod_times <- vapply(matches, function(match) {
    fs::file_info(match$file)$modification_time
  }, as.POSIXct(NA))

  newest_idx <- which.max(mod_times)
  matches[[newest_idx]]$session_dir
}

#' Clean up stale RStudio sessions
#'
#' Deletes session directories that were not identified as the active session.
#'
#' @param session_dirs A character vector of all session directories.
#' @param matching_session A character string path to the active session directory.
#' @param verbose Logical. If `TRUE`, print message when deleting stale sessions.
#' @keywords internal
.cleanup_stale_sessions <- function(session_dirs, matching_session, verbose = TRUE) {
  stale_sessions <- setdiff(session_dirs, matching_session)

  if (length(stale_sessions) > 0 && isTRUE(verbose)) {
    message("Removing ", length(stale_sessions), " stale RStudio session director",
            if (length(stale_sessions) > 1) "ies:" else "y:", "\n",
            paste("  -", stale_sessions, collapse = "\n"))
  }

  for (dir in stale_sessions) {
    tryCatch(
      fs::dir_delete(dir),
      error = function(e) warning("Could not delete stale session dir: ", dir)
    )
  }
} #### ____ ####




#' RStudio Docs filtered and Reordered
#'
#' Filtered to ensure only files that are in the `filePath` orgPath and are
#' project notes (not unknown files or project docs).
#'
#' Reordered to list all files from the right of the selected file (`filePath`).
#'
get_rstudio_doc_list_filtered_reordered <- function(filePath, settings) {

  orgPath <- fs::path_expand(find_org_directory(filePath))

  # get all open RStudio Doc paths:
  fileList <- get_rstudio_open_doc_IDs()

  foundActiveDoc = FALSE

  reorderedFileList = list() # all files to RIGHT of current active doc are stored here in order L>R
  firstFileList = list() # all files to LEFT of current active doc are stored here in order L>R
  numberedFileList <- list() # will fill the choices var for selectInput shiny widget
  # must be a list of numbers, with the names of each item the String that is presented in selectInput

  foundIndex <- 0

  for(i in 1:length(fileList) ) {

    if(foundActiveDoc == TRUE) {

      foundIndex <- foundIndex + 1

      reorderedFileList[[foundIndex]] <- fileList[[i]][2]

      numberedFileList[(i-1)] <- i-1 # AFTER skipped Active Doc, so set list[i-1] to i-1

    }
    else if( fileList[[i]][2] != filePath ) {
      firstFileList[[i]] <- fileList[[i]][2]

      numberedFileList[i] <- i # BEFORE skipping Active Doc, so set list[i] to i
    }
    else if( fileList[[i]][2] == filePath ) {

      foundActiveDoc = TRUE

      # This is the Active Doc - so DO NOT add an index to numberedFileList

    }

  }

  # concat the list of file paths, with files to the RIGHT of active doc FIRST:
  reorderedFileList <- c(reorderedFileList, firstFileList)

  if(length(reorderedFileList) == 0) {
    return(reorderedFileList) # return blank list
  }

  # determine file type and FILTER
  fileType <- lapply(reorderedFileList, get_file_type, settings)
  reorderedNoteList <- reorderedFileList[fileType != "DOC" & fileType != "UNKNOWN"]

  if(length(reorderedNoteList) == 0) {
    return(reorderedNoteList) # return blank list
  }

  # determine orgPath and FILTER
  orgPaths <- lapply(reorderedNoteList, find_org_directory)
  reorderedNoteOrgList <- reorderedNoteList[orgPaths == orgPath]

  if(length(reorderedNoteOrgList) == 0) {
    return(reorderedNoteOrgList) # return blank list
  }

  numFileList <- as.list(seq(length(reorderedNoteOrgList)))
  #names(numFileList) <- lapply(reorderedNoteOrgList, basename)
  names(numFileList) <- reorderedNoteOrgList

  numFileList

} #### ____ ####

#' Get Source Editor Context (safe wrapper)
#'
#' Returns the active source‑editor context, or `list()` when there is none
#' (e.g. the user is on the console or running tests in a non‑RStudio
#' session).  The `tryCatch()` makes **unit‑testing** and **headless CI**
#' runs safer: instead of crashing with an RStudio API error you get an
#' empty sentinel the calling code can handle gracefully.
#' @keywords internal
.get_source_editor_context <- function() {
  tryCatch(rstudioapi::getSourceEditorContext(), error = function(e) list())
}


# A helper that enriches the raw context with cursor info we need later.
fetch_active_context <- function(get_context = .get_source_editor_context) {
  ctx <- get_context()
  if (length(ctx) == 0L) {      # empty list → no active editor
    stop("No active source editor – can’t insert a hyperlink.", call. = FALSE)
  }

  cursor <- rstudioapi::primary_selection(ctx)

  list(
    id           = ctx$id,
    path         = ctx$path,
    cursor_range = cursor$range,
    line         = cursor$range[[1]][1],
    column       = cursor$range[[1]][2]
  )
}



#' Get Active Project Path (wrapper)
#'
#' Thin wrapper around rstudioapi::getActiveProject().
#' Returns the project path or NULL if not in a project.
#' @keywords internal
.get_active_project_path <- function() {
  tryCatch(rstudioapi::getActiveProject(), error = function(e) NULL)
}


.get_context_path <- function() {
  .get_source_editor_context()$path
}


.get_context_contents <- function() {
  .get_source_editor_context()$contents
}

.get_context_id <- function() {
  .get_source_editor_context()$id
}

.get_context_row <- function() {
  .get_source_editor_context()$selection[[1]]$range$start[1]
}

.get_context_col <- function() {
  .get_source_editor_context()$selection[[1]]$range$start[2]
}

.get_context_cursor_range <- function() {
  rstudioapi::primary_selection(.get_source_editor_context())$range
}

.get_context_row_end <- function() {
  .get_source_editor_context()$selection[[1]]$range$end[1]
}

#' Check if RStudio API is available
#' @keywords internal
.is_rstudio_available <- function() {
  rstudioapi::isAvailable()
}

#' Set document contents in RStudio
#' @keywords internal
.set_document_contents <- function(text, id) {
  rstudioapi::setDocumentContents(text = text, id = id)
}

.save_context_doc <- function() {
  if( .is_rstudio_available() ) { rstudioapi::documentSave(id = .get_context_id()) }
}

.save_all_doc <- function() {
  if( .is_rstudio_available() ) { rstudioapi::documentSaveAll() }
}

#' Insert Text at Cursor (wrapper)
#'
#' Thin wrapper around rstudioapi::insertText.
#' @keywords internal
.insert_text <- function(...) {
  if( .is_rstudio_available() ) { rstudioapi::insertText(...) }
}

#' Navigate to File (wrapper)
#'
#' Thin wrapper around rstudioapi::insertText.
#' @keywords internal
.navigate_to_file <- function(path) {
  if( .is_rstudio_available() ) { rstudioapi::navigateToFile(path) }
}

#' Navigate Pane to File (wrapper)
#'
#' Thin wrapper around rstudioapi::insertText.
#' @keywords internal
.navigate_pane_to_file <- function(path) {
  if( .is_rstudio_available() ) { rstudioapi::filesPaneNavigate(dirname(path)) }
}#### ____ ####


#' Set RStudio Keybindings to Addins
#'
#' Sets the default keybindings for projectmanagr functions in rstudio, via the
#' keybindings/addins.json file.
#'
#' The template file can be found in every projectmanagr organisation root:
#'
#' `.config/addins.json`
#'
#' This file can be modified for a different default set of projectmanagr
#' functions, and different keyboard shortcuts.
#'
#' @export
set_rstudio_keybindings_addins <- function(path=getwd()) {

  # Can edit via rstudio.prefs package : https://www.danieldsjoberg.com/rstudio.prefs/
  #
  # UNIX : ~/.R/rstudio/keybindings/addins.json
  # WINDOWS : C:/Users/sjobergd/AppData/Roaming/RStudio/keybindings/addins.json
  #
  # {HOME_DIR} :
  # * unix : .R/rstudio/keybindings/addins.json
  # * windows : AppData/Roaming/RStudio/keybindings.addins.json

  # get keybindings path
  keybindings_path <- fs::path(get_rstudio_config_dir(), "keybindings")
  if( !fs::dir_exists(keybindings_path) ) {
    fs::dir_create(keybindings_path)
  }

  # check if addins.json exists
  keybindings_json_path <- fs::path(keybindings_path, "addins.json")

  # read the current keybindings
  if( !fs::file_exists(keybindings_json_path)) {
    keybindings_json <- list() # generate blank list
  } else {
    keybindings_json <- jsonlite::read_json(keybindings_json_path)
  }

  # read the projectmanagr organisation - config - keybindings json template
  orgPath <- find_org_directory(path)
  if(orgPath == "" ) { # only if orgPath not identified
    stop( paste0("  path is not in an Organisation: ", path) )
  }
  confPath <- get_config_dir(orgPath)
  keybindings_json_template_path <- fs::path(confPath, "addins.json")
  keybindings_json_template <- jsonlite::read_json(keybindings_json_template_path)

  # add or replace all named list entries in template into keybindings_json
  keybindings_json <- rlist::list.merge(keybindings_json, keybindings_json_template)

  # write new keybindings ot rstudio config file
  jsonlite::write_json(keybindings_json, keybindings_json_path)

}


#' Get RStudio Config Dir
#'
#' Returns this from the settings or the default depending on OS.type (unix or Windows)
#'
#' Returns config directory depending on OS.type (unix or Windows) & rstudio
#' version (1.2 and prior OR 1.3+)
#'
#' * 1.3+ unix : ~/.config/rstudio/keybindings/rstudio_bindings.json
#'
#' This is where key configuration files are kept - eg. keybindings
#'
#' See :
#'
#' https://support.posit.co/hc/en-us/articles/206382178
#' 'Customizing Keyboard Shortcuts in the RStudio IDE'
#'
#'
get_rstudio_config_dir <- function() {
  if( .Platform$OS.type == "unix") {
    path <- rappdirs::user_config_dir("rstudio", os="unix")
  } else {
    path <- rappdirs::user_config_dir("RStudio", appauthor=NULL)
  }
  fs::path(path) # return
}






#' Convert plain text colour to ANSI code
#'
#' @param colour colour in plain text ("red", "green", etc.) to convert to ANSI
#'
#' @return string representing provided colour as ANSI encoding
#'
#' @examples
#' colour_to_ansi("red") # gives: "\033[31m"
colour_to_ansi <- function(colour) {
  # Note ANSI colour codes
  colour_codes <- list(
    "black" = 30,
    "red" = 31,
    "green" = 32,
    "yellow" = 33,
    "blue" = 34,
    "magenta" = 35,
    "cyan" = 36,
    "white" = 37
  )

  # Check colour provided in codes above
  if (colour %in% names(colour_codes) == FALSE) {
    stop(
      paste0(
        "Colour provided (", colour, ") can't be converted to ANSI. ",
        "Must be one of: \n", paste(names(colour_codes), collapse = ",")
      )
    )
  }

  # Create ANSI version of colour
  ansi_colour <- paste0("\033[", colour_codes[[colour]], "m")

  return(ansi_colour)
}

#' Print (cat) progress text as coloured text
#'
#' @param text string to print using cat()
#' @param colour plain text colour ("red", "green", etc.). Defaults to "green"
#'
#' @examples
#' coloured_print("This is a test", colour = "blue")
coloured_print <- function(text, colour = "green") {
  cat(colour_to_ansi(colour), text, "\033[0m\n")
}


