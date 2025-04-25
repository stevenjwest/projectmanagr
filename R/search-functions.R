#' Search Dir Tree
#'
#' Searches all Project Docs & Notes in a Dir Tree within an Organisation,
#' returning a datatable with FILENAME, CONTEXT, LOCATION LINE columns.
#'
#' @param path Dir Tree Path for searching text files.
#'
#' @param searchTerm The sring to search for.
#'
#' @param updateProgress Object to show progress in shiny gadget.
#'
#' @param settings Settings list from projectmanagr config
#'
#' @param orgPath The organisation path where search takes place.
#'
#' @param ignoreCase Whether search should ignore case of searchTerm. TRUE by
#' default.
#'
#' @param fixed When fixed is TRUE, no regular expression patterns are parsed.
#' More efficient search is performed with fixed TRUE.
#'
#'@export
search_dir_tree <- function(path, searchTerm, updateProgress = NULL,
                            settings, orgPath, ignoreCase=TRUE, fixed=TRUE) {

  cat( "\nprojectmanagr::search_dir_tree():\n" )
  cat("  ignoreCase : ", ignoreCase, "\n")
  cat("  fixed : ", fixed, "\n")


  #### instance variables ####

  # if not an absolute path:
  if( R.utils::isAbsolutePath(path) == FALSE ) {
    path <- R.utils::getAbsolutePath(path )
  }

  # normalize path - remove HOME REF ~
  path <- fs::path_expand(path)



  # get config templates settings yml
  confPath <- get_config_dir(orgPath)
  tempPath <- get_template_dir(orgPath)
  settings <- get_settings_yml(orgPath)

  # get dirs in root to EXCLUDE from search
  volPath <- get_volumes_dir(orgPath, settings)
  sitePath <- get_site_dir(orgPath, settings)
  journalPath <- get_journal_dir(orgPath, settings)

  # get all project notes in orgPath RECURSIVELY
  filePaths <- get_file_list_to_project_notes(
    orgPath, settings,
    pathExclusions = c(confPath, volPath, sitePath, journalPath) )
  # get all Project Docs/Notes inside path
  # they all contain "~_" in filename and end with .Rmd
  filePaths <- fs::path(unlist(filePaths))
  projDocsNotesList <- filePaths[ (regexpr(settings[["ProjectPrefixSep"]], filePaths) > 0) &
                              (regexpr(settings[["FileTypeSuffix"]], filePaths) > 0) ]


  # get all Project Docs/Notes inside path - they all contain "~_" in filename and end with .Rmd
  #projDocsNotesList <- list.files(path, pattern=paste0("*", settings[["FileTypeSuffix"]]), full.names = TRUE, recursive=TRUE)
  #projDocsNotesList <- projDocsNotesList[ (regexpr(settings[["ProjectPrefixSep"]], projDocsNotesList) > 0) & (regexpr(settings[["FileTypeSuffix"]], projDocsNotesList) > 0) ]

  projDocsNotesListLength <- length(projDocsNotesList) # for updateProgress

  # generate a blank table to initialise addin with
  FILENAME <- ""
  CONTEXT <- ""
  LOCATION <- "" # storing location but not adding it to the table
  LINE <- "" # storing line byt not adding to table

  if( ignoreCase == TRUE ) { # use tolower() as ignore.case doesnt work with fixed in grep
    # convert search term to lower OUTSIDE for loop
    searchTerm <- tolower(searchTerm)
  }

  for(s in projDocsNotesList) {

    # for each Rmd
    fn <-  basename(s)
    cat( "\n  reading file : ", fn, "\n" )

    # update progress of search
    if (is.function(updateProgress)) {
      progressFraction <- match(s, projDocsNotesList) / projDocsNotesListLength
      text <- paste0("Rmd file : ", fn )
      updateProgress(value = progressFraction, detail = text)
    }

    # open rmd
    rmd_file_conn <- file( s )
    rmd_contents <- readLines( rmd_file_conn )
    close(rmd_file_conn)

    # grep for search term
    if( ignoreCase == TRUE ) { # use tolower() as ignore.case doesnt work with fixed in grep
      grepRes <- grep(searchTerm, tolower(rmd_contents), fixed=fixed)
    } else {
      grepRes <- grep(searchTerm, rmd_contents, fixed=fixed)
    }
    #grepRes <- grep(searchTerm, rmd_contents, ignore.case = ignoreCase, fixed=fixed)

    # add results to searchResults table for each grep'd line
    for(g in grepRes) {
      FILENAME <- c(FILENAME, fn)
      CONTEXT <- c(CONTEXT, rmd_contents[g])
      LOCATION <- c(LOCATION, s)
      LINE <- c(LINE, g)
    } # end g

  } # end s

  # create table from vectors
  FILENAME <- FILENAME[2:length(FILENAME)] # trim first val - blank ""
  CONTEXT <- CONTEXT[2:length(CONTEXT)]
  LOCATION <- LOCATION[2:length(LOCATION)]
  LINE <- LINE[2:length(LINE)]

  # form table
  searchResults <- tibble::tibble(FILENAME, CONTEXT)

  # return list
  list(searchResults, list(LOCATION, LINE) )

}





#### ____ ####


#### Inverted Index Implementation ####

# tried to implement an inverted index for faster searches
# finally ChatGPT o3 has got basic solution working on a test case!



#' Trigram index utilities  (internal)
#'
#' Functions for building, caching and querying a 3‑character inverted index
#' of an organisation’s R Markdown files.  The main user entry point is
#' \code{\link{search_dir_tree}}.
#'
#' @name trigram_index
#' @keywords internal
NULL


#' Tokenise text into lower‑case trigrams
#'
#' @param txt Character vector (a document; may contain many lines).
#' @return    Character vector of 3‑character n‑grams.
#' @keywords internal
trigram_tokenizer <- function(txt) {
  txt <- paste(txt, collapse = " ")
  txt <- tolower(gsub("\\s+", " ", txt, perl = TRUE))
  if (nchar(txt) < 3) return(character())
  substring(txt, 1:(nchar(txt) - 2), 3:nchar(txt))
}

#' Make paths absolute relative to organisation root
#'
#' @param paths   Character vector of paths.
#' @param orgPath Organisation root.
#' @return Absolute, normalised paths.
#' @keywords internal
make_abs <- function(paths, orgPath) {
  rel <- !fs::is_absolute_path(paths)
  paths[rel] <- fs::path(orgPath, paths[rel])
  fs::path_norm(paths)
}

#' Build a sparse trigram DTM
#'
#' Reads each file into one long string, tokenises it **before** handing it to
#' \code{text2vec}; this guarantees exactly one row per file and prevents the
#' Dimnames mismatch error.
#'
#' @param files     Character vector of absolute file paths.
#' @param n_threads Threads for \code{text2vec::create_dtm()}.
#' @param verbose   Logical; print diagnostics?
#' @return          \code{dgCMatrix} with file paths as row names.
#' @keywords internal
build_trigram_dtm <- function(files, n_threads = 1, verbose = FALSE) {
  if (!requireNamespace("text2vec", quietly = TRUE))
    stop("Package 'text2vec' is required.")

  if (verbose) message("[DTM] building for ", length(files), " files")

  # Pre‑tokenise so every element is a list of n‑grams
  tokens <- lapply(files, function(p) {
    trigram_tokenizer(readLines(p, warn = FALSE))
  })

  names(tokens) <- files                       # will be the row names

  it <- text2vec::itoken(tokens,
                         ids         = files,
                         tokenizer   = identity,   # already tokenised
                         progressbar = FALSE)

  vocab <- text2vec::create_vocabulary(it, term_count_min = 1)
  vec   <- text2vec::vocab_vectorizer(vocab)

  text2vec::create_dtm(it, vec, type = "dgCMatrix",
                       n_threads = n_threads)
}


#' Load or refresh the cached trigram index
#'
#' @inheritParams search_dir_tree
#' @param n_threads Threads for DTM creation.
#' @param verbose   Logical; print diagnostics?
#' @return List with \code{dtm}, \code{files}, \code{mtime}, \code{version}.
#' @keywords internal
load_or_update_index <- function(orgPath, settings,
                                 n_threads = 1, verbose = FALSE) {

  CACHE_VERSION <- 7L

  confPath   <- get_config_dir(orgPath)
  cache_file <- fs::path(confPath, ".projectmanagr_trigram_index.rds")

  idx <- if (file.exists(cache_file)) readRDS(cache_file) else NULL
  if (!is.null(idx) && is.null(idx$version)) idx$version <- 0L

  files_now <- get_file_list_to_project_notes(
    orgPath, settings,
    pathExclusions = c(confPath,
                       get_volumes_dir(orgPath, settings),
                       get_site_dir(orgPath, settings),
                       get_journal_dir(orgPath, settings))) |>
    unlist() |>
    make_abs(orgPath)

  sel <- grepl(settings[["ProjectPrefixSep"]], files_now) &
    grepl(settings[["FileTypeSuffix"]],   files_now)
  files_now  <- files_now[sel]
  mtime_now  <- file.info(files_now, extra_cols = FALSE)$mtime

  if (is.null(idx)) idx <- list(dtm = NULL, files = character(),
                                mtime = as.POSIXct(character()),
                                version = 0L)

  new_or_changed <- is.na(match(files_now, idx$files)) |
    mtime_now > idx$mtime[match(files_now, idx$files)]
  deleted <- !idx$files %in% files_now

  rebuild <- idx$version < CACHE_VERSION ||
    any(new_or_changed) || any(deleted)

  if (verbose)
    message("[index] rebuild:", rebuild,
            " | new/changed:", sum(new_or_changed),
            " | deleted:", sum(deleted))

  if (rebuild) {
    dtm_new <- if (any(new_or_changed))
      build_trigram_dtm(files_now[new_or_changed],
                        n_threads, verbose) else NULL

    # drop deleted rows
    if (!is.null(idx$dtm) && any(deleted) && nrow(idx$dtm))
      idx$dtm <- idx$dtm[ !rownames(idx$dtm) %in% idx$files[deleted], , drop = FALSE]

    # merge
    if (!is.null(dtm_new)) {
      rn      <- rownames(dtm_new)
      idx$dtm <- if (is.null(idx$dtm)) dtm_new else
        rbind(idx$dtm[ setdiff(rownames(idx$dtm), rn), ], dtm_new)
    }

    idx$files   <- rownames(idx$dtm)
    idx$mtime   <- mtime_now[ match(idx$files, files_now) ]
    idx$version <- CACHE_VERSION
    saveRDS(idx, cache_file)
  }

  idx
}


#' Fast trigram search through a directory tree
#'
#' @param path Dir Tree Path for searching text files.
#'
#' @param searchTerm The sring to search for.
#'
#' @param updateProgress Object to show progress in shiny gadget.
#'
#' @param settings Settings list from projectmanagr config
#'
#' @param orgPath The organisation path where search takes place.
#'
#' @param ignoreCase Whether search should ignore case of searchTerm. TRUE by
#' default.
#'
#' @param fixed When fixed is TRUE, no regular expression patterns are parsed.
#' More efficient search is performed with fixed TRUE.
#'
#' @param verbose Logical; emit diagnostics?
#'
#' @return List: tibble of matches and list of locations/lines.
#'
#' @export
search_dir_tree_INDEX <- function(
    path,
    searchTerm,
    updateProgress = NULL,
    settings,
    orgPath,
    ignoreCase = TRUE,
    fixed = TRUE,
    n_threads  = 1,
    verbose = FALSE) {

  if (!R.utils::isAbsolutePath(path)) path <- R.utils::getAbsolutePath(path)
  path <- fs::path_norm(path)

  idx   <- load_or_update_index(orgPath, settings, n_threads, verbose)
  dtm   <- idx$dtm
  files <- idx$files

  if (ignoreCase) searchTerm <- tolower(searchTerm)
  grams <- trigram_tokenizer(searchTerm)

  # 1. directory filter ------------------------------------------------------
  candidate <- files[ fs::path_has_parent(files, path) ]

  # 2. trigram filter --------------------------------------------------------
  if (length(grams) && nrow(dtm)) {
    keep_cols <- intersect(grams, colnames(dtm))
    if (length(keep_cols))
      candidate <- candidate[
        Matrix::rowSums(dtm[candidate, keep_cols, drop = FALSE]) > 0]
  }

  # 3. exact grep ------------------------------------------------------------
  FILENAME <- CONTEXT <- LOCATION <- LINE <- character()

  for (file in candidate) {
    txt  <- readLines(file, warn = FALSE)
    hits <- grep(searchTerm,
                 if (ignoreCase) tolower(txt) else txt,
                 fixed = fixed)
    if (length(hits)) {
      FILENAME <- c(FILENAME, rep(basename(file), length(hits)))
      CONTEXT  <- c(CONTEXT, txt[hits])
      LOCATION <- c(LOCATION, rep(file, length(hits)))
      LINE     <- c(LINE, as.character(hits))
    }
    if (is.function(updateProgress))
      updateProgress(length(FILENAME), basename(file))
  }

  list(tibble::tibble(FILENAME, CONTEXT), list(LOCATION, LINE))
}
