




#' Extract TODO items from a project
#'
#' Reads all `.Rmd`/`.qmd` files under *location*, extracts the HTML‑comment
#' todo blocks, and renders a single R Markdown note that can be pasted into
#' your organiser.
#' This version recognises **inline priority tags** (`P1`, `P2`, …) and
#' **group tags** (`#planning`, `#release`, …), lets you rename the former
#' *priority‑first* layout to *grouping‑first*, and skips repeat todos that
#' share any group inside the same source file.
#'
#' @param location Directory containing the project.
#' @param date     The *reference* date (defaults to `get_date()`).
#' @param fromFilePath  Path to the calling file (used for relative links).
#' @param onlyFirstTodoPerFile Logical; keep just the first todo block in each
#'        file **regardless** of group.
#' @param sortByFileModTimeDesc Logical; when *note‑first* layout is used,
#'        newest‑touched files come first.
#' @param groupings Named integer vector giving the display‑order for the five
#'        schedule/deadline states.  (Was `priorities`; the old arg still works
#'        for backward compatibility.)
#' @param collectionTemplate,noteTemplate,itemTemplate
#'        Template filenames (looked up inside `get_template_dir()`).
#' @param groupingTemplate  Template used when `groupingFirst = TRUE`
#'        (was  `priorityTemplate`).
#' @param groupingFirst Logical; `TRUE` ⇒ headings are the *state/grouping*
#'        sections first, `FALSE` ⇒ headings are the source file names first.
#'
#' @return A character vector of R Markdown lines invisibly.  Internally the
#'         function writes the same text to the clipboard (`clipr`) for easy
#'         pasting.
#' @export
extract_todos <- function(
    location,
    date = get_date(),
    fromFilePath = NULL,
    onlyFirstTodoPerFile = FALSE,
    sortByFileModTimeDesc = TRUE,

    groupings = list(
      DEADLINE_PASSED = 4,
      SCHEDULE_PASSED = 3,
      DUE_TODAY       = 2,
      SCHEDULED_TODAY = 1,
      OPEN_ITEMS      = 0
    ),

    collectionTemplate = "Todo-Collection-Template.Rmd",
    noteTemplate       = "Todo-Note-Template.Rmd",
    itemTemplate       = "Todo-Item-Template.Rmd",

    groupingTemplate   = "Todo-Grouping-Template.Rmd",
    groupingFirst      = FALSE
) {
  cat("\nprojectmanagr::extract_todos():\n")

  priorities       <- groupings
  priorityTemplate <- groupingTemplate
  priorityFirst    <- groupingFirst

  orgPath  <- confirm_find_org(location)
  tempPath <- get_template_dir(orgPath)
  settings <- get_settings_yml(orgPath)

  files <- get_projectmanagr_files(location, orgPath, settings)

  collectionLines <- read_file(fs::path(tempPath, collectionTemplate))
  noteLines       <- read_file(fs::path(tempPath, noteTemplate))
  itemLines       <- read_file(fs::path(tempPath, itemTemplate))

  prioritySectionLines <- NULL
  if (priorityFirst) {
    prioritySectionLines <- read_file(fs::path(tempPath, priorityTemplate))
  }

  if (is.null(fromFilePath)) {
    fromFilePath <- location
  }

  allTodos <- todo_data_str()
  for (fl in files) {
    todos    <- parse_todo_blocks_with_heading(fl)
    allTodos <- write_todo_data_str(
      fl, todos, allTodos,
      onlyFirstTodoPerFile = onlyFirstTodoPerFile,
      today                = date
    )
  }

  if (nrow(allTodos) == 0) {
    return(
      no_todo_items_found(allTodos, collectionLines, orgPath, date, location)
    )
  }

  if (length(priorities) != 5) {
    stop(
      "The 'groupings' list must contain exactly 5 elements ",
      "(DEADLINE_PASSED…OPEN_ITEMS)."
    )
  }
  invMap <- setNames(names(priorities), as.character(priorities))

  allTodos$priority     <- 0L
  allTodos$itemDateTime <- NA_real_

  for (r in seq_len(nrow(allTodos))) {
    rowPriority <- priorities[["OPEN_ITEMS"]]

    if (allTodos$isDeadlinePassed[r] == 1)
      rowPriority <- max(rowPriority, priorities[["DEADLINE_PASSED"]])
    if (allTodos$isSchedulePassed[r] == 1)
      rowPriority <- max(rowPriority, priorities[["SCHEDULE_PASSED"]])
    if (allTodos$isDueToday[r] == 1)
      rowPriority <- max(rowPriority, priorities[["DUE_TODAY"]])
    if (allTodos$isScheduledToday[r] == 1)
      rowPriority <- max(rowPriority, priorities[["SCHEDULED_TODAY"]])

    allTodos$priority[r] <- rowPriority

    dtSch  <- parse_schedule_datetime(allTodos$schedule[r])
    dtDead <- parse_deadline_datetime(allTodos$deadline[r])
    if (!is.na(dtSch) && !is.na(dtDead)) {
      allTodos$itemDateTime[r] <- min(dtSch, dtDead)
    } else if (!is.na(dtSch)) {
      allTodos$itemDateTime[r] <- dtSch
    } else if (!is.na(dtDead)) {
      allTodos$itemDateTime[r] <- dtDead
    }
  }

  finalDoc <- if (priorityFirst) {
    build_priority_first_layout(
      allTodos, invMap,
      itemLines, noteLines, prioritySectionLines,
      fromFilePath, orgPath, date, location, collectionLines
    )
  } else {
    build_note_first_layout(
      allTodos, sortByFileModTimeDesc,
      itemLines, noteLines,
      fromFilePath, orgPath, date, location, collectionLines
    )
  }

  replace_sep_values(finalDoc, orgPath)
}


#' Empty TODO data frame with the correct columns
#'
#' Helper that always returns a data.frame with the  *full* schema the
#' extractor uses, now including **`pTagPriority`** and **`groups`**.
#'
#' @inheritParams todo_data_str
#' @return Zero‑row data frame.
#' @keywords internal
todo_data_str <- function(
    file             = character(),
    fileMtime        = as.POSIXct(character()),
    indexInFile      = integer(),
    heading          = character(),
    title            = character(),
    text             = character(),
    deadline         = character(),
    schedule         = character(),
    isScheduledToday = integer(),
    isSchedulePassed = integer(),
    isDueToday       = integer(),
    isDeadlinePassed = integer(),
    itemDateTime     = numeric(),
    pTagPriority     = integer(),
    groups           = character()
) {
  data.frame(
    file, fileMtime, indexInFile, heading, title, text,
    deadline, schedule,
    isScheduledToday, isSchedulePassed, isDueToday, isDeadlinePassed,
    itemDateTime,
    pTagPriority,
    groups,
    stringsAsFactors = FALSE
  )
}


#' Parse a single HTML‑comment todo block
#'
#' Splits out title, body text, schedule/deadline strings, **inline priority**
#' (`P<n>`) and an arbitrary number of *group* tags (`#group‑name`).
#'
#' @param blockLines Character vector – the lines between the opening
#'        `<!--` and closing `-->`.
#' @return A named list with elements:
#'   \describe{
#'     \item{text}{Full body text (no schedule/deadline lines).}
#'     \item{title}{Inline title after the closing brace.}
#'     \item{deadline,schedule}{Raw strings, possibly empty.}
#'     \item{pTagPriority}{`NA` or integer priority extracted from `P<n>`.}
#'     \item{groups}{`character()` vector of group names (may be empty).}
#'   }
#' @keywords internal
parse_one_todo_block <- function(blockLines) {
  if (!length(blockLines)) return(NULL)

  # Strip any trailing "-->"
  lastLineIdx <- length(blockLines)
  if (grepl("-->", blockLines[lastLineIdx])) {
    blockLines[lastLineIdx] <- sub("-->.*$", "", blockLines[lastLineIdx])
  }

  firstLine <- blockLines[1]
  if (grepl("<!--", firstLine)) firstLine <- sub("<!--", "", firstLine)

  # Inside the braces: priority tag(s) + group tag(s)
  inside <- sub("^.*\\{#todo\\s*", "", firstLine)
  inside <- sub("\\}.*$", "", inside)
  insideParts <- strsplit(trimws(inside), "\\s+")[[1]]

  pTagPriority <- NA_integer_
  groups       <- character()

  if (length(insideParts)) {
    for (pt in insideParts) {
      if (grepl("^P\\d+$", pt)) {
        pTagPriority <- as.integer(sub("^P", "", pt))
      } else if (grepl("^#.+", pt)) {
        groups <- c(groups, sub("^#", "", pt))
      }
    }
  }

  # Everything after the closing brace → inline title (optional)
  firstLine <- sub("^.*\\}\\s*", "", firstLine)
  firstLine <- trimws(firstLine)
  todoTitle <- if (nzchar(firstLine)) firstLine else ""

  deadlineVal <- ""
  scheduleVal <- ""
  textLines   <- character()

  for (ln in seq_along(blockLines)[-1]) {
    ltxt <- blockLines[ln]
    if (grepl("\\{#deadline", ltxt)) {
      val <- sub(".*\\{#deadline\\s+", "", ltxt)
      val <- sub("\\}.*", "", val)
      deadlineVal <- val
    } else if (grepl("\\{#schedule", ltxt)) {
      val <- sub(".*\\{#schedule\\s+", "", ltxt)
      val <- sub("\\}.*", "", val)
      scheduleVal <- val
    } else {
      textLines <- c(textLines, ltxt)
    }
  }

  list(
    text         = paste(textLines, collapse = "\n"),
    title        = todoTitle,
    deadline     = deadlineVal,
    schedule     = scheduleVal,
    pTagPriority = pTagPriority,
    groups       = groups
  )
}






#' Append parsed todos from one file to the master data.frame
#'
#' Applies date‑status flags, **drops any todo whose groups were already seen
#' earlier in the same file**, and fills the two new columns (`pTagPriority`,
#' `groups`).
#'
#' @param fl Path to the source file being processed.
#' @param todos List of list objects (each from `parse_one_todo_block()`).
#' @param allTodos The cumulative data.frame built so far.
#' @param onlyFirstTodoPerFile Logical flag forwarded from `extract_todos()`.
#' @param today  Date object forwarded from `extract_todos()`.
#' @return Updated data.frame.
#' @keywords internal
write_todo_data_str <- function(fl, todos, allTodos,
                                onlyFirstTodoPerFile, today) {

  if (length(todos) == 0) return(allTodos)
  modTime   <- file.info(fl)$mtime
  itemIndex <- 1
  seenGroups <- character()

  for (tdi in seq_along(todos)) {
    if (onlyFirstTodoPerFile && tdi > 1) break
    td <- todos[[tdi]]

    # Skip if any of this item's groups were already encountered in this file
    if (length(td$groups) &&
        length(intersect(td$groups, seenGroups)) > 0) next
    seenGroups <- union(seenGroups, td$groups)

    flags <- interpret_todo_flags(td$schedule, td$deadline, today)

    allTodos <- rbind(
      allTodos,
      todo_data_str(
        file             = fs::path_expand(fl),
        fileMtime        = modTime,
        indexInFile      = itemIndex,
        heading          = td$heading,
        title            = td$title,
        text             = td$text,
        deadline         = td$deadline,
        schedule         = td$schedule,
        isScheduledToday = flags$isScheduledToday,
        isSchedulePassed = flags$isSchedulePassed,
        isDueToday       = flags$isDueToday,
        isDeadlinePassed = flags$isDeadlinePassed,
        itemDateTime     = NA_real_,
        pTagPriority     = ifelse(is.na(td$pTagPriority), 99L, td$pTagPriority),
        groups           = paste(td$groups, collapse = ";")
      )
    )
    itemIndex <- itemIndex + 1
  }

  allTodos
}



#' Parse an Rmd file for HTML comment TODO blocks, capturing headings
#'
#' Searches for lines matching the pattern \strong{exactly at the start of a line}:
#' \code{^<!--\\s*\\{#todo\\}}. It then collects subsequent lines until
#' encountering \code{-->} (end of comment). It also tracks the nearest markdown
#' heading (\code{"## My Heading"}) encountered before the TODO block.
#'
#' If an inline title appears on the same line as \code{{#todo}}, it is captured
#' (e.g. `<!-- {#todo} My Title`).
#'
#' @param filePath The path to the Rmd file to parse.
#'
#' @return A list of lists, each containing \code{text}, \code{deadline},
#'   \code{schedule}, \code{title}, and \code{heading}.
#' @keywords internal
#' Parse an Rmd file for HTML comment TODO blocks, capturing headings
#' (updated: recognises "{#todo P<n> #group ...}" as well)
#' @keywords internal
parse_todo_blocks_with_heading <- function(filePath) {
  contents <- read_file(filePath)
  inBlock      <- FALSE
  blockLines   <- character()
  results      <- list()
  currentHeading <- ""

  for (ln in seq_along(contents)) {
    line <- contents[ln]

    # record the most recent markdown heading
    if (!inBlock && grepl("^#+\\s+", line)) {
      currentHeading <- trimws(sub("^#+", "", line))
    }

    # ── detect start of a TODO block (allow extra text inside {#todo …}) ──
    if (!inBlock) {
      if (grepl("^<!--\\s*\\{#todo\\b", line)) {   # ← relaxed pattern
        inBlock    <- TRUE
        blockLines <- c(line)
      }
      next
    }

    # already inside a block – collect lines until closing comment
    blockLines <- c(blockLines, line)
    if (grepl("-->", line)) {
      parsed <- parse_one_todo_block(blockLines)
      if (!is.null(parsed)) {
        parsed$heading <- currentHeading
        results[[length(results) + 1]] <- parsed
      }
      inBlock    <- FALSE
      blockLines <- character()
    }
  }

  results
}





#' Interpret schedule/deadline => set flags (passed, due, etc.)
#'
#' @param schedule A string in the form \code{"YYYY-MM-DD:HHmm"} if scheduling is set.
#' @param deadline A string in the form \code{"YYYY-MM-DD"} if there's a deadline.
#' @param today A string representing "today," e.g. "2025-04-01."
#'
#' @return A list with \code{isScheduledToday}, \code{isSchedulePassed},
#'   \code{isDueToday}, \code{isDeadlinePassed}.
#' @keywords internal
interpret_todo_flags <- function(schedule, deadline, today) {
  dtToday <- safe_as_date(today)
  isScheduledToday  <- 0
  isSchedulePassed  <- 0
  isDueToday        <- 0
  isDeadlinePassed  <- 0

  scDateTime <- parse_schedule_datetime(schedule)
  ddDateTime <- parse_deadline_datetime(deadline)

  # schedule passed if scDateTime < today's 00:00
  # scheduled today if as.Date(scDateTime) == dtToday
  if (!is.na(scDateTime) && !is.na(dtToday)) {
    if (scDateTime < as.POSIXct(paste0(dtToday," 00:00"))) {
      isSchedulePassed <- 1
    } else if (as.Date(scDateTime) == dtToday) {
      isScheduledToday <- 1
    }
  }

  # deadline passed if ddDateTime < today's 00:00
  # due today if ddDateTime == today's 00:00
  if (!is.na(ddDateTime) && !is.na(dtToday)) {
    if (ddDateTime < as.POSIXct(paste0(dtToday," 00:00"))) {
      isDeadlinePassed <- 1
    } else if (ddDateTime == as.POSIXct(paste0(dtToday," 00:00"))) {
      isDueToday <- 1
    }
  }

  list(
    isScheduledToday = isScheduledToday,
    isSchedulePassed = isSchedulePassed,
    isDueToday       = isDueToday,
    isDeadlinePassed = isDeadlinePassed
  )
}



#' Construct minimal doc if no TODO items found
#'
#' @param allTodos The existing data frame of all collected TODO items (empty).
#' @param collectionLines The lines from \code{collectionTemplate}.
#' @param orgPath The base path for the project.
#' @param date The user-supplied "today" string.
#' @param location The original location passed in, used to derive a scope label.
#'
#' @return The lines from \code{collectionTemplate} with placeholders for notes
#'   replaced by empty strings.
#' @keywords internal
no_todo_items_found <- function(allTodos, collectionLines, orgPath, date, location) {
  scope <- derive_scope_string(location)
  collectionLines <- sub_template_param(collectionLines, "{{ALL_NOTES_SECTION}}", "", orgPath)
  collectionLines <- sub_template_param(collectionLines, "{{DATE}}", date, orgPath)
  collectionLines <- sub_template_param(collectionLines, "{{SCOPE}}", scope, orgPath)
  collectionLines
}


#' Parse a schedule string (YYYY-MM-DD:HHmm) into a POSIXct
#' @keywords internal
parse_schedule_datetime <- function(scheduleStr) {
  if (!nzchar(scheduleStr)) return(NA)
  parts <- strsplit(scheduleStr, ":")[[1]]
  if (length(parts) < 2) return(NA)
  ymd  <- parts[1]
  hhmm <- parts[2]
  if (nchar(hhmm) < 3) return(NA) # minimal "HMM"

  hour <- substr(hhmm, 1, nchar(hhmm)-2)
  mins <- substr(hhmm, nchar(hhmm)-1, nchar(hhmm))

  dateTimeStr <- paste0(ymd, " ", hour, ":", mins, ":00")
  tryCatch(as.POSIXct(dateTimeStr), error=function(e) NA)
}


#' Parse a deadline string (YYYY-MM-DD) => interpret as 00:00 that day
#' @keywords internal
parse_deadline_datetime <- function(deadlineStr) {
  if (!nzchar(deadlineStr)) return(NA)
  dateTimeStr <- paste0(deadlineStr, " 00:00:00")
  tryCatch(as.POSIXct(dateTimeStr), error=function(e) NA)
}


#' Safely convert a string like "2025-04-01" to Date
#' @keywords internal
safe_as_date <- function(x) {
  tryCatch(as.Date(x), error=function(e) NA)
}


#' Derive a short scope label from a file/directory location
#' @keywords internal
derive_scope_string <- function(location) {
  nm <- fs::path_file(location)
  if (endsWith(tolower(nm), ".rmd")) {
    nm <- sub("\\.rmd$", "", nm, ignore.case=TRUE)
  }
  nm
}

#' Return TODO description verbatim (legacy shim)
#'
#' Older versions converted each line into Markdown bullets.  The extractor
#' now leaves the description exactly as written, so this helper simply
#' returns the input followed by two line‑breaks.
#'
#' @param txt The raw multiline string from the parsed TODO block.
#' @return A single character string.
#' @keywords internal
format_todo_description <- function(txt) {
  paste0(txt, "\n\n")
}




#' Build text/deadline/schedule fields for a single TODO
#'
#' Copies the description exactly as written in the Rmd and appends blank
#' lines so the next template chunk starts on a new paragraph.  Schedule and
#' deadline strings are still indented for clarity.
#'
#' @param row One row of the allTodos data frame (i.e. one TODO).
#'
#' @return A list with \code{$TODO_TEXT}, \code{$TODO_DEADLINE},
#'   \code{$TODO_SCHEDULE}.
#' @keywords internal
build_item_fields <- function(row) {

  ## --- verbatim description -------------------------------------------------
  textStr <- paste0(row$text, "\n\n")   # keep line breaks as‑is

  ## --- optional deadline / schedule lines ----------------------------------
  deadlineStr <- ""
  if (nzchar(row$deadline)) {
    deadlineStr <- paste0("  (DEADLINE: ", row$deadline, ")\n\n")
  }

  scheduleStr <- ""
  if (nzchar(row$schedule)) {
    scheduleStr <- paste0("  (SCHEDULE: ", row$schedule, ")\n\n")
  }

  list(
    TODO_TEXT     = textStr,
    TODO_DEADLINE = deadlineStr,
    TODO_SCHEDULE = scheduleStr
  )
}


build_note_first_layout <- function(allTodos,
                                    sortByFileModTimeDesc,
                                    itemLines,
                                    noteLines,
                                    fromFilePath,
                                    orgPath,
                                    date,
                                    location,
                                    collectionLines) {

  fileModTime   <- sapply(unique(allTodos$file),
                          function(ff) file.info(ff)$mtime)
  distinctFiles <- names(sort(fileModTime,
                              decreasing = sortByFileModTimeDesc))

  finalNoteChunks <- character()

  for (fl in distinctFiles) {
    subdf <- allTodos[allTodos$file == fl, , drop = FALSE]
    if (nrow(subdf) == 0) next

    subdf <- subdf[order(
      -subdf$priority,                     # “grouping” first
      as.integer(subdf$pTagPriority),      # then P‑tag
      subdf$indexInFile                    # then appearance order
    ), , drop = FALSE]

    baseName <- fs::path_file(fl)
    noteLink <- create_hyperlink_section(
      toFileName    = baseName,
      toFileSection = "",
      toFilePath    = fl,
      fromFilePath  = fromFilePath
    )

    noteBlock <- noteLines
    noteBlock <- sub_template_param(noteBlock, "{{NOTE_BASENAME}}",
                                    baseName, orgPath)
    noteBlock <- sub_template_param(noteBlock, "{{NOTE_LINK}}",
                                    noteLink, orgPath)

    itemExpansions <- character()
    for (rowi in seq_len(nrow(subdf))) {
      row  <- subdf[rowi, ]
      line <- itemLines

      itemHeader <- if (nzchar(row$title)) row$title else
        if (nzchar(row$heading)) row$heading else "top"

      itemLink <- create_hyperlink_section(
        toFileName    = baseName,
        toFileSection = row$heading,
        toFilePath    = row$file,
        fromFilePath  = fromFilePath
      )

      line <- sub_template_param(line, "{{TODO_ITEM_LINK}}",
                                 itemLink,   orgPath)

      prefixParts <- character()
      if (row$isDeadlinePassed == 1) prefixParts <- c(prefixParts,"DEADLINE_PASSED")
      if (row$isSchedulePassed == 1) prefixParts <- c(prefixParts,"SCHEDULE_PASSED")
      if (row$isDueToday       == 1) prefixParts <- c(prefixParts,"DUE_TODAY")
      if (row$isScheduledToday == 1) prefixParts <- c(prefixParts,"SCHEDULED_TODAY")
      if (length(prefixParts)  == 0) prefixParts <- "OPEN_ITEMS"

      line <- sub_template_param(line, "{{TODO_ITEM_HEADER}}",
                                 paste(c(prefixParts,itemHeader), collapse=" "),
                                 orgPath)

      flds <- build_item_fields(row)
      line <- sub_template_param(line, "{{TODO_TEXT}}",
                                 flds$TODO_TEXT,     orgPath)
      line <- sub_template_param(line, "{{TODO_DEADLINE}}",
                                 flds$TODO_DEADLINE, orgPath)
      line <- sub_template_param(line, "{{TODO_SCHEDULE}}",
                                 flds$TODO_SCHEDULE, orgPath)

      itemExpansions <- c(itemExpansions, line)
    }

    noteBlock <- sub_template_param(noteBlock, "{{NOTE_TODO_ITEMS}}",
                                    itemExpansions, orgPath)
    finalNoteChunks <- c(finalNoteChunks, noteBlock)
  }

  collectionLines <- sub_template_param(collectionLines,"{{ALL_NOTES_SECTION}}",
                                        finalNoteChunks, orgPath)
  collectionLines <- sub_template_param(collectionLines,"{{DATE}}",
                                        date,            orgPath)
  collectionLines <- sub_template_param(collectionLines,"{{SCOPE}}",
                                        derive_scope_string(location), orgPath)
  collectionLines
}

build_priority_first_layout <- function(allTodos,
                                        invMap,
                                        itemLines,
                                        noteLines,
                                        prioritySectionLines,
                                        fromFilePath,
                                        orgPath,
                                        date,
                                        location,
                                        collectionLines) {

  usedStates  <- sort(unique(allTodos$priority), decreasing = TRUE)
  finalBlocks <- character()

  fileInfo      <- file.info(unique(allTodos$file))
  distinctFiles <- rownames(fileInfo)[order(fileInfo$mtime, decreasing = TRUE)]

  for (st in usedStates) {
    stateDF <- allTodos[allTodos$priority == st, , drop = FALSE]
    if (nrow(stateDF) == 0) next

    stateName   <- invMap[as.character(st)]
    stateChunks <- character()

    for (fl in distinctFiles) {
      subdf <- stateDF[stateDF$file == fl, , drop = FALSE]
      if (nrow(subdf) == 0) next

      subdf <- subdf[order(
        as.integer(subdf$pTagPriority),
        subdf$indexInFile
      ), , drop = FALSE]

      baseName <- fs::path_file(fl)
      noteLink <- create_hyperlink(
        toFileName   = baseName,
        toFilePath   = fl,
        fromFilePath = fromFilePath
      )

      noteBlock <- noteLines
      noteBlock <- sub_template_param(noteBlock, "{{NOTE_BASENAME}}",
                                      baseName, orgPath)
      noteBlock <- sub_template_param(noteBlock, "{{NOTE_LINK}}",
                                      noteLink, orgPath)

      itemChunks <- character()
      for (i in seq_len(nrow(subdf))) {
        row   <- subdf[i, ]
        chunk <- itemLines

        hdr <- if (nzchar(row$title)) row$title else
          if (nzchar(row$heading)) row$heading else "top"

        itemLink <- create_hyperlink_section(
          toFileName    = baseName,
          toFileSection = row$heading,
          toFilePath    = row$file,
          fromFilePath  = fromFilePath
        )

        chunk <- sub_template_param(chunk, "{{TODO_ITEM_LINK}}",
                                    itemLink, orgPath)
        chunk <- sub_template_param(chunk, "{{TODO_ITEM_HEADER}}",
                                    hdr,      orgPath)

        flds  <- build_item_fields(row)
        chunk <- sub_template_param(chunk, "{{TODO_TEXT}}",
                                    flds$TODO_TEXT,     orgPath)
        chunk <- sub_template_param(chunk, "{{TODO_DEADLINE}}",
                                    flds$TODO_DEADLINE, orgPath)
        chunk <- sub_template_param(chunk, "{{TODO_SCHEDULE}}",
                                    flds$TODO_SCHEDULE, orgPath)

        itemChunks <- c(itemChunks, chunk)
      }

      noteBlock   <- sub_template_param(noteBlock, "{{NOTE_TODO_ITEMS}}",
                                        itemChunks, orgPath)
      stateChunks <- c(stateChunks, noteBlock, "")
    }

    sec <- prioritySectionLines
    sec <- sub_template_param(sec, "{{PRIORITY_HEADER}}", stateName,     orgPath)
    sec <- sub_template_param(sec, "{{SEP2}}",            "",            orgPath)
    sec <- sub_template_param(sec, "{{PRIORITY_ITEMS}}",  stateChunks,   orgPath)

    finalBlocks <- c(finalBlocks, sec)
  }

  out <- sub_template_param(collectionLines, "{{ALL_NOTES_SECTION}}",
                            finalBlocks, orgPath)
  out <- sub_template_param(out, "{{DATE}}",  date, orgPath)
  out <- sub_template_param(out, "{{SCOPE}}",
                            derive_scope_string(location), orgPath)
  out
}



 #### ____________ ####


#' Extract events from Google Calendar for one day
#'
#' This convenience wrapper\
#' • Returns extracted google calendar events from `extract_google_calendar_events()`
#'   as a block of markdown text, for direct insertion into a Rmd file
#'
#' @param day [`Date`][base::Date] Date to query (default: `Sys.Date()`).
#' @param settings `list` Named options; currently only
#'        `googleCalendars = c("Calendar A", "Calendar B")`
#'        to filter the calendar list.
#'
#' @return A [tibble][tibble::tibble] with the columns\
#'   `calendar`, `summary`, `start`, `end`, `description`, `htmlLink`,
#'   `location`.
#' @export
extract_google_calendar_events_markdown <- function(
    day = Sys.Date(),
    location = getwd(),
    settings = list(),
    calEventsTemplate = "Calendar-Events-Template.Rmd",
    extract_google_calendar_events_fun = extract_google_calendar_events
) {

  cat("\nprojectmanagr::extract_google_calendar_events_markdown():\n")

  # Set up environment and read user templates
  orgPath  <- confirm_find_org(location)
  tempPath <- get_template_dir(orgPath)
  if( is.list(settings) && length(settings) == 0 ) { # fetch only if settings is blank list
    settings <- get_settings_yml(orgPath)
  }

  # Read top-level template
  calEventsLines <- read_file(fs::path(tempPath, calEventsTemplate))

  # get events
  events_table <- extract_google_calendar_events_fun(day = day, settings = settings)
  # testing:
  #events_table <- extract_google_calendar_events_fun(day = lubridate::as_date("2025/04/30"))

  message("  forming markdown text block")

  tpl <- calEventsLines
  events <- events_table

  # Sort events: all-day events first, then by start time ascending
  events <- events |>
    dplyr::mutate(
      is_all_day = !grepl("T", start),
      start_time = ifelse(
        is_all_day,
        NA,
        as.character(lubridate::ymd_hms(start, quiet = TRUE))
      )
    ) |>
    dplyr::arrange(dplyr::desc(is_all_day), start_time)

  message("  forming markdown text block")

  replace_brace <- function(text, key, value) {
    gsub(sprintf("\\{\\{%s\\}\\}", key), value, text, fixed = FALSE)
  }

  out <- unlist(lapply(seq_len(nrow(events)), function(i) {

    ev  <- events[i, ]
    blk <- tpl                      # fresh copy for each event

    blk <- replace_brace(blk, "EVENT_TITLE",  ev$summary)
    blk <- replace_brace(blk, "EVENT_CALENDAR", ev$calendar)

    pretty_time <- function(x) {
      sub(".*T(\\d{2}:\\d{2}).*", "\\1", x)
    }
    blk <- replace_brace(blk, "EVENT_START_TIME", pretty_time(ev$start))
    blk <- replace_brace(blk, "EVENT_END_TIME",   pretty_time(ev$end))

    link <- ifelse(is.na(ev$htmlLink), "", ev$htmlLink)
    blk  <- replace_brace(blk, "EVENT_HTML_LINK", link)

    loc  <- ifelse(is.na(ev$location), "", paste0(" : ", ev$location))
    blk  <- replace_brace(blk, "EVENT_LOCATION_BLOCK", loc)

    desc <- clean_zoom_description(ev$description)
    blk  <- replace_brace(blk, "EVENT_DESCRIPTION_BLOCK", desc)

    blk[!grepl("\\{\\{[^}]*\\}\\}", blk)]
  }))

  c(out, "")        # ensure trailing line-break for a nice paste

}


clean_zoom_description <- function(desc) {
  if (is.na(desc) || !nzchar(desc)) return("")

  # 1. Extract Zoom link from <a href="..."> *before* removing tags
  zoom_pat <- "(?i)https?://[^\\s\"'>]*zoom\\.us[^\\s\"'>]*"
  url <- stringr::str_extract(desc, zoom_pat)
  if (!is.na(url) && nzchar(url)) {
    return(sprintf("[Zoom link](%s)", trimws(url)))
  }

  # 2. Replace <br> tags with newlines
  desc <- gsub("(?i)<br\\s*/?>", "\n", desc, perl = TRUE)

  # 3. Strip remaining HTML tags
  desc <- gsub("<[^>]+>", " ", desc, perl = TRUE)

  # 4. Normalize whitespace but preserve newlines
  desc <- gsub("\n", "___NEWLINE___", desc, fixed = TRUE)
  desc <- gsub("[ \t]+", " ", desc)
  desc <- gsub("___NEWLINE___", "\n", desc, fixed = TRUE)

  # 5. Split by lines and wrap each
  paragraphs <- unlist(strsplit(desc, "\n"))
  wrapped <- lapply(paragraphs, function(line) {
    line <- trimws(line)
    if (nchar(line) == 0) return("")
    words <- strsplit(line, " ")[[1]]
    out <- ""
    current_line <- ""
    for (word in words) {
      test_line <- paste0(current_line, if (nzchar(current_line)) " ", word)
      if (nchar(test_line) > 80) {
        out <- paste0(out, current_line, "\n")
        current_line <- word
      } else {
        current_line <- test_line
      }
    }
    paste0(out, current_line)
  })

  paste(unlist(wrapped), collapse = "\n")
}



#' Extract events from Google Calendar for one day
#'
#' This convenience wrapper\
#' • **authenticates** with Google Calendar via the builtin
#'   [default_google_auth()] helper;\
#' • retrieves the user’s **calendar list** (or a subset supplied in
#'   `settings$googleCalendars`);\
#' • fetches **all events** that start _or_ end on the requested `day`
#'   (UTC, exclusive upper bound) and\
#' • returns the result as a tidy tibble with the most commonly used
#'   columns.
#'
#' Every heavy-lifting call (auth, calendar list, events) is injected as a
#' function argument so the behaviour can be **unit-tested or mocked**
#' without hitting the network.
#'
#' @section Time handling:
#' Google returns either `date` (all-day) **or** `dateTime`
#' (time-specific) fields.
#' The helper collapses those into the single character columns
#' `start` / `end`, preserving the original RFC 3339 string when present.
#'
#' @param day [`Date`][base::Date] Date to query (default: `Sys.Date()`).
#' @param settings `list` Named options; currently only
#'        `googleCalendars = c("Calendar A", "Calendar B")`
#'        to filter the calendar list.
#' @param auth_fn            Function that performs authentication
#'        (default: [default_google_auth()]).
#' @param calendar_list_fn   Function that returns the calendar list
#'        as a two-column tibble (`id`, `summary`).
#' @param events_fn          Function that fetches one calendar’s
#'        events; must return the parsed JSON list produced by
#'        googleAuthR/Calendar v3.
#'
#' @return A [tibble][tibble::tibble] with the columns\
#'   `calendar`, `summary`, `start`, `end`, `description`, `htmlLink`,
#'   `location`.
#' @export
extract_google_calendar_events <- function(
    day = Sys.Date(),
    settings = list(),
    auth_fn = default_google_auth,
    calendar_list_fn = default_calendar_list_fn,
    events_fn = default_events_list_fn
) {

  cat("\nprojectmanagr::extract_google_calendar_events():\n")

  options(
    gargle_oauth_cache    = rappdirs::user_cache_dir("projectmanagr"),
    gargle_oob_default    = FALSE # allow token to be passed via local web server
  )

  # Authenticate
  auth_fn()

  # Time window
  t_min <- as.POSIXct(day, tz = "UTC")
  t_max <- t_min + lubridate::days(1)

  # Retrieve calendars
  cal_df <- calendar_list_fn()

  # Filter calendars
  if (!is.null(settings$googleCalendars)) {
    cal_df <- dplyr::filter(cal_df, summary %in% settings$googleCalendars)
  }

  # Loop and fetch events
  purrr::map_dfr(seq_len(nrow(cal_df)), function(i) {
    cal_id <- cal_df$id[i]
    cal_name <- cal_df$summary[i]
    ev <- events_fn(cal_id, t_min, t_max)

    if (length(ev$items) == 0) {
      return(tibble::tibble(
        calendar = character(0),
        summary = character(0),
        start = character(0),
        end = character(0),
        description = character(0),
        htmlLink = character(0),
        location = character(0)
      ))
    }

    #ev_wide <- purrr::map_dfr(ev$items, tibble::as_tibble) |>
    #  tidyr::unnest_wider(start, names_sep = ".") |>
    #  tidyr::unnest_wider(end,   names_sep = ".")
    ev_wide <- tibble::as_tibble(ev$items) |>
      tidyr::unnest_wider(start, names_sep = ".") |>
      tidyr::unnest_wider(end,   names_sep = ".")

    has_start_datetime <- "start.dateTime" %in% names(ev_wide)
    has_start_date     <- "start.date"     %in% names(ev_wide)
    has_end_datetime   <- "end.dateTime"   %in% names(ev_wide)
    has_end_date       <- "end.date"       %in% names(ev_wide)
    has_location       <- "location"       %in% names(ev_wide)
    has_description    <- "description"    %in% names(ev_wide)
    has_htmlLink       <- "htmlLink"       %in% names(ev_wide)

    ev_wide |>
      dplyr::mutate(
        start = dplyr::coalesce(
          if (has_start_datetime) .data[["start.dateTime"]] else NULL,
          if (has_start_date)     .data[["start.date"]]     else NULL
        ),
        end = dplyr::coalesce(
          if (has_end_datetime) .data[["end.dateTime"]] else NULL,
          if (has_end_date)     .data[["end.date"]]     else NULL
        )
      ) |>
      dplyr::transmute(
        calendar = cal_name,
        summary,
        start,
        end,
        description = if (has_description) description else NA_character_,
        htmlLink    = if (has_htmlLink) htmlLink else NA_character_,
        location    = if (has_location) location else NA_character_
      )
  })
}


#' Authenticate once and cache the Google Calendar token
#'
#' The helper expects a *desktop-app* OAuth client file called
#' `gcal_oauth_client.json` inside the user-level `rappdirs`
#' **config** directory for the package:
#'
#' ```
#' rappdirs::user_config_dir("projectmanagr")/
#'   gcal_oauth_client.json
#' ```
#'
#' It performs a minimal structural check, registers the client with
#' **googleAuthR**, then calls `googleAuthR::gar_auth()` ─ the token is
#' cached under `rappdirs::user_cache_dir("projectmanagr")`.
#'
#' @inheritParams extract_google_calendar_events
#' @param gar_set_client,gar_auth  Dependency-injected versions of
#'        [googleAuthR::gar_set_client()] / [googleAuthR::gar_auth()]
#'        (facilitates test stubs).
#'
#' @return (Invisibly) the object returned by `gar_auth()`.
#' @family authentication helpers
#' @export
default_google_auth <- function(
    scopes      = "https://www.googleapis.com/auth/calendar.readonly",
    cfg_name    = "gcal_oauth_client.json",
    gar_set_client = googleAuthR::gar_set_client,
    gar_auth      = googleAuthR::gar_auth
) {

  cfg_dir  <- rappdirs::user_config_dir("projectmanagr")
  json_path <- file.path(cfg_dir, cfg_name)

  if (!file.exists(json_path))
    stop(
      "No OAuth client JSON found at:\n  ", json_path,
      "\n\nCreate a Desktop-App credential in Google Cloud Console, ",
      "download the JSON, and place it there."
    )

  # quick structural check
  j <- jsonlite::fromJSON(json_path)
  must <- c("client_id", "client_secret", "redirect_uris")
  if (!all(must %in% names(j$installed)))
    stop("JSON is missing one of: ", paste(must, collapse = ", "))

  gar_set_client(json = json_path, scopes = scopes)

  invisible(gar_auth(email = TRUE, use_oob = FALSE))
}


#' Retrieve Google Calendar List
#'
#' This function defines and calls a default API request to retrieve the list of
#' calendars associated with the authenticated Google account.
#'
#' Internally, it uses `googleAuthR::gar_api_generator()` to generate a wrapper for the
#' `calendarList.list` endpoint of the Google Calendar API. The function returns a tibble
#' with the calendar ID and its human-readable summary (name).
#'
#' This function is intended to be injected into other functions (e.g.,
#' `get_google_calendar_events()`) to support testing and mocking.
#'
#' @return A tibble with two columns:
#'   \describe{
#'     \item{id}{The calendar ID (used to query events).}
#'     \item{summary}{The human-readable name of the calendar.}
#'   }
#' @export
default_calendar_list_fn <- function(
    gar_api_generator = googleAuthR::gar_api_generator
) {
  cal_fn <- gar_api_generator(
    "https://www.googleapis.com/calendar/v3/users/me/calendarList",
    "GET",
    data_parse_function = base::identity
  )

  cal_raw <- cal_fn()

  tibble::tibble(
    id      = cal_raw$items$id,
    summary = cal_raw$items$summary
  )
}




#' Retrieve Events from a Google Calendar
#'
#' This function defines and executes a default API request to the Google Calendar API
#' to fetch all events for a specified calendar within a given time window.
#'
#' It uses `googleAuthR::gar_api_generator()` to construct a call to the
#' `events.list` endpoint for a specific calendar.
#'
#' This function is intended for injection into higher-level functions (e.g.,
#' `get_google_calendar_events()`) and can be mocked for testing.
#'
#' @param cal_id A character string specifying the calendar ID.
#' @param tmin A POSIXct object representing the start of the time window (UTC).
#' @param tmax A POSIXct object representing the end of the time window (UTC).
#'
#' @return A parsed list object (from JSON) representing events returned by the
#'   Google Calendar API. If no events are found, `$items` will be an empty list.
#'
#' @export
default_events_list_fn <- function(
    cal_id, tmin, tmax,
    gar_api_generator = googleAuthR::gar_api_generator
) {
  url <- sprintf(
    "https://www.googleapis.com/calendar/v3/calendars/%s/events",
    utils::URLencode(cal_id, reserved = TRUE)
  )

  gar_api_generator(
    url, "GET",
    pars_args = list(
      timeMin      = format(tmin, "%Y-%m-%dT%H:%M:%SZ"),
      timeMax      = format(tmax, "%Y-%m-%dT%H:%M:%SZ"),
      singleEvents = "true",
      orderBy      = "startTime",
      maxResults   = 2500
    ),
    data_parse_function = base::identity
  )()
}


#### _________________ ####


