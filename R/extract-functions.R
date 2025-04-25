
#' Extract HTML-comment-based TODOs from Rmd files with custom ordering
#'
#' This function scans a file or directory of R Markdown documents, looking for
#' specially-formatted TODO comments in HTML comment blocks. Each block is
#' extracted, given a priority based on deadlines/schedules, and finally
#' rendered into a single Rmd output using templates.
#'
#' A valid TODO block must begin at the very start of a line with
#' \code{<!-- {#todo}}, with no leading spaces. For example:
#' \preformatted{
#' <!-- {#todo}
#'   multiline body
#'   {#deadline 2025-12-31}
#' -->
#' }
#' If there are spaces before \code{<!--}, that block is ignored.
#'
#' @param location A path to either a directory containing Rmd files (searched
#'   recursively), or a single Rmd file.
#' @param date A string (e.g., "2025-04-01") representing "today." Used to
#'   interpret deadlines and schedules to see which are overdue or due today.
#' @param fromFilePath Optional, a path from which to calculate relative links
#'   back to each Rmd file. If \code{NULL}, defaults to \code{location}.
#' @param onlyFirstTodoPerFile If \code{TRUE}, only the first \code{<!-- {#todo}} block
#'   per file is extracted (the rest are ignored).
#' @param sortByFileModTimeDesc Logical; if \code{TRUE}, the note-first layout
#'   sorts files by descending modification time.
#' @param priorities A named list of length 5, e.g.
#'   \code{list(DEADLINE_PASSED=4, SCHEDULE_PASSED=3, DUE_TODAY=2, SCHEDULED_TODAY=1, OPEN_ITEMS=0)}.
#'   The \strong{order} of these five elements must match:
#'   \enumerate{
#'     \item \code{isDeadlinePassed} => \code{DEADLINE_PASSED}
#'     \item \code{isSchedulePassed} => \code{SCHEDULE_PASSED}
#'     \item \code{isDueToday} => \code{DUE_TODAY}
#'     \item \code{isScheduledToday} => \code{SCHEDULED_TODAY}
#'     \item \code{OPEN_ITEMS} => 0
#'   }
#'   A higher numeric priority means it appears first in the list.
#' @param collectionTemplate Name of the Rmd template to be used for the overall
#'   collection. Should contain placeholders like \code{{ALL_NOTES_SECTION}},
#'   \code{{DATE}}, \code{{SCOPE}}, etc.
#' @param noteTemplate Name of the template used to render each file's block. It
#'   should contain \code{{NOTE_BASENAME}}, \code{{NOTE_LINK}}, and
#'   \code{{NOTE_TODO_ITEMS}} placeholders.
#' @param itemTemplate Name of the template used to render each individual TODO
#'   item. Should contain placeholders like \code{{TODO_ITEM_HEADER}},
#'   \code{{TODO_ITEM_LINK}}, \code{{TODO_TEXT}}, \code{{TODO_DEADLINE}},
#'   \code{{TODO_SCHEDULE}}, etc.
#' @param priorityTemplate Name of the template used when \code{priorityFirst=TRUE}
#'   to render each priority section. Should contain placeholders like
#'   \code{{PRIORITY_HEADER}}, \code{{SEP2}}, and \code{{PRIORITY_ITEMS}}.
#' @param priorityFirst Logical; if \code{TRUE}, we use a "priority-first" layout
#'   grouping items under top-level headers by priority. If \code{FALSE}, we use
#'   a "note-first" layout, grouping items by file.
#'
#' @return A character vector of R Markdown lines representing the final doc.
#' @export
extract_todos <- function(location,
                          date = get_date(),
                          fromFilePath = NULL,
                          onlyFirstTodoPerFile = FALSE,
                          sortByFileModTimeDesc = TRUE,
                          priorities = list(
                            DEADLINE_PASSED = 4,
                            SCHEDULE_PASSED = 3,
                            DUE_TODAY       = 2,
                            SCHEDULED_TODAY = 1,
                            OPEN_ITEMS      = 0
                          ),
                          collectionTemplate = "Todo-Collection-Template.Rmd",
                          noteTemplate       = "Todo-Note-Template.Rmd",
                          itemTemplate       = "Todo-Item-Template.Rmd",
                          priorityTemplate   = "Todo-Priority-Template.Rmd",
                          priorityFirst      = FALSE) {

  cat("\nprojectmanagr::extract_todos():\n")

  # Set up environment and read user templates
  orgPath  <- confirm_find_org(location)
  tempPath <- get_template_dir(orgPath)
  settings <- get_settings_yml(orgPath)

  files <- get_projectmanagr_files(location, orgPath, settings)

  # Read top-level templates
  collectionLines <- read_file(fs::path(tempPath, collectionTemplate))
  noteLines       <- read_file(fs::path(tempPath, noteTemplate))
  itemLines       <- read_file(fs::path(tempPath, itemTemplate))

  # For priority-first layout, read the priority-section template
  prioritySectionLines <- NULL
  if (priorityFirst) {
    prioritySectionLines <- read_file(fs::path(tempPath, priorityTemplate))
  }

  if (is.null(fromFilePath)) {
    fromFilePath <- location
  }

  # Collect TODOs from all files
  allTodos <- todo_data_str()
  for (fl in files) {
    todos <- parse_todo_blocks_with_heading(fl)
    allTodos <- write_todo_data_str(
      fl, todos, allTodos,
      onlyFirstTodoPerFile = onlyFirstTodoPerFile,
      today = date
    )
  }

  if (nrow(allTodos) == 0) {
    # If no items, return minimal doc
    return(no_todo_items_found(allTodos, collectionLines, orgPath, date, location))
  }

  # Validate and invert priorities
  if (length(priorities) != 5) {
    stop("The 'priorities' list must have exactly 5 elements (DEADLINE_PASSED..OPEN_ITEMS).")
  }
  invMap <- setNames(names(priorities), as.character(priorities))

  # Assign numeric priority & parse itemDateTime
  allTodos$priority <- 0
  allTodos$itemDateTime <- NA_real_
  for (r in seq_len(nrow(allTodos))) {
    rowPriority <- 0

    if (allTodos$isDeadlinePassed[r] == 1) {
      rowPriority <- max(rowPriority, priorities[[1]])
    }
    if (allTodos$isSchedulePassed[r] == 1) {
      rowPriority <- max(rowPriority, priorities[[2]])
    }
    if (allTodos$isDueToday[r] == 1) {
      rowPriority <- max(rowPriority, priorities[[3]])
    }
    if (allTodos$isScheduledToday[r] == 1) {
      rowPriority <- max(rowPriority, priorities[[4]])
    }
    # else rowPriority => 0 => OPEN_ITEMS
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

  # Build the final doc using either note-first or priority-first layout
  if (priorityFirst) {
    finalDoc <- build_priority_first_layout(
      allTodos             = allTodos,
      invMap               = invMap,
      itemLines            = itemLines,
      noteLines            = noteLines,
      prioritySectionLines = prioritySectionLines,
      fromFilePath         = fromFilePath,
      orgPath              = orgPath,
      date                 = date,
      location             = location,
      collectionLines      = collectionLines
    )
  } else {
    finalDoc <- build_note_first_layout(
      allTodos             = allTodos,
      sortByFileModTimeDesc= sortByFileModTimeDesc,
      itemLines            = itemLines,
      noteLines            = noteLines,
      fromFilePath         = fromFilePath,
      orgPath              = orgPath,
      date                 = date,
      location             = location,
      collectionLines      = collectionLines
    )
  }

  finalDoc <- replace_sep_values(finalDoc, orgPath)

  finalDoc
}


#' Construct a data frame to store all extracted TODO items
#'
#' @param file A character vector of file paths.
#' @param fileMtime Modification times for each file.
#' @param indexInFile The index of the TODO within the file (1-based).
#' @param heading The nearest markdown heading found in the file.
#' @param title The inline title found after the \code{{#todo}} tag, if any.
#' @param text The multiline text of the TODO block.
#' @param deadline A string for \code{YYYY-MM-DD} if present.
#' @param schedule A string for \code{YYYY-MM-DD:HHmm} if present.
#' @param isScheduledToday,isSchedulePassed,isDueToday,isDeadlinePassed Integers
#'   (0 or 1) indicating whether the item is scheduled today, schedule passed,
#'   due today, or deadline passed.
#' @param itemDateTime A numeric or POSIXct used internally to sort items with
#'   the same priority.
#'
#' @return A data frame with the columns described above.
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
    itemDateTime     = numeric()
) {
  data.frame(
    file, fileMtime, indexInFile, heading, title, text,
    deadline, schedule,
    isScheduledToday, isSchedulePassed, isDueToday, isDeadlinePassed,
    itemDateTime,
    stringsAsFactors = FALSE
  )
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
parse_todo_blocks_with_heading <- function(filePath) {
  contents <- read_file(filePath)
  inBlock <- FALSE
  blockLines <- character()
  results <- list()
  currentHeading <- ""

  for (ln in seq_along(contents)) {
    line <- contents[ln]
    # If we see a markdown heading, store it
    if (!inBlock && grepl("^#+\\s+", line)) {
      currentHeading <- trimws(gsub("^#+", "", line))
    }

    # If not in a block, look for the start \strong{exactly} at the line start
    # (no leading spaces).
    if (!inBlock) {
      # Changed to require line to begin with `<!-- ...`
      if (grepl("^<!--\\s*\\{#todo\\}", line)) {
        inBlock <- TRUE
        blockLines <- c(line)
      }
    } else {
      # Already in a block, keep collecting lines
      blockLines <- c(blockLines, line)
      if (grepl("-->", line)) {
        # Reached end of block
        parsed <- parse_one_todo_block(blockLines)
        if (!is.null(parsed)) {
          parsed$heading <- currentHeading
          results[[length(results) + 1]] <- parsed
        }
        inBlock <- FALSE
        blockLines <- character()
      }
    }
  }
  results
}


#' Parse a single HTML todo block
#'
#' This function extracts an optional inline title (the text appearing on
#' the same line as `{#todo}` in the comment), a multiline description,
#' plus any deadlines or schedules from the block content.
#'
#' @param blockLines A character vector of lines from the start to end of
#'   the HTML comment block.
#'
#' @return A list with the elements:
#'   \describe{
#'     \item{\code{text}}{A character string of the main multiline description.}
#'     \item{\code{title}}{A character string for the todo block's inline title.}
#'     \item{\code{deadline}}{A string with the parsed deadline, if any.}
#'     \item{\code{schedule}}{A string with the parsed schedule, if any.}
#'   }
#'   Currently, no \code{heading} is assigned; that may be added later.
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' blines <- c("<!-- {#todo} My Title", "{#deadline 2025-04-10}", "This is the body", "-->")
#' parse_one_todo_block(blines)
#' }
#'
#' @keywords internal
parse_one_todo_block <- function(blockLines) {
  if (!length(blockLines)) return(NULL)

  # Remove trailing --> if present
  lastLineIdx <- length(blockLines)
  if (grepl("-->", blockLines[lastLineIdx])) {
    blockLines[lastLineIdx] <- sub("-->.*$", "", blockLines[lastLineIdx])
  }

  # The first line typically has <!-- {#todo} maybeTitle
  firstLine <- blockLines[1]
  if (grepl("<!--", firstLine)) {
    firstLine <- sub("<!--", "", firstLine)
  }
  # remove {#todo} but preserve anything after that => inline title
  # e.g. " {#todo} My Title" => "My Title"
  firstLine <- sub("\\{#todo\\}", "", firstLine)

  # Trim leading/trailing whitespace
  firstLine <- trimws(firstLine)

  # If there's anything left in firstLine, that's our 'title'
  todoTitle <- ""
  if (nzchar(firstLine)) {
    todoTitle <- firstLine
  }

  deadlineVal  <- ""
  scheduleVal  <- ""
  textLines    <- character()

  # Start collecting lines after the first line (since the first line might
  # have included a title). We'll store the multiline description in textLines.
  for (ln in seq_along(blockLines)) {
    if (ln == 1) {
      # skip the first line, we've partially parsed it for the title
      next
    }
    ltxt <- trimws(blockLines[ln])

    # if there's a {#deadline ...} or {#schedule ...}, parse them
    if (grepl("\\{#deadline\\s+", ltxt)) {
      val <- sub(".*\\{#deadline\\s+", "", ltxt)
      val <- sub("\\}.*", "", val)
      deadlineVal <- val
    } else if (grepl("\\{#schedule\\s+", ltxt)) {
      val <- sub(".*\\{#schedule\\s+", "", ltxt)
      val <- sub("\\}.*", "", val)
      scheduleVal <- val
    } else {
      textLines <- c(textLines, ltxt)
    }
  }

  mainText <- paste(textLines, collapse="\n")

  list(
    text     = mainText,
    title    = todoTitle,
    deadline = deadlineVal,
    schedule = scheduleVal
  )
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


#' Write parsed TODO items to the master data frame
#'
#' @param fl The file path from which TODOs were extracted.
#' @param todos A list of parsed items (each a list with \code{text}, \code{title},
#'   \code{deadline}, \code{schedule}, etc.).
#' @param allTodos The existing data frame of all collected TODO items so far.
#' @param onlyFirstTodoPerFile If \code{TRUE}, we add only the first item from
#'   each file.
#' @param today The "today" date string for schedule/deadline logic.
#'
#' @return The updated \code{allTodos} data frame with new rows appended.
#' @keywords internal
write_todo_data_str <- function(fl, todos, allTodos,
                                onlyFirstTodoPerFile, today) {

  if (length(todos) == 0) return(allTodos)
  modTime <- file.info(fl)$mtime
  itemIndex <- 1

  for (tdi in seq_along(todos)) {
    if (onlyFirstTodoPerFile && tdi > 1) {
      break
    }
    td <- todos[[tdi]]
    flags <- interpret_todo_flags(td$schedule, td$deadline, today)

    # Insert a row for this item
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
        itemDateTime     = NA_real_
      )
    )
    itemIndex <- itemIndex + 1
  }

  allTodos
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


#' Format a multi-line TODO description into Markdown bullets
#'
#' Splits the text by newline. The first line is prefixed with `* `,
#' subsequent lines each get `    + `, with a blank line after each bullet.
#'
#' @param txt A multiline string from the parsed TODO block.
#'
#' @return A character string with bullet indentation.
#' @keywords internal
format_todo_description <- function(txt) {
  lines <- strsplit(txt, "\n", fixed = TRUE)[[1]]
  # Remove empty lines if you prefer:
  lines <- lines[lines != ""]

  if (length(lines) == 0) {
    return("* \n\n")  # at least a single bullet
  }
  # First line => "* first line\n\n"
  out <- c(paste0("* ", lines[1], "\n\n"))
  # Subsequent => "    + line\n\n"
  if (length(lines) > 1) {
    for (ln in lines[-1]) {
      out <- c(out, paste0("    + ", ln, "\n\n"))
    }
  }
  paste(out, collapse = "")
}


#' Build text/deadline/schedule fields for a single TODO
#'
#' Indents the schedule and deadline lines, if present, each followed by a blank line.
#'
#' @param row One row of the allTodos data frame (i.e. one TODO).
#'
#' @return A list with \code{$TODO_TEXT}, \code{$TODO_DEADLINE}, \code{$TODO_SCHEDULE}.
#' @keywords internal
build_item_fields <- function(row) {
  textStr <- format_todo_description(row$text)

  # Indent schedule & deadline with two spaces and a blank line if present
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


#' Build the final output with note-first layout
#'
#' In "note-first" mode, we group items by file, sorted by newest file first
#' (if \code{sortByFileModTimeDesc=TRUE}), then within the file we sort items by
#' descending priority, ascending date/time, and ascending index in file.
#'
#' @param allTodos The data frame of all collected TODO items.
#' @param sortByFileModTimeDesc Boolean indicating whether to sort files by newest first.
#' @param itemLines The lines from \code{itemTemplate}.
#' @param noteLines The lines from \code{noteTemplate}.
#' @param fromFilePath Path from which links are computed.
#' @param orgPath Path to the project root for relative references.
#' @param date The user's "today" date string.
#' @param location The original location passed to \code{extract_todos()}.
#' @param collectionLines The lines from \code{collectionTemplate}.
#'
#' @return A character vector of the final doc in Rmd form.
#' @keywords internal
build_note_first_layout <- function(allTodos,
                                    sortByFileModTimeDesc,
                                    itemLines,
                                    noteLines,
                                    fromFilePath,
                                    orgPath,
                                    date,
                                    location,
                                    collectionLines) {

  # Sort files by mod time if needed
  fileModTime   <- sapply(unique(allTodos$file), function(ff) file.info(ff)$mtime)
  distinctFiles <- names(sort(fileModTime, decreasing = sortByFileModTimeDesc))

  finalNoteChunks <- character()

  for (fl in distinctFiles) {
    subdf <- allTodos[allTodos$file == fl, , drop=FALSE]
    if (nrow(subdf) == 0) next

    # Sort items by priority desc, date/time asc, index asc
    subdf <- subdf[order(
      -subdf$priority,
      subdf$itemDateTime,
      subdf$indexInFile
    ), , drop=FALSE]

    baseName <- fs::path_file(fl)
    noteLink <- create_hyperlink_section(
      toFileName    = baseName,
      toFileSection = "",
      toFilePath    = fl,
      fromFilePath  = fromFilePath
    )

    noteBlock <- noteLines
    noteBlock <- sub_template_param(noteBlock, "{{NOTE_BASENAME}}", baseName, orgPath)
    noteBlock <- sub_template_param(noteBlock, "{{NOTE_LINK}}",     noteLink, orgPath)

    itemExpansions <- character()

    for (rowi in seq_len(nrow(subdf))) {
      row <- subdf[rowi, ]
      line <- itemLines

      # If there's an inline title for this TODO, use that; otherwise use the heading
      if (nzchar(row$title)) {
        itemHeader <- row$title
      } else {
        itemHeader <- row$heading
        if (!nzchar(itemHeader)) itemHeader <- "top"
      }

      # The item link is based on the file heading, to preserve the anchor references
      itemLink <- create_hyperlink_section(
        toFileName    = baseName,
        toFileSection = row$heading,
        toFilePath    = row$file,
        fromFilePath  = fromFilePath
      )
      line <- sub_template_param(line, "{{TODO_ITEM_LINK}}", itemLink, orgPath)

      # In note-first layout, we show the flags + heading
      prefixParts <- character()
      if (row$isDeadlinePassed == 1) prefixParts <- c(prefixParts, "DEADLINE_PASSED")
      if (row$isSchedulePassed == 1) prefixParts <- c(prefixParts, "SCHEDULE_PASSED")
      if (row$isDueToday       == 1) prefixParts <- c(prefixParts, "DUE_TODAY")
      if (row$isScheduledToday == 1) prefixParts <- c(prefixParts, "SCHEDULED_TODAY")
      if (length(prefixParts) == 0) prefixParts <- c("OPEN_ITEMS")

      # Combine flags with whichever heading we decided above
      todoItemHeader <- paste(c(prefixParts, itemHeader), collapse=" ")
      line <- sub_template_param(line, "{{TODO_ITEM_HEADER}}", todoItemHeader, orgPath)

      # Expand text, deadline, schedule
      fields <- build_item_fields(row)
      line <- sub_template_param(line, "{{TODO_TEXT}}",     fields$TODO_TEXT,     orgPath)
      line <- sub_template_param(line, "{{TODO_DEADLINE}}", fields$TODO_DEADLINE, orgPath)
      line <- sub_template_param(line, "{{TODO_SCHEDULE}}", fields$TODO_SCHEDULE, orgPath)

      itemExpansions <- c(itemExpansions, line)
    }

    noteBlock <- sub_template_param(noteBlock, "{{NOTE_TODO_ITEMS}}", itemExpansions, orgPath)
    finalNoteChunks <- c(finalNoteChunks, noteBlock)
  }

  # Fill the top-level template
  collectionLines <- sub_template_param(collectionLines, "{{ALL_NOTES_SECTION}}", finalNoteChunks, orgPath)
  collectionLines <- sub_template_param(collectionLines, "{{DATE}}", date, orgPath)

  scope <- derive_scope_string(location)
  collectionLines <- sub_template_param(collectionLines, "{{SCOPE}}", scope, orgPath)

  collectionLines
}


#' Build the final output with priority-first layout
#'
#' In "priority-first" mode, we group items by priority level (descending).
#' Each priority becomes a top-level heading (expanded in the
#' \code{prioritySectionLines}), and under it we list files (in descending mod
#' time), and under each file, the items that match that priority.
#'
#' @param allTodos The data frame of all collected TODO items.
#' @param invMap A named character vector inverting numeric -> priority name,
#'   e.g. \code{c("4"="DEADLINE_PASSED", "3"="SCHEDULE_PASSED", ...)}.
#' @param itemLines The lines from \code{itemTemplate}.
#' @param noteLines The lines from \code{noteTemplate}.
#' @param prioritySectionLines The lines from \code{priorityTemplate}.
#' @param fromFilePath Path from which links are computed.
#' @param orgPath Path to the project root for relative references.
#' @param date The user's "today" date string.
#' @param location The original location passed to \code{extract_todos()}.
#' @param collectionLines The lines from \code{collectionTemplate}.
#'
#' @return A character vector of the final doc in Rmd form.
#' @keywords internal
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

  usedPriorities <- unique(allTodos$priority)
  usedPriorities <- sort(usedPriorities, decreasing=TRUE)

  finalPriorityChunks <- character()

  # Sort files by descending mod time
  fileModTime   <- sapply(unique(allTodos$file), function(ff) file.info(ff)$mtime)
  distinctFiles <- names(sort(fileModTime, decreasing = TRUE))

  for (prio in usedPriorities) {
    subdfPrio <- allTodos[allTodos$priority == prio, , drop=FALSE]
    if (nrow(subdfPrio) == 0) next

    # numeric priority -> named key
    priorityName <- if (!is.null(invMap[as.character(prio)])) {
      invMap[as.character(prio)]
    } else {
      paste("PRIORITY", prio)
    }

    priorityFileChunks <- character()
    for (fl in distinctFiles) {
      subdfFile <- subdfPrio[subdfPrio$file == fl, , drop=FALSE]
      if (nrow(subdfFile) == 0) next

      # sort items by date/time asc, index asc
      subdfFile <- subdfFile[order(subdfFile$itemDateTime, subdfFile$indexInFile), , drop=FALSE]

      baseName <- fs::path_file(fl)
      noteLink <- create_hyperlink(
        toFileName    = baseName,
        toFilePath    = fl,
        fromFilePath  = fromFilePath
      )

      noteBlock <- noteLines
      noteBlock <- sub_template_param(noteBlock, "{{NOTE_BASENAME}}", baseName, orgPath)
      noteBlock <- sub_template_param(noteBlock, "{{NOTE_LINK}}",     noteLink, orgPath)

      itemExpansions <- character()
      for (rowi in seq_len(nrow(subdfFile))) {
        row <- subdfFile[rowi, ]
        line <- itemLines

        # If there's an inline title, use it; else use the heading
        if (nzchar(row$title)) {
          itemHeader <- row$title
        } else {
          itemHeader <- row$heading
          if (!nzchar(itemHeader)) itemHeader <- "top"
        }

        # The link anchor uses the heading
        itemLink <- create_hyperlink_section(
          toFileName    = baseName,
          toFileSection = row$heading,
          toFilePath    = row$file,
          fromFilePath  = fromFilePath
        )
        line <- sub_template_param(line, "{{TODO_ITEM_LINK}}", itemLink, orgPath)

        # priority-first => no flags in the item header
        line <- sub_template_param(line, "{{TODO_ITEM_HEADER}}", itemHeader, orgPath)

        # expand multiline text, schedule, deadline
        fields <- build_item_fields(row)
        line <- sub_template_param(line, "{{TODO_TEXT}}",     fields$TODO_TEXT,     orgPath)
        line <- sub_template_param(line, "{{TODO_DEADLINE}}", fields$TODO_DEADLINE, orgPath)
        line <- sub_template_param(line, "{{TODO_SCHEDULE}}", fields$TODO_SCHEDULE, orgPath)

        itemExpansions <- c(itemExpansions, line)
      }

      noteBlock <- sub_template_param(noteBlock, "{{NOTE_TODO_ITEMS}}", itemExpansions, orgPath)
      priorityFileChunks <- c(priorityFileChunks, noteBlock, "")
    }

    # Fill the priority-section template
    prioBlock <- prioritySectionLines
    prioBlock <- sub_template_param(prioBlock, "{{PRIORITY_HEADER}}", priorityName, orgPath)
    prioBlock <- sub_template_param(prioBlock, "{{SEP2}}", "", orgPath)
    prioBlock <- sub_template_param(prioBlock, "{{PRIORITY_ITEMS}}", priorityFileChunks, orgPath)

    finalPriorityChunks <- c(finalPriorityChunks, prioBlock)
  }

  # fill the top-level
  collectionLines <- sub_template_param(collectionLines, "{{ALL_NOTES_SECTION}}", finalPriorityChunks, orgPath)
  collectionLines <- sub_template_param(collectionLines, "{{DATE}}", date, orgPath)

  scope <- derive_scope_string(location)
  collectionLines <- sub_template_param(collectionLines, "{{SCOPE}}", scope, orgPath)

  collectionLines

} #### ____ ####


#' Extract Google Calendar events for a specific day
#'
#' Authenticates using a built-in OAuth2 client, retrieves the user's calendars,
#' and fetches events for a given day (default: today).
#'
#' @param day A date (defaults to today) to fetch events for
#' @param settings An optional list, can include `googleCalendars` for filtering
#' @param auth_fn Function for handling authentication (used for testing)
#' @param calendar_list_fn Function that returns calendar list
#' @param events_fn Function to fetch events given a calendar ID
#'
#' @return A tibble with calendar events
#' @export
extract_google_calendar_events <- function(
    day = Sys.Date(),
    settings = list(),
    auth_fn = default_google_auth,
    calendar_list_fn = default_calendar_list_fn,
    events_fn = default_events_list_fn
) {

  cat("\nprojectmanagr::extract_google_calendar_events():\n")

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




#' Authenticate with Google Calendar using projectmanagr's built-in OAuth client
#'
#' This function configures `googleAuthR` using a pre-bundled OAuth 2.0 client
#' JSON, and sets the token cache to a user-specific location using `rappdirs`.
#'
#' It then attempts to authenticate the user by selecting an existing cached
#' token automatically (using `googleAuthR::gar_auth(email = TRUE)`), or
#' prompting browser sign-in if no token exists yet.
#'
#' @details
#' This function sets a global R option:
#' \itemize{
#'   \item \code{options(gargle_oauth_cache = rappdirs::user_cache_dir("projectmanagr"))}
#' }
#' to store refreshable tokens in a consistent location across sessions.
#'
#' @return Invisibly returns the result of `googleAuthR::gar_auth()`
#' @export
default_google_auth <- function(
    gar_set_client = googleAuthR::gar_set_client,
    gar_auth = googleAuthR::gar_auth
) {
  gar_set_client(
    json = system.file("google-oauth", "projectmanagr_gcal_oauth_client.json",
                       package = "projectmanagr"),
    scopes = "https://www.googleapis.com/auth/calendar.readonly"
  )

  options(gargle_oauth_cache = rappdirs::user_cache_dir("projectmanagr"))

  invisible(gar_auth(email = TRUE))
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









