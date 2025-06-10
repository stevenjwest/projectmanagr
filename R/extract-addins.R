


#' Extract TODOs Shiny Gadget (Interactive) - Write to File
#'
#' Opens a Shiny gadget where the user can set parameters for \code{\link{extract_todos}},
#' such as:
#' \itemize{
#'   \item location (via directory chooser),
#'   \item date (via date input),
#'   \item onlyFirstTodoPerFile (checkbox),
#'   \item sortByFileModTimeDesc (checkbox),
#'   \item priorityFirst (checkbox).
#' }
#' The gadget then calls \code{extract_todos()}, inserts the resulting lines
#' directly into the currently open source file (on disk) at the cursor line,
#' and re-opens that file in RStudio.
#'
#' @export
addin_extract_todos <- function() {

  cat("\nprojectmanagr::addin_extract_todos():\n")

  # SAVE the document before processing!
  .save_context_doc()

  # Identify active doc path and ensure it's valid
  context <- .get_source_editor_context()
  if (is.null(context$path) || !nzchar(context$path)) {
    stop("No active document found. Please open a file in RStudio and try again.")
  }

  # Ensure the working directory is set to the active doc's directory
  set_wd_active_doc()

  # Identify the ORG path from the current working directory
  orgPath <- find_org_directory(getwd())
  if (orgPath == "") {
    addin_error_org("Extract TODOs")
  }

  # Retrieve settings
  settings <- get_settings_yml(orgPath)

  # 5) Launch the Shiny gadget
  runGadget(
    app     = addin_extract_todos_ui(orgPath, context$path, settings),
    server = addin_extract_todos_server,
    viewer = addin_create_dialog_viewer("Extract TODOs", settings)
  )
}


#' UI for the Extract TODOs Shiny Gadget (Write to File)
#'
#' Creates a \code{miniUI::miniPage} with:
#' \itemize{
#'   \item The path of the active RStudio file (read-only).
#'   \item A directory chooser for \code{location}.
#'   \item A date input for choosing \code{date}.
#'   \item Checkboxes for \code{onlyFirstTodoPerFile}, \code{sortByFileModTimeDesc},
#'         and \code{priorityFirst}.
#' }
#'
#' @param orgPath The organisation path, used as a root in \code{shinyDirChoose}.
#' @param fromFilePath The path of the currently active RStudio document (display only).
#' @param settings A list of settings from \code{get_settings_yml(orgPath)}.
#'
#' @keywords internal
addin_extract_todos_ui <- function(orgPath, fromFilePath, settings) {

  # set a single named root for the directory chooser
  roots <- c("ORG Directory" = orgPath)

  miniUI::miniPage(

    tags$style(HTML(get_css_theme())),  # apply theme if needed

    miniUI::gadgetTitleBar("Extract TODOs"),

    miniUI::miniContentPanel(
      fillCol(

        fillRow(
          checkboxInput("onlyFirst", "Only first TODO per file?", FALSE),
          checkboxInput("sortMod",   "Sort by file mod time desc?", TRUE)
        ),
        fillRow(
          checkboxInput("prioFirst", "Group-first layout?", FALSE)
        ),
        br(),

        fillRow(
          h4("Active Document Path:")
        ),
        fillRow(
          textOutput("fromFilePathDisplay")
        ),
        br(),

        fillRow(
          flex = c(5,1),
          verbatimTextOutput("dirLocation", placeholder = TRUE),
          shinyFiles::shinyDirButton("dir", "Select Directory", "Extract from Dir")
        ),
        br(),

        fillRow(
          dateInput("date", "Select 'Today':", value = Sys.Date(), width = "50%")
        ),
        br()

      )
    )
  )
}


#' Server logic for the Extract TODOs Shiny Gadget (Write to File)
#'
#' Observes user inputs, calls \code{extract_todos()}, and writes the
#' returned lines into the file open in the RStudio editor at the cursor line.
#'
#' @param input,output,session Standard Shiny server arguments.
#'
#' @keywords internal
addin_extract_todos_server <- function(input, output, session) {

  #### Identify environment and RStudio context ####

  orgPath <- find_org_directory(getwd())
  # get data from rstudio context
  path <- .get_context_path()
  contents <- .get_context_contents()
  row <- .get_context_row()

  output$fromFilePathDisplay <- renderText({
    path
  })

  # store user-chosen directory as location in a reactive
  # Initialize with orgPath as a default
  shinyFiles::shinyDirChoose(input, "dir",
                             roots = c("ORG Directory" = orgPath),
                             filetypes = c(""))

  rv <- reactiveValues(location = orgPath)

  observeEvent(input$dir, {
    if (!"path" %in% names(input$dir)) return()
    selRoot    <- input$dir$root
    selPathVec <- input$dir$path
    if (length(selPathVec) == 0) return() # user canceled or no selection
    rootPath <- c("ORG Directory" = orgPath)[[selRoot]]

    fullPath <- fs::path(rootPath, paste(selPathVec, collapse = .Platform$file.sep))
    rv$location <- fullPath
  })

  output$dirLocation <- renderText({
    rv$location
  })


  #### On "Done" call extract_todos & write to the file on disk ####

  observeEvent(input$done, {

    # fetch user choices
    loc     <- rv$location
    dt      <- as.character(input$date)
    onlyF   <- input$onlyFirst
    sortMod <- input$sortMod
    prioF   <- input$prioFirst

    # 2a) run extract_todos
    todos_vector <- extract_todos(
      location             = loc,
      date                 = dt,
      fromFilePath         = path,  # link references
      onlyFirstTodoPerFile = onlyF,
      sortByFileModTimeDesc= sortMod,
      groupingFirst        = prioF
    )

    # insert into current document
    write_file(insert_at_indices(contents, row, todos_vector), path)

    # navigate to path & close addin:
    addin_rstudio_nav(path)

  })

  observeEvent(input$cancel, {
    stopApp()
  })
} #### ____ ####



#' Extract Google-calendar events (markdown) – Shiny gadget
#'
#' Opens a miniUI gadget where the user
#' * chooses a **date** (defaults = today) and
#' * optionally selects which **calendars** to include.
#'
#' On *Done* the gadget
#' 1. calls \code{\link{extract_google_calendar_events_markdown}()};
#' 2. inserts the returned lines into the active document at the
#'    current cursor row; and
#' 3. reloads the document in the editor.
#'
#' @export
addin_extract_google_calendar_events_markdown <- function() {

  cat("\nprojectmanagr::addin_extract_google_calendar_events_markdown():\n")

  .save_context_doc()

  ctx <- .get_source_editor_context()
  if (is.null(ctx$path) || !nzchar(ctx$path))
    stop("Open a document in RStudio and try again.")

  set_wd_active_doc()
  orgPath <- find_org_directory(getwd())
  if (orgPath == "") addin_error_org("Extract Google events")

  settings <- get_settings_yml(orgPath)

  shiny::runGadget(
    app    = addin_extract_gcal_md_ui(orgPath, ctx$path, settings),
    server = addin_extract_gcal_md_server,
    viewer = addin_create_dialog_viewer("Extract Google events", settings)
  )
}

addin_extract_gcal_md_ui <- function(orgPath, fromFilePath, settings) {

  miniUI::miniPage(

    tags$style(HTML(get_css_theme() %||% "")),

    miniUI::gadgetTitleBar("Extract Google-calendar events"),

    miniUI::miniContentPanel(
      fillCol(

        fillRow(h4("Active document:"), br()),
        verbatimTextOutput("docPath"), br(),

        fillRow(dateInput("date", "Day to import:",
                          value = Sys.Date(), width = "50%")), br(),

        fillRow(selectInput("calendars",
                            "Calendars (empty = all):",
                            choices  = character(0),  # filled server-side
                            multiple = TRUE,
                            width    = "100%")), br()
      )
    )
  )
}

addin_extract_gcal_md_server <- function(input, output, session) {

  ## ---- context & helper values ---------------------------------------
  path     <- .get_context_path()
  contents <- .get_context_contents()
  row      <- .get_context_row()

  output$docPath <- renderText(path)

  ## ---- populate calendar selector ------------------------------------
  # one live API call – fast and silent
  try({
    cal_df <- default_calendar_list_fn()
    updateSelectInput(session, "calendars",
                      choices = cal_df$summary,
                      selected = character(0))
  }, silent = TRUE)

  ## ---- Done: build markdown & insert ---------------------------------
  observeEvent(input$done, {

    ev_txt <- extract_google_calendar_events_markdown(
      day      = input$date,
      settings = list(
        googleCalendars = if (length(input$calendars))
          input$calendars else NULL
      )
    )

    write_file(insert_at_indices(contents, row, ev_txt), path)
    addin_rstudio_nav(path)      # reload & place cursor
  })

  observeEvent(input$cancel, stopApp())
}




