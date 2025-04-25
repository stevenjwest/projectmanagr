


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
  save_context_doc()

  # 1) Identify active doc path and ensure it's valid
  context <- rstudioapi::getSourceEditorContext()
  if (is.null(context$path) || !nzchar(context$path)) {
    stop("No active document found. Please open a file in RStudio and try again.")
  }

  # 2) Ensure the working directory is set to the active doc's directory
  set_wd_active_doc()

  # 3) Identify the ORG path from the current working directory
  orgPath <- find_org_directory(getwd())
  if (orgPath == "") {
    addin_error_org("Extract TODOs")
  }

  # 4) Retrieve settings if needed
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

  # We'll set a single named root for the directory chooser
  roots <- c("ORG Directory" = orgPath)

  miniUI::miniPage(

    tags$style(HTML(get_css_theme())),  # apply theme if needed

    miniUI::gadgetTitleBar("Extract TODOs"),

    miniUI::miniContentPanel(
      fillCol(

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
        br(),

        fillRow(
          checkboxInput("onlyFirst", "Only first TODO per file?", FALSE),
          checkboxInput("sortMod",   "Sort by file mod time desc?", TRUE)
        ),
        fillRow(
          checkboxInput("prioFirst", "Priority-first layout?", FALSE)
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

  #### 1) Identify environment and RStudio context ####

  orgPath <- find_org_directory(getwd())
  context <- rstudioapi::getSourceEditorContext()
  fromFile <- context$path

  # get data from rstudio context
  path <- get_context_path()
  contents <- get_context_contents()
  row <- get_context_row()

  output$fromFilePathDisplay <- renderText({
    fromFile
  })

  # We'll store the user-chosen directory as location in a reactive
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


  #### 2) On "Done," call extract_todos & write to the file on disk ####

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
      fromFilePath         = fromFile,  # link references
      onlyFirstTodoPerFile = onlyF,
      sortByFileModTimeDesc= sortMod,
      priorityFirst        = prioF
    )

    # insert into current document
    write_file(insert_at_indices(contents, row, todos_vector), path)

    # navigate to path & close addin:
    addin_rstudio_nav(path)

  })

  observeEvent(input$cancel, {
    stopApp()
  })
}


