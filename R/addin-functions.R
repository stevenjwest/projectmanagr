#' Present Error Message as Addin
#'
#' @param errorTitle Title for gadgetTitleBar and dialogViewer.
#' @param errorMessage Message presented on addin gadget.
#'
addin_error <- function(errorTitle, errorMessage, settings) {

  ui <- miniPage(
    gadgetTitleBar(errorTitle),
    h2(errorMessage, align="center", style="color:red")
  )

  server <- function(input, output, session) {

    observeEvent(input$done, {
      stopApp()
    })

  }

  viewer <- dialogViewer("Add New Project Note",
                         width = settings[["GadgetWidth"]],
                         height = settings[["GadgetHeight"]])

  runGadget(ui, server, viewer = viewer)
}



#' Present Error Message as Addin
#'
#' @param errorTitle Title for gadgetTitleBar and dialogViewer.
#' @param errorMessage Message presented on addin gadget.
#'
addin_error_path <- function(errorTitle, errorMessage, path, settings) {

  ui <- miniPage(
    gadgetTitleBar(errorTitle),
    h2(errorMessage, align="center", style="color:red"),
    fillRow( code( path ) )
  )

  server <- function(input, output, session) {

    observeEvent(input$done, {
      stopApp()
    })

  }

  viewer <- dialogViewer("Add New Project Note",
                         width = settings[["GadgetWidth"]],
                         height = settings[["GadgetHeight"]])

  runGadget(ui, server, viewer = viewer)
}
