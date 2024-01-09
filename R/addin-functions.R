#' Present Error Message as Addin
#'
#' @param errorTitle Title for gadgetTitleBar and dialogViewer.
#' @param errorMessage Message presented on addin gadget.
#' @param settings A settings object, if it has been created, otherwise null.
#' Used to set the gadget width and height from settings, otherwise 1000x800
#'
addin_error <- function(errorTitle, errorMessage, settings=NULL) {

  ui <- miniPage(
    gadgetTitleBar(errorTitle),
    h2(errorMessage, align="center", style="color:red")
  )

  server <- function(input, output, session) {

    observeEvent(input$done, {
      stopApp()
    })

  }

  if( is.null(settings) ) {
    viewer <- dialogViewer("ERROR",
                           width = 1000,
                           height = 800)

  } else {
    viewer <- dialogViewer("ERROR",
                           width = settings[["GadgetWidth"]],
                           height = settings[["GadgetHeight"]])
  }

  runGadget(ui, server, viewer = viewer)
}



#' Present Error Message with path as Addin
#'
#' @param errorTitle Title for gadgetTitleBar and dialogViewer.
#' @param errorMessage Message presented on addin gadget.
#' @param path The path with the error
#' @param settings A settings object, if it has been created, otherwise null.
#' Used to set the gadget width and height from settings, otherwise 1000x800
#'
addin_error_path <- function(errorTitle, errorMessage, path, settings=NULL) {

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

  if( is.null(settings) ) {
    viewer <- dialogViewer("PATH ERROR",
                           width = 1000,
                           height = 800)

  } else {
  viewer <- dialogViewer("PATH ERROR",
                         width = settings[["GadgetWidth"]],
                         height = settings[["GadgetHeight"]])
  }

  runGadget(ui, server, viewer = viewer)
}
