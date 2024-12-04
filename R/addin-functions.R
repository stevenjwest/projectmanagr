

#' Create Dialog Viewer for Addin
addin_create_dialog_viewer <- function(title, settings) {
  dialogViewer(title,
               width = settings[["GadgetWidth"]],
               height = settings[["GadgetHeight"]])
}

#' Navigate to file in RStudio
#'
#' Uses `rstudioapi` to navigate to filePath and its parent, and set the working
#' directory, once files have been created by ADDIN.
#'
addin_rstudio_nav <- function(filePath) {

  if( rstudioapi::isAvailable() ) {
    rstudioapi::navigateToFile(filePath)
    rstudioapi::filesPaneNavigate(dirname(filePath))
  }

  setwd(dirname(filePath))

  # Close Gadget after 'done' is clicked.
  stopApp()

}



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


addin_error_org <- function(addinErrorTitle) {
  addin_error_path(addinErrorTitle,
                 "No Organisation identified - ensure working directory is in an Organisation.",
                 orgPath)
  stop( paste0("  No Organisation identified - ensure working directory is in an Organisation: \n    ", getwd()))

}
