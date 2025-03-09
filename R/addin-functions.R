
#' Return CSS theme that matches RStudio theme: Light or Dark Mode.
get_css_theme <- function() {

  # Check the current theme
  themeInfo <- rstudioapi::getThemeInfo()
  isDarkTheme <- themeInfo$dark

  # Define CSS for light and dark themes
  lightThemeCSS <- "
    body {
      background-color: #FFFFFF;
      color: #000000;
    }
    .miniUI-gadget {
      background-color: #F7F7F7;
    }
    .gadget-title {
      background-color: #EFEFEF;
      color: #000000;
      border-bottom: 1px solid #DADADA;
    }
    .gadget-title .close {
      color: #000000;
    }
    .btn {
      background-color: #EFEFEF;
      color: #000000;
      border: 1px solid #DADADA;
    }
    .btn:hover {
      background-color: #DADADA;
      color: #000000;
    }
  "

  darkThemeCSS <- "
    body {
      background-color: #222222;
      color: #FFFFFF;
    }
    .miniUI-gadget {
      background-color: #333333;
    }
    .gadget-title {
      background-color: #444444;
      color: #FFFFFF;
      border-bottom: 1px solid #555555;
    }
    .gadget-title .close {
      color: #FFFFFF;
    }
    .btn {
      background-color: #555555;
      color: #FFFFFF;
      border: 1px solid #666666;
    }
    .btn:hover {
      background-color: #666666;
      color: #FFFFFF;
    }
    /* Additional styling for shinyDirChoose modal dialog in Dark Theme */
    .modal-content {
      background-color: #333333 !important;
      color: #FFFFFF !important;
      border: 1px solid #555555;
    }
    .modal-header {
      background-color: #444444 !important;
      border-bottom: 1px solid #555555;
    }
    .modal-footer {
      background-color: #444444 !important;
      border-top: 1px solid #555555;
    }
  "

  # Select the appropriate CSS based on the theme
  themeCSS <- if (isDarkTheme) darkThemeCSS else lightThemeCSS

  return(themeCSS)
}


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
    tags$style(HTML( get_css_theme() )),  # Apply the theme-specific CSS: dark or light
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
    tags$style(HTML( get_css_theme() )),  # Apply the theme-specific CSS: dark or light
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
