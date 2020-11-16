#' Add a New Protocol or CheatSheet
#'
#' Generates a Shiny Gadget for adding a new Protocol ot CheatSheet inside
#' the SOP/ dir of the containing Programme, and inserting a link to this
#' inside the active Proejct Note.
#'
#' The User must be in an active Project Note, and the selected
#' line and column is where the Protocol/CheatSheet Link will be inserted.
#' This command works for all Project Notes.  A reciprocal link is formed
#' from the Protocol/Cheatsheet to the Project Note.
#'
#' User selects a Protocol/CheatSheet name, and (Optionally) Protocol/CheatSheet
#'  title (filled by default with SPACES replacing "_" and "-" from name).
#'
#' Stipulates any errors in the input, and can only be completed
#' when these errors have been resolved.
#'
#'
#' @export
addinAddProtocolCheatSheet <- function() {


  # Retrieve cursorSelection from current Active Doc in rstudio:
    # is it:
      # a GROUP NOTE (HEADER or SUBNOTE) -> addSubNoteToGroup()
      # TASK or within bounds of a TASK - addProjectNote() OR addProjectNoteGroup()
  selection <- projectNoteSelection()


  context <- rstudioapi::getSourceEditorContext()
  cursor <- rstudioapi::primary_selection(context)

  # If no project Task is selected, present ERROR MESSAGE:

  if( selection[[1]] == "FALSE" ) {

    ui <- miniPage(
      gadgetTitleBar("Add New Protocol or Cheat Sheet"),
      h2("Select a Project Note to generate a new Protocol or Cheat Sheet in.", align="center", style="color:red")
    )

    server <- function(input, output, session) {

      observeEvent(input$done, {
        stopApp()
      })

    }

    viewer <- dialogViewer("Add New Protocol or Cheat Sheet", width = 500, height = 300)

    runGadget(ui, server, viewer = viewer)

  }
  else {


      ### CREATE GADGET ###

      ui <- miniPage(

        gadgetTitleBar("Add New Protocol or Cheat Sheet"),

        miniContentPanel(

          fillCol(

            fillRow( h5("Add a new Protocol or Cheat Sheet to a Project File") ),

            fillRow( p("Protocol Name MUST end with _Protocol or _Cheatsheet:") ),

            fillRow(  textInput("protocolName", "Protocol Name", value="_Protocol", width="100%")  ),

            fillRow(  span( textOutput("warningName"), style="color:red")  ),

            fillRow(  textInput("protocolTitle", "Protocol Title:", width="100%")  ),

            fillRow(   span( textOutput("warningDirectory"), style="color:red")  ),

            fillRow(   textOutput("projectNotePath")  )

          )

        )

      )


      ### ENCODE BEHAVIOUR ###

      server <- function(input, output, session) {

        # update protocolTitle when protocolName is changed:
        observe({

          updateTextInput(session, "protocolTitle", value = gsub("-", " ", gsub("_", " ", input$protocolName) )  )

        })

        output$projectNotePath <- renderText({
          #selection[["projectNotePath"]]
          projectNotePath <- normalizePath(selection[["projectNotePath"]])
          progPath <- findProgDir(projectNotePath)
          protocolsPath <- paste0(progPath, .Platform$file.sep, "SOP")
          protocolDirPath <- paste( protocolsPath, .Platform$file.sep, input$protocolName, sep="")
          paste( protocolDirPath, .Platform$file.sep, input$protocolName, ".Rmd", sep="")
        })



        observe({

          if( grepl("\\s", input$protocolName)  ) {
            # set the warningName TextOutput:
            output$warningName <- renderText({
              "PROTOCOL NAME CANNOT CONTAIN SPACES"
            })
          }
          else {
            output$warningName <- renderText({
              ""
            })
          }
        })



        # perform computations to create new Programme:
        observeEvent(input$done, {

          if(input$protocolName == "") {
            # set the warningName TextOutput:
            output$warningName <- renderText({
              "*** PROVIDE PROTOCOL/CHEATSHEET NAME ***"
            })
          }
          else if(! endsWith( tolower(input$protocolName), "_protocol") & ! endsWith( tolower(input$protocolName), "_cheatsheet")  ) {
            # if protocolName does not end with protocol or cheatsheet, stop and inform user:
            # set the warningName TextOutput:
            output$warningName <- renderText({
              "*** NAME MUST END WITH 'PROTOCOL' OR 'CHEATSHEET' ***"
            })
          }
          else if( grepl("\\s", input$protocolName)  ) {
            # set the warningName TextOutput:
            output$warningName <- renderText({
              "*** PROTOCOL NAME CANNOT CONTAIN SPACES ***"
            })
          }
          else {

            if( endsWith( tolower(input$protocolName), "_protocol") ) {

            projectmanagr::addProtocol(
                                  projectNotePath = selection[["projectNotePath"]],
                                  protocolName = input$protocolName,
                                  protocolTitle = input$protocolTitle,
                                  protocolTemplate="Protocol-Template.Rmd" )

            # normalize path - remove HOME REF ~
            projectNotePath <- normalizePath(selection[["projectNotePath"]])
            progPath <- findProgDir(projectNotePath)
            protocolsPath <- paste0(progPath, .Platform$file.sep, "SOP")
            protocolDirPath <- paste( protocolsPath, .Platform$file.sep, input$protocolName, sep="")
            protocolPath <- paste( protocolDirPath, .Platform$file.sep, input$protocolName, ".Rmd", sep="")
            #protocolPdfPath <- paste( protocolDirPath, .Platform$file.sep, input$protocolName, "pdf", sep="")

            # Now the protocol has been made, add a link to it in the Project Note at line and column:

            DocLink <- R.utils::getRelativePath(protocolPath, relativeTo=selection[["projectNotePath"]])
            DocLink <- substring(DocLink, first=4, last=nchar(DocLink))

            DocName <- basename(protocolPath)
            DocName <- gsub("-", " ",  gsub("_", " ", substring(DocName, first=1, last=regexpr("\\.[^\\.]*$", DocName)-1 ) )  )

            DocTitleLink <- paste( "[", DocName, "](", DocLink, ")", sep="" )


            rstudioapi::insertText(cursor$range, DocTitleLink, id = context$id)


            rstudioapi::navigateToFile( protocolPath )

            # Close Gadget after computations are complete:
            stopApp()

            }

            else if( endsWith( tolower(input$protocolName), "_cheatsheet") ) {

              projectmanagr::addCheatSheet(
                projectNotePath = selection[["projectNotePath"]],
                cheatsheetName = input$protocolName,
                cheatsheetTitle = input$protocolTitle,
                cheatsheetTemplate="CheatSheet-Template.Rmd" )

              # normalize path - remove HOME REF ~
              projectNotePath <- normalizePath(selection[["projectNotePath"]])
              progPath <- findProgDir(projectNotePath)
              protocolsPath <- paste0(progPath, .Platform$file.sep, "SOP")
              protocolDirPath <- paste( protocolsPath, .Platform$file.sep, input$protocolName, sep="")
              protocolPath <- paste( protocolDirPath, .Platform$file.sep, input$protocolName, ".Rmd", sep="")
              #protocolPdfPath <- paste( protocolDirPath, .Platform$file.sep, input$protocolName, "pdf", sep="")

              # Now the protocol has been made, add a link to it in the Project Note at line and column:

              DocLink <- R.utils::getRelativePath(protocolPath, relativeTo=selection[["projectNotePath"]])
              DocLink <- substring(DocLink, first=4, last=nchar(DocLink))

              DocName <- basename(protocolPath)
              DocName <- gsub("-", " ",  gsub("_", " ", substring(DocName, first=1, last=regexpr("\\.[^\\.]*$", DocName)-1 ) )  )

              DocTitleLink <- paste( "[", DocName, "](", DocLink, ")", sep="" )


              rstudioapi::insertText(cursor$range, DocTitleLink, id = context$id)


              rstudioapi::navigateToFile( protocolPath )

              # Close Gadget after computations are complete:
              stopApp()

            }

          }

        })

      }


      ### VIEW GADGET ###

      viewer <- dialogViewer("Create New Project Document", width = 1000, height = 1000)

      runGadget(ui, server, viewer = viewer)


  }

}
