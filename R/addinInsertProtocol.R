#' Insert Protocol into Experimental Note
#'
#' Generates a Shiny Gadget for inserting an SOP Protocol into an Experimental Note.  SOPs
#' from the Experimental Note's Programme are provided in a dropdown list for selection.
#' The PROTOCOL from the SOP (excluding Notes by default) will be pasted at the cursor
#' location.
#'
#' This allows standard operating procedures to be defined and inserted for repeatable and
#' reproducible experiments and documentation.  The SOPs can include Procedures and detailed
#' steps to follow, and R chunks of code for encoding data on the processes and samples.
#'
#' The active document MUST be a Project Note, MUST be inside a PROGRAMME, and there MUST
#' be SOPs to add!  User can specify to include all notes in the Experimental Note if
#' desired - these can provide tips and tricks for successfully implementing the procedures.
#'
#'
#' @export
addinInsertProtocol <- function() {

  context <- rstudioapi::getSourceEditorContext()
  row <- context$selection[[1]]$range$start[1]

  # SAVE the document before processing!
  rstudioapi::documentSave(id = context$id)

  projNotePath <- checkProjNote(context$path) # returns blank string if not proj note

  progDir <- findProgDir(context$path)
  sopDir <- paste0(progDir, .Platform$file.sep, "SOP")
  sops <- list.files(sopDir)

  protocols <- sops[endsWith(sops, "_Protocol")] # extract only Protocols

  # If active doc is not a Project Note, present ERROR MESSAGE:
  if( projNotePath == "" ) {

    ui <- miniPage(
      gadgetTitleBar("Insert Protocol"),
      h2("Protocols can only be inserted into Project Notes.", align="center", style="color:red")
    )

    server <- function(input, output, session) {

      observeEvent(input$done, {
        stopApp()
      })

    }

    viewer <- dialogViewer("Insert Protocol", width = 500, height = 300)

    runGadget(ui, server, viewer = viewer)

  }
  # If there are no protocols, present ERROR MESSAGE:
  else if( length(protocols) == 0 ) {

    ui <- miniPage(
      gadgetTitleBar("Insert Protocol"),
      h2("SOP contains no Protocols to be inserted into Project Note.", align="center", style="color:red")
    )

    server <- function(input, output, session) {

      observeEvent(input$done, {
        stopApp()
      })

    }

    viewer <- dialogViewer("Insert Protocol", width = 500, height = 300)

    runGadget(ui, server, viewer = viewer)

  }
  else {

    # present protocols for user to select:
      ui <- miniPage(

        gadgetTitleBar("Insert Protocol"),

        miniContentPanel(

          fillCol(

            fillRow( h5("Insert a Protocol into a Project Note") ),

            fillRow( selectInput("select", "Select Protocol:",
                                 choices = protocols, selected = protocols[1]) ),

            #fillRow(   span( textOutput("warningFile"), style="color:red")  ),

            fillRow(   span( textOutput("overviewProtocol") )  ),

            fillRow(  selectInput("includeEquip", "Include Equipment and Material:",
                                  choices = list("No" = 1, "Yes" = 2),
                                  selected = 2)  ),

            fillRow(  selectInput("includeNotes", "Include Protocol Notes:",
                                  choices = list("No" = 1, "Yes" = 2),
                                  selected = 1)  )

          )
        )
      )


      server <- function(input, output, session) {

        output$overviewProtocol <- renderText({
          paste0( projectmanagr::getProtocolSummary(projNotePath, input$select) )
        })

        # Insert the Protocol from selected SOP
        observeEvent(input$done, {

          # insertProtocol:
          if(input$includeNotes == "1" && input$includeEquip == "1") {
            # NO - includeNotes is FALSE & includeNotes is FALSE
            cat( "  projectmanagr::insertProtocol():  includeNotes=FALSE, includeEquip=FALSE \n" )
            projectmanagr::insertProtocol(projNotePath, row, input$select, includeNotes=FALSE, includeEquip=FALSE)

          } else if(input$includeNotes == "2" && input$includeEquip == "1") {
            # YES - includeNotes is TRUE BUT includeNotes is FALSE
            cat( "  projectmanagr::insertProtocol():  includeNotes=TRUE, includeEquip=FALSE \n" )
            projectmanagr::insertProtocol(projNotePath, row, input$select, includeNotes=TRUE, includeEquip=FALSE)

          } else if(input$includeNotes == "1" && input$includeEquip == "2") {
            # NO - includeNotes is FALSE BUT includeNotes is TRUE
            cat( "  projectmanagr::insertProtocol():  includeNotes=FALSE, includeEquip=TRUE \n" )
            projectmanagr::insertProtocol(projNotePath, row, input$select, includeNotes=FALSE, includeEquip=TRUE)

          } else if(input$includeNotes == "2" && input$includeEquip == "2") {
            # YES - includeNotes is TRUE & includeNotes is TRUE
            cat( "  projectmanagr::insertProtocol():  includeNotes=TRUE, includeEquip=TRUE \n" )
            projectmanagr::insertProtocol(projNotePath, row, input$select, includeNotes=TRUE, includeEquip=TRUE)

          }

          # Close Gadget after computations are complete:
          stopApp()

        })
      }

      viewer <- dialogViewer("Create New Project Note", width = 1000, height = 800)

      runGadget(ui, server, viewer = viewer)

  }
}
