#' Link Project Document GDT to a Project Note
#'
#' Generates a Shiny Gadget for linking a Project Doc GDT to a project note.
#' The Link must be FROM a (Source) Project Docs' Goal/Del/Task TO a Project
#' Note (Single, SubNote, or Group Header).
#'
#' The User must select a Task in the Source Project Document. A link will then
#' be made to a User selected Project Note, which must be open in RStduio.
#'
#' The ADDIN stipulates any errors in the input, and can only be completed
#' when these errors have been resolved.
#'
#'
#' @export
addin_link_doc_to_note <- function() {

  cat( "\nprojectmanagr::addin_link_doc_to_note():\n" )


  #### check errors ####

  # Retrieve cursor_selection from current Active Doc in rstudio:
  selection <- cursor_selection() # must be Project Doc

  # show error if selected Project Note - Simple, Header, SubNote
  if(selection[["rmdType"]]=="UNKNOWN") { # return error message from selection
    addin_error("Link Project Note", selection[["errorMessage"]])
    stop( paste0("  ", selection[["errorMessage"]]))
  }

  if(selection[["rmdType"]]=="HEAD" || selection[["rmdType"]]=="SUB" ||
     selection[["rmdType"]]=="NOTE") { # return error message from selection
    addin_error("Link Project Note", "Project Note selected - select a project doc GDT to link.")
    stop( paste0("  ", "Project Note selected - select a project doc GDT to link."))
  }

  # show error if no project doc task is selected
  # indicated by selection[["addingSubNote"]] == TRUE
  if( selection[["addingSubNote"]] == TRUE ) {
    addin_error("Link Project Note", "Select a TASK in a Project Document.")
    stop( paste0("  ", "Select a TASK in a Project Document."))
  }


  #### instance variables ####

  # get the orgPath:
  orgPath <- find_org_directory(selection$filePath)

  # if orgPath not identified present error interface and then stop this function
  if(orgPath=="") {
    addin_error_path("Add Project Note",
                     "No Organisation identified - ensure active document is in an Organisation.",
                     selection$filePath)
    stop( paste0("  No Organisation identified - ensure active document is in an Organisation: \n    ", selection$filePath))
  }

  # get config templates settings yml
  confPath <- get_config_dir(orgPath)
  tempPath <- get_template_dir(orgPath)
  settings <- get_settings_yml(orgPath)

  # get status yml
  status <- get_status_yml(orgPath, settings)

  # get progPath
  progPath <- find_prog_dir(selection$filePath)


  # compute the goal/del/task NUM and TITLE:
  # extract GDT titles from selection
  goalTitle <- get_goal_title(selection[["goal"]], settings)
  delTitle <- get_deliverable_title(selection[["deliverable"]], settings)
  taskTitle <- get_deliverable_title(selection[["task"]], settings)


  # get open rstudio file paths filtered & ordered from active doc
  numberedFilePathList <- get_rstudio_doc_list_filtered_reordered(selection[["filePath"]], settings)

  # show error if no Notes in current Docs Org exist
  if( length(numberedFilePathList) == 0 ) {
    addin_error("Link Project Note", "No project notes are open from the current Doc's organisation.")
    stop( paste0("  ", "No project notes are open from the current Doc's organisation."))
  }

  # get numbered filename list
  numberedFileNameList <- numberedFilePathList
  names(numberedFileNameList) <- lapply(names(numberedFilePathList), basename)

  # get filepath & filename list
  filePathList <- names(numberedFilePathList)
  fileNameList <- lapply(filePathList, basename)


  #### user interface ####

  ui <- miniPage(

    gadgetTitleBar("Link Project Doc GDT to Project Note"),

    miniContentPanel(

      fillCol(

        fillRow( h5("Link Project Document GDT to Project Note.") ),

        fillRow(
          helpText(  h3("GOAL", align="center")   ),
          helpText(  h3("DELIVERABLE", align="center")   ),
          helpText(  h3("TASK", align="center")   )
        ),

        fillRow(
          helpText( p(goalTitle, align="center") ),
          helpText( p(delTitle, align="center") ),
          helpText( p(taskTitle, align="center") )
        ),

        fillRow(
          selectInput("select", "Select rstudio file to link:",
                      choices = numberedFileNameList,
                      selected = numberedFileNameList[1],
                      width="100%")
        )
      )
    )
  )


  #### server function ####

  server <- function(input, output, session) {

    # perform computations to create new Programme:
    observeEvent(input$done, {

      print(input$select)
      filePath <- filePathList[[as.integer(input$select)]]
      print(filePath)

      # determine if file is SIMPLE/SUBNOTE, HEADER NOTE or DOC:
      type <- get_file_type(filePath, settings )

      cat( "\nTYPE: ", type, "\n" )

      if( check_prog_sub_dir( filePath ) == "") {
        output$warningFile <- renderText({
          "*** FILE PATH NOT IN VALID PROGRAMME ***"
        })
      } else if(type == "DOC") {
        output$warningFile <- renderText({
          "*** CANNOT LINK DOC GDT TO DOC ***"
        })
      } else if(type == "UNKNOWN") {
        output$warningFile <- renderText({
          "*** CANNOT LINK DOC GDT TO UNKNOWN FILE ***"
        })
      } else {

        # link between project doc goal/del/task in selection & selected filePath
        if( type == "NOTE" ) {

            projectmanagr::link_doc_project_note( selection, filePath )

        } else if( type == "SUB" ) {

          projectmanagr::link_doc_sub_note( selection, filePath )

        } else if( type == "HEAD" ) {

            projectmanagr::link_doc_group_note( selection, filePath )
        }

        # Close Gadget after computations are complete:
        stopApp()

      }
    })
  }


  #### view gadget ####

  viewer <- dialogViewer("Project Doc GDT to Project Note",
                         width = settings[["GadgetWidth"]],
                         height = settings[["GadgetHeight"]])

  runGadget(ui, server, viewer = viewer)

}
