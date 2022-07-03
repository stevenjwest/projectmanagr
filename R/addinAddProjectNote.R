#' Add a New Project Note
#'
#' Generates a Shiny Gadget for adding a new Project Note inside
#' a Project Doc.
#'
#' The User must select a Task or Project Header Note Link in a
#' Project Document.  If a Project Task is selected, the User
#' can build a new Project Note - either a Simple Note or a Group
#' Note (Header plus one SubNote).  If a Project Header Note Link
#' is selected, the User can ADD a new SubNote to this Group Note.
#'
#' User selects a destination in
#' the file system (MUST be a Programme Dir), Project name, and
#' Project title (for the html page).
#'
#' Stipulates any errors in the input, and can only be completed
#' when these errors have been resolved.
#'
#'
#' @export
addinAddProjectNote <- function() {


  # Retrieve cursorSelection from current Active Doc in rstudio:
    # is it:
      # a GROUP NOTE (HEADER or SUBNOTE) -> addSubNoteToGroup()
      # TASK or within bounds of a TASK - addProjectNote() OR addProjectNoteGroup()
  selection <- cursorSelection()

  # get the orgPath:
  orgPath <- findOrgDir(selection[["projectDocPath"]])

  # get progPath
  progPath <- findProgDir(selection[["projectDocPath"]])
  progPath <- normalizePath(progPath)

  # If no project Task is selected, present ERROR MESSAGE:

  if( selection[[1]] == "FALSE" ) {

    ui <- miniPage(
      gadgetTitleBar("Add New Project Note"),
      h2("Select a Task OR Project Header Link in a Project Document.", align="center", style="color:red")
    )

    server <- function(input, output, session) {

      observeEvent(input$done, {
        stopApp()
      })

    }

    viewer <- dialogViewer("Add New Project Note", width = 500, height = 300)

    runGadget(ui, server, viewer = viewer)

  }
  else {

    # compute the goal/del/task NUM and TITLE:
    goalNum <- as.integer(  substring(selection[["goal"]],  first=9, last=(regexpr(":", selection[["goal"]])-1) )  )
    taskNum <- as.integer(  substring(selection[["task"]],  first=11, last=(regexpr(":", selection[["task"]])-1) )  )
    delNum <- as.integer(  substring(selection[["deliverable"]],  first=17, last=(regexpr(":", selection[["deliverable"]])-1) )  )

    taskTitle <- substring(selection[["task"]],  first=(regexpr(":", selection[["task"]])+2 ) )
    delTitle <- substring(selection[["deliverable"]],  first=(regexpr(":", selection[["deliverable"]])+2 ) )
    goalTitle <- substring(selection[["goal"]],  first=(regexpr(":", selection[["goal"]])+2 ) )


    # if selection[["addingSubNote"]] is TRUE, then just want to ADD A SUBNOTE:
        # addSubNoteToGroup()

    if( selection[["addingSubNote"]] == TRUE ) {


      ### CREATE GADGET ###

      ui <- miniPage(

        gadgetTitleBar("Add New Sub Note"),

        miniContentPanel(

          fillCol( #flex=NA, # use the natural size of the elements in col

            fillRow( h5("Add a new Sub Note to a Project Note Group.") ),

            fillRow(
              helpText(  h3(  paste("GOAL", goalNum), align="center" )   ),
              helpText(  h3(  paste("DELIVERABLE", delNum), align="center" )   ),
              helpText(  h3(  paste("TASK", taskNum) ), align="center"   )
            ),

            fillRow(
              helpText( p(goalTitle, align="center") ),
              helpText( p(delTitle, align="center") ),
              helpText( p(taskTitle, align="center") )
            ),

            fillRow(  textInput("projectNoteName", "Sub Note Name:", width='100%')  ),

            fillRow(  span( textOutput("warningName"), style="color:red")  ),

            fillRow(  textInput("projectNoteTitle", "Sub Note Title:", width='100%')  ),

            # fillRow( flex = c(7, 1),  verbatimTextOutput("dir", placeholder = TRUE), shinyDirButton("dir", "Select Directory", "Note Parent Directory")  ),

            fillRow(   span( textOutput("warningDirectory"), style="color:red")  ),

            fillRow(   textOutput("projectNotePath")  )

          )

        )

      )


      ### ENCODE BEHAVIOUR ###

      server <- function(input, output, session) {

        # update projectNoteTitle when projectNoteName is changed:
        observe({

          updateTextInput(session, "projectNoteTitle", value = gsub("-", " ", gsub("_", " ", input$projectNoteName) )  )

        })

        # compute Dir selection:
        global <- reactiveValues(   datapath = findOrgDir(  selection[["projectDocPath"]]  )   )

        observe({
          if(global$datapath == "") {
            output$warningDirectory <- renderText({
              "PROJECT DOC NOT INSIDE PROJECT ORGANISATION"
            })
          }
          else {
            output$warningDirectory <- renderText({
              ""
            })
          }
        })


        observe({

          if( global$datapath != "" && input$projectNoteName != "" ) {

            # first, extract the RELATIVE path to the headerNote from projectDocPath:
            headerNoteRelPath <- substring(  selection[["headerNoteLink"]],
                                             first=(regexpr("](", selection[["headerNoteLink"]], fixed=TRUE)+2 ),
                                             last=nchar(selection[["headerNoteLink"]])-3 )

            subNotePath <- getSubNotePath(selection[["projectDocPath"]], headerNoteRelPath, input$projectNoteName )

            output$projectNotePath <- renderText({
              subNotePath
            })

          }

          else {

            output$projectNotePath <- renderText({
              ""
            })

          }
        })


        observe({

          if( grepl("\\s", input$projectNoteName)  ) {
            # set the warningName TextOutput:
            output$warningName <- renderText({
              "PROJECT NAME CANNOT CONTAIN SPACES"
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

          if(input$projectNoteName == "") {
            # set the warningName TextOutput:
            output$warningName <- renderText({
              "*** PROVIDE PROJECT NAME ***"
            })
          }
          else if( grepl("\\s", input$projectNoteName)  ) {
            # set the warningName TextOutput:
            output$warningName <- renderText({
              "*** PROJECT NAME CANNOT CONTAIN SPACES ***"
            })
          }
          else if(global$datapath == "") {
            output$warningDirectory <- renderText({
              "*** DIR PATH NOT VALID PROGRAMME ***"
            })
          }
          else {

              # compute the headerDirPath and nextSubNotePrefix:
              headerNotePath <- computePath(
                                       selection[["projectDocPath"]],
                                       substring(  selection[["headerNoteLink"]],
                                                   first=(regexpr("](", selection[["headerNoteLink"]], fixed=TRUE)+2 ),
                                                   last=nchar(selection[["headerNoteLink"]])-3 )
                                       )

              headerDirPath <- substring(headerNotePath, first=1, last=(regexpr("~_", headerNotePath, fixed=TRUE) -1 ))

              nextSubNotePrefix <- getNextGroupPrefix(headerDirPath)

              projectmanagr::addSubNoteToGroup(
                                      subNotePrefix = nextSubNotePrefix,
                                      subNoteName = input$projectNoteName,
                                      subNoteDir = headerDirPath,
                                      selection = selection,
                                      subNoteTitle = input$projectNoteTitle,
                                      subNoteTemp="Project-Sub-Note-Template.Rmd")

              rstudioapi::navigateToFile( paste( headerDirPath, .Platform$file.sep, nextSubNotePrefix, "~_", input$projectNoteName, ".Rmd", sep="") )

              # navigate to containing dir
              #rstudioapi::filesPaneNavigate(  paste( headerDirPath, sep="") )

            # Close Gadget after computations are complete:
            stopApp()

          }

        })

      }


      ### VIEW GADGET ###

      if(orgPath == "") {
        viewer <- dialogViewer("Add New Project Note", width = 1000,
                               height = 1000 )
      } else {
        confPath <- paste0( orgPath, .Platform$file.sep, "config" )
        settingsFile = paste( confPath, .Platform$file.sep, "settings.yml", sep="" )
        settingsContents <- yaml::yaml.load( yaml::read_yaml( settingsFile ) )

        viewer <- dialogViewer("Add New Project Note", width = settingsContents$gadgetWidth,
                               height = settingsContents$gadgetHeight )
      }

      runGadget(ui, server, viewer = viewer)

    }


    else {


      # else if selection[["addingSubNote"]] is FALSE, want to add a new Note - either SINGLE or GROUP:
          # addProjectNote()    OR    addProjectNoteGroup()


      ### CREATE GADGET ###

      ui <- miniPage(

        shinyjs::useShinyjs(),

        gadgetTitleBar("Add New Project Note"),

        miniContentPanel(

          fillCol( #flex=NA, # use the natural size of the elements in col

            fillRow( h5("Add a new Project Note to a Project Document.") ),

            fillRow(
              helpText(  h3(  paste("GOAL", goalNum), align="center" )   ),
              helpText(  h3(  paste("DELIVERABLE", delNum), align="center" )   ),
              helpText(  h3(  paste("TASK", taskNum) ), align="center"   )
            ),

            fillRow(
              helpText( p(goalTitle, align="center") ),
              helpText( p(delTitle, align="center") ),
              helpText( p(taskTitle, align="center") )
            ),

            fillRow(  selectInput("prefixType", "Select Project Note Type:",
                                  choices = list("Single" = 1, "Group" = 2),
                                  selected = 1, width = '50%')  ),

            fillRow( br() ),

            fillRow(  span( textOutput("warningName"), style="color:red")  ),

            fillRow(  textInput("projectNoteName", "Project Note Name:", value = "Note_Name", width='95%'),
                      textInput("projectNoteTitle", "Project Note Title:", value = "Note Name", width='95%')   ),

            fillRow(   span( textOutput("warningDirectory"), style="color:red")  ),

            fillRow( flex = c(5, 1),  verbatimTextOutput("dir", placeholder = TRUE), shinyDirButton("dir", "Select Directory", "Note Parent Directory")  ),

            fillRow(   textOutput("projectNotePath")  ),

            fillRow( br() ),

            fillRow(  textInput("subNoteName", "Project SubNote Name:", width='95%'),
                      textInput("subNoteTitle", "Project SubNote Title:", width='95%')  ),

            fillRow( br() ),

            fillRow(   textOutput("subNotePath")  ),

          ),

          padding = 10

        )

      )


      ### ENCODE BEHAVIOUR ###

      server <- function(input, output, session) {

        # update projectNoteTitle when projectNoteName is changed:
        observe({

          updateTextInput(session, "projectNoteTitle", value = gsub("-", " ", gsub("_", " ", input$projectNoteName) )  )

        })

        # update subNoteTitle when subNoteName is changed:
        observe({

          updateTextInput(session, "subNoteTitle", value = gsub("-", " ", gsub("_", " ", input$subNoteName) )  )

        })


        # compute Dir selection:
        global <- reactiveValues(datapath = checkProgSubDir(getProjectDocDirPath( selection[["projectDocPath"]] ) )  ) # this sets initial val to current working DIR

        # allows selection of Dir, with Volume set to HOME Dir
        shinyDirChoose(
          input,
          'dir',
          roots = c(home = orgPath), # set to orgPath - so user can select any DIR inside the ORG!
          filetypes = c('', 'txt', 'bigWig', "tsv", "csv", "bw")
        )

        dir <- reactive(input$dir)

        observeEvent(ignoreNULL = TRUE,
                     eventExpr = {
                       input$dir
                     },
                     handlerExpr = {
                       if (!"path" %in% names(dir())) return() # check the path element exists in dir
                       #home <- normalizePath("~")
                       global$datapath <-
                         file.path(orgPath, paste(unlist(dir()$path[-1]), collapse = .Platform$file.sep))
                     })




        observe({
          if(global$datapath != "") { # dir MUST be in a dir in a Programme
            global$datapath <- checkProgSubDir(global$datapath)
          }
        })

        output$dir <- renderText({
          global$datapath
        })

        observe({
          if(global$datapath == "") {
            output$warningDirectory <- renderText({
              "DIR PATH NOT VALID DIRECTORY - Must be INSIDE a PROGRAMME DIR"
            })
          }
          else {
            output$warningDirectory <- renderText({
              ""
            })
          }
        })


        observe({

          if(input$prefixType == "1") { # SINGLE NOTE selected, render projectNoteName as SINGLE note then DISABLE subnote names & title inputs

            if( global$datapath != "" && input$projectNoteName != "" ) {

              projNotePath <- getProjectNotePath(global$datapath, input$projectNoteName )

              output$projectNotePath <- renderText({
                projNotePath
              })

            }

            else {

              output$projectNotePath <- renderText({
                ""
              })

            }

            shinyjs::disable("subNoteName")
            shinyjs::disable("subNoteTitle")

          }

          else if(input$prefixType == "2") { # GROUP NOTE selected,

            # render projectNoteName as GROUP HEADER note, THEN enable subnote name/title and render subNoteName as SUB NOTE

            if( global$datapath != "" && input$projectNoteName != "" ) {

              headerNotePath <- getHeaderNotePath(global$datapath, input$projectNoteName )

              output$projectNotePath <- renderText({
                headerNotePath
              })

            }

            else {

              output$projectNotePath <- renderText({
                ""
              })

            }

            shinyjs::enable("subNoteName")
            shinyjs::enable("subNoteTitle")

            if( global$datapath != "" && input$subNoteName != "" ) {

              # first, extract the RELATIVE path to the headerNote from projectDocPath:
              headerNotePath <- getHeaderNotePath(global$datapath, input$projectNoteName )

              subNotePath <- computeSubNotePath(headerNotePath, input$subNoteName )

              output$subNotePath <- renderText({
                subNotePath
              })

            }

            else {

              output$subNotePath <- renderText({
                ""
              })

            }

          }

        })


        observe({

          if( grepl("\\s", input$projectNoteName)  ) {
            # set the warningName TextOutput:
            output$warningName <- renderText({
              "PROJECT NAME CANNOT CONTAIN SPACES"
            })
          }
          else {
            output$warningName <- renderText({
              ""
            })
          }
        })


        observe({

          if( grepl("\\s", input$subNoteName)  ) {
            # set the warningSubName TextOutput:
            output$warningName <- renderText({
              "SUBNOTE NAME CANNOT CONTAIN SPACES"
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

          if(input$projectNoteName == "") {
            # set the warningName TextOutput:
            output$warningName <- renderText({
              "*** PROVIDE PROJECT NAME ***"
            })
          }
          else if( grepl("\\s", input$projectNoteName)  ) {
            # set the warningName TextOutput:
            output$warningName <- renderText({
              "*** PROJECT NAME CANNOT CONTAIN SPACES ***"
            })
          }
          else if(input$prefixType == "2" && input$subNoteName == "") {
            # set the warningName TextOutput:
            output$warningName <- renderText({
              "*** PROVIDE SUBNOTE NAME ***"
            })
          }
          else if( input$prefixType == "2" && grepl("\\s", input$subNoteName)  ) {
            # set the warningName TextOutput:
            output$warningName <- renderText({
              "*** SUBNOTE NAME CANNOT CONTAIN SPACES ***"
            })
          }
          else if(global$datapath == "") {
            output$warningDirectory <- renderText({
              "*** DIR PATH NOT VALID PROGRAMME ***"
            })
          }
          else {


              if(input$prefixType == "1") {
              # If SINGLE note is Selected, addProjectNote():
              # call projectmanagr::addProjectNote:
                nextNotePrefix <- getNextSimplePrefix(global$datapath)
                projectmanagr::addProjectNote(
                                           projectNotePrefix = nextNotePrefix,
                                           projectNoteName = input$projectNoteName,
                                           projectNotePath = global$datapath,
                                           selection = selection,
                                           projectNoteTitle = input$projectNoteTitle,
                                           projNoteTemplate="Project-Note-Template.Rmd"
                                )
                rstudioapi::navigateToFile( paste( global$datapath, .Platform$file.sep,
                                                   nextNotePrefix, "~_",
                                                   input$projectNoteName, ".Rmd", sep="")
                                            )

                # navigate to containing dir
                #rstudioapi::filesPaneNavigate(  paste( global$datapath, sep="") )
              }

              else {
                # If GROUP note is Selected, addProjectNoteGroup():
                nextNotePrefix <- getNextSimplePrefix(global$datapath)
                projectmanagr::addProjectNoteGroup(
                                               projectNotePrefix = nextNotePrefix,
                                               projectNoteName = input$projectNoteName,
                                               projectNoteDir = global$datapath,
                                               selection = selection,
                                               subNoteName = input$subNoteName,
                                               projectNoteTitle = input$projectNoteTitle,
                                               subNoteTitle = input$subNoteTitle,
                                               projNoteTemplate="Project-Header-Note-Template.Rmd",
                                               subNoteTemplate="Project-Sub-Note-Template.Rmd"
                             )
                rstudioapi::navigateToFile( paste(
                          paste( global$datapath, .Platform$file.sep,
                                 paste(nextNotePrefix, "-00", sep=""), sep=""),
                          .Platform$file.sep, paste(nextNotePrefix, "-001", sep=""),
                          "~_", input$subNoteName, ".Rmd", sep="")
                          )

                # navigate to containing dir
                #rstudioapi::filesPaneNavigate(
                 #         paste( global$datapath, .Platform$file.sep,
                  #        paste(nextNotePrefix, "-00", sep=""), sep="")
                   #       )
              }



            # Close Gadget after computations are complete:
            stopApp()

          }

        })

      }


      ### VIEW GADGET ###

      if(orgPath == "") {
        viewer <- dialogViewer("Add New Project Note", width = 1000,
                               height = 1000 )
      } else {
        confPath <- paste0( orgPath, .Platform$file.sep, "config" )
        settingsFile = paste( confPath, .Platform$file.sep, "settings.yml", sep="" )
        settingsContents <- yaml::yaml.load( yaml::read_yaml( settingsFile ) )

        viewer <- dialogViewer("Add New Project Note", width = settingsContents$gadgetWidth,
                               height = settingsContents$gadgetHeight )
      }

      runGadget(ui, server, viewer = viewer)

    }


  }

}
