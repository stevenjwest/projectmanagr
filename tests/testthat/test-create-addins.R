


test_that("create addins testing", {


  #### ________________ ####

  #### setup : mocked bindings and get WD ####

  cat( paste0('\n\ntestthat edition: ', testthat::edition_get()) )

  WD <- getwd()

  # mock rstudio navigation functions
  local_mocked_bindings(
    addin_rstudio_nav = function(orgIndexPath) { stopApp() }, # just stop the shiny gadget
    set_wd_active_doc = function() { }, # make a blank function - do not change working directory!
    get_css_theme = function() { NULL },
    get_username = function() { "sjwest" } # for consistent author name in Rmd files!
    # this adjusts addin look based on RStudio theme
    # want to this return the default theme for consistent testing
    )

  #### generate test organisation temp directory ####

  tmpdir <- create_tmpdir_rsess()
  tmpdirNoRoot <- fs::path_join(fs::path_split(tmpdir)[[1]][-1])

  #### ________________ ####

  #### test addin : CREATE PROJECT ORG ####


  ##### addin_create_project_org_ui() creates expected HTML #####

  expect_snapshot(addin_create_project_org_ui())


  ##### addin_create_project_org_server() creates Org ######

  orgName <- "_T_Ots"
  orgPath <- fs::path(tmpdir, orgName)

  shiny::testServer(addin_create_project_org_server, {

    # CHECK ERRORS
    session$setInputs(
      dir = list(path=c("root", tmpdirNoRoot)),
      organisationName="",
      organisationTitle="T O")
    Sys.sleep(0.2)
    session$setInputs(done=1)
    Sys.sleep(0.2)

    expect_equal(output$warning, "PROVIDE ORGANISATION NAME")

    session$setInputs(dir = list(path=c("root", tmpdirNoRoot)),
                      organisationName=orgName, organisationTitle="")
    Sys.sleep(0.2)
    session$setInputs(done=1)
    Sys.sleep(0.2)

    expect_equal(output$warning2, "PROVIDE ORGANISATION TITLE")

    # Create Organisation: set server inputs
    session$setInputs(dir = list(path=c("root", tmpdirNoRoot)),
                      organisationName=orgName, organisationTitle="T O")

    # execute the gadget - press done button
    Sys.sleep(0.2)
    session$setInputs(done=1)
    Sys.sleep(0.2)

    # can check reactive input setting values
    print( paste0("dir: ", input$dir))
    # print(input$dir)
    # print(input$organisationName)
    # print(input$organisationTitle)
    #
    # print(global$datapath)
    # print(output$dirO)
    #
    # print(global$datapath)
    #
    # print( paste0("OUTPUT WARNING: ", output$warning))
    # print( paste0("OUTPUT WARNING2: ", output$warning2))

    # define outputs to check
    orgIndex <- fs::path(orgPath, paste0("_index_", orgName, ".Rmd"))
    settingsYml <- fs::path(orgPath, ".config", "settings.yml")
    statusYml <- fs::path(orgPath, ".config", "status.yml")
    addinsJson <- fs::path(orgPath, ".config", "addins.json")
    volumesRmd <- fs::path(orgPath, "volumes", "volumes.Rmd")

    # to use `-` and `_` only in proj prefix and index seps
    modify_test_settings_yaml(orgPath)

    # check outputs
    # check org name correctly generates index Rmd
    expect_true( fs::file_exists(orgIndex) )

    # check index Rmd file contents are correctly filled
    expect_snapshot_file( orgIndex )

    # check .config files are created
    expect_true( fs::file_exists(settingsYml) )
    expect_true( fs::file_exists(statusYml) )
    expect_true( fs::file_exists(addinsJson) )

    # check volumes Rmd exists
    expect_true( fs::file_exists(volumesRmd) )

  })



  ##### addin_create_project_org() logic #####

  # mock ui and server functions to test higher infrastructure
   # return strings for checking args
  local_mocked_bindings(
    addin_create_project_org_ui = function() {
      paste0("ui ")
    },
    addin_create_project_org_server = function(input, output, session) {
      paste0(" server")
    },
    runGadget = function(UI, SERVER, viewer) {
      paste0(UI, SERVER())
    }
  )

  expect_snapshot( paste0("CREATE ORG ADDIN: ", addin_create_project_org() ) )



  #### define org variables ####

  # define test org variables in case useful:
  orgIndex <- fs::path(orgPath, paste0("_index_", orgName, ".Rmd"))
  settingsYml <- fs::path(orgPath, ".config", "settings.yml")
  statusYml <- fs::path(orgPath, ".config", "status.yml")
  addinsJson <- fs::path(orgPath, ".config", "addins.json")
  volumesRmd <- fs::path(orgPath, "volumes", "volumes.Rmd")
  settings <- get_settings_yml(orgPath)

  # set working directory to inside the newly created org
  setwd(orgPath)



  #### ________________ ####

  #### test addin : CREATE PROGRAMME ####


  ##### addin_create_programme_ui creates expected HTML #####

  expect_snapshot(addin_create_programme_ui(orgName))
  # using orgName to avoid current system full path - that can vary across systems!
   # so using orgName to render the UI makes for reproducible testing

  ##### addin_create_programme_server creates Prog  #####

  # define args
  progName <- "0-PR-ts"

  shiny::testServer(addin_create_programme_server, {

    ### Test server inputs

    # set name to blank string - check error
    session$setInputs(orgPath=orgPath, programmeName="")
    Sys.sleep(0.2)
    session$setInputs(done=1)
    Sys.sleep(0.2)

    expect_equal(output$warningName, "*** PROVIDE PROGRAMME NAME ***")

    # set name to string with space - check error
    session$setInputs(orgPath=orgPath, programmeName="123-test prog")
    Sys.sleep(0.2)
    session$setInputs(done=1)
    Sys.sleep(0.2)

    expect_equal(output$warningName, "*** PROGRAMME NAME CANNOT CONTAIN SPACES ***")

    # set name and title to acceptable values - check programme created correctly
    session$setInputs(orgPath=orgPath, programmeName=progName, programmeTitle="0 PR ts")
    Sys.sleep(0.2)
    session$setInputs(done=1)
    Sys.sleep(0.2)

    # define outputs to check
    progIndex <- fs::path(orgPath, progName, paste0("_index_", progName, ".Rmd"))

    # check programme name correctly generates index Rmd
    expect_true( fs::file_exists(progIndex) )

    # check index Rmd file contents are correctly filled
    expect_snapshot_file( progIndex )

  })


  ##### addin_create_programme() logic #####

  # mock ui and server functions to test higher infrastructure
  # return strings for checking args
  local_mocked_bindings(
    addin_create_programme_ui = function(orgPath) {
      paste0("ui - orgName: ", fs::path_file(orgPath) )
    }, # org file name for reproducible return across systems
    addin_create_programme_server = function(input, output, session) {
      paste0(" server")
    },
    runGadget = function(UI, SERVER, viewer) {
      paste0(UI, SERVER())
    }
  )

  expect_snapshot( paste0("CREATE PROGRAMME ADDIN: ", addin_create_programme() ) )



  ##### define programme variables #####

  progPath <- fs::path(orgPath, progName)
  progIndex <- fs::path(progPath, paste0("_index_", progName, ".Rmd"))


  #### ________________ ####

  #### test addin : CREATE PROGRAMME SECTION ####


  ##### addin_create_programme_section_ui creates expected HTML #####

  expect_snapshot(addin_create_programme_section_ui(orgName))
  # using orgName to avoid current system full path - that can vary across systems!
  # so using orgName to render the UI makes for reproducible testing

  ##### addin_create_programme_section_server creates Prog Sect  #####

  # define args
  sectName <- "pr-st"

  shiny::testServer(addin_create_programme_section_server, {

    ### Test server inputs

    # set name to blank string - check error
    session$setInputs(orgPath=orgPath, sectionName="")
    Sys.sleep(0.2)
    session$setInputs(done=1)
    Sys.sleep(0.2)

    expect_equal(output$warningName, "*** PROVIDE PROGRAMME SECTION NAME ***")

    # set name to string with space - check error
    session$setInputs(orgPath=orgPath, sectionName="123-test prog")
    Sys.sleep(0.2)
    session$setInputs(done=1)
    Sys.sleep(0.2)

    expect_equal(output$warningName, "*** PROGRAMME SECTION NAME CANNOT CONTAIN SPACES ***")

    # set name and title to acceptable values - check programme created correctly
    session$setInputs(orgPath=orgPath, sectionName=sectName, sectionTitle="pr st")
    Sys.sleep(0.2)
    session$setInputs(done=1)
    Sys.sleep(0.2)

    # define outputs to check
    sectIndex <- fs::path(orgPath, progName, sectName, paste0("_index_", sectName, ".Rmd"))

    # check programme section name correctly generates index Rmd
    expect_true( fs::file_exists(sectIndex) )

    # check index Rmd file contents are correctly filled
    expect_snapshot_file( sectIndex )

  })


  ##### addin_create_programme_section() logic #####

  # mock ui and server functions to test higher infrastructure
  # return strings for checking args
  local_mocked_bindings(
    addin_create_programme_section_ui = function(orgPath) {
      paste0("ui - orgName: ", fs::path_file(orgPath) )
    }, # org file name for reproducible return across systems
    addin_create_programme_section_server = function(input, output, session) {
      paste0(" server")
    },
    runGadget = function(UI, SERVER, viewer) {
      paste0(UI, SERVER())
    }
  )

  expect_snapshot( paste0("CREATE PROGRAMME SECTION ADDIN: ", addin_create_programme_section() ) )



  ##### define programme variables #####

  sectPath <- fs::path(orgPath, progName, sectName)
  progIndex <- fs::path(sectPath, paste0("_index_", sectName, ".Rmd"))


  #### ________________ ####

  #### test addin : CREATE PROJECT DOC ####


  ##### addin_create_project_doc_ui creates expected HTML #####

  #expect_snapshot(addin_create_project_doc_ui(orgName, get_settings_yml(orgPath), progPath, 1 ) )
  expect_snapshot(addin_create_project_doc_ui(orgName, get_settings_yml(orgPath)))
  # using orgName to avoid current system full path - that can vary across systems!
  # so using orgName to render the UI makes for reproducible testing


  ##### addin_create_project_doc_server creates ProjDoc  #####

  # define args
  projectDocPrefix <- "PrP"
  projectDocName <- "Project_Doc_Test"
  projectDocTitle <- "Project Doc Test"
  progPathCh <- as.character(progPath)

  setwd(progPathCh)

  cat("\n\n  programme path: ", progPath, "\n\n")

  shiny::testServer(addin_create_project_doc_server, {

    # set name to string with space - check error
    session$setInputs(
      dir = list(root=progPathCh, path=""),
      projectPrefix=projectDocPrefix,
      projectName="Project Doc_Test",
      projectTitle=projectDocTitle)

    # can check reactive input setting values
    # print( paste0("dir: ", input$dir))
    # print( paste0("dirROOT: ", input$dir$root))
    # print( paste0("dirPATH: ", input$dir$path))
    # print( paste0("projectPrefix: ", input$projectPrefix))
    # print( paste0("projectName: ", input$projectName))
    # print( paste0("projectTitle: ", input$projectTitle))
    # print( paste0("listCH: ", list(root=progPathCh, path="")))
    # print( paste0("list: ", list(root=progPath, path="")))
    # print( paste0("OUTPUT WARNING: ", output$warning))
    # print( paste0("OUTPUT WARNING2: ", output$warning2))

    Sys.sleep(0.2)
    session$setInputs(done=1)
    Sys.sleep(0.2)

    expect_equal(output$warningName, "*** PROJECT NAME CANNOT CONTAIN SPACES ***")

    # set name to blank string - check error
    session$setInputs(
      dir = list(root=progPathCh, path=""),
      projectPrefix=projectDocPrefix,
      projectName="",
      projectTitle=projectDocTitle)

    Sys.sleep(0.2)
    session$setInputs(done=1)
    Sys.sleep(0.5)

    expect_equal(output$warningName, "*** PROVIDE PROJECT NAME ***")


    # set prefix to blank string - check error
    session$setInputs(
      dir = list(root=progPathCh, path=""),
      projectPrefix="",
      projectName=projectDocName,
      projectTitle=projectDocTitle)

    Sys.sleep(0.2)
    session$setInputs(done=1)
    Sys.sleep(0.2)

    expect_equal(output$warningName, "*** PROVIDE PROJECT PREFIX ***")

    # set prefix to string with punctuation in - check error
    # set name to string with space - check error
    session$setInputs(
      dir = list(root=progPathCh, path=""),
      projectPrefix="PrP:",
      projectName=projectDocName,
      projectTitle=projectDocTitle)

    Sys.sleep(0.2)
    session$setInputs(done=1)
    Sys.sleep(0.2)

    expect_equal(output$warningName, "*** PROJECT PREFIX ONLY SUPPORTS ALPHANUMERICS ***")


    # set all vars to acceptable values - check project doc created correctly
    session$setInputs(
      dir = list(root=progPathCh, path=""),
      projectPrefix=projectDocPrefix,
      projectName=projectDocName,
      projectTitle=projectDocTitle)

    # print( paste0("projectPrefix: ", input$projectPrefix))
    # print( paste0("projectName: ", input$projectName))
    # print( paste0("projectTitle: ", input$projectTitle))
    # print( paste0("dir: ", input$dir))
    # print(global$datapath)

    Sys.sleep(0.2)
    session$setInputs(done=1)
    Sys.sleep(0.2)

    # define outputs to check
    projectDocDir <- fs::path(progPath, projectDocPrefix)
    projectDocRmd <- fs::path(progPath, paste0(projectDocPrefix, "_--_", projectDocName, ".Rmd") )

    ## TESTS ##

    # check project Doc Rmd & Dir generated
    expect_true(  fs::file_exists( projectDocRmd )  )
    expect_true(  fs::dir_exists( projectDocDir )  )

    # check Project Doc Rmd file contents are correctly filled
    expect_snapshot_file( projectDocRmd )

  })


  ##### addin_create_project_doc() logic #####

  # mock ui and server functions to test higher infrastructure
  # return strings for checking args
  local_mocked_bindings(
    addin_create_project_doc_ui = function(orgPath, settings) {
      paste0("ui - orgName: ", fs::path_file(orgPath), " settings-names: ", paste(names(settings), collapse=' '))
    }, # org & prog file names for reproducible return across systems
    addin_create_project_doc_server = function(input, output, session) {
      paste0(" server")
    },
    runGadget = function(UI, SERVER, viewer) {
      paste0(UI, SERVER())
    }
  )

  expect_snapshot( paste0("CREATE PROJECT DOC ADDIN: ", addin_create_project_doc() ) )


  ##### define project doc variables #####
  projectDocDir <- fs::path(progPath, projectDocPrefix)
  projectDocRmd <- fs::path(progPath, paste0(projectDocPrefix, "_--_", projectDocName, ".Rmd") )


  #### ________________ ####

  #### test addin : CREATE PROJECT NOTE ####


  ##### From Project Doc GDT ####

  # modify gdt titles for unique headers - so can test links!
  taskLine <- local_modify_project_doc_gdt_titles(settingsYml, projectDocRmd)

  # make selection object
  selection <- user_selection(projectDocRmd, taskLine)

  ###### addin_create_prn_doc_gdt_ui creates expected HTML ######

  goalTitle <- get_goal_title(selection[["goal"]], settings)
  delTitle <- get_deliverable_title(selection[["deliverable"]], settings)
  taskTitle <- get_deliverable_title(selection[["task"]], settings)

  expect_snapshot( addin_create_prn_doc_gdt_ui(goalTitle, delTitle, taskTitle) )


  ##### addin_create_prn_doc_gdt_server creates ProjNote  #####

  # define parameters
  projectNoteName <- "Proj_No"
  noteIndex="___001"
  projectNotePath <- fs::path(projectDocDir, 't-no')
  fs::dir_create(projectNotePath)

  roots <- c(projectDocDir, progPath, orgPath) # can use projectDirPath, progPath, or orgPath as roots
  names(roots) <- c(basename(projectDocDir), basename(progPath), basename(orgPath))

  projectDocDirCh <- as.character(projectDocDir)

  setwd(projectDocDirCh)

  cat("\n\n  set path to project doc dir: ", projectDocDirCh, "\n\n")

  # mock functions
  local_mocked_bindings(
    cursor_selection = function() { return(selection) } # return the existing selection!
  )

  shiny::testServer(addin_create_prn_doc_gdt_server, {

    # check the current input values
    #print( paste0("projectNoteName: ", input$projectNoteName))
    #print( paste0("datapath: ", global$datapath))
    #print( paste0("selection: ", paste(selection, collapse=' ')))
    #print( paste0("projectNoteTitle: ", input$projectNoteTitle))

    # # set name to blank string - check error
    session$setInputs(
      dir = list(root=basename(projectDocDirCh), path="t-no"),
      projectNoteName = "",
      subNoteName = "",
      prefixType = list(Single = 1) )

    Sys.sleep(0.2)
    session$setInputs(done=1)
    Sys.sleep(0.2)

    #print( paste0("warningName: ", output$warningName))

    expect_equal(output$warningName, "*** PROVIDE PROJECT NAME ***")

    # set name to string with space - check error
    session$setInputs(
      dir = list(root=basename(projectDocDirCh), path="t-no"),
      projectNoteName = "Proj No",
      subNoteName = "",
      prefixType = list(Single = 1) )

    Sys.sleep(0.2)
    session$setInputs(done=1)
    Sys.sleep(0.2)

    expect_equal(output$warningName, "*** PROJECT NAME CANNOT CONTAIN SPACES ***")

    # set all vars to acceptable values - check project note created correctly
     # dir reads root from roots by the name of roots list
     # dir reads everything except the FIRST item in path
    session$setInputs(
      dir = list(root=basename(projectDocDirCh), path=c("", "t-no") ),
      projectNoteName = projectNoteName,
      projectNoteTitle = gsub("-", " ", gsub("_", " ", projectNoteName) ),
      subNoteName = "",
      prefixType = list(Single = 1) )

    # check the current input values
    #print( paste0("projectNoteName: ", input$projectNoteName))
    #print( paste0("datapath: ", global$datapath))
    #print( paste0("selection: ", paste(selection, collapse=' ')))
    #print( paste0("projectNoteTitle: ", input$projectNoteTitle))

    Sys.sleep(0.2)
    session$setInputs(done=1)
    Sys.sleep(0.2)

    # define outputs to check
    projectNoteRmd <- fs::path(projectNotePath, paste0(basename(projectNotePath), noteIndex,"_--_", projectNoteName, ".Rmd") )
    projectNoteDir <- fs::path(projectNotePath, paste0(basename(projectNotePath), noteIndex) )

    cat(projectNoteRmd)
    cat(projectNoteDir)

    ## TESTS ##

    # check project Doc Rmd & Dir generated
    expect_true(  fs::file_exists( projectNoteRmd )  )
    expect_true(  fs::dir_exists( projectNoteDir )  )

    # check Project Doc Rmd file contents are correctly filled
    expect_snapshot_file( projectNoteRmd )

  })

  #### ________________ ####

  #### test addin : OPEN DAILY JOURNAL ####

  ##### addin_open_daily_journal_ui() creates expected HTML #####

  # define args
  calDate <- as.Date("2024-12-02")
  journalDir <- get_journal_dir(orgPath, settings)

  # Mock Calendar function
  mock_calendar_input <- function(inputId, value, ...) {
    div(
      class = "mock-calendar",
      paste0("Mock Calendar Input: ", inputId, ", Value: ", value)
    )
  }


  # mock geo location for fixed return - avoid using internet search!
  local_mocked_bindings(
    get_geo_loc = function(location_str) {
      return( list(lat=51.52, long=-0.14) )
    }, # for consistent latitude and longitude values
    get_locale = function() {
      return("Europe/London")
    }, # for consistent locale string
    get_sunlight_times = function(date, lat, lon, locale) {
      list(
        date = date,
        lat = lat,
        lon = lon,
        solarNoon = as.POSIXct("2024-12-02 11:51:34", tz = "Europe/London"),
        nadir = as.POSIXct("2024-12-01 23:51:34", tz = "Europe/London"),
        sunrise = as.POSIXct("2024-12-02 07:47:09", tz = "Europe/London"),
        sunset = as.POSIXct("2024-12-02 15:55:59", tz = "Europe/London"),
        sunriseEnd = as.POSIXct("2024-12-02 07:51:24", tz = "Europe/London"),
        sunsetStart = as.POSIXct("2024-12-02 15:51:45", tz = "Europe/London"),
        dawn = as.POSIXct("2024-12-02 07:07:59", tz = "Europe/London"),
        dusk = as.POSIXct("2024-12-02 16:35:10", tz = "Europe/London"),
        nauticalDawn = as.POSIXct("2024-12-02 06:25:38", tz = "Europe/London"),
        nauticalDusk = as.POSIXct("2024-12-02 17:17:31", tz = "Europe/London"),
        nightEnd = as.POSIXct("2024-12-02 05:45:20", tz = "Europe/London"),
        night = as.POSIXct("2024-12-02 17:57:48", tz = "Europe/London"),
        goldenHourEnd = as.POSIXct("2024-12-02 08:45:58", tz = "Europe/London"),
        goldenHour = as.POSIXct("2024-12-02 14:57:10", tz = "Europe/London"),
        check = "CHECK"
      )
    }, # for consistent day metadata
    get_moon_times = function(date, lat, lon, locale) {
      list(
        date = date,
        lat = lat,
        lon = lon,
        rise = as.POSIXct("2024-12-02 09:30:27", tz = "Europe/London"),
        set = as.POSIXct("2024-12-02 16:07:22", tz = "Europe/London"),
        alwaysUp = FALSE,
        alwaysDown = FALSE,
        check = "CHECK" # to check mocked function is working
      )
    },
    lunar_phase = function(date_obj) {
      factor("New", levels = c("New", "Waxing", "Full", "Waning"))
    },
    lunar_illumination = function(date_obj) {
      0.02454095
    })


  # Snapshot test for addin_open_weekly_journal_ui()
  expect_snapshot(addin_open_daily_journal_ui(orgName, calDate = calDate,
                                               calendar_function = mock_calendar_input))
  # using orgName to avoid current system full path - that can vary across systems!
   # so using orgName to render the UI makes for reproducible testing
  # must parameterise the calendar function call to shiny.fluent package
   # to mock the function for reproducible testing..
  # below code does not work!
   # if calendar function is directly called in ui function..
  # expect_snapshot(addin_open_weekly_journal_ui(orgPath))
  # unable to reproducibly test the ui, as calendar produces unique id
   # under jsmodule findAndRenderReactData()
   # <div class="react-container" data-react-id="uxllcjkxgshwrtuptkuc">
   # <script>jsmodule['@/shiny.react'].findAndRenderReactData('uxllcjkxgshwrtuptkuc')</script>


  ##### addin_open_daily_journal_server() creates Prog  #####


  shiny::testServer(addin_open_daily_journal_server, {

    ### Test server inputs

    session$setInputs(calendar=calDate)
    Sys.sleep(0.2)
    session$setInputs(done=1)
    Sys.sleep(0.2)

    # define outputs to check
    journalRmd <- fs::path(journalDir, "2024", "12",
                           paste0(calDate, "_", orgName, ".Rmd"))


    # check correctly generates journal Rmd
    expect_true( fs::file_exists(journalRmd) )

    # check journal Rmd file contents are correctly filled
    expect_snapshot_file( journalRmd )

  })


  ##### addin_open_daily_journal() logic #####

  # mock ui and server functions to test higher infrastructure
  # return strings for checking args
  local_mocked_bindings(
    addin_open_daily_journal_ui = function(orgPath) {
      paste0("ui - orgName: ", fs::path_file(orgPath) )
    }, # org file name for reproducible return across systems
    addin_open_daily_journal_server = function(input, output, session) {
      paste0(" server")
    },
    runGadget = function(UI, SERVER, viewer) {
      paste0(UI, SERVER())
    }
  )

  expect_snapshot( paste0("OPEN DAILY JOURNAL ADDIN: ", addin_open_daily_journal() ) )


  #### ________________ ####


  #### tidyup : set WD ####

  setwd(WD) # must set back to original WD at end otherwise test coverage fails!

})




