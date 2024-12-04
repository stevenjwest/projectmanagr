


test_that("create addins testing", {

  WD <- getwd()

  #### gadget mocked bindings of functions ####

  # mock the rstudio navigation function
  local_mocked_bindings(addin_rstudio_nav = function(orgIndexPath) stopApp() ) # just stop the shiny gadget

  #### generate test organisation temp directory ####

  tmpdir <- create_tmpdir_rsess()
  tmpdirNoRoot <- fs::path_join(fs::path_split(tmpdir)[[1]][-1])


  #### addin_create_project_org_ui creates expected HTML ####

  expect_snapshot(addin_create_project_org_ui())


  #### addin_create_project_org_server creates Org #####

  orgName <- "_T_Ots"
  orgDir <- fs::path(tmpdir, orgName)

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

    # # can check reactive input setting values
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
    orgIndex <- fs::path(orgDir, paste0("_index_", orgName, ".Rmd"))
    settingsYml <- fs::path(orgDir, ".config", "settings.yml")
    statusYml <- fs::path(orgDir, ".config", "status.yml")
    addinsJson <- fs::path(orgDir, ".config", "addins.json")
    volumesRmd <- fs::path(orgDir, "volumes", "volumes.Rmd")

    modify_test_settings_yaml(orgDir)

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


  #### modify yaml settings ####

  # defer removal of orgDir
  #withr::defer(fs::dir_delete(orgDir), envir = parent.frame())

  #modify_test_settings_yaml(orgDir) # seems to fail ??


  #### Org variables ####

  # define test org variables in case useful:
  orgIndex <- fs::path(orgDir, paste0("_index_", orgName, ".Rmd"))
  settingsYml <- fs::path(orgDir, ".config", "settings.yml")
  statusYml <- fs::path(orgDir, ".config", "status.yml")
  addinsJson <- fs::path(orgDir, ".config", "addins.json")
  volumesRmd <- fs::path(orgDir, "volumes", "volumes.Rmd")


  #### addin_create_programme_ui creates expected HTML ####

  expect_snapshot(addin_create_programme_ui(orgDir))

  #### addin_create_programme_server creates Prog  ####

  # define args
  progName <- "0-PR-ts"

  shiny::testServer(addin_create_programme_server, {

    ### Test server inputs

    # set name to blank string - check error
    session$setInputs(orgPath=orgDir, programmeName="")
    Sys.sleep(0.2)
    session$setInputs(done=1)
    Sys.sleep(0.2)

    expect_equal(output$warningName, "*** PROVIDE PROGRAMME NAME ***")

    # set name to string with space - check error
    session$setInputs(orgPath=orgDir, programmeName="123-test prog")
    Sys.sleep(0.2)
    session$setInputs(done=1)
    Sys.sleep(0.2)

    expect_equal(output$warningName, "*** PROGRAMME NAME CANNOT CONTAIN SPACES ***")

    # set name and title to acceptable values - check programme created correctly
    session$setInputs(orgPath=orgDir, programmeName=progName, programmeTitle="0 PR ts")
    Sys.sleep(0.2)
    session$setInputs(done=1)
    Sys.sleep(0.2)

    # define outputs to check
    progIndex <- fs::path(orgDir, progName, paste0("_index_", progName, ".Rmd"))

    # check programme name correctly generates index Rmd
    expect_true( fs::file_exists(progIndex) )

    # check index Rmd file contents are correctly filled
    expect_snapshot_file( progIndex )

  })


  #### programme variables ####

  progPath <- fs::path(orgDir, progName)
  progIndex <- fs::path(progPath, paste0("_index_", progName, ".Rmd"))


  #### addin_create_project_doc_ui creates expected HTML ####

  expect_snapshot(addin_create_project_doc_ui(orgDir, get_settings_yml(orgDir), progPath, 1 ) )


  #### addin_create_project_doc_server creates ProjDoc  ####

  # define args
  projectDocPrefix <- "PrP"
  projectDocName <- "Project_Doc_Test"
  projectDocTitle <- "Project Doc Test"
  progPathCh <- as.character(progPath)

  setwd(progPathCh)

  cat("\n\n  programme path: ", progPathCh, "\n\n")

  shiny::testServer(addin_create_project_doc_server, {

    # # set name to blank string - check error
    session$setInputs(
      dir = list(root=progPathCh, path=""),
      projectPrefix=projectDocPrefix,
      projectName="",
      projectTitle=projectDocTitle)

    Sys.sleep(0.2)
    session$setInputs(done=1)
    Sys.sleep(0.2)

    expect_equal(output$warningName, "*** PROVIDE PROJECT NAME ***")

    # set name to string with space - check error
    session$setInputs(
      dir = list(root=progPathCh, path=""),
      projectPrefix=projectDocPrefix,
      projectName="Project Doc_Test",
      projectTitle=projectDocTitle)

    Sys.sleep(0.2)
    session$setInputs(done=1)
    Sys.sleep(0.2)

    expect_equal(output$warningName, "*** PROJECT NAME CANNOT CONTAIN SPACES ***")

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


  setwd(WD)

})







### CURRENTLY DOES NOT WORK - issues with reactive values && shinyDirChoose() functionality

# this issue on a github repo may help resolve the integration of reactive values and shinyDirChoose function
# https://github.com/thomasp85/shinyFiles/issues/138

# suggests can define the shintDirChoose in a reactive context - and so use reactive values for its inputs
# However have not been able to get this to work right now
# So NOT testing the addins any further in this package for now...

# define args
# projectDocPrefix <- "PrP"
# projectDocName <- "Project_Doc_Test"
# progPathCh <- as.character(progPath)


# test_that("addin_create_project_doc_server creates Doc correctly", {
#
#   # mock the rstudio navigation function
#   local_mocked_bindings(addin_rstudio_nav = function(orgIndexPath) stopApp() ) # just stop the shiny gadget
#
#   shiny::testServer(addin_create_project_doc_server, {
#
#     # set name to blank string - check error
#     session$setInputs(dir = list(path=c(progPathCh, "")),
#                       projectPrefix=projectDocPrefix,
#                       projectName="",
#                       programmeDirPaths=progPathCh,
#                       progSelected="1"  )
#
#     print(input$projectPrefix)
#     print(input$projectName)
#     print(input$programmeDirPaths)
#     print(input$progSelected)
#
#     print(global$datapath)
#     print(dir())
#
#     Sys.sleep(0.2)
#     session$setInputs(done=1)
#     Sys.sleep(0.2)
#
#     expect_equal(output$warningName, "*** PROVIDE PROJECT NAME ***")
#
#     # set name to string with space - check error
#     session$setInputs(projectPrefix=projectDocPrefix,
#                       projectName="Project Doc_Test",
#                       programmeDirPaths=progPath,
#                       progSelected="1")
#     Sys.sleep(0.2)
#     session$setInputs(done=1)
#     Sys.sleep(0.2)
#
#     expect_equal(output$warningName, "*** PROJECT NAME CANNOT CONTAIN SPACES ***")
#
#     # set prefix to blank string - check error
#     session$setInputs(projectPrefix="",
#                       projectName=projectDocName,
#                       programmeDirPaths=progPath,
#                       progSelected="1")
#     Sys.sleep(0.2)
#     session$setInputs(done=1)
#     Sys.sleep(0.2)
#     expect_equal(output$warningName, "*** PROVIDE PROJECT PREFIX ***")
#
#     # set prefix to string with punctuation in - check error
#     session$setInputs(projectPrefix="PrP:",
#                       projectName=projectDocName,
#                       programmeDirPaths=progPath,
#                       progSelected="1")
#     Sys.sleep(0.2)
#     session$setInputs(done=1)
#     Sys.sleep(0.2)
#     expect_equal(output$warningName, "*** PROJECT PREFIX ONLY SUPPORTS ALPHANUMERICS ***")
#
#     # set all vars to acceptable values - check project doc created correctly
#     session$setInputs(projectPrefix=projectDocPrefix,
#                       projectName=projectDocName,
#                         programmeDirPaths=progPath,
#                       progSelected="1")
#     Sys.sleep(0.2)
#     session$setInputs(done=1)
#     Sys.sleep(0.2)
#
#     # define outputs to check
#     projectDocDir <- fs::path(progPath, projectDocPrefix)
#     projectDocRmd <- fs::path(progPath, paste0(projectDocPrefix, "~_", projectDocName, ".Rmd") )
#
#     ## TESTS ##
#
#     # check project Doc Rmd & Dir generated
#     expect_true(  fs::file_exists( projectDocRmd )  )
#     expect_true(  fs::dir_exists( projectDocDir )  )
#
#     # check Project Doc Rmd file contents are correctly filled
#     expect_snapshot_file( projectDocRmd )
#
#   })
# })


# projectDocDir <- fs::path(progPath, projectDocPrefix)
# projectDocRmd <- fs::path(progPath, paste0(projectDocPrefix, "~_", projectDocName, ".Rmd") )



### SHINYTEST2 ####

# this currently does not work - will move to using shinytest2 with recordApp() and testApp()
# as described in docs: https://rstudio.github.io/shinytest2/articles/shinytest2.html

# test_that("addin_create_project_org ADDIN creates Org correctly", {
#
#   # define args
#   orgName <- "_T_Osd"
#   orgutime <- "2024-02-22:09:56" # for consistent datetime added to status.yml snapshot
#
#   app <- shinytest2::AppDriver$new(shinyApp(addin_create_project_org_ui(),
#                                               addin_create_project_org_server),
#                                    timeout=10000)
#
#   app$set_inputs(dirInput = tmpdir)
#   app$set_inputs(organisationName=orgName, organisationTitle="T O")
#
#   app$click('done', wait_ = FALSE)
#
#   # define output variable
#   orgDir <- fs::path(tmpdir, orgName)
#
#   # defer removal of orgDir
#   withr::defer(fs::dir_delete(orgDir), envir = parent.frame())
#
#   # define outputs to check
#   orgIndex <- fs::path(orgDir, paste0("_index_", orgName, ".Rmd"))
#   settingsYml <- fs::path(orgDir, ".config", "settings.yml")
#   statusYml <- fs::path(orgDir, ".config", "status.yml")
#   addinsJson <- fs::path(orgDir, ".config", "addins.json")
#   volumesRmd <- fs::path(orgDir, "volumes", "volumes.Rmd")
#
#   # check outputs
#   # check org name correctly generates index Rmd
#   expect_true( fs::file_exists(orgIndex) )
#
#   # check index Rmd file contents are correctly filled
#   expect_snapshot_file( orgIndex )
#
#   # check .config files are created
#   expect_true( fs::file_exists(settingsYml) )
#   expect_true( fs::file_exists(statusYml) )
#   expect_true( fs::file_exists(addinsJson) )
#
#   # check volumes Rmd exists
#   expect_true( fs::file_exists(volumesRmd) )
#
# })


