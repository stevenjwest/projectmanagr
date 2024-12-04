
test_that("test insert functions", {

  #### generate test organisation temp directory ####

  tmpdir <- create_tmpdir_rsess()

  #### create org prog doc note ####

  orgName <- "_T_O"
  # mock the function that returns the update datetime
  local_mocked_bindings(
    get_datetime = function (timezone = "UTC", split="-", splitTime=":") { "2024-02-22:09:56" } )


  # create test Organisation - using local helper function and withr package
  orgDir <- local_create_org(orgName, orgParentPath=tmpdir, syp=tmpdir)

  # define outputs to check
  orgIndex <- fs::path(orgDir, paste0("_index_", orgName, ".Rmd"))
  settingsYml <- fs::path(orgDir, ".config", "settings.yml")
  statusYml <- fs::path(orgDir, ".config", "status.yml")
  addinsJson <- fs::path(orgDir, ".config", "addins.json")
  volumesRmd <- fs::path(orgDir, "volumes", "volumes.Rmd")

  settings <- yaml::yaml.load( yaml::read_yaml( settingsYml ) )

  progName <- "0-PR"
  # mock the function that returns the programme creation datetime
  local_mocked_bindings(
    get_datetime = function (timezone = "UTC", split="-", splitTime=":") { "2024-02-22:09:58" } )


  # create test programme - using local helper function and withr package
  progDir <- local_create_prog(progName, orgDir)

  # define outputs to check
  progIndex <- fs::path(progDir, paste0("_index_", progName, ".Rmd"))

  # create test Project Doc
  projectDocPrefix <- "PrDoS"
  projectDocName <- "Proj_Do"
  projectDocRmd <- local_create_project(projectDocPrefix, projectDocName, progDir)
  projectDocDir <- fs::path(progDir, projectDocPrefix)

  # modify gdt titles for unique headers - so can test links!
  taskLine <- local_modify_project_doc_gdt_titles(settingsYml, projectDocRmd)

  # create test Project Note : simple
  projectNoteName <- "Proj_No"
  projectNotePath <- fs::path(projectDocDir, 't-no')
  fs::dir_create(projectNotePath)
  projectNoteRmd <- local_create_project_note_simple(projectNoteName, projectNotePath,
                                                     projectDocRmd, taskLine)
  projectNoteDir <- get_project_note_dir_path(projectNoteRmd, settings)



  #### insert content works ####

  # create test Project Doc
  projectDocPrefix <- "PDo"
  projectDocName <- "Proj_Do"
  projectDocRmd <- local_create_project(projectDocPrefix, projectDocName, progDir)
  projectDocDir <- fs::path(progDir, projectDocPrefix)




})

