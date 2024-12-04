

test_that("datatables read rmd functions testing", {

  #### ________________ ####

  #### setup : mocked bindings and vars ####

  WD <- getwd()
  local_mocked_bindings(addin_rstudio_nav = function(orgIndexPath) { stopApp() } ) # prevent addins from opening the file - just stop the shiny gadget

  #### generate test organisation temp directory ####

  tmpdir <- create_tmpdir_rsess()
  tmpdirNoRoot <- fs::path_join(fs::path_split(tmpdir)[[1]][-1])

  ##### create test Org with Project Notes for datatable insertion ####

  orgName <- "_TORRF"
  #orgutime <- "2020-01-01:12:00" # for consistent datetime added to status.yml snapshot
  # mock the function that returns the update datetime
  local_mocked_bindings(
    get_datetime = function (timezone = "UTC", split="-", splitTime=":") { "2020-01-01:12:00" } )

  # create test Organisation - using local helper function and withr package
  orgDir <- local_create_org(orgName, orgParentPath=tmpdir, syp=tmpdir)

  orgIndex <- fs::path(orgDir, paste0("_index_", orgName, ".Rmd"))
  settingsYml <- fs::path(orgDir, ".config", "settings.yml")
  statusYml <- fs::path(orgDir, ".config", "status.yml")
  addinsJson <- fs::path(orgDir, ".config", "addins.json")
  volumesRmd <- fs::path(orgDir, "volumes", "volumes.Rmd")

  settings <- yaml::yaml.load( yaml::read_yaml( settingsYml ) )

  # create test Programme
  progName <- "0-PR"
  #progctime <- "2020-01-02:12:00" # for consistent datetime added to status.yml snapshot
  # mock the function that returns the programme creation datetime
  local_mocked_bindings(
    get_datetime = function (timezone = "UTC", split="-", splitTime=":") { "2020-01-02:12:00" } )

  progDir <- local_create_prog(progName, orgDir)
  progIndex <- fs::path(progDir, paste0("_index_", progName, ".Rmd"))

  # create test Project Doc for initial project note link
  projectDocPrefix <- "PD"
  projectDocName <- "Pr_Do"
  projectDocRmd <- local_create_project(projectDocPrefix, projectDocName, progDir)
  projectDocDir <- fs::path(progDir, projectDocPrefix)

  # modify gdt titles for unique headers
  taskLine <- local_modify_project_doc_gdt_titles(settingsYml, projectDocRmd)

  # create test Project Note directory
  projectNotePath <- fs::path(projectDocDir, 'tn-t')
  fs::dir_create(projectNotePath)


  # create test Project Note for datatables : EXPORT 1
  projectNoteNameEx1 <- "PN_ex1"
  projectNoteRmdEx1 <- local_create_project_note_simple(projectNoteNameEx1, projectNotePath,
                                                        projectDocRmd, taskLine)
  projectNoteDirEx1 <- get_project_note_dir_path(projectNoteRmdEx1, settings)


  # create test Project Note for datatables : EXPORT 2
  projectNoteNameEx2 <- "PN_ex2"
  projectNoteRmdEx2 <- local_create_project_note_simple(projectNoteNameEx2, projectNotePath,
                                                        projectDocRmd, taskLine,noteIndex="___002")
  projectNoteDirEx2 <- get_project_note_dir_path(projectNoteRmdEx2, settings)


  # create test Project Note for datatables : EXPORT 3
  projectNoteNameEx3 <- "PN_ex3"
  projectNoteRmdEx3 <- local_create_project_note_simple(projectNoteNameEx3, projectNotePath,
                                                        projectDocRmd, taskLine,noteIndex="___003")
  projectNoteDirEx3 <- get_project_note_dir_path(projectNoteRmdEx3, settings)


  # create test Project Note for datatables : IMPORT 1
  projectNoteNameIm1 <- "PN_im1"
  projectNoteRmdIm1 <- local_create_project_note_simple(projectNoteNameIm1, projectNotePath,
                                                        projectDocRmd, taskLine,noteIndex="___004")
  projectNoteDirIm1 <- get_project_note_dir_path(projectNoteRmdIm1, settings)

  #### ________________ ####

  #### test function : datatable find ####

  # add datatables to ex1-3 : including same datatable names, sub-samplings, and some ID clashes

  dt_find_add_create_dt1(projectNoteRmdEx1, 75)
  dt_find_add_create_dt2(projectNoteRmdEx2, 75)
  dt_find_add_create_dt3(projectNoteRmdEx3, 75)

  path <- fs::path_dir(projectNoteRmdEx1)

  summ_dt <- datatable_find(path, settings)

  expect_snapshot(summ_dt)
  expect_snapshot(attr(summ_dt, "path"))
  expect_snapshot(attr(summ_dt, "col_names"))

  # define variables
  # rmd_path <- projectNoteRmdIm1
  # row <- 75


})

