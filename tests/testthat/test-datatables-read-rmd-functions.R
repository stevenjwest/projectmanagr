

test_that("datatables read rmd functions testing", {

  ############## ________________ ##############################################

  ############## setup : mocked bindings and vars ##############################

  WD <- getwd()
  local_mocked_bindings(
    addin_rstudio_nav = function(orgIndexPath) { stopApp() } # prevent addins from opening the file - just stop the shiny gadget
  )

  ############## generate test organisation temp directory #####################

  tmpdir <- create_tmpdir_rsess()
  #tmpdirNoRoot <- fs::path_join(fs::path_split(tmpdir)[[1]][-1])


  ############## create test Org with Project Notes for datatable insertion ####
  orgName <- "_TORRF"
  authorValue="sjwest"
  # mock the function that returns the update datetime
  # and function the generates status yaml content - write fixed org paths
  local_mocked_bindings(
    get_datetime = function (timezone = "UTC", split="-", splitTime=":") {
      "2020-01-01:12:00" },
    create_status_yaml_content = function(orgPaths, orgPath, orgName, orgTitle,
                                          updateTime, sitePath) {

      org <- list(c("/tmp/Rsess/_T_O", "/tmp/Rsess/_T_O2"),
                  "/tmp/Rsess/_T_O", orgName, orgTitle, updateTime,
                  "/tmp/Rsess/_T_O/site" )
      # orgPaths contains all EXISTING ORGs and THIS ORG path at end
      names(org) <- c("orgPaths", "orgPath", "orgName", "orgTitle", "updateTime",
                      "sitePath")
      return(org)
    } )

  # create test Organisation - using local helper function and withr package
  orgDir <- local_create_org(orgName, authorValue, orgParentPath=tmpdir, syp=tmpdir)

  orgIndex <- fs::path(orgDir, paste0("_index_", orgName, ".Rmd"))
  settingsYml <- fs::path(orgDir, ".config", "settings.yml")
  statusYml <- fs::path(orgDir, ".config", "status.yml")
  addinsJson <- fs::path(orgDir, ".config", "addins.json")
  volumesRmd <- fs::path(orgDir, "volumes", "volumes.Rmd")

  settings <- yaml::yaml.load( yaml::read_yaml( settingsYml ) )

  # create test Programme
  progName <- "0-PR"
  # mock the function that returns the programme creation datetime
  local_mocked_bindings(
    get_datetime = function (timezone = "UTC", split="-", splitTime=":") {
      "2024-02-22:09:58" } )

  progDir <- local_create_prog(progName, orgDir, authorValue)
  progIndex <- fs::path(progDir, paste0("_index_", progName, ".Rmd"))

  # create test Project Doc for initial project note link
  projectDocPrefix <- "PD"
  projectDocName <- "Pr_Do"
  projectDocRmd <- local_create_project(projectDocPrefix, projectDocName,
                                        progDir, authorValue)
  projectDocDir <- fs::path(progDir, projectDocPrefix)

  # modify gdt titles for unique headers
  taskLine <- local_modify_project_doc_gdt_titles(settingsYml, projectDocRmd)

  # create test Project Note directory
  projectNotePath <- fs::path(projectDocDir, 'tn-t')
  fs::dir_create(projectNotePath)


  ############## project notes : datatables read rmd ###########################

  # create test Project Note for datatables : CREATE
  projectNoteNameCre <- "PN_cre"
  projectNoteRmdCre <- local_create_project_note_simple(projectNoteNameCre, projectNotePath,
                                                        projectDocRmd, taskLine, authorValue)
  projectNoteDirCre <- get_project_note_dir_path(projectNoteRmdCre, settings)


  # create test Project Note for datatables : ADD DATA SAMPLES
  projectNoteNameADS <- "PN_ADS"
  projectNoteRmdADS <- local_create_project_note_simple(projectNoteNameADS, projectNotePath,
                                                        projectDocRmd, taskLine, authorValue,
                                                        noteIndex="___002")
  projectNoteDirADS <- get_project_note_dir_path(projectNoteRmdADS, settings)


  # create test Project Note for datatables : ADD DATA VARIABLES
  projectNoteNameADV <- "PN_ADV"
  projectNoteRmdADV <- local_create_project_note_simple(projectNoteNameADV, projectNotePath,
                                                        projectDocRmd, taskLine, authorValue,
                                                        noteIndex="___003")
  projectNoteDirADV <- get_project_note_dir_path(projectNoteRmdADV, settings)


  # create test Project Note for datatables : ADD DATA TIMETABLE
  projectNoteNameADT <- "PN_ADT"
  projectNoteRmdADT <- local_create_project_note_simple(projectNoteNameADT, projectNotePath,
                                                        projectDocRmd, taskLine, authorValue,
                                                        noteIndex="___004")
  projectNoteDirADT <- get_project_note_dir_path(projectNoteRmdADT, settings)


  # create test Project Note for datatables : ADD GROUP
  projectNoteNameADG <- "PN_ADG"
  projectNoteRmdADG <- local_create_project_note_simple(projectNoteNameADG, projectNotePath,
                                                        projectDocRmd, taskLine, authorValue,
                                                        noteIndex="___005")
  projectNoteDirADG <- get_project_note_dir_path(projectNoteRmdADG, settings)


  # create test Project Note for datatables : DISPOSE
  projectNoteNameDSP <- "PN_DSP"
  projectNoteRmdDSP <- local_create_project_note_simple(projectNoteNameDSP, projectNotePath,
                                                        projectDocRmd, taskLine, authorValue,
                                                        noteIndex="___006")
  projectNoteDirDSP <- get_project_note_dir_path(projectNoteRmdDSP, settings)


  # create test Project Note for datatables : RESAMPLE
  projectNoteNameRSP <- "PN_RSP"
  projectNoteRmdRSP <- local_create_project_note_simple(projectNoteNameRSP, projectNotePath,
                                                        projectDocRmd, taskLine, authorValue,
                                                        noteIndex="___007")
  projectNoteDirRSP <- get_project_note_dir_path(projectNoteRmdRSP, settings)



  ############## projects notes : import export samples ########################

  # create test Project Note directory : Export
  projectNotePathExport <- fs::path(projectDocDir, 'tn-ex')
  fs::dir_create(projectNotePathExport)


  # create test Project Note for datatables : EXPORT 1
  projectNoteNameEx1 <- "PN_ex1"
  projectNoteRmdEx1 <- local_create_project_note_simple(projectNoteNameEx1, projectNotePathExport,
                                                        projectDocRmd, taskLine, authorValue)
  projectNoteDirEx1 <- get_project_note_dir_path(projectNoteRmdEx1, settings)


  # create test Project Note for datatables : EXPORT 2
  projectNoteNameEx2 <- "PN_ex2"
  projectNoteRmdEx2 <- local_create_project_note_simple(projectNoteNameEx2, projectNotePathExport,
                                                        projectDocRmd, taskLine, authorValue,
                                                        noteIndex="___002")
  projectNoteDirEx2 <- get_project_note_dir_path(projectNoteRmdEx2, settings)


  # create test Project Note for datatables : EXPORT 3
  projectNoteNameEx3 <- "PN_ex3"
  projectNoteRmdEx3 <- local_create_project_note_simple(projectNoteNameEx3, projectNotePathExport,
                                                        projectDocRmd, taskLine, authorValue,
                                                        noteIndex="___003")
  projectNoteDirEx3 <- get_project_note_dir_path(projectNoteRmdEx3, settings)


  # create test Project Note directory : Export Sub
  projectSubPathExport <- fs::path(projectNotePathExport, 'tex')
  fs::dir_create(projectSubPathExport)

  # create test Project Sub for datatables : EXPORT 1
  projectSubNameEx1 <- "SN_ex1"
  projectSubRmdEx1 <- local_create_project_note_simple(projectSubNameEx1, projectSubPathExport,
                                                       projectDocRmd, taskLine, authorValue)
  projectSubDirEx1 <- get_project_note_dir_path(projectSubRmdEx1, settings)


  # create test Project Note directory : Import
  projectNotePathImport <- fs::path(projectDocDir, 'tn-im')
  fs::dir_create(projectNotePathImport)


  # create test Project Note for datatables : IMPORT 1
  projectNoteNameIm1 <- "PN_im1"
  projectNoteRmdIm1 <- local_create_project_note_simple(projectNoteNameIm1, projectNotePathImport,
                                                        projectDocRmd, taskLine, authorValue,
                                                        noteIndex="___001")
  projectNoteDirIm1 <- get_project_note_dir_path(projectNoteRmdIm1, settings)



  ############## ________________ ##############################################


  ############## test function : datatable find ################################

  # add datatables to ex1-3 :
  # including same datatable names, sub-samplings, and some ID clashes

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

