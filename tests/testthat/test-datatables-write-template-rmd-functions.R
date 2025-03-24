

test_that("datatable write template Rmd", {

  ############## ________________ ##############################################

  ############## generate test organisation temp directory #####################

  tmpdir <- create_tmpdir_rsess()


  ############## create test Org with Project Notes for datatable insertion ####

  orgName <- "_T_O_DT"
  authorValue="sjwest"
  # mock the function that returns the update datetime
  # and function the generates status yaml content - write fixed org paths
  local_mocked_bindings(
    get_datetime = function (timezone = "UTC", split="-", splitTime=":") {
      "2024-02-22:09:56" },
    create_status_yaml_content = function(orgPaths, orgPath, orgName, orgTitle, updateTime, sitePath) {

      org <- list(c("/tmp/Rsess/_T_O", "/tmp/Rsess/_T_O2"),
                  "/tmp/Rsess/_T_O", orgName, orgTitle, updateTime, "/tmp/Rsess/_T_O/site" )
      # orgPaths contains all EXISTING ORGs and THIS ORG path at end
      names(org) <- c("orgPaths", "orgPath", "orgName", "orgTitle", "updateTime", "sitePath")
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
  progName <- "0-PR-DT"
  # mock the function that returns the programme creation datetime
  local_mocked_bindings(
    get_datetime = function (timezone = "UTC", split="-", splitTime=":") { "2024-02-22:09:58" } )

  progDir <- local_create_prog(progName, orgDir, authorValue)
  progIndex <- fs::path(progDir, paste0("_index_", progName, ".Rmd"))

  # create test Project Doc for initial project note link
  projectDocPrefix <- "PD"
  projectDocName <- "Proj_Do"
  projectDocRmd <- local_create_project(projectDocPrefix, projectDocName,
                                        progDir, authorValue)
  projectDocDir <- fs::path(progDir, projectDocPrefix)

  # modify gdt titles for unique headers
  taskLine <- local_modify_project_doc_gdt_titles(settingsYml, projectDocRmd)

  # create test Project Note directory
  projectNotePath <- fs::path(projectDocDir, 'tn-j')
  fs::dir_create(projectNotePath)


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


  ### IMPORT EXPORT
  # create test Project Note for datatables : source
  projectNoteName <- "PN_src"
  projectNoteRmd <- local_create_project_note_simple(projectNoteName, projectNotePath,
                                                     projectDocRmd, taskLine, authorValue,
                                                     noteIndex="___008")
  projectNoteDir <- get_project_note_dir_path(projectNoteRmd, settings)


  # create test Project Note for datatables : destination (export/import testing)
  projectNoteNameDest <- "PN_des"
  projectNoteRmdDest <- local_create_project_note_simple(projectNoteNameDest, projectNotePath,
                                                         projectDocRmd, taskLine, authorValue,
                                                         noteIndex="___009")
  projectNoteDirDest <- get_project_note_dir_path(projectNoteRmdDest, settings)


  ############## ________________ ##############################################


  ############## datatable_create_template_rmd() function calls ################

  # test standard dt generation
  rmd_path=projectNoteRmdCre

  # first add CREATE DATATABLE + TEMPLATE
  add_dt_create_test(rmd_path, 75)
  add_template_create_datatables(rmd_path, 96)
  add_template_create_datatables_ids(rmd_path, 120)

  # test filling template DT using existing DT IDs
  rmd_line=103 # line inside TEMPLATE
  datatable_name = "samples_CNS"
  template_datatable_name="lipid-extracts"
  IDs=NULL
  all_ids=FALSE
  dt_length = 100

  # test function
  datatable_create_template_rmd(
    rmd_path, rmd_line, datatable_name, template_datatable_name,
    IDs, all_ids, dt_length)
  # SUCCESS
   # [x] writes new CREATE datatable in place of TEMPLATE  :  CREATE
   # [x] correctly re-writes <<IDS>> using ID from inpur datatable: samples_CNS
   # [x] correctly adds <<ID>>-CNS & <<ID>>-CTL ID values

  # test filling template DT using specified IDs
  rmd_line=131 # line inside TEMPLATE - extended as above function increases Rmd lines!
  datatable_name = NULL # set to NULL to use IDs param
  template_datatable_name="fix-sol-wts" # this is fixed - default read by ADDIN
  IDs=c(1011, 1012, 1013, 1014) # vector of IDs to fill new DT with
  all_ids=FALSE
  dt_length = 100

  # test function
  datatable_create_template_rmd(
    rmd_path, rmd_line, datatable_name, template_datatable_name,
    IDs, all_ids, dt_length)
  # SUCCESS
  # [x] writes new CREATE datatable in place of TEMPLATE  :  CREATE
  # [x] correctly re-writes <<IDS>> using IDs vector
  # [x] correctly adds <<ID>>-CNS & <<ID>>-CTL ID values


  ############## datatable_create_template_rmd() run tests #####################

  # check Project Note Rmd file contents are correctly filled
  expect_snapshot_file(rmd_path)


  ############## datatable_dispose_template_rmd() function calls ###############

  # test standard dt generation
  rmd_path=projectNoteRmdDSP

  # first add CREATE DATATABLES
  add_template_dispose_datatables(rmd_path, 75)

  datatable_name = "samples_CNS"
  dt_length=100
  summarise_reps=FALSE
  all_reps=FALSE

  ### test errors
  # dispose col name
  rmd_line=100 # line inside TEMPLATE
  expect_error(datatable_dispose_template_rmd(rmd_path, rmd_line,
                                              datatable_name, dt_length,
                                              summarise_reps, all_reps))
  # multi obs input
  rmd_line=122 # line inside TEMPLATE
  expect_error(datatable_dispose_template_rmd(rmd_path, rmd_line,
                                              datatable_name, dt_length,
                                              summarise_reps, all_reps))

  # test function
  rmd_line=145 # line inside TEMPLATE
  datatable_dispose_template_rmd(rmd_path, rmd_line,
                                 datatable_name, dt_length,
                                 summarise_reps, all_reps)


  ############## datatable_dispose_template_rmd() run tests ####################

  # check Project Note Rmd file contents are correctly filled
  expect_snapshot_file(rmd_path)


  ############## datatable_resample_template_rmd() function calls ##############

  # test standard dt generation
  rmd_path=projectNoteRmdRSP

  # first add CREATE + RESAMPLE Template DATATABLES
  add_template_resample_datatables(rmd_path, 75)

  datatable_name = "samples"
  dt_length=100
  summarise_reps=FALSE
  all_reps=FALSE
  ### test errors
  # resample reps col names
  rmd_line=101 # line inside TEMPLATE
  expect_error(datatable_resample_template_rmd(rmd_path, rmd_line,
                                              datatable_name, dt_length,
                                              summarise_reps, all_reps))
  # multi obs input same in both cols
  rmd_line=127 # line inside TEMPLATE
  expect_error(datatable_resample_template_rmd(rmd_path, rmd_line,
                                              datatable_name, dt_length,
                                              summarise_reps, all_reps))

  # test function
  rmd_line=153 # line inside TEMPLATE
  datatable_resample_template_rmd(rmd_path, rmd_line,
                                 datatable_name, dt_length,
                                 summarise_reps, all_reps)


  ############## datatable_resample_template_rmd() run tests ###################

  # check Project Note Rmd file contents are correctly filled
  expect_snapshot_file(rmd_path)


  ############## datatable_add_data_samples_template_rmd() function calls ######

  # test standard dt generation
  rmd_path=projectNoteRmdADS

  # first add CREATE DATATABLES
  add_template_add_data_samples_datatables(rmd_path, 75)

  rmd_line=101 # line inside TEMPLATE
  datatable_name = "samples_CNS"
  dt_length=100
  summarise_reps=FALSE
  all_reps=FALSE
  # test function
  datatable_add_data_samples_template_rmd(rmd_path, rmd_line,
                                          datatable_name, dt_length,
                                          summarise_reps, all_reps)


  ############## datatable_add_data_samples_template_rmd() run tests ###########

  # check Project Note Rmd file contents are correctly filled
  expect_snapshot_file(rmd_path)


  ############## datatable_add_group_template_rmd() function calls #############

  # test standard dt generation
  rmd_path=projectNoteRmdADG

  # first add CREATE + RESAMPLE Template DATATABLES
  add_template_add_group_datatables(rmd_path, 75)

  rmd_line=101 # line inside TEMPLATE
  datatable_name = "samples"
  dt_length=100
  summarise_reps=FALSE
  all_reps=FALSE
  # test function
  datatable_add_group_template_rmd(rmd_path, rmd_line,
                                  datatable_name, dt_length,
                                  summarise_reps, all_reps)


  ############## datatable_add_group_template_rmd() run tests ##################

  # check Project Note Rmd file contents are correctly filled
  expect_snapshot_file(rmd_path)

})



