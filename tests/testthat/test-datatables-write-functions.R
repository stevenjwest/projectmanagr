

test_that("datatable write Rmd & write template Rmd", {

  #### generate test organisation temp directory ####

  tmpdir <- create_tmpdir_rsess()

  ##### create test Org with Project Notes for datatable insertion ####

  orgName <- "_T_O_DT"
  # mock the function that returns the update datetime
  local_mocked_bindings(
    get_datetime = function (timezone = "UTC", split="-", splitTime=":") { "2024-02-22:09:56" } )

  # create test Organisation - using local helper function and withr package
  orgDir <- local_create_org(orgName, orgParentPath=tmpdir, syp=tmpdir)

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

  progDir <- local_create_prog(progName, orgDir)
  progIndex <- fs::path(progDir, paste0("_index_", progName, ".Rmd"))

  # create test Project Doc for initial project note link
  projectDocPrefix <- "PD"
  projectDocName <- "Proj_Do"
  projectDocRmd <- local_create_project(projectDocPrefix, projectDocName, progDir)
  projectDocDir <- fs::path(progDir, projectDocPrefix)

  # modify gdt titles for unique headers
  taskLine <- local_modify_project_doc_gdt_titles(settingsYml, projectDocRmd)

  # create test Project Note directory
  projectNotePath <- fs::path(projectDocDir, 'tn-j')
  fs::dir_create(projectNotePath)


  # create test Project Note for datatables : CREATE
  projectNoteNameCre <- "PN_cre"
  projectNoteRmdCre <- local_create_project_note_simple(projectNoteNameCre, projectNotePath,
                                                        projectDocRmd, taskLine)
  projectNoteDirCre <- get_project_note_dir_path(projectNoteRmdCre, settings)


  # create test Project Note for datatables : ADD DATA SAMPLES
  projectNoteNameADS <- "PN_ADS"
  projectNoteRmdADS <- local_create_project_note_simple(projectNoteNameADS, projectNotePath,
                                                        projectDocRmd, taskLine,noteIndex="___002")
  projectNoteDirADS <- get_project_note_dir_path(projectNoteRmdADS, settings)


  # create test Project Note for datatables : ADD DATA VARIABLES
  projectNoteNameADV <- "PN_ADV"
  projectNoteRmdADV <- local_create_project_note_simple(projectNoteNameADV, projectNotePath,
                                                        projectDocRmd, taskLine,noteIndex="___003")
  projectNoteDirADV <- get_project_note_dir_path(projectNoteRmdADV, settings)


  # create test Project Note for datatables : ADD DATA TIMETABLE
  projectNoteNameADT <- "PN_ADT"
  projectNoteRmdADT <- local_create_project_note_simple(projectNoteNameADT, projectNotePath,
                                                        projectDocRmd, taskLine,noteIndex="___004")
  projectNoteDirADT <- get_project_note_dir_path(projectNoteRmdADT, settings)


  # create test Project Note for datatables : ADD GROUP
  projectNoteNameADG <- "PN_ADG"
  projectNoteRmdADG <- local_create_project_note_simple(projectNoteNameADG, projectNotePath,
                                                        projectDocRmd, taskLine,noteIndex="___005")
  projectNoteDirADG <- get_project_note_dir_path(projectNoteRmdADG, settings)


  # create test Project Note for datatables : DISPOSE
  projectNoteNameDSP <- "PN_DSP"
  projectNoteRmdDSP <- local_create_project_note_simple(projectNoteNameDSP, projectNotePath,
                                                        projectDocRmd, taskLine,noteIndex="___006")
  projectNoteDirDSP <- get_project_note_dir_path(projectNoteRmdDSP, settings)


  # create test Project Note for datatables : RESAMPLE
  projectNoteNameRSP <- "PN_RSP"
  projectNoteRmdRSP <- local_create_project_note_simple(projectNoteNameRSP, projectNotePath,
                                                        projectDocRmd, taskLine,noteIndex="___007")
  projectNoteDirRSP <- get_project_note_dir_path(projectNoteRmdRSP, settings)


  ### IMPORT EXPORT
  # create test Project Note for datatables : source
  projectNoteName <- "PN_src"
  projectNoteRmd <- local_create_project_note_simple(projectNoteName, projectNotePath,
                                                     projectDocRmd, taskLine,noteIndex="___008")
  projectNoteDir <- get_project_note_dir_path(projectNoteRmd, settings)


  # create test Project Note for datatables : destination (export/import testing)
  projectNoteNameDest <- "PN_des"
  projectNoteRmdDest <- local_create_project_note_simple(projectNoteNameDest, projectNotePath,
                                                         projectDocRmd, taskLine,noteIndex="___009")
  projectNoteDirDest <- get_project_note_dir_path(projectNoteRmdDest, settings)

  # create test Project Note for datatables : source
  projectNoteNameSrc <- "PN_src2"
  projectNoteRmdSrc <- local_create_project_note_simple(projectNoteNameSrc, projectNotePath,
                                                     projectDocRmd, taskLine,noteIndex="___010")
  projectNoteDirSrc <- get_project_note_dir_path(projectNoteRmdSrc, settings)


  ### INSERT TIBBLE
  # create test Project Note for datatables : INSERT TIBBLE
  projectNoteNameINT <- "PN_INT"
  projectNoteRmdINT <- local_create_project_note_simple(projectNoteNameINT, projectNotePath,
                                                        projectDocRmd, taskLine,noteIndex="___011")
  projectNoteDirINT <- get_project_note_dir_path(projectNoteRmdINT, settings)


  #### ________________ ####



  #### datatable_create_rmd() function calls ####

  # test standard dt generation
  rmd_path=projectNoteRmdCre
  rmd_line=75 # blank line after project note structures
  IDs=c(1001, 1002, 1003, 1004)
  data_cols=c("c", "cage", "genotype", "strain_breed_type", "dob_dt")# range of data col lengths
  datatable_name="samples"
  default_data_vals=list()
  dt_length = 100
  expand=FALSE
  datatable_create_rmd(rmd_path, rmd_line, datatable_name, data_cols, IDs,
                       default_data_vals, dt_length, expand)

  # add second set of sample IDs via CREATE datatable with default_data_vals
  rmd_line=95 # blank line after first CREATE datatable
  IDs=c(2001, 2002, 2003, 2004)
  default_data_vals=list(c("F", "F", "M", "M"),
                         c("CID101", "CID102", "CID103", "CID104"),
                         c("vgat:wt", "vgat:wt", "vgat:wt", "vgat:wt"),
                         c("c57bl1", "c57bl2", "c57bl3", "c57bl4"),
                         c("2024-08-21:12:11", "2024-08-21:12:12",
                           "2024-08-21:12:13", "2024-08-21:12:14"))
  datatable_create_rmd(rmd_path, rmd_line, datatable_name, data_cols, IDs,
                       default_data_vals, dt_length, expand)


  # add third set of sample IDs via CREATE datatable with default_data_vals
  # using expand is TRUE and default_data_vals vectors of length 1
  rmd_line=115 # blank line after CREATE datatables
  IDs=c(3001, 3002, 3003, 3004)
  default_data_vals=list(c("F"),c("CID101"), c("vgat:wt"),c("c57bl1"),
                         c("2024-08-21:12:11"))
  expand=TRUE
  datatable_create_rmd(rmd_path, rmd_line, datatable_name, data_cols, IDs,
                       default_data_vals, dt_length, expand)

  # add fourth set of sample IDs via CREATE datatable with many data cols
  # so it spills into making a second datatable ADD_DATA
  rmd_line=135 # blank line after CREATE datatables
  IDs=c(4001, 4002, 4003, 4004)
  data_cols=c("x","wt-g", "perfuse_dt", "perfusion_con", "group-fix",
              "postfix_dt", "postfix_con", "group-postfix")
  datatable_name="samples2" # new dt name so can read these datatables without error
  default_data_vals=list()
  expand=FALSE
  datatable_create_rmd(rmd_path, rmd_line, datatable_name, data_cols, IDs,
                       default_data_vals, dt_length, expand)


  #### datatable_create_rmd() tests ####

  # check Project Note Rmd file contents are correctly filled
  expect_snapshot_file(rmd_path)


  #### datatable_insert_from_tibble() function calls ####

  rmd_path=projectNoteRmdINT
  # Test generating CREATE datatable from tibble
  rmd_line=75 # blank line after intro
  tb <- tibble::tibble(ID=c("1001","1002","1003"), wt_g = c(1.23, 2.23, 3.23))
  dt_name <- "mice"
  dt_function = "CREATE"
  dt_length = 100
  datatable_insert_from_tibble(rmd_path, rmd_line, tb, dt_name,
                               dt_function, dt_length)


  #### datatable_insert_from_tibble() tests ####

  # check Project Note Rmd file contents are correctly filled
  expect_snapshot_file(rmd_path)


  #### datatable_dispose_rmd() function calls ####

  rmd_path=projectNoteRmdDSP

  # first add CREATE DATATABLES
  add_create_datatables(rmd_path)

  # now TEST DISPOSE FUNCTION
  rmd_line=175 # blank line after CREATE Datatables
  datatable_name = "samples"
  dt_length = 100
  summarise_reps=FALSE
  all_reps=FALSE
  cdt="2024-09-10:1330B"
  # test function
  datatable_dispose_rmd(rmd_path, rmd_line, datatable_name,
                        dt_length, summarise_reps, all_reps, cdt)

  # remove IDs 1001 - 2004 from DISPOSE datatable
  rm_lines(rmd_path, 183, 200)

  # dispose second time from same datatable - ensure previously disposed samples are OMITTED
  rmd_line=195
  datatable_name = "samples"
  dt_length = 100
  summarise_reps=TRUE
  all_reps=FALSE
  cdt="2024-09-10:1330B"
  # test function
  datatable_dispose_rmd(rmd_path, rmd_line, datatable_name,
                        dt_length, summarise_reps, all_reps, cdt)

  rm_lines(rmd_path, 203, 212)


  ### test function with samples with reps ###

  # first add RESAMPLE DATATABLE - for reps
  add_resample_datatables(rmd_path, 215)

  # now TEST DISPOSE FUNCTION
  # do not summarise reps or ALL reps
  rmd_line=247 # blank line after RESAMPLE Datatables
  datatable_name = "samples_CNS"
  dt_length = 100
  summarise_reps=FALSE
  all_reps=FALSE
  cdt="2024-09-10:1330B"
  # test function
  datatable_dispose_rmd(rmd_path, rmd_line, datatable_name,
                        dt_length, summarise_reps, all_reps, cdt)
  # remove IDs 1002 - 1004 from DISPOSE datatable
  rm_lines(rmd_path, 263, 288)

  # summarise reps
  rmd_line=267
  datatable_name = "samples_CNS"
  dt_length = 100
  summarise_reps=TRUE
  all_reps=FALSE
  cdt="2024-09-10:1331B"
  # test function
  datatable_dispose_rmd(rmd_path, rmd_line, datatable_name,
                        dt_length, summarise_reps, all_reps, cdt)
  # remove IDs 1003 - 1004 from DISPOSE datatable
  rm_lines(rmd_path, 277, 282)

  # ALL reps
  rmd_line=281
  datatable_name = "samples_CNS"
  dt_length = 100
  summarise_reps=TRUE
  all_reps=TRUE
  cdt="2024-09-10:1332B"
  # test function
  datatable_dispose_rmd(rmd_path, rmd_line, datatable_name,
                        dt_length, summarise_reps, all_reps, cdt)
  # remove ID 1004 from DISPOSE datatable
  rm_lines(rmd_path, 291, 294)


  #### datatable_dispose_rmd() tests ####

  # check Project Note Rmd file contents are correctly filled
  expect_snapshot_file(rmd_path)


  #### datatable_resample_rmd() function calls ####

  rmd_path=projectNoteRmdRSP

  # first add CREATE DATATABLES
  add_create_datatables(rmd_path)

  # now TEST RESAMPLE FUNCTION
  rmd_line=175 # blank line after CREATE Datatables
  datatable_name = "samples"
  # resampling to four sub-samples
  resample_vector=c("CNS", "SC-LUM", "DRG-L4-LT", "DRG-L4-RT")
  # variable reps
  rep_vector=c(1,2,2, 2)
  dt_length = 100
  summarise_reps=TRUE
  all_reps=FALSE
  # test function
  datatable_resample_rmd(rmd_path, rmd_line, datatable_name, resample_vector,
                         rep_vector, dt_length, summarise_reps, all_reps)

  # test resampling existing resampled data
  rmd_line=247
  datatable_name = "samples_CNS"
  # resampling to two sub-samples
  resample_vector=c("50µm", "100µm")
  # variable reps
  rep_vector=c(200,100)
  dt_length = 100
  summarise_reps=TRUE
  all_reps=FALSE
  # test function
  datatable_resample_rmd(rmd_path, rmd_line, datatable_name, resample_vector,
                         rep_vector, dt_length, summarise_reps, all_reps)

  # test resampling existing resampled data with reps
  rmd_line=295
  datatable_name = "samples_SC-LUM"
  # resampling to two sub-samples
  resample_vector=c("50µm", "100µm")
  # variable reps
  rep_vector=c(1,2)
  dt_length = 100
  summarise_reps=TRUE
  all_reps=FALSE
  # test function
  datatable_resample_rmd(rmd_path, rmd_line, datatable_name, resample_vector,
                         rep_vector, dt_length, summarise_reps, all_reps)

  # test resampling existing resampled data with reps - do not summarise reps
  rmd_line=343
  datatable_name = "samples_DRG-L4-LT"
  # resampling to two sub-samples
  resample_vector=c("50µm", "100µm")
  # variable reps
  rep_vector=c(1,2)
  dt_length = 100
  summarise_reps=FALSE
  all_reps=FALSE
  # test function
  datatable_resample_rmd(rmd_path, rmd_line, datatable_name, resample_vector,
                         rep_vector, dt_length, summarise_reps, all_reps)

  # test resampling existing resampled data with reps - declare ALL reps
  rmd_line=427
  datatable_name = "samples_DRG-L4-RT"
  # resampling to two sub-samples
  resample_vector=c("50µm", "100µm")
  # variable reps
  rep_vector=c(1,2)
  dt_length = 100
  summarise_reps=TRUE
  all_reps=TRUE
  # test function
  datatable_resample_rmd(rmd_path, rmd_line, datatable_name, resample_vector,
                         rep_vector, dt_length, summarise_reps, all_reps)


  ### TEST resample of EXISTING SAMPLES - add DISPOSE

  # add DISPOSE DATATABLES
  add_create_datatables(rmd_path, 475, "smp")
  add_dispose_datatables(rmd_path, rmd_line=575, datatable_name="smp")

  # now TEST RESAMPLE FUNCTION
  rmd_line=595 # blank line after CREATE Datatables
  datatable_name = "smp"
  # resampling to four sub-samples
  resample_vector=c("CNS", "SC-LUM", "DRG-L4-LT", "DRG-L4-RT")
  # variable reps
  rep_vector=c(1,2,2, 2)
  dt_length = 100
  summarise_reps=FALSE
  all_reps=FALSE
  # test function
  datatable_resample_rmd(rmd_path, rmd_line, datatable_name, resample_vector,
                         rep_vector, dt_length, summarise_reps, all_reps)
   # correctly resamples only existing samples!


  #### datatable_resample_rmd() tests ####

  # check Project Note Rmd file contents are correctly filled
  expect_snapshot_file(rmd_path)


  #### datatable_add_group_rmd() function calls ####

  rmd_path=projectNoteRmdADG

  # first add CREATE DATATABLES
  add_create_datatables(rmd_path)

  # now TEST ADD GROUP FUNCTIONS
  rmd_startline=175 # blank line after CREATE Datatables
  rmd_endline=175 # blank line after CREATE Datatables
  datatable_name = "samples"
  group_names=c("group-solvent-inc", "group-ab-conc")
  # 3-value and 2-value additions
  groups=list(c("1Hr","2Hr", "4Hr"),
              c(" 1mg/mL","0.5mg/mL"))
  dt_length=100
  summarise_reps=FALSE
  all_reps = FALSE
  # test function
  datatable_add_group_rmd(rmd_path, rmd_startline, rmd_endline,
                          datatable_name, group_names, groups, dt_length,
                          summarise_reps, all_reps)

  ### Test with disposed samples ###
  add_dispose_datatables(rmd_path, rmd_line=211, datatable_name="samples")

  # now TEST ADD GROUP FUNCTIONS
  rmd_startline=231 # blank line after CREATE Datatables
  rmd_endline=231 # blank line after CREATE Datatables
  datatable_name = "samples"
  group_names=c("group-solvent", "group-ab")
  # 3-value and 2-value additions
  groups=list(c("1Hr","2Hr", "4Hr"),
              c(" 1mg/mL","0.5mg/mL"))
  dt_length=100
  summarise_reps=FALSE
  all_reps = FALSE
  # test function
  datatable_add_group_rmd(rmd_path, rmd_startline, rmd_endline,
                          datatable_name, group_names, groups, dt_length,
                          summarise_reps, all_reps)
  # correctly groups only existing samples!


  ### test function with samples with reps ###

  # first add RESAMPLE DATATABLE - for reps
  add_resample_datatables(rmd_path, 259)

  # now TEST ADD GROUP FUNCTIONS
  rmd_startline=311 # blank line after CREATE Datatables
  rmd_endline=311 # blank line after CREATE Datatables
  datatable_name = "samples_SC-LUM"
  group_names=c("group-solvent", "group-ab")
  # 3-value and 2-value additions
  groups=list(c("1Hr","2Hr", "4Hr"),
              c(" 1mg/mL","0.5mg/mL"))
  dt_length=100
  summarise_reps=FALSE
  all_reps = FALSE
  # test function
  datatable_add_group_rmd(rmd_path, rmd_startline, rmd_endline,
                          datatable_name, group_names, groups, dt_length,
                          summarise_reps, all_reps)

  # summarise_reps
  rmd_startline=371 # blank line after CREATE Datatables
  rmd_endline=371 # blank line after CREATE Datatables
  datatable_name = "samples_CNS"
  group_names=c("group-solvent", "group-ab")
  # 3-value and 2-value additions
  groups=list(c("1Hr","2Hr", "4Hr"),
              c(" 1mg/mL","0.5mg/mL"))
  dt_length=100
  summarise_reps=TRUE
  all_reps = FALSE
  # test function
  datatable_add_group_rmd(rmd_path, rmd_startline, rmd_endline,
                          datatable_name, group_names, groups, dt_length,
                          summarise_reps, all_reps)

  # ALL reps
  rmd_startline=399 # blank line after CREATE Datatables
  rmd_endline=399 # blank line after CREATE Datatables
  datatable_name = "samples_CNS"
  group_names=c("group-solvent-inc", "group-ab-conc")
  # 3-value and 2-value additions
  groups=list(c("1Hr","2Hr", "4Hr"),
              c(" 1mg/mL","0.5mg/mL"))
  dt_length=100
  summarise_reps=TRUE
  all_reps = TRUE
  # test function
  datatable_add_group_rmd(rmd_path, rmd_startline, rmd_endline,
                          datatable_name, group_names, groups, dt_length,
                          summarise_reps, all_reps)

  ### test adding groups as string in rmd file ###

  # first add RESAMPLE LINES - text that describes groups
  add_group_lines(rmd_path, 427)

  # now TEST ADD GROUP FUNCTIONS
  rmd_startline=427 # blank line before resample lines
  rmd_endline=457 # blank line after resample lines
  datatable_name = "samples_DRG-L4-LT"
  group_names=c("group-postfix-time", "group-postfix-temp")
  # 3-value and 2-value additions
  groups=list(c("1day","3day", "7day"),
              c("RT","4C"))
  dt_length=100
  summarise_reps=FALSE
  all_reps = FALSE
  # test function
  datatable_add_group_rmd(rmd_path, rmd_startline, rmd_endline,
                          datatable_name, group_names, groups, dt_length,
                          summarise_reps, all_reps)

  #### datatable_add_group_rmd() tests ####

  # check Project Note Rmd file contents are correctly filled
  expect_snapshot_file(rmd_path)


  #### datatable_add_data_samples_rmd() function calls ####

  rmd_path=projectNoteRmdADS

  # first add CREATE DATATABLES
  add_create_datatables(rmd_path)

  # now TEST ADD DAT Samples FUNCTION
  # test standard dt generation
  rmd_line=175 # blank line after CREATE Datatables
  datatable_name = "samples"
  data_cols=c("w", "wt-g", "fix_con", "perfuse_con", "wt-g_dt" ) # range of data col lengths
  ids_vector=c(1001, 1002, 1003, 1004)
  default_data_vals=list() # test with blank list first
  dt_length = 100
  summarise_reps = FALSE
  all_reps = FALSE
  datatable_add_data_samples_rmd(rmd_path, rmd_line, datatable_name,
                                 data_cols, ids_vector, default_data_vals,
                                 dt_length, summarise_reps, all_reps)

  # test with default_data_vals set
  rmd_line=195 # blank line after Datatables
  data_cols=c("perfuse_wash_dt", "perfuse_wash_con", "perf_loc", "perf", "p")
  default_data_vals=list( c("2024-08-19:14:12"), c("PBS_RT"), c("L168:BENCH"),
                          c("PBS"), c("RT") )
  datatable_add_data_samples_rmd(rmd_path, rmd_line, datatable_name,
                                 data_cols, ids_vector, default_data_vals,
                                 dt_length, summarise_reps, all_reps)

  # test with default_data_vals set as multi vals
  rmd_line=215 # blank line after Datatables
  data_cols=c("berfuse_wash_dt", "berfuse_wash_con", "berf_loc", "berf", "b")
  default_data_vals=list(
    c("2024-08-19:14:12", "2024-08-19:14:12", "2024-08-19:14:12", "2024-08-19:14:12"),
    c("PBS_RT", "PBS_RT", "PBS_RT", "PBS_RT"), c("BENCH", "BENCH", "BENCH", "BENCH"),
    c("PBS", "PBS", "PBS", "PBS"), c("RT", "RT", "RT", "RT"))
  datatable_add_data_samples_rmd(rmd_path, rmd_line, datatable_name,
                                 data_cols, ids_vector, default_data_vals,
                                 dt_length, summarise_reps, all_reps)

  # test ALL
  rmd_line=247 # blank line after Datatables
  data_cols=c("cerfuse_wash_dt", "cerfuse_wash_con", "cerf_loc", "cerf", "ce")
  ids_vector="ALL"
  datatable_add_data_samples_rmd(rmd_path, rmd_line, datatable_name,
                                 data_cols, ids_vector, default_data_vals,
                                 dt_length, summarise_reps, all_reps)

  # test by GROUP
  rmd_line=264 # blank line after Datatables
  datatable_name = "samples2"
  data_cols=c("derfuse_wash_dt", "derfuse_wash_con", "derf_loc", "derf", "d")
  ids_vector="DATA__VALUE"
  datatable_add_data_samples_rmd(rmd_path, rmd_line, datatable_name,
                                 data_cols, ids_vector, default_data_vals,
                                 dt_length, summarise_reps, all_reps)

  # test subset IDs
  rmd_line=281 # blank line after Datatables
  datatable_name = "samples"
  data_cols=c("eerfuse_wash_dt", "eerfuse_wash_con", "eerf_loc", "eerf", "e")
  ids_vector=c("1001","1003")
  datatable_add_data_samples_rmd(rmd_path, rmd_line, datatable_name,
                                 data_cols, ids_vector, default_data_vals,
                                 dt_length, summarise_reps, all_reps)


  ### Test with disposed samples ###
  add_dispose_datatables(rmd_path, rmd_line=303, datatable_name="samples")

  # test standard dt generation
  rmd_line=323 # blank line after CREATE Datatables
  datatable_name = "samples"
  data_cols=c("y", "yt-g", "yix_con", "yerfuse_con", "yt-g_dt" ) # range of data col lengths
  ids_vector="" # blank to use all existing IDs
  default_data_vals=list() # test with blank list first
  dt_length = 100
  summarise_reps = FALSE
  all_reps = FALSE
  datatable_add_data_samples_rmd(rmd_path, rmd_line, datatable_name,
                                 data_cols, ids_vector, default_data_vals,
                                 dt_length, summarise_reps, all_reps)
  # correctly adds only to 1001-1004 & 2001-2004


  ### test function with samples with reps ###

  # first add RESAMPLE DATATABLE - for reps
  add_resample_datatables(rmd_path, 351)

  # test standard dt generation
  rmd_line=403 # blank line after CREATE Datatables
  datatable_name = "samples_SC-LUM"
  data_cols=c("y", "yt-g", "yix_con", "yerfuse_con", "yt-g_dt" ) # range of data col lengths
  ids_vector="" # blank to use all existing IDs
  default_data_vals=list() # test with blank list first
  dt_length = 100
  summarise_reps = FALSE
  all_reps = FALSE
  datatable_add_data_samples_rmd(rmd_path, rmd_line, datatable_name,
                                 data_cols, ids_vector, default_data_vals,
                                 dt_length, summarise_reps, all_reps)

  # summarise reps
  rmd_line=463 # blank line after CREATE Datatables
  datatable_name = "samples_CNS"
  data_cols=c("y", "yt-g", "yix_con", "yerfuse_con", "yt-g_dt" ) # range of data col lengths
  ids_vector="" # blank to use all existing IDs
  default_data_vals=list() # test with blank list first
  dt_length = 100
  summarise_reps = TRUE
  all_reps = FALSE
  datatable_add_data_samples_rmd(rmd_path, rmd_line, datatable_name,
                                 data_cols, ids_vector, default_data_vals,
                                 dt_length, summarise_reps, all_reps)

  # ALL reps
  rmd_line=491 # blank line after CREATE Datatables
  datatable_name = "samples_CNS"
  data_cols=c("z", "zt-g", "zix_con", "zerfuse_con", "zt-g_dt" ) # range of data col lengths
  ids_vector="" # blank to use all existing IDs
  default_data_vals=list() # test with blank list first
  dt_length = 100
  summarise_reps = TRUE
  all_reps = TRUE
  datatable_add_data_samples_rmd(rmd_path, rmd_line, datatable_name,
                                 data_cols, ids_vector, default_data_vals,
                                 dt_length, summarise_reps, all_reps)


  #### datatable_add_data_samples_rmd() tests ####

  # check Project Note Rmd file contents are correctly filled
  expect_snapshot_file(rmd_path)


  #### datatable_add_data_variables_rmd() function calls ####

  rmd_path=projectNoteRmdADV

  # first add CREATE DATATABLES
  add_create_datatables(rmd_path)

  # now TEST ADD DATA Variables FUNCTION
  # test standard dt generation
  rmd_line=175 # blank line after CREATE Datatables
  var_names=c("perfuse_wash_dt", "perfuse_wash_con", "p", "perf")
  datatable_name = "samples"
  # first test special group ALL
  group_names="ALL"
  default_data_vals=list()
  dt_length = 100
  datatable_add_data_variables_rmd(rmd_path, rmd_line, datatable_name,
                                   var_names, group_names, default_data_vals, dt_length)


  ### Test with groups ###

  # add groups
  rmd_startline=195 # blank line after CREATE Datatables
  rmd_endline=195 # blank line after CREATE Datatables
  datatable_name = "samples"
  group_names=c("group-solvent-inc", "group-ab-conc")
  # 3-value and 2-value additions
  groups=list(c("1Hr","2Hr", "4Hr"),
              c(" 1mg/mL","0.5mg/mL"))
  dt_length=100
  summarise_reps=FALSE
  all_reps = FALSE
  # test function
  datatable_add_group_rmd(rmd_path, rmd_startline, rmd_endline,
                          datatable_name, group_names, groups, dt_length,
                          summarise_reps, all_reps)

  # now TEST ADD DATA Variables FUNCTION
  # test standard dt generation
  rmd_line=231 # blank line after CREATE Datatables
  var_names=c("rerfuse_wash_dt", "rerfuse_wash_con", "r", "rerf")
  datatable_name = "samples"
  # first test special group ALL
  group_names=c("1Hr", "2Hr", "4Hr")
  default_data_vals=list()
  dt_length = 100
  datatable_add_data_variables_rmd(rmd_path, rmd_line, datatable_name,
                                   var_names, group_names, default_data_vals, dt_length)


  #### datatable_add_data_variables_rmd() tests ####

  # check Project Note Rmd file contents are correctly filled
  expect_snapshot_file(rmd_path)


  #### datatable_add_data_timetable_rmd() function calls ####

  rmd_path=projectNoteRmdADT

  # first add CREATE DATATABLES
  add_create_datatables(rmd_path)

  # now TEST ADD DAT Samples FUNCTION
  # test standard dt generation
  rmd_line=175 # blank line after CREATE Datatables
  datatable_name = "samples2"
  step_names=c("HT_RT", "MT_RT", "DMT_RT", "MT_RT", "HT_RT", "PBS_RT")
  # test group_names is vector of group IDs
  group_names=c("DATA__VALUE")
  col_name <- "delip"
  dt_length = 100
  datatable_add_data_timetable_rmd(rmd_path, rmd_line, datatable_name,
                                   step_names, group_names, col_name, dt_length)


  #### datatable_add_data_timetable_rmd() tests ####

  # check Project Note Rmd file contents are correctly filled
  expect_snapshot_file(rmd_path)


  #### datatable_import_export_rmd() function calls ####

  # as datatable_import_rmd() & datatable_export_rmd() ultimately work together
  # combined them into one function - datatable_import_export_rmd()
  # so running tests together too!

  source_rmd_path=projectNoteRmd # SOURCE project note
  destination_rmd_path=projectNoteRmdDest

  # first add CREATE DATATABLES to the source note
  add_create_datatables(source_rmd_path)

  # add blank lines to end of file - to test export/import works with source line set AFTER where EXPORT table is written!
  # actually DO NOT DO THIS : causes weird bug in devtools::test_coverage_active_file() call
   # does not run the code!  So OMIT THIs - the tests run fine wihtout it now anyway :D
  #insert_lines(source_rmd_path, rep("", 10), 175)
  #insert_lines(source_rmd_path, rep("", 10), 175)
  #insert_lines(source_rmd_path, rep("", 10), 175)
  #insert_lines(source_rmd_path, rep("", 10), 175)

  # test function : initial export of subset of sample IDs
  source_rmd_line=175 # blank line after CREATE Datatables
  destination_rmd_line=75 # blank line after project note structures
  datatable_name = "samples"
  ids_vector=c("1001","1002","1003","1004")
  reps_vector=""
  dt_length = 100
  summarise_reps=FALSE
  exportTemplate="Datatables-Export-Template.Rmd"
  # test function
  drl <- datatable_import_export_rmd(source_rmd_path, source_rmd_line, destination_rmd_path,
                              destination_rmd_line, settings, datatable_name, ids_vector,
                              reps_vector, dt_length, summarise_reps, exportTemplate)

  # test function : second export of subset of sample IDs
  source_rmd_line=210 # blank line after CREATE Datatables
  destination_rmd_line=100 # blank line after project note structures
  datatable_name = "samples"
  ids_vector=c("2001","2002","2003","2004")
  reps_vector=""
  dt_length = 100
  summarise_reps=FALSE
  exportTemplate="Datatables-Export-Template.Rmd"
  # test function
  drl <- datatable_import_export_rmd(source_rmd_path, source_rmd_line, destination_rmd_path,
                              destination_rmd_line, settings, datatable_name, ids_vector,
                              reps_vector, dt_length, summarise_reps, exportTemplate)


  # test function : third export of subset of sample IDs
  source_rmd_line=240 # blank line after CREATE Datatables
  destination_rmd_line=125 # blank line after project note structures
  datatable_name = "samples"
  ids_vector=c("3001","3002","3003")
  reps_vector=""
  dt_length = 100
  summarise_reps=FALSE
  exportTemplate="Datatables-Export-Template.Rmd"
  # test function
  drl <- datatable_import_export_rmd(source_rmd_path, source_rmd_line, destination_rmd_path,
                              destination_rmd_line, settings, datatable_name, ids_vector,
                              reps_vector, dt_length, summarise_reps, exportTemplate)


  # test import export : import from second note with same IDs correctly alters import sample IDs
  source_rmd_path=projectNoteRmdSrc # SECOND SOURCE project note
  destination_rmd_path=projectNoteRmdDest

  # first add CREATE DATATABLES to the source note
  add_create_datatables(source_rmd_path)

  # test function : initial export of subset of sample IDs
  source_rmd_line=175 # blank line after CREATE Datatables in new source note
  destination_rmd_line=150 # blank line after previous imports
  datatable_name = "samples"
  ids_vector=c("1001","1002","1003","1004") # same IDs in same datatable name as src 1
  reps_vector=""
  dt_length = 100
  summarise_reps=FALSE
  exportTemplate="Datatables-Export-Template.Rmd"
  # test function
  drl <- datatable_import_export_rmd(source_rmd_path, source_rmd_line, destination_rmd_path,
                              destination_rmd_line, settings, datatable_name, ids_vector,
                              reps_vector, dt_length, summarise_reps, exportTemplate)
  # correctly writes the import datatable IDs with a suffix of second src filename:
   # IDs : 1001.tnj - 1004.tnj

  #### datatable_import_export_rmd() tests ####

  # check Project Note Rmd files contents are correctly filled
  expect_snapshot_file(destination_rmd_path)
  expect_snapshot_file(projectNoteRmd) # src 1
  expect_snapshot_file(projectNoteRmdSrc) # src 2


})



