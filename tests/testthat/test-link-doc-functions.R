

test_that("test link doc functions", {

  cat("=====================")
  cat("test link doc functions")
  cat("")


  #### generate test organisation temp directory ####

  tmpdir <- create_tmpdir_rsess()


  ################ generate test ORG PROG Doc Notes ################

  orgName <- "_TOl"
  authorValue="sjwest"
  # mock the function that returns the update datetime
  local_mocked_bindings(
    get_datetime = function (timezone = "UTC", split="-", splitTime=":") { "2024-02-22:09:56" } )


  # create test Organisation - using local helper function and withr package
  orgDir <- local_create_org(orgName, authorValue, orgParentPath=tmpdir, syp=tmpdir)

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
  progDir <- local_create_prog(progName, orgDir, authorValue)

  # define outputs to check
  progIndex <- fs::path(progDir, paste0("_index_", progName, ".Rmd"))


  # create test Project Doc
  projectDocPrefix <- "PrDoS"
  projectDocName <- "Proj_Do_sim"
  projectDocRmd <- local_create_project(
    projectDocPrefix, projectDocName, progDir,  authorValue)
  projectDocDir <- fs::path(progDir, projectDocPrefix)

  # modify gdt titles for unique headers - so can test links!
  taskLine <- local_modify_project_doc_gdt_titles(settingsYml, projectDocRmd)


  # create test Project Note : simple
  projectNoteName <- "Proj_No"
  projectNotePath <- fs::path(projectDocDir, 't-no')
  fs::dir_create(projectNotePath)
  projectNoteRmd <- local_create_project_note_simple(
    projectNoteName, projectNotePath, projectDocRmd,
    taskLine,  authorValue, noteIndex="___001")
  projectNoteDir <- get_project_note_dir_path(projectNoteRmd, settings)

  # create test Project Note : group
  groupNoteName <- "Gr_No_Head"
  groupNotePath <- fs::path(projectDocDir, 't-no')
  fs::dir_create(groupNotePath)
  subNoteName <- "SNo_01"
  # use local_create_project_note_group2 to output prefix: 002-00
  groupNoteRmd <- local_create_project_note_group2(groupNoteName, groupNotePath,
                                                   projectDocRmd, taskLine,
                                                   subNoteName,  authorValue)

  groupNoteDir <- get_project_note_dir_path(groupNoteRmd, settings)

  subNoteRmd <- fs::path(groupNoteDir, paste0(basename(groupNotePath),
                                              "___002-001", "_--_", subNoteName, ".Rmd") )
  subNoteDir <- get_project_note_dir_path(subNoteRmd, settings)


  ################ link_doc_project_note() works ###############################

  link_doc_project_note(
    selection = user_selection(projectDocRmd, 355), # is G3 D1 T1
    projNoteRmdPath = projectNoteRmd )

  ################ link_doc_group_note() works ######################

  link_doc_group_note(
    selection = user_selection(projectDocRmd, 355), # is G3 D1 T1
    headerNoteRmdPath = groupNoteRmd )

  ################ link_doc_sub_note() works #############################

  # check the link to this subnote via group note link is correctly detected
  link_doc_sub_note(
    selection = user_selection(projectDocRmd, 355), # is G3 D1 T1
    subNoteRmdPath = subNoteRmd )
  # [x] PASS

  link_doc_sub_note(
    selection = user_selection(projectDocRmd, 240), # is G2 D1 T1
    subNoteRmdPath = subNoteRmd )

  # check the whole group note link is added correctly when one subnote link already exists
  link_doc_group_note(
    selection = user_selection(projectDocRmd, 240), # is G3 D1 T1
    headerNoteRmdPath = groupNoteRmd )
   # [x] PASS

  ### TESTS

  # confirm Goal Deliverable Task Sections are written to projectDocRmd
  expect_snapshot_file( projectDocRmd )
  # [x] PASS

})



