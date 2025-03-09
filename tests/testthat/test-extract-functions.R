
test_that("test extract functions", {


  #### generate test organisation temp directory ####

  tmpdir <- create_tmpdir_rsess()


  ################ generate test ORG PROG Doc Notes ################

  orgName <- "_T_O"
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

  subNoteRmd <- fs::path(groupNoteDir, paste0(basename(groupNotePath), "___002-001", "_--_", subNoteName, ".Rmd") )
  subNoteDir <- get_project_note_dir_path(subNoteRmd, settings)

  # get link to group header note from project doc
  headerLinkLine <- local_get_project_doc_file_link_line(projectDocRmd, groupNoteRmd, settings)

  subNoteName2 <- "SNo_02"
  subNoteName3 <- "SNo_03"
  subNotePath <- groupNoteDir

  # use local_create_project_note_group2 to output prefix: 002-002
  subNoteRmd2 <- local_create_project_note_sub2(subNoteName2, subNotePath,
                                                projectDocRmd, headerLinkLine,
                                                authorValue)

  # use local_create_project_note_group2 to output prefix: 002-003
  subNoteRmd3 <- local_create_project_note_sub_head_sel2(subNoteName3, subNotePath,
                                                         groupNoteRmd, 50,  authorValue)

  subNoteDir2 <- get_project_note_dir_path(subNoteRmd2, settings)
  subNoteDir3 <- get_project_note_dir_path(subNoteRmd3, settings)


  ################ extract_todos() extracts todos from location ################

  # add some todo items to project notes to extract
  local_add_todos_to_project_note(projectNoteRmd)
  local_add_todos_to_sub_note1(subNoteRmd)
  local_add_todos_to_sub_note2(subNoteRmd2)
  local_add_todos_to_sub_note3(subNoteRmd3)

  # extract todo items
  todoItems <- extract_todos(projectNotePath)


  ## TESTS ##

  # test todo items correctly extracted
  expect_snapshot(todoItems)


})
