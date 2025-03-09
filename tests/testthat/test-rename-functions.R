
test_that("test rename functions", {


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
  renameDocName <- "RENAMED_PrDoS"
  # using special rename() function so withr:: knows to DELTE NEW FILE NAME
  projectDocRmd <- local_create_project_rename(projectDocPrefix, projectDocName,
                                               progDir,  authorValue,
                                               renamedDocName = renameDocName)
  projectDocDir <- fs::path(progDir, projectDocPrefix)

  # modify gdt titles for unique headers - so can test links!
  taskLine <- local_modify_project_doc_gdt_titles(settingsYml, projectDocRmd)


  # create test Project Note : simple
  projectNoteName <- "Proj_No"
  renameNoteName <- "RENAMED_Proj_No"
  projectNotePath <- fs::path(projectDocDir, 't-no')
  fs::dir_create(projectNotePath)
  # using special rename() function so withr:: knows to DELTE NEW FILE NAME
  projectNoteRmd <- local_create_project_note_simple_rename(
                          projectNoteName, projectNotePath, projectDocRmd,
                          taskLine,  authorValue,
                          noteIndex="___001", renameNoteName=renameNoteName)
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


  ################ add links ###################################################

  # add links to other files to confirm the links update correctly

  hpl <- create_hyperlink(projectDocName, projectDocRmd, progIndex)

  # programme index
  # link project doc
  local_insert_lines(progIndex, hpl, 67) # line 67 is in section: # Programme Background Information
  # link project note
  local_insert_lines(progIndex,
               create_hyperlink(projectNoteName, projectNoteRmd, progIndex),
               68) # line 68 is in section: # Programme Background Information


  # project doc
  # link project note
  local_insert_lines(projectDocRmd,
               create_hyperlink(projectNoteName, projectNoteRmd, projectDocRmd),
               73) # in section: # Project Background Information

  # project notes
  # group header
  # link project doc
  local_insert_lines(groupNoteRmd,
               create_hyperlink(projectDocName, projectDocRmd, groupNoteRmd),
               75)
  # link project note
  local_insert_lines(groupNoteRmd,
               create_hyperlink(projectNoteName, projectNoteRmd, groupNoteRmd),
               76) # in section: # Overview

  # subnote
  # link project doc
  local_insert_lines(subNoteRmd,
               create_hyperlink(projectDocName, projectDocRmd, subNoteRmd),
               88)
  # link project note
  local_insert_lines(subNoteRmd,
               create_hyperlink(projectNoteName, projectNoteRmd, subNoteRmd),
               89) # in section: # Introduction



  #################### rename_project_note() works #############################

  # rename project note to new name
  rename_project_note(projectNoteRmd, renameNoteName,
                      newProjectNoteTitle="", replaceLinksFileExtensions = list("Rmd") )
  renamedNoteRmd <- fs::path(paste0(projectNoteDir,"_--_", renameNoteName, ".Rmd"))

  ## TESTS ##

  # test old Rmd deleted, renamed Rmd exists
  expect_true(fs::file_exists(projectNoteRmd) == FALSE)
  expect_true(fs::file_exists(renamedNoteRmd) == TRUE)

  #################### rename_project_doc() works #############################

  # rename project note to new name
  rename_project_doc(projectDocRmd, renameDocName,
                     newProjectDocTitle="", replaceLinksFileExtensions = list("Rmd") )
  renamedDocRmd <- fs::path(progDir, paste0(projectDocPrefix, "_--_", renameDocName, ".Rmd") )


  ## TESTS ##

  # test old Rmd deleted, renamed Rmd exists
  expect_true(fs::file_exists(projectDocRmd) == FALSE)
  expect_true(fs::file_exists(renamedDocRmd) == TRUE)




  #################### rename_project_doc_goal() works #########################

  # goal with no links
  goalSelection <- user_selection(renamedDocRmd, 215)
  newGoalName <- "RENAMED GOAL HEADER"
  rename_project_doc_goal(goalSelection, newGoalName)

  # goal with links to project notes
  goalSelection <- user_selection(renamedDocRmd, 330)
  newGoalName <- "RENAMED EXAMPLE GOAL HEADER"
  rename_project_doc_goal(goalSelection, newGoalName)


  #################### rename_project_doc_deliverable() works ##################

  deliverableSelection <- user_selection(renamedDocRmd, 228)
  newDeliverableName <- "RENAMED DEL HEADER"
  rename_project_doc_deliverable(deliverableSelection, newDeliverableName)

  deliverableSelection <- user_selection(renamedDocRmd, 394)
  newDeliverableName <- "RENAMED EXAMPLE DEL HEADER"
  rename_project_doc_deliverable(deliverableSelection, newDeliverableName)


  #################### rename_project_doc_task() ###############################

  taskSelection <- user_selection(renamedDocRmd, 241)
  newTaskName <- "RENAMED TASK HEADER"
  rename_project_doc_task(taskSelection, newTaskName)

  taskSelection <- user_selection(renamedDocRmd, 426)
  newTaskName <- "EXAMPLE RENAMED TASK HEADER"
  rename_project_doc_task(taskSelection, newTaskName)


  #### add links to headers ####

  projDocSection <- "Project Background Information"
  projNoteSection <- "Introduction"

  # programme index
  # link project doc
  local_insert_lines(progIndex,
               create_hyperlink_section(renameDocName, projDocSection, renamedDocRmd, progIndex),
               69) # line 67 is in section: # Programme Background Information
  # link project note
  local_insert_lines(progIndex,
               create_hyperlink_section(renameNoteName, projNoteSection, renamedNoteRmd, progIndex),
               70) # line 68 is in section: # Programme Background Information

  # subnote
  # link project doc
  local_insert_lines(subNoteRmd,
               create_hyperlink_section(renameDocName, projDocSection, renamedDocRmd, subNoteRmd),
               90)
  # link project note
  local_insert_lines(subNoteRmd,
               create_hyperlink_section(renameNoteName, projNoteSection, renamedNoteRmd, subNoteRmd),
               91) # in section: # Introduction



  #################### rename_project_file_header() works ######################

  # on project doc
  selection <- user_selection(renamedDocRmd, 70)
  headerName <- "RENAMED Project Background Information RENAMED"
  rename_project_file_header(selection, headerName)

  # on project note
  selection <- user_selection(renamedNoteRmd, 73)
  headerName <- "RENAMED Introduction RENAMED"
  rename_project_file_header(selection, headerName)



  #### LINK TESTS ####

  # testing the links are correctly written in all the renamed files after all processing!

  # test the updated links are written correctly
  expect_snapshot_file(progIndex)
  expect_snapshot_file(renamedDocRmd)
  expect_snapshot_file(renamedNoteRmd)
  expect_snapshot_file(groupNoteRmd)
  expect_snapshot_file(subNoteRmd)



})
