context("Test insert functions")

test_that("test insert functions", {

  message("=====================")
  message("test insert functions")
  message("")

  #### generate test organisation temp directory ####

  tmpdir <- create_tmpdir_rsess()


  ################ generate test ORG PROG Doc Notes ################

  orgName <- "_TOi"
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

  subNoteRmd <- fs::path(groupNoteDir,
                         paste0(basename(groupNotePath), "___002-001",
                                "_--_", subNoteName, ".Rmd") )
  subNoteDir <- get_project_note_dir_path(subNoteRmd, settings)


  ################ insert_doc_goal_section() works #############################

  insert_doc_goal_section(
    selection = user_selection(projectDocRmd, 329) ) # 329 is selecting last GOAL


  ################ insert_doc_deliverable_section() works ######################

  insert_doc_deliverable_section(
    selection = user_selection(projectDocRmd, 329) ) # 329 is selecting last GOAL


  ################ insert_doc_task_section() works #############################

  insert_doc_task_section(
    selection = user_selection(projectDocRmd, 329) ) # 329 is selecting last GOAL


  ################ insert_content() works ######################################

  # create content in Project Note
  contentName <- "ex-con"
  contentDescription <- "Example Content Description"
  contentSourcePath <- projectNoteDir
  noteLine <- 80 # set to a line below the Introduction default header but within Rmd lines
  contentTitle <- "Example Content"

  contentRmd <- local_create_content(contentName, contentDescription, contentSourcePath,
                                     projectNoteRmd, noteLine, contentTitle)

  # add some content to contentRmd!
  exampleContent <- c(
    "",
    "",
    "",
    "# Example Content",
    "",
    "",
    "-------------------------------------------------------------------------------",
    "-------------------------------------------------------------------------------",
    "",
    "",
    "",
    "# Example Content SOP",
    "",
    "",
    "-----------------------------------------------------------------------",
    "-----------------------------------------------------------------------",
    "",
    "",
    "",
    "# Example Content LOG",
    "",
    "",
    "-----------------------------------------------------------------------",
    "-----------------------------------------------------------------------",
    "",
    "",
    "",
    "",
    "",
    "" )

  write_file(exampleContent, contentRmd)

  # insert this content into a second project note
  # performed AFTER running insert_template_section_content()
  # to test this functionality at same time


  ################ insert_template_section_content() works #####################

  # create template section in Content
  templateSectionName <- "t-s"
  templateSectionDir <- "."
  contentLine <- 27 # set to a line below LOG Section

  templateSectionRmd <- local_create_template_section(
                                templateSectionName, templateSectionDir,
                                contentRmd, contentLine)

  # add some content to contentRmd!
  exampleContent <- c(
    "",
    "",
    "",
    "# Example Content",
    "",
    "",
    "-------------------------------------------------------------------------------",
    "-------------------------------------------------------------------------------",
    "",
    "",
    "",
    "# Example Content SOP",
    "",
    "",
    "-----------------------------------------------------------------------",
    "-----------------------------------------------------------------------",
    "",
    "",
    "",
    "# Example Content LOG",
    "",
    "",
    "-----------------------------------------------------------------------",
    "-----------------------------------------------------------------------",
    "",
    "",
    "",
    "",
    "",
    "" )

  write_file(exampleContent, contentRmd)

  # insert this content into a second project note
  insert_content(
    selectionSource = user_selection(projectNoteRmd, noteLine),
    selectionDestination = user_selection(subNoteRmd, noteLine)
  )

  ################ TESTS #######################################################

  # confirm Goal Deliverable Task Sections are written to projectDocRmd
  expect_snapshot_file( projectDocRmd )
  # [x] PASS

  # confirm content is written to subNoteRmd
  expect_snapshot_file( subNoteRmd )
   # [x] PASS


})

