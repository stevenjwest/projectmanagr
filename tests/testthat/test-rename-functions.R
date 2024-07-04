

#### create permanent test Organisation & Programme ####

#for further tests
tmpdir <- fs::path(dirname(tempdir()), "Rsess")
fs::dir_create(tmpdir)

orgName <- "_TEST_ORG_PDN"
orgutime <- "2024-02-22:09:56" # for consistent datetime added to status.yml snapshot
orgDir <- local_create_org(orgName, orgutime, orgParentPath=tmpdir)
orgIndex <- fs::path(orgDir, paste0("_index_", orgName, ".Rmd"))
settingsYml <- fs::path(orgDir, ".config", "settings.yml")
statusYml <- fs::path(orgDir, ".config", "status.yml")
addinsJson <- fs::path(orgDir, ".config", "addins.json")
volumesRmd <- fs::path(orgDir, "volumes", "volumes.Rmd")

settings <- yaml::yaml.load( yaml::read_yaml( settingsYml ) )

# create test Programme
progName <- "00-PROG-PDN"
progctime <- "2024-02-22:09:58" # for consistent datetime added to status.yml snapshot
progDir <- local_create_prog(progName, orgDir, progctime)
progIndex <- fs::path(progDir, paste0("_index_", progName, ".Rmd"))



test_that("rename_project_note rnames note and updates links", {

  # create test Project Doc for initial project note link
  projectDocPrefix <- "PD"
  projectDocName <- "Proj_Do"
  projectDocRmd <- local_create_project(projectDocPrefix, projectDocName, progDir)
  projectDocDir <- fs::path(progDir, projectDocPrefix)

  # modify gdt titles for unique headers - so can test links!
  taskLine <- local_modify_project_doc_gdt_titles(settingsYml, projectDocRmd)

  # create test Project Note for linking : simple
  projectNoteName <- "PN_jou"
  projectNotePir <- fs::path(projectDocDir, 'tn-j')
  fs::dir_create(projectNotePir)

  selection <- user_selection(projectDocRmd, taskLine)
  create_project_note(projectNoteName, projectNotePir, selection)
  projectNoteRmd <- fs::path(projectNotePir, paste0(basename(projectNotePir),
                                                     "___001____", projectNoteName, ".Rmd") )
  projectNoteDir <- get_project_note_dir_path(projectNoteRmd, settings)

  # rename project note
  projectNotePath=projectNoteRmd
  newProjectNoteName="PN_rn"
  newProjectNoteTitle=""
  replaceLinksFileExtensions = list("Rmd")

  projectNoteRmdRename <- local_rename_project_note_simple(projectNotePath, newProjectNoteName,
                                                     newProjectNoteTitle,replaceLinksFileExtensions)
  projectNoteDirRename <- fs::path(projectNotePir,
                                   paste0(basename(projectNotePir),
                                          "___001") )

  ## TESTS ##

  # check renamed project Note Rmd & Dir generated
  expect_true(  fs::file_exists( projectNoteRmdRename )  )
  expect_true(  fs::dir_exists( projectNoteDirRename )  )

  # check renamed Project Note Rmd file contents are correctly filled
  expect_snapshot_file( projectNoteRmdRename )

  # check project Doc Rmd & Dir generated
  expect_true(  fs::file_exists( projectDocRmd )  )
  expect_true(  fs::dir_exists( projectDocDir )  )

  # check Project Doc Rmd file contents are correctly filled
  expect_snapshot_file( projectDocRmd )



})



test_that("rename_project_doc rnames note and updates links", {

  # create test Project Doc for initial project note link
  projectDocPrefix <- "PD"
  projectDocName <- "Proj_Do"
  projectDocRmd <- local_create_project(projectDocPrefix, projectDocName, progDir)
  projectDocDir <- fs::path(progDir, projectDocPrefix)

  # modify gdt titles for unique headers - so can test links!
  taskLine <- local_modify_project_doc_gdt_titles(settingsYml, projectDocRmd)

  # create test Project Note for linking : simple
  projectNoteName <- "PN_jou"
  projectNotePir <- fs::path(projectDocDir, 'tn-j')
  fs::dir_create(projectNotePir)

  selection <- user_selection(projectDocRmd, taskLine)
  create_project_note(projectNoteName, projectNotePir, selection)
  projectNoteRmd <- fs::path(projectNotePir, paste0(basename(projectNotePir),
                                                    "___001____", projectNoteName, ".Rmd") )
  projectNoteDir <- get_project_note_dir_path(projectNoteRmd, settings)

  # rename project note
  projectNotePath=projectNoteRmd
  newProjectNoteName="PN_rn"
  newProjectNoteTitle=""
  replaceLinksFileExtensions = list("Rmd")

  projectNoteRmdRename <- local_rename_project_note_simple(projectNotePath, newProjectNoteName,
                                                           newProjectNoteTitle,replaceLinksFileExtensions)
  projectNoteDirRename <- fs::path(projectNotePir,
                                   paste0(basename(projectNotePir),
                                          "___001") )

  ## TESTS ##

  # check renamed project Note Rmd & Dir generated
  expect_true(  fs::file_exists( projectNoteRmdRename )  )
  expect_true(  fs::dir_exists( projectNoteDirRename )  )

  # check renamed Project Note Rmd file contents are correctly filled
  expect_snapshot_file( projectNoteRmdRename )

  # check project Doc Rmd & Dir generated
  expect_true(  fs::file_exists( projectDocRmd )  )
  expect_true(  fs::dir_exists( projectDocDir )  )

  # check Project Doc Rmd file contents are correctly filled
  expect_snapshot_file( projectDocRmd )



})



