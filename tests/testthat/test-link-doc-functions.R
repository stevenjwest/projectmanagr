

#### create permanent test Organisation & Programme ####

#for further tests
tmpdir <- fs::path(dirname(tempdir()), "Rsess")
fs::dir_create(tmpdir)

orgName <- "_T_O_PDN"
orgutime <- "2024-02-22:09:56" # for consistent datetime added to status.yml snapshot
orgDir <- local_create_org(orgName, orgutime, orgParentPath=tmpdir)
orgIndex <- fs::path(orgDir, paste0("_index_", orgName, ".Rmd"))
settingsYml <- fs::path(orgDir, ".config", "settings.yml")
statusYml <- fs::path(orgDir, ".config", "status.yml")
addinsJson <- fs::path(orgDir, ".config", "addins.json")
volumesRmd <- fs::path(orgDir, "volumes", "volumes.Rmd")

settings <- yaml::yaml.load( yaml::read_yaml( settingsYml ) )

# create test Programme
progName <- "0-PR-PDN"
progctime <- "2024-02-22:09:58" # for consistent datetime added to status.yml snapshot
progDir <- local_create_prog(progName, orgDir, progctime)
progIndex <- fs::path(progDir, paste0("_index_", progName, ".Rmd"))



test_that("link_doc_project_note creates link note <-> Doc GDT", {

  # create test Project Doc for initial project note link
  projectDocPrefix <- "PD"
  projectDocName <- "Proj_Do"
  projectDocRmd <- local_create_project(projectDocPrefix, projectDocName, progDir)
  projectDocDir <- fs::path(progDir, projectDocPrefix)

  # modify gdt titles for unique headers - so can test links!
  taskLine <- local_modify_project_doc_gdt_titles(settingsYml, projectDocRmd)

  # create second test Project Doc for initial project note link
  projectDoc2Prefix <- "PD2"
  projectDoc2Name <- "Proj_Do_2"
  projectDoc2Rmd <- local_create_project(projectDoc2Prefix, projectDoc2Name, progDir)
  projectDoc2Dir <- fs::path(progDir, projectDoc2Prefix)

  # modify gdt titles for unique headers - so can test links!
  taskLine2 <- local_modify_project_doc_gdt_titles(settingsYml, projectDoc2Rmd)

  # create test Project Note for linking : simple
  projectNoteName <- "PN_jou"
  projectNotePath <- fs::path(projectDocDir, 'tn-j')
  fs::dir_create(projectNotePath)
  projectNoteRmd <- local_create_project_note_simple(projectNoteName, projectNotePath,
                                                     projectDocRmd, taskLine)
  projectNoteDir <- get_project_note_dir_path(projectNoteRmd, settings)

  # link project note and project doc 2
  # ARGS
  selection = user_selection(projectDoc2Rmd, taskLine2)
  projNoteRmdPath = projectNoteRmd

  link_doc_project_note(selection, projNoteRmdPath)

  ## TESTS ##

  # check project Doc Rmd & Dir generated
  expect_true(  fs::file_exists( projectDoc2Rmd )  )
  expect_true(  fs::dir_exists( projectDoc2Dir )  )

  # check Project Doc Rmd file contents are correctly filled
  expect_snapshot_file( projectDoc2Rmd )

  # check project Note Rmd & Dir generated
  expect_true(  fs::file_exists( projectNoteRmd )  )
  expect_true(  fs::dir_exists( projectNoteDir )  )

  # check Project Note Rmd file contents are correctly filled
  expect_snapshot_file( projectNoteRmd )


})



test_that("link_doc_group_note creates link note <-> Doc GDT", {

  # create test Project Doc for initial project note link
  projectDocPrefix <- "PG"
  projectDocName <- "Proj_Do"
  projectDocRmd <- local_create_project(projectDocPrefix, projectDocName, progDir)
  projectDocDir <- fs::path(progDir, projectDocPrefix)

  # modify gdt titles for unique headers - so can test links!
  taskLine <- local_modify_project_doc_gdt_titles(settingsYml, projectDocRmd)

  # create second test Project Doc for initial project note link
  projectDoc2Prefix <- "PG2"
  projectDoc2Name <- "Proj_Do_2"
  projectDoc2Rmd <- local_create_project(projectDoc2Prefix, projectDoc2Name, progDir)
  projectDoc2Dir <- fs::path(progDir, projectDoc2Prefix)

  # modify gdt titles for unique headers - so can test links!
  taskLine2 <- local_modify_project_doc_gdt_titles(settingsYml, projectDoc2Rmd)

  # create test Project Note : group
  groupNoteName <- "Gr_No_Head"
  groupNotePath <- fs::path(projectDocDir, 't-no')
  fs::dir_create(groupNotePath)
  subNoteName <- "SNo_01"
  groupNoteRmd <- local_create_project_note_group(groupNoteName, groupNotePath,
                                                  projectDocRmd, taskLine, subNoteName)

  groupNoteDir <- get_project_note_dir_path(groupNoteRmd, settings)

  subNoteRmd <- fs::path(groupNoteDir, paste0(basename(groupNotePath), "___001-001", "____", subNoteName, ".Rmd") )
  subNoteDir <- get_project_note_dir_path(subNoteRmd, settings)

  # link group note and project doc 2
  # ARGS
  selection = user_selection(projectDoc2Rmd, taskLine2)
  headerNoteRmdPath = groupNoteRmd

  link_doc_group_note(selection, headerNoteRmdPath)

  ## TESTS ##

  # check project Doc Rmd & Dir generated
  expect_true(  fs::file_exists( projectDoc2Rmd )  )
  expect_true(  fs::dir_exists( projectDoc2Dir )  )

  # check Project Doc Rmd file contents are correctly filled
  expect_snapshot_file( projectDoc2Rmd )

  # check project Note Rmd & Dir generated
  expect_true(  fs::file_exists( groupNoteRmd )  )
  expect_true(  fs::dir_exists( groupNoteDir )  )

  # check Project Note Rmd file contents are correctly filled
  expect_snapshot_file( groupNoteRmd )

  # check sub Note Rmd & Dir generated
  expect_true(  fs::file_exists( subNoteRmd )  )
  expect_true(  fs::dir_exists( subNoteDir )  )

  # check sub Note Rmd file contents are correctly filled
  expect_snapshot_file( subNoteRmd )

})



test_that("link_doc_sub_note creates link note <-> Doc GDT", {

  # create test Project Doc
  projectDocPrefix <- "PrDoSu"
  projectDocName <- "Proj_Do_su"
  projectDocRmd <- local_create_project(projectDocPrefix, projectDocName, progDir)
  projectDocDir <- fs::path(progDir, projectDocPrefix)

  # modify gdt titles for unique headers - so can test links!
  taskLine <- local_modify_project_doc_gdt_titles(settingsYml, projectDocRmd)

  # create test Project Note : group
  groupNoteName <- "Gr_No_Head2"
  groupNotePath <- fs::path(projectDocDir, 't-no')
  fs::dir_create(groupNotePath)
  subNoteName <- "SNo2_01"
  groupNoteRmd <- local_create_project_note_group(groupNoteName, groupNotePath,
                                                  projectDocRmd, taskLine, subNoteName)

  groupNoteDir <- get_project_note_dir_path(groupNoteRmd, settings)

  subNoteRmd <- fs::path(groupNoteDir, paste0(basename(groupNotePath), "___001-001", "____", subNoteName, ".Rmd") )
  subNoteDir <- get_project_note_dir_path(subNoteRmd, settings)

  # get link to group header note from project doc
  headerLinkLine <- local_get_project_doc_file_link_line(projectDocRmd, groupNoteRmd, settings)

  subNoteName2 <- "SNo2_02"
  subNotePath <- groupNoteDir

  subNoteRmd2 <- local_create_project_note_sub(subNoteName2, subNotePath,
                                               projectDocRmd, headerLinkLine)

  subNoteDir2 <- get_project_note_dir_path(subNoteRmd2, settings)

  # create second test Project Doc for initial project note link
  projectDoc2Prefix <- "PS2"
  projectDoc2Name <- "Proj_Do_2"
  projectDoc2Rmd <- local_create_project(projectDoc2Prefix, projectDoc2Name, progDir)
  projectDoc2Dir <- fs::path(progDir, projectDoc2Prefix)

  # modify gdt titles for unique headers - so can test links!
  taskLine2 <- local_modify_project_doc_gdt_titles(settingsYml, projectDoc2Rmd)


  # link group note and project doc 2
  # ARGS
  selection = user_selection(projectDoc2Rmd, taskLine2)
  subNoteRmdPath = subNoteRmd2

  link_doc_sub_note(selection, subNoteRmdPath)


  ## TESTS ##

  # check project Doc Rmd & Dir generated
  expect_true(  fs::file_exists( projectDoc2Rmd )  )
  expect_true(  fs::dir_exists( projectDoc2Dir )  )

  # check Project Doc Rmd file contents are correctly filled
  expect_snapshot_file( projectDoc2Rmd )
  # interactively confirmed:
  # [x] can navigate groupNoteRmd link under GDT
  # [x] can navigate subNoteRmd link under GDT
  # [x] can navigate subNoteRmd2 link under GDT


  # check group header Note Rmd & Dir generated
  expect_true(  fs::file_exists( groupNoteRmd )  )
  expect_true(  fs::dir_exists( groupNoteDir )  )

  # check group header Note Rmd file contents are correctly filled
  expect_snapshot_file( groupNoteRmd )
  # interactively confirmed:
  # [x] can navigate GDT links to projectDocRmd
  # [x] can navigate subNoteRmd link under CONTENTS
  # [x] can navigate subNoteRmd2 link under CONTENTS


  # check new sub Note Rmd & Dir generated
  expect_true(  fs::file_exists( subNoteRmd2 )  )
  expect_true(  fs::dir_exists( subNoteDir2 )  )

  # check new sub Note Rmd file contents are correctly filled
  expect_snapshot_file( subNoteRmd2 )
  # interactively confirmed:
  # [x] can navigate GDT links to projectDocRmd
  # [x] can navigate groupNoteRmd link under GROUP CONTENTS


})



