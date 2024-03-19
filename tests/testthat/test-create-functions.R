

#### generate test organisation temp directory ####

# define temporary directory with defined name - for reproducible testing
# extract tempdir parts [1] + [2] to avoid issue with check() - that places tempdir() in a randomly named dir
tmpdir <- fs::path( unlist(fs::path_split(tempdir()))[1], unlist(fs::path_split(tempdir()))[2], "Rsess")
fs::dir_create(tmpdir)
withr::defer(fs::dir_delete(tmpdir), envir = parent.frame()) # delete once tests finish


test_that("Project Org is created correctly on filesystem", {

  # create test Organisation
  orgName <- "_TEST_ORG"
  orgutime <- "2024-02-22:09:56" # for consistent datetime added to status.yml snapshot
  orgDir <- local_create_org(orgName, orgutime, orgParentPath=tmpdir)
  orgIndex <- fs::path(orgDir, paste0("_index_", orgName, ".Rmd"))
  settingsYml <- fs::path(orgDir, ".config", "settings.yml")
  statusYml <- fs::path(orgDir, ".config", "status.yml")
  addinsJson <- fs::path(orgDir, ".config", "addins.json")
  volumesRmd <- fs::path(orgDir, "volumes", "volumes.Rmd")

  # check org name correctly generates index Rmd
  expect_true( fs::file_exists(orgIndex) )

  # check index Rmd file contents are correctly filled
  expect_snapshot_file( orgIndex )

  # check .config files are created
  expect_true( fs::file_exists(settingsYml) )
  expect_true( fs::file_exists(statusYml) )
  expect_true( fs::file_exists(addinsJson) )

  # check volumes Rmd exists
  expect_true( fs::file_exists(volumesRmd) )

})



test_that("Programme is created correctly on filesystem", {

  # create test Organisation
  orgName <- "_TEST_ORG_P"
  orgutime <- "2024-02-22:09:56" # for consistent datetime added to status.yml snapshot
  orgDir <- local_create_org(orgName, orgutime, orgParentPath=tmpdir)
  orgIndex <- fs::path(orgDir, paste0("_index_", orgName, ".Rmd"))
  settingsYml <- fs::path(orgDir, ".config", "settings.yml")
  statusYml <- fs::path(orgDir, ".config", "status.yml")
  addinsJson <- fs::path(orgDir, ".config", "addins.json")
  volumesRmd <- fs::path(orgDir, "volumes", "volumes.Rmd")

  # create test Programme
  progName <- "00-PROG"
  progctime <- "2024-02-22:09:58" # for consistent datetime added to status.yml snapshot
  progDir <- local_create_prog(progName, orgDir, progctime)
  progIndex <- fs::path(progDir, paste0("_index_", progName, ".Rmd"))

  # check programme name correctly generates index Rmd
  expect_true( fs::file_exists(progIndex) )

  # check index Rmd file contents are correctly filled
  expect_snapshot_file( progIndex )

  # check status.yml is written correctly
  expect_snapshot_file( statusYml )


  # check org name correctly generates index Rmd
  expect_true( fs::file_exists(orgIndex) )

  # check index Rmd file contents are correctly filled
  expect_snapshot_file( orgIndex )

  # check .config files are created
  expect_true( fs::file_exists(settingsYml) )
  expect_true( fs::file_exists(statusYml) )
  expect_true( fs::file_exists(addinsJson) )

  # check volumes Rmd exists
  expect_true( fs::file_exists(volumesRmd) )


})


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




test_that("Project Doc is created correctly on filesystem", {

  # create test Project Doc
  projectDocPrefix <- "PrDoc"
  projectDocName <- "Project_Doc"
  projectDocRmd <- local_create_project(projectDocPrefix, projectDocName, progDir)
  projectDocDir <- fs::path(progDir, projectDocPrefix)


  ## TESTS ##

  # check project Doc Rmd & Dir generated
  expect_true(  fs::file_exists( projectDocRmd )  )
  expect_true(  fs::dir_exists( projectDocDir )  )

  # check Project Doc Rmd file contents are correctly filled
  expect_snapshot_file( projectDocRmd )


  # check programme name correctly generates index Rmd
  expect_true( fs::file_exists(progIndex) )

  # check index Rmd file contents are correctly filled
  expect_snapshot_file( progIndex )

})




test_that("Project Notes (simple) created & linked correctly", {

  # create test Project Doc
  projectDocPrefix <- "PrDocSim"
  projectDocName <- "Project_Doc_simple"
  projectDocRmd <- local_create_project(projectDocPrefix, projectDocName, progDir)
  projectDocDir <- fs::path(progDir, projectDocPrefix)

  # modify gdt titles for unique headers - so can test links!
  taskLine <- local_modify_project_doc_gdt_titles(settingsYml, projectDocRmd)

  # create test Project Note : simple
  projectNoteName <- "Project_Note"
  projectNotePath <- fs::path(projectDocDir, 'test-notes')
  fs::dir_create(projectNotePath)
  projectNoteRmd <- local_create_project_note_simple(projectNoteName, projectNotePath,
                                                     projectDocRmd, taskLine)
  projectNoteDir <- get_project_note_dir_path(projectNoteRmd, settings)


  ## TESTS ##

  # check project Doc Rmd & Dir generated
  expect_true(  fs::file_exists( projectDocRmd )  )
  expect_true(  fs::dir_exists( projectDocDir )  )

  # check Project Doc Rmd file contents are correctly filled
  expect_snapshot_file( projectDocRmd )

  # check project Note Rmd & Dir generated
  expect_true(  fs::file_exists( projectNoteRmd )  )
  expect_true(  fs::dir_exists( projectNoteDir )  )

  # check Project Note Rmd file contents are correctly filled
  expect_snapshot_file( projectNoteRmd )

})




test_that("Project Notes (group) created & linked correctly", {

  # create test Project Doc
  projectDocPrefix <- "PrDocGrp"
  projectDocName <- "Project_Doc_group"
  projectDocRmd <- local_create_project(projectDocPrefix, projectDocName, progDir)
  projectDocDir <- fs::path(progDir, projectDocPrefix)

  # modify gdt titles for unique headers - so can test links!
  taskLine <- local_modify_project_doc_gdt_titles(settingsYml, projectDocRmd)

  # create test Project Note : group
  groupNoteName <- "Group_Note_Header"
  groupNotePath <- fs::path(projectDocDir, 'test-notes')
  fs::dir_create(groupNotePath)
  subNoteName <- "SubNote_01"
  groupNoteRmd <- local_create_project_note_group(groupNoteName, groupNotePath,
                                                     projectDocRmd, taskLine, subNoteName)

  groupNoteDir <- get_project_note_dir_path(groupNoteRmd, settings)

  subNoteRmd <- fs::path(groupNoteDir, paste0(basename(groupNotePath), "~001-001", "~_", subNoteName, ".Rmd") )
  subNoteDir <- get_project_note_dir_path(subNoteRmd, settings)


  ## TESTS ##

  # check project Doc Rmd & Dir generated
  expect_true(  fs::file_exists( projectDocRmd )  )
  expect_true(  fs::dir_exists( projectDocDir )  )

  # check Project Doc Rmd file contents are correctly filled
  expect_snapshot_file( projectDocRmd )


  # check group header Note Rmd & Dir generated
  expect_true(  fs::file_exists( groupNoteRmd )  )
  expect_true(  fs::dir_exists( groupNoteDir )  )

  # check group header Note Rmd file contents are correctly filled
  expect_snapshot_file( groupNoteRmd )


  # check sub Note Rmd & Dir generated
  expect_true(  fs::file_exists( subNoteRmd )  )
  expect_true(  fs::dir_exists( subNoteDir )  )

  # check sub Note Rmd file contents are correctly filled
  expect_snapshot_file( subNoteRmd )


})



test_that("Project Notes (sub) created & linked correctly", {

  # create test Project Doc
  projectDocPrefix <- "PrDocSub"
  projectDocName <- "Project_Doc_sub"
  projectDocRmd <- local_create_project(projectDocPrefix, projectDocName, progDir)
  projectDocDir <- fs::path(progDir, projectDocPrefix)

  # modify gdt titles for unique headers - so can test links!
  taskLine <- local_modify_project_doc_gdt_titles(settingsYml, projectDocRmd)

  # create test Project Note : group
  groupNoteName <- "Group_Note_Header2"
  groupNotePath <- fs::path(projectDocDir, 'test-notes')
  fs::dir_create(groupNotePath)
  subNoteName <- "SubNote2_01"
  groupNoteRmd <- local_create_project_note_group(groupNoteName, groupNotePath,
                                                  projectDocRmd, taskLine, subNoteName)

  groupNoteDir <- get_project_note_dir_path(groupNoteRmd, settings)

  subNoteRmd <- fs::path(groupNoteDir, paste0(basename(groupNotePath), "~001-001", "~_", subNoteName, ".Rmd") )
  subNoteDir <- get_project_note_dir_path(subNoteRmd, settings)

  # get link to group header note from project doc
  headerLinkLine <- local_get_project_doc_file_link_line(projectDocRmd, groupNoteRmd, settings)

  subNoteName2 <- "SubNote2_02"
  subNotePath <- groupNoteDir

  subNoteRmd2 <- local_create_project_note_sub(subNoteName2, subNotePath,
                                                  projectDocRmd, headerLinkLine)

  subNoteDir2 <- get_project_note_dir_path(subNoteRmd2, settings)


  ## TESTS ##

  # check project Doc Rmd & Dir generated
  expect_true(  fs::file_exists( projectDocRmd )  )
  expect_true(  fs::dir_exists( projectDocDir )  )

  # check Project Doc Rmd file contents are correctly filled
  expect_snapshot_file( projectDocRmd )
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






test_that("Content created correctly in Project Note", {

  # create test Project Doc for content
  projectDocPrefix <- "PrDocCon"
  projectDocName <- "Project_Doc_content"
  projectDocRmd <- local_create_project(projectDocPrefix, projectDocName, progDir)
  projectDocDir <- fs::path(progDir, projectDocPrefix)

  # modify gdt titles for unique headers - so can test links!
  taskLine <- local_modify_project_doc_gdt_titles(settingsYml, projectDocRmd)

  # create test Project Note for content insert : simple
  projectNoteName <- "Project_Note_content"
  projectNotePath <- fs::path(projectDocDir, 'test-notes-content')
  fs::dir_create(projectNotePath)
  projectNoteRmd <- local_create_project_note_simple(projectNoteName, projectNotePath,
                                                     projectDocRmd, taskLine)
  projectNoteDir <- get_project_note_dir_path(projectNoteRmd, settings)

  # create content in Project Note
  contentName <- "example-content"
  contentDescription <- "Example Content Description"
  contentSourcePath <- projectNoteDir
  noteLine <- 90 # set to a line below the Introduction default header but within Rmd lines
  contentTitle <- "Example Content"

  contentRmd <- local_create_content(contentName, contentDescription, contentSourcePath,
                                     projectNoteRmd, noteLine, contentTitle)

  ## TESTS ##

  # check project Doc Rmd & Dir generated
  expect_true(  fs::file_exists( projectDocRmd )  )
  expect_true(  fs::dir_exists( projectDocDir )  )

  # check Project Doc Rmd file contents are correctly filled
  expect_snapshot_file( projectDocRmd )

  # check project Note Rmd & Dir generated
  expect_true(  fs::file_exists( projectNoteRmd )  )
  expect_true(  fs::dir_exists( projectNoteDir )  )

  # check Project Note Rmd file contents are correctly filled
  expect_snapshot_file( projectNoteRmd )

  # check content Rmd generated (Rmd is inside Dir, so do not need to separately check contentDir)
  expect_true(  fs::file_exists( contentRmd )  )

  # check Project Note Rmd file contents are correctly filled
  expect_snapshot_file( contentRmd )

})






