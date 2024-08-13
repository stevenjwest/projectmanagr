#### create test Org with Project Notes for datatable insertion ####

#for further tests
tmpdir <- fs::path(dirname(tempdir()), "Rsess")
fs::dir_create(tmpdir)

orgName <- "_T_O_DT"
orgutime <- "2024-02-22:09:56" # for consistent datetime added to status.yml snapshot
orgDir <- local_create_org(orgName, orgutime, orgParentPath=tmpdir)
orgIndex <- fs::path(orgDir, paste0("_index_", orgName, ".Rmd"))
settingsYml <- fs::path(orgDir, ".config", "settings.yml")
statusYml <- fs::path(orgDir, ".config", "status.yml")
addinsJson <- fs::path(orgDir, ".config", "addins.json")
volumesRmd <- fs::path(orgDir, "volumes", "volumes.Rmd")

settings <- yaml::yaml.load( yaml::read_yaml( settingsYml ) )

# create test Programme
progName <- "0-PR-DT"
progctime <- "2024-02-22:09:58" # for consistent datetime added to status.yml snapshot
progDir <- local_create_prog(progName, orgDir, progctime)
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

# create test Project Note for datatables : source
projectNoteName <- "PN_src"
projectNoteRmd <- local_create_project_note_simple(projectNoteName, projectNotePath,
                                                   projectDocRmd, taskLine)
projectNoteDir <- get_project_note_dir_path(projectNoteRmd, settings)


# create test Project Note for datatables : destination (export/import testing)
projectNoteNameDest <- "PN_des"
projectNoteRmdDest <- local_create_project_note_simple(projectNoteNameDest, projectNotePath,
                                                       projectDocRmd, taskLine,noteIndex="___002")
projectNoteDirDest <- get_project_note_dir_path(projectNoteRmdDest, settings)



test_that("datatable_create_rmd writes to project note", {

  # test standard dt generation
  rmd_path=projectNoteRmd
  rmd_line=80 # blank line after project note structures
  settings=settings
  IDs=c(1001, 1002, 1003, 1004)
  data_cols=c("sex","wt-g")
  datatable_name = "samples"
  dt_length = 100

  datatable_create_rmd(rmd_path, rmd_line, settings,IDs, data_cols,
                       datatable_name, dt_length)


  ## TESTS ##

  # check Project Doc Rmd file contents are correctly filled
  expect_snapshot_file( rmd_path )


})

