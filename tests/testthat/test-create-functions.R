

test_that("test create functions", {

  ################ generate test organisation temp directory ###################

  tmpdir <- create_tmpdir_rsess()


  ################ create_project_org creates Org ##############################

  orgName <- "_T_O"
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

  # define outputs to check
  orgIndex <- fs::path(orgDir, paste0("_index_", orgName, ".Rmd"))
  settingsYml <- fs::path(orgDir, ".config", "settings.yml")
  statusYml <- fs::path(orgDir, ".config", "status.yml")
  addinsJson <- fs::path(orgDir, ".config", "addins.json")
  volumesRmd <- fs::path(orgDir, "volumes", "volumes.Rmd")

  settings <- yaml::yaml.load( yaml::read_yaml( settingsYml ) )


  # create second test org -to test get_org_paths() function
  orgName2 <- "_T_O2"
  orgutime2 <- "2024-02-22:09:56" # for consistent datetime added to status.yml snapshot

  # mock function to write to status yaml: do nothing, to maintain fixed status yaml content
  local_mocked_bindings(
    write_yaml_status = function(status, statusFile) { } ) # do nothing

  orgDir2 <- local_create_org(orgName2, authorValue, orgParentPath=tmpdir, syp="")

  # check status correctly written? cannot test as file name on disk is same as
   #status yaml file in original org!
  #statusYml2 <- fs::path(orgDir2, ".config", "status.yml")
  #expect_snapshot_file( statusYml2 )


  ################ create_programme creates programme ##########################

  progName <- "0-PR"
  # mock the function that returns the programme creation datetime
  local_mocked_bindings(
    get_datetime = function (timezone = "UTC", split="-", splitTime=":") {
      "2024-02-22:09:58" })


  # create test programme - using local helper function and withr package
  progDir <- local_create_prog(progName, orgDir, authorValue)

  # define outputs to check
  progIndex <- fs::path(progDir, paste0("_index_", progName, ".Rmd"))


  ################ create_programme_section creates ps1 in prog ################

  # create test Programme Section
  sectionName <- "ps1"
  sectionParentPath <- progDir
  sectDir <- local_create_prog_section(sectionName, sectionParentPath, authorValue)
  sectIndex <- fs::path(sectDir, paste0("_index_", sectionName, ".Rmd"))


  ################ create_programme_section creates ps2 in section #############

  # create test Programme Section nested in section
  sectionName <- "ps2"
  sectionParentPath <- sectDir
  sect2Dir <- local_create_prog_section(sectionName, sectionParentPath, authorValue)
  sect2Index <- fs::path(sect2Dir, paste0("_index_", sectionName, ".Rmd"))


  ################ create_project_doc creates doc in prog ######################

  # create test Project Doc
  projectDocPrefix <- "PDo"
  projectDocName <- "P_Do"
  projectParentPath <- progDir
  projectDocRmd <- local_create_project(projectDocPrefix, projectDocName,
                                        projectParentPath, authorValue)
  projectDocDir <- fs::path(projectParentPath, projectDocPrefix)


  ## TESTS ##

  # check project Doc Rmd & Dir generated
  expect_true(  fs::file_exists( projectDocRmd )  )
  expect_true(  fs::dir_exists( projectDocDir )  )

  # check Project Doc Rmd file contents are correctly filled
  expect_snapshot_file( projectDocRmd )


  ################ create_project_doc creates doc in section ###################

  # create test Project Doc
  projectDocPrefix <- "PDs"
  projectDocName <- "P_DoS"
  projectParentPath <- sectDir
  projectDocRmd <- local_create_project(projectDocPrefix, projectDocName,
                                        projectParentPath, authorValue)
  projectDocDir <- fs::path(projectParentPath, projectDocPrefix)


  ## TESTS ##

  # check project Doc Rmd & Dir generated
  expect_true(  fs::file_exists( projectDocRmd )  )
  expect_true(  fs::dir_exists( projectDocDir )  )

  # check Project Doc Rmd file contents are correctly filled
  expect_snapshot_file( projectDocRmd )


  ################ create_project_doc creates section and doc ##################

  # create new section directory
  sect3Dir <- fs::path(sectDir, "psd")
  fs::dir_create(sect3Dir)

  # create test Project Doc
  projectDocPrefix <- "Pas"
  projectDocName <- "P_DaS"
  projectParentPath <- sect3Dir
  projectDocRmd <- local_create_project(projectDocPrefix, projectDocName,
                                        projectParentPath, authorValue)
  projectDocDir <- fs::path(projectParentPath, projectDocPrefix)


  ## TESTS ##

  # check project Doc Rmd & Dir generated
  expect_true(  fs::file_exists( projectDocRmd )  )
  expect_true(  fs::dir_exists( projectDocDir )  )

  # check Project Doc Rmd file contents are correctly filled
  expect_snapshot_file( projectDocRmd )

  ################ create_project_note creates simple Project Note #############

  # create test Project Doc
  projectDocPrefix <- "PrDoS"
  projectDocName <- "Proj_Do_sim"
  projectDocRmd <- local_create_project(projectDocPrefix, projectDocName,
                                        progDir, authorValue)

  projectDocDir <- fs::path(progDir, projectDocPrefix)

  # modify gdt titles for unique headers - so can test links!
  taskLine <- local_modify_project_doc_gdt_titles(settingsYml, projectDocRmd)

  # create test Project Note : simple
  projectNoteName <- "Proj_No"
  projectNotePath <- fs::path(projectDocDir, 't-no')
  fs::dir_create(projectNotePath)
  projectNoteRmd <- local_create_project_note_simple(projectNoteName, projectNotePath,
                                                     projectDocRmd, taskLine, authorValue)
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


  ################ create_group_note creates group Project Note ################

  # create test Project Doc
  projectDocPrefix <- "PrDoG"
  projectDocName <- "Proj_Do_gr"
  projectDocRmd <- local_create_project(projectDocPrefix, projectDocName,
                                        progDir, authorValue)
  projectDocDir <- fs::path(progDir, projectDocPrefix)

  # modify gdt titles for unique headers - so can test links!
  taskLine <- local_modify_project_doc_gdt_titles(settingsYml, projectDocRmd)

  # create test Project Note : group
  groupNoteName <- "Gr_No_Head"
  groupNotePath <- fs::path(projectDocDir, 't-no')
  fs::dir_create(groupNotePath)
  subNoteName <- "SNo_01"
  groupNoteRmd <- local_create_project_note_group(groupNoteName, groupNotePath,
                                                  projectDocRmd, taskLine,
                                                  subNoteName, authorValue)

  groupNoteDir <- get_project_note_dir_path(groupNoteRmd, settings)

  subNoteRmd <- fs::path(groupNoteDir, paste0(basename(groupNotePath), "___001-001", "_--_", subNoteName, ".Rmd") )
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


  ################ create_sub_note creates sub Project Note ####################

  # create test Project Doc
  projectDocPrefix <- "PrDoSu"
  projectDocName <- "Proj_Do_su"
  projectDocRmd <- local_create_project(projectDocPrefix, projectDocName,
                                        progDir, authorValue)
  projectDocDir <- fs::path(progDir, projectDocPrefix)

  # modify gdt titles for unique headers - so can test links!
  taskLine <- local_modify_project_doc_gdt_titles(settingsYml, projectDocRmd)

  # create test Project Note : group
  groupNoteName <- "Gr_No_Head2"
  groupNotePath <- fs::path(projectDocDir, 't-no')
  fs::dir_create(groupNotePath)
  subNoteName <- "SNo2_01"
  groupNoteRmd <- local_create_project_note_group(groupNoteName, groupNotePath,
                                                  projectDocRmd, taskLine,
                                                  subNoteName, authorValue)

  groupNoteDir <- get_project_note_dir_path(groupNoteRmd, settings)

  subNoteRmd <- fs::path(groupNoteDir, paste0(basename(groupNotePath), "___001-001", "_--_", subNoteName, ".Rmd") )
  subNoteDir <- get_project_note_dir_path(subNoteRmd, settings)

  # get link to group header note from project doc
  headerLinkLine <- local_get_project_doc_file_link_line(projectDocRmd, groupNoteRmd, settings)

  subNoteName2 <- "SNo2_02"
  subNoteName3 <- "SNo3_03"
  subNotePath <- groupNoteDir

  subNoteRmd2 <- local_create_project_note_sub(subNoteName2, subNotePath,
                                               projectDocRmd, headerLinkLine,
                                               authorValue)

  subNoteRmd3 <- local_create_project_note_sub_head_sel(subNoteName3, subNotePath,
                                                        groupNoteRmd, 50, authorValue)

  subNoteDir2 <- get_project_note_dir_path(subNoteRmd2, settings)
  subNoteDir3 <- get_project_note_dir_path(subNoteRmd3, settings)


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

  # check new sub Note Rmd & Dir generated
  expect_true(  fs::file_exists( subNoteRmd3 )  )
  expect_true(  fs::dir_exists( subNoteDir3 )  )

  # check new sub Note Rmd file contents are correctly filled
  expect_snapshot_file( subNoteRmd2 )
  expect_snapshot_file( subNoteRmd3 )
  # interactively confirmed:
  # [x] can navigate GDT links to projectDocRmd
  # [x] can navigate groupNoteRmd link under GROUP CONTENTS


  ################ create_content creates insertable content in Proj Note ######

  # create test Project Doc for content
  projectDocPrefix <- "PDCon"
  projectDocName <- "Proj_Do_con"
  projectDocRmd <- local_create_project(projectDocPrefix, projectDocName,
                                        progDir, authorValue)
  projectDocDir <- fs::path(progDir, projectDocPrefix)

  # modify gdt titles for unique headers - so can test links!
  taskLine <- local_modify_project_doc_gdt_titles(settingsYml, projectDocRmd)

  # create test Project Note for content insert : simple
  projectNoteName <- "PN_con"
  projectNotePath <- fs::path(projectDocDir, 'tn-c')
  fs::dir_create(projectNotePath)
  projectNoteRmd <- local_create_project_note_simple(projectNoteName, projectNotePath,
                                                     projectDocRmd, taskLine, authorValue)
  projectNoteDir <- get_project_note_dir_path(projectNoteRmd, settings)

  # create content in Project Note
  contentName <- "ex-con"
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


  ################ create_daily_journal creates a journal Rmd #################

  # mock the
  local_mocked_bindings(
    get_loc = function(location_str) {
      return( list(lat=51.52, long=-0.14) )
    })
  # create test Project Doc for content
  projectDocPrefix <- "PDJou"
  projectDocName <- "Proj_Do_jou"
  projectDocRmd <- local_create_project(projectDocPrefix, projectDocName,
                                        progDir, authorValue)
  projectDocDir <- fs::path(progDir, projectDocPrefix)

  # modify gdt titles for unique headers - so can test links!
  taskLine <- local_modify_project_doc_gdt_titles(settingsYml, projectDocRmd)

  # create test Project Note for content insert : simple
  projectNoteName <- "PN_jou"
  projectNotePath <- fs::path(projectDocDir, 'tn-j')
  fs::dir_create(projectNotePath)
  projectNoteRmd <- local_create_project_note_simple(projectNoteName, projectNotePath,
                                                     projectDocRmd, taskLine, authorValue)
  projectNoteDir <- get_project_note_dir_path(projectNoteRmd, settings)

  # create journal in Org
  date=lubridate::ymd("2024-05-10") #  this date is interesting as it has no moonset time!
  organisationPath=orgDir

  journalRmd <- local_create_journal(date, organisationPath, authorValue)

  # generate second journal
  # this time with get_loc() returning failure to test tryCatch() error handling
  date=lubridate::ymd("2024-05-11")

  local_mocked_bindings(
    get_loc = function(location_str) {
      stop()
    })
  journalRmd2 <- local_create_journal2(date, organisationPath, authorValue)


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
  expect_true(  fs::file_exists( journalRmd )  )

  # check Journsl Rmd files are correctly filled
  expect_snapshot_file( journalRmd )
  expect_snapshot_file( journalRmd2 )


  ################ INDEX TESTS #################################################

  # placing all INDEX tests after creation of all artefacts in the Organisation
  # so these tests check all the links & contents are correctly written at this point
   # including all project doc & programme section links

  ### TESTS : ORG INDEX

  # check outputs
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

  expect_error( create_project_org(tmpdir, orgName) )


  ### TESTS : PROG INDEX

  # check programme name correctly generates index Rmd
  expect_true( fs::file_exists(progIndex) )

  # check index Rmd file contents are correctly filled
  expect_snapshot_file( progIndex )

  # check status.yml is written correctly
  expect_snapshot_file( statusYml )


  ### TESTS : PROG SECTION INDEX s1

  # check programme section name correctly generates index Rmd
  expect_true(  fs::file_exists( sectIndex )  )

  # check index Rmd file contents are correctly filled
  expect_snapshot_file( sectIndex )


  ### TESTS : PROG SECTION INDEX s2

  # check programme section name correctly generates index Rmd
  expect_true(  fs::file_exists( sect2Index )  )

  # check index Rmd file contents are correctly filled
  expect_snapshot_file( sect2Index )

})


