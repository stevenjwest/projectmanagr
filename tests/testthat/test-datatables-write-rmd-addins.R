

cat("=====================")
cat("test datatables write rmd addins")
cat("")


test_that("datatables write addins testing", {

  ############## ________________ ##############################################

  ############## setup : mocked bindings and vars ##############################

  WD <- getwd()
  local_mocked_bindings(
    addin_rstudio_nav = function(orgIndexPath) { stopApp() } # prevent addins from opening the file - just stop the shiny gadget
    )

  ############## generate test organisation temp directory #####################

  tmpdir <- create_tmpdir_rsess()
  #tmpdirNoRoot <- fs::path_join(fs::path_split(tmpdir)[[1]][-1])


  ############## create test Org with Project Notes for datatable insertion ####
  orgName <- "_T_ODwa"
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

  progDir <- local_create_prog(progName, orgDir, authorValue)
  progIndex <- fs::path(progDir, paste0("_index_", progName, ".Rmd"))

  # create test Project Doc for initial project note link
  projectDocPrefix <- "PD"
  projectDocName <- "Proj_Do"
  projectDocRmd <- local_create_project(projectDocPrefix, projectDocName,
                                        progDir, authorValue)
  projectDocDir <- fs::path(progDir, projectDocPrefix)

  # modify gdt titles for unique headers
  taskLine <- local_modify_project_doc_gdt_titles(settingsYml, projectDocRmd)

  # create test Project Note directory
  projectNotePath <- fs::path(projectDocDir, 'tn-t')
  fs::dir_create(projectNotePath)


  ############## project notes : gen datatables ################################
  # create test Project Note for datatables : CREATE
  projectNoteNameCre <- "PN_cre"
  projectNoteRmdCre <- local_create_project_note_simple(projectNoteNameCre, projectNotePath,
                                                        projectDocRmd, taskLine, authorValue)
  projectNoteDirCre <- get_project_note_dir_path(projectNoteRmdCre, settings)


  # create test Project Note for datatables : ADD DATA SAMPLES
  projectNoteNameADS <- "PN_ADS"
  projectNoteRmdADS <- local_create_project_note_simple(projectNoteNameADS, projectNotePath,
                                                        projectDocRmd, taskLine, authorValue,
                                                        noteIndex="___002")
  projectNoteDirADS <- get_project_note_dir_path(projectNoteRmdADS, settings)


  # create test Project Note for datatables : ADD DATA VARIABLES
  projectNoteNameADV <- "PN_ADV"
  projectNoteRmdADV <- local_create_project_note_simple(projectNoteNameADV, projectNotePath,
                                                        projectDocRmd, taskLine, authorValue,
                                                        noteIndex="___003")
  projectNoteDirADV <- get_project_note_dir_path(projectNoteRmdADV, settings)


  # create test Project Note for datatables : ADD DATA TIMETABLE
  projectNoteNameADT <- "PN_ADT"
  projectNoteRmdADT <- local_create_project_note_simple(projectNoteNameADT, projectNotePath,
                                                        projectDocRmd, taskLine, authorValue,
                                                        noteIndex="___004")
  projectNoteDirADT <- get_project_note_dir_path(projectNoteRmdADT, settings)


  # create test Project Note for datatables : ADD GROUP
  projectNoteNameADG <- "PN_ADG"
  projectNoteRmdADG <- local_create_project_note_simple(projectNoteNameADG, projectNotePath,
                                                        projectDocRmd, taskLine, authorValue,
                                                        noteIndex="___005")
  projectNoteDirADG <- get_project_note_dir_path(projectNoteRmdADG, settings)


  # create test Project Note for datatables : DISPOSE
  projectNoteNameDSP <- "PN_DSP"
  projectNoteRmdDSP <- local_create_project_note_simple(projectNoteNameDSP, projectNotePath,
                                                        projectDocRmd, taskLine, authorValue,
                                                        noteIndex="___006")
  projectNoteDirDSP <- get_project_note_dir_path(projectNoteRmdDSP, settings)


  # create test Project Note for datatables : RESAMPLE
  projectNoteNameRSP <- "PN_RSP"
  projectNoteRmdRSP <- local_create_project_note_simple(projectNoteNameRSP, projectNotePath,
                                                        projectDocRmd, taskLine, authorValue,
                                                        noteIndex="___007")
  projectNoteDirRSP <- get_project_note_dir_path(projectNoteRmdRSP, settings)



  ############## projects notes : import export samples ########################

  # create test Project Note directory : Export
  projectNotePathExport <- fs::path(projectDocDir, 'tn-ex')
  fs::dir_create(projectNotePathExport)


  # create test Project Note for datatables : EXPORT 1
  projectNoteNameEx1 <- "PN_ex1"
  projectNoteRmdEx1 <- local_create_project_note_simple(projectNoteNameEx1, projectNotePathExport,
                                                        projectDocRmd, taskLine, authorValue)
  projectNoteDirEx1 <- get_project_note_dir_path(projectNoteRmdEx1, settings)


  # create test Project Note for datatables : EXPORT 2
  projectNoteNameEx2 <- "PN_ex2"
  projectNoteRmdEx2 <- local_create_project_note_simple(projectNoteNameEx2, projectNotePathExport,
                                                        projectDocRmd, taskLine, authorValue,
                                                        noteIndex="___002")
  projectNoteDirEx2 <- get_project_note_dir_path(projectNoteRmdEx2, settings)


  # create test Project Note for datatables : EXPORT 3
  projectNoteNameEx3 <- "PN_ex3"
  projectNoteRmdEx3 <- local_create_project_note_simple(projectNoteNameEx3, projectNotePathExport,
                                                        projectDocRmd, taskLine, authorValue,
                                                        noteIndex="___003")
  projectNoteDirEx3 <- get_project_note_dir_path(projectNoteRmdEx3, settings)


  # create test Project Note directory : Export Sub
  projectSubPathExport <- fs::path(projectNotePathExport, 'tex')
  fs::dir_create(projectSubPathExport)

  # create test Project Sub for datatables : EXPORT 1
  projectSubNameEx1 <- "SN_ex1"
  projectSubRmdEx1 <- local_create_project_note_simple(projectSubNameEx1, projectSubPathExport,
                                                        projectDocRmd, taskLine, authorValue)
  projectSubDirEx1 <- get_project_note_dir_path(projectSubRmdEx1, settings)


  # create test Project Note directory : Import
  projectNotePathImport <- fs::path(projectDocDir, 'tn-im')
  fs::dir_create(projectNotePathImport)


  # create test Project Note for datatables : IMPORT 1
  projectNoteNameIm1 <- "PN_im1"
  projectNoteRmdIm1 <- local_create_project_note_simple(projectNoteNameIm1, projectNotePathImport,
                                                        projectDocRmd, taskLine, authorValue,
                                                        noteIndex="___001")
  projectNoteDirIm1 <- get_project_note_dir_path(projectNoteRmdIm1, settings)



  ############## ________________ ##############################################

  ############## test addin : Datatable CREATE #################################


  ##### dt_create_ui() creates expected HTML #####

  # define variables
  rmd_path <- projectNoteRmdCre
  row <- 75

  expect_snapshot(dt_create_ui(rmd_path, row))


  ##### dt_create_server() creates datatable ######

  # mock the rstudio context retrieval functions
  local_mocked_bindings(
    get_context_path = function() { rmd_path },
    get_context_row = function() { 75 },
    get_context_contents = function() { read_file(rmd_path) } )

  shiny::testServer(dt_create_server, {

    # CHECK ERRORS
    session$setInputs(
      name = "")
    Sys.sleep(0.2)
    session$setInputs(done=1)
    Sys.sleep(0.2)

    expect_equal(output$warningName, "*** PROVIDE DATATABLE NAME ***")

    # test create datatable
    session$setInputs(
      name = "samples_CNS",
      ids = "1001 1002 1003 1004",
      data_cols = "x wt_g perfuse_dt perfusion_con group_fix",
      expand = FALSE )

    Sys.sleep(0.2)
    session$setInputs(done=1)
    Sys.sleep(0.2)

  })

  ##### dt_create_template_ui() creates expected HTML #####

  add_template_create_datatables(rmd_path, 96)

  row <- 101 # selecting line inside template datatable

  contents <- read_file(rmd_path)
  indices <- which( startsWith( contents, "+===") )
  indices_row <- which( startsWith( contents[1:row], "+===") )
  startrow <- indices_row[length(indices_row)]
  endrow <- indices[length(indices_row)+1]
  template_dt_vector <- contents[ startrow : endrow ]

  dts <- datatable_read_vector(contents[1:startrow])
  dt_names <- names(dts)
  lt <- as.list( 1:length(dt_names) )
  names(lt) <- dt_names

  template_datatable_name <- extract_template_named_table(template_dt_vector)

  expect_snapshot(dt_create_template_ui(rmd_path, startrow, endrow, lt, template_datatable_name))


  ##### dt_create_template_server() creates datatable ######

  # mock the rstudio context retrieval functions
  local_mocked_bindings(
    get_context_path = function() { rmd_path },
    get_context_row = function() { 101 },
    get_context_contents = function() { read_file(rmd_path )} )

  shiny::testServer(dt_create_template_server, {

    session$setInputs(
      defIDs = FALSE,
      dt = "1", # selects first datatable: samples_CNS
      template_datatable_name = "fix-solution-wts",
      allIDs = FALSE )

    Sys.sleep(0.2)
    session$setInputs(done=1)
    Sys.sleep(0.2)

  })


  ##### dt_create_file_ui() creates expected HTML #####


  row <- 130
  dtT <- get_datatable_types_list()

  expect_snapshot(dt_create_file_ui(rmd_path, row, dtT))


  ##### dt_create_file_server() creates datatable #####

  # write mock csv to tmp directory
  datafile <- fs::path(tmpdir, paste0("datafile.csv"))
  readr::write_delim(
    tibble::tibble(ID=c("1001","1002","1003"), wt_g = c(1.23, 2.23, 3.23)),
    datafile,
    delim=",")

  #mock the rstudio context retrieval functions
  local_mocked_bindings(
    get_context_path = function() { rmd_path },
    get_context_row = function() { 130 },
    get_context_contents = function() { read_file(rmd_path )} )

  shiny::testServer(dt_create_file_server, {

    session$setInputs(
      dtType = "1", # selects first datatable type: CREATE
      file1 = list(datapath = datafile),
      dt_name = "fix-solutions",
      allIDs = FALSE )

    Sys.sleep(0.2)
    session$setInputs(done=1)
    Sys.sleep(0.2)

  })


  #### expect snapshot : rmd ####

  # check index Rmd file contents are correctly filled - both the create samples_CNS && create from template
  expect_snapshot_file( rmd_path )


  ##### addin_datatable_create() #####

  ###### create ######

  row <- 129

  # mock ui and server functions to test higher infrastructure - return strings for testing args
  local_mocked_bindings(
    get_context_row = function() { 129 },
    dt_create_ui = function(path, row) {
      paste0("path: ", get_relative_path_org(path), " row: ", row, " endrow: ", endrow)
    },
    dt_create_server = function(input, output, session) {
      paste0(" server")
    },
    runGadget = function(UI, SERVER, viewer) {
      paste0(UI, SERVER())
    }
  )

  expect_snapshot( paste0("CREATE: ", addin_datatable_create() ) )

  ###### create template ######

  rm_lines(rmd_path, 97, 125) # remove the template that has been filled
  add_template_create_datatables(rmd_path, 96)
  row <- 101


  # mock ui and server functions to test higher infrastructure - return strings for testing args
  local_mocked_bindings(
    get_context_row = function() { 101 },
    dt_create_template_ui = function(path, startrow, endrow, lt, template_datatable_name) {
      paste0("path: ", get_relative_path_org(path), " startrow: ", startrow, " endrow: ", endrow,
             " lt-names: ", names(lt), " template_datatable_name: ", template_datatable_name )
    },
    dt_create_template_server = function(input, output, session) {
      paste0(" server")
    },
    runGadget = function(UI, SERVER, viewer) {
      paste0(UI, SERVER())
    }
  )

  expect_snapshot( paste0("TEMPLATE CREATE: ", addin_datatable_create() ) )


  #### expect errors : check_dt_create_template() ####

  # length indices <= indices_row
  expect_error( check_dt_create_template(
    indices = c(1,2,3),
    indices_row = c(1,2,3,4),
    table_name = "TMPLATE",
    table_function="CRETE") )

  # table_name != TEMPLATE
  expect_error( check_dt_create_template(
    indices = c(1,2,3),
    indices_row = c(1,2),
    table_name = "TMPLATE",
    table_function="CRETE") )

  # table_function != CREATE
  expect_error( check_dt_create_template(
    indices = c(1,2,3),
    indices_row = c(1,2),
    table_name = "TEMPLATE",
    table_function="CRETE") )


  #### addin_datatable_create_from_file() ####


  ###### create ######

  row <- 129

  # mock ui and server functions to test higher infrastructure - return strings for testing args
  local_mocked_bindings(
    get_context_row = function() { 129 },

    dt_create_file_ui = function(path, row, dtT) {
      paste0("path: ", get_relative_path_org(path), " row: ", row, " dtT names: ", paste(names(dtT), collapse=' ') )
    },
    dt_create_file_server = function(input, output, session) {
      paste0(" server")
    },
    runGadget = function(UI, SERVER, viewer) {
      paste0(UI, SERVER())
    }
  )

  expect_snapshot( paste0("CREATE from file: ", addin_datatable_create_from_file() ) )


  ############## _____________ #################################################

  ############## test addin : Datatable ADD DATA ###############################


  ##### dt_add_data_ui() creates expected HTML #####

  # define variables
  rmd_path <- projectNoteRmdADS
  row <- 75

  # add a create datatable
  add_dt_create_test(rmd_path, row)

  row <- 100

  contents <- read_file(rmd_path)

  dts <- datatable_read_vector(contents[1:row])
  dt_names <- names(dts)
  lt <- as.list( 1:length(dt_names) )
  names(lt) <- dt_names

  # get add _data named list of types
  type <- get_datatable_add_data_types_list()

  # select group cols from first datatable INITIALLY
  gdt <- get_group_cols(dts[[1]])
  ltid <- get_id_group_selection_list(gdt) # includes all-IDs

  expect_snapshot(dt_add_data_ui(rmd_path, row, lt, type, ltid))


  ##### dt_add_data_server() creates datatable ######

  # mock the rstudio context retrieval functions
  local_mocked_bindings(
    get_context_path = function() { rmd_path },
    get_context_row = function() { 100 },
    get_context_contents = function() { read_file(rmd_path )} )

  shiny::testServer(dt_add_data_server, {

    session$setInputs(
      dt = "1", # selects first datatable - samples
      type = "1", # select sample-first datatable type
      id = "2", # select all-IDs
      data_cols = "y yt_g yerfuse_dt yerfusion_con group_yix" )

    Sys.sleep(0.2)
    session$setInputs(done=1)
    Sys.sleep(0.2)

  })


  row <- 122

  # mock the rstudio context retrieval functions
  local_mocked_bindings(
    get_context_row = function() { 122 } )

  shiny::testServer(dt_add_data_server, {

    session$setInputs(
      dt = "1", # selects first datatable - samples
      type = "2", # select variable-first datatable type
      id = "2", # select group_fix
      data_cols = "z zt_g zerfuse_dt zerfusion_con group_zix" )

    Sys.sleep(0.2)
    session$setInputs(done=1)
    Sys.sleep(0.2)

  })

  row <- 145

  # mock the rstudio context retrieval functions
  local_mocked_bindings(
    get_context_row = function() { 145 } )

  shiny::testServer(dt_add_data_server, {

    session$setInputs(
      dt = "1", # selects first datatable - samples
      type = "3", # select variable-first datatable type
      id = "2", # select group_yix
      data_cols = "HT_RT MT_RT MDT_RT MDT_RT MT_RT HT_RT",
      colName = "delip")

    Sys.sleep(0.2)
    session$setInputs(done=1)
    Sys.sleep(0.2)

  })


  #### dt_add_data_template Function Testing ####

  add_template_add_data_datatables(rmd_path, 170)

  # test ADD_DATA Template with named output
  # named output datatable with ADD_DATA defines all inputs necessary
   # so no addin is required as no user input required!

  # mock the rstudio context retrieval functions
  local_mocked_bindings(
    get_context_path = function() { rmd_path },
    get_context_row = function() { 175 },
    get_context_contents = function() { read_file(rmd_path )} )

  # execute parent function - as no addin is run with a named template ADD_DATA
  addin_datatable_add_data() # will be checked via file snapshot!


  # test ADD_DATA Template with no named output

  ##### dt_add_data_template_ui() creates expected HTML #####

  row <- 196 # selecting line inside template datatable

  dts <- datatable_read_vector(contents[1:row])
  dt_names <- names(dts)
  lt <- as.list( 1:length(dt_names) )
  names(lt) <- dt_names

  expect_snapshot(dt_add_data_template_ui(rmd_path, row, lt))


  ##### dt_add_data_template_server() creates datatable ######

  # mock the rstudio context retrieval functions
  local_mocked_bindings(
    get_context_row = function() { 196 } )

  shiny::testServer(dt_add_data_template_server, {

    session$setInputs(
      dt = "1", # selects first datatable: samples_CNS
      summarise_reps = FALSE,
      all_reps = FALSE )

    Sys.sleep(0.2)
    session$setInputs(done=1)
    Sys.sleep(0.2)

  })


  #### expect snapshot : rmd ####

  # check index Rmd file contents are correctly filled - both the create samples_CNS && create from template
  expect_snapshot_file( rmd_path )


  ##### addin_datatable_add_data() #####

  ###### add_data ######

  row <- 214

  # mock ui and server functions to test higher infrastructure - return strings for testing args
  local_mocked_bindings(
    get_context_row = function() { 214
      },
    dt_add_data_ui = function(path, row, lt, type, ltid) {
      paste0("path: ", get_relative_path_org(path), " row: ", row, " lt-names: ", paste(names(lt), collapse=' '),
             " type-names: ", paste(names(type), collapse=' ')," ltid-names: ", paste(names(ltid), collapse=' ') )
      },
    dt_add_data_server = function(input, output, session) {
      paste0(" server")
      },
    runGadget = function(UI, SERVER, viewer) {
      paste0(UI, SERVER())
      }
  )

  expect_snapshot( paste0("ADD_DATA: ", addin_datatable_add_data() ) )

  ###### add_data template ######

  row <- 215
  add_template_add_data_datatables(rmd_path, row)

  row <- 241

  # mock ui and server functions to test higher infrastructure - return strings for testing args
  local_mocked_bindings(
    get_context_row = function() { 241
      },
    dt_add_data_template_ui = function(path, row, lt) {
      paste0("path: ", get_relative_path_org(path), " row: ", row, " lt-names: ", paste(names(lt), collapse=' ') )
      },
    dt_add_data_template_server = function(input, output, session) {
      paste0(" server")
      },
    runGadget = function(UI, SERVER, viewer) {
      paste0(UI, SERVER())
      }
  )

  expect_snapshot( paste0("TEMPLATE ADD_DATA: ", addin_datatable_add_data() ) )


  #### _____________ ####


  #### test addin : Datatable ADD GROUP ####


  ##### dt_add_group_ui() creates expected HTML #####

  # define variables
  rmd_path <- projectNoteRmdADG
  row <- 75

  # add a create datatable
  add_dt_create_test(rmd_path, row)

  row <- 100

  contents <- read_file(rmd_path)

  dts <- datatable_read_vector(contents[1:row])
  dt_names <- names(dts)
  lt <- as.list( 1:length(dt_names) )
  names(lt) <- dt_names

  group_declaration <- format_group_declaration_bullets(contents[row:row])

  expect_snapshot(dt_add_group_ui(rmd_path, row, lt, group_declaration))


  ##### dt_add_data_server() creates datatable ######

  # mock the rstudio context retrieval functions
  local_mocked_bindings(
    get_context_path = function() { rmd_path },
    get_context_row = function() { 100 },
    get_context_row_end = function() { 100 },
    get_context_contents = function() { read_file(rmd_path )} )

  # test adding two group cols - with 3 & 2 values resp
  shiny::testServer(dt_add_group_server, {

    # CHECK ERRORS
    session$setInputs(
      dt = "1", # selects first datatable - samples_CNS
      data_cols = "grup_delip MT DMT HT grup_inc 1Hr 2Hr", # groups string - typo in group
      summarise_reps = FALSE,
      all_reps = FALSE )

    Sys.sleep(0.2)
    session$setInputs(done=1)
    Sys.sleep(0.2)

    expect_equal(output$warningName, "Group Col definition does not contain a `group` col header!")

    # test group table generation
    session$setInputs(
      dt = "1", # selects first datatable - samples_CNS
      data_cols = "group_delip MT DMT HT group_inc 1Hr 2Hr", # groups string
      summarise_reps = FALSE,
      all_reps = FALSE )

    Sys.sleep(0.2)
    session$setInputs(done=1)
    Sys.sleep(0.2)

  })

  # test adding group cols via comments in TEMPLATE group with selection over the template

  row <- 122
  add_text_group_decl(rmd_path, row)

  contents <- read_file(rmd_path)
  row <- 124
  rowEnd <- 143
  group_declaration <- format_group_declaration_bullets(contents[row:rowEnd])

  #vset selection 124:143 - to capture group decl bullets
  local_mocked_bindings(
    get_context_path = function() { rmd_path },
    get_context_row = function() { 124 },
    get_context_row_end = function() { 143 },
    get_context_contents = function() { read_file(rmd_path )} )

  shiny::testServer(dt_add_group_server, {

    session$setInputs(
      dt = "1", # selects first datatable - samples_CNS
      data_cols = group_declaration, # groups string
      summarise_reps = FALSE,
      all_reps = FALSE )

    Sys.sleep(0.2)
    session$setInputs(done=1)
    Sys.sleep(0.2)

  })

  #### expect snapshot : rmd ####

  # check index Rmd file contents are correctly filled
  expect_snapshot_file( rmd_path )


  #### addin_datatable_add_groups() ####

  row <- 145 # after last group table
  rowEnd <- 145
  group_declaration <- format_group_declaration_bullets(contents[row:rowEnd])

  # mock ui and server functions to test higher infrastructure - return strings for testing args
  local_mocked_bindings(
    get_context_row = function() { 145
    },
    get_context_row_end = function() { 145
    },
    dt_add_group_ui = function(path, row, lt, group_declaration) {
      paste0("path: ", get_relative_path_org(path), " row: ", row, " lt-names: ", paste(names(lt), collapse=' '),
             " group_declaration: ", group_declaration, " - ")
    },
    dt_add_group_server = function(input, output, session) {
      paste0(" server")
    },
    runGadget = function(UI, SERVER, viewer) {
      paste0(UI, SERVER())
    }
  )

  expect_snapshot( paste0("ADD_GROUP: ", addin_datatable_add_groups() ) )


  #### _____________ ####

  #### test addin : Datatable RESAMPLE ####


  ##### dt_resample_ui() creates expected HTML #####

  # define variables
  rmd_path <- projectNoteRmdRSP
  row <- 75

  # add a create datatable
  add_dt_create_test_mice(rmd_path, row)

  row <- 100 # select beyond the newly created dt

  contents <- read_file(rmd_path)

  dts <- datatable_read_vector(contents[1:row])
  dt_names <- names(dts)
  lt <- as.list( 1:length(dt_names) )
  names(lt) <- dt_names

  expect_snapshot(dt_resample_ui(rmd_path, row, lt))


  ##### dt_resample_server() creates datatable ######

  # mock the rstudio context retrieval functions
  local_mocked_bindings(
    get_context_path = function() { rmd_path },
    get_context_row = function() { 100 },
    get_context_contents = function() { read_file(rmd_path )} )

  shiny::testServer(dt_resample_server, {

    session$setInputs(
      dt = "1", # selects first datatable - mice
      data_cols = "CNS SC DRG-L4-LT DRG-L4-RT" )

    Sys.sleep(0.2)
    session$setInputs(done=1)
    Sys.sleep(0.2)

  })


  #### dt_resample_template Function Testing ####

  add_ad_template_resample_datatables(rmd_path, 134)

  # test RESAMPLE Template with named output
  # named output datatable with ADD_DATA defines all inputs necessary
  # so no addin is required as no user input required!

  row <- 160 # select beyond the newly created dt
  # mock the rstudio context retrieval functions
  local_mocked_bindings(
    get_context_path = function() { rmd_path },
    get_context_row = function() { 160 },
    get_context_contents = function() { read_file(rmd_path )} )

  # execute parent function - as no addin is run with a named template RESAMPLE
  addin_datatable_resample() # will be checked via file snapshot!


  # test RESAMPLE Template with no named output

  ##### dt_resample_template_ui() creates expected HTML #####

  row <- 220 # selecting line inside template datatable

  dts <- datatable_read_vector(contents[1:row])
  dt_names <- names(dts)
  lt <- as.list( 1:length(dt_names) )
  names(lt) <- dt_names

  expect_snapshot(dt_resample_template_ui(rmd_path, row, lt))


  ##### dt_resample_template_server() gens datatable ######

  # mock the rstudio context retrieval functions
  local_mocked_bindings(
    get_context_path = function() { rmd_path },
    get_context_contents = function() { read_file(rmd_path )},
    get_context_row = function() { 220 } )

  shiny::testServer(dt_resample_template_server, {

    session$setInputs(
      dt = "12", # selects final datatable: samples2
      summarise_reps = FALSE,
      all_reps = FALSE )

    Sys.sleep(0.2)
    session$setInputs(done=1)
    Sys.sleep(0.2)

  })


  #### expect snapshot : rmd ####

  # check index Rmd file contents are correctly filled - both the create samples_CNS && create from template
  expect_snapshot_file( rmd_path )


  ##### addin_datatable_resample() #####

  ###### resample ######

  row <- 255

  # mock ui and server functions to test higher infrastructure - return strings for testing args
  local_mocked_bindings(
    get_context_row = function() { 255
    },
    dt_resample_ui = function(path, row, lt) {
      paste0("path: ", get_relative_path_org(path), " row: ", row, " lt-names: ", paste(names(lt), collapse=' ') )
    },
    dt_resample_server = function(input, output, session) {
      paste0(" server")
    },
    runGadget = function(UI, SERVER, viewer) {
      paste0(UI, SERVER())
    }
  )

  expect_snapshot( paste0("RESAMPLE: ", addin_datatable_resample() ) )

  ###### resample template ######

  row <- 255
  add_ad2_template_resample_datatables(rmd_path, row)

  row <- 260

  # mock ui and server functions to test higher infrastructure - return strings for testing args
  local_mocked_bindings(
    get_context_path = function() { rmd_path },
    get_context_contents = function() { read_file(rmd_path) },
    get_context_row = function() { 260
    },
    dt_resample_template_ui = function(path, row, lt) {
      paste0("path: ", get_relative_path_org(path), " row: ", row, " lt-names: ", paste(names(lt), collapse=' ') )
    },
    dt_resample_template_server = function(input, output, session) {
      paste0(" server")
    },
    runGadget = function(UI, SERVER, viewer) {
      paste0(UI, SERVER())
    }
  )

  expect_snapshot( paste0("TEMPLATE RESAMPLE: ", addin_datatable_resample() ) )


  #### _____________ ####


  #### test addin : Datatable DISPOSE ####


  ##### dt_dispose_ui() creates expected HTML #####

  # define variables
  rmd_path <- projectNoteRmdDSP
  row <- 75

  # add a create datatable
  add_dt_create_test_mice(rmd_path, row)

  row <- 100 # select beyond the newly created dt

  contents <- read_file(rmd_path)

  dts <- datatable_read_vector(contents[1:row])
  dt_names <- names(dts)
  lt <- as.list( 1:length(dt_names) )
  names(lt) <- dt_names

  cdt <- "2023-10-14:12:00"

  expect_snapshot(dt_dispose_ui(rmd_path, row, lt, cdt))


  ##### dt_dispose_server() creates datatable ######

  # mock the rstudio context retrieval functions
  local_mocked_bindings(
    get_context_path = function() { rmd_path },
    get_context_row = function() { 100 },
    get_context_contents = function() { read_file(rmd_path )} )

  shiny::testServer(dt_dispose_server, {

    session$setInputs(
      dt = "1", # selects first datatable
      cdt = "2023-10-14:12:00",
      summarise_reps = FALSE,
      all_reps = FALSE )

    Sys.sleep(0.2)
    session$setInputs(done=1)
    Sys.sleep(0.2)

  })


  #### dt_dispose_template Function Testing ####

  add_ad_template_dispose_datatables(rmd_path, 122)

  # test DISPOSE Template with named output
  # named output datatable with DISPOSE defines all inputs necessary
  # so no addin is required as no user input required!

  row <- 149 # select beyond the newly created dt
  # mock the rstudio context retrieval functions
  local_mocked_bindings(
    get_context_path = function() { rmd_path },
    get_context_row = function() { 149 },
    get_context_contents = function() { read_file(rmd_path )} )

  # execute parent function - as no addin is run with a named template DISPOSE
  addin_datatable_dispose() # will be checked via file snapshot!


  # test DISPOSE Template with no named output

  ##### dt_dispose_template_ui() creates expected HTML #####

  row <- 193 # selecting line inside template datatable

  dts <- datatable_read_vector(contents[1:row])
  dt_names <- names(dts)
  lt <- as.list( 1:length(dt_names) )
  names(lt) <- dt_names

  expect_snapshot(dt_dispose_template_ui(rmd_path, row, lt))


  ##### dt_dispose_template_server() gens datatable ######

  # mock the rstudio context retrieval functions
  local_mocked_bindings(
    get_context_path = function() { rmd_path },
    get_context_contents = function() { read_file(rmd_path )},
    get_context_row = function() { 193 } )

  shiny::testServer(dt_dispose_template_server, {

    session$setInputs(
      dt = "3", # selects final datatable: samples_CNS
      summarise_reps = FALSE,
      all_reps = FALSE )

    Sys.sleep(0.2)
    session$setInputs(done=1)
    Sys.sleep(0.2)

  })


  #### expect snapshot : rmd ####

  # check index Rmd file contents are correctly filled - both the create samples_CNS && create from template
  expect_snapshot_file( rmd_path )


  ##### addin_datatable_dispose() #####

  ###### dispose ######

  row <- 212

  # mock ui and server functions to test higher infrastructure - return strings for testing args
  local_mocked_bindings(
    get_context_row = function() { 212
    },
    dt_dispose_ui = function(path, row, lt, cdt) {
      paste0("path: ", get_relative_path_org(path), " row: ", row, " lt-names: ",
             paste(names(lt), collapse=' '), " cdt: 2024-10-14:12:00" )
    },
    dt_dispose_server = function(input, output, session) {
      paste0(" server")
    },
    runGadget = function(UI, SERVER, viewer) {
      paste0(UI, SERVER())
    }
  )

  expect_snapshot( paste0("DISPOSE: ", addin_datatable_dispose() ) )

  ###### dispose template ######

  row <- 212
  add_ad2_template_dispose_datatables(rmd_path, row)

  row <- 217

  # mock ui and server functions to test higher infrastructure - return strings for testing args
  local_mocked_bindings(
    get_context_path = function() { rmd_path },
    get_context_contents = function() { read_file(rmd_path) },
    get_context_row = function() { 217
    },
    dt_dispose_template_ui = function(path, row, lt) {
      paste0("path: ", get_relative_path_org(path), " row: ", row, " lt-names: ", paste(names(lt), collapse=' ') )
    },
    dt_dispose_template_server = function(input, output, session) {
      paste0(" server")
    },
    runGadget = function(UI, SERVER, viewer) {
      paste0(UI, SERVER())
    }
  )

  expect_snapshot( paste0("TEMPLATE DISPOSE: ", addin_datatable_dispose() ) )


  #### _____________ ####


  #### test addin : Datatable IMPORT EXPORT ####

  # add some datatables to the export notes
  dt_find_add_create_dt1(projectNoteRmdEx1, 75)
  dt_find_add_create_dt2(projectNoteRmdEx2, 75)
  dt_find_add_create_dt3(projectNoteRmdEx3, 75)
  # and subnote: test ability to write unique ID
   # && read from sub-dir note & produce correct link
  dt_find_add_create_dt1(projectSubRmdEx1, 75)

  testdatapath <- fs::path_dir(projectNoteRmdEx1)

  rmd_path <- fs::path(projectNoteRmdIm1)
  row <- 75

  ##### dt_import_export_ui() creates expected HTML #####

  expect_snapshot(dt_import_export_ui(rmd_path, row))


  ##### dt_import_export_server() creates datatables ######

  # get orgPath
  orgPath <- find_org_directory(rmd_path)

  # extract summary datatable and manually modify the IMPORT col
  sDT <- datatable_find(testdatapath, settings)
  sDT$IMPORT[1] <- 1 # IMPORT/EXPORT 1001 from subnote 001
  sDT$IMPORT[2] <- 1 # IMPORT/EXPORT 1002 from subnote 001
  sDT$IMPORT[5] <- 1 # IMPORT/EXPORT 1001 from note 001
  sDT$IMPORT[7] <- 1 # IMPORT/EXPORT 1003 from note 001
  sDT$IMPORT[9] <- 1 # IMPORT/EXPORT 2001 from note 002
  sDT$IMPORT[13] <- 1 # IMPORT/EXPORT 3001 from note 003

  # mock the rstudio context retrieval functions
  local_mocked_bindings(
    get_context_path = function() { rmd_path },
    get_context_row = function() { 75 },
    get_context_contents = function() { read_file(rmd_path )},
    find_org_directory = function(path) { orgPath } )
  # must ensure the find_org_directory returns the orgPath correctly!

  shiny::testServer(dt_import_export_server, {

    #cat("b4 output$dirtxt ", output$dirtxt, "\n")

    session$setInputs(
      dir = list(
        root=orgPath,
        path=list("","0-PR-DT", "PD", "tn-ex")
        ) # must include a blank string as first entry in list for path!
    )

    cat("output$dirtxt ", output$dirtxt, "\n")
    cat("value of global$datapath: ", global$datapath , "\n")
    cat("value of summary names (col headers): ", names(global$summary), "\n")
    cat("value of summary ID col: ", global$summary$ID, "\n")
    cat("value of summary IMPORT col: ", global$summary$IMPORT, "\n")

    # Validate initial state
    expect_equal(global$summary$IMPORT, rep(0, 16))

    # Simulate selecting several rows, then clicking the increment button
    session$setInputs(mytable1_rows_selected = c(1, 2, 5, 7, 9, 13))
    session$setInputs(increment = 1)  # Simulate button click
    expect_equal(global$summary$IMPORT, c(1, 1, 0, 0, 1, 0, 1, 0, 1, 0, 0, 0, 1, 0, 0, 0))  # Rows incremented

    # Simulate selecting rows 1, 2 then clicking the decrement button
    session$setInputs(mytable1_rows_selected = c(1, 2))
    session$setInputs(decrement = 1)  # Simulate button click
    expect_equal(global$summary$IMPORT, c(0, 0, 0, 0, 1, 0, 1, 0, 1, 0, 0, 0, 1, 0, 0, 0))  # Rows in/decremented

    # Simulate selecting rows 1 2, then clicking the increment button
    session$setInputs(mytable1_rows_selected = c(1, 2))
    session$setInputs(increment = 2)  # Simulate button click
    expect_equal(global$summary$IMPORT, c(1, 1, 0, 0, 1, 0, 1, 0, 1, 0, 0, 0, 1, 0, 0, 0))  # Rows incremented

#
#     # can edit the summary datatable in place directly
#     global$summary$IMPORT[1] <- 1 # IMPORT/EXPORT 1001 from subnote 001
#     global$summary$IMPORT[2] <- 1 # IMPORT/EXPORT 1002 from subnote 001
#     global$summary$IMPORT[5] <- 1 # IMPORT/EXPORT 1001 from note 001
#     global$summary$IMPORT[7] <- 1 # IMPORT/EXPORT 1003 from note 001
#     global$summary$IMPORT[9] <- 1 # IMPORT/EXPORT 2001 from note 002
#     global$summary$IMPORT[13] <- 1 # IMPORT/EXPORT 3001 from note 003
    cat("value of summary IMPORT col post-inc: ", global$summary$IMPORT, "\n")

    # perform the import/export
    Sys.sleep(0.2)
    session$setInputs(done=1)
    Sys.sleep(0.2)

  })


  #### expect snapshot : rmds ####

  # check all Rmd files have correct export and import datatables written
  expect_snapshot_file(projectNoteRmdEx1)
  expect_snapshot_file(projectNoteRmdEx2)
  expect_snapshot_file(projectNoteRmdEx3)
  expect_snapshot_file(projectSubRmdEx1)

  expect_snapshot_file(projectNoteRmdIm1)


  ##### addin_datatable_import_export() #####

  ###### dispose ######

  row <- 212

  # mock ui and server functions to test higher infrastructure - return strings for testing args
  local_mocked_bindings(
    get_context_row = function() { 212
    },
    dt_import_export_ui = function(path, row) {
      paste0("path: ", get_relative_path_org(path), " row: ", row )
    },
    dt_import_export_server = function(input, output, session) {
      paste0(" server")
    },
    runGadget = function(UI, SERVER, viewer) {
      paste0(UI, SERVER())
    }
  )

  expect_snapshot( paste0("IMPORT EXPORT: ", addin_datatable_import_export() ) )

  #### _____________ ####


})



