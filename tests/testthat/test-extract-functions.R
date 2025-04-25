
test_that("test extract functions", {


  ################ generate test organisation temp directory ###################

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
  #local_add_todos_to_sub_note1(subNoteRmd)
  #local_add_todos_to_sub_note2(subNoteRmd2)
  #local_add_todos_to_sub_note3(subNoteRmd3)
  #local_add_todos_to_group_note(groupNoteRmd)
  #local_add_todos_to_project_doc(projectDocRmd)
  #local_add_todos_to_prog_index(progIndex)
  #local_add_todos_to_org_index(orgIndex)


  # mock geo location for fixed return - avoid using internet search!
  local_mocked_bindings(
    get_geo_loc = function(location_str) {
      return( list(lat=51.52, long=-0.14) )
    }, # for consistent latitude and longitude values
    get_locale = function() {
      return("Europe/London")
    }) # for consistent locale string

  # create journal in Org
  date=lubridate::ymd("2024-05-10") #  this date is interesting as it has no moonset time!
  organisationPath=orgDir

  journalRmd <- local_create_journal(date, organisationPath, authorValue)


  # mock the function that returns the date
  local_mocked_bindings(
    get_date = function (timezone = get_locale(), split = "-") { "2025-02-22" } )

  # function variables - for interactive testing
  # location <- projectNotePath
  # date=get_date()
  # fromFilePath <- journalRmd # using journal Rmd to mimic extraction to this location
  # onlyFirstTodoPerFile <- FALSE # flags for sorting todo items
  # sortByFileModTimeDesc <- TRUE
  # priorities = list(
  #   DEADLINE_PASSED = 4,
  #   SCHEDULE_PASSED = 3,
  #   DUE_TODAY       = 2,
  #   SCHEDULED_TODAY = 1
  # )
  # collectionTemplate <- "Todo-Collection-Template.Rmd" # todo templates
  # noteTemplate <- "Todo-Note-Template.Rmd"
  # itemTemplate <- "Todo-Item-Template.Rmd"
  # priorityFirst <- FALSE

  # extract todo items
  todoItems <- extract_todos(
    location = projectNotePath,
    date=get_date(),
    fromFilePath=journalRmd, # using journal Rmd to mimic extraction to this location
    onlyFirstTodoPerFile = FALSE, # flags for sorting todo items
    sortByFileModTimeDesc = TRUE,
    priorities = list(
      DEADLINE_PASSED = 4,
      SCHEDULE_PASSED = 3,
      DUE_TODAY       = 2,
      SCHEDULED_TODAY = 1,
      OPEN_ITEMS      = 0
    ),
    collectionTemplate = "Todo-Collection-Template.Rmd", # todo templates
    noteTemplate = "Todo-Note-Template.Rmd",
    itemTemplate = "Todo-Item-Template.Rmd",
    priorityTemplate   = "Todo-Priority-Template.Rmd",
    priorityFirst = FALSE)

  # extract todo items
  todoItemsP <- extract_todos(
    location = projectNotePath,
    date=get_date(),
    fromFilePath=journalRmd, # using journal Rmd to mimic extraction to this location
    onlyFirstTodoPerFile = FALSE, # flags for sorting todo items
    sortByFileModTimeDesc = TRUE,
    priorities = list(
      DEADLINE_PASSED = 4,
      SCHEDULE_PASSED = 3,
      DUE_TODAY       = 2,
      SCHEDULED_TODAY = 1,
      OPEN_ITEMS      = 0
    ),
    collectionTemplate = "Todo-Collection-Template.Rmd", # todo templates
    noteTemplate = "Todo-Note-Template.Rmd",
    itemTemplate = "Todo-Item-Template.Rmd",
    priorityTemplate   = "Todo-Priority-Template.Rmd",
    priorityFirst = TRUE)


  ## TESTS ##

  # test todo items correctly extracted
  expect_snapshot(todoItems)

  expect_snapshot(todoItemsP)


})


#### ____ ####

#### extract_google_calendar_events() ####


test_that("Handles mixed all-day and time events with real-data mock", {

  fake_events <- function(cal_id, tmin, tmax) {
    list(items = list(
      summary = c("test all day event", "test event", "NEUROVISION"),
      description = c(
        "here is a description of an all day event",
        paste("here is a test event description",
              "here is a second description line",
              "here is a third line", sep = "\n"),
        NA_character_
      ),
      location = c("London, UK", "London, UK", NA_character_),
      start = data.frame(
        date = c("2025-04-23", NA_character_, NA_character_),
        dateTime = c(NA_character_, "2025-04-23T10:00:00+01:00", "2025-04-23T14:00:00+01:00"),
        timeZone = c(NA_character_, "Europe/London", "Europe/London")
      ),
      end = data.frame(
        date = c("2025-04-24", NA_character_, NA_character_),
        dateTime = c(NA_character_, "2025-04-23T11:00:00+01:00", "2025-04-23T16:00:00+01:00"),
        timeZone = c(NA_character_, "Europe/London", "Europe/London")
      ),
      htmlLink = c(
        "https://www.google.com/calendar/event?eid=MTJ0YzBudWcxNQ32OGIxdWpqZzd0OTZ2NjIgc3RldmVuam9ud2VzdtEB",
        "https://www.google.com/calendar/event?eid=MjM0MmxmcG8yaNnqYmgxODk4bXI0ZGZwdGcgc3RldmVuam9ud2VzdtEB",
        "https://www.google.com/calendar/event?eid=MWltMWU0YW5rbFXxZGE3c2plczQ3aW42MDUgc3RldmVuam9ud2VzdtEB"
      )
    ))
  }


  result <- extract_google_calendar_events(
    day = as.Date("2025-04-23"),
    auth_fn = function() NULL,
    calendar_list_fn = function() tibble::tibble(id = "1", summary = "Events"),
    events_fn = fake_events
  )

  expect_equal(nrow(result), 3)
  expect_true(all(result$calendar == "Events"))
  expect_true(any(result$start == "2025-04-23"))
  expect_true(any(result$start == "2025-04-23T10:00:00+01:00"))
  expect_true(any(result$start == "2025-04-23T14:00:00+01:00"))
})



test_that("Handles only all-day events", {
  fake_events <- function(cal_id, tmin, tmax) {
    list(items = list(
      summary = c("All Day 1", "All Day 2"),
      description = c("desc1", "desc2"),
      location = c("Loc1", "Loc2"),
      start = data.frame(date = c("2025-04-23", "2025-04-23"),
                         dateTime = NA_character_,
                         timeZone = NA_character_),
      end = data.frame(date = c("2025-04-24", "2025-04-24"),
                       dateTime = NA_character_,
                       timeZone = NA_character_),
      htmlLink = c("http://a.com", "http://b.com")
    ))
  }

  result <- extract_google_calendar_events(
    day = as.Date("2025-04-23"),
    auth_fn = function() NULL,
    calendar_list_fn = function() tibble::tibble(id = "1", summary = "AllDay"),
    events_fn = fake_events
  )

  expect_equal(nrow(result), 2)
  expect_true(all(result$start == "2025-04-23"))
})


test_that("Handles only time-specific events", {
  fake_events <- function(cal_id, tmin, tmax) {
    list(items = list(
      summary = c("Time 1", "Time 2"),
      description = c("desc1", "desc2"),
      location = c("Loc1", "Loc2"),
      start = data.frame(date = NA_character_,
                         dateTime = c("2025-04-23T10:00:00+01:00", "2025-04-23T12:00:00+01:00"),
                         timeZone = rep("Europe/London", 2)),
      end = data.frame(date = NA_character_,
                       dateTime = c("2025-04-23T11:00:00+01:00", "2025-04-23T13:00:00+01:00"),
                       timeZone = rep("Europe/London", 2)),
      htmlLink = c("http://a.com", "http://b.com")
    ))
  }

  result <- extract_google_calendar_events(
    day = as.Date("2025-04-23"),
    auth_fn = function() NULL,
    calendar_list_fn = function() tibble::tibble(id = "1", summary = "TimeOnly"),
    events_fn = fake_events
  )

  expect_equal(nrow(result), 2)
  expect_true(all(grepl("T", result$start)))
})


test_that("Handles calendar with no events", {
  fake_events <- function(cal_id, tmin, tmax) {
    list(items = list())
  }

  result <- extract_google_calendar_events(
    day = as.Date("2025-04-23"),
    auth_fn = function() NULL,
    calendar_list_fn = function() tibble::tibble(id = "1", summary = "Empty"),
    events_fn = fake_events
  )

  expect_equal(nrow(result), 0)
})


test_that("Handles missing location, description, and htmlLink", {
  fake_events <- function(cal_id, tmin, tmax) {
    list(items = list(
      summary = c("Missing fields"),
      start = data.frame(date = "2025-04-23",
                         dateTime = NA_character_,
                         timeZone = NA_character_),
      end = data.frame(date = "2025-04-24",
                       dateTime = NA_character_,
                       timeZone = NA_character_)
      # No description, location, htmlLink
    ))
  }

  result <- extract_google_calendar_events(
    day = as.Date("2025-04-23"),
    auth_fn = function() NULL,
    calendar_list_fn = function() tibble::tibble(id = "1", summary = "Missing"),
    events_fn = fake_events
  )

  expect_equal(nrow(result), 1)
  expect_true(is.na(result$description))
  expect_true(is.na(result$htmlLink))
  expect_true(is.na(result$location))
})



test_that("default_google_auth uses injected methods", {
  mock_set_client <- function(json, scopes) {
    expect_match(json, "projectmanagr_gcal_oauth_client.json")
    expect_equal(scopes, "https://www.googleapis.com/auth/calendar.readonly")
  }

  mock_auth <- function(email) {
    expect_true(email)
    "mock_token"
  }

  result <- default_google_auth(
    gar_set_client = mock_set_client,
    gar_auth = mock_auth
  )

  expect_equal(result, "mock_token")
})


test_that("default_calendar_list_fn uses injected generator", {
  mock_gen <- function(...) {
    function() list(items = list(
      id = c("cal1", "cal2"),
      summary = c("Main", "Other")
    ))
  }

  result <- default_calendar_list_fn(gar_api_generator = mock_gen)
  expect_equal(nrow(result), 2)
  expect_named(result, c("id", "summary"))
})

test_that("default_events_list_fn constructs and calls API generator correctly", {
  fake_data <- list(
    items = list(
      summary = c("Test event"),
      start = data.frame(dateTime = "2025-04-23T10:00:00+01:00"),
      end   = data.frame(dateTime = "2025-04-23T11:00:00+01:00")
    )
  )

  # Mock gar_api_generator
  mock_gen <- function(url, method, pars_args, data_parse_function) {
    expect_equal(method, "GET")
    expect_true(grepl("https://www.googleapis.com/calendar/v3/calendars/", url))
    expect_true(grepl("events", url))

    expect_equal(pars_args$singleEvents, "true")
    expect_equal(pars_args$orderBy, "startTime")
    expect_equal(pars_args$maxResults, 2500)

    # Return function that simulates API call
    function() fake_data
  }

  # Call with injected mock
  cal_id <- "test_calendar@example.com"
  tmin <- as.POSIXct("2025-04-23", tz = "UTC")
  tmax <- tmin + lubridate::days(1)

  result <- default_events_list_fn(
    cal_id, tmin, tmax,
    gar_api_generator = mock_gen
  )

  expect_type(result, "list")
  expect_named(result, "items")
  expect_equal(result$items$summary[[1]], "Test event")
})


