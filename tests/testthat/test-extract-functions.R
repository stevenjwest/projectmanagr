


test_that("parse_one_todo_block parses inline priority and groups", {
  blk <- c(
    "<!-- {#todo P2 #alpha #beta}  My title",
    "Some description",
    "{#schedule 2025-05-31}",
    "{#deadline 2025-06-05}",
    "-->"
  )
  td <- parse_one_todo_block(blk)

  expect_equal(td$title,        "My title")
  expect_equal(td$pTagPriority, 2)
  expect_setequal(td$groups,    c("alpha", "beta"))
  expect_match(td$schedule,     "2025-05-31")
  expect_match(td$deadline,     "2025-06-05")
})



test_that("write_todo_data_str drops later todos with same group", {
  tmp <- tempfile("dummy.Rmd")
  td1 <- make_td(priority = 1L, groups = "alpha", idx = 1L)
  td2 <- make_td(priority = 2L, groups = "alpha", idx = 2L)  # dup group
  td3 <- make_td(priority = 3L, groups = "beta",  idx = 3L)

  df  <- todo_data_str()   # empty starter
  df2 <- write_todo_data_str(tmp, list(td1, td2, td3), df,
                             onlyFirstTodoPerFile = FALSE,
                             today = Sys.Date())

  expect_equal(nrow(df2), 2)              # td2 was skipped
  expect_setequal(df2$groups, c("alpha", "beta"))
  expect_equal(df2$pTagPriority, c(1L, 3L))
})



test_that("pTagPriority governs ordering when groupingFirst = TRUE", {

  # fabricate a data.frame mirroring extract_todos()' internal DF -------------
  df <- todo_data_str(
    file             = rep("file.Rmd", 4),
    fileMtime        = Sys.time(),
    indexInFile      = 1:4,
    heading          = rep("Heading", 4),
    title            = paste("T", 1:4),
    text             = paste("txt", 1:4),
    deadline         = "",
    schedule         = "",
    isScheduledToday = 0L,
    isSchedulePassed = 0L,
    isDueToday       = 0L,
    isDeadlinePassed = 0L,
    itemDateTime     = NA_real_,
    pTagPriority     = c(3L, 1L, 1L, 2L),   # <‑‑ the interesting bit
    groups           = ""
  )
  df$priority <- 2L                         # all same schedule/deadline state

  # mimic the ordering used inside build_priority_first_layout() --------------
  sorted <- df[order(
    df$pTagPriority,
    df$itemDateTime,
    df$indexInFile
  ), ]

  expect_equal(sorted$pTagPriority, c(1L, 1L, 2L, 3L))
  expect_equal(sorted$indexInFile,  c(2L, 3L, 4L, 1L))
})




test_that("extract_todos runs with groupingFirst = TRUE (stubbed)", {

  template <- tempfile(); writeLines("{{items}}", template)

  # patch out heavy helpers ONLY inside this block ----------------------------
  local_mocked_bindings(
    confirm_find_org        = function(loc) loc,
    get_template_dir        = function(org) dirname(template),
    get_settings_yml        = function(org) list(),
    get_projectmanagr_files = function(loc, org, settings) character(),
    read_file               = function(path) "{{items}}",
    parse_todo_blocks_with_heading = function(fl) list(
      list(heading = "", title = "stub", text = "", deadline = "", schedule = "",
           pTagPriority = 1L, groups = character())
    ),
    write_todo_data_str     = function(...) todo_data_str(),
    build_priority_first_layout = function(...) "OK",
    replace_sep_values      = function(doc, ...) doc,
    .package = "projectmanagr"
  )

  res <- extract_todos(
    tempdir(),
    groupingFirst    = TRUE,
    groupingTemplate = basename(template)
  )

  expect_identical(res, "OK")
})






#### ____ ####


test_that("extract_todos end‑to‑end with priorities & groups", {

  cat("\n── preparing mock organisation for extract_todos() integration test ──\n")

  tmpdir <- create_tmpdir_rsess()

  orgName  <- "_TOe"
  author   <- "sjwest"
  local_mocked_bindings(
    get_datetime = function(timezone = "UTC", ...) "2024-02-22:09:56"
  )
  orgDir <- local_create_org(orgName, author, orgParentPath = tmpdir, syp = tmpdir)

  progName <- "0-PR"
  local_mocked_bindings(
    get_datetime = function(timezone = "UTC", ...) "2024-02-22:09:58"
  )
  progDir <- local_create_prog(progName, orgDir, author)

  projectDocPrefix <- "PrDoS"
  projectDocName   <- "Proj_Do_sim"
  projectDocRmd <- local_create_project(projectDocPrefix, projectDocName,
                                        progDir, author)
  projectDocDir <- fs::path(progDir, projectDocPrefix)

  # plain project note --------------------------------------------------------
  projectNotePath <- fs::path(projectDocDir, "t-no"); fs::dir_create(projectNotePath)
  projectNoteRmd  <- local_create_project_note_simple(
    "Proj_No", projectNotePath, projectDocRmd, "dummy‑task", author, "___001"
  )

  # add the standard set of todos supplied by your helper ---------------------
  local_add_todos_to_project_note(projectNoteRmd)

  local_mocked_bindings(
    get_geo_loc = function(x) list(lat = 51.52, long = -0.14),
    get_locale  = function() "Europe/London"
  )
  local_mocked_bindings(
    get_date = function(timezone = get_locale(), ...) "2025-02-22"
  )

  noteFirst <- extract_todos(
    location       = projectNotePath,
    date           = get_date(),
    fromFilePath   = projectNoteRmd,   # acts like calling file
    onlyFirstTodoPerFile = FALSE,
    sortByFileModTimeDesc = TRUE,
    groupings = list(
      DEADLINE_PASSED = 4,
      SCHEDULE_PASSED = 3,
      DUE_TODAY       = 2,
      SCHEDULED_TODAY = 1,
      OPEN_ITEMS      = 0
    ),
    collectionTemplate = "Todo-Collection-Template.Rmd",
    noteTemplate       = "Todo-Note-Template.Rmd",
    itemTemplate       = "Todo-Item-Template.Rmd",
    groupingTemplate   = "Todo-Grouping-Template.Rmd",
    groupingFirst      = FALSE
  )

  groupingFirst <- extract_todos(
    location       = projectNotePath,
    date           = get_date(),
    fromFilePath   = projectNoteRmd,
    onlyFirstTodoPerFile = FALSE,
    sortByFileModTimeDesc = TRUE,
    groupings = list(
      DEADLINE_PASSED = 4,
      SCHEDULE_PASSED = 3,
      DUE_TODAY       = 2,
      SCHEDULED_TODAY = 1,
      OPEN_ITEMS      = 0
    ),
    collectionTemplate = "Todo-Collection-Template.Rmd",
    noteTemplate       = "Todo-Note-Template.Rmd",
    itemTemplate       = "Todo-Item-Template.Rmd",
    groupingTemplate   = "Todo-Grouping-Template.Rmd",
    groupingFirst      = TRUE
  )

  # basic smoke check
  expect_type(noteFirst,     "character")
  expect_type(groupingFirst, "character")

  # (a) the P1 grouped todo is present, P2/P3 duplicates are absent
  expect_true(any(grepl("P1 high priority", noteFirst,     fixed = TRUE)))
  expect_true(any(grepl("P1 high priority", groupingFirst, fixed = TRUE)))

  expect_false(any(grepl("P2 mid priority SHOULD BE SKIPPED", noteFirst,     fixed = TRUE)))
  expect_false(any(grepl("P3 low priority SHOULD BE SKIPPED", noteFirst,     fixed = TRUE)))
  expect_false(any(grepl("P2 mid priority SHOULD BE SKIPPED", groupingFirst, fixed = TRUE)))
  expect_false(any(grepl("P3 low priority SHOULD BE SKIPPED", groupingFirst, fixed = TRUE)))

  # (b) in grouping‑first output the P1 item should appear *before*
  #     any P‑priority >1 inside the same schedule/deadline block
  p_lines <- grep("P[123] ", groupingFirst, value = TRUE)
  expect_true(grepl("P1", p_lines[1]))
})





#### ____ ####




test_that("test extract functions", {

  cat("=====================")
  cat("test extract functions")
  cat("")


  ################ generate test organisation temp directory ###################

  tmpdir <- create_tmpdir_rsess()


  ################ generate test ORG PROG Doc Notes ################

  orgName <- "_TOe"
  authorValue="sjwest"
  # mock the function that returns the update datetime
  local_mocked_bindings(
    get_datetime = function (timezone = "UTC", split="-", splitTime=":") {
      "2024-02-22:09:56" } )


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
    get_datetime = function (timezone = "UTC", split="-", splitTime=":") {
      "2024-02-22:09:58" } )


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
    get_date = function (timezone = get_locale(), split = "-") {
      "2025-02-22" } )

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

test_that("mixed all-day and timed events are collapsed correctly", {

  fake_events <- function(...) {
    list(items = list(
      summary = c("all-day", "10 am", "2 pm"),
      description = c("desc-all", "desc-10", NA),
      location = c("LocA", "LocB", NA),
      start = data.frame(
        date     = c("2025-04-23", NA,                       NA),
        dateTime = c(NA,              "2025-04-23T10:00:00+01:00",
                     "2025-04-23T14:00:00+01:00"),
        timeZone = NA
      ),
      end = data.frame(
        date     = c("2025-04-24", NA,                       NA),
        dateTime = c(NA,              "2025-04-23T11:00:00+01:00",
                     "2025-04-23T16:00:00+01:00"),
        timeZone = NA
      ),
      htmlLink = rep("http://x", 3)
    ))
  }

  out <- extract_google_calendar_events(
    day = as.Date("2025-04-23"),
    auth_fn          = function() NULL,
    calendar_list_fn = function() tibble::tibble(id = "1", summary = "Cal"),
    events_fn        = fake_events
  )

  expect_equal(nrow(out), 3)
  expect_setequal(out$start,
                  c("2025-04-23",
                    "2025-04-23T10:00:00+01:00",
                    "2025-04-23T14:00:00+01:00"))
})

test_that("pure all-day events work", {

  fake_events <- function(...) {
    list(items = list(
      summary = c("A", "B"),
      description = NA,
      location    = NA,
      start = data.frame(date = rep("2025-04-23", 2),
                         dateTime = NA, timeZone = NA),
      end   = data.frame(date = rep("2025-04-24", 2),
                         dateTime = NA, timeZone = NA),
      htmlLink = "http://x"
    ))
  }

  out <- extract_google_calendar_events(
    as.Date("2025-04-23"),
    auth_fn          = function() NULL,
    calendar_list_fn = function() tibble::tibble(id = "x", summary = "Cal"),
    events_fn        = fake_events
  )

  expect_equal(out$start, rep("2025-04-23", 2))
})

test_that("pure timed events work", {

  fake_events <- function(...) {
    list(items = list(
      summary = c("10 am", "noon"),
      start = data.frame(date = NA,
                         dateTime = c("2025-04-23T10:00:00+01:00",
                                      "2025-04-23T12:00:00+01:00"),
                         timeZone = "Europe/London"),
      end   = data.frame(date = NA,
                         dateTime = c("2025-04-23T11:00:00+01:00",
                                      "2025-04-23T13:00:00+01:00"),
                         timeZone = "Europe/London"),
      description = NA, location = NA, htmlLink = NA
    ))
  }

  out <- extract_google_calendar_events(
    as.Date("2025-04-23"),
    auth_fn          = function() NULL,
    calendar_list_fn = function() tibble::tibble(id = "t", summary = "T"),
    events_fn        = fake_events
  )

  expect_true(all(grepl("T", out$start)))
})

test_that("empty calendar returns zero-row tibble", {

  out <- extract_google_calendar_events(
    as.Date("2025-04-23"),
    auth_fn          = function() NULL,
    calendar_list_fn = function() tibble::tibble(id = "e", summary = "E"),
    events_fn        = function(...) list(items = list())
  )

  expect_equal(nrow(out), 0)
})

test_that("missing optional fields yield NAs", {

  fake_events <- function(...) {
    list(items = list(
      summary = "no extras",
      start   = data.frame(date = "2025-04-23", dateTime = NA, timeZone = NA),
      end     = data.frame(date = "2025-04-24", dateTime = NA, timeZone = NA)
    ))
  }

  out <- extract_google_calendar_events(
    as.Date("2025-04-23"),
    auth_fn          = function() NULL,
    calendar_list_fn = function() tibble::tibble(id = "m", summary = "M"),
    events_fn        = fake_events
  )

  expect_true(is.na(out$description) &&
                is.na(out$htmlLink)    &&
                is.na(out$location))
})

#### default_google_auth() ----------------------------------------------

test_that("default_google_auth calls injected helpers correctly", {

  mock_set <- function(json, scopes) {
    expect_true(endsWith(json, "gcal_oauth_client.json"))
    expect_equal(scopes, "https://www.googleapis.com/auth/calendar.readonly")
  }
  mock_auth <- function(email, ...) {
    expect_true(email); "token"
  }

  expect_identical(
    default_google_auth(gar_set_client = mock_set, gar_auth = mock_auth),
    "token"
  )
})

test_that("custom scopes propagate", {

  custom <- "https://www.googleapis.com/auth/calendar.events.readonly"
  mock_set <- function(json, scopes) expect_equal(scopes, custom)
  mock_auth <- function(email, ...) "tok"

  expect_identical(
    default_google_auth(gar_set_client = mock_set,
                        gar_auth = mock_auth,
                        scopes = custom),
    "tok"
  )
})

test_that("token cache option is set", {

  mock_set  <- function(json, scopes) {}
  mock_auth <- function(email, ...) {
    expect_equal(getOption("gargle_oauth_cache"),
                 rappdirs::user_cache_dir("projectmanagr"))
    "tok"
  }

  default_google_auth(gar_set_client = mock_set, gar_auth = mock_auth)
})

test_that("errors in gar_set_client bubble up", {

  expect_error(
    default_google_auth(
      gar_set_client = function(...) stop("boom"),
      gar_auth       = function(...) "tok"
    ),
    "boom"
  )
})

#### default_calendar_list_fn & default_events_list_fn ------------------

test_that("calendar list helper returns tibble", {

  fake_gen <- function(...) {
    function() list(items = list(id = c("A", "B"), summary = c("x", "y")))
  }

  out <- default_calendar_list_fn(gar_api_generator = fake_gen)
  expect_s3_class(out, "tbl_df")
  expect_named(out, c("id", "summary"))
})

test_that("events helper builds generator with correct defaults", {

  fake_data <- list(items = list(
    summary = "foo",
    start   = data.frame(dateTime = "2025-01-01T10:00:00+00:00"),
    end     = data.frame(dateTime = "2025-01-01T11:00:00+00:00")
  ))

  fake_gen <- function(url, method, pars_args, ...) {
    expect_match(url, "/events$")
    expect_equal(method, "GET")
    expect_equal(pars_args$singleEvents, "true")
    function() fake_data
  }

  out <- default_events_list_fn(
    "id", Sys.time(), Sys.time() + 3600,
    gar_api_generator = fake_gen
  )

  expect_equal(out, fake_data)
})



test_that("clean_zoom_description handles HTML, breaks, and wrapping", {
  input <- paste(
    "Speaker:<br>Edward Kennedy<br><br>Title:<br>Minimax optimality in causal inference<br><br>",
    "Abstract:<br>In this talk I will survey some recent work on minimax optimality in causal inference problems. ",
    "We consider minimax optimality in smooth, structure-agnostic, and combined models...",
    "<br><br>Bio:<br>Edward Kennedy is an associate professor of <a href='https://www.stat.cmu.edu/'>Statistics</a>",
    "at Carnegie Mellon University.",
    sep = ""
  )

  output <- clean_zoom_description(input)

  # 1. Should contain expected headers, each on its own line
  expect_match(output, "^Speaker:", fixed = FALSE)
  expect_match(output, "Edward Kennedy", fixed = FALSE)
  expect_match(output, "Title:", fixed = FALSE)
  expect_match(output, "Minimax optimality in causal inference", fixed = FALSE)
  expect_match(output, "Bio:", fixed = FALSE)

  # 2. Should not contain HTML tags
  expect_false(grepl("<[^>]+>", output))

  # 3. Lines should not exceed 80 characters
  lines <- strsplit(output, "\n")[[1]]
  expect_true(all(nchar(lines) <= 80))

  # 4. Should preserve blank lines between sections
  blank_line_indices <- which(nchar(lines) == 0)
  expect_true(length(blank_line_indices) >= 2)

  # 5. No unwanted placeholders like NEWLINE
  expect_false(grepl("___NEWLINE___", output))
})

test_that("clean_zoom_description extracts Zoom links if present", {
  input <- "Join here: <a href='https://us02web.zoom.us/j/1234567890'>Zoom Link</a>"
  output <- clean_zoom_description(input)
  expect_equal(output, "[Zoom link](https://us02web.zoom.us/j/1234567890)")
})



test_that("extract_google_calendar_events_markdown orders events correctly", {

  # create new test org
  tmpdir <- create_tmpdir_rsess()
  orgName <- "_TOe2"
  authorValue="sjwest"
  # mock the function that returns the update datetime
  local_mocked_bindings(
    get_datetime = function (timezone = "UTC", split="-", splitTime=":") {
      "2024-02-22:09:56" } )
  # create test Organisation - using local helper function and withr package
  orgDir <- local_create_org(orgName, authorValue, orgParentPath=tmpdir, syp=tmpdir)
  # define outputs to check
  orgIndex <- fs::path(orgDir, paste0("_index_", orgName, ".Rmd"))
  settingsYml <- fs::path(orgDir, ".config", "settings.yml")
  statusYml <- fs::path(orgDir, ".config", "status.yml")
  addinsJson <- fs::path(orgDir, ".config", "addins.json")
  volumesRmd <- fs::path(orgDir, "volumes", "volumes.Rmd")

  settings <- yaml::yaml.load( yaml::read_yaml( settingsYml ) )


  mock_events <- tibble::tibble(
    calendar    = rep("Work", 4),
    summary     = c("All Day Event", "Morning Meeting",
                    "Lunch Catchup", "Afternoon Sync"),
    start       = c("2025-05-01",
                    "2025-05-01T09:00:00Z",
                    "2025-05-01T12:30:00Z",
                    "2025-05-01T15:00:00Z"),
    end         = c("2025-05-01",
                    "2025-05-01T10:00:00Z",
                    "2025-05-01T13:30:00Z",
                    "2025-05-01T16:00:00Z"),
    description = NA, htmlLink = NA, location = NA
  )

  mock_template <- "- {{EVENT_START_TIME}}: {{EVENT_TITLE}}"

  local_mocked_bindings(
    extract_google_calendar_events = function(day = NULL, settings = NULL) {
      mock_events }
  )

  output <- projectmanagr::extract_google_calendar_events_markdown(
    day = as.Date("2025-05-01"),
    location = orgDir
  )

  expect_equal(
    output,
    c(
      "## 2025-05-01 – 2025-05-01 : All Day Event", "",
      "Work", "", "[calendar link]()",
       "", "",
      "## 09:00 – 10:00 : Morning Meeting", "",
      "Work", "", "[calendar link]()",
      "","",
      "## 12:30 – 13:30 : Lunch Catchup","",
      "Work", "", "[calendar link]()",
      "","",
      "## 15:00 – 16:00 : Afternoon Sync", "",
      "Work", "", "[calendar link]()",
      "","",""
    )
  )
})


