

cat("=====================")
cat("test gdt links functions")
cat("")


test_that("get_*_title functions extract titles correctly", {
  settings <- list(
    ProjectGoalDivider = ":",
    ProjectDeliverableDivider = ":",
    ProjectTaskDivider = ":"
  )

  expect_equal(get_goal_title("# GOAL: Write grant", settings), "Write grant")
  expect_equal(get_deliverable_title("## DELIVERABLE: Run test", settings), "Run test")
  expect_equal(get_task_title("### TASK: Preprocess", settings), "Preprocess")
})

test_that("compute_*_link functions return correctly formatted links", {
  settings <- list(
    ProjectGoalHeader = "# GOAL:",
    ProjectGoalTitle = "GOAL",
    ProjectDeliverableHeader = "## DELIVERABLE:",
    ProjectDeliverableTitle = "DELIVERABLE",
    ProjectTaskHeader = "### TASK:",
    ProjectTaskTitle = "TASK",
    NoteGoalLinkLine = "* [GOAL",
    NoteDeliverableLinkLine = "    + [DELIVERABLE",
    NoteTaskLinkLine = "        - [TASK"
  )

  expect_match(
    compute_goal_link("# GOAL: Write grant", "doc.Rmd", settings),
    "\\* \\[GOAL: Write grant\\]\\(doc\\.Rmd#goal-write-grant\\)"
  )

  expect_match(
    compute_deliverable_link("## DELIVERABLE: Pilot", "doc.Rmd", settings),
    "    \\+ \\[DELIVERABLE: Pilot\\]\\(doc\\.Rmd#deliverable-pilot\\)"
  )

  expect_match(
    compute_task_link("### TASK: Segment images", "doc.Rmd", settings),
    "        - \\[TASK: Segment images\\]\\(doc\\.Rmd#task-segment-images\\)"
  )
})


test_that("get_*_title_from_link functions reconstruct headers correctly", {
  settings <- list(
    ProjectGoalHeader = "# GOAL:",
    ProjectGoalTitle = "GOAL",
    ProjectDeliverableHeader = "## DELIVERABLE:",
    ProjectDeliverableTitle = "DELIVERABLE",
    ProjectTaskHeader = "### TASK:",
    ProjectTaskTitle = "TASK"
  )

  expect_equal(get_goal_title_from_link("+ [GOAL: Analyse](dummy)", settings), "# GOAL: Analyse")
  expect_equal(get_deliverable_title_from_link("+ [DELIVERABLE: QA](dummy)", settings), "## DELIVERABLE: QA")
  expect_equal(get_task_title_from_link("+ [TASK: Clean data](dummy)", settings), "### TASK: Clean data")
})


test_that("get_doc_task_title functions produce expected output", {
  settings <- list(
    NoteSummaryTitle = "## ",
    ProjectTaskDivider = ":"  # required by get_task_title
  )

  expect_equal(
    get_doc_task_title("DOC1", "### TASK: Segment", settings),
    "## DOC1 : Segment"
  )

  expect_equal(
    get_doc_task_title_name("DOC2", "Align images", settings),
    "## DOC2 : Align images"
  )
})


test_that("compute_doc_GDT_link returns expected named list using real get_prefix()", {
  settings <- list(
    ProjectLinkFormat = "+ ",
    NoteGoalLinkLine = "+ [GOAL:",
    NoteDeliverableLinkLine = "+ [DELIVERABLE:",
    NoteTaskLinkLine = "+ [TASK:",
    ProjectGoalTitle = "GOAL",
    ProjectDeliverableTitle = "DELIVERABLE",
    ProjectTaskTitle = "TASK",
    ProjectGoalHeader = "# GOAL:",
    ProjectGoalDivider = ":",
    ProjectDeliverableHeader = "## DELIVERABLE:",
    ProjectDeliverableDivider = ":",
    ProjectTaskHeader = "### TASK:",
    ProjectTaskDivider = ":",
    NoteSummaryTitle = "## ",
    ProjectPrefixSep = "__"
  )

  project_doc_path <- "docs/PrDoSu__Main_Project.Rmd"

  result <- compute_doc_GDT_link(
    projectDocPath = project_doc_path,
    projNoteRmdPath = "notes/Note1.Rmd",
    settings = settings,
    goal = "# GOAL: Analyse",
    deliverable = "## DELIVERABLE: Results",
    task = "### TASK: Plot graphs"
  )

  expect_named(result, c("title", "link", "goal", "del", "task"))

  expect_match(result$title, "PrDoSu : Plot graphs")
  expect_match(result$link, "\\[PrDoSu__Main_Project\\]\\(.*PrDoSu__Main_Project\\.Rmd\\)")
  expect_match(result$goal, "\\[GOAL: Analyse\\]\\(\\.\\./docs/PrDoSu__Main_Project\\.Rmd#goal-analyse\\)")
  expect_match(result$del, "\\[DELIVERABLE: Results\\]\\(\\.\\./docs/PrDoSu__Main_Project\\.Rmd#deliverable-results\\)")
  expect_match(result$task, "\\[TASK: Plot graphs\\]\\(\\.\\./docs/PrDoSu__Main_Project\\.Rmd#task-plot-graphs\\)")
})





test_that("trim hash functions correctly remove markdown header hashes", {
  settings <- list(
    ProjectGoalHeader = "# GOAL:",
    ProjectGoalTitle = "GOAL",
    ProjectGoalDivider = ":",
    ProjectDeliverableHeader = "## DELIVERABLE:",
    ProjectDeliverableTitle = "DELIVERABLE",
    ProjectDeliverableDivider = ":",
    ProjectTaskHeader = "### TASK:",
    ProjectTaskTitle = "TASK",
    ProjectTaskDivider = ":"
  )

  expect_equal(trim_goal_hash("# GOAL: Write grant", settings), "GOAL: Write grant")
  expect_equal(trim_deliverable_hash("## DELIVERABLE: Pilot experiment", settings), "DELIVERABLE: Pilot experiment")
  expect_equal(trim_task_hash("### TASK: Image processing", settings), "TASK: Image processing")
})


