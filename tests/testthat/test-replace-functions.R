
cat("\n\n=====================\n")
cat("test replace functions\n\n")



test_that("replaces exact text block without link adjustment", {
  old_block <- "a\nb\nc"
  new_block <- "x\ny\nz"

  tmp_file <- tempfile(fileext = ".Rmd")
  writeLines(c("intro", "a", "b", "c", "conclusion"), tmp_file)

  result <- replace_rmd_text_block(
    file = tmp_file,
    oldCharVector = old_block,
    newCharVector = new_block,
    source_file = tmp_file
  )

  expect_true(result$changed)
  expect_true(any(grepl("x", result$new_text)))
  expect_false(any(grepl("a", result$new_text)))
})

test_that("skips replacement if old block is not present", {
  old_block <- "this\nwill\nnot\nmatch"
  new_block <- "should\nnot\napply"

  tmp_file <- tempfile(fileext = ".Rmd")
  writeLines(c("foo", "bar", "baz"), tmp_file)

  result <- replace_rmd_text_block(
    file = tmp_file,
    oldCharVector = old_block,
    newCharVector = new_block,
    source_file = tmp_file
  )

  expect_false(result$changed)
  expect_equal(readLines(tmp_file), result$new_text)
})

test_that("adjusts relative markdown links correctly", {
  # Setup: Source file at /tmp/a/source.Rmd
  #        Target file at /tmp/a/b/target.Rmd
  tmp_dir <- tempfile()
  dir.create(file.path(tmp_dir, "a", "b"), recursive = TRUE)
  source_file <- file.path(tmp_dir, "a", "source.Rmd")
  target_file <- file.path(tmp_dir, "a", "b", "target.Rmd")
  link_target <- file.path(tmp_dir, "a", "some_doc.Rmd")
  writeLines("dummy", source_file)
  writeLines("x\ny\nz", target_file)
  writeLines("content", link_target)

  old_block <- "x\ny\nz"
  new_block <- paste0("link to doc: [doc](some_doc.Rmd)")

  result <- replace_rmd_text_block(
    file = target_file,
    oldCharVector = old_block,
    newCharVector = new_block,
    source_file = source_file
  )

  expect_true(result$changed)
  expect_true(any(grepl("\\]\\(\\.\\./some_doc\\.Rmd\\)", result$new_text)))
})


test_that("preserves anchors in adjusted links", {
  # Use tempdir + unique folder name to avoid tempfile() bug
  tmp_dir <- file.path(tempdir(), paste0("test-anch-link", as.integer(Sys.time())))
  dir.create(file.path(tmp_dir, "source"), recursive = TRUE)
  dir.create(file.path(tmp_dir, "target"), recursive = TRUE)

  source_file <- file.path(tmp_dir, "source", "source.Rmd")
  target_file <- file.path(tmp_dir, "target", "target.Rmd")
  writeLines("dummy", source_file)
  writeLines("a\nb\nc", target_file)

  old_block <- "a\nb\nc"
  new_block <- "[title](some_doc.Rmd#section-header)"

  result <- replace_rmd_text_block(
    file = target_file,
    oldCharVector = old_block,
    newCharVector = new_block,
    source_file = source_file
  )

  expect_true(result$changed)
  expect_true(any(grepl("\\.Rmd#section-header\\)", result$new_text)))
})



test_that("replace_in_rmd_files modifies files in-place", {
  tmp_dir <- file.path(tempdir(), paste0("test-rep-in-place", as.integer(Sys.time())))
  dir.create(tmp_dir, recursive = TRUE)

  rmd_file <- file.path(tmp_dir, "doc.Rmd")
  writeLines(c("Header", "x", "y", "z", "Footer"), rmd_file)

  old_block <- "x\ny\nz"
  new_block <- "a\nb\nc"

  # Mock RStudio context functions
  local_mocked_bindings(
    .is_rstudio_available = function() TRUE,
    get_context_path = function() file.path(tmp_dir, "source.Rmd"),
    .package = "projectmanagr"
  )

  writeLines("dummy source", get_context_path())  # Create source file

  # Run replacement
  replace_in_rmd_files(
    directory = tmp_dir,
    oldCharVector = old_block,
    newCharVector = new_block,
    dry_run = FALSE
  )

  result <- readLines(rmd_file)
  expect_true(any(grepl("a", result)))
  expect_false(any(grepl("x", result)))
})


test_that("replace_in_rmd_files reports dry run without modifying", {
  tmp_dir <- file.path(tempdir(), paste0("test-dryrun-", as.integer(Sys.time())))
  dir.create(tmp_dir, recursive = TRUE)

  rmd_file <- file.path(tmp_dir, "dryrun.Rmd")
  old_block <- "1\n2\n3"
  new_block <- "a\nb\nc"

  original_lines <- c("start", "1", "2", "3", "end")
  writeLines(original_lines, rmd_file)

  local_mocked_bindings(
    .is_rstudio_available = function() TRUE,
    get_context_path = function() file.path(tmp_dir, "dry-source.Rmd"),
    .package = "projectmanagr"
  )

  writeLines("placeholder", get_context_path())

  # Capture message (dry run preview message only)
  expect_message(
    replace_in_rmd_files(
      directory = tmp_dir,
      oldCharVector = old_block,
      newCharVector = new_block,
      dry_run = TRUE
    ),
    regexp = "\\[Dry Run\\] Would update"
  )

  # ✅ Check full file content unchanged
  result <- readLines(rmd_file)
  expect_equal(result, original_lines)
})


test_that("replace_in_rmd_files exits with error if source_file is missing and no RStudio context", {
  tmp_dir <- file.path(tempdir(), paste0("test-rep-err", as.integer(Sys.time())))
  dir.create(tmp_dir, recursive = TRUE)

  rmd_file <- file.path(tmp_dir, "test.Rmd")
  writeLines("unchanged text", rmd_file)

  local_mocked_bindings(
    .is_rstudio_available = function() FALSE,
    .package = "projectmanagr"
  )

  expect_error(
    replace_in_rmd_files(
      directory = tmp_dir,
      oldCharVector = "something",
      newCharVector = "else",
      source_file = NULL
    ),
    regexp = "RStudio API is not available"
  )
})


#### ____ ####




test_that("wraps long unordered bullet lines correctly", {
  input <- c("* This is a very long bullet point that will need to be wrapped because it exceeds 80 characters.")
  result <- wrap_rmd_lines(input)

  expect_gt(length(result), 1)
  expect_true(startsWith(result[1], "* "))
  expect_true(grepl("^\\s{2,}", result[2]))
})

test_that("wraps long indented sub-bullets with correct hanging indent", {
  input <- c("    + This sub-bullet has a long line of text that should wrap with proper indentation.")
  result <- wrap_rmd_lines(input)

  expect_gt(length(result), 1)
  expect_true(startsWith(result[1], "    + "))
  expect_true(startsWith(result[2], "      "))  # Hanging indent
})

test_that("wraps numbered list items with correct indent", {
  input <- c("  1. This is a long numbered list item that will be wrapped across multiple lines properly.")
  result <- wrap_rmd_lines(input)

  expect_gt(length(result), 1)
  expect_true(startsWith(result[1], "  1."))
  expect_true(startsWith(result[2], "     "))  # Hanging indent
})


test_that("skips lines with multiple consecutive spaces", {
  input <- c("This  line   has   lots   of   spaces.")
  result <- wrap_rmd_lines(input)

  expect_identical(result, input)
})

test_that("skips ASCII separator lines", {
  input <- c("+=============================+")
  result <- wrap_rmd_lines(input)

  expect_identical(result, input)
})

test_that("preserves long markdown links as a single word", {
  input <- c("* [link](https://example.com/some/very/long/path/that/exceeds/eighty/characters)")
  result <- wrap_rmd_lines(input)

  expect_identical(result, input)
})

test_that("preserves blank lines", {
  input <- c("* bullet line", "", "* another bullet")
  result <- wrap_rmd_lines(input)

  expect_equal(result[2], "")
})




test_that("wrap_active_rmd_document wraps and updates document contents", {
  # Prepare mocked data
  mock_lines <- c("* This is a very long bullet point that should be wrapped because it exceeds the 80 character limit.")

  captured <- new.env()
  captured$text <- NULL
  captured$id <- NULL

  # Define fake wrapper functions
  fake_is_rstudio_available <- function() TRUE
  fake_get_context_contents <- function() mock_lines
  fake_get_context_id <- function() "mock-id"
  fake_set_document_contents <- function(text, id) {
    captured$text <- text
    captured$id <- id
  }

  # Mock all wrapper functions used in wrap_active_rmd_document
  local_mocked_bindings(
    .is_rstudio_available = fake_is_rstudio_available,
    get_context_contents = fake_get_context_contents,
    get_context_id = fake_get_context_id,
    .set_document_contents = fake_set_document_contents,
    .package = "projectmanagr"
  )

  result <- wrap_active_rmd_document()

  # Tests
  expect_true(result)
  expect_type(captured$text, "character")
  expect_true(grepl("\n", captured$text))  # Should have wrapped into multiple lines
  expect_equal(captured$id, "mock-id")
})

test_that("wrap_active_rmd_document returns FALSE if RStudio is not available", {
  local_mocked_bindings(
    .is_rstudio_available = function() FALSE,
    .package = "projectmanagr"
  )

  expect_false(wrap_active_rmd_document())
})

test_that("wrap_active_rmd_document returns FALSE if document is empty", {
  local_mocked_bindings(
    .is_rstudio_available = function() TRUE,
    get_context_contents = function() character(0),
    .package = "projectmanagr"
  )

  expect_false(wrap_active_rmd_document())
})



