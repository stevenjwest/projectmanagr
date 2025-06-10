
cat("=====================")
cat("test template utils functions")
cat("")


test_that("insert_at_indices behaves correctly across a range of inputs", {
  x <- c("a", "b", "c", "d")

  # Single index, single-line insert
  expect_equal(
    insert_at_indices(x, indices = 2, replacementVector = "X"),
    c("a", "X", "b", "c", "d")
  )

  # Single index, multi-line insert
  expect_equal(
    insert_at_indices(x, indices = 2, replacementVector = c("X", "Y")),
    c("a", "X", "Y", "b", "c", "d")
  )

  # Multiple indices, single-line insert
  expect_equal(
    insert_at_indices(x, indices = c(2, 4), replacementVector = "X"),
    c("a", "X", "b", "c", "X", "d")
  )

  # Multiple indices, multi-line insert
  expect_equal(
    insert_at_indices(x, indices = c(2, 4), replacementVector = c("X", "Y")),
    c("a", "X", "Y", "b", "c", "X", "Y", "d")
  )

  # Insert at beginning
  expect_equal(
    insert_at_indices(x, indices = 1, replacementVector = "X"),
    c("X", "a", "b", "c", "d")
  )

  # Insert at end (index == length + 1)
  expect_equal(
    insert_at_indices(x, indices = 5, replacementVector = "X"),
    c("a", "b", "c", "d", "X")
  )

  # Multi-line insert at beginning and end
  expect_equal(
    insert_at_indices(x, indices = 1, replacementVector = c("intro1", "intro2")),
    c("intro1", "intro2", "a", "b", "c", "d")
  )
  expect_equal(
    insert_at_indices(x, indices = 5, replacementVector = c("outro1", "outro2")),
    c("a", "b", "c", "d", "outro1", "outro2")
  )

  # Error: index = 0
  expect_error(
    insert_at_indices(x, indices = 0, replacementVector = "X"),
    "All indices must be between"
  )

  # Error: index beyond length + 1
  expect_error(
    insert_at_indices(x, indices = 6, replacementVector = "X"),
    "All indices must be between"
  )

  # Error: negative index
  expect_error(
    insert_at_indices(x, indices = -1, replacementVector = "X"),
    "All indices must be between"
  )

  # Error: mixed valid and invalid indices
  expect_error(
    insert_at_indices(x, indices = c(2, 6), replacementVector = "X"),
    "All indices must be between"
  )
})



test_that("replace_at_indices behaves correctly across a range of inputs", {
  x <- c("a", "b", "c", "d")

  # Replace a single element with a single value
  expect_equal(
    replace_at_indices(x, indices = 2, replacementVector = "X"),
    c("a", "X", "c", "d")
  )

  # Replace a single element with multiple lines
  expect_equal(
    replace_at_indices(x, indices = 2, replacementVector = c("X", "Y")),
    c("a", "X", "Y", "c", "d")
  )

  # Replace multiple elements with a single-line vector
  expect_equal(
    replace_at_indices(x, indices = c(2, 4), replacementVector = "X"),
    c("a", "X", "c", "X")
  )

  # Replace multiple elements with a multi-line vector
  expect_equal(
    replace_at_indices(x, indices = c(2, 4), replacementVector = c("X", "Y")),
    c("a", "X", "Y", "c", "X", "Y")
  )

  # Replace first element
  expect_equal(
    replace_at_indices(x, indices = 1, replacementVector = "X"),
    c("X", "b", "c", "d")
  )

  # Replace last element
  expect_equal(
    replace_at_indices(x, indices = 4, replacementVector = "X"),
    c("a", "b", "c", "X")
  )

  # Replace first element with multi-line
  expect_equal(
    replace_at_indices(x, indices = 1, replacementVector = c("intro1", "intro2")),
    c("intro1", "intro2", "b", "c", "d")
  )

  # Replace last element with multi-line
  expect_equal(
    replace_at_indices(x, indices = 4, replacementVector = c("outro1", "outro2")),
    c("a", "b", "c", "outro1", "outro2")
  )

  # Invalid index: 0
  expect_error(
    replace_at_indices(x, indices = 0, replacementVector = "X"),
    "All indices must be between"
  )

  # Invalid index: too large
  expect_error(
    replace_at_indices(x, indices = 5, replacementVector = "X"),
    "All indices must be between"
  )

  # Invalid index: negative
  expect_error(
    replace_at_indices(x, indices = -1, replacementVector = "X"),
    "All indices must be between"
  )

  # Mixed valid and invalid index
  expect_error(
    replace_at_indices(x, indices = c(2, 5), replacementVector = "X"),
    "All indices must be between"
  )
})



test_that("replace_knitr_include_graphics_link replaces paths correctly", {
  tmpdir <- create_tmpdir_rsess()

  fs::dir_create(fs::path(tmpdir, "srcdir/images"))
  fs::dir_create(fs::path(tmpdir, "destdir/subfolder"))
  fs::file_create(fs::path(tmpdir, "srcdir/images/figure1.png"))
  fs::file_create(fs::path(tmpdir, "srcdir/images/figure two.png"))

  source_file <- fs::path(tmpdir, "srcdir/note.Rmd")
  destination_file <- fs::path(tmpdir, "destdir/subfolder/note-copy.Rmd")

  # Simulate content
  input_lines <- c(
    "Some text before.",
    "```{r}",
    "plot(1:10)",
    "knitr::include_graphics('images/figure1.png')",
    "```",
    "",
    "```{r another}",
    "knitr::include_graphics( \"images/figure two.png\" )",
    "```",
    "This line doesn't contain any include_graphics call."
  )

  result <- replace_knitr_include_graphics_link(
    contentContents = input_lines,
    sourceFilePath = source_file,
    destinationFilePath = destination_file
  )

  # Compute expected relative paths manually
  abs1 <- fs::path_abs(fs::path(tmpdir, "srcdir/images/figure1.png"))
  abs2 <- fs::path_abs(fs::path(tmpdir, "srcdir/images/figure two.png"))
  rel1 <- fs::path_rel(abs1, start = fs::path(tmpdir, "destdir/subfolder"))
  rel2 <- fs::path_rel(abs2, start = fs::path(tmpdir, "destdir/subfolder"))

  expect_equal(
    result[4],
    paste0("knitr::include_graphics('", rel1, "')")
  )

  expect_equal(
    result[8],
    paste0("knitr::include_graphics( \"", rel2, "\" )")
  )

  # Unrelated lines should remain unchanged
  expect_equal(result[1], input_lines[1])
  expect_equal(result[2], input_lines[2])
  expect_equal(result[10], input_lines[10])
})


test_that("replace_knitr_include_graphics_link skips malformed calls", {
  input_lines <- c(
    "knitr::include_graphics(images/figure1.png)",  # no quotes
    "knitr::include_graphics('images/figure1.png)"  # missing closing quote
  )

  result <- replace_knitr_include_graphics_link(
    contentContents = input_lines,
    sourceFilePath = "srcdir/note.Rmd",
    destinationFilePath = "destdir/note-copy.Rmd"
  )

  # Malformed lines should be returned unchanged
  expect_equal(result, input_lines)
})



test_that("replace_hyper_links replaces single file links", {
  tmpdir <- create_tmpdir_rsess()

  fs::dir_create(fs::path(tmpdir, "srcdir"))
  fs::dir_create(fs::path(tmpdir, "destdir"))
  fs::file_create(fs::path(tmpdir, "srcdir", "note1.Rmd"))
  Sys.sleep(0.1)

  source_file <- fs::path(tmpdir, "srcdir", "main.Rmd")
  destination_file <- fs::path(tmpdir, "destdir", "main-copy.Rmd")

  input <- c("See the [Note](note1.Rmd) for details.")

  result <- replace_hyper_links(input, source_file, destination_file)

  abs <- fs::path_abs(fs::path(tmpdir, "srcdir", "note1.Rmd"))
  rel <- fs::path_rel(abs, start = fs::path(tmpdir, "destdir"))

  expect_equal(result, paste0("See the [Note](", rel, ") for details."))
})

test_that("replace_hyper_links preserves header anchors", {
  tmpdir <- create_tmpdir_rsess()

  fs::dir_create(fs::path(tmpdir, "srcdir"))
  fs::dir_create(fs::path(tmpdir, "destdir"))
  fs::file_create(fs::path(tmpdir, "srcdir", "note2.Rmd"))
  Sys.sleep(0.1)

  source_file <- fs::path(tmpdir, "srcdir", "main.Rmd")
  destination_file <- fs::path(tmpdir, "destdir", "main-copy.Rmd")

  input <- c("Refer to the [Section](note2.Rmd#header-info).")

  result <- replace_hyper_links(input, source_file, destination_file)

  abs <- fs::path_abs(fs::path(tmpdir, "srcdir", "note2.Rmd"))
  rel <- fs::path_rel(abs, start = fs::path(tmpdir, "destdir"))

  expect_equal(result, paste0("Refer to the [Section](", rel, "#header-info)."))
})

test_that("replace_hyper_links skips internal-only header links", {
  input <- c("Jump to [this section](#methods) in the same file.")

  result <- replace_hyper_links(
    input,
    sourceFilePath = "srcdir/file.Rmd",
    destinationFilePath = "destdir/file-copy.Rmd"
  )

  expect_equal(result, input)
})

test_that("replace_hyper_links handles multiple links per line", {
  tmpdir <- create_tmpdir_rsess()

  fs::dir_create(fs::path(tmpdir, "srcdir"))
  fs::dir_create(fs::path(tmpdir, "destdir"))
  fs::file_create(fs::path(tmpdir, "srcdir", "a.Rmd"))
  fs::file_create(fs::path(tmpdir, "srcdir", "b.Rmd"))
  Sys.sleep(0.1)

  source_file <- fs::path(tmpdir, "srcdir", "master.Rmd")
  destination_file <- fs::path(tmpdir, "destdir", "master-copy.Rmd")

  input <- c("See [A](a.Rmd) and also [B](b.Rmd) for references.")

  result <- replace_hyper_links(input, source_file, destination_file)

  a_rel <- fs::path_rel(fs::path(tmpdir, "srcdir", "a.Rmd"), start = fs::path(tmpdir, "destdir"))
  b_rel <- fs::path_rel(fs::path(tmpdir, "srcdir", "b.Rmd"), start = fs::path(tmpdir, "destdir"))

  expected <- paste0("See [A](", a_rel, ") and also [B](", b_rel, ") for references.")

  expect_equal(result, expected)
})

test_that("replace_hyper_links skips malformed or broken links", {
  input <- c(
    "Broken link [MissingParen](missing.Rmd",
    "Empty link [](file.Rmd)",
    "Garbage line"
  )

  result <- replace_hyper_links(
    contentContents = input,
    sourceFilePath = "srcdir/source.Rmd",
    destinationFilePath = "destdir/dest.Rmd"
  )

  expect_equal(result, input)
})

test_that("replace_hyper_links skips links to non-existent files", {
  input <- c("Link to [Ghost](ghost.Rmd) which doesn't exist.")

  result <- replace_hyper_links(
    contentContents = input,
    sourceFilePath = "srcdir/source.Rmd",
    destinationFilePath = "destdir/dest.Rmd"
  )

  expect_equal(result, input)
})

test_that("replace_hyper_links leaves untouched lines as-is", {
  input <- c(
    "No links here.",
    "Another plain line."
  )

  result <- replace_hyper_links(
    input,
    sourceFilePath = "srcdir/source.Rmd",
    destinationFilePath = "destdir/dest.Rmd"
  )

  expect_equal(result, input)
})


