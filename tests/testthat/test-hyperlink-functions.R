
cat("=====================")
cat("test hyperlink functions")
cat("")


test_that("split_path_header splits correctly when # present", {
  result <- split_path_header("file.Rmd#section")
  expect_equal(result$path, "file.Rmd")
  expect_equal(result$header, "#section")
})

test_that("split_path_header returns full path with empty header when # absent", {
  result <- split_path_header("file.Rmd")
  expect_equal(result$path, "file.Rmd")
  expect_equal(result$header, "")
})


test_that("create_hyperlink generates correct relative link", {
  # simulate directory structure:
  to <- "notes/project-note.Rmd"
  from <- "docs/index.Rmd"

  result <- create_hyperlink("project-note", to, from)
  expect_equal(result, "[project-note](../notes/project-note.Rmd)")
})



test_that("create_hyperlink_no_ext strips extension and creates correct link", {
  toFilePath <- "notes/note-001.Rmd"
  fromFilePath <- "docs/index.Rmd"

  result <- create_hyperlink_no_ext(toFilePath, fromFilePath)

  expect_equal(result, "[note-001](../notes/note-001.Rmd)")
})


test_that("create_hyperlink_section adds sanitized section anchor", {
  toFile <- "notes/myDoc.Rmd"
  fromFile <- "docs/index.Rmd"
  header <- "My Special--Section!"

  result <- create_hyperlink_section("myDoc.Rmd", header, toFile, fromFile)
  expect_equal(result, "[myDoc.Rmd : My Special--Section!](../notes/myDoc.Rmd#my-special-section)")
})




test_that("link_add_section appends sanitized anchor", {
  result <- link_add_section("notes/doc.Rmd", "Header with *! chars")
  expect_equal(result, "notes/doc.Rmd#header-with-chars")
})


test_that("sanitize_header_for_anchor creates pandoc-style anchor", {
  expect_equal(sanitize_header_for_anchor("Hello World!"), "hello-world")
  expect_equal(sanitize_header_for_anchor("___This_ is a Test___"), "this-is-a-test")
  expect_equal(sanitize_header_for_anchor("Trim--Dashes--"), "trim-dashes")
})


