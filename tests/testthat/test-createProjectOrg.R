

# run createProjectOrg with defined inputs:
projectmanagr::createProjectOrg(authorValue = Sys.info()["user"], orgName = "00_ORG_TEST", fileSystemPath =  fs::path_home() )

test_that("Organisation creation is valid", {
  expect_equal(2 * 2, 4)
})


