
test_that("datatable_create generates plaintext datatable", {

  # test default dt generation
  IDs=""
  data_cols=""
  datatable_name = "samples"
  default_data_vals=list()
  dt_length = 100
  expect_snapshot(cat(datatable_create(IDs, data_cols, datatable_name,
                                       default_data_vals, dt_length), sep='\n'))

  # test dt generation with IDs, cols with default - cover all value lengths!
  IDs=c(1001, 1002, 1003, 1004)
  data_cols=c("x","wt-g", "perfuse_dt", "perfusion_condition")
  datatable_name = "samples"
  default_data_vals=list()
  dt_length = 100
  expect_snapshot(cat(datatable_create(IDs, data_cols, datatable_name,
                                       default_data_vals, dt_length), sep='\n'))

  # test dt generation with IDs, cols and vals - cover all value lengths!
  IDs=c(1001, 1002, 1003, 1004)
  data_cols=c("x","wt-g", "perfuse_dt", "perfusion_condition")
  datatable_name = "samples"
  default_data_vals=list(c("F","F","M","M"),
                         c("30.0", "31.1", "32.2", "33.3"),
                         c("2020-01-01:12:01", "2020-01-01:12:02",
                           "2020-01-01:12:03", "2020-01-01:12:04"),
                         c("F4M1PB_RT", "F4M2PB_RT", "F4M3PB_RT", "F4M4PB_RT"))

  dt_length = 100
  expect_snapshot(cat(datatable_create(IDs, data_cols, datatable_name,
                                       default_data_vals, dt_length), sep='\n'))


  # test duplicated cols throw an error
  IDs=c(1001, 1002, 1003, 1004)
  data_cols=c("x","wt-g", "perfuse_dt", "perfuse_dt")
  datatable_name = "samples"
  default_data_vals=list()
  dt_length = 100
  expect_error(datatable_create(IDs, data_cols, datatable_name,
                                default_data_vals, dt_length))

})
