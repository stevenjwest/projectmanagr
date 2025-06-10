
cat("=====================")
cat("test datatables gen functions")
cat("")


test_that("datatable_create()", {

  # test default dt generation
  IDs=""
  data_cols=""
  datatable_name = "samples"
  default_data_vals=list()
  dt_length = 100
  expand=FALSE
  expect_snapshot(cat(datatable_create(IDs, data_cols, datatable_name,
                                       default_data_vals, dt_length, expand), sep='\n'))

  # test dt generation with IDs, cols with default - cover all value lengths!
  IDs=c(1001, 1002, 1003, 1004)
  data_cols=c("c", "cage", "genotype", "strain_breed_type", "dob_dt")# range of data col lengths
  datatable_name = "samples"
  default_data_vals=list()
  dt_length = 100
  expand=FALSE
  expect_snapshot(cat(datatable_create(IDs, data_cols, datatable_name,
                                       default_data_vals, dt_length, expand), sep='\n'))

  # test dt generation with IDs, cols and vals - cover all value lengths!
  IDs=c(1001, 1002, 1003, 1004)
  data_cols=c("c", "cage", "genotype", "strain_breed_type", "dob_dt")# range of data col lengths
  datatable_name = "samples"
  default_data_vals=list(c("F", "F", "M", "M"),
                         c("CID101", "CID102", "CID103", "CID104"),
                         c("vgat:wt", "vgat:wt", "vgat:wt", "vgat:wt"),
                         c("c57bl1", "c57bl2", "c57bl3", "c57bl4"),
                         c("2024-08-21:12:11", "2024-08-21:12:12",
                           "2024-08-21:12:13", "2024-08-21:12:14"))
  dt_length = 100
  expand=FALSE
  expect_snapshot(cat(datatable_create(IDs, data_cols, datatable_name,
                                       default_data_vals, dt_length, expand), sep='\n'))

  # test adding many cols cols - to generate two data tables for data
  IDs=c(1001, 1002, 1003, 1004)
  data_cols=c("x","wt-g", "perfuse_dt", "perfusion_con", "group-fix",
              "postfix_dt", "postfix_con", "group-postfix")
  datatable_name = "samples"
  default_data_vals=list()
  dt_length = 100
  expand=FALSE
  expect_snapshot(cat(datatable_create(IDs, data_cols, datatable_name,
                                       default_data_vals, dt_length, expand), sep='\n'))

  # test expand is TRUE correctly expands default_data_vals vectors to length of IDs
  data_cols=c("c", "cage", "genotype", "strain_breed_type", "dob_dt")# range of data col lengths
  default_data_vals=list(c("M"),
                         c("CID101"),
                         c("vgat:wt"),
                         c("c57bl6"),
                         c("2024-08-21:12:17"))
  expand=TRUE
  expect_snapshot(cat(datatable_create(IDs, data_cols, datatable_name,
                                       default_data_vals, dt_length, expand), sep='\n'))


  ### ERRORS

  # test duplicated cols throw an error
  IDs=c(1001, 1002, 1003, 1004)
  data_cols=c("x","wt-g", "perfuse_dt", "perfuse_dt")
  datatable_name = "samples"
  default_data_vals=list()
  dt_length = 100
  expand=FALSE
  expect_error(datatable_create(IDs, data_cols, datatable_name,
                                default_data_vals, dt_length))

  # test default_data_vals length != data_cols length throw an error
  IDs=c(1001, 1002, 1003, 1004)
  data_cols=c("x","wt-g", "perfuse_dt") # 3
  datatable_name = "samples"
  default_data_vals=list(c("F","F","M","M"),
                         c("30.0", "31.1", "32.2", "33.3"),
                         c("2020-01-01:12:01", "2020-01-01:12:02",
                           "2020-01-01:12:03", "2020-01-01:12:04"),
                         c("F4M1PB_RT", "F4M2PB_RT", "F4M3PB_RT", "F4M4PB_RT")) # 4

  dt_length = 100
  expand=FALSE
  expect_error(datatable_create(IDs, data_cols, datatable_name,
                                default_data_vals, dt_length, expand))

  # test default_data_vals vectors length != IDs length throws an error
  # WHEN EXPAND IS FALSE
  default_data_vals=list(c("M"),
                         c("CID101"),
                         c("vgat:wt"),
                         c("c57bl6"),
                         c("2024-08-21:12:17"))
  expand=FALSE
  expect_error(datatable_create(IDs, data_cols, datatable_name,
                                default_data_vals, dt_length, expand))

})



test_that("datatable_add_data_samples()", {

  # test default ADD_DATA dt generation - with all possible value lengths
  # ids_vector is default blank string
  contents <- c("",
                "+===============================================================================",
                "",
                "",
                "    samples  :  CREATE",
                "",
                "",
                "      ID      x      wt-g         perfuse_dt         perfusion_con      group-fix    ",
                "    ======  =====  ========  ====================  =================  =============  ",
                "",
                "     1001     F      30.0      2020-01-01:12:01        F4M1PB_RT          fix_RT     ",
                "                                                                                     ",
                "     1002     F      31.1      2020-01-01:12:02        F4M2PB_RT          fix_RT     ",
                "                                                                                     ",
                "     1003     M      32.2      2020-01-01:12:03        F4M3PB_4C          fix_4C     ",
                "                                                                                     ",
                "     1004     M      33.3      2020-01-01:12:04        F4M4PB_4C          fix_4C     ",
                "                                                                                     ",
                "",
                "+===============================================================================",
                "")
  # testing all lengths of data cols
  data_cols=c("perfuse_wash_dt", "perfuse_wash_con", "p", "perf")
  datatable_name = "samples"
  ids_vector=""
  default_data_vals=list()
  dt_length = 100
  summarise_reps = FALSE
  all_reps = FALSE
  expect_snapshot(cat(datatable_add_data_samples(contents, data_cols, datatable_name,
                                                 ids_vector, default_data_vals,
                                                 dt_length, summarise_reps, all_reps),
                      sep='\n'))

  # test with default_data_vals set
  default_data_vals=list(
    c("2024-08-19:14:12"),
    c("PBS_RT"),
    c("PBS"),
    c("RT")
  )
  expect_snapshot(cat(datatable_add_data_samples(contents, data_cols, datatable_name,
                                                 ids_vector, default_data_vals,
                                                 dt_length, summarise_reps, all_reps),
                      sep='\n'))

  # test with default_data_vals set as multi vals
  default_data_vals=list(
    c("2024-08-19:14:12", "2024-08-19:14:12", "2024-08-19:14:12", "2024-08-19:14:12"),
    c("PBS_RT", "PBS_RT", "PBS_RT", "PBS_RT"),
    c("PBS", "PBS", "PBS", "PBS"),
    c("RT", "RT", "RT", "RT")
  )
  expect_snapshot(cat(datatable_add_data_samples(contents, data_cols, datatable_name,
                                                 ids_vector, default_data_vals,
                                                 dt_length, summarise_reps, all_reps),
                      sep='\n'))

  # test ids_vector is "ALL"
  ids_vector="ALL"
  expect_snapshot(cat(datatable_add_data_samples(contents, data_cols, datatable_name,
                                                 ids_vector, default_data_vals,
                                                 dt_length, summarise_reps, all_reps),
                      sep='\n'))

  # test ids_vector is <GROUP-IDS>
  ids_vector="fix_4C"
  expect_snapshot(cat(datatable_add_data_samples(contents, data_cols, datatable_name,
                                                 ids_vector, default_data_vals,
                                                 dt_length, summarise_reps, all_reps),
                      sep='\n'))

  # test ids_vector is <subset of IDs>
  ids_vector=c("1001", "1003")
  expect_snapshot(cat(datatable_add_data_samples(contents, data_cols, datatable_name,
                                                 ids_vector, default_data_vals,
                                                 dt_length, summarise_reps, all_reps),
                      sep='\n'))

  # EXPECT ERROR : test ids_vector is <subset of IDs> - one non-existant ID
  ids_vector=c("1001", "1003", "1005")
  expect_error(datatable_add_data_samples(contents, data_cols, datatable_name,
                                          ids_vector, default_data_vals,
                                          dt_length, summarise_reps, all_reps))

  ### Check ADD DATA to subst of samples to EXISTING COLUMN

  # add data to just two IDs
  contents <- c(contents, "",
                "+===============================================================================",
                "",
                "",
                "    samples  :  ADD_DATA",
                "",
                "",
                "      ID         fix_dt                fix_con       ",
                "    ======  ====================  =================  ",
                "",
                "     1001     2020-01-01:12:01        F4M1PB_RT      ",
                "                                                     ",
                "     1002     2020-01-01:12:02        F4M2PB_RT      ",
                "                                                     ",
                "",
                "+===============================================================================",
                "")
  # now add data to SAME DATA COLS
  data_cols=c("fix_dt", "fix_con")
  ids_vector=""
  default_data_vals=list()
  expect_snapshot(cat(datatable_add_data_samples(contents, data_cols, datatable_name,
                                                 ids_vector, default_data_vals,
                                                 dt_length, summarise_reps, all_reps),
                      sep='\n'))

  default_data_vals=list(c("2024-08-19:1200B"), c("PBS_RT"))
  expect_snapshot(cat(datatable_add_data_samples(contents, data_cols, datatable_name,
                                                 ids_vector, default_data_vals,
                                                 dt_length, summarise_reps, all_reps),
                      sep='\n'))

  ids_vector=c("1001","1003")
  default_data_vals=list()
  expect_snapshot(cat(datatable_add_data_samples(contents, data_cols, datatable_name,
                                                 ids_vector, default_data_vals,
                                                 dt_length, summarise_reps, all_reps),
                      sep='\n'))

  default_data_vals=list(c("2024-08-19:1200B"), c("PBS_RT"))
  expect_snapshot(cat(datatable_add_data_samples(contents, data_cols, datatable_name,
                                                 ids_vector, default_data_vals,
                                                 dt_length, summarise_reps, all_reps),
                      sep='\n'))


  ### check ADD DATA to samples with reps

  # add RESAMPLE datatables to contents:
  contents <- c(contents,
                "",
                "+===============================================================================",
                "",
                "",
                "    samples  :  RESAMPLE",
                "",
                "",
                "      ID       resample      reps    ",
                "    ======  =============  ========  ",
                "",
                "     1001        CNS           1     ",
                "                SC-LUM         1     ",
                "              DRG-L4-LT        1     ",
                "                                     ",
                "     1002        CNS           1     ",
                "                SC-LUM         1     ",
                "              DRG-L4-LT        1     ",
                "                                     ",
                "     1003        CNS           1     ",
                "                SC-LUM         1     ",
                "              DRG-L4-LT        1     ",
                "                                     ",
                "     1004        CNS           1     ",
                "                SC-LUM         1     ",
                "              DRG-L4-LT        1     ",
                "                                     ",
                "",
                "+===============================================================================",
                "",
                "",
                "",
                "+===============================================================================",
                "",
                "",
                "    samples_SC-LUM  :  RESAMPLE",
                "",
                "",
                "      ID      resample      reps    ",
                "    ======  ============  ========  ",
                "",
                "     1001       50µm          8     ",
                "                                    ",
                "     1002       50µm          8     ",
                "                                    ",
                "     1003       50µm          8     ",
                "                                    ",
                "     1004       50µm          8     ",
                "                                    ",
                "",
                "+===============================================================================",
                "",
                "")

  # add data to data col to RESAMPLED SAMPLE IDs
  data_cols=c("perfuse_wash_dt", "perfuse_wash_con", "p", "perf")
  datatable_name = "samples_SC-LUM_50µm"
  ids_vector=""
  default_data_vals=list()
  dt_length = 100
  summarise_reps = FALSE
  all_reps = FALSE
  expect_snapshot(cat(datatable_add_data_samples(contents, data_cols, datatable_name,
                                                 ids_vector, default_data_vals,
                                                 dt_length, summarise_reps, all_reps),
                      sep='\n'))

  # try with summarise_reps TRUE
  summarise_reps = TRUE
  expect_snapshot(cat(datatable_add_data_samples(contents, data_cols, datatable_name,
                                                 ids_vector, default_data_vals,
                                                 dt_length, summarise_reps, all_reps),
                      sep='\n'))

  # try with all_reps TRUE
  all_reps = TRUE
  expect_snapshot(cat(datatable_add_data_samples(contents, data_cols, datatable_name,
                                                 ids_vector, default_data_vals,
                                                 dt_length, summarise_reps, all_reps),
                      sep='\n'))

  # repeat addition of data col to RESAMPLED SAMPLE IDs with default_data_vals set
  default_data_vals=list(
    c("2024-08-19:14:12"),
    c("PBS_RT"),
    c("PBS"),
    c("RT")
  )
  summarise_reps = FALSE
  all_reps = FALSE
  expect_snapshot(cat(datatable_add_data_samples(contents, data_cols, datatable_name,
                                                 ids_vector, default_data_vals,
                                                 dt_length, summarise_reps, all_reps),
                      sep='\n'))

  summarise_reps = TRUE
  expect_snapshot(cat(datatable_add_data_samples(contents, data_cols, datatable_name,
                                                 ids_vector, default_data_vals,
                                                 dt_length, summarise_reps, all_reps),
                      sep='\n'))

  all_reps = TRUE
  expect_snapshot(cat(datatable_add_data_samples(contents, data_cols, datatable_name,
                                                 ids_vector, default_data_vals,
                                                 dt_length, summarise_reps, all_reps),
                      sep='\n'))

  # repeat addition of data col to RESAMPLED SAMPLE IDs with default_data_vals set as multi vals
  default_data_vals=list(
    c("2024-08-19:14:12", "2024-08-19:14:12", "2024-08-19:14:12", "2024-08-19:14:12"),
    c("PBS_RT", "PBS_RT", "PBS_RT", "PBS_RT"),
    c("PBS", "PBS", "PBS", "PBS"),
    c("RT", "RT", "RT", "RT")
  )
  summarise_reps = FALSE
  all_reps = FALSE
  expect_snapshot(cat(datatable_add_data_samples(contents, data_cols, datatable_name,
                                                 ids_vector, default_data_vals,
                                                 dt_length, summarise_reps, all_reps),
                      sep='\n'))

  summarise_reps = TRUE
  expect_snapshot(cat(datatable_add_data_samples(contents, data_cols, datatable_name,
                                                 ids_vector, default_data_vals,
                                                 dt_length, summarise_reps, all_reps),
                      sep='\n'))

  all_reps = TRUE
  expect_snapshot(cat(datatable_add_data_samples(contents, data_cols, datatable_name,
                                                 ids_vector, default_data_vals,
                                                 dt_length, summarise_reps, all_reps),
                      sep='\n'))


  # repeat addition of data col to RESAMPLED SAMPLE IDs with ids_vector ALL
  data_cols=c("perfuse_wash_dt", "perfuse_wash_con", "p", "perf")
  datatable_name = "samples_SC-LUM_50µm"
  ids_vector="ALL"
  default_data_vals=list()
  dt_length = 100
  summarise_reps = FALSE
  all_reps = FALSE
  expect_snapshot(cat(datatable_add_data_samples(contents, data_cols, datatable_name,
                                                 ids_vector, default_data_vals,
                                                 dt_length, summarise_reps, all_reps),
                      sep='\n'))

  summarise_reps = TRUE
  expect_snapshot(cat(datatable_add_data_samples(contents, data_cols, datatable_name,
                                                 ids_vector, default_data_vals,
                                                 dt_length, summarise_reps, all_reps),
                      sep='\n'))

  all_reps = TRUE
  expect_snapshot(cat(datatable_add_data_samples(contents, data_cols, datatable_name,
                                                 ids_vector, default_data_vals,
                                                 dt_length, summarise_reps, all_reps),
                      sep='\n'))

  # repeat with ids_vector as subset
  data_cols=c("perfuse_wash_dt", "perfuse_wash_con", "p", "perf")
  datatable_name = "samples_SC-LUM_50µm"
  ids_vector=c("1001", "1003")
  default_data_vals=list()
  dt_length = 100
  summarise_reps = FALSE
  all_reps = FALSE
  expect_snapshot(cat(datatable_add_data_samples(contents, data_cols, datatable_name,
                                                 ids_vector, default_data_vals,
                                                 dt_length, summarise_reps, all_reps),
                      sep='\n'))

  summarise_reps = TRUE
  expect_snapshot(cat(datatable_add_data_samples(contents, data_cols, datatable_name,
                                                 ids_vector, default_data_vals,
                                                 dt_length, summarise_reps, all_reps),
                      sep='\n'))

  all_reps = TRUE
  expect_snapshot(cat(datatable_add_data_samples(contents, data_cols, datatable_name,
                                                 ids_vector, default_data_vals,
                                                 dt_length, summarise_reps, all_reps),
                      sep='\n'))


  # add data to just two IDs
  contents <- c(contents, "",
                "+===============================================================================",
                "",
                "",
                "    samples_SC-LUM_50µm  :  ADD_DATA",
                "",
                "",
                "      ID      rep        fix_dt                fix_con       ",
                "    ======  =======  ====================  =================  ",
                "",
                "     1001     1:2      2020-01-01:12:01        F4M1PB_RT      ",
                "                                                              ",
                "     1002     1:2      2020-01-01:12:02        F4M2PB_RT      ",
                "                                                              ",
                "",
                "+===============================================================================",
                "")

  data_cols=c("fix_dt", "fix_con")
  datatable_name = "samples_SC-LUM_50µm"
  ids_vector=""
  default_data_vals=list()
  dt_length = 100
  summarise_reps = FALSE
  all_reps = FALSE
  expect_snapshot(cat(datatable_add_data_samples(contents, data_cols, datatable_name,
                                                 ids_vector, default_data_vals,
                                                 dt_length, summarise_reps, all_reps),
                      sep='\n'))

  summarise_reps = TRUE
  expect_snapshot(cat(datatable_add_data_samples(contents, data_cols, datatable_name,
                                                 ids_vector, default_data_vals,
                                                 dt_length, summarise_reps, all_reps),
                      sep='\n'))

  all_reps = TRUE
  expect_snapshot(cat(datatable_add_data_samples(contents, data_cols, datatable_name,
                                                 ids_vector, default_data_vals,
                                                 dt_length, summarise_reps, all_reps),
                      sep='\n'))

  default_data_vals=list(c("2024-08-19:1200B"), c("PBS_RT"))
  summarise_reps = FALSE
  all_reps = FALSE
  expect_snapshot(cat(datatable_add_data_samples(contents, data_cols, datatable_name,
                                                 ids_vector, default_data_vals,
                                                 dt_length, summarise_reps, all_reps),
                      sep='\n'))

  summarise_reps = TRUE
  expect_snapshot(cat(datatable_add_data_samples(contents, data_cols, datatable_name,
                                                 ids_vector, default_data_vals,
                                                 dt_length, summarise_reps, all_reps),
                      sep='\n'))

  all_reps = TRUE
  expect_snapshot(cat(datatable_add_data_samples(contents, data_cols, datatable_name,
                                                 ids_vector, default_data_vals,
                                                 dt_length, summarise_reps, all_reps),
                      sep='\n'))

  ids_vector=c("1001","1003")
  default_data_vals=list()
  summarise_reps = FALSE
  all_reps = FALSE
  expect_snapshot(cat(datatable_add_data_samples(contents, data_cols, datatable_name,
                                                 ids_vector, default_data_vals,
                                                 dt_length, summarise_reps, all_reps),
                      sep='\n'))

  default_data_vals=list(c("2024-08-19:1200B"), c("PBS_RT"))
  expect_snapshot(cat(datatable_add_data_samples(contents, data_cols, datatable_name,
                                                 ids_vector, default_data_vals,
                                                 dt_length, summarise_reps, all_reps),
                      sep='\n'))

  ids_vector=c("1001","1003")
  default_data_vals=list()
  summarise_reps = TRUE
  expect_snapshot(cat(datatable_add_data_samples(contents, data_cols, datatable_name,
                                                 ids_vector, default_data_vals,
                                                 dt_length, summarise_reps, all_reps),
                      sep='\n'))
  all_reps = TRUE
  expect_snapshot(cat(datatable_add_data_samples(contents, data_cols, datatable_name,
                                                 ids_vector, default_data_vals,
                                                 dt_length, summarise_reps, all_reps),
                      sep='\n'))

  default_data_vals=list(c("2024-08-19:1200B"), c("PBS_RT"))
  all_reps = FALSE
  expect_snapshot(cat(datatable_add_data_samples(contents, data_cols, datatable_name,
                                                 ids_vector, default_data_vals,
                                                 dt_length, summarise_reps, all_reps),
                      sep='\n'))
  all_reps = TRUE
  expect_snapshot(cat(datatable_add_data_samples(contents, data_cols, datatable_name,
                                                 ids_vector, default_data_vals,
                                                 dt_length, summarise_reps, all_reps),
                      sep='\n'))

  # try to add to a GROUP of samples which have REPs

  # add group datatable to samples_SC-LUM_50µm
  contents <- c(contents,
                "+===============================================================================",
                "",
                "",
                "    samples_SC-LUM_50µm  :  GROUP",
                "",
                "",
                "      ID      rep      group-solvent-inc      group-ab-conc    ",
                "    ======  =======  =====================  =================  ",
                "",
                "     1001      1              1Hr                 1mg/mL       ",
                "                                                               ",
                "     1001      2              2Hr                0.5mg/mL      ",
                "                                                               ",
                "     1001      3              4Hr                 1mg/mL       ",
                "                                                               ",
                "     1001      4              1Hr                0.5mg/mL      ",
                "                                                               ",
                "     1001      5              2Hr                 1mg/mL       ",
                "                                                               ",
                "     1001      6              4Hr                0.5mg/mL      ",
                "                                                               ",
                "     1001      7              1Hr                 1mg/mL       ",
                "                                                               ",
                "     1001      8              2Hr                0.5mg/mL      ",
                "                                                               ",
                "     1002      1              4Hr                 1mg/mL       ",
                "                                                               ",
                "     1002      2              1Hr                0.5mg/mL      ",
                "                                                               ",
                "     1002      3              2Hr                 1mg/mL       ",
                "                                                               ",
                "     1002      4              4Hr                0.5mg/mL      ",
                "                                                               ",
                "     1002      5              1Hr                 1mg/mL       ",
                "                                                               ",
                "     1002      6              2Hr                0.5mg/mL      ",
                "                                                               ",
                "     1002      7              4Hr                 1mg/mL       ",
                "                                                               ",
                "     1002      8              1Hr                0.5mg/mL      ",
                "                                                               ",
                "     1003      1              2Hr                 1mg/mL       ",
                "                                                               ",
                "     1003      2              4Hr                0.5mg/mL      ",
                "                                                               ",
                "     1003      3              1Hr                 1mg/mL       ",
                "                                                               ",
                "     1003      4              2Hr                0.5mg/mL      ",
                "                                                               ",
                "     1003      5              4Hr                 1mg/mL       ",
                "                                                               ",
                "     1003      6              1Hr                0.5mg/mL      ",
                "                                                               ",
                "     1003      7              2Hr                 1mg/mL       ",
                "                                                               ",
                "     1003      8              4Hr                0.5mg/mL      ",
                "                                                               ",
                "     1004      1              1Hr                 1mg/mL       ",
                "                                                               ",
                "     1004      2              2Hr                0.5mg/mL      ",
                "                                                               ",
                "     1004      3              4Hr                 1mg/mL       ",
                "                                                               ",
                "     1004      4              1Hr                0.5mg/mL      ",
                "                                                               ",
                "     1004      5              2Hr                 1mg/mL       ",
                "                                                               ",
                "     1004      6              4Hr                0.5mg/mL      ",
                "                                                               ",
                "     1004      7              1Hr                 1mg/mL       ",
                "                                                               ",
                "     1004      8              2Hr                0.5mg/mL      ",
                "                                                               ",
                "",
                "+===============================================================================",
                "")

  # testing all lengths of data cols
  # by GROUP
  data_cols=c("perfuse_wash_dt", "perfuse_wash_con", "perf_loc", "perf", "p")
  datatable_name = "samples_SC-LUM_50µm"
  ids_vector="4Hr"
  default_data_vals=list()
  dt_length = 100
  summarise_reps = FALSE
  all_reps = FALSE
  expect_snapshot(cat(datatable_add_data_samples(contents, data_cols, datatable_name,
                                                 ids_vector, default_data_vals,
                                                 dt_length, summarise_reps, all_reps),
                      sep='\n'))



  ### ERRORS

  # test add data to already resampled IDs throw an error
  # IDs and REPs datatable:
  contents <- c(contents,
                "",
                "+===============================================================================",
                "",
                "",
                "    samples_SC-LUM_50µm  :  RESAMPLE",
                "",
                "",
                "      ID      rep      resample      reps    ",
                "    ======  =======  ============  ========  ",
                "",
                "     1001      1         DH-LT         1     ",
                "                         DH-RT         1     ",
                "                                             ",
                "     1001      2         DH-LT         1     ",
                "                         DH-RT         1     ",
                "                                             ",
                "     1001      3         DH-LT         1     ",
                "                         DH-RT         1     ",
                "                                             ",
                "     1001      4         DH-LT         1     ",
                "                         DH-RT         1     ",
                "                                             ",
                "     1001      5         DH-LT         1     ",
                "                         DH-RT         1     ",
                "                                             ",
                "     1001      6         DH-LT         1     ",
                "                         DH-RT         1     ",
                "                                             ",
                "     1001      7         DH-LT         1     ",
                "                         DH-RT         1     ",
                "                                             ",
                "     1001      8         DH-LT         1     ",
                "                         DH-RT         1     ",
                "                                             ",
                "     1002      1         DH-LT         1     ",
                "                         DH-RT         1     ",
                "                                             ",
                "     1002      2         DH-LT         1     ",
                "                         DH-RT         1     ",
                "                                             ",
                "     1002      3         DH-LT         1     ",
                "                         DH-RT         1     ",
                "                                             ",
                "     1002      4         DH-LT         1     ",
                "                         DH-RT         1     ",
                "                                             ",
                "     1002      5         DH-LT         1     ",
                "                         DH-RT         1     ",
                "                                             ",
                "     1002      6         DH-LT         1     ",
                "                         DH-RT         1     ",
                "                                             ",
                "     1002      7         DH-LT         1     ",
                "                         DH-RT         1     ",
                "                                             ",
                "     1002      8         DH-LT         1     ",
                "                         DH-RT         1     ",
                "                                             ",
                "     1003      1         DH-LT         1     ",
                "                         DH-RT         1     ",
                "                                             ",
                "     1003      2         DH-LT         1     ",
                "                         DH-RT         1     ",
                "                                             ",
                "     1003      3         DH-LT         1     ",
                "                         DH-RT         1     ",
                "                                             ",
                "     1003      4         DH-LT         1     ",
                "                         DH-RT         1     ",
                "                                             ",
                "     1003      5         DH-LT         1     ",
                "                         DH-RT         1     ",
                "                                             ",
                "     1003      6         DH-LT         1     ",
                "                         DH-RT         1     ",
                "                                             ",
                "     1003      7         DH-LT         1     ",
                "                         DH-RT         1     ",
                "                                             ",
                "     1003      8         DH-LT         1     ",
                "                         DH-RT         1     ",
                "                                             ",
                "     1004      1         DH-LT         1     ",
                "                         DH-RT         1     ",
                "                                             ",
                "     1004      2         DH-LT         1     ",
                "                         DH-RT         1     ",
                "                                             ",
                "     1004      3         DH-LT         1     ",
                "                         DH-RT         1     ",
                "                                             ",
                "     1004      4         DH-LT         1     ",
                "                         DH-RT         1     ",
                "                                             ",
                "     1004      5         DH-LT         1     ",
                "                         DH-RT         1     ",
                "                                             ",
                "     1004      6         DH-LT         1     ",
                "                         DH-RT         1     ",
                "                                             ",
                "     1004      7         DH-LT         1     ",
                "                         DH-RT         1     ",
                "                                             ",
                "     1004      8         DH-LT         1     ",
                "                         DH-RT         1     ",
                "                                             ",
                "",
                "+===============================================================================",
                "",
                "")

  # IDs ONLY datatable
  datatable_name = "samples"
  expect_error(datatable_add_data_samples(contents, data_cols, datatable_name,
                                          ids_vector, default_data_vals,
                                          dt_length, summarise_reps, all_reps))

  # try with summarise_reps FALSE
  data_cols=c("perfuse_wash_dt", "perfuse_wash_con", "p", "perf")
  datatable_name = "samples_SC-LUM_50µm"
  ids_vector=""
  default_data_vals=list()
  dt_length = 100
  summarise_reps = FALSE
  all_reps = FALSE
  expect_error(datatable_add_data_samples(contents, data_cols, datatable_name,
                                          ids_vector, default_data_vals,
                                          dt_length, summarise_reps, all_reps))
  summarise_reps = TRUE
  expect_error(datatable_add_data_samples(contents, data_cols, datatable_name,
                                          ids_vector, default_data_vals,
                                          dt_length, summarise_reps, all_reps))

  # test duplicated cols throw an error
  datatable_name = "samples_CNS"
  data_cols=c("perfuse_wash_dt", "perfuse_wash_con", "p", "p")
  expect_error(datatable_add_data_samples(contents, data_cols, datatable_name,
                                          ids_vector, default_data_vals,
                                          dt_length, summarise_reps, all_reps))

  # test incorrect datatable name throw an error
  data_cols=c("perfuse_wash_dt", "perfuse_wash_con", "p", "perf")
  datatable_name = "samples2"
  expect_error(datatable_add_data_samples(contents, data_cols, datatable_name,
                                          ids_vector, default_data_vals,
                                          dt_length, summarise_reps, all_reps))


})



test_that("datatable_add_data_variables()", {

  # test default ADD_DATA dt generation - with all possible value lengths
  # ids_vector is default blank string
  contents <- c("",
                "+===============================================================================",
                "",
                "",
                "    samples  :  CREATE",
                "",
                "",
                "      ID      x      wt-g         perfuse_dt         perfusion_con      group-fix    ",
                "    ======  =====  ========  ====================  =================  =============  ",
                "",
                "     1001     F      30.0      2020-01-01:12:01        F4M1PB_RT          fix_RT     ",
                "                                                                                     ",
                "     1002     F      31.1      2020-01-01:12:02        F4M2PB_RT          fix_RT     ",
                "                                                                                     ",
                "     1003     M      32.2      2020-01-01:12:03        F4M3PB_4C          fix_4C     ",
                "                                                                                     ",
                "     1004     M      33.3      2020-01-01:12:04        F4M4PB_4C          fix_4C     ",
                "                                                                                     ",
                "",
                "+===============================================================================",
                "")
  # testing all lengths of data cols
  var_names=c("perfuse_wash_dt", "perfuse_wash_con", "p", "perf")
  datatable_name = "samples"
  # first test special group ALL
  group_names="ALL"
  default_data_vals=list()
  dt_length = 100
  expect_snapshot(cat(datatable_add_data_variables(contents, var_names,
                                                   datatable_name, group_names,
                                                   default_data_vals, dt_length),
                      sep='\n'))

  # test group_names is vector of group IDs
  group_names=c("fix_RT", "fix_4C")
  expect_snapshot(cat(datatable_add_data_variables(contents, var_names,
                                                   datatable_name, group_names,
                                                   default_data_vals, dt_length),
                      sep='\n'))

  # test setting default data vals
  default_data_vals=list(c("RT_Datetime", "4C_Datetime"),c("RT_wash_con", "4C_wash_con"),
                         c("RT_p", "4C_p"), c("RT_perf", "4C_perf"))
  expect_snapshot(cat(datatable_add_data_variables(contents, var_names,
                                                   datatable_name, group_names,
                                                   default_data_vals, dt_length),
                      sep='\n'))

  ### ERRORS

  # test not passing all group values throws an error
  group_names=c("fix_RT")
  expect_error(datatable_add_data_variables(contents, var_names, datatable_name,
                                            group_names, default_data_vals, dt_length))


  # test passing group name throws an error
  group_names=c("group-fix")
  expect_error(datatable_add_data_variables(contents, var_names, datatable_name,
                                            group_names, default_data_vals, dt_length))

  # test duplicated cols throw an error
  group_names="ALL"
  var_names=c("perfuse_wash_dt", "perfuse_wash_con", "p", "p")
  expect_error(datatable_add_data_variables(contents, var_names, datatable_name,
                                            group_names,default_data_vals, dt_length))

  # test incorrect datatable name throw an error
  var_names=c("perfuse_wash_dt", "perfuse_wash_con", "p", "perf")
  datatable_name = "samples2"
  expect_error(datatable_add_data_variables(contents, var_names, datatable_name,
                                            group_names, default_data_vals, dt_length))


})



test_that("datatable_add_data_timetable()", {

  contents <- c("",
                "+===============================================================================",
                "",
                "",
                "    samples  :  CREATE",
                "",
                "",
                "      ID      x      wt-g         perfuse_dt         perfusion_con      group-fix    ",
                "    ======  =====  ========  ====================  =================  =============  ",
                "",
                "     1001     F      30.0      2020-01-01:12:01        F4M1PB_RT          fix_RT     ",
                "                                                                                     ",
                "     1002     F      31.1      2020-01-01:12:02        F4M2PB_RT          fix_RT     ",
                "                                                                                     ",
                "     1003     M      32.2      2020-01-01:12:03        F4M3PB_4C          fix_4C     ",
                "                                                                                     ",
                "     1004     M      33.3      2020-01-01:12:04        F4M4PB_4C          fix_4C     ",
                "                                                                                     ",
                "",
                "+===============================================================================",
                "")

  # testing all lengths of data cols
  step_names=c("HT_RT", "MT_RT", "DMT_RT", "MT_RT", "HT_RT", "PBS_RT")
  datatable_name = "samples"
  # test group_names is vector of group IDs
  group_names=c("fix_4C", "fix_RT")
  col_name <- "delip"
  dt_length = 100
  expect_snapshot(cat(datatable_add_data_timetable(contents, step_names,
                                                   datatable_name, group_names,
                                                   col_name, dt_length),
                      sep='\n'))

  # test group_names reversed
  group_names=c("fix_RT", "fix_4C")
  expect_snapshot(cat(datatable_add_data_timetable(contents, step_names,
                                                   datatable_name, group_names,
                                                   col_name, dt_length),
                      sep='\n'))


  ### ERRORS

  step_names=c("HT_RT", "MT_RT", "DMT_RT", "M T_RT", "HT_RT", "PBS_RT")
  expect_error(datatable_add_data_timetable(contents, step_names, datatable_name,
                                            group_names, col_name, default_data_vals,
                                            dt_length))

  # test group_names as IDs fails
  step_names=c("HT_RT", "MT_RT", "DMT_RT", "MT_RT", "HT_RT", "PBS_RT")
  group_names=c(1001, 1002, 1003, 1004)
  expect_error(datatable_add_data_timetable(contents, step_names, datatable_name,
                                            group_names, col_name, default_data_vals,
                                            dt_length))

  # test group_name ALL throws error
  group_names=c("ALL")
  expect_error(datatable_add_data_timetable(contents, step_names, datatable_name,
                                            group_names, col_name, default_data_vals,
                                            dt_length))


  # test not passing all group values throws an error
  group_names=c("fix_RT")
  expect_error(datatable_add_data_timetable(contents, step_names, datatable_name,
                                            group_names, col_name, default_data_vals,
                                            dt_length))

  # test passing group name throws an error
  group_names=c("group-fix")
  expect_error(datatable_add_data_timetable(contents, step_names, datatable_name,
                                            group_names, col_name, default_data_vals,
                                            dt_length))

  # test incorrect datatable name throw an error
  group_names=c("fix_RT", "fix_4C")
  datatable_name = "samples2"
  expect_error(datatable_add_data_timetable(contents, step_names, datatable_name,
                                            group_names, col_name, default_data_vals,
                                            dt_length))

})



test_that("datatable_add_group()", {

  contents <- c("",
                "+===============================================================================",
                "",
                "",
                "    samples  :  CREATE",
                "",
                "",
                "      ID      x      wt-g         perfuse_dt         perfusion_con      group-fix    ",
                "    ======  =====  ========  ====================  =================  =============  ",
                "",
                "     1001     F      30.0      2020-01-01:12:01        F4M1PB_RT          fix_RT     ",
                "                                                                                     ",
                "     1002     F      31.1      2020-01-01:12:02        F4M2PB_RT          fix_RT     ",
                "                                                                                     ",
                "     1003     M      32.2      2020-01-01:12:03        F4M3PB_4C          fix_4C     ",
                "                                                                                     ",
                "     1004     M      33.3      2020-01-01:12:04        F4M4PB_4C          fix_4C     ",
                "                                                                                     ",
                "",
                "+===============================================================================",
                "")

  # adding two new groups
  group_names=c("group-solvent-inc", "group-ab-conc")
  datatable_name = "samples"
  # 3-value and 2-value additions
  groups=list(c("1Hr","2Hr", "4Hr"),
              c(" 1mg/mL","0.5mg/mL"))
  dt_length = 100
  summarise_reps = FALSE
  all_reps = FALSE
  expect_snapshot(cat(datatable_add_group(contents, group_names, datatable_name,
                                          groups, dt_length, summarise_reps, all_reps),
                      sep='\n'))

  # adding default groups
  group_names=c("group_solvent_inc", "group_ab")
  datatable_name = "samples"
  groups=list(c(""), c(""))
  dt_length = 100
  summarise_reps = FALSE
  all_reps = FALSE
  expect_snapshot(cat(datatable_add_group(contents, group_names, datatable_name,
                                          groups, dt_length, summarise_reps, all_reps),
                      sep='\n'))


  # check ADD GROUPS to samples with reps
  contents <- c(contents,
                "",
                "+===============================================================================",
                "",
                "",
                "    samples  :  RESAMPLE",
                "",
                "",
                "      ID       resample      reps    ",
                "    ======  =============  ========  ",
                "",
                "     1001        CNS           1     ",
                "                SC-LUM         1     ",
                "              DRG-L4-LT        1     ",
                "                                     ",
                "     1002        CNS           1     ",
                "                SC-LUM         1     ",
                "              DRG-L4-LT        1     ",
                "                                     ",
                "     1003        CNS           1     ",
                "                SC-LUM         1     ",
                "              DRG-L4-LT        1     ",
                "                                     ",
                "     1004        CNS           1     ",
                "                SC-LUM         1     ",
                "              DRG-L4-LT        1     ",
                "                                     ",
                "",
                "+===============================================================================",
                "",
                "",
                "",
                "+===============================================================================",
                "",
                "",
                "    samples_SC-LUM  :  RESAMPLE",
                "",
                "",
                "      ID      resample      reps    ",
                "    ======  ============  ========  ",
                "",
                "     1001       50µm          2     ",
                "                                    ",
                "     1002       50µm          2     ",
                "                                    ",
                "     1003       50µm          2     ",
                "                                    ",
                "     1004       50µm          2     ",
                "                                    ",
                "",
                "+===============================================================================",
                "",
                "")

  # adding two new groups
  group_names=c("group-solvent-inc", "group-ab-conc")
  datatable_name = "samples_SC-LUM_50µm"
  # 3-value and 2-value additions
  groups=list(c("1Hr","2Hr", "4Hr"),
              c(" 1mg/mL","0.5mg/mL"))
  dt_length = 100
  summarise_reps = FALSE # try with summarise_reps FALSE
  all_reps = FALSE
  expect_snapshot(cat(datatable_add_group(contents, group_names, datatable_name,
                                          groups, dt_length, summarise_reps, all_reps),
                      sep='\n'))

  summarise_reps = TRUE # & summarise_reps TRUE
  expect_snapshot(cat(datatable_add_group(contents, group_names, datatable_name,
                                          groups, dt_length, summarise_reps, all_reps),
                      sep='\n'))

  all_reps = TRUE # & all_reps TRUE
  expect_snapshot(cat(datatable_add_group(contents, group_names, datatable_name,
                                          groups, dt_length, summarise_reps, all_reps),
                      sep='\n'))



  ### ERRORS

  # test group_name does not start with group- throws error
  group_names=c("group-solvent-inc", "ab-conc")
  expect_error(datatable_add_group(contents, group_names, datatable_name, groups,
                                   dt_length, summarise_reps, all_reps))


  # test group_name contains duplicates throws error
  group_names=c("group-solvent-inc", "group-solvent-inc")
  expect_error(datatable_add_group(contents, group_names, datatable_name, groups,
                                   dt_length, summarise_reps, all_reps))

  # test group_name must be same length as groups
  group_names=c("group-solvent-inc", "group-solvent", "group-ab-conc")
  expect_error(datatable_add_group(contents, group_names, datatable_name, groups,
                                   dt_length, summarise_reps, all_reps))

  # test incorrect datatable name throw an error
  group_names=c("group-solvent-inc", "group-ab-conc")
  datatable_name = "samples2"
  expect_error(datatable_add_group(contents, group_names, datatable_name, groups,
                                   dt_length, summarise_reps, all_reps))

})


test_that("datatable_dispose()", {

  contents <- c("",
                "+===============================================================================",
                "",
                "",
                "    samples  :  CREATE",
                "",
                "",
                "      ID      x      wt-g         perfuse_dt         perfusion_con      group-fix    ",
                "    ======  =====  ========  ====================  =================  =============  ",
                "",
                "     1001     F      30.0      2020-01-01:12:01        F4M1PB_RT          fix_RT     ",
                "                                                                                     ",
                "     1002     F      31.1      2020-01-01:12:02        F4M2PB_RT          fix_RT     ",
                "                                                                                     ",
                "     1003     M      32.2      2020-01-01:12:03        F4M3PB_4C          fix_4C     ",
                "                                                                                     ",
                "     1004     M      33.3      2020-01-01:12:04        F4M4PB_4C          fix_4C     ",
                "                                                                                     ",
                "",
                "+===============================================================================",
                "")

  datatable_name = "samples"
  dt_length = 100
  summarise_reps = FALSE
  all_reps = FALSE
  cdt="2024-08-16:14:23" # add fixed datetime


  expect_snapshot(cat(datatable_dispose(contents, datatable_name, dt_length, summarise_reps, all_reps, cdt),
                      sep='\n'))



  ### ERRORS

  # test incorrect datatable name throw an error
  datatable_name = "samples2"
  expect_error(datatable_dispose(contents, datatable_name, dt_length, summarise_reps, all_reps, cdt))

})



test_that("datatable_resample()", {

  contents <- c("",
                "+===============================================================================",
                "",
                "",
                "    samples  :  CREATE",
                "",
                "",
                "      ID      x      wt-g         perfuse_dt         perfusion_con      group-fix    ",
                "    ======  =====  ========  ====================  =================  =============  ",
                "",
                "     1001     F      30.0      2020-01-01:12:01        F4M1PB_RT          fix_RT     ",
                "                                                                                     ",
                "     1002     F      31.1      2020-01-01:12:02        F4M2PB_RT          fix_RT     ",
                "                                                                                     ",
                "     1003     M      32.2      2020-01-01:12:03        F4M3PB_4C          fix_4C     ",
                "                                                                                     ",
                "     1004     M      33.3      2020-01-01:12:04        F4M4PB_4C          fix_4C     ",
                "                                                                                     ",
                "",
                "+===============================================================================",
                "")

  datatable_name = "samples"
  # resampling to three sub-samples
  resample_vector=c("CNS", "SC-LUM", "DRG-L4-LT")
  # all are rep 1 - having single value gets multiplied across all resample elements
  rep_vector=c(1)
  dt_length = 100
  summarise_reps = TRUE
  all_reps = FALSE

  expect_snapshot(cat(datatable_resample(contents, datatable_name, resample_vector,
                                         rep_vector, dt_length, summarise_reps, all_reps),
                      sep='\n'))

  # test with rep_vector length == resample_vector length
  rep_vector=c(1,1,1)
  expect_snapshot(cat(datatable_resample(contents, datatable_name, resample_vector,
                                         rep_vector, dt_length, summarise_reps, all_reps),
                      sep='\n'))

  # test with various rep values
  rep_vector=c(1,1,2)
  expect_snapshot(cat(datatable_resample(contents, datatable_name, resample_vector,
                                         rep_vector, dt_length, summarise_reps, all_reps),
                      sep='\n'))

  # test reasmpling with samples that have EXISTING REPs
  contents <- c(contents,
                "",
                "+===============================================================================",
                "",
                "",
                "    samples  :  RESAMPLE",
                "",
                "",
                "      ID       resample      reps    ",
                "    ======  =============  ========  ",
                "",
                "     1001        CNS           1     ",
                "                SC-LUM         1     ",
                "              DRG-L4-LT        1     ",
                "                                     ",
                "     1002        CNS           1     ",
                "                SC-LUM         1     ",
                "              DRG-L4-LT        1     ",
                "                                     ",
                "     1003        CNS           1     ",
                "                SC-LUM         1     ",
                "              DRG-L4-LT        1     ",
                "                                     ",
                "     1004        CNS           1     ",
                "                SC-LUM         1     ",
                "              DRG-L4-LT        1     ",
                "                                     ",
                "",
                "+===============================================================================",
                "",
                "",
                "",
                "+===============================================================================",
                "",
                "",
                "    samples_SC-LUM  :  RESAMPLE",
                "",
                "",
                "      ID      resample      reps    ",
                "    ======  ============  ========  ",
                "",
                "     1001       50µm          8     ",
                "                                    ",
                "     1002       50µm          8     ",
                "                                    ",
                "     1003       50µm          8     ",
                "                                    ",
                "     1004       50µm          8     ",
                "                                    ",
                "",
                "+===============================================================================",
                "",
                "")

  # resampling the SC-LUM_50µm reps
  datatable_name = "samples_SC-LUM_50µm"
  # resampling to three sub-samples
  resample_vector=c("DH-LT", "DH-RT", "VH-LT", "VH-RT")
  # all are rep 1 - having single value gets multiplied across all resample elements
  rep_vector=c(1)
  dt_length = 100

  expect_snapshot(cat(datatable_resample(contents, datatable_name, resample_vector,
                                         rep_vector, dt_length, summarise_reps, all_reps),
                      sep='\n'))

  ### ERRORS

  # test resample_vector element has underscore throws error
  resample_vector=c("CNS", "SC_LUM", "DRG-L4-LT")
  expect_error(datatable_resample(contents, datatable_name, resample_vector,
                                  rep_vector, dt_length, summarise_reps, all_reps))

  # test rep_vector length != resample_vector length throws error
  resample_vector=c("CNS", "SC-LUM", "DRG-L4-LT")
  rep_vector=c(1,1,1,1)
  expect_error(datatable_resample(contents, datatable_name, resample_vector,
                                  rep_vector, dt_length, summarise_reps, all_reps))

  # test incorrect datatable name throw an error
  rep_vector=c(1)
  datatable_name = "samples2"
  expect_error(datatable_resample(contents, datatable_name, resample_vector,
                                  rep_vector, dt_length, summarise_reps, all_reps))

})



test_that("datatable_export()", {

  # test default EXPORT dt generation - with all possible value lengths
  contents <- c("",
                "+===============================================================================",
                "",
                "",
                "    samples  :  CREATE",
                "",
                "",
                "      ID      x      wt-g         perfuse_dt         perfusion_con      group-fix    ",
                "    ======  =====  ========  ====================  =================  =============  ",
                "",
                "     1001     F      30.0      2020-01-01:12:01        F4M1PB_RT          fix_RT     ",
                "                                                                                     ",
                "     1002     F      31.1      2020-01-01:12:02        F4M2PB_RT          fix_RT     ",
                "                                                                                     ",
                "     1003     M      32.2      2020-01-01:12:03        F4M3PB_4C          fix_4C     ",
                "                                                                                     ",
                "     1004     M      33.3      2020-01-01:12:04        F4M4PB_4C          fix_4C     ",
                "                                                                                     ",
                "",
                "+===============================================================================",
                "")
  datatable_name = "samples"
  destination_rel_path_link="[dn-t___001](../dn-t/dn-t___001_--_dest_note.Rmd)" # using EXAMPLE source and destination paths
  ids_vector=""
  reps_vector=""
  dt_length = 100
  summarise_reps = FALSE
  expect_snapshot( cat(datatable_export(contents, datatable_name, destination_rel_path_link,
                                       ids_vector, reps_vector, dt_length, summarise_reps),
                      sep='\n') )

  # test when path is very looooooong
  destination_rel_path_link="[dn-t___001](../../../../../programme-path/project-doc-path/project-note-path/note-sub-path/dn-t/dn-t___001_--_dest_note.Rmd)"
  # using EXAMPLE source and destination paths
  expect_snapshot( cat(datatable_export(contents, datatable_name, destination_rel_path_link,
                                        ids_vector, reps_vector, dt_length, summarise_reps),
                       sep='\n') )

  # test subset of IDs
  destination_rel_path_link="[dn-t___001](../dn-t/dn-t___001_--_dest_note.Rmd)" # using EXAMPLE source and destination paths
  ids_vector=c("1002", "1003")
  expect_snapshot( cat(datatable_export(contents, datatable_name, destination_rel_path_link,
                                        ids_vector, reps_vector, dt_length, summarise_reps),
                       sep='\n') )

  # test subset of IDs in different order
  ids_vector=c("1004", "1002", "1003")
  expect_snapshot( cat(datatable_export(contents, datatable_name, destination_rel_path_link,
                                        ids_vector, reps_vector, dt_length, summarise_reps),
                       sep='\n') )


  ### check EXPORT to samples with reps

  # add RESAMPLE datatables to contents:

  contents <- c(contents,
                "",
                "+===============================================================================",
                "",
                "",
                "    samples  :  RESAMPLE",
                "",
                "",
                "      ID       resample      reps    ",
                "    ======  =============  ========  ",
                "",
                "     1001        CNS           1     ",
                "                SC-LUM         1     ",
                "              DRG-L4-LT        1     ",
                "                                     ",
                "     1002        CNS           1     ",
                "                SC-LUM         1     ",
                "              DRG-L4-LT        1     ",
                "                                     ",
                "     1003        CNS           1     ",
                "                SC-LUM         1     ",
                "              DRG-L4-LT        1     ",
                "                                     ",
                "     1004        CNS           1     ",
                "                SC-LUM         1     ",
                "              DRG-L4-LT        1     ",
                "                                     ",
                "",
                "+===============================================================================",
                "",
                "",
                "",
                "+===============================================================================",
                "",
                "",
                "    samples_SC-LUM  :  RESAMPLE",
                "",
                "",
                "      ID      resample      reps    ",
                "    ======  ============  ========  ",
                "",
                "     1001       50µm          2     ",
                "                                    ",
                "     1002       50µm          2     ",
                "                                    ",
                "     1003       50µm          2     ",
                "                                    ",
                "     1004       50µm          2     ",
                "                                    ",
                "",
                "+===============================================================================",
                "",
                "")

  # try with summarise_reps FALSE
  datatable_name = "samples_SC-LUM_50µm"
  ids_vector=""
  reps_vector=""
  dt_length = 100
  summarise_reps = FALSE
  expect_snapshot( cat(datatable_export(contents, datatable_name, destination_rel_path_link,
                                        ids_vector, reps_vector, dt_length, summarise_reps),
                       sep='\n') )

  # try with summarise_reps TRUE
  summarise_reps = TRUE
  expect_snapshot( cat(datatable_export(contents, datatable_name, destination_rel_path_link,
                                        ids_vector, reps_vector, dt_length, summarise_reps),
                       sep='\n') )

  # try subset of IDs with summarise_reps FALSE
  datatable_name = "samples_SC-LUM_50µm"
  ids_vector="1002"
  reps_vector=""
  dt_length = 100
  summarise_reps = FALSE
  expect_snapshot( cat(datatable_export(contents, datatable_name, destination_rel_path_link,
                                        ids_vector, reps_vector, dt_length, summarise_reps),
                       sep='\n') )

  # try with summarise_reps TRUE
  summarise_reps = TRUE
  expect_snapshot( cat(datatable_export(contents, datatable_name, destination_rel_path_link,
                                        ids_vector, reps_vector, dt_length, summarise_reps),
                       sep='\n') )

  # try subset of IDs & REPs with summarise_reps FALSE
  datatable_name = "samples_SC-LUM_50µm"
  ids_vector="1002"
  reps_vector="2"
  dt_length = 100
  summarise_reps = FALSE
  expect_snapshot( cat(datatable_export(contents, datatable_name, destination_rel_path_link,
                                        ids_vector, reps_vector, dt_length, summarise_reps),
                       sep='\n') )

  # [x] SUCCESS
  # generates EXPORT plaintext datatable with correct spacing & cols for IDs with reps
  # subset - 1002 - one rep on separate lines

  # try with summarise_reps TRUE
  summarise_reps = TRUE
  expect_snapshot( cat(datatable_export(contents, datatable_name, destination_rel_path_link,
                                        ids_vector, reps_vector, dt_length, summarise_reps),
                       sep='\n') )
  # [x] SUCCESS
  # generates EXPORT plaintext datatable with correct spacing & cols for IDs with reps


  # try subset of IDs & REPs with summarise_reps FALSE
  datatable_name = "samples_SC-LUM_50µm"
  ids_vector=c("1002", "1003", "1001", "1002")
  reps_vector=c("2", "1", "1", "1")
  dt_length = 100
  summarise_reps = FALSE
  expect_snapshot( cat(datatable_export(contents, datatable_name, destination_rel_path_link,
                                        ids_vector, reps_vector, dt_length, summarise_reps),
                       sep='\n') )
  # [x] SUCCESS
  # generates EXPORT plaintext datatable with correct spacing & cols for IDs with reps
  # subset - 1002 - both reps on separate lines

  # try with summarise_reps TRUE
  summarise_reps = TRUE
  expect_snapshot( cat(datatable_export(contents, datatable_name, destination_rel_path_link,
                                        ids_vector, reps_vector, dt_length, summarise_reps),
                       sep='\n') )
  # [x] SUCCESS
  # generates EXPORT plaintext datatable with correct spacing & cols for IDs with reps
  # correctly combines 1002 reps into one line!


  # more tests with subsets of IDs and REPs
  contents <- c(contents,
                "",
                "+===============================================================================",
                "",
                "",
                "    samples_SC-LUM_50µm  :  RESAMPLE",
                "",
                "",
                "      ID      rep      resample      reps    ",
                "    ======  =======  ============  ========  ",
                "",
                "     1001      1         DH-LT         1     ",
                "                         DH-RT         3     ",
                "                                             ",
                "     1001      2         DH-LT         1     ",
                "                         DH-RT         3     ",
                "                                             ",
                "     1002      1         DH-LT         1     ",
                "                         DH-RT         1     ",
                "                                             ",
                "     1002      2         DH-LT         1     ",
                "                         DH-RT         3     ",
                "                                             ",
                "     1003      1         DH-LT         1     ",
                "                         DH-RT         3     ",
                "                                             ",
                "     1003      2         DH-LT         1     ",
                "                         DH-RT         3     ",
                "                                             ",
                "     1004      1         DH-LT         1     ",
                "                         DH-RT         3     ",
                "                                             ",
                "     1004      2         DH-LT         1     ",
                "                         DH-RT         3     ",
                "                                             ",
                "",
                "+===============================================================================",
                "",
                "")

  # confirm will pass with any other combos - and in random order!
  datatable_name = "samples_SC-LUM_50µm-1_DH-RT"
  ids_vector=c("1002", "1003", "1004", "1001", "1003")
  reps_vector = c("1", "3", "2", "3", "1")
  summarise_reps = FALSE
  expect_snapshot( cat(datatable_export(contents, datatable_name, destination_rel_path_link,
                                        ids_vector, reps_vector, dt_length, summarise_reps),
                       sep='\n') )
  # [x] SUCCESS
  # generates EXPORT plaintext datatable with correct spacing & cols for IDs with reps


  summarise_reps = TRUE
  expect_snapshot( cat(datatable_export(contents, datatable_name, destination_rel_path_link,
                                        ids_vector, reps_vector, dt_length, summarise_reps),
                       sep='\n') )
  # [x] SUCCESS
  # generates EXPORT plaintext datatable with correct spacing & cols for IDs with reps


  ### ERRORS

  # test export of already resampled IDs throw an error

  # try with summarise_reps FALSE
  datatable_name = "samples_SC-LUM_50µm"
  ids_vector=""
  reps_vector=""
  dt_length = 100
  summarise_reps = FALSE
  expect_error(datatable_export(contents, datatable_name, destination_rel_path_link,
                                ids_vector, reps_vector, dt_length, summarise_reps))
  # correct failure
  summarise_reps = TRUE
  expect_error(datatable_export(contents, datatable_name, destination_rel_path_link,
                                ids_vector, reps_vector, dt_length, summarise_reps))
  # correct failure


  # IDs ONLY datatable
  datatable_name = "samples"
  expect_error(datatable_export(contents, datatable_name, destination_rel_path_link,
                                ids_vector, reps_vector, dt_length, summarise_reps))
  # correct failure

  # test incorrect datatable name throw an error
  datatable_name = "samples2"
  expect_error(datatable_export(contents, datatable_name, destination_rel_path_link,
                                ids_vector, reps_vector, dt_length, summarise_reps))


  # confirm that disposing some IDs from IDs-only dt and trying to export ANY of those IDs fails
  contents <- c(contents,
                "",
                "",
                "+===============================================================================",
                "",
                "",
                "    samples_SC-LUM_50µm-1_DH-LT  :  ADD_DATA",
                "",
                "",
                "      ID         dispose        ",
                "    ======  ==================  ",
                "",
                "     1001    2024-09-26:15:37   ",
                "                                ",
                "     1004    2024-09-26:15:37   ",
                "                                ",
                "",
                "+===============================================================================",
                "",
                "")

  # test subset of IDs
  datatable_name = "samples_SC-LUM_50µm-1_DH-LT"
  ids_vector=c("1001", "1003")
  expect_error(datatable_export(contents, datatable_name, destination_rel_path_link,
                                ids_vector, reps_vector, dt_length, summarise_reps))

  # confirm disposing some ID-REP combos then trying to export any of those ID-REPs fails

  contents <- c(contents,
                "",
                "",
                "+===============================================================================",
                "",
                "",
                "    samples_SC-LUM_50µm-1_DH-RT  :  ADD_DATA",
                "",
                "",
                "      ID     rep        dispose        ",
                "    ======  =====  ==================  ",
                "",
                "     1001     1     2024-09-26:15:37   ",
                "                                       ",
                "     1004     1     2024-09-26:15:37   ",
                "                                       ",
                "",
                "+===============================================================================",
                "",
                "")
  datatable_name = "samples_SC-LUM_50µm-1_DH-RT"
  ids_vector=c("1001", "1003", "1004", "1004")
  reps_vector = c("1", "3", "1", "3")
  expect_error(datatable_export(contents, datatable_name, destination_rel_path_link,
                                ids_vector, reps_vector, dt_length, summarise_reps))
  # correct failure


})



test_that("datatable_import()", {

  # test default IMPORT dt generation - with all possible value lengths
  source_contents <- c("",
                "+===============================================================================",
                "",
                "",
                "    samples  :  CREATE",
                "",
                "",
                "      ID      x      wt-g         perfuse_dt         perfusion_con      group-fix    ",
                "    ======  =====  ========  ====================  =================  =============  ",
                "",
                "     1001     F      30.0      2020-01-01:12:01        F4M1PB_RT          fix_RT     ",
                "                                                                                     ",
                "     1002     F      31.1      2020-01-01:12:02        F4M2PB_RT          fix_RT     ",
                "                                                                                     ",
                "     1003     M      32.2      2020-01-01:12:03        F4M3PB_4C          fix_4C     ",
                "                                                                                     ",
                "     1004     M      33.3      2020-01-01:12:04        F4M4PB_4C          fix_4C     ",
                "                                                                                     ",
                "",
                "+===============================================================================",
                "")
  destination_contents <- ""
  datatable_name = "samples"
  source_rel_path_link="[sn-t___001](../sn-t/sn-t___001_--_source_note.Rmd)" # using EXAMPLE source and destination paths
  ids_vector=""
  reps_vector=""
  dt_length = 100
  summarise_reps = FALSE
  expect_snapshot( cat(datatable_import(source_contents, destination_contents, datatable_name,
                                       source_rel_path_link, ids_vector, reps_vector,
                                       dt_length, summarise_reps),
                      sep='\n') )

  # test when path is very looooooong
  source_rel_path_link="[sn-t___001]((../../../../../programme-path/project-doc-path/project-note-path/note-sub-path/sn-t/sn-t___001_--_source_note.Rmd)"
  # using EXAMPLE source and destination paths
  expect_snapshot( cat(datatable_import(source_contents, destination_contents, datatable_name,
                                       source_rel_path_link, ids_vector, reps_vector,
                                       dt_length, summarise_reps),
                      sep='\n') )

  # test subset IDs
  ids_vector=c("1002", "1004")
  expect_snapshot( cat(datatable_import(source_contents, destination_contents, datatable_name,
                                        source_rel_path_link, ids_vector, reps_vector,
                                        dt_length, summarise_reps),
                       sep='\n') )


  # test subset IDs different order
  ids_vector=c("1003", "1002", "1004")
  expect_snapshot( cat(datatable_import(source_contents, destination_contents, datatable_name,
                                        source_rel_path_link, ids_vector, reps_vector,
                                        dt_length, summarise_reps),
                       sep='\n') )



  ### check IMPORT to samples with reps

  # add RESAMPLE datatables to contents:

  source_contents <- c(source_contents,
                "",
                "+===============================================================================",
                "",
                "",
                "    samples  :  RESAMPLE",
                "",
                "",
                "      ID       resample      reps    ",
                "    ======  =============  ========  ",
                "",
                "     1001        CNS           1     ",
                "                SC-LUM         1     ",
                "              DRG-L4-LT        1     ",
                "                                     ",
                "     1002        CNS           1     ",
                "                SC-LUM         1     ",
                "              DRG-L4-LT        1     ",
                "                                     ",
                "     1003        CNS           1     ",
                "                SC-LUM         1     ",
                "              DRG-L4-LT        1     ",
                "                                     ",
                "     1004        CNS           1     ",
                "                SC-LUM         1     ",
                "              DRG-L4-LT        1     ",
                "                                     ",
                "",
                "+===============================================================================",
                "",
                "",
                "",
                "+===============================================================================",
                "",
                "",
                "    samples_SC-LUM  :  RESAMPLE",
                "",
                "",
                "      ID      resample      reps    ",
                "    ======  ============  ========  ",
                "",
                "     1001       50µm          2     ",
                "                                    ",
                "     1002       50µm          2     ",
                "                                    ",
                "     1003       50µm          2     ",
                "                                    ",
                "     1004       50µm          2     ",
                "                                    ",
                "",
                "+===============================================================================",
                "",
                "")
  destination_contents <- ""
  # try with summarise_reps FALSE
  datatable_name = "samples_SC-LUM_50µm"
  ids_vector=""
  reps_vector=""
  dt_length = 100
  summarise_reps = FALSE
  expect_snapshot( cat(datatable_import(source_contents, destination_contents, datatable_name,
                                        source_rel_path_link, ids_vector, reps_vector,
                                        dt_length, summarise_reps),
                       sep='\n') )

  # try with summarise_reps TRUE
  summarise_reps = TRUE
  expect_snapshot( cat(datatable_import(source_contents, destination_contents, datatable_name,
                                        source_rel_path_link, ids_vector, reps_vector,
                                        dt_length, summarise_reps),
                       sep='\n') )

  # try with subset IDs REPs & summarise_reps FALSE
  datatable_name = "samples_SC-LUM_50µm"
  ids_vector=c("1004","1002","1002","1003")
  reps_vector = c("2", "1", "2", "1")
  dt_length = 100
  summarise_reps = FALSE
  expect_snapshot( cat(datatable_import(source_contents, destination_contents, datatable_name,
                                        source_rel_path_link, ids_vector, reps_vector,
                                        dt_length, summarise_reps),
                       sep='\n') )

  # try with summarise_reps TRUE
  summarise_reps = TRUE
  expect_snapshot( cat(datatable_import(source_contents, destination_contents, datatable_name,
                                        source_rel_path_link, ids_vector, reps_vector,
                                        dt_length, summarise_reps),
                       sep='\n') )



  ### ERRORS

  # test add data to already resampled IDs throw an error
  # IDs and REPs datatable:
  source_contents <- c(source_contents,
                "",
                "+===============================================================================",
                "",
                "",
                "    samples_SC-LUM_50µm  :  RESAMPLE",
                "",
                "",
                "      ID      rep      resample      reps    ",
                "    ======  =======  ============  ========  ",
                "",
                "     1001      1         DH-LT         1     ",
                "                         DH-RT         1     ",
                "                                             ",
                "     1001      2         DH-LT         1     ",
                "                         DH-RT         1     ",
                "                                             ",
                "     1002      1         DH-LT         1     ",
                "                         DH-RT         1     ",
                "                                             ",
                "     1002      2         DH-LT         1     ",
                "                         DH-RT         1     ",
                "                                             ",
                "     1003      1         DH-LT         1     ",
                "                         DH-RT         1     ",
                "                                             ",
                "     1003      2         DH-LT         1     ",
                "                         DH-RT         1     ",
                "                                             ",
                "     1004      1         DH-LT         1     ",
                "                         DH-RT         1     ",
                "                                             ",
                "     1004      2         DH-LT         1     ",
                "                         DH-RT         1     ",
                "                                             ",
                "",
                "+===============================================================================",
                "",
                "")
  ids_vector=""
  reps_vector=""
  dt_length = 100
  summarise_reps = FALSE

  # IDs ONLY datatable
  datatable_name = "samples"
  expect_error(datatable_import(source_contents, destination_contents, datatable_name,
                                source_rel_path_link,ids_vector, reps_vector,
                                dt_length, summarise_reps))

  # try with summarise_reps TRUE
  datatable_name = "samples_SC-LUM_50µm"
  dt_length = 100
  summarise_reps = TRUE
  expect_error(datatable_import(source_contents, destination_contents, datatable_name,
                                source_rel_path_link,ids_vector, reps_vector,
                                dt_length, summarise_reps))

  # test incorrect datatable name throw an error
  datatable_name = "samples2"
  expect_error(datatable_import(source_contents, destination_contents, datatable_name,
                                source_rel_path_link,ids_vector, reps_vector,
                                dt_length, summarise_reps))

})


test_that("modify_matching_import_ids()", {
  # testing the validity of recursive function - concat_ids_ext()

  ### TEST IDs are not modified with no ID matches
  dest_datatables2 <- list(
    samples = tibble::tibble(ID=c('1002','2001','3002','4001'))
  )
  datatable_name2 <- "samples"
  dc_list2 <- list(IDs=c("1001", "3001", "5001"))
  source_rel_path2 <- fs::path("dir", "tn-j___008_--_PN_src.Rmd")
  source_rel_path2 <- paste0("[tn-j___008](", source_rel_path2, ")")
  #modify_matching_import_ids(dest_datatables2, datatable_name2, dc_list2, source_rel_path2)
  # "1001" "3001" "5001"
  expect_snapshot(cat(modify_matching_import_ids(dest_datatables2, datatable_name2, dc_list2, source_rel_path2)))

  ### TEST IDs are not modified with no match in datatable names
  dest_datatables2 <- list(
    samples = tibble::tibble(ID=c('1001','2001','3001','4001'))
  )
  datatable_name2 <- "samples2"
  dc_list2 <- list(IDs=c("1001", "3001", "5001"))
  source_rel_path2 <- fs::path("dir", "tn-j___008_--_PN_src.Rmd")
  source_rel_path2 <- paste0("[tn-j___008](", source_rel_path2, ")")
  #modify_matching_import_ids(dest_datatables2, datatable_name2, dc_list2, source_rel_path2)
  # "1001" "3001" "5001"
  expect_snapshot(cat(modify_matching_import_ids(dest_datatables2, datatable_name2, dc_list2, source_rel_path2)))

  ### TEST IDs are modified with small string from source note name
  dest_datatables2 <- list(
    samples = tibble::tibble(ID=c('1001','2001','3001','4001'))
  )
  datatable_name2 <- "samples"
  dc_list2 <- list(IDs=c("1001", "3001", "5001"))
  source_rel_path2 <- fs::path("dir", "tn-j___008_--_PN_src.Rmd")
  source_rel_path_link2 <- paste0("[tn-j___008](", source_rel_path2, ")")
  #modify_matching_import_ids(dest_datatables2, datatable_name2, dc_list2, source_rel_path2)
  # "1001.tnj" "3001.tnj" "5001"
  expect_snapshot(cat(modify_matching_import_ids(
                        dest_datatables=dest_datatables2,
                        datatable_name=datatable_name2,
                        dc_list=dc_list2,
                        source_rel_path_link=source_rel_path_link2)))


  ### TEST ALL IDs are modified further with shifted small string IF ONE ID matches first small string
   # testing the recursive algorithm!
  dest_datatables2 <- list(
    samples = tibble::tibble(ID=c('1001', '1001.tnj', '2001','3001','4001'))
  )
  datatable_name2 <- "samples"
  dc_list2 <- list(IDs=c("1001", "3001", "5001"))
  source_rel_path2 <- fs::path("dir", "tn-j___008_--_PN_src.Rmd")
  source_rel_path2 <- paste0("[tn-j___008](", source_rel_path2, ")")
  #modify_matching_import_ids(dest_datatables2, datatable_name2, dc_list2, source_rel_path2)
  # "1001.nj0" "3001.nj0" "5001"
  expect_snapshot(cat(modify_matching_import_ids(dest_datatables2, datatable_name2, dc_list2, source_rel_path2)))

  ### TEST ALL IDs are modified further with EXPANDED small string IF ONE ID continually matches first small strings
  # testing the recursive algorithm - that the increment increases appropriately
  dest_datatables2 <- list(
    samples = tibble::tibble(ID=c('1001', '1001.tnj', '1001.nj0', '1001.j00', '1001.008', '1001.08P', '1001.8PN',
                                  '1001.PNs', '1001.Nsr', '1001.src', '1001.rcR', '1001.cRm', '1001.Rmd',
                                  '2001','3001','4001'))
  )
  datatable_name2 <- "samples"
  dc_list2 <- list(IDs=c("1001", "3001", "5001"))
  source_rel_path2 <- fs::path("dir", "tn-j___008_--_PN_src.Rmd")
  source_rel_path_link2 <- paste0("[tn-j___008](", source_rel_path2, ")")
  #modify_matching_import_ids(dest_datatables2, datatable_name2, dc_list2, source_rel_path2)
  # "1001.tnj0" "3001.tnj0" "5001"
  expect_snapshot(cat(modify_matching_import_ids(dest_datatables2, datatable_name2,
                                                 dc_list2, source_rel_path_link2)))

})


test_that("datatable_build_from_tibble()", {

  # test default dt generation
  tb <- tibble::tibble(c(1001, 1002, 1003, 1004))
  tb <- tibble::add_column( tb, c("PBS_RT PBS_RT", "PBS_RT PBS_RT",
                                  "PBS_RT PBS_RT", "PBS_RT PBS_RT") )
  names(tb) <- c("ID", "wash_con")
  datatable_name <- "samples"
  dt_function="CREATE"
  dt_length=100
  DATATABLE_SPACER_CHAR="="
  # test function
  expect_snapshot(cat(build_datatable_from_tibble(tb, datatable_name, dt_function,
                                                  dt_length, DATATABLE_SPACER_CHAR ),
                      sep='\n'))

})

