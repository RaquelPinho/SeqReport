test_that("The error messages are working", {
  parent_testdata_dir <- file.path(system.file(paste0("extdata/testdata/"), package = "SeqReport"))
  testdata_dir <- file.path(parent_testdata_dir, "FastQC")
  expect_error(
    fastqc_group_report(
      path = parent_testdata_dir,
      info = "all",
      stats = TRUE,
      samples = NULL
    ),
    "There is no .html file in the directory!"
  )
  expect_error(
    fastqc_group_report(
      info = "all",
      stats = TRUE,
      samples = NULL
    ),
    "Argument 'path' is missing, with no default"
  )
  expect_error(
    fastqc_group_report(
      path = file.path(parent_testdata_dir, "Fake_dir"),
      info = "all",
      stats = TRUE,
      samples = NULL
    ),
    "Directory does not exist."
  )
  expect_error(
    fastqc_group_report(
      path = testdata_dir,
      info = "all",
      stats = TRUE,
      samples = c("sample1", "sample2")
    ),
    "Vector of samples names, and file list are different lengths!"
  )
})

test_that("The results are as expected", {
  parent_testdata_dir <- file.path(system.file(paste0("extdata/testdata/"), package = "SeqReport"))
  testdata_dir <- file.path(parent_testdata_dir, "FastQC")
  test_table <- readRDS(file.path(parent_testdata_dir, "test_fqc_table_samples.RDS"))
  expect_equal(# with sample list
    fastqc_group_report(
      path = testdata_dir,
      info = "all",
      stats = TRUE,
      samples = c("sample1", "sample2", "sample3")
    ),
    test_table
  )
  test_table <- readRDS(file.path(parent_testdata_dir, "test_fqc_table_no_samples.RDS"))
  expect_equal(# with no samples list
    fastqc_group_report(
      path = testdata_dir,
      info = "all",
      stats = TRUE,
      samples = NULL
    ),
    test_table
  )
  test_table <- readRDS(file.path(parent_testdata_dir, "test_fqc_table_no_samples_PASS.RDS"))
  expect_equal(# with no samples list
    fastqc_group_report(
      path = testdata_dir,
      info = "pass",
      stats = TRUE,
      samples = NULL
    ),
    test_table
  )
  test_table <- readRDS(file.path(parent_testdata_dir, "test_fqc_table_no_samples_WARNING.RDS"))
  expect_equal(# with no samples list
    fastqc_group_report(
      path = testdata_dir,
      info = "warnings",
      stats = TRUE,
      samples = NULL
    ),
    test_table
  )
  test_table <- readRDS(file.path(parent_testdata_dir, "test_fqc_table_no_samples_FAIL.RDS"))
  expect_equal(# with no samples list
    fastqc_group_report(
      path = testdata_dir,
      info = "fail",
      stats = TRUE,
      samples = NULL
    ),
    test_table
  )
  test_table <- readRDS(file.path(parent_testdata_dir, "test_fqc_table_no_samples_WARN_FAIL.RDS"))
  expect_equal(# with no samples list
    fastqc_group_report(
      path = testdata_dir,
      info = "warn_fail",
      stats = TRUE,
      samples = NULL
    ),
    test_table
  )
  test_table <- readRDS(file.path(parent_testdata_dir, "test_fqc_table_no_samples_no_stats.RDS"))
  expect_equal(# with no samples list
    fastqc_group_report(
      path = testdata_dir,
      info = "all",
      stats = FALSE,
      samples = NULL
    ),
    test_table
  )
})
