test_that("The error messages are working", {
  expect_error(
    bwa_report_group(suffix = ".txt",
                     suffix_to_remove = "_report.txt",
                     samples = NULL),
    "Path argument is missing with no default."
  )
  expect_error(
    bwa_report_group(path = "./fakedir",
                     suffix = ".txt",
                     suffix_to_remove = "_report.txt",
                     samples = NULL),
    "Path doesn't exist."
  )
})

test_that("The results are as expected", {
  parent_testdata_dir <- file.path(system.file(paste0("extdata/testdata/"),
                          package = "SeqReport" ))
  testdata_dir <- file.path(parent_testdata_dir, "BWA")
  test_report <- readRDS(file = file.path(parent_testdata_dir, "bwa_report.RDS"))
  expect_equal(
    # without sample list
    bwa_report_group( path = testdata_dir, suffix = ".txt",
                       suffix_to_remove = "_report.txt",
                       samples = NULL),
    test_report
  )
   # with sample list
  test_report_samples <- readRDS(file.path(parent_testdata_dir, "bwa_report_samples.RDS"))
  expect_equal(
    # with no samples list
    bwa_report_group( path = testdata_dir, suffix = ".txt",
                      suffix_to_remove = "_report.txt",
                      samples = c("sp1", "sp2", "sp3")),
    test_report_samples
  )
})
