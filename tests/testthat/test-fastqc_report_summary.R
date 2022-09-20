test_that("Test error messages.", {
  parent_testdata_dir <- file.path(system.file(paste0("extdata/testdata/"),
                                               package = "SeqReport"
  ))
  data_table <- readRDS(file.path(parent_testdata_dir, "test_fqc_table_no_samples.RDS"))

  testthat::expect_error(fastqc_report_summary(fastqc_table = NULL,
                                               path = NULL,
                                               exclude = NULL),
                         "Neither a table nor a path was informed.")
  testthat::expect_error(fastqc_report_summary(fastqc_table = NULL,
                                               path = file.path(parent_testdata_dir, "fake_dir"),
                                               exclude = NULL),
                         "Path does not exist!")
  testthat::expect_error(fastqc_report_summary(fastqc_table = data_table[, -c(16, 18, 20)],
                                               path = NULL,
                                               exclude = NULL),
                         "Fastq table contain only the flag information!")

})

test_that("Test if  the printed output is correct.", {
  parent_testdata_dir <- file.path(system.file(paste0("extdata/testdata/"),
                                               package = "SeqReport"
  ))
  datadir <- file.path(parent_testdata_dir, "FastQC")
  data_table <- readRDS(file.path(parent_testdata_dir, "test_fqc_table_no_samples.RDS"))
  testthat::expect_output(
    fastqc_report_summary(fastqc_table = data_table,
                                                path = NULL,
                                                exclude = NULL),

  "Summary of the FASTQC files.
There were 3 files analyzed.")
  testthat::expect_output(
    fastqc_report_summary(fastqc_table = NULL,
                          path = datadir,
                          exclude = NULL),

    "Summary of the FASTQC files.
There were 3 files analyzed."
    )

})
