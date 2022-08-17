test_that("Test error messages.", {
  parent_testdata_dir <- file.path(system.file(paste0("extdata/testdata/"),
                                               package = "SeqReport"
  ))

  testthat::expect_error(bowtie_report_summary(dt_bowtie = NULL,
                                               path = NULL,
                                               exclude = NULL),
                         "Path and dt_bowtie arguments are missing with no default.")
  })

test_that("Test if  the printed output is correct.", {
  parent_testdata_dir <- file.path(system.file(paste0("extdata/testdata/"),
                                               package = "SeqReport"
  ))
  datadir <- file.path(parent_testdata_dir, "Bowtie")
  data_table <- readRDS(file.path(parent_testdata_dir, "test_bowtie_no_samples.RDS"))
  testthat::expect_output(
    bowtie_report_summary(dt_bowtie = data_table,
                          path = NULL,
                          exclude = NULL),

    "Summary of the bowtie log files.
There were 3
samples analyzed.")
  testthat::expect_output(
    bowtie_report_summary(dt_bowtie = NULL,
                          path = datadir,
                          exclude = NULL),

    "Summary of the bowtie log files.
There were 3
samples analyzed."
  )
})

