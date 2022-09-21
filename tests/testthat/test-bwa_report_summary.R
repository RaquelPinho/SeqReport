parent_testdata_dir <- file.path(system.file(paste0("extdata/testdata/"),
package = "SeqReport"))
testdata_dir <- file.path(parent_testdata_dir, "BWA")
dt_bwa <- readRDS(file =  file.path(parent_testdata_dir, "bwa_report.RDS"))

bwa_report_summary(path = testdata_dir, dt_bwa = NULL,
suffix = ".txt",
suffix_to_remove = "_report.txt",
samples = NULL,
exclude = NULL)

test_that("Test error messages.", {
 testthat::expect_error(bwa_report_summary(suffix = ".txt",
                                            suffix_to_remove = "_report.txt",
                                            samples = NULL,
                                            exclude = NULL),
      "Path and dt_bwa arguments are missing with no default.")
})

test_that("Test if  the printed output is correct.", {

  testthat::expect_output(
  bwa_report_summary(dt_bwa = dt_bwa,
                          path = NULL,
                          exclude = NULL),

    "Summary of the bwa report files.
There were 3
samples analyzed.")
  testthat::expect_output(
    bwa_report_summary(dt_bwa = NULL,
                          path = testdata_dir,
                          exclude = NULL),

    "Summary of the bwa report files.
There were 3
samples analyzed."
  )
})
