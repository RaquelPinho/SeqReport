parent_testdata_dir <- file.path(system.file(paste0("extdata/testdata/"),
package = "SeqReport"))
testdata_dir <- file.path(parent_testdata_dir, "BWA")
dt_bwa_ex <- bwa_report_group(path = testdata_dir, suffix = ".txt",
                              suffix_to_remove = "_report.txt",
                              samples = NULL)

test_that("The error messages are working", {
   expect_error(
     bwa_heatmap(total_reads = TRUE,
                 alignment_rate = TRUE,
                 samples = NULL,
                 value = "raw",
                 filter_samples = NULL,
                 barplot = FALSE),
    "dt_bwa was not provided!"
  )
  expect_error(
    bwa_heatmap(dt_bwa = dt_bwa_ex,
                total_reads = TRUE,
                alignment_rate = TRUE,
                samples = c("sp4", "sp5"),
                value = "raw",
                filter_samples = NULL,
                barplot = FALSE),
    "Samples has different length than the number of rows in dt_bwa!"
  )
  expect_error(
    bwa_heatmap(dt_bwa = dt_bwa_ex,
                total_reads = TRUE,
                alignment_rate = TRUE,
                samples = NULL,
                value = "raw",
                filter_samples = c("sample4"),
                barplot = FALSE),
    "Samples to filter not present in the data."
  )
})
test_that("The function is working with data.frame, sample names", {
    vdiffr::expect_doppelganger(
    "bwa heatmap samples",
     bwa_heatmap(dt_bwa = dt_bwa_ex,
                total_reads = TRUE,
                alignment_rate = TRUE,
                samples = c("sp1", "sp2", "sp3"))

  )
})
test_that("The function is working with data.frame, sample names, filter_samples", {
   vdiffr::expect_doppelganger(
   "bwa heatmap samples filter",
    bwa_heatmap(dt_bwa = dt_bwa_ex,
                total_reads = TRUE,
                alignment_rate = TRUE,
                samples = c("sp1", "sp2", "sp3"),
                filter_samples = c("sp1"))
  )
})
