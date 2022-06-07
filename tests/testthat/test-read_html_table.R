test_that("The function work with info = stats ", {
  testdata_dir <- file.path(system.file(paste0("extdata/testdata/"), package = "SeqReport"))
  test_file <- file.path(testdata_dir, "FastQC", "sample1_fastqc.html")
  test_table_stats_t <- readRDS(file.path(testdata_dir, "test_table_stats_t.RDS"))
  expect_equal(
    read_html_table(file = test_file, info = "stats"), test_table_stats_t
    )
})
test_that("The function work with transpose = FALSE ", {
  testdata_dir <- file.path(system.file(paste0("extdata/testdata/"), package = "SeqReport"))
  test_file <- file.path(testdata_dir, "FastQC", "sample1_fastqc.html")
  test_table_stats <- readRDS(file.path(testdata_dir, "test_table_stats.RDS"))
  expect_equal(
    read_html_table(file = test_file, info = "stats", transpose = FALSE), test_table_stats
  )
})

test_that("The function work with info = hits and transpose = FALSE ", {
  testdata_dir <- file.path(system.file(paste0("extdata/testdata/"), package = "SeqReport"))
  test_file <- file.path(testdata_dir, "FastQC", "sample1_fastqc.html")
  test_table_hits <- readRDS(file.path(testdata_dir, "test_table_hits.RDS"))
  expect_equal(
    read_html_table(file = test_file, info = "hits", transpose = FALSE), test_table_hits
  )
})

test_that("There will be an error when no info argument is present.", {
  testdata_dir <- file.path(system.file(paste0("extdata/testdata/"), package = "SeqReport"))
  test_file <- file.path(testdata_dir, "FastQC", "sample1_fastqc.html")
  test_table_hits <- readRDS(file.path(testdata_dir, "test_table_hits.RDS"))
  expect_error(
    read_html_table(file = test_file, transpose = FALSE),
    "info argument is missing or wrong,  with no default!")
})
