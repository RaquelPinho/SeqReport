test_that("The error messages are working", {
  parent_testdata_dir <- file.path(system.file(paste0("extdata/testdata/"), package = "SeqReport"))
  test_table <- readRDS(file.path(parent_testdata_dir, "test_fqc_table_samples.RDS"))
  expect_error(
    fastqc_heatmap_stats(
      fastqc_table = test_table[, c(1, 13:20)],
      total_sequences = TRUE
    ),
    "No statistical results in the fastqc_table!"
  )
  expect_error(
    fastqc_heatmap_stats(
      fastqc_table = NULL,
      total_sequences = TRUE
    ),
    "fastqc_table not provided!"
  )
  bad_table <- test_table %>%
    dplyr::rename(totalseq = `Total Sequences`)

  expect_error(
    fastqc_heatmap_stats(
      fastqc_table = bad_table,
      total_sequences = TRUE
    ),
    "total_sequences = TRUE, but there is no column 'Total Sequences'."
  )
})

test_that("The function is working", {
  parent_testdata_dir <- file.path(system.file(paste0("extdata/testdata/"), package = "SeqReport"))
  test_table <- readRDS(file.path(parent_testdata_dir, "test_fqc_table_samples.RDS"))
  plot <- readRDS(file.path(parent_testdata_dir, "heatmap_fqc_ts.RDS"))
  set.seed(111)
  expect_s4_class(
    fastqc_heatmap_stats(
      fastqc_table = test_table,
      total_sequences = TRUE
    ),
    "Heatmap"
  )
  expect_equal(
    fastqc_heatmap_stats(
      fastqc_table = test_table,
      total_sequences = TRUE
    )@matrix,
    plot@matrix
  )
  plot <- readRDS(file.path(parent_testdata_dir, "heatmap_fqc.RDS"))
  expect_s4_class(
    fastqc_heatmap_stats(
      fastqc_table = test_table,
      total_sequences = FALSE
    ),
    "Heatmap"
  )
  expect_equal(
    fastqc_heatmap_stats(
      fastqc_table = test_table,
      total_sequences = FALSE
    )@matrix,
    plot@matrix
  )
})
