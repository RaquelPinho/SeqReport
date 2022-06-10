test_that("The error messages are working", {
  parent_testdata_dir <- file.path(system.file(paste0("extdata/testdata/"),
                                               package = "SeqReport"))
  testdata_dir <- file.path(parent_testdata_dir, "Bowtie")
  expect_error(
    bowtie_group_report(
      path = parent_testdata_dir,
      suffix = '.log',
      samples = NULL
    ),
    "There is no file with specified suffix in the directory!"
  )
  expect_error(
    bowtie_group_report(
      path = NULL,
      suffix = '.log',
      samples = NULL
    ),
    "Argument 'path' is missing, with no default"
  )
  expect_error(
    bowtie_group_report(
      path = file.path(parent_testdata_dir, "Fake_dir"),
      suffix = '.log',
      samples = NULL
    ),
    "Directory does not exist."
  )
  expect_error(
    bowtie_group_report(
      path = testdata_dir,
      suffix = '.log',
      samples = c("sample1", "sample2")
    ),
    "Vector of samples names, and file list are different lengths!"
  )
})
