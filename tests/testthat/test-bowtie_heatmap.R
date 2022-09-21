test_that("The error messages are working", {
  dir <- file.path(system.file(paste0("extdata/testdata/Bowtie/"), package = "SeqReport"))
  dt_bowtie <- bowtie_group_report(path = dir, samples = c("sp1", "sp2", "sp3"))
  expect_error(
    bowtie_heatmap(dt_bowtie, filter_samples = c("sp4")),
    "Samples to filter not present in the data."
  )
  expect_error(
    bowtie_heatmap(dt_bowtie, samples = c("sp4", "sp5")),
    "Samples has different length than the number of rows in dt_bowtie!"
  )
  expect_error(
    bowtie_heatmap(samples = c("sp4", "sp5")),
    "dt_bowtie not provided!"
  )
})

dir <- file.path(system.file(paste0("extdata/testdata/Bowtie/"), package = "SeqReport"))
dt_bowtie <- bowtie_group_report(path = dir)

test_that("The function is working with data.frame, sample names", {
   vdiffr::expect_doppelganger(
   "bowtie heatmap samples",
   bowtie_heatmap(dt_bowtie,
                  samples = c("sp1", "sp2", "sp3"))

  )
})


test_that("The function is working with data.frame, sample names, filter_samples", {
  vdiffr::expect_doppelganger(
   "bowtie heat samples filter",
   bowtie_heatmap(dt_bowtie, samples = c("sp1", "sp2", "sp3"),
                  filter_samples = c("sp1"))
  )
})
