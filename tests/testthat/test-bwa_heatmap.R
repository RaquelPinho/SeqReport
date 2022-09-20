parent_testdata_dir <- file.path(system.file(paste0("extdata/testdata/"),
package = "SeqReport" ))
testdata_dir <- file.path(parent_testdata_dir, "BWA")
dt_bwa_ex <- bwa_report_group(path = testdata_dir, suffix = ".txt",
suffix_to_remove = "_report.txt",
samples = NULL)
bwa_heatmap(dt_bwa = dt_bwa_ex,
total_reads = TRUE,
alignment_rate = TRUE,
samples = NULL,
value = "raw",
filter_samples = NULL,
barplot = FALSE)
