#' bowtie_report_summary
#'
#'
#' @param dt_bowtie data.frame or tibble resulting from bowtie_group_report
#'
#' @param path The path to the directory containing the log files from the bowtie
#' alignment. Only necessary if no dt_bowtie is given.
#' @param suffix The suffix of the report files created by the code similar to:
#' bowtie2 -x ref -1 sample1_R1.fq -2 sample1_R2.fq -S sample1.sam 2> sample1.log
#' In this case it would be .log. Default ".log". Only used if path is not null.
#' @param samples A vector containing the names of the samples, should be the same
#' length as list.files(path, pattern = suffix), and follow the same order. If NULL,
#' sample names will be the file name without the suffix given. Can only be
#' used if path is not null.
#' @param exclude character vector containing the name of the samples to be excluded
#' from the summary.
#'
#'
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
#' @importFrom dplyr filter
#' @importFrom stringr str_split
#' @importFrom stats median
#'
#'
#' @return A printed summary of the bowtie logs.
#'
#' @export
#'
#' @examples
bowtie_report_summary <- function(path = NULL, dt_bowtie = NULL,
                                  suffix = ".log", samples = NULL,
                                  exclude = NULL) {
  if (is.null(dt_bowtie)) {
    if (is.null(path)) {
      stop("Path and dt_bowtie arguments are missing with no default.")
    } else {
      dt_bowtie <- bowtie_group_report(
        path = path,
        suffix = suffix,
        samples = samples
      )
    }
  }
  # Excluding chosen samples if exclude is not null.
  if (!is.null(exclude)) {
    dt_bowtie <- dt_bowtie %>%
      dplyr::filter(!.data$Sample %in% exclude)
  }
  # Total reads stats
  mean_total_reads <- mean(dt_bowtie$total_reads)
  median_total_reads <- stats::median(dt_bowtie$total_reads)
  max_total_reads <- max(dt_bowtie$total_reads)
  max_total_reads_samples <- dt_bowtie$Sample[which(dt_bowtie$total_reads == max_total_reads)]
  min_total_reads <- min(dt_bowtie$total_reads)
  min_total_reads_samples <- dt_bowtie$Sample[which(dt_bowtie$total_reads == min_total_reads)]
  # Pairing information frequency of
  paired_reads_prop <- stringr::str_split(dt_bowtie$were_paired_H, " ", simplify = TRUE)[, 2]
  paired_prop <- as.double(gsub("\\(|%)", "", paired_reads_prop))
  mean_paired_prop <- mean(paired_prop)
  median_paired_prop <- stats::median(paired_prop)
  max_paired_prop <- max(paired_prop)
  max_paired_prop_samples <- dt_bowtie$Sample[which(paired_prop == max_paired_prop)]
  if (length(max_paired_prop_samples) == nrow(dt_bowtie)) {
    max_paired_prop_samples <- "all"
  }
  min_paired_prop <- min(paired_prop)
  min_paired_prop_samples <- dt_bowtie$Sample[which(paired_prop == min_paired_prop)]
  if (length(min_paired_prop_samples) == nrow(dt_bowtie)) {
    min_paired_prop_samples <- "all"
  }
  # Alignment rate stats
  alig_rate <- as.double(gsub("%", "", dt_bowtie$alignment_rate))
  mean_alig_rate <- mean(alig_rate)
  median_alig_rate <- stats::median(alig_rate)
  max_alig_rate <- max(alig_rate)
  max_alig_rate_samples <- dt_bowtie$Sample[which(alig_rate == max_alig_rate)]
  if (length(max_alig_rate_samples) == nrow(dt_bowtie)) {
    max_alig_rate_samples <- "all"
  }
  min_alig_rate <- min(alig_rate)
  min_alig_rate_samples <- dt_bowtie$Sample[which(alig_rate == min_alig_rate)]
  if (length(min_alig_rate_samples) == nrow(dt_bowtie)) {
    min_alig_rate_samples <- "all"
  }
  # Alignment concordant stats
  alig_conc_rate <- stringr::str_split(dt_bowtie$aligned_concordantly_exactly_1_time, " ", simplify = TRUE)[, 2]
  alig_conc_rate <- as.double(gsub("\\(|%)", "", alig_conc_rate))
  mean_alig_conc_rate <- mean(alig_conc_rate)
  median_alig_conc_rate <- median(alig_conc_rate)
  max_alig_conc_rate <- max(alig_conc_rate)
  max_alig_conc_rate_samples <- dt_bowtie$Sample[which(alig_conc_rate == max_alig_conc_rate)]
  if (length(max_alig_conc_rate_samples) == nrow(dt_bowtie)) {
    max_alig_conc_rate_samples <- "all"
  }
  min_alig_conc_rate <- min(alig_conc_rate)
  min_alig_conc_rate_samples <- dt_bowtie$Sample[which(alig_conc_rate == min_alig_conc_rate)]
  if (length(min_alig_conc_rate_samples) == nrow(dt_bowtie)) {
    min_alig_conc_rate_samples <- "all"
  }


  cat("Summary of the bowtie log files.\n",
    "There were ", nrow(dt_bowtie), "samples analyzed.\n",
    "In those, the average number of reads was ", mean_total_reads, " and the median ",
    median_total_reads, ".\n",
    "The minimun number of reads was ", min_total_reads, " from samples: ",
    paste(min_total_reads_samples, collapse = "\n"),
    "\n",
    "The maximun number of reads was ", max_total_reads, " from samples: ",
    paste(max_total_reads_samples, collapse = "\n"),
    "\n",
    "The average, median, minimun and maximun rate of paired reads were ",
    mean_paired_prop, "%, ", median_paired_prop, "%, ",
    min_paired_prop, "% and ", max_paired_prop, "% respectively.\n",
    "Samples with the minimum rate of paired reads were: ",
    paste(min_paired_prop_samples, collapse = "\n"),
    "\n",
    "The average, median, minimum and maximun alignement rate were ",
    mean_alig_rate, "%, ", median_alig_rate, "%, ",
    min_alig_rate, "% and ", max_alig_rate, "% respectively.\n",
    "Samples with the minimum alignment rate were: ", paste(min_alig_rate_samples, collapse = "\n"),
    "\n",
    "The concordant alignment rate for the paired reads were in average  ", mean_alig_conc_rate,
    "% with a median, minimum and maximun of ", median_alig_conc_rate, "%, ", min_alig_conc_rate, "%, ",
    max_alig_conc_rate, "% respectively.\n",
    "Samples with the minimun concordant alignment rate were:",
    paste(min_alig_conc_rate_samples, collapse = "\n"),
    ".\n",
    fill = 60, sep = ""
  )
}
