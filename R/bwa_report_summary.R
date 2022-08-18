#' bwa_report_summary
#'
#' @param dt_bwa data.frame or tibbles produced by the function bwa_group_report.
#' @param path path to the directory containing the bwamem report files
#' @param suffix suffix of the files containing the bwamem report, usually ".txt"
#' @param suffix_to_remove if you want the sample name to be derived from the files
#' with a suffix removal. default: "_merged_panel_nt_report.txt"
#' @param samples a character vector containing the names to replace the samples names
#' in the same order as the files. default: NULL
#' @param exclude @param exclude character vector containing the name of the samples to be excluded
#' from the summary.
#'
#'
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
#' @importFrom mgsub mgsub
#' @importFrom stats median
#'
#' @return
#' @export
#'
#' @examples
bwa_report_summary <- function(path = NULL, dt_bwa = NULL,
                               suffix = ".txt",
                               suffix_to_remove = "_merged_panel_nt_report.txt",
                               samples = NULL,
                               exclude = NULL) {
  if (is.null(dt_bwa)) {
    if (is.null(path)) {
      stop("Path and dt_bwa arguments are missing with no default.")
    } else {
      dt_bwa <- bwa_group_report(
        path = path,
        suffix = suffix,
        suffix_to_remove = suffix_to_remove,
        samples = samples
      )
    }
  }
  # Excluding chosen samples if exclude is not null.
  if (!is.null(exclude)) {
    dt_bwa <- dt_bwa %>%
      dplyr::filter(!.data$Sample %in% exclude)
  }

  # Formating colnames
  options(scipen = 999, digits = 2)
  colnames(dt_bwa) <- mgsub::mgsub(pattern = c(" ", ":"),
                                   replacement = c("_", ""), colnames(dt_bwa))
  # Total reads stats
  mean_raw_total_sequences <- mean(dt_bwa$raw_total_sequences)
  median_raw_total_sequences <- stats::median(dt_bwa$raw_total_sequences)
  max_raw_total_sequences <- max(dt_bwa$raw_total_sequences)
  max_raw_total_sequences_samples <- dt_bwa$Sample[which(dt_bwa$raw_total_sequences == max_raw_total_sequences)]
  min_raw_total_sequences <- min(dt_bwa$raw_total_sequences)
  min_raw_total_sequences_samples <- dt_bwa$Sample[which(dt_bwa$raw_total_sequences == min_raw_total_sequences)]
  # Pairing information frequency of
  paired_reads_prop <- dt_bwa$reads_paired/dt_bwa$raw_total_sequences
  mean_paired_prop <- mean(paired_prop)
  median_paired_prop <- stats::median(paired_reads_prop)
  max_paired_prop <- max(paired_reads_prop)
  max_paired_prop_samples <- dt_bwa$Sample[which(paired_reads_prop == max_paired_prop)]
  if (length(max_paired_prop_samples) == nrow(dt_bwa)) {
    max_paired_prop_samples <- "all"
  }
  min_paired_prop <- min(paired_reads_prop)
  min_paired_prop_samples <- dt_bwa$Sample[which(paired_reads_prop == min_paired_prop)]
  if (length(min_paired_prop_samples) == nrow(dt_bwa)) {
    min_paired_prop_samples <- "all"
  }
  # Fragment length
  frag_length <- as.double(dt_bwa$average_length)
  mean_avg_frag_length <- mean(frag_length)
  max_average_frag_length <- max(frag_length)
  min_average_frag_length <- min(frag_length)
  # Alignment rate stats
  alig_rate <- as.double(gsub("%", "", dt_bwa$alignment_rate))
  mean_alig_rate <- mean(alig_rate)
  median_alig_rate <- stats::median(alig_rate)
  max_alig_rate <- max(alig_rate)
  max_alig_rate_samples <- dt_bwa$Sample[which(alig_rate == max_alig_rate)]
  if (length(max_alig_rate_samples) == nrow(dt_bwa)) {
    max_alig_rate_samples <- "all"
  }
  min_alig_rate <- min(alig_rate)
  min_alig_rate_samples <- dt_bwa$Sample[which(alig_rate == min_alig_rate)]
  if (length(min_alig_rate_samples) == nrow(dt_bwa)) {
    min_alig_rate_samples <- "all"
  }
  # Inward directly mapping
  alig_conc_rate <- as.double(dt_bwa$nward_oriented_pairs/ dt_bwa$reads_mapped_and_paired * 100)
  mean_alig_conc_rate <- mean(alig_conc_rate)
  median_alig_conc_rate <- median(alig_conc_rate)
  max_alig_conc_rate <- max(alig_conc_rate)
  max_alig_conc_rate_samples <- dt_bwa$Sample[which(alig_conc_rate == max_alig_conc_rate)]
  if (length(max_alig_conc_rate_samples) == nrow(dt_bwa)) {
    max_alig_conc_rate_samples <- "all"
  }
  min_alig_conc_rate <- min(alig_conc_rate)
  min_alig_conc_rate_samples <- dt_bwa$Sample[which(alig_conc_rate == min_alig_conc_rate)]
  if (length(min_alig_conc_rate_samples) == nrow(dt_bwa)) {
    min_alig_conc_rate_samples <- "all"
  }


  cat("Summary of the bwa report files.\n",
      "There were ", nrow(dt_bwa), "samples analyzed.\n",
      "In those, the average number of reads was ", mean_raw_total_sequences, " and the median ",
      median_raw_total_sequences, ".\n",
      "The minimun number of reads was ", min_raw_total_sequences, " from samples: ",
      paste(min_raw_total_sequences_samples, collapse = "\n"),
      "\n",
      "The maximun number of reads was ", max_raw_total_sequences, " from samples: ",
      paste(max_raw_total_sequences_samples, collapse = "\n"),
      "\n",
      "The average, median, minimun and maximun rate of paired reads were ",
      mean_paired_prop, "%, ", median_paired_prop, "%, ",
      min_paired_prop, "% and ", max_paired_prop, "% respectively.\n",
      "Samples with the minimum rate of paired reads were: ",
      paste(min_paired_prop_samples, collapse = "\n"),
      "\n",
      "The mean, minimum and maximum average fragment size were ", mean_avg_frag_length,
      ", ", min_average_frag_length, " and ", max_average_frag_length, ", respectivbely.\n",
      "The average, median, minimum and maximun alignement rate were ",
      mean_alig_rate, "%, ", median_alig_rate, "%, ",
      min_alig_rate, "% and ", max_alig_rate, "% respectively.\n",
      "Samples with the minimum alignment rate were: ", paste(min_alig_rate_samples, collapse = "\n"),
      "\n",
      "The inward, concordant alignment rate for the paired reads were in average  ", mean_alig_conc_rate,
      "% with a median, minimum and maximun of ", median_alig_conc_rate, "%, ", min_alig_conc_rate, "%, ",
      max_alig_conc_rate, "% respectively.\n",
      "Samples with the minimun concordant alignment rate were:",
      paste(min_alig_conc_rate_samples, collapse = "\n"),
      ".\n",
      fill = 60, sep = ""
  )
}
