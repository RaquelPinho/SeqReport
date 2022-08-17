#' Fastqc report summary
#'
#' @param fastqc_table table created from the fastqc_group_report. if the table is
#' not available the directory containing the fastqc .html files can be input on the
#' path parameter
#'
#' @param path if there is no fastqc_table, a path to the directory containing the
#' fastqc .html files can be added
#'
#' @param exclude character vector containing the name of samples to be
#' excluded in the summary
#'
#' @import dplyr
#' @importFrom rlang .data
#'
#'
#' @return a printed summary of the information in the .html, such as:
#' Average number of reads,
#' Minimum number of reads (and samples),
#' Maximum number of reads (and samples),
#' Average sequence length,
#' Minimum sequence length (and samples),
#' Samples with over represented adapter sequences.
#' Red flags (fastqc flags that failed in at least 50% of the samples)
#'
#'
#' @export
#'
#' @examples
#'
#' parent_testdata_dir <- file.path(system.file(paste0("extdata/testdata/"),
#' package = "SeqReport"))
#' data_table <- readRDS(file.path(parent_testdata_dir, "test_fqc_table_no_samples.RDS"))
#' fastqc_report_summary(fastqc_table = data_table)
#'
#'
#'
fastqc_report_summary <- function(fastqc_table = NULL, path = NULL, exclude = NULL) {
 # Columns to be used
  cols <- c("Sample", "Total Sequences", "Sequence length", "Over_rep")
 # Check if fastqc_table is present
  if (!is.null(fastqc_table)) {
    # checking the name of the columns
    if (!(all(cols %in% colnames(fastqc_table)))) {
      stop("Fastq table contain only the flag information!")
    }

  } else {
    if (is.null(path)) {
      stop("Neither a table nor a path was informed.")
    } else {
     # check if path exist
      if (file.exists(path)) {
        fastqc_table <- fastqc_group_report(path = path, info = "all", stats = TRUE, samples = NULL)
      } else  {
        stop("Path does not exist!")
      }

    }

  }
  # check if there are samples to be excluded
  if (!is.null(exclude)) {
    fastqc_table <- fastqc_table %>%
                     dplyr::filter(!.data$Sample %in% exclude)

  }
  # Making sure the cols are numeric
  fastqc_table <- fastqc_table %>%
                  dplyr::mutate_at(c("Total Sequences", "Sequence length"), as.numeric)
  # getting the information for summary
  avg_reads <- round(
                 mean(fastqc_table$`Total Sequences`), digits = 2)

  min_reads <- min(fastqc_table$`Total Sequences`)

  min_read_samples <- fastqc_table$Sample[fastqc_table$`Total Sequences` == min_reads]

  max_reads <- max(fastqc_table$`Total Sequences`)

  max_read_samples <- fastqc_table$Sample[fastqc_table$`Total Sequences` == max_reads]

  avg_length <- round(
                      mean(fastqc_table$`Sequence length`), digits = 2)
  min_length <- min(fastqc_table$`Sequence length`)

  min_length_samples <- fastqc_table$Sample[fastqc_table$`Sequence length` == min_length]

  if (length(min_length_samples) == nrow(fastqc_table)) {
                                   min_length_samples <- "all"
                                   }

  max_length <- max(fastqc_table$`Sequence length`)

  max_length_samples <- fastqc_table$Sample[fastqc_table$`Sequence length` == max_length]

  if (length(max_length_samples) == nrow(fastqc_table)) {
              max_length_samples <- "all"
              }
  seq_over <- fastqc_table$Sample[grep("Illumina", fastqc_table$Over_rep)]
  red_flags <- red_flags_fun(fastqc_table)

  cat("Summary of the FASTQC files.\n",
      "There were ", nrow(fastqc_table), " files analyzed.\n",
      "In those, the average number of reads was ", avg_reads, ".\n",
      "The minimun number of reads was ", min_reads,  " from samples: ", paste(min_read_samples, collapse = "\n"),
      "\n",
      "The maximun number of reads was ", max_reads, " from samples: ", paste(max_read_samples, collapse = "\n"),
      "\n",
      "The average, minimun and maximun read lengths were ", avg_length, ", ", min_length, ", ",
       max_length,
      " respectively.\n",
      "Samples with the minimum average read length were: ", paste(min_length_samples, collapse = "\n"),
      "\n",
      "Samples with overrepresented adapter sequences: ", paste(seq_over, collapse = "\n"), "\n",
      "FASTQC flags that fail in more than 50% of the samples: ", paste(red_flags, collapse = "\n"),
      fill = 60, sep = ""
    )
}


#' Helper for the fastq_report_summary function
#'
#' @description extracts the columns that have failed flags for more than 50% of the samples.
#'
#' @param  fastqc_table table created from the fastqc_group_report.
#'
#' @return a character vector containing the name of the columns.
#'
#' @keywords internal
#'
red_flags_fun <- function(fastqc_table) {
  cols_to_test <- grep("\\[", fastqc_table[1, ])
  count_flags <- apply(fastqc_table[, cols_to_test], 2,
                     FUN = function(col) {
                     count <- length(grep("FAIL", col))
                     return(count)
                     })
  red_cols <- which(count_flags >= nrow(fastqc_table) / 2)
  red_names <- colnames(fastqc_table[, cols_to_test])[red_cols]
  return(red_names)
}
