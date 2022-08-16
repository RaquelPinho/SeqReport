#' Creates a table from the alignment reports from bowtie, containing information from a
#' all .log files in a directory.
#'
#' @param path The path to the directory containing the log files from the bowtie
#' alignment.
#' @param suffix The suffix of the report files created by the code similar to:
#' bowtie2 -x ref -1 sample1_R1.fq -2 sample1_R2.fq -S sample1.sam 2> sample1.log
#' In this case it would be .log. Default ".log"
#' @param samples A vector containing the names of the samples, should be the same
#' length as list.files(path, pattern = suffix), and follow the same order. If NULL,
#' sample names will be the file name without the suffix given.
#'
#' @importFrom purrr reduce
#' @import dplyr
#'
#' @return data frame of the stats contained in the log file from bowtie for all samples
#' in the directory.
#' @export
#'
#' @examples
#' dir <- file.path(system.file(paste0("extdata/testdata/Bowtie/"), package = "SeqReport"))
#' bowtie_group_report(path = dir)
#' bowtie_group_report(path = dir, samples = c("sp1", "sp2", "sp3"))
#'
bowtie_group_report <- function(path = NULL, suffix = ".log", samples = NULL) {
  if (is.null(path)) {
    stop("Argument 'path' is missing, with no default")
  }
  if (!dir.exists(path)) {
    stop("Directory does not exist.")
  }
  # creating the list of files to be used
  l_files <- list.files(path = path, pattern = suffix)
  if (length(l_files) == 0) {
    stop("There is no file with specified suffix in the directory!")
  }
  # Getting the name of the samples
  if (!is.null(samples)) {
    if (length(samples) != length(l_files)) {
      stop("Vector of samples names, and file list are different lengths!")
    }
  } else {
    samples <- gsub(suffix, "", basename(l_files))
  }
  # Getting the path to each individual file
  f_path <- file.path(path, l_files)
  # Getting the list of dataframes containing the information
  l_bowtie <- lapply(f_path, .import_text_from_log)
  # Combining the information in the list
  dt_bowtie <- l_bowtie %>%
    purrr::reduce(dplyr::full_join) %>%
    dplyr::mutate(Sample = samples) %>%
    dplyr::relocate(.data$Sample)
  return(dt_bowtie)
}

#' helper for bowtie_group_report
#'
#' @param file
#'
#' @import dplyr
#' @import tibble
#'
#' @return datatable of individual files
#' @keywords internal
#'
.import_text_from_log <- function(file) {
  # Read the text inside file
  text <- readLines(file)
  # Clean the text of extra lines or blank spaces
  text <- gsub("  ", "", text)
  text <- text[which(text != "----")]
  # Get info of total reads and overall alignment rate
  total_reads <- as.numeric(strsplit(text[1], " ")[[1]][1])
  index_rate <- grep("alignment rate", text)
  alignment_rate <- strsplit(text[index_rate], " ")[[1]][1]
  # Find the headers of the info
  text <- text[-c(1, index_rate)]
  headers <- grep("; of these:", text)
  # Removing 'of these'
  text <- gsub("; of these:", "", text)
  t_split <- strsplit(text, ") ")
  i <- 1
  names_dt <- c()
  dt <- c()
  while (i < length(t_split)) {
    if (i %in% headers) {
      if (length(t_split[[i]]) == 2) {
        names_dt[i] <- t_split[[i]][2]
        dt[i] <- t_split[[i]][1]
      } else {
        n_word <- length(strsplit(t_split[[i]], " ")[[1]])
        names_dt[i] <- paste(strsplit(
          t_split[[i]], " "
        )[[1]][3:n_word], collapse = "_")
        dt[i] <- paste(strsplit(
          t_split[[i]], " "
        )[[1]][1:2], collapse = "")
      }
      names_dt[i] <- paste0(names_dt[i], "_H")
    } else {
      names_dt[i] <- t_split[[i]][2]
      dt[i] <- t_split[[i]][1]
    }
    names_dt[i] <- gsub(" ", "_", names_dt[i])
    dt[i] <- gsub("%", "%)", dt[i])
    i <- i + 1
  }
  dt <- dt %>%
    t() %>%
    tibble::as_tibble(.name_repair = "unique") %>%
    dplyr::mutate(
      File = basename(file),
      total_reads = total_reads,
      alignment_rate = alignment_rate
    )
  colnames(dt) <- c(names_dt, "file", "total_reads", "alignment_rate")
  return(dt)
}
