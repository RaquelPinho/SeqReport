#' fastqc_group_report
#'
#' @description This function will get the information from all .html files from the
#' FastQC evaluation presented in a directory and will combine the information in a
#' data_table.
#'
#' @import xml2
#' @importFrom rvest html_nodes
#' @import dplyr
#' @import tibble
#'
#' @param path path to the directory that contain one or more .html files from the
#' FastQC.
#'
#' @param info which information should be added to the table: "all" All basic stats,
#' "warnings" the statistics with warning, "warn_fail" the statistics with warning and the
#' ones with fail, "fails" only the statistics that failed, "pass" the statistics that passed.
#' default "all".
#'
#' @param stats logical, if the sequence length, total number of sequences, %GC content
#' and unique overrepresented sequences sources should be added to the table. default: TRUE
#'
#' @param samples character vector. Vector containing the names of the samples for the files
#' to be reported in the directory in the same order as in the directory. default NULL.
#' If NULL, the basename of the files are going to be used.
#'
#' @return dataframe containing the statistical information from all fastqc.html files in the directory
#' in path.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' path <- "path/to/directory/containing/html/files"
#' table_fastqc <- fastqc_group_report(
#'   path = path,
#'   info = all,
#'   stats = TRUE,
#'   samples = NULL
#' )
#' }
#'
fastqc_group_report <- function(path = NULL, info = "all", stats = TRUE, samples = NULL) {
  # Checking if there is a path argument and if the path given exists
  if (is.null(path)) {
    stop("Argument 'path' is missing, with no default")
  }
  if (!dir.exists(path)) {
    stop("Directory does not exist.")
  }
  # creating the list of files to be used
  l_files <- list.files(path = path, pattern = ".html")
  if (length(l_files) == 0) {
    stop("There is no .html file in the directory!")
  }
  # Getting the name of the samples
  if (!is.null(samples)) {
    if (length(samples) != length(l_files)) {
      stop("Vector of samples names, and file list are different lengths!")
    }
  } else {
    samples <- gsub(".html", "", basename(l_files))
  }

  f_files <- lapply(seq_along(l_files), function(i) {
    # Getting file path
    f_path <- file.path(path, l_files[i])
    # Getting the body of the html file
    html_body <- xml2::read_html(f_path) %>%
      rvest::html_nodes("body")
    # extracting the pass, warning, fail results
    res_info <- html_body %>%
      xml2::xml_find_all("//div[@class='summary']//img") %>%
      xml2::xml_attr(attr = "alt")
    # extracting the name of the stats relative to the results and combining them
    names(res_info) <- html_body %>%
      xml2::xml_find_all("//div[@class='summary']//a/text()") %>%
      xml2::xml_text()
    # creating a table
    table_info <- res_info %>%
      t() %>%
      tibble::as_tibble()
    table_info <- cbind("Sample" = samples[i], table_info)
    # getting the stats information
    if (stats == FALSE) {
      table_stats <- table_info
    } else {
      table_stats <- read_html_table(file = f_path, info = "stats") %>%
        dplyr::mutate(Sample = samples[i])
      over_rep <- unlist(unique(XML::readHTMLTable(f_path)[[2]][, "Possible Source"]))
      table_stats <- table_info %>%
        dplyr::full_join(table_stats, by = "Sample") %>%
        dplyr::mutate(Over_rep = paste(over_rep, collapse = ", "))
    }
    return(table_stats)
  })

  fastqc_table <- do.call(rbind, f_files)

  # Using the helper function to select the columns based in the info parameter
  if (info != "all") {
    col_list <- .filter_info(fastqc_table, info = info)
    fastqc_table <- fastqc_table[, col_list]
  }

  return(fastqc_table)
}

#' Internal function - generic function to filter the basic stats columns based on info.
#'
#'
#' @param col which column to apply the function to.
#'
#' @param info which information should be added to the table: "all" All basic stats,
#' "warnings" the statistics with warning, "warn_fail" the statistics with warning and the
#' ones with fail, "fails" only the statistics that failed, "pass" the statistics that passed.
#' default "all".
#'
#'
#' @keywords internal
#'
#'
.filter_info <- function(fastqc_table, info = "warnings") {
  if (info == "pass") {
    pattern <- "PASS"
  }
  if (info == "warnings") {
    pattern <- "WARNING"
  }
  if (info == "warn_fail") {
    pattern <- "WARNING|FAIL"
  }
  if (info == "fail") {
    pattern <- "FAIL"
  }
  col_keep <- c()
  for (col in 2:12) {
    if (length(grep(pattern, fastqc_table[, col])) != 0) {
      col_keep <- c(col_keep, col)
    }
  }
  col_list <- seq_along(fastqc_table)[-c(1:12)]
  col_list <- c(1, col_keep, col_list)
  return(col_list)
}
