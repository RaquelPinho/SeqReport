#' bwa_report_group
#'
#' @param path path to the directory containing the bwamem report files
#' @param suffix suffix of the files containing the bwamem report, usually ".txt"
#' @param suffix_to_remove if you want the sample name to be derived from the files
#' with a suffix removal. default: "_merged_panel_nt_report.txt"
#' @param samples a character vector containing the names to replace the samples names
#' in the same order as the files. default: NULL
#'
#' @importFrom dplyr mutate
#' @importFrom magrittr "%>%"
#' @importFrom dplyr mutate_at
#' @importFrom tibble rownames_to_column
#'
#'
#' @return a data.frame object
#'
#' @export
#'
#' @examples
bwa_report_group <- function(path = NULL, suffix = ".txt",
                             suffix_to_remove = "_merged_panel_nt_report.txt",
                             samples = NULL) {
  if (is.null(path)) {
    stop("Path argument is missing with no default.")
  } else {
    if (!file.exists(path = path)) {
      stop("Path doesn't exist.")
    } else {
      # Geting the list of files
      files <- list.files(path, pattern = suffix, recursive = TRUE)
      # Getting the information on the files
      list_bwa <- lapply(files, function(file) {
        options(scipen=999)
        dt <- read.delim(file.path(path, file), header = FALSE, comment.char = "#")
        dt <- dt[grep("SN", dt$V1),c(2:3)]
        names(dt) <- c( "Stats", file)
        dt <- t(dt)
        colnames(dt) <- as.vector(dt[1,])
        dt <- dt[-1,]
      })
      names(list_bwa) <- files
      dt_bwa <- do.call(rbind, list_bwa)


    }

  }
   # Formatting data.frame
  if (!is.null(suffix_to_remove)) {
    dt_bwa <- dt_bwa %>%
              as.data.frame() %>%
              tibble::rownames_to_column(var = "Sample") %>%
              dplyr::mutate(Sample = gsub(suffix_to_remove, "", Sample),
                            Files = files) %>%
              dplyr::mutate_at(vars(-(c("Sample","Files"))), as.numeric)


  }
  if (!is.null(samples)) {
    dt_bwa <- dt_bwa %>%
              dplyr::mutate(Sample = samples)
  }

  return(dt_bwa)
}
