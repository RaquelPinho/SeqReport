#' Title
#'
#' @import ComplexHeatmap
#' @import dplyr
#' @import readr
#'
#'
#' @param dt_bowtie
#' @param total_reads
#' @param alignment_rate
#' @param all_headers
#' @param samples
#'
#' @return
#' @export
#'
#' @examples
fastqc_heatmap_reads <- function(dt_bowtie = NULL,
                                 total_reads = TRUE,
                                 alignment_rate = TRUE,
                                 headers = "all",
                                 samples = NULL,
                                 value = "raw"
                                 ) {
  # Checking if table exists
  if (is.null(dt_bowtie)) {
    stop("fastqc_table not provided!")
  }
  # Getting the column that have the info on the header chosen by the user
  headers_i <- grep("_H", colnames(dt_bowtie))
  file_i <- grep("file",  colnames(dt_bowtie))

  if (headers == "main") {
    index_k <-  seq(headers_i[1], headers_i[2] - 1)

  } else {
    index_k <- seq(headers_i[1],file_i - 1)
  }

  # Getting columns of interest

  if (total_reads == TRUE) {
    t_reads_i <-  grep("total_reads", colnames(dt_bowtie))
    index_k <- c(index_k, t_reads_i)
  }
  if (alignment_rate == TRUE) {
    a_rate_i = grep("alignment_rate", colnames(dt_bowtie))
    index_k = c(index_k, a_rate_i)
  }
  if (value == "percent") {
  dt_int <- dt_bowtie %>% dplyr::select(Sample, !!(index_k)) %>%
                     dplyr::mutate(dplyr::across(!c(Sample, total_reads), .perc_fun)) %>%
                     dplyr::mutate(dplyr::across(!c(Sample, total_reads),
                                   readr::parse_number))
  if (value == "raw") {
  dt_int <- dt_bowtie %>% dplyr::select(Sample, !!(index_k)) %>%
    dplyr::mutate(dplyr::across(!c(Sample, total_reads), .value_fun)) %>%
    dplyr::mutate(dplyr::across(!c(Sample, total_reads),
                                readr::parse_number))
  }
  # Making a matrix with only the column that should be in the heatmap
  mat <- dt_int %>%
           dplyr::select(!c(Sample, total_reads, alignment_rate)) %>%
           as.matrix()
  row.names(mat) <- dt$Sample

  # Making the pallete
  colors <- structure(c("green3", "darkorange2", "firebrick"),
                      names = c("[PASS]", "[WARNING]", "[FAIL]")
  )
  # Heatmap
  if (total_sequences == TRUE) {
    if ("Total Sequences" %in% colnames(fastqc_table)) {
      bar_ha <- ComplexHeatmap::HeatmapAnnotation(
        T_reads = anno_barplot(as.numeric(fastqc_table$`Total Sequences`),
                               gp = grid::gpar(fill = "red")
        )
      )
      htmp <- ComplexHeatmap::Heatmap(t(t_number),
                                      name = "Results",
                                      col = colors,
                                      rect_gp = grid::gpar(col = "white", lwd = 1),
                                      row_names_side = "left",
                                      column_names_max_height = grid::unit(11, "cm"),
                                      column_names_gp = grid::gpar(fontsize = 10),
                                      row_names_gp = grid::gpar(fontsize = 10),
                                      top_annotation = bar_ha
      )
    } else {
      stop("total_sequences = TRUE, but there is no column 'Total Sequences'.")
    }

  }
}

}


.perc_fun <- function(x) {
  str_col <-  sapply(x, function(ele) {
    spl_ele <- unlist(strsplit(ele, " "))
    ele <- ifelse(length(spl_ele) == 2,
                        spl_ele[2],
                        spl_ele[1]
    )
    return(ele)

  })
}

.value_fun <- function(x) {
  str_col <-  sapply(x, function(ele) {
    spl_ele <- unlist(strsplit(ele, " "))
    ele <- spl_ele[1]
    return(ele)

  })
}
