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
bowtie_heatmap <- function(dt_bowtie = NULL,
                                 total_reads = TRUE,
                                 alignment_rate = TRUE,
                                 headers = "all",
                                 samples = NULL,
                                 value = "raw",
                                 filter_samples = NULL,
                                 bar_plot = FALSE
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
  }
  if (value == "raw") {
  dt_int <- dt_bowtie %>% dplyr::select(Sample, !!(index_k)) %>%
    dplyr::mutate(dplyr::across(!c(Sample, total_reads), .value_fun)) %>%
    dplyr::mutate(dplyr::across(!c(Sample, total_reads),
                                readr::parse_number))
  }
  if (!is.null(filter_samples)) {
    dt_int <- dt_int %>%
                dplyr::filter(Sample %in% filter_samples)
  }
  # Making a matrix with only the column that should be in the heatmap
  mat <- dt_int %>%
           dplyr::select(!c(Sample, total_reads, alignment_rate)) %>%
           as.matrix()
  row.names(mat) <- dt_int$Sample

  # Making the pallete
  colors = RColorBrewer::brewer.pal(n = 9 , name ="Greens")
  colors = viridis::viridis(256, option = "D", direction = -1)

  # Heatmap
  # Annotations
  # Deciding colors
  col_fun2 <- circlize::colorRamp2(c(0,100, 2000, 100000, 1000000),
                                   c("#FDE725FF", "#5DC863FF", "#21908CFF", "#2C728EFF", "#440154FF"))
  ## total reads barplot
  if (total_reads == TRUE) {
    if ("total_reads" %in% colnames(dt_int)) {
      bar_ha <- ComplexHeatmap::rowAnnotation(
        T_reads = ComplexHeatmap::anno_barplot(as.numeric(dt_int$total_reads),
                               gp = grid::gpar(fill = "red")
        )
      )
      t_reads <- as.numeric(format(dt_int$total_reads, scientific = TRUE,
                        digits = 1))
      text_reads <- as.character(t_reads)

      Tr_ha <- ComplexHeatmap::rowAnnotation(
        T_reads = ComplexHeatmap::anno_text(paste0("  ", text_reads),
                                           just = "left",
                                           gp = grid::gpar(fontsize = 9,
                                                          fill = col_fun2(t_reads),
                                                          col = "white",
                                                          border = "white"),
                                           show_name = TRUE),
                                           annotation_name_rot = 0,
                                           width = (ComplexHeatmap::max_text_width(text_reads)*1.2)

      )

    } else {
      stop("total_sequences = TRUE, but there is no column 'total_reads'.")
    }
  }
  if (alignment_rate == TRUE) {
      if ("alignment_rate" %in% colnames(dt_int)) {
        text_ha <- ComplexHeatmap::rowAnnotation(
          Al_rate = ComplexHeatmap::anno_text(paste0(dt_int$alignment_rate, "%"),
                                                     gp = grid::gpar(fontsize = 9),
                                                     show_name = TRUE,
                                                     just = "left"),
          annotation_name_rot = 0

          )

      } else {
        stop("alignment_rate = TRUE, but there is no column 'alignment_rate'.")
      }
  }
  if (headers == "main") {
    factor <- "Main"
  } else {
    headers_i_m <- grep("_H", colnames(mat))
    factor <- c()
    for (i in 1:length(headers_i_m)) {
      if (i == 1) {
        factor[seq(1,(headers_i_m[i+1]-1))] <- "Main"
      } else if (1 < i & headers_i_m[i] < max(headers_i_m)) {
        factor[seq(headers_i_m[i],(headers_i_m[i+1]-1))] <- paste0("Sec_", i)
      } else if (headers_i_m[i] >= max(headers_i_m)) {
        factor[seq(headers_i_m[i], ncol(mat))] <- paste0("Sec_", i)
      }
    }
  }
  # Plotting
  # Increasing legend size
  lgd = list(col_fun = col_fun2, title = "N_reads",
                               at = c(0,100, 1500, 3e+05, 5e+05, 1e+06),
                               break_dist = c(1, 2, 3, 2, 3),
                               legend_height = grid::unit(6, "cm"))
  # Plot without the annotations
  htmp <- ComplexHeatmap::Heatmap(mat,
                                  name = "N reads",
                                  col = col_fun2,
                                  rect_gp = grid::gpar(col = "white", lwd = 1),
                                  cluster_columns = FALSE,
                                  cluster_rows = FALSE,
                                  row_names_side = "left",
                                  column_names_side = "top",
                                  column_split = factor,
                                  row_names_gp = grid::gpar(fontsize = 10),
                                  column_names_max_height = grid::unit(11, "cm"),
                                  column_names_gp = grid::gpar(fontsize = 10),
                                  column_gap = grid::unit(3, "mm"),
                                  heatmap_legend_param = lgd
  )
  if(alignment_rate == FALSE & total_reads == FALSE) {
    htmp <- htmp
  } else if (alignment_rate == TRUE & total_reads == FALSE) {
    htmp <- htmp + text_ha
  } else if (alignment_rate == FALSE & total_reads == TRUE) {
    if (bar_plot = FALSE) {
      htmp <- htmp + Tr_ha
    } else {
      htmp <- htmp + bar_ha
    }
  } else if (alignment_rate == TRUE & total_reads == TRUE) {
    if (bar_plot = FALSE) {
      htmp <- htmp + Tr_ha + text_ha
    } else {
      htmp <- htmp + bar_ha + text_ha
    }
  }
  return(htmp)
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

