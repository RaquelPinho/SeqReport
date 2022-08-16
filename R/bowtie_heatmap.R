#' Bowtie alignment heatmap
#'
#' @description produces a heatmap of the number of reads aligned in the different
#' alignment parameterd given by the report from bowtie. It needs to be used with data.frames
#' or tibbles produced by the function bowtie_group_report.
#'
#' @import ComplexHeatmap
#' @import dplyr
#' @import readr
#' @import grid
#' @import circlize
#' @importFrom rlang .data
#'
#'
#' @param dt_bowtie data.frame or tibbles produced by the function bowtie_group_report.
#' @param total_reads if you would like to display the total number of reads for each sample.
#' Default = TRUE.
#' @param alignment_rate if you would like to display the alignment rate for each sample.
#' Default = TRUE.
#' @param headers if only the main statistical headers or all headers should be used.
#' the main statistical headers are the first 4 stats of the bowtie report. Can be "main" or
#' "all". Default = "all".
#' @param samples a vector of the same length as the number of rows in dt_bowtie, to be used to name
#' the samples. If not present the name of the samples will be the the "Sample" column
#' from the dt_bowtie table.
#' @param value which value should be displayed in the heatmap. The percentages or the raw number of reads.
#' Possibilities: "raw" or "percent". Default = "raw".
#' @param filter_samples vector with names of samples to be excluded from visualization.
#' if samples parameter is provided, the samples to filter should be present in the samples parameter.
#' @param barplot logical, default = FALSE. if the total reads should be displayed as a bar plot.
#' Only works if the total_reads is set to TRUE.
#'
#'
#' @return a Heatmap object.
#'
#' @export
#'
#' @examples
#'
#' dir <- file.path(system.file(paste0("extdata/testdata/Bowtie/"), package = "SeqReport"))
#' dt_bowtie <- bowtie_group_report(path = dir, samples = c("sp1", "sp2", "sp3"))
#' bowtie_heatmap(dt_bowtie)
#'
bowtie_heatmap <- function(dt_bowtie = NULL,
                           total_reads = TRUE,
                           alignment_rate = TRUE,
                           headers = "all",
                           samples = NULL,
                           value = "raw",
                           filter_samples = NULL,
                           barplot = FALSE) {
  # Checking if table exists
  if (is.null(dt_bowtie)) {
    stop("dt_bowtie not provided!")
  }
  # Checking if the name of samples should be changed
  if (!is.null(samples)) {
    if (length(samples) != nrow(dt_bowtie)) {
      stop("Samples has different length than the number of rows in dt_bowtie!")
    } else {
      dt_bowtie$Sample <- samples
    }
  }
  # Checking if there are any samlpes that need to be removed
  if (!is.null(filter_samples)) {
    if (!filter_samples %in% dt_bowtie$Sample) {
      stop("Samples to filter not present in the data.")
    } else {
      dt_bowtie <- dt_bowtie %>%
        dplyr::filter(!.data$Sample %in% filter_samples)
    }
  }
  # Getting the column that have the info on the header chosen by the user
  headers_i <- grep("_H", colnames(dt_bowtie))
  file_i <- grep("file", colnames(dt_bowtie))

  if (headers == "main") {
    index_k <- seq(headers_i[1], headers_i[2] - 1)
  } else {
    index_k <- seq(headers_i[1], file_i - 1)
  }

  # Getting columns of interest

  if (total_reads == TRUE) {
    t_reads_i <- grep("total_reads", colnames(dt_bowtie))
    index_k <- c(index_k, t_reads_i)
  }
  if (alignment_rate == TRUE) {
    a_rate_i <- grep("alignment_rate", colnames(dt_bowtie))
    index_k <- c(index_k, a_rate_i)
  }
  if (value == "percent") {
    dt_int <- dt_bowtie %>%
      dplyr::select(.data$Sample, !!(index_k)) %>%
      dplyr::mutate(dplyr::across(!c(.data$Sample, .data$total_reads), .perc_fun)) %>%
      dplyr::mutate(dplyr::across(
        !c(.data$Sample, .data$total_reads),
        readr::parse_number
      ))
  }
  if (value == "raw") {
    dt_int <- dt_bowtie %>%
      dplyr::select(.data$Sample, !!(index_k)) %>%
      dplyr::mutate(dplyr::across(!c(.data$Sample, total_reads), .value_fun)) %>%
      dplyr::mutate(dplyr::across(
        !c(.data$Sample, total_reads),
        readr::parse_number
      ))
  }

  # Making a matrix with only the column that should be in the heatmap
  mat <- dt_int %>%
    dplyr::select(!c(.data$Sample, total_reads, alignment_rate)) %>%
    as.matrix()
  row.names(mat) <- dt_int$Sample

  # Heatmap
  # Annotations
  # Deciding colors
  col_fun2 <- circlize::colorRamp2(
    c(0, 100, 2000, 100000, 1000000),
    c("#FDE725FF", "#5DC863FF", "#21908CFF", "#2C728EFF", "#440154FF")
  )
  ## total reads barplot
  if (total_reads == TRUE) {
    if ("total_reads" %in% colnames(dt_int)) {
      bar_ha <- ComplexHeatmap::rowAnnotation(
        T_reads = ComplexHeatmap::anno_barplot(as.numeric(dt_int$total_reads),
          gp = grid::gpar(fill = "darkblue")
        )
      )
      t_reads <- as.numeric(format(dt_int$total_reads,
        scientific = TRUE,
        digits = 1
      ))
      text_reads <- as.character(t_reads)

      tr_ha <- ComplexHeatmap::rowAnnotation(
        T_reads = ComplexHeatmap::anno_text(paste0("  ", text_reads),
          just = "left",
          gp = grid::gpar(
            fontsize = 9,
            fill = col_fun2(t_reads),
            col = "white",
            border = "white"
          ),
          show_name = TRUE
        ),
        annotation_name_rot = 0,
        width = (ComplexHeatmap::max_text_width(text_reads) * 1.2)
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
          just = "left"
        ),
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
    for (i in seq_along(headers_i_m)) {
      if (i == 1) {
        factor[seq(1, (headers_i_m[i + 1] - 1))] <- "Main"
      } else if (1 < i && headers_i_m[i] < max(headers_i_m)) {
        factor[seq(headers_i_m[i], (headers_i_m[i + 1] - 1))] <- paste0("Sec_", i)
      } else if (headers_i_m[i] >= max(headers_i_m)) {
        factor[seq(headers_i_m[i], ncol(mat))] <- paste0("Sec_", i)
      }
    }
  }
  # Plotting
  # Increasing legend size
  lgd <- list(
    col_fun = col_fun2, title = "N_reads",
    at = c(0, 100, 1500, 3e+05, 5e+05, 1e+06),
    break_dist = c(1, 2, 3, 2, 3),
    legend_height = grid::unit(6, "cm")
  )
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
  if (alignment_rate == FALSE && total_reads == FALSE) {
    htmp <- htmp
  } else if (alignment_rate == TRUE && total_reads == FALSE) {
    htmp <- htmp + text_ha
  } else if (alignment_rate == FALSE && total_reads == TRUE) {
    if (barplot == FALSE) {
      htmp <- htmp + tr_ha
    } else {
      htmp <- htmp + bar_ha
    }
  } else if (alignment_rate == TRUE && total_reads == TRUE) {
    if (barplot == FALSE) {
      htmp <- htmp + tr_ha + text_ha
    } else {
      htmp <- htmp + bar_ha + text_ha
    }
  }
  return(htmp)
}

#' Helper for the bowtie_heatmap function
#'
#' @description extracts the percent of the data.frames
#' or tibbles produced by the function bowtie_group_report.
#'
#' @param  x data.frame from function bowtie_group_report.
#'
#' @return a tibble.
#'
#' @keywords internal
#'


.perc_fun <- function(x) {
  str_col <- sapply(x, function(ele) {
    spl_ele <- unlist(strsplit(ele, " "))
    ele <- ifelse(length(spl_ele) == 2,
      spl_ele[2],
      spl_ele[1]
    )
    return(ele)
  })
  return(str_col)
}


#' Helper for the bowtie_heatmap function
#'
#' @description extracts the raw value of the data.frames
#' or tibbles produced by the function bowtie_group_report.
#'
#' @param  x data.frame from function bowtie_group_report.
#'
#' @return a tibble.
#'
#' @keywords internal
#'
.value_fun <- function(x) {
  str_col <- sapply(x, function(ele) {
    spl_ele <- unlist(strsplit(ele, " "))
    ele <- spl_ele[1]
    return(ele)
    })
  return(str_col)
}
