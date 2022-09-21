#' bwa_heatmap
#'
#' @param dt_bwa data.frame or tibbles produced by the function bwa_report_group.
#' @param total_reads if you would like to display the total number of reads for each sample.
#' Default = TRUE.
#' @param alignment_rate if you would like to display the alignment rate for each sample.
#' Default = TRUE.
#' @param samples a vector of the same length as the number of rows in dt_bwa, to be used to name
#' the samples. If not present the name of the samples will be the the "Sample" column
#' from the dt_bwa table.
#' @param value which value should be displayed in the heatmap. The percentages or the raw number of reads.
#' Possibilities: "raw" or "percent". Default = "raw".
#' @param filter_samples vector with names of samples to be excluded from visualization.
#' if samples parameter is provided, the samples to filter should be present in the samples parameter.
#' @param barplot logical, default = FALSE. if the total reads should be displayed as a bar plot.
#' Only works if the total_reads is set to TRUE.
#'
#' @importFrom magrittr "%>%"
#' @importFrom dplyr across
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom mgsub mgsub
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom circlize colorRamp2
#' @importFrom grid unit
#' @importFrom grid gpar
#' @import ComplexHeatmap
#'
#' @return a Heatmap object.
#'
#' @export
#'
#' @examples
#'
#' parent_testdata_dir <- file.path(system.file(paste0("extdata/testdata/"),
#' package = "SeqReport" ))
#' testdata_dir <- file.path(parent_testdata_dir, "BWA")
#' dt_bwa_ex <- bwa_report_group(path = testdata_dir, suffix = ".txt",
#' suffix_to_remove = "_report.txt",
#' samples = NULL)
#' bwa_heatmap(dt_bwa = dt_bwa_ex,
#' total_reads = TRUE,
#' alignment_rate = TRUE,
#' samples = NULL,
#' value = "raw",
#' filter_samples = NULL,
#' barplot = FALSE)
#'
#'
bwa_heatmap <- function(dt_bwa = NULL,
                           total_reads = TRUE,
                           alignment_rate = TRUE,
                           samples = NULL,
                           value = "raw",
                           filter_samples = NULL,
                           barplot = FALSE) {
  # Checking if table exists
  if (is.null(dt_bwa)) {
    stop("dt_bwa was not provided!")
  }
  # Checking if the name of samples should be changed
  if (!is.null(samples)) {
    if (length(samples) != nrow(dt_bwa)) {
      stop("Samples has different length than the number of rows in dt_bwa!")
    } else {
      dt_bwa$Sample <- samples
    }
  }
  # Checking if there are any samples that need to be removed
  if (!is.null(filter_samples)) {
    if (!filter_samples %in% dt_bwa$Sample) {
      stop("Samples to filter not present in the data.")
    } else {
      dt_bwa <- dt_bwa %>%
        dplyr::filter(!.data$Sample %in% filter_samples)
    }
  }
  # Getting the column that have the mains stats info
  # formating col names
  options(scipen = 999, digits = 2)
  colnames(dt_bwa) <- mgsub::mgsub(pattern = c(" ", ":"),
                                  replacement = c("_", ""), colnames(dt_bwa))

  cols <- c("Sample",
            "raw_total_sequences",
            "filtered_sequences",
            "reads_mapped",
            "reads_mapped_and_paired",
            "reads_unmapped",
            "reads_properly_paired",
            "average_length",
            "maximum_length",
            "percentage_of_properly_paired_reads_(%)"
                    )
  dt_bwa <- dt_bwa[, cols]

  # Getting the alignement rate
  dt_bwa <- dt_bwa %>%
              dplyr::mutate(alignment_rate = round(.data$reads_mapped / .data$raw_total_sequences * 100,
                                                   digits = 2))

  # Getting columns of interest

  if (total_reads == FALSE) {
    dt_bwa <- dt_bwa %>%
              dplyr::select(-.data$raw_total_sequences)

  }
  if (alignment_rate == FALSE) {
    dt_bwa <- dt_bwa %>%
              dplyr::select(-.data$alignment_rate)
  }
  if (value == "percent") {
    dt_int <- dt_bwa %>%
      dplyr::mutate(dplyr::across(-c(.data$Sample,
                                     .data$raw_total_sequences,
                                     .data$average_length,
                                     .data$maximum_length,
                                     .data$alignment_rate,
                                     .data$`percentage_of_properly_paired_reads_(%)`),
                                      ~ .x / raw_total_sequences * 100))
  }
  if (value == "raw") {
    dt_int <- dt_bwa
  }
  # Making a matrix with only the column that should be in the heatmap
  mat <- dt_int %>%
    dplyr::select(!c(.data$Sample,
                     .data$raw_total_sequences,
                     .data$`percentage_of_properly_paired_reads_(%)`,
                     .data$alignment_rate)) %>%
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
    if ("raw_total_sequences" %in% colnames(dt_int)) {
      bar_ha <- ComplexHeatmap::rowAnnotation(
        T_reads = ComplexHeatmap::anno_barplot(as.numeric(dt_int$raw_total_sequences),
                                               gp = grid::gpar(fill = "darkblue")
        )
      )
      t_reads <- as.numeric(format(dt_int$raw_total_sequences,
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
      stop("total_sequences = TRUE, but there is no column 'raw_total_sequences'.")
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
