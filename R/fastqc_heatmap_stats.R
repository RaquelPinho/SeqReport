#' Graphical vision of the FastQC results from multiple samples
#'
#' @description Graphical results of fastqc for multiple samples, using the data.frame
#' resulting from fastqc_group_report function.
#'
#' @param fastqc_table A table resulting from fastqc_group_report function. If the
#' total_sequence parameter is set to TRUE, the dable should have a column names
#' "Total Sequences".
#' @param total_sequences If a barplot of the total number of reads should be displayed
#' on top of the sample heatmap. Defaul = TRUE.
#'
#' @import dplyr
#' @import grid
#' @import ComplexHeatmap
#'
#' @return An object (class Heatmap/ ComplexHeatmap)
#'
#' @export
#'
#'
#' @examples
#' \dontrun{
#' fastqc_heatmap_stats(fastqc_table)
#' }
#'
fastqc_heatmap_stats <- function(fastqc_table = NULL, total_sequences = TRUE) {
  # Checking if table exists
  if (is.null(fastqc_tableL)) {
    stop("fastqc_table not provided!")
  }
  # Getting the column that have the stats
  stats_cols <- grep("\\[", fastqc_table)

  # Checking if table has stats info
  if (length(stats_cols) == 0) {
    stop("No statistical results in the fastqc_table!")
  }
  # Making a tibble and a value matrix with only the stats cols
  t_number <- as.matrix(fastqc_table[, stats_cols])
  row.names(t_number) <- fastqc_table$Sample

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
  } else {
    htmp <- ComplexHeatmap::Heatmap(t(t_number),
      name = "Results",
      col = colors,
      rect_gp = grid::gpar(col = "white", lwd = 1),
      row_names_side = "left",
      column_names_max_height = grid::unit(11, "cm"),
      column_names_gp = grid::gpar(fontsize = 10),
      row_names_gp = grid::gpar(fontsize = 10)
    )
  }
  return(htmp)
}
