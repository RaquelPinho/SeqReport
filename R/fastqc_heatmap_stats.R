#' Title
#' @import ztable
#' @import ggplot2
#' @import dplyr
#' @import tidyr
#'
#'
#'
#' @param fastq_table
#'
#' @return
#' @export
#'
#'
#' @examples
fastqc_heatmap_stats <- function(fastqc_table = NULL) {
  # Getting the column that have the stats
  stats_cols <- grep("\\[", fastqc_table)

  # Making a tibble and a value matrix with only the stats cols
  t_number <- fastqc_table %>%
              dplyr::select(Sample, stats_cols) %>%
              tidyr::gather("stats", "result", -Sample)
  # Making the heatmap
  g <- t_number %>%
               ggplot2::ggplot(ggplot2::aes(x = Sample, y = stats, fill = result)) +
                 ggplot2::geom_tile(color = "white") +
                 ggplot2::scale_fill_manual(values = c("[PASS]" =  "green3",
                                                        "[WARNING]" = "darkorange2",
                                                        "[FAIL]" = "firebrick")) +
                 ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))
  return(g)

}


