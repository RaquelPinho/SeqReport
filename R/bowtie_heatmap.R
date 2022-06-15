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
                                 filter_samples = NULL
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
  if (total_reads == TRUE) {
    if ("total_reads" %in% colnames(dt_int)) {
      bar_ha <- ComplexHeatmap::HeatmapAnnotation(
        T_reads = anno_barplot(as.numeric(dt_int$total_reads),
                               gp = grid::gpar(fill = "red")
        )
      )

    } else {
      stop("total_sequences = TRUE, but there is no column 'total_reads'.")
    }
  } else {
   bar_ha <- NULL
  }

  if (alignment_rate == TRUE) {
      if ("alignment_rate" %in% colnames(fastqc_table)) {
        text_ha <- ComplexHeatmap::HeatmapAnnotation(
          Alignment_rate = anno_text(paste0(dt_int$alignment_rate, "%"))
          )


      } else {
        stop("alignment_rate = TRUE, but there is no column 'alignment_rate'.")
      }
  } else {
    text_ha <- NULL
  }

  if (headers == "main") {
    factor <- "Main"
  } else {
    headers_i_m <- grep("_H", colnames(mat))
    mat_list <- list()
    for (i in 1:length(headers_i_m)) {
      if (i == 1) {
        mat_list[[i]] <- mat[,seq(1,(headers_i_m[i+1]-1))]
      } else if (1 < i & headers_i_m[i] < max(headers_i_m)) {
        mat_list[[i]] <- mat[,seq(headers_i_m[i],(headers_i_m[i+1]-1))]
      } else if (headers_i_m[i] >= max(headers_i_m)) {
        mat_list[[i]] <- mat[,seq(headers_i_m[i], ncol(mat))]
      }
    }

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
  if(alignment_rate == FALSE & total_reads == FALSE) {
    htmp <- ComplexHeatmap::Heatmap(mat,
                                    name = "N reads",
                                    col = col_fun,
                                    rect_gp = grid::gpar(col = "white", lwd = 1),
                                    cluster_columns = FALSE,
                                    cluster_rows = FALSE,
                                    row_names_side = "left",
                                    column_names_side = "top",
                                    column_split = factor,
                                    row_names_gp = grid::gpar(fontsize = 10),
                                    column_names_max_height = grid::unit(11, "cm"),
                                    column_names_gp = grid::gpar(fontsize = 10),
                                    column_gap = grid::unit(3, "mm")
    )
  }
}
col_fun <- circlize::colorRamp2(c(0, 30, 50,100, 180, 1500, 2000, 220000, 250000, 270000, 300000,  600000,650000, 1500000), hcl_palette = "viridis", reverse = TRUE )
col_fun <- circlize::colorRamp2(c(0,50, 180, 1500, 2000, 25000,30000, 60000, 120000), hcl_palette = "viridis", reverse = TRUE )
scales::show_col( col_fun(c(0, 10, 20, 30, 50,100, 180, 1500, 2000, 220000, 250000, 270000, 300000, 550000, 600000,650000, 1000000,1500000)))
viridis::viridis(9)
[1] "#440154FF" "#472D7BFF" "#3B528BFF" "#2C728EFF" "#21908CFF"
[6] "#27AD81FF" "#5DC863FF" "#AADC32FF" "#FDE725FF"
col_fun2 <- circlize::colorRamp2(c(0,100, 2000, 100000, 1000000), c("yellow", "green", "darkgreen", "lightblue", "darkblue"))
htmp <- ComplexHeatmap::Heatmap(mat,
                                name = "N reads",
                                col = colors,
                                rect_gp = grid::gpar(col = "white", lwd = 1),
                                cluster_columns = FALSE,
                                cluster_rows = FALSE,
                                row_names_side = "left",
                                column_names_side = "top"


)

                                ,
                                row_names_side = "left",
                                column_names_max_height = grid::unit(11, "cm"),
                                column_names_gp = grid::gpar(fontsize = 10),
                                row_names_gp = grid::gpar(fontsize = 10),

)



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
