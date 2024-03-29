% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fastqc_heatmap_stats.R
\name{fastqc_heatmap_stats}
\alias{fastqc_heatmap_stats}
\title{Graphical vision of the FastQC results from multiple samples}
\usage{
fastqc_heatmap_stats(fastqc_table = NULL, total_sequences = TRUE)
}
\arguments{
\item{fastqc_table}{A table resulting from fastqc_group_report function. If the
total_sequence parameter is set to TRUE, the dable should have a column names
"Total Sequences".}

\item{total_sequences}{If a barplot of the total number of reads should be displayed
on top of the sample heatmap. Defaul = TRUE.}
}
\value{
An object (class Heatmap/ ComplexHeatmap)
}
\description{
Graphical results of fastqc for multiple samples, using the data.frame
resulting from fastqc_group_report function.
}
\examples{
\dontrun{
fastqc_heatmap_stats(fastqc_table)
}

}
