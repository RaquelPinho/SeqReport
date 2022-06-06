#' reads a html table from file into R
#'
#' @description helper function to get the table from the html file and transpose it.
#'
#' @param file path to the html file
#' @param info if you want the stats table or the over represented sequences table.
#'
#' @return a tibble from the chosen table transposed
#' @export
#'
#' @examples
readHTMLTable <- function(file, info = c("stats", "hits")) {
  index <- ifelse(info == "stats", 1, 2)
  table <- XML::readHTMLTable(file.path(path,l_files[i]))[[index]] %>%
             t() %>%
             dplyr::as_tibble() %>%
             janitor::row_to_names(row_number = 1)

  return(table)
}
