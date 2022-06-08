#' reads a html table from file into R
#'
#' @description function to get the table from the html file and transpose it.
#'
#' @import tibble
#' @import janitor
#' @import  XML
#'
#'
#' @param file path to the html file.
#' @param info The table to be extracted: "stats" stats table or "hits" the overrepresented
#' sequences table. Defaul "stats".
#' @param transpose logical, if the table should be transposed or not.
#' Default = TRUE.
#' @return a tibble from the chosen table from the fastqc html file.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' file <- "path/to/file/file.html"
#' table <- read_html_table(file = file, info = stats, transpose = TRUE)
#' }
#'
read_html_table <- function(file, info = "stats", transpose = TRUE) {

  if (!(info %in% c("stats", "hits"))) {
    stop("info argument is wrong!")
  }

  index <- ifelse(info == "stats", 1, 2)
  table <- XML::readHTMLTable(file)[[index]]

  if (transpose == TRUE) {
    table <- table %>%
      t() %>%
      tibble::as_tibble(.name_repair = 'unique') %>%
      janitor::row_to_names(row_number = 1)
  }
  if (!tibble::is_tibble(table)) {
    table <- tibble::as_tibble(table)
  }
  return(table)
}
