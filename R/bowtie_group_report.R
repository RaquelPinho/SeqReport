#' Title
#'
#' @param path
#' @param suffix
#' @param samples
#'
#' @return
#' @export
#'
#' @examples
bowtie_group_report <- function(path = NULL, suffix = "log", samples = NULL) {

}


.import_text_from_log <- function(file) {
  # Read the text inside file
  text <- readLines(file)
  # Clean the text of extra lines or blank spaces
  text <- gsub("  ", "", text)
  text <- text[which(text != "----")]
  # Get info of total reads and overall alignment rate
  total_reads <- as.numeric(strsplit(text[1], " ")[[1]][1])
  index_rate <- grep("alignment rate", text)
  alignment_rate <- strsplit(text[index_rate], " ")[[1]][1]
  # Find the headers of the info
  text <- text[-c(1,index_rate)]
  headers <- grep("; of these:", text)
  # Removing 'of these'
  text <- gsub("; of these:", "", text)
  t_split <- strsplit(text, ") ")
  i <- 1
  names_dt <- c()
  dt <- c()
  while (i < length(t_split)) {
    if (i %in% headers) {
      if(length(t_split[[i]]) == 2) {
        names_dt[i] <- t_split[[i]][2]
        dt[i] <- t_split[[i]][1]
      } else {
        n_word <- length(strsplit(t_split[[i]], " ")[[1]])
        names_dt[i] <- paste(strsplit(
                                 t_split[[i]], " "
                               )[[1]][3:n_word], collapse = "_")
        dt[i] <- paste(strsplit(
                               t_split[[i]], " "
                               )[[1]][1:2], collapse = "")
      }
      names_dt[i] <- paste0(names_dt[i], "_H")
    } else {
      names_dt[i] <- t_split[[i]][2]
      dt[i] <- t_split[[i]][1]
    }
    names_dt[i] <- gsub(" ", "_", names_dt[i])
    dt[i] <- gsub("%", "%)", dt[i])
    i <- i + 1
  }
  dt <- dt %>%
          t() %>%
             tibble::as_tibble(.name_repair = "unique") %>%
                mutate(File = basename(file),
                       total_reads = total_reads,
                       alignment_rate = alignment_rate)
  colnames(dt) <- c(names_dt, "file", "total_reads", "alignment_rate")
  return(dt)
}
