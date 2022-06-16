# Making a package to analyse both fastqc, and alignemts bwamem and bowtie.
# The package is named SeqReport
library(devtools)
use_this::create_package("D:/Raquel/Desktop/SeqReport/")
## conventions:
# variables and fucntions will be snake_case
# lines will have up to 90 characters
# lintr::lint_package() and styler::style_file() are going to be used to check the
# formatation.

# Doing the  version control
# I used GitBash to create the remote origin repository
use_this::use_git()

## first function
# This first funciton will be used to get information from the html file from FastQC
# for all files in a directory and putting them in a table.
## Creating a function file
usethis::use_r("fastqc_group_report")

# Creating an auxiliary function to convert the html table into a dataframe in R
usethis::use_r("readHTMLtable")
# I will rename the function because it has the same name as the XML function.
usethis::rename_files("readHTMLTable", "read_html_table")

# loading all functions
devtools::load_all()

## Creating documetation
devtools::document()

## Import package that will be used is the two functions, this will be added to the
# documentation
usethis::use_package("tibble")
usethis::use_package("XML")
usethis::use_package("xml2")
usethis::use_package("rvest")
usethis::use_package("dplyr")
usethis::use_package("janitor")
usethis::use_pipe(export = TRUE)

## updating the document
devtools::document()
usethis::use_mit_license()
devtools::check()
usethis::use_git(message = "added the examples and fixed the read_html_table function")

## checking formating so far
lintr::lint_package()
styler::style_file("R/read_html_table.R")
styler::style_file("R/fastqc_group_report.R")

usethis::use_git(message = "finished the fastqc_group_report.R function and added description")

# Creating test files for the read_html_table and fastqc_group_report.R
usethis::use_testthat()
usethis::use_test("read_html_table")
testthat::test_file("./tests/testthat/test-read_html_table.R")
styler::style_file("tests/")
usethis::use_git()
devtools::load_all()

usethis::use_test("fastqc_group_report")
testthat::test_file("./tests/testthat/test-fastqc_group_report.R")
styler::style_file("./tests/testthat/test-fastqc_group_report.R")

# Creating the function to get the data from bowtie log
usethis::use_r("bowtie_group_report")
devtools::document()
styler::style_file("R/bowtie_group_report.R")
usethis::use_test("bowtie_group_report")
testthat::test_file("./tests/testthat/test-bowtie_group_report.R")
usethis::use_package("purrr")
lintr::lint_package()
styler::style_file("./tests/testthat/test-bowtie_group_report.R")
devtools::check()

# Creating function to visualize Fasqc results
usethis::use_r("fastqc_heatmap_stats")
usethis::use_package("ggplot2")
styler::style_file("R/fastqc_heatmap_stats.R")

# Creating a function
usethis::use_r("fastqc_heatmap_reads")
usethis::use_package("grid")
usethis::use_package("ComplexHeatmap")
usethis::rename_files("fastqc_heatmap_reads", "bowtie_heatmap")
usethis::use_r("bowtie_heatmap")
usethis::use_test("fastqc_heatmap_stats")
testthat::test_file("./tests/testthat/test-fastqc_heatmap_stats.R")
styler::style_file("./tests/testthat/test-fastqc_heatmap_stats.R")
